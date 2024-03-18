(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

(** {!module: Mast} to {!module: Mir} translation of M programs. *)

(** {1 Translation context} *)

(** {2 Variable declarations}*)

(** {2 General translation context} *)

let get_var_from_name (var_data : Mir.Variable.t StrMap.t)
    (name : Mast.variable_name Pos.marked) : Mir.Variable.t =
  try StrMap.find (Pos.unmark name) var_data
  with Not_found ->
    Errors.raise_spanned_error
      (Format.asprintf "variable %s has not been declared" (Pos.unmark name))
      (Pos.get_position name)

(**{1 Translation}*)

(**{2 Variables}*)

(** Variables are tricky to translate; indeed, we have unrolled all the loops,
    and generic variables depend on the loop parameters. We have to interrogate
    the loop context for the current values of the loop parameter and then
    replace *inside the string* the loop parameter by its value to produce the
    new variable. *)

let get_var (var_data : Mir.Variable.t StrMap.t)
    (name : Mast.variable_name Pos.marked) : Mir.expression =
  Mir.Var (Pos.same_pos_as (get_var_from_name var_data name) name)

(**{2 Preliminary passes}*)

(**{2 SSA construction}*)

let translate_variable (var_data : Mir.Variable.t StrMap.t)
    (var : Mast.variable Pos.marked) : Mir.expression Pos.marked =
  match Pos.unmark var with
  | Mast.Normal name ->
      Pos.same_pos_as (get_var var_data (Pos.same_pos_as name var)) var
  | Mast.Generic _ -> assert false

(** {2 Translation of expressions}*)

let rec translate_expression (cats : Mir.cat_variable_data Mir.CatVarMap.t)
    (var_data : Mir.Variable.t StrMap.t) (f : Mast.expression Pos.marked) :
    Mir.expression Pos.marked =
  let expr =
    match Pos.unmark f with
    | Mast.TestInSet (positive, e, values) ->
        let new_e = translate_expression cats var_data e in
        let new_set_values =
          List.map
            (function
              | Com.FloatValue f -> Com.FloatValue f
              | Com.VarValue (v, pos) ->
                  let new_v =
                    match v with
                    | Mast.Normal name -> StrMap.find name var_data
                    | Mast.Generic _ -> assert false
                  in
                  Com.VarValue (new_v, pos)
              | Com.Interval (bv, ev) -> Com.Interval (bv, ev))
            values
        in
        Mir.TestInSet (positive, new_e, new_set_values)
    | Mast.Comparison (op, e1, e2) ->
        let new_e1 = translate_expression cats var_data e1 in
        let new_e2 = translate_expression cats var_data e2 in
        Mir.Comparison (op, new_e1, new_e2)
    | Mast.Binop (op, e1, e2) ->
        (* if
             Pos.unmark op = Mast.Mul
             && (Pos.unmark e1 = Mast.Literal (Float 0.)
                || Pos.unmark e2 = Mast.Literal (Float 0.))
           then
             (* It is difficult to do a broader or deeper analysis because of
                constant substitutions that could wrongly trigger the warning *)
             Errors.print_spanned_warning
               "Nullifying constant multiplication found." (Pos.get_position f);*)
        let new_e1 = translate_expression cats var_data e1 in
        let new_e2 = translate_expression cats var_data e2 in
        Mir.Binop (op, new_e1, new_e2)
    | Mast.Unop (op, e) ->
        let new_e = translate_expression cats var_data e in
        Mir.Unop (op, new_e)
    | Mast.Index (t, i) ->
        let t_var = translate_variable var_data t in
        let new_i = translate_expression cats var_data i in
        Mir.Index
          ( (match Pos.unmark t_var with
            | Mir.Var v -> v
            | _ -> assert false (* should not happen *)),
            new_i )
    | Mast.Conditional (e1, e2, e3) ->
        let new_e1 = translate_expression cats var_data e1 in
        let new_e2 = translate_expression cats var_data e2 in
        let new_e3 =
          match e3 with
          | Some e3 -> translate_expression cats var_data e3
          | None -> Pos.same_pos_as (Mir.Literal Mir.Undefined) e2
          (* the absence of a else branch for a ternary operators can yield an
             undefined term *)
        in
        Mir.Conditional (new_e1, new_e2, new_e3)
    | Mast.FunctionCall (f_name, args) ->
        let new_args = translate_func_args cats var_data args in
        Mir.FunctionCall (f_name, new_args)
    | Mast.Literal l -> (
        match l with
        | Mast.Float f -> Mir.Literal (Mir.Float f)
        | Mast.Undefined -> Mir.Literal Mir.Undefined)
    | Mast.Var var ->
        let new_var = translate_variable var_data (Pos.same_pos_as var f) in
        Pos.unmark new_var
    | Mast.NbCategory l ->
        Mir.NbCategory (Check_validity.mast_to_catvars l cats)
    | Mast.Attribut (v, a) -> (
        if
          Mir.CatVarMap.fold
            (fun _ { Mir.attributs; _ } res ->
              res
              && StrMap.fold
                   (fun attr _ res -> res && attr <> Pos.unmark a)
                   attributs true)
            cats true
        then Errors.raise_spanned_error "unknown attribut" (Pos.get_position a);
        let v_name =
          match Pos.unmark v with
          | Mast.Normal v_name -> v_name
          | _ -> assert false
        in
        match StrMap.find_opt v_name var_data with
        | Some var -> (
            if var.is_it then Mir.Attribut (Pos.same_pos_as v_name v, var, a)
            else
              match StrMap.find_opt (Pos.unmark a) var.attributes with
              | Some l -> Mir.Literal (Mir.Float (float (Pos.unmark l)))
              | None -> Mir.Literal Mir.Undefined)
        | _ ->
            let msg = Format.sprintf "unknown variable %s" v_name in
            Errors.raise_spanned_error msg (Pos.get_position v))
    | Mast.Size v -> (
        let v_name =
          match Pos.unmark v with
          | Mast.Normal v_name -> v_name
          | _ -> assert false
        in
        let var = StrMap.find v_name var_data in
        if var.is_it then Mir.Size (Pos.same_pos_as var v)
        else
          match var.is_table with
          | Some i -> Mir.Literal (Mir.Float (float_of_int i))
          | None -> Mir.Literal (Mir.Float 1.0))
    | Mast.NbAnomalies -> Mir.NbAnomalies
    | Mast.NbDiscordances -> Mir.NbDiscordances
    | Mast.NbInformatives -> Mir.NbInformatives
    | Mast.NbBloquantes -> Mir.NbBloquantes
    | Mast.Loop _ -> assert false
  in
  Pos.same_pos_as expr f

(** Mutually recursive with {!val: translate_expression} *)
and translate_func_args (cats : Mir.cat_variable_data Mir.CatVarMap.t)
    (var_data : Mir.Variable.t StrMap.t) (args : Mast.func_args) :
    Mir.expression Pos.marked list =
  match args with
  | Mast.ArgList args ->
      List.map (fun arg -> translate_expression cats var_data arg) args
  | Mast.LoopList _ -> assert false

(** {2 Translation of source file items}*)

let rec translate_prog (error_decls : Mir.Error.t StrMap.t)
    (cats : Mir.cat_variable_data Mir.CatVarMap.t)
    (var_data : Mir.Variable.t StrMap.t) prog =
  let rec aux res = function
    | [] -> List.rev res
    | (Mast.Formula f, pos) :: il -> begin
        match f with
        | Mast.SingleFormula sf, _ ->
            let var_e = translate_expression cats var_data sf.Mast.formula in
            let var =
              match
                Pos.unmark
                  (translate_variable var_data
                     (Pos.unmark sf.Mast.lvalue).Mast.var)
              with
              | Mir.Var var -> Pos.unmark var
              | _ -> assert false
              (* should not happen *)
            in
            let var_idx, var_e =
              match (Pos.unmark sf.Mast.lvalue).Mast.index with
              | Some ti -> (
                  let ei = translate_expression cats var_data ti in
                  match var.Mir.Variable.is_table with
                  | Some size -> (Some (size, ei), var_e)
                  | None -> (None, var_e))
              | None -> (None, var_e)
            in
            aux ((Mir.Affectation (var.id, var_idx, var_e), pos) :: res) il
        | Mast.MultipleFormulaes _, _ -> assert false
      end
    | (Mast.IfThenElse (e, ilt, ile), pos) :: il ->
        let expr, _ = translate_expression cats var_data e in
        let prog_then = aux [] ilt in
        let prog_else = aux [] ile in
        aux ((Mir.IfThenElse (expr, prog_then, prog_else), pos) :: res) il
    | (Mast.ComputeTarget tn, pos) :: il ->
        aux ((Mir.ComputeTarget tn, pos) :: res) il
    | (Mast.VerifBlock instrs, pos) :: il ->
        let instrs' = aux [] instrs in
        aux ((Mir.VerifBlock instrs', pos) :: res) il
    | (Mast.Print (std, args), pos) :: il ->
        let mir_args =
          List.rev
            (List.fold_left
               (fun res arg ->
                 let mir_arg =
                   match Pos.unmark arg with
                   | Mast.PrintString s -> Mir.PrintString s
                   | Mast.PrintName v -> (
                       let name =
                         match Pos.unmark v with
                         | Mast.Normal name -> name
                         | Mast.Generic _ -> assert false
                       in
                       match StrMap.find_opt name var_data with
                       | Some var ->
                           if var.is_it then
                             Mir.PrintName (Pos.same_pos_as name v, var)
                           else Mir.PrintString (Pos.unmark var.name)
                       | _ ->
                           let msg =
                             Format.sprintf "unknown variable %s" name
                           in
                           Errors.raise_spanned_error msg (Pos.get_position v))
                   | Mast.PrintAlias v -> (
                       let name =
                         match Pos.unmark v with
                         | Mast.Normal name -> name
                         | Mast.Generic _ -> assert false
                       in
                       match StrMap.find_opt name var_data with
                       | Some var ->
                           if var.is_it then
                             Mir.PrintAlias (Pos.same_pos_as name v, var)
                           else
                             Mir.PrintString
                               (match var.alias with
                               | Some a -> Pos.unmark a
                               | None -> "")
                       | _ ->
                           let msg =
                             Format.sprintf "unknown variable %s" name
                           in
                           Errors.raise_spanned_error msg (Pos.get_position v))
                   | Mast.PrintIndent e ->
                       Mir.PrintIndent (translate_expression cats var_data e)
                   | Mast.PrintExpr (e, min, max) ->
                       Mir.PrintExpr
                         (translate_expression cats var_data e, min, max)
                 in
                 Pos.same_pos_as mir_arg arg :: res)
               [] args)
        in
        aux ((Mir.Print (std, mir_args), pos) :: res) il
    | (Mast.Iterate (vn, vcats, expr, instrs), pos) :: il ->
        let var_name = Pos.unmark vn in
        let var_pos = Pos.get_position vn in
        (match StrMap.find_opt var_name var_data with
        | Some v ->
            let msg =
              Format.asprintf "variable already declared %a" Pos.format_position
                (Pos.get_position v.name)
            in
            Errors.raise_spanned_error msg pos
        | _ -> ());
        let var =
          Mir.Variable.new_var (var_name, var_pos) None ("iterator", var_pos)
            ~attributes:StrMap.empty ~cats:None ~typ:None ~is_table:None
            ~is_temp:false ~is_it:true
        in
        let var_data = StrMap.add var_name var var_data in
        let catSet = Check_validity.cats_variable_from_decl_list vcats cats in
        let mir_expr = translate_expression cats var_data expr in
        let prog_it = translate_prog error_decls cats var_data instrs in
        aux
          ((Mir.Iterate (var.Mir.Variable.id, catSet, mir_expr, prog_it), pos)
          :: res)
          il
    | (Mast.Restore (rest_params, instrs), pos) :: il ->
        let vars, var_params =
          List.fold_left
            (fun (vars, var_params) rest_param ->
              match Pos.unmark rest_param with
              | Mast.VarList vl ->
                  let vars =
                    List.fold_left
                      (fun vars vn ->
                        let var_name = Pos.unmark vn in
                        let var_pos = Pos.get_position vn in
                        match StrMap.find_opt var_name var_data with
                        | Some v -> begin
                            match Mir.VariableMap.find_opt v vars with
                            | None -> Mir.VariableMap.add v var_pos vars
                            | Some old_pos ->
                                Errors.raise_spanned_error
                                  (Format.asprintf
                                     "variable already specified %a"
                                     Pos.format_position old_pos)
                                  var_pos
                          end
                        | None ->
                            Errors.raise_spanned_error "unknown variable"
                              var_pos)
                      vars vl
                  in
                  (vars, var_params)
              | Mast.VarCats (vn, vcats, expr) ->
                  let var_name = Pos.unmark vn in
                  let var_pos = Pos.get_position vn in
                  (match StrMap.find_opt var_name var_data with
                  | Some v ->
                      let msg =
                        Format.asprintf "variable already declared %a"
                          Pos.format_position (Pos.get_position v.name)
                      in
                      Errors.raise_spanned_error msg pos
                  | _ -> ());
                  let var =
                    Mir.Variable.new_var (var_name, var_pos) None
                      ("iterator", var_pos) ~attributes:StrMap.empty ~cats:None
                      ~typ:None ~is_table:None ~is_temp:false ~is_it:true
                  in
                  let var_data = StrMap.add var_name var var_data in
                  let catSet =
                    Check_validity.cats_variable_from_decl_list vcats cats
                  in
                  let mir_expr = translate_expression cats var_data expr in
                  let var_params = (var, catSet, mir_expr) :: var_params in
                  (vars, var_params))
            (Mir.VariableMap.empty, [])
            rest_params
        in
        let prog_rest = translate_prog error_decls cats var_data instrs in
        aux ((Mir.Restore (vars, var_params, prog_rest), pos) :: res) il
    | (Mast.RaiseError (err_name, var_opt), pos) :: il ->
        let err_decl = StrMap.find (Pos.unmark err_name) error_decls in
        let var_res = Option.map Pos.unmark var_opt in
        aux ((Mir.RaiseError (err_decl, var_res), pos) :: res) il
    | (Mast.CleanErrors, pos) :: il -> aux ((Mir.CleanErrors, pos) :: res) il
    | (Mast.ExportErrors, pos) :: il -> aux ((Mir.ExportErrors, pos) :: res) il
    | (Mast.FinalizeErrors, pos) :: il ->
        aux ((Mir.FinalizeErrors, pos) :: res) il
    | (Mast.ComputeDomain _, _) :: _
    | (Mast.ComputeChaining _, _) :: _
    | (Mast.ComputeVerifs (_, _), _) :: _ ->
        assert false
  in
  aux [] prog

let get_targets (error_decls : Mir.Error.t StrMap.t)
    (cats : Mir.cat_variable_data Mir.CatVarMap.t)
    (var_data : Mir.Variable.t StrMap.t) (ts : Mast.target StrMap.t) :
    Mir.target_data Mir.TargetMap.t =
  StrMap.fold
    (fun _ t targets ->
      let target_name = t.Mast.target_name in
      let target_file = t.Mast.target_file in
      let target_apps = t.Mast.target_apps in
      let target_tmp_vars =
        StrMap.map (fun ((_, pos), size) -> (pos, size)) t.Mast.target_tmp_vars
      in
      let tmp_var_data =
        StrMap.fold
          (fun name (pos, size) tmp_var_data ->
            let size' = Pos.unmark_option (Mast.get_table_size_opt size) in
            let var =
              Mir.Variable.new_var (name, pos) None ("temporary", pos)
                ~attributes:StrMap.empty ~cats:None ~typ:None ~is_table:size'
                ~is_temp:true ~is_it:false
            in
            let tmp_var_data = StrMap.add name var tmp_var_data in
            tmp_var_data)
          target_tmp_vars var_data
      in
      let target_tmp_vars =
        StrMap.mapi
          (fun vn (pos, size) ->
            let var = StrMap.find vn tmp_var_data in
            let size' = Pos.unmark_option (Mast.get_table_size_opt size) in
            (var, pos, size'))
          target_tmp_vars
      in
      let target_prog =
        translate_prog error_decls cats tmp_var_data t.Mast.target_prog
      in
      let target_data =
        Mir.
          {
            target_name;
            target_file;
            target_apps;
            target_tmp_vars;
            target_prog;
          }
      in
      Mir.TargetMap.add (Pos.unmark target_name) target_data targets)
    ts Mir.TargetMap.empty

let translate (p : Mast.program) : Mir.program =
  let p = Expand_macros.proceed p in
  let prog = Check_validity.proceed p in
  let prog_targets = prog.prog_targets in
  let var_category_map = prog.prog_var_cats in
  let var_data = prog.prog_vars in
  let errs = prog.prog_errors in
  let targets = get_targets errs var_category_map var_data prog_targets in
  Mir.
    {
      program_safe_prefix = prog.prog_prefix;
      program_applications = prog.prog_apps;
      program_var_categories = var_category_map;
      program_rule_domains = prog.prog_rdoms;
      program_verif_domains = prog.prog_vdoms;
      program_vars = var_data;
      program_targets = targets;
    }
