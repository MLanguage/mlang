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

let get_var_from_name (var_data : Com.Var.t StrMap.t)
    (name : Mast.variable_name Pos.marked) : Com.Var.t =
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

let get_var (var_data : Com.Var.t StrMap.t)
    (name : Mast.variable_name Pos.marked) : Mir.expression =
  Com.Var (get_var_from_name var_data name)

(**{2 Preliminary passes}*)

(**{2 SSA construction}*)

let translate_variable (var_data : Com.Var.t StrMap.t)
    (var : Mast.variable Pos.marked) : Mir.expression Pos.marked =
  match Pos.unmark var with
  | Mast.Normal name ->
      Pos.same_pos_as (get_var var_data (Pos.same_pos_as name var)) var
  | Mast.Generic _ -> assert false

(** {2 Translation of expressions}*)

let rec translate_expression (p : Check_validity.program)
    (var_data : Com.Var.t StrMap.t) (f : Mast.expression Pos.marked) :
    Mir.expression Pos.marked =
  let open Com in
  let expr =
    match Pos.unmark f with
    | TestInSet (positive, e, values) ->
        let new_e = translate_expression p var_data e in
        let new_set_values =
          List.map
            (function
              | FloatValue f -> FloatValue f
              | VarValue (v, pos) ->
                  let new_v =
                    match v with
                    | Mast.Normal name -> StrMap.find name var_data
                    | Mast.Generic _ -> assert false
                  in
                  VarValue (new_v, pos)
              | Interval (bv, ev) -> Interval (bv, ev))
            values
        in
        TestInSet (positive, new_e, new_set_values)
    | Comparison (op, e1, e2) ->
        let new_e1 = translate_expression p var_data e1 in
        let new_e2 = translate_expression p var_data e2 in
        Comparison (op, new_e1, new_e2)
    | Binop (op, e1, e2) ->
        (* if
             Pos.unmark op = Mast.Mul
             && (Pos.unmark e1 = Mast.Literal (Float 0.)
                || Pos.unmark e2 = Mast.Literal (Float 0.))
           then
             (* It is difficult to do a broader or deeper analysis because of
                constant substitutions that could wrongly trigger the warning *)
             Errors.print_spanned_warning
               "Nullifying constant multiplication found." (Pos.get_position f);*)
        let new_e1 = translate_expression p var_data e1 in
        let new_e2 = translate_expression p var_data e2 in
        Binop (op, new_e1, new_e2)
    | Unop (op, e) ->
        let new_e = translate_expression p var_data e in
        Unop (op, new_e)
    | Index (t, i) ->
        let t_var = translate_variable var_data t in
        let new_i = translate_expression p var_data i in
        Index
          ( (match Pos.unmark t_var with
            | Var v -> (v, Pos.get_position f)
            | _ -> assert false (* should not happen *)),
            new_i )
    | Conditional (e1, e2, e3) ->
        let new_e1 = translate_expression p var_data e1 in
        let new_e2 = translate_expression p var_data e2 in
        let new_e3 = Option.map (translate_expression p var_data) e3 in
        Conditional (new_e1, new_e2, new_e3)
    | FuncCall (f_name, args) ->
        let new_args =
          List.map (fun arg -> translate_expression p var_data arg) args
        in
        FuncCall (f_name, new_args)
    | Literal l -> Literal l
    | Var var ->
        let new_var = translate_variable var_data (Pos.same_pos_as var f) in
        Pos.unmark new_var
    | NbCategory cs ->
        NbCategory (Check_validity.mast_to_catvars cs p.prog_var_cats)
    | Attribut (v, a) -> (
        if
          CatVar.Map.fold
            (fun _ CatVar.{ attributs; _ } res ->
              res
              && StrMap.fold
                   (fun attr _ res -> res && attr <> Pos.unmark a)
                   attributs true)
            p.prog_var_cats true
        then Errors.raise_spanned_error "unknown attribut" (Pos.get_position a);
        let v_name =
          match Pos.unmark v with
          | Mast.Normal v_name -> v_name
          | _ -> assert false
        in
        match StrMap.find_opt v_name var_data with
        | Some var -> (
            if Com.Var.is_ref var then Attribut (Pos.same_pos_as var v, a)
            else
              match StrMap.find_opt (Pos.unmark a) (Com.Var.attrs var) with
              | Some l -> Literal (Float (float (Pos.unmark l)))
              | None -> Literal Undefined)
        | _ ->
            let msg = Format.sprintf "unknown variable %s" v_name in
            Errors.raise_spanned_error msg (Pos.get_position v))
    | EventField (e, f, _) ->
        let new_e = translate_expression p var_data e in
        let i = (StrMap.find (Pos.unmark f) p.prog_event_fields).index in
        EventField (new_e, f, i)
    | Size v -> (
        let v_name =
          match Pos.unmark v with
          | Mast.Normal v_name -> v_name
          | _ -> assert false
        in
        let var = StrMap.find v_name var_data in
        if Com.Var.is_ref var then Size (Pos.same_pos_as var v)
        else
          match Com.Var.is_table var with
          | Some i -> Literal (Float (float_of_int i))
          | None -> Literal (Float 1.0))
    | NbAnomalies -> NbAnomalies
    | NbDiscordances -> NbDiscordances
    | NbInformatives -> NbInformatives
    | NbBloquantes -> NbBloquantes
    | FuncCallLoop _ | Loop _ -> assert false
  in
  Pos.same_pos_as expr f

(** {2 Translation of source file items}*)

let rec translate_prog (p : Check_validity.program)
    (var_data : Com.Var.t StrMap.t) (it_depth : int) (itval_depth : int) prog =
  let rec aux res = function
    | [] -> List.rev res
    | (Com.Affectation (SingleFormula decl, _), pos) :: il ->
        let decl' =
          match decl with
          | VarDecl (v, idx, e) ->
              let var =
                match Pos.unmark (translate_variable var_data v) with
                | Com.Var var -> Pos.same_pos_as var v
                | _ -> assert false
                (* should not happen *)
              in
              let idx' = Option.map (translate_expression p var_data) idx in
              let e' = translate_expression p var_data e in
              Com.VarDecl (var, idx', e')
          | EventFieldDecl (idx, f, _, e) ->
              let idx' = translate_expression p var_data idx in
              let i = (StrMap.find (Pos.unmark f) p.prog_event_fields).index in
              let e' = translate_expression p var_data e in
              Com.EventFieldDecl (idx', f, i, e')
        in
        let m_form = (Com.SingleFormula decl', pos) in
        aux ((Com.Affectation m_form, pos) :: res) il
    | (Com.Affectation (MultipleFormulaes _, _), _) :: _ -> assert false
    | (Com.IfThenElse (e, ilt, ile), pos) :: il ->
        let expr = translate_expression p var_data e in
        let prog_then = aux [] ilt in
        let prog_else = aux [] ile in
        aux ((Com.IfThenElse (expr, prog_then, prog_else), pos) :: res) il
    | (Com.WhenDoElse (wdl, ed), pos) :: il ->
        let map_wdl (expr, dl, pos) =
          let expr' = translate_expression p var_data expr in
          let dl' = aux [] dl in
          (expr', dl', pos)
        in
        let wdl' = List.map map_wdl wdl in
        let ed' = Pos.same_pos_as (aux [] (Pos.unmark ed)) ed in
        aux ((Com.WhenDoElse (wdl', ed'), pos) :: res) il
    | (Com.ComputeTarget (tn, targs), pos) :: il ->
        let map v =
          match Pos.unmark (translate_variable var_data v) with
          | Com.Var var -> Pos.same_pos_as var v
          | _ -> assert false
          (* should not happen *)
        in
        let targs' = List.map map targs in
        aux ((Com.ComputeTarget (tn, targs'), pos) :: res) il
    | (Com.VerifBlock instrs, pos) :: il ->
        let instrs' = aux [] instrs in
        aux ((Com.VerifBlock instrs', pos) :: res) il
    | (Com.Print (std, args), pos) :: il ->
        let mir_args =
          List.rev
            (List.fold_left
               (fun res arg ->
                 let mir_arg =
                   match Pos.unmark arg with
                   | Com.PrintString s -> Com.PrintString s
                   | Com.PrintName v -> (
                       let name =
                         match Pos.unmark v with
                         | Mast.Normal name -> name
                         | Mast.Generic _ -> assert false
                       in
                       match StrMap.find_opt name var_data with
                       | Some var ->
                           if Com.Var.is_ref var then
                             Com.PrintName (Pos.same_pos_as var v)
                           else Com.PrintString (Pos.unmark var.name)
                       | _ ->
                           let msg =
                             Format.sprintf "unknown variable %s" name
                           in
                           Errors.raise_spanned_error msg (Pos.get_position v))
                   | Com.PrintAlias v -> (
                       let name =
                         match Pos.unmark v with
                         | Mast.Normal name -> name
                         | Mast.Generic _ -> assert false
                       in
                       match StrMap.find_opt name var_data with
                       | Some var ->
                           if Com.Var.is_ref var then
                             Com.PrintAlias (Pos.same_pos_as var v)
                           else Com.PrintString (Com.Var.alias_str var)
                       | _ ->
                           let msg =
                             Format.sprintf "unknown variable %s" name
                           in
                           Errors.raise_spanned_error msg (Pos.get_position v))
                   | Com.PrintEventName (e, f, _) ->
                       let e' = translate_expression p var_data e in
                       let i =
                         (StrMap.find (Pos.unmark f) p.prog_event_fields).index
                       in
                       Com.PrintEventName (e', f, i)
                   | Com.PrintEventAlias (e, f, _) ->
                       let e' = translate_expression p var_data e in
                       let i =
                         (StrMap.find (Pos.unmark f) p.prog_event_fields).index
                       in
                       Com.PrintEventAlias (e', f, i)
                   | Com.PrintIndent e ->
                       Com.PrintIndent (translate_expression p var_data e)
                   | Com.PrintExpr (e, min, max) ->
                       Com.PrintExpr
                         (translate_expression p var_data e, min, max)
                 in
                 Pos.same_pos_as mir_arg arg :: res)
               [] args)
        in
        aux ((Com.Print (std, mir_args), pos) :: res) il
    | (Com.Iterate (vn, vars, var_params, instrs), pos) :: il ->
        let var_pos = Pos.get_position vn in
        let var_name = Mast.get_normal_var (Pos.unmark vn) in
        (match StrMap.find_opt var_name var_data with
        | Some v ->
            let msg =
              Format.asprintf "variable already declared %a" Pos.format_position
                (Pos.get_position v.name)
            in
            Errors.raise_spanned_error msg pos
        | _ -> ());
        let var = Com.Var.new_ref ~name:(var_name, var_pos) ~loc_int:it_depth in
        let var_data = StrMap.add var_name var var_data in
        let vars' =
          List.map
            (fun vn ->
              Pos.same_pos_as
                (StrMap.find (Mast.get_normal_var (Pos.unmark vn)) var_data)
                vn)
            vars
        in
        let var_params' =
          List.map
            (fun (vcats, expr) ->
              let catSet =
                Check_validity.mast_to_catvars vcats p.prog_var_cats
              in
              let mir_expr = translate_expression p var_data expr in
              (catSet, mir_expr))
            var_params
        in
        let prog_it =
          translate_prog p var_data (it_depth + 1) itval_depth instrs
        in
        let m_var = Pos.same_pos_as var vn in
        aux ((Com.Iterate (m_var, vars', var_params', prog_it), pos) :: res) il
    | (Com.Iterate_values (vn, var_intervals, instrs), pos) :: il ->
        let var_pos = Pos.get_position vn in
        let var_name = Mast.get_normal_var (Pos.unmark vn) in
        (match StrMap.find_opt var_name var_data with
        | Some v ->
            let msg =
              Format.asprintf "variable already declared %a" Pos.format_position
                (Pos.get_position v.name)
            in
            Errors.raise_spanned_error msg pos
        | _ -> ());
        let var =
          Com.Var.new_temp ~name:(var_name, var_pos) ~is_table:None
            ~loc_int:itval_depth
        in
        let var_data = StrMap.add var_name var var_data in
        let var_intervals' =
          List.map
            (fun (e0, e1, step) ->
              let e0' = translate_expression p var_data e0 in
              let e1' = translate_expression p var_data e1 in
              let step' = translate_expression p var_data step in
              (e0', e1', step'))
            var_intervals
        in
        let prog_it =
          translate_prog p var_data it_depth (itval_depth + 1) instrs
        in
        let m_var = Pos.same_pos_as var vn in
        aux
          ((Com.Iterate_values (m_var, var_intervals', prog_it), pos) :: res)
          il
    | (Com.Restore (vars, var_params, evts, instrs), pos) :: il ->
        let vars' =
          List.map
            (fun vn ->
              Pos.same_pos_as
                (StrMap.find (Mast.get_normal_var (Pos.unmark vn)) var_data)
                vn)
            vars
        in
        let var_params' =
          List.map
            (fun (vn, vcats, expr) ->
              let var_pos = Pos.get_position vn in
              let var_name = Mast.get_normal_var (Pos.unmark vn) in
              let var =
                Com.Var.new_ref ~name:(var_name, var_pos) ~loc_int:it_depth
              in
              let var_data = StrMap.add var_name var var_data in
              let catSet =
                Check_validity.mast_to_catvars vcats p.prog_var_cats
              in
              let mir_expr = translate_expression p var_data expr in
              (Pos.mark var_pos var, catSet, mir_expr))
            var_params
        in
        let evts' = List.map (translate_expression p var_data) evts in
        let prog_rest = translate_prog p var_data it_depth itval_depth instrs in
        aux
          ((Com.Restore (vars', var_params', evts', prog_rest), pos) :: res)
          il
    | (Com.ArrangeEvents (sort, filter, instrs), pos) :: il ->
        let sort', itval_depth' =
          match sort with
          | Some (var0, var1, expr) ->
              let var0_pos = Pos.get_position var0 in
              let var0_name = Mast.get_normal_var (Pos.unmark var0) in
              (match StrMap.find_opt var0_name var_data with
              | Some v ->
                  let msg =
                    Format.asprintf "variable already declared %a"
                      Pos.format_position (Pos.get_position v.name)
                  in
                  Errors.raise_spanned_error msg pos
              | _ -> ());
              let var0' =
                Com.Var.new_temp ~name:(var0_name, var0_pos) ~is_table:None
                  ~loc_int:itval_depth
              in
              let var1_pos = Pos.get_position var1 in
              let var1_name = Mast.get_normal_var (Pos.unmark var1) in
              (match StrMap.find_opt var1_name var_data with
              | Some v ->
                  let msg =
                    Format.asprintf "variable already declared %a"
                      Pos.format_position (Pos.get_position v.name)
                  in
                  Errors.raise_spanned_error msg pos
              | _ -> ());
              let var1' =
                Com.Var.new_temp ~name:(var1_name, var1_pos) ~is_table:None
                  ~loc_int:(itval_depth + 1)
              in
              let var_data =
                var_data |> StrMap.add var0_name var0'
                |> StrMap.add var1_name var1'
              in
              let m_var0 = Pos.same_pos_as var0' var0 in
              let m_var1 = Pos.same_pos_as var1' var1 in
              let expr' = translate_expression p var_data expr in
              (Some (m_var0, m_var1, expr'), itval_depth + 2)
          | None -> (None, itval_depth)
        in
        let filter', itval_depth' =
          match filter with
          | Some (var, expr) ->
              let var_pos = Pos.get_position var in
              let var_name = Mast.get_normal_var (Pos.unmark var) in
              (match StrMap.find_opt var_name var_data with
              | Some v ->
                  let msg =
                    Format.asprintf "variable already declared %a"
                      Pos.format_position (Pos.get_position v.name)
                  in
                  Errors.raise_spanned_error msg pos
              | _ -> ());
              let var' =
                Com.Var.new_temp ~name:(var_name, var_pos) ~is_table:None
                  ~loc_int:itval_depth
              in
              let var_data = StrMap.add var_name var' var_data in
              let m_var = Pos.same_pos_as var' var in
              let expr' = translate_expression p var_data expr in
              (Some (m_var, expr'), max itval_depth' (itval_depth + 1))
          | None -> (None, itval_depth')
        in
        let instrs' = translate_prog p var_data it_depth itval_depth' instrs in
        aux ((Com.ArrangeEvents (sort', filter', instrs'), pos) :: res) il
    | (Com.RaiseError (err_name, var_opt), pos) :: il ->
        let err_decl = StrMap.find (Pos.unmark err_name) p.prog_errors in
        let m_err_decl = Pos.same_pos_as err_decl err_name in
        aux ((Com.RaiseError (m_err_decl, var_opt), pos) :: res) il
    | (Com.CleanErrors, pos) :: il -> aux ((Com.CleanErrors, pos) :: res) il
    | (Com.ExportErrors, pos) :: il -> aux ((Com.ExportErrors, pos) :: res) il
    | (Com.FinalizeErrors, pos) :: il ->
        aux ((Com.FinalizeErrors, pos) :: res) il
    | (Com.ComputeDomain _, _) :: _
    | (Com.ComputeChaining _, _) :: _
    | (Com.ComputeVerifs (_, _), _) :: _ ->
        assert false
  in
  aux [] prog

let get_targets (is_function : bool) (p : Check_validity.program)
    (var_data : Com.Var.t StrMap.t) (ts : Mast.target StrMap.t) :
    Mir.target_data Com.TargetMap.t =
  StrMap.fold
    (fun _ (t : Mast.target) targets ->
      let target_name = t.target_name in
      let target_file = t.target_file in
      let target_apps = t.target_apps in
      let target_nb_refs = t.target_nb_refs in
      let tmp_var_data, _ =
        if is_function then
          List.fold_left
            (fun (tmp_var_data, n) (name, pos) ->
              let var = Com.Var.new_arg ~name:(name, pos) ~loc_int:n in
              let tmp_var_data = StrMap.add name var tmp_var_data in
              (tmp_var_data, n + 1))
            (var_data, 0) t.target_args
        else
          List.fold_left
            (fun (tmp_var_data, n) (name, pos) ->
              let var = Com.Var.new_ref ~name:(name, pos) ~loc_int:n in
              let tmp_var_data = StrMap.add name var tmp_var_data in
              (tmp_var_data, n + 1))
            (var_data, -target_nb_refs)
            t.target_args
      in
      let target_sz_tmps = t.target_sz_tmps in
      let tmp_var_data, itval_depth =
        StrMap.fold
          (fun name ((_, pos), size) (tmp_var_data, n) ->
            let size' = Pos.unmark_option (Mast.get_table_size_opt size) in
            let var =
              Com.Var.new_temp ~name:(name, pos) ~is_table:size' ~loc_int:n
            in
            let tmp_var_data = StrMap.add name var tmp_var_data in
            (tmp_var_data, n + Com.Var.size var))
          t.target_tmp_vars
          (tmp_var_data, -target_sz_tmps)
      in
      let tmp_var_data =
        if is_function then
          let vn, vpos = Option.get t.target_result in
          let var = Com.Var.new_res ~name:(vn, vpos) in
          StrMap.add vn var tmp_var_data
        else tmp_var_data
      in
      let target_args =
        List.map
          (fun (vn, pos) -> (StrMap.find vn tmp_var_data, pos))
          t.target_args
      in
      let target_tmp_vars =
        StrMap.mapi
          (fun vn ((_, pos), size) ->
            let var = StrMap.find vn tmp_var_data in
            let size' = Pos.unmark_option (Mast.get_table_size_opt size) in
            (var, pos, size'))
          t.target_tmp_vars
      in
      let target_result =
        match t.target_result with
        | Some (vn, vpos) -> Some (StrMap.find vn tmp_var_data, vpos)
        | None -> None
      in
      let target_prog =
        translate_prog p tmp_var_data
          (List.length target_args - target_nb_refs)
          itval_depth t.target_prog
      in
      let target_data =
        Mir.
          {
            target_name;
            target_file;
            target_apps;
            target_args;
            target_result;
            target_tmp_vars;
            target_prog;
            target_nb_tmps = t.target_nb_tmps;
            target_sz_tmps;
            target_nb_refs;
          }
      in
      Com.TargetMap.add (Pos.unmark target_name) target_data targets)
    ts Com.TargetMap.empty

let translate (p : Mast.program) (main_target : string) : Mir.program =
  let p = Expand_macros.proceed p in
  let prog = Check_validity.proceed p main_target in
  let prog_functions = prog.prog_functions in
  let prog_targets = prog.prog_targets in
  let var_category_map = prog.prog_var_cats in
  let var_data = prog.prog_vars in
  let rules =
    let map_rule (rule : Check_validity.rule) =
      let id = Pos.unmark rule.rule_id in
      Format.sprintf "%s_regle_%d" prog.prog_prefix id
    in
    IntMap.map map_rule prog.prog_rules
  in
  let verifs =
    let map_verif (verif : Check_validity.verif) =
      let id = Pos.unmark verif.verif_id in
      Format.sprintf "%s_verif_%d" prog.prog_prefix id
    in
    IntMap.map map_verif prog.prog_verifs
  in
  let chainings =
    let map_chainings (chaining : Check_validity.chaining) =
      let name = Pos.unmark chaining.chain_name in
      Format.sprintf "%s_chaining_%s" prog.prog_prefix name
    in
    StrMap.map map_chainings prog.prog_chainings
  in
  let errs = prog.prog_errors in
  let functions = get_targets true prog var_data prog_functions in
  let targets = get_targets false prog var_data prog_targets in
  Mir.
    {
      program_safe_prefix = prog.prog_prefix;
      program_applications = prog.prog_apps;
      program_var_categories = var_category_map;
      program_rule_domains = prog.prog_rdoms;
      program_verif_domains = prog.prog_vdoms;
      program_vars = var_data;
      program_alias = prog.prog_alias;
      program_event_fields = prog.prog_event_fields;
      program_event_field_idxs = prog.prog_event_field_idxs;
      program_rules = rules;
      program_verifs = verifs;
      program_chainings = chainings;
      program_errors = errs;
      program_functions = functions;
      program_targets = targets;
      program_main_target = prog.prog_main_target;
      program_stats = prog.prog_stats;
    }
