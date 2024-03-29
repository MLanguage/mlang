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

type var_decl_data = {
  var_decl_typ : Mast.value_typ option;
  var_decl_is_table : int option;
  var_decl_descr : string option;
  var_pos : Pos.t;
}
(** Intermediate container for variable declaration info *)

(** {2 General translation context} *)

type translating_context = {
  table_definition : bool;
      (** [true] if translating an expression susceptible to contain a generic
          table index *)
  idmap : Mir.idmap;  (** Current string-to-{!type: Mir.Variable.t} mapping *)
}
(** This context will be passed along during the translation *)

let get_var_from_name (d : Mir.Variable.t Pos.VarNameToID.t)
    (name : Mast.variable_name Pos.marked) : Mir.Variable.t =
  try Pos.VarNameToID.find (Pos.unmark name) d
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

let get_var (d : Mir.Variable.t Pos.VarNameToID.t)
    (name : Mast.variable_name Pos.marked) : Mir.expression =
  Mir.Var (get_var_from_name d name)

(**{2 Preliminary passes}*)

(* hackish way to ignore M rules bound to out-of-scope applications *)
let belongs_to_iliad_app (r : Mast.application Pos.marked list) : bool =
  List.exists (fun app -> Pos.unmark app = "iliad") r

let sort_attributes (attrs : Mast.variable_attribute list) =
  List.sort
    (fun c1 c2 -> String.compare (Pos.unmark (fst c1)) (Pos.unmark (fst c2)))
    attrs

let get_var_categories (p : Mast.program) =
  let categories =
    List.fold_left
      (fun decls source_file ->
        List.fold_left
          (fun decls source_file_item ->
            match Pos.unmark source_file_item with
            | Mast.VarCatDecl (catdecl, pos) ->
                let normalized_decl =
                  {
                    catdecl with
                    var_category =
                      List.sort
                        (fun c1 c2 ->
                          String.compare (Pos.unmark c1) (Pos.unmark c2))
                        catdecl.var_category;
                    var_attributes =
                      List.sort
                        (fun c1 c2 ->
                          String.compare (Pos.unmark c1) (Pos.unmark c2))
                        catdecl.var_attributes;
                  }
                in
                let already_defined =
                  let decl_l = List.length normalized_decl.var_category in
                  List.find_opt
                    (fun (decl, _pos) ->
                      decl_l = List.length decl.Mast.var_category
                      && List.for_all2
                           (fun a b ->
                             String.equal (Pos.unmark a) (Pos.unmark b))
                           normalized_decl.var_category decl.Mast.var_category)
                    decls
                in
                begin
                  match already_defined with
                  | None -> ()
                  | Some (_decl, posDecl) ->
                      Errors.raise_spanned_error
                        (Format.asprintf
                           "Category \"%s\" defined more than once:@;\
                            Already defined %a"
                           (String.concat " "
                              (Format_mast.format_var_type
                                 normalized_decl.var_type
                              :: List.map Pos.unmark
                                   normalized_decl.var_category))
                           Pos.format_position posDecl)
                        pos
                end;
                (normalized_decl, pos) :: decls
            | _ -> decls)
          decls source_file)
      [] p
  in
  let categories =
    (* Sorted to match longest category first *)
    List.sort
      (fun c1 c2 ->
        compare
          (List.length (Pos.unmark c2).Mast.var_category)
          (List.length (Pos.unmark c1).Mast.var_category))
      categories
  in
  categories

let check_var_category (categories : Mast.var_category_decl Pos.marked list)
    (var : Mast.variable_decl) =
  let rec category_included_in cbase ctest =
    (* assume sorted lists *)
    match (cbase, ctest) with
    | [], _ -> true
    | _, [] -> false
    | chb :: ctb, cht :: ctt ->
        if String.equal chb cht then category_included_in ctb ctt
        else
          (* Allows variables to have more tags than the declared category.
             Since the declaration are sorted by tag number, we will match the
             most precise one first. We can however have have two declared
             categories that fits but are not included in one another. *)
          category_included_in cbase ctt
  in
  let attributes_triaging abase atest =
    let rec aux (missing, surplus) abase atest =
      match (abase, atest) with
      | [], _ ->
          (missing, List.map (fun a -> Pos.unmark (fst a)) atest @ surplus)
      | _, [] -> (List.map (fun a -> Pos.unmark a) abase @ missing, surplus)
      | ahb :: atb, aht :: att ->
          let ahb = Pos.unmark ahb in
          let aht = Pos.unmark (fst aht) in
          let comp = String.compare ahb aht in
          if comp = 0 then aux (missing, surplus) atb att
          else if comp < 0 then aux (ahb :: missing, surplus) atb atest
          else aux (missing, aht :: surplus) abase att
    in
    aux ([], []) abase atest
  in
  let var_name, var_pos, var_typ, var_cat, var_attrs =
    match var with
    | Mast.ConstVar _ -> assert false
    | Mast.ComputedVar v ->
        let v = Pos.unmark v in
        ( Pos.unmark v.comp_name,
          snd v.comp_name,
          Mast.Computed,
          v.comp_category,
          v.comp_attributes )
    | Mast.InputVar v ->
        let v = Pos.unmark v in
        ( Pos.unmark v.input_name,
          snd v.input_name,
          Mast.Input,
          v.input_category,
          v.input_attributes )
  in
  let var_cat = List.map Pos.unmark var_cat in
  let categories =
    List.filter (fun cat -> (Pos.unmark cat).Mast.var_type = var_typ) categories
  in
  let var_cat = List.sort String.compare var_cat in
  let var_attrs = sort_attributes var_attrs in
  match
    List.find_all
      (fun cat ->
        category_included_in
          (List.map Pos.unmark (Pos.unmark cat).Mast.var_category)
          var_cat)
      categories
  with
  | [] ->
      Errors.raise_spanned_error
        "Variable does not fit in any declared categories." var_pos
  | [ cat ] ->
      let missing, surplus =
        attributes_triaging (Pos.unmark cat).var_attributes var_attrs
      in
      if missing <> [] then
        Errors.raise_spanned_error
          (Format.sprintf
             "Variable %s (category %s) is missing the following attributes: %s"
             var_name
             (String.concat " " var_cat)
             (String.concat " " missing))
          var_pos;
      if surplus <> [] then
        Errors.raise_spanned_error
          (Format.sprintf
             "Variable %s (category %s) has some unexpected attributes: %s"
             var_name
             (String.concat " " var_cat)
             (String.concat " " surplus))
          var_pos
  | multiple_cats ->
      Errors.raise_spanned_error
        (Format.asprintf "Variable fits more than one category:@\n%a"
           (Format.pp_print_list ~pp_sep:Format.pp_print_newline (fun fmt cat ->
                Format.fprintf fmt "- %s"
                  (String.concat " "
                     (List.map Pos.unmark (Pos.unmark cat).Mast.var_category))))
           multiple_cats)
        var_pos

(** Retrieves variable declaration data. Done in a separate pass because we
    don't want to deal with sorting the dependencies between files or inside
    files. *)
let get_variables_decl (p : Mast.program)
    (categories : Mast.var_category_decl Pos.marked list) :
    var_decl_data Mir.VariableMap.t * Mir.Error.t list * Mir.idmap =
  let vars, idmap, errors, out_list =
    List.fold_left
      (fun (vars, (idmap : Mir.idmap), errors, out_list) source_file ->
        List.fold_left
          (fun (vars, (idmap : Mir.idmap), errors, out_list) source_file_item ->
            match Pos.unmark source_file_item with
            | Mast.VariableDecl var_decl -> (
                match var_decl with
                | Mast.ConstVar (_, _) ->
                    (vars, idmap, errors, out_list) (* already treated before *)
                | Mast.ComputedVar _ | Mast.InputVar _ -> (
                    check_var_category categories var_decl;
                    let var_name =
                      match var_decl with
                      | Mast.ComputedVar v -> (Pos.unmark v).Mast.comp_name
                      | Mast.InputVar v -> (Pos.unmark v).Mast.input_name
                      | Mast.ConstVar _ -> assert false
                    in
                    (* First we check if the variable has not been declared a
                       first time *)
                    try
                      let old_pos =
                        Pos.get_position
                          (Pos.VarNameToID.find (Pos.unmark var_name) idmap)
                            .Mir.Variable.name
                      in
                      Cli.var_info_print
                        "Dropping declaration of %s %a because variable was \
                         previously defined %a"
                        (Pos.unmark var_name) Pos.format_position
                        (Pos.get_position var_name)
                        Pos.format_position old_pos;
                      (vars, idmap, errors, out_list)
                    with Not_found -> (
                      match var_decl with
                      | Mast.ComputedVar cvar ->
                          let cvar = Pos.unmark cvar in
                          let cat =
                            let comp_set =
                              List.fold_left
                                (fun res (str, pos) ->
                                  let elt =
                                    match str with
                                    | "base" -> Mir.Base
                                    | "restituee" -> Mir.GivenBack
                                    | _ ->
                                        Errors.raise_spanned_error
                                          "unknown computed category (must be \
                                           \"base\" or \"restituee\")"
                                          pos
                                  in
                                  Mir.CatCompSet.add elt res)
                                Mir.CatCompSet.empty cvar.comp_category
                            in
                            Mir.CatComputed comp_set
                          in
                          let new_var =
                            Mir.Variable.new_var cvar.Mast.comp_name None
                              cvar.Mast.comp_description
                              ~attributes:cvar.comp_attributes ~cats:(Some cat)
                              ~origin:None
                              ~is_table:
                                (Pos.unmark_option
                                   (Mast.get_table_size_opt cvar.Mast.comp_table))
                              ~is_temp:false ~is_it:false
                          in
                          let new_var_data =
                            {
                              var_decl_typ =
                                Pos.unmark_option cvar.Mast.comp_typ;
                              var_decl_is_table =
                                Pos.unmark_option
                                  (Mast.get_table_size_opt cvar.Mast.comp_table);
                              var_decl_descr =
                                Some (Pos.unmark cvar.Mast.comp_description);
                              var_pos = Pos.get_position source_file_item;
                            }
                          in
                          let new_vars =
                            Mir.VariableMap.add new_var new_var_data vars
                          in
                          let new_idmap =
                            Pos.VarNameToID.add
                              (Pos.unmark cvar.Mast.comp_name)
                              new_var idmap
                          in
                          let new_out_list =
                            if cvar.Mast.comp_is_givenback then
                              cvar.Mast.comp_name :: out_list
                            else out_list
                          in
                          (new_vars, new_idmap, errors, new_out_list)
                      | Mast.InputVar ivar ->
                          let ivar = Pos.unmark ivar in
                          let cat =
                            let input_set =
                              List.fold_left
                                (fun res (str, _pos) -> StrSet.add str res)
                                StrSet.empty ivar.input_category
                            in
                            Mir.CatInput input_set
                          in
                          let new_var =
                            Mir.Variable.new_var ivar.Mast.input_name
                              (Some (Pos.unmark ivar.Mast.input_alias))
                              ivar.Mast.input_description
                              ~attributes:ivar.input_attributes ~origin:None
                              ~cats:(Some cat) ~is_table:None ~is_temp:false
                              ~is_it:false
                            (* Input variables also have a low order *)
                          in
                          let new_var_data =
                            {
                              var_decl_typ =
                                Pos.unmark_option ivar.Mast.input_typ;
                              var_decl_is_table = None;
                              var_decl_descr =
                                Some (Pos.unmark ivar.Mast.input_description);
                              var_pos = Pos.get_position source_file_item;
                            }
                          in
                          let new_vars =
                            Mir.VariableMap.add new_var new_var_data vars
                          in
                          let new_idmap =
                            Pos.VarNameToID.add
                              (Pos.unmark ivar.Mast.input_name)
                              new_var idmap
                          in
                          (new_vars, new_idmap, errors, out_list)
                      | Mast.ConstVar _ -> assert false)))
            | Mast.Output out_name -> (vars, idmap, errors, out_name :: out_list)
            | Mast.Error err ->
                let err =
                  Mir.Error.new_error err.Mast.error_name err
                    (Pos.unmark err.error_typ)
                in
                (vars, idmap, err :: errors, out_list)
            | _ -> (vars, idmap, errors, out_list))
          (vars, idmap, errors, out_list)
          source_file)
      (Mir.VariableMap.empty, Pos.VarNameToID.empty, [], [])
      p
  in
  let vars : var_decl_data Mir.VariableMap.t =
    List.fold_left
      (fun vars out_name ->
        try
          let out_var = get_var_from_name idmap out_name in
          let data = Mir.VariableMap.find out_var vars in
          Mir.VariableMap.add out_var data vars
        with Not_found -> assert false
        (* should not happen *))
      vars out_list
  in
  (vars, errors, idmap)

(**{2 SSA construction}*)

let translate_variable (ctx : translating_context)
    (var : Mast.variable Pos.marked) : Mir.expression Pos.marked =
  match Pos.unmark var with
  | Mast.Normal name ->
      Pos.same_pos_as (get_var ctx.idmap (Pos.same_pos_as name var)) var
  | Mast.Generic _ -> assert false

(** Linear pass that fills [idmap] with all the variable assignments along with
    their execution number. *)
let get_var_redefinitions (p : Mast.program) (idmap : Mir.idmap) : Mir.idmap =
  let idmap =
    List.fold_left
      (fun (idmap : Mir.idmap) source_file ->
        List.fold_left
          (fun (idmap : Mir.idmap) source_file_item ->
            match Pos.unmark source_file_item with
            | Mast.Rule r ->
                if not (belongs_to_iliad_app r.Mast.rule_applications) then
                  idmap
                else
                  fst
                    (List.fold_left
                       (fun (idmap, seq_number) formula ->
                         let ctx = { idmap; table_definition = false } in
                         match Pos.unmark formula with
                         | Mast.SingleFormula f ->
                             let lvar =
                               match
                                 Pos.unmark
                                   (translate_variable ctx
                                      (Pos.unmark f.Mast.lvalue).Mast.var)
                               with
                               | Mir.Var var -> var
                               | _ -> assert false
                               (* should not happen *)
                             in
                             let new_idmap =
                               Pos.VarNameToID.add
                                 (Pos.unmark lvar.Mir.Variable.name)
                                 (Pos.VarNameToID.find
                                    (Pos.unmark lvar.Mir.Variable.name)
                                    idmap)
                                 idmap
                             in
                             (new_idmap, seq_number + 1)
                         | Mast.MultipleFormulaes _ -> assert false)
                       (idmap, 0) r.Mast.rule_formulaes)
            | _ -> idmap)
          idmap source_file)
      (idmap : Mir.idmap)
      p
  in
  idmap

(** {2 Translation of expressions}*)

let translate_table_index (ctx : translating_context)
    (i : Mast.table_index Pos.marked) : Mir.expression Pos.marked =
  match Pos.unmark i with
  | Mast.LiteralIndex i' ->
      Pos.same_pos_as (Mir.Literal (Mir.Float (float_of_int i'))) i
  | Mast.SymbolIndex v ->
      let var = translate_variable ctx (Pos.same_pos_as v i) in
      var

(** Only accepts functions in {!type: Mir.func}*)
let translate_function_name (f_name : string Pos.marked) =
  match Pos.unmark f_name with
  | "somme" -> Mir.SumFunc
  | "min" -> Mir.MinFunc
  | "max" -> Mir.MaxFunc
  | "abs" -> Mir.AbsFunc
  | "positif" -> Mir.GtzFunc
  | "positif_ou_nul" -> Mir.GtezFunc
  | "null" -> Mir.NullFunc
  | "arr" -> Mir.ArrFunc
  | "inf" -> Mir.InfFunc
  | "present" -> Mir.PresentFunc
  | "multimax" -> Mir.Multimax
  | "supzero" -> Mir.Supzero
  | "numero_verif" -> Mir.VerifNumber
  | "numero_compl" -> Mir.ComplNumber
  | x ->
      Errors.raise_spanned_error
        (Format.asprintf "unknown function %s" x)
        (Pos.get_position f_name)

let rec translate_expression (cats : Mir.cat_variable_data Mir.CatVarMap.t)
    (idmap : Mir.idmap) (ctx : translating_context)
    (f : Mast.expression Pos.marked) : Mir.expression Pos.marked =
  let expr =
    match Pos.unmark f with
    | Mast.TestInSet (positive, e, values) ->
        let new_e = translate_expression cats idmap ctx e in
        let local_var = Mir.LocalVariable.new_var () in
        let local_var_expr = Mir.LocalVar local_var in
        let or_chain =
          List.fold_left
            (fun or_chain set_value ->
              let equal_test =
                match set_value with
                | Mast.VarValue set_var ->
                    Mir.Comparison
                      ( Pos.same_pos_as Mast.Eq set_var,
                        Pos.same_pos_as local_var_expr e,
                        translate_variable ctx set_var )
                | Mast.FloatValue i ->
                    Mir.Comparison
                      ( Pos.same_pos_as Mast.Eq i,
                        Pos.same_pos_as local_var_expr e,
                        Pos.same_pos_as
                          (Mir.Literal (Mir.Float (Pos.unmark i)))
                          i )
                | Mast.Interval (bn, en) ->
                    if Pos.unmark bn > Pos.unmark en then
                      Errors.raise_spanned_error "wrong interval bounds"
                        (Pos.get_position bn)
                    else
                      Mir.Binop
                        ( Pos.same_pos_as Mast.And bn,
                          Pos.same_pos_as
                            (Mir.Comparison
                               ( Pos.same_pos_as Mast.Gte bn,
                                 Pos.same_pos_as local_var_expr e,
                                 Pos.same_pos_as
                                   (Mir.Literal
                                      (Mir.Float (float_of_int (Pos.unmark bn))))
                                   bn ))
                            bn,
                          Pos.same_pos_as
                            (Mir.Comparison
                               ( Pos.same_pos_as Mast.Lte en,
                                 Pos.same_pos_as local_var_expr e,
                                 Pos.same_pos_as
                                   (Mir.Literal
                                      (Mir.Float (float_of_int (Pos.unmark en))))
                                   en ))
                            en )
              in
              Pos.same_pos_as
                (Mir.Binop
                   ( Pos.same_pos_as Mast.Or f,
                     or_chain,
                     Pos.same_pos_as equal_test f ))
                f)
            (Pos.same_pos_as (Mir.Literal Mir.Undefined) f)
            values
        in
        let or_chain =
          if not positive then
            Pos.same_pos_as (Mir.Unop (Mast.Not, or_chain)) or_chain
          else or_chain
        in
        Mir.LocalLet (local_var, new_e, or_chain)
    | Mast.Comparison (op, e1, e2) ->
        let new_e1 = translate_expression cats idmap ctx e1 in
        let new_e2 = translate_expression cats idmap ctx e2 in
        Mir.Comparison (op, new_e1, new_e2)
    | Mast.Binop (op, e1, e2) ->
        if
          Pos.unmark op = Mast.Mul
          && (Pos.unmark e1 = Mast.Literal (Float 0.)
             || Pos.unmark e2 = Mast.Literal (Float 0.))
        then
          (* It is difficult to do a broader or deeper analysis because of
             constant substitutions that could wrongly trigger the warning *)
          Errors.print_spanned_warning
            "Nullifying constant multiplication found." (Pos.get_position f);
        let new_e1 = translate_expression cats idmap ctx e1 in
        let new_e2 = translate_expression cats idmap ctx e2 in
        Mir.Binop (op, new_e1, new_e2)
    | Mast.Unop (op, e) ->
        let new_e = translate_expression cats idmap ctx e in
        Mir.Unop (op, new_e)
    | Mast.Index (t, i) ->
        let t_var = translate_variable ctx t in
        let new_i = translate_table_index ctx i in
        Mir.Index
          ( (match Pos.unmark t_var with
            | Mir.Var v -> Pos.same_pos_as v t_var
            | _ -> assert false (* should not happen *)),
            new_i )
    | Mast.Conditional (e1, e2, e3) ->
        let new_e1 = translate_expression cats idmap ctx e1 in
        let new_e2 = translate_expression cats idmap ctx e2 in
        let new_e3 =
          match e3 with
          | Some e3 -> translate_expression cats idmap ctx e3
          | None -> Pos.same_pos_as (Mir.Literal Mir.Undefined) e2
          (* the absence of a else branch for a ternary operators can yield an
             undefined term *)
        in
        Mir.Conditional (new_e1, new_e2, new_e3)
    | Mast.FunctionCall (f_name, args) ->
        let f_correct = translate_function_name f_name in
        let new_args = translate_func_args cats idmap ctx args in
        Mir.FunctionCall (f_correct, new_args)
    | Mast.Literal l -> (
        match l with
        | Mast.Variable var ->
            let new_var = translate_variable ctx (Pos.same_pos_as var f) in
            Pos.unmark new_var
        | Mast.Float f -> Mir.Literal (Mir.Float f)
        | Mast.Undefined -> Mir.Literal Mir.Undefined)
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
        match Pos.VarNameToID.find_opt v_name idmap with
        | Some var -> (
            if var.is_it then Mir.Attribut (Pos.same_pos_as v_name v, var, a)
            else
              match
                List.find_opt
                  (fun (attr, _) -> Pos.unmark a = Pos.unmark attr)
                  var.attributes
              with
              | Some (_, l) -> Mir.Literal (Mir.Float (float (Pos.unmark l)))
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
        match Pos.VarNameToID.find_opt v_name idmap with
        | Some var -> (
            if var.is_it then Mir.Size var
            else
              match var.is_table with
              | Some i -> Mir.Literal (Mir.Float (float_of_int i))
              | None -> Mir.Literal (Mir.Float 1.0))
        | _ ->
            let msg = Format.sprintf "unknown variable %s" v_name in
            Errors.raise_spanned_error msg (Pos.get_position v))
    | Mast.NbAnomalies -> Mir.NbAnomalies
    | Mast.NbDiscordances -> Mir.NbDiscordances
    | Mast.NbInformatives -> Mir.NbInformatives
    | Mast.NbBloquantes -> Mir.NbBloquantes
    | Mast.Loop _ -> assert false
  in
  Pos.same_pos_as expr f

(** Mutually recursive with {!val: translate_expression} *)
and translate_func_args (cats : Mir.cat_variable_data Mir.CatVarMap.t)
    (idmap : Mir.idmap) (ctx : translating_context) (args : Mast.func_args) :
    Mir.expression Pos.marked list =
  match args with
  | Mast.ArgList args ->
      List.map (fun arg -> translate_expression cats idmap ctx arg) args
  | Mast.LoopList _ -> assert false

(** {2 Translation of source file items}*)

(** Helper type to indicate the kind of variable assignment *)
type index_def = NoIndex | ConstIndex of int | DynamicIndex of Mir.variable

let translate_index_def (ctx : translating_context)
    ((v, pos) : Mast.variable Pos.marked) : translating_context * index_def =
  match translate_variable ctx (v, pos) with
  | Mir.Var v, _ -> (ctx, DynamicIndex v)
  | _ -> assert false

(** Translates lvalues into the assigning variable as well as the type of
    assignment *)
let translate_lvalue (ctx : translating_context) (lval : Mast.lvalue Pos.marked)
    : translating_context * Mir.Variable.t * index_def =
  let var =
    match Pos.unmark (translate_variable ctx (Pos.unmark lval).Mast.var) with
    | Mir.Var (var : Mir.Variable.t) -> var
    | _ -> assert false
    (* should not happen *)
  in
  match (Pos.unmark lval).Mast.index with
  | Some ti -> (
      match Pos.unmark ti with
      | Mast.LiteralIndex i -> (ctx, var, ConstIndex i)
      | Mast.SymbolIndex (Mast.Normal _ as v) ->
          let ctx, index_def = translate_index_def ctx (Pos.same_pos_as v ti) in
          (ctx, var, index_def)
      | Mast.SymbolIndex (Mast.Generic _) -> assert false)
  | None -> (ctx, var, NoIndex)

(** Date types are not supported *)
let translate_value_typ (typ : Mast.value_typ Pos.marked option) :
    Mir.typ option =
  match typ with
  | Some (Mast.Boolean, _) -> Some Mir.Real
  (* Indeed, the BOOLEEN annotations are useless because they feed it to
     functions that expect reals *)
  | Some (Mast.Real, _) -> Some Mir.Real
  | Some (_, _) -> Some Mir.Real
  | None -> None

let create_var_def (var_lvalue : Mir.Variable.t)
    (var_expr : Mir.expression Pos.marked) (def_kind : index_def)
    (var_decl_data : var_decl_data Mir.VariableMap.t) (idmap : Mir.idmap) :
    Mir.variable_data =
  let var_at_declaration =
    Pos.VarNameToID.find (Pos.unmark var_lvalue.name) idmap
  in
  let decl_data =
    try Mir.VariableMap.find var_at_declaration var_decl_data
    with Not_found -> assert false
    (* should not happen *)
  in
  let var_typ =
    translate_value_typ
      (Option.map (fun x -> (x, decl_data.var_pos)) decl_data.var_decl_typ)
  in
  match decl_data.var_decl_is_table with
  | Some size -> (
      match def_kind with
      | NoIndex -> assert false (* should not happen *)
      | ConstIndex i ->
          {
            Mir.var_definition =
              Mir.TableVar
                (size, Mir.IndexTable (Mir.IndexMap.singleton i var_expr));
            Mir.var_typ;
          }
      | DynamicIndex v ->
          {
            Mir.var_definition =
              Mir.TableVar (size, Mir.IndexGeneric (v, var_expr));
            Mir.var_typ;
          })
  | None ->
      if def_kind = NoIndex then
        { Mir.var_definition = Mir.SimpleVar var_expr; Mir.var_typ }
      else
        Errors.raise_multispanned_error
          (Format.asprintf
             "variable %s is defined as a table but has been declared as a \
              non-table"
             (Pos.unmark var_lvalue.Mir.Variable.name))
          [
            (Some "variable definition", Pos.get_position var_expr);
            ( Some "variable declaration",
              try (Mir.VariableMap.find var_lvalue var_decl_data).var_pos
              with Not_found -> assert false
              (* should not happen since we already looked into idmap to get the
                 var value from its name *) );
          ]

(** At this point [var_data] contains the definition data for all the times a
    variable is defined. However the M language deals with undefined variable,
    so for each variable we have to insert a dummy definition corresponding to
    the declaration and whose value and whose value is found in the TGV at the
    beginning of the execution *)
let add_dummy_definitions_for_variable_declarations
    (var_data : Mir.variable_data Mir.VariableMap.t)
    (var_decl_data : var_decl_data Mir.VariableMap.t) :
    Mir.variable_data Mir.VariableMap.t =
  Mir.VariableMap.fold
    (fun var decl (var_data : Mir.variable_data Mir.VariableMap.t) ->
      Mir.VariableMap.add var
        {
          Mir.var_definition = Mir.InputVar;
          Mir.var_typ =
            translate_value_typ
              (match decl.var_decl_typ with
              | None -> None
              | Some typ -> Some (Pos.same_pos_as typ var.name));
        }
        var_data)
    var_decl_data var_data

let rec translate_prog (error_decls : Mir.Error.t list)
    (cats : Mir.cat_variable_data Mir.CatVarMap.t)
    (var_data : Mir.VariableDict.t) idmap var_decl_data prog =
  let new_ctx = { idmap; table_definition = false } in
  let rec aux (res, var_data) = function
    | [] -> (List.rev res, var_data)
    | (Mast.Formula f, pos) :: il -> begin
        let ctx = new_ctx in
        match f with
        | Mast.SingleFormula sf, _ ->
            let ctx, var_lvalue, def_kind =
              translate_lvalue ctx sf.Mast.lvalue
            in
            let var_e = translate_expression cats idmap ctx sf.Mast.formula in
            let var_d =
              create_var_def var_lvalue var_e def_kind var_decl_data idmap
            in
            aux
              ( (Mir.Affectation (var_lvalue.Mir.Variable.id, var_d), pos) :: res,
                var_data )
              il
        | Mast.MultipleFormulaes _, _ -> assert false
      end
    | (Mast.IfThenElse (e, ilt, ile), pos) :: il ->
        let ctx = new_ctx in
        let expr, _ = translate_expression cats idmap ctx e in
        let prog_then, var_data = aux ([], var_data) ilt in
        let prog_else, var_data = aux ([], var_data) ile in
        aux
          ((Mir.IfThenElse (expr, prog_then, prog_else), pos) :: res, var_data)
          il
    | (Mast.ComputeTarget tn, pos) :: il ->
        aux ((Mir.ComputeTarget tn, pos) :: res, var_data) il
    | (Mast.VerifBlock instrs, pos) :: il ->
        let instrs', var_data = aux ([], var_data) instrs in
        aux ((Mir.VerifBlock instrs', pos) :: res, var_data) il
    | (Mast.Print (std, args), pos) :: il ->
        let ctx = new_ctx in
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
                       match Pos.VarNameToID.find_opt name idmap with
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
                       match Pos.VarNameToID.find_opt name idmap with
                       | Some var ->
                           if var.is_it then
                             Mir.PrintAlias (Pos.same_pos_as name v, var)
                           else
                             Mir.PrintString
                               (match var.alias with Some a -> a | None -> "")
                       | _ ->
                           let msg =
                             Format.sprintf "unknown variable %s" name
                           in
                           Errors.raise_spanned_error msg (Pos.get_position v))
                   | Mast.PrintIndent e ->
                       Mir.PrintIndent (translate_expression cats idmap ctx e)
                   | Mast.PrintExpr (e, min, max) ->
                       Mir.PrintExpr
                         (translate_expression cats idmap ctx e, min, max)
                 in
                 Pos.same_pos_as mir_arg arg :: res)
               [] args)
        in
        aux ((Mir.Print (std, mir_args), pos) :: res, var_data) il
    | (Mast.Iterate (vn, vcats, expr, instrs), pos) :: il ->
        let var_name = Pos.unmark vn in
        let var_pos = Pos.get_position vn in
        (match Pos.VarNameToID.find_opt var_name idmap with
        | Some v ->
            let msg =
              Format.asprintf "variable already declared %a" Pos.format_position
                (Pos.get_position v.name)
            in
            Errors.raise_spanned_error msg pos
        | _ -> ());
        let var =
          Mir.Variable.new_var (var_name, var_pos) None ("iterator", var_pos)
            ~attributes:[] ~origin:None ~cats:None ~is_table:None ~is_temp:false
            ~is_it:true
        in
        let var_data = Mir.VariableDict.add var var_data in
        let idmap = Pos.VarNameToID.add var_name var idmap in
        let var_decl =
          {
            var_decl_typ = None;
            var_decl_is_table = None;
            var_decl_descr = None;
            var_pos;
          }
        in
        let var_decl_data = Mir.VariableMap.add var var_decl var_decl_data in
        let catSet = Check_validity.cats_variable_from_decl_list vcats cats in
        let ctx = new_ctx in
        let ctx = { ctx with idmap } in
        let mir_expr = translate_expression cats idmap ctx expr in
        let prog_it, var_data =
          translate_prog error_decls cats var_data idmap var_decl_data instrs
        in
        aux
          ( (Mir.Iterate (var.Mir.id, catSet, mir_expr, prog_it), pos) :: res,
            var_data )
          il
    | (Mast.Restore (rest_params, instrs), pos) :: il ->
        let vars, var_params, var_data =
          List.fold_left
            (fun (vars, var_params, var_data) rest_param ->
              match Pos.unmark rest_param with
              | Mast.VarList vl ->
                  let vars =
                    List.fold_left
                      (fun vars vn ->
                        let var_name = Pos.unmark vn in
                        let var_pos = Pos.get_position vn in
                        match Pos.VarNameToID.find_opt var_name idmap with
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
                  (vars, var_params, var_data)
              | Mast.VarCats (vn, vcats, expr) ->
                  let var_name = Pos.unmark vn in
                  let var_pos = Pos.get_position vn in
                  (match Pos.VarNameToID.find_opt var_name idmap with
                  | Some v ->
                      let msg =
                        Format.asprintf "variable already declared %a"
                          Pos.format_position (Pos.get_position v.name)
                      in
                      Errors.raise_spanned_error msg pos
                  | _ -> ());
                  let var =
                    Mir.Variable.new_var (var_name, var_pos) None
                      ("iterator", var_pos) ~attributes:[] ~origin:None
                      ~cats:None ~is_table:None ~is_temp:false ~is_it:true
                  in
                  let var_data = Mir.VariableDict.add var var_data in
                  let idmap = Pos.VarNameToID.add var_name var idmap in
                  let catSet =
                    Check_validity.cats_variable_from_decl_list vcats cats
                  in
                  let ctx = new_ctx in
                  let ctx = { ctx with idmap } in
                  let mir_expr = translate_expression cats idmap ctx expr in
                  let var_params = (var, catSet, mir_expr) :: var_params in
                  (vars, var_params, var_data))
            (Mir.VariableMap.empty, [], var_data)
            rest_params
        in
        let prog_rest, var_data =
          translate_prog error_decls cats var_data idmap var_decl_data instrs
        in
        aux
          ((Mir.Restore (vars, var_params, prog_rest), pos) :: res, var_data)
          il
    | (Mast.RaiseError (err_name, var_opt), pos) :: il ->
        let err_decl =
          try
            List.find
              (fun e ->
                String.equal (Pos.unmark e.Mir.Error.name) (Pos.unmark err_name))
              error_decls
          with Not_found ->
            Errors.raise_error
              (Format.asprintf "undeclared error %s %a" (Pos.unmark err_name)
                 Pos.format_position
                 (Pos.get_position err_name))
        in
        let var_res = Option.map Pos.unmark var_opt in
        aux ((Mir.RaiseError (err_decl, var_res), pos) :: res, var_data) il
    | (Mast.CleanErrors, pos) :: il ->
        aux ((Mir.CleanErrors, pos) :: res, var_data) il
    | (Mast.ExportErrors, pos) :: il ->
        aux ((Mir.ExportErrors, pos) :: res, var_data) il
    | (Mast.FinalizeErrors, pos) :: il ->
        aux ((Mir.FinalizeErrors, pos) :: res, var_data) il
    | (Mast.ComputeDomain _, _) :: _
    | (Mast.ComputeChaining _, _) :: _
    | (Mast.ComputeVerifs (_, _), _) :: _ ->
        assert false
  in
  aux ([], var_data) prog

let get_targets (error_decls : Mir.Error.t list)
    (cats : Mir.cat_variable_data Mir.CatVarMap.t) (apps : Pos.t StrMap.t)
    (var_data : Mir.VariableDict.t) (idmap : Mir.variable Pos.VarNameToID.t)
    (var_decl_data : var_decl_data Mir.VariableMap.t) (p : Mast.program) :
    Mir.target_data Mir.TargetMap.t * Mir.VariableDict.t =
  List.fold_left
    (fun (targets, var_data) source_file ->
      List.fold_left
        (fun (targets, var_data) (item, pos_item) ->
          match item with
          | Mast.Target t ->
              let target_name = t.Mast.target_name in
              let name = Pos.unmark target_name in
              (match Mir.TargetMap.find_opt name targets with
              | Some data ->
                  let old_pos = Pos.get_position data.Mir.target_name in
                  let msg =
                    Format.asprintf "target %s already defined %a" name
                      Pos.format_position old_pos
                  in
                  Errors.raise_spanned_error msg pos_item
              | None -> ());
              let target_file = t.Mast.target_file in
              let target_apps = t.Mast.target_applications in
              List.iter
                (fun (app, pos) ->
                  if not (StrMap.mem app apps) then
                    Errors.raise_spanned_error "unknown application" pos)
                target_apps;
              let target_tmp_vars =
                List.fold_left
                  (fun vars ((var, pos), size) ->
                    match Pos.VarNameToID.find_opt var idmap with
                    | Some v ->
                        let msg =
                          Format.asprintf "variable already declared %a"
                            Pos.format_position (Pos.get_position v.name)
                        in
                        Errors.raise_spanned_error msg pos
                    | _ -> begin
                        match StrMap.find_opt var vars with
                        | Some (old_pos, _) ->
                            let msg =
                              Format.asprintf "variable already declared %a"
                                Pos.format_position old_pos
                            in
                            Errors.raise_spanned_error msg pos
                        | None -> StrMap.add var (pos, size) vars
                      end)
                  StrMap.empty t.Mast.target_tmp_vars
              in
              let var_data, tmp_idmap, tmp_var_decl_data =
                StrMap.fold
                  (fun name (pos, size) (var_data, map, decls) ->
                    let size' =
                      Pos.unmark_option (Mast.get_table_size_opt size)
                    in
                    let var =
                      Mir.Variable.new_var (name, pos) None ("temporary", pos)
                        ~attributes:[] ~origin:None ~cats:None ~is_table:size'
                        ~is_temp:true ~is_it:false
                    in
                    let var_data = Mir.VariableDict.add var var_data in
                    let map = Pos.VarNameToID.add name var map in
                    let var_decl =
                      {
                        var_decl_typ = None;
                        var_decl_is_table = size';
                        var_decl_descr = None;
                        var_pos = pos;
                      }
                    in
                    let decls = Mir.VariableMap.add var var_decl decls in
                    (var_data, map, decls))
                  target_tmp_vars
                  (var_data, idmap, var_decl_data)
              in
              let target_tmp_vars =
                StrMap.mapi
                  (fun vn (pos, size) ->
                    let var = Pos.VarNameToID.find vn tmp_idmap in
                    let size' =
                      Pos.unmark_option (Mast.get_table_size_opt size)
                    in
                    (var, pos, size'))
                  target_tmp_vars
              in
              let target_prog, var_data =
                translate_prog error_decls cats var_data tmp_idmap
                  tmp_var_decl_data t.Mast.target_prog
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
              ( Mir.TargetMap.add (Pos.unmark target_name) target_data targets,
                var_data )
          | _ -> (targets, var_data))
        (targets, var_data) source_file)
    (Mir.TargetMap.empty, var_data)
    p

let get_conds (verif_domains : Mir.verif_domain Mast.DomainIdMap.t)
    (cats : Mir.cat_variable_data Mir.CatVarMap.t)
    (error_decls : Mir.Error.t list) (idmap : Mir.idmap) (p : Mast.program) :
    Mir.condition_data Mir.RuleMap.t =
  List.fold_left
    (fun conds source_file ->
      List.fold_left
        (fun conds source_file_item ->
          match Pos.unmark source_file_item with
          | Mast.Verification verif
            when belongs_to_iliad_app verif.Mast.verif_applications ->
              let rule_number = Pos.unmark verif.verif_number in
              let conds, _ =
                List.fold_left
                  (fun (conds, id_offset) verif_cond ->
                    let rule_number = rule_number + id_offset in
                    let cond_domain =
                      let vdom_id =
                        Mast.DomainId.from_marked_list
                          (Pos.unmark verif.verif_tag_names)
                      in
                      match Mast.DomainIdMap.find_opt vdom_id verif_domains with
                      | Some vdom -> vdom
                      | None ->
                          Errors.raise_spanned_error "Unknown verif domain"
                            (Pos.get_position verif.verif_tag_names)
                    in
                    let e =
                      translate_expression cats idmap
                        { idmap; table_definition = false }
                        (Pos.unmark verif_cond).Mast.verif_cond_expr
                    in
                    let cond_cats =
                      Mir.fold_expr_var
                        (fun subtypes (var : Mir.variable) ->
                          match var.Mir.cats with
                          | None -> subtypes
                          | Some c ->
                              Mir.CatVarMap.add c
                                (1 + Mir.CatVarMap.find c subtypes)
                                subtypes)
                        (Mir.CatVarMap.map (fun _ -> 0) cats)
                        (Pos.unmark e)
                    in
                    let err =
                      let err_name, err_var =
                        (Pos.unmark verif_cond).Mast.verif_cond_error
                      in
                      try
                        ( List.find
                            (fun e ->
                              String.equal
                                (Pos.unmark e.Mir.Error.name)
                                (Pos.unmark err_name))
                            error_decls,
                          Option.map
                            (fun v -> Mir.get_var (Pos.unmark v) idmap)
                            err_var )
                      with Not_found ->
                        Errors.raise_error
                          (Format.asprintf "undeclared error %s %a"
                             (Pos.unmark err_name) Pos.format_position
                             (Pos.get_position err_name))
                    in
                    let cond_seq_id = Mir.Variable.fresh_id () in
                    let rov = Mir.VerifID rule_number in
                    match Mir.RuleMap.find_opt rov conds with
                    | Some c ->
                        Errors.raise_spanned_error
                          (Format.asprintf "verif number %d already defined: %a"
                             rule_number Pos.format_position
                             (Pos.get_position c.Mir.cond_number))
                          (Pos.get_position verif.verif_number)
                    | None ->
                        ( Mir.RuleMap.add rov
                            Mir.
                              {
                                cond_seq_id;
                                cond_number =
                                  Pos.same_pos_as rov verif.verif_number;
                                cond_domain;
                                cond_expr = e;
                                cond_error = err;
                                cond_cats;
                              }
                            conds,
                          id_offset + 1 ))
                  (conds, 0) verif.Mast.verif_conditions
              in
              conds
          | _ -> conds)
        conds (List.rev source_file)) (* Order important for DGFiP *)
    Mir.RuleMap.empty p

let translate (p : Mast.program) : Mir.program =
  let p = Expand_macros.proceed p in
  let prog = Check_validity.proceed p in
  let prog_targets =
    let targets =
      StrMap.fold
        (fun tname t prog_targets ->
          let target_applications = [ (prog.prog_app, Pos.no_pos) ] in
          let target_tmp_vars =
            StrMap.fold
              (fun vn (sz_opt, pos) tmp_vars ->
                let size =
                  Option.map (fun i -> (Mast.LiteralSize i, pos)) sz_opt
                in
                ((vn, pos), size) :: tmp_vars)
              t.Check_validity.target_tmp_vars []
          in
          let target =
            Mast.
              {
                target_name = (tname, Pos.no_pos);
                target_file = t.Check_validity.target_file;
                target_applications;
                target_tmp_vars;
                target_prog = t.Check_validity.target_prog;
              }
          in
          (Mast.Target target, Pos.no_pos) :: prog_targets)
        prog.prog_targets []
    in
    [ targets ]
  in
  let apps = prog.Check_validity.prog_apps in
  let var_category_decls = get_var_categories p in
  let var_category_map = prog.Check_validity.prog_var_cats in
  let var_decl_data, error_decls, idmap =
    get_variables_decl p var_category_decls
  in
  let idmap = get_var_redefinitions p idmap in
  let var_data =
    add_dummy_definitions_for_variable_declarations Mir.VariableMap.empty
      var_decl_data
  in
  let var_data =
    Mir.VariableMap.fold
      (fun var _data var_dict -> Mir.VariableDict.add var var_dict)
      var_data Mir.VariableDict.empty
  in
  let targets, var_data =
    get_targets error_decls var_category_map apps var_data idmap var_decl_data
      prog_targets
  in
  Mir.
    {
      program_safe_prefix = prog.prog_prefix;
      program_applications = apps;
      program_var_categories = var_category_map;
      program_rule_domains = prog.Check_validity.prog_rdoms;
      program_verif_domains = prog.Check_validity.prog_vdoms;
      program_chainings = Mast.ChainingMap.empty;
      program_vars = var_data;
      program_targets = targets;
      program_idmap = idmap;
    }
