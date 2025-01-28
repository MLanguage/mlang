(* This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

module Err = struct
  let constant_already_defined old_pos pos =
    Errors.raise_spanned_error
      (Format.asprintf "constant already defined %a" Pos.format_position old_pos)
      pos

  let unknown_constant pos = Errors.raise_spanned_error "unknown constant" pos

  let table_size_must_be_positive pos =
    Errors.raise_spanned_error "table size must be a positive integer" pos

  let unbounded_parameter_in_variable_name p pos =
    Errors.raise_spanned_error
      (Format.sprintf "unbounded parameter '%c' in variable name" p)
      pos

  let variable_name_does_not_match_computed_size pos =
    Errors.raise_spanned_error "variable name does not match computed size" pos

  let integer_representation_exceeds_computed_size pos =
    Errors.raise_spanned_error "integer representation exceeds computed size"
      pos

  let same_parameter_in_nested_loops p pos =
    Errors.raise_spanned_error
      (Format.asprintf "Same loop parameter %c used in two nested loop contexts"
         p)
      pos

  let generic_variable_not_allowed_in_left_part_of_loop pos =
    Errors.raise_spanned_error
      "generic variables not allowed in left part of loop" pos

  let loop_variables_have_different_sizes pos =
    Errors.raise_spanned_error "loop variables have different sizes" pos

  let non_numeric_range_bounds_must_be_a_single_character pos =
    Errors.raise_spanned_error
      "non-numeric range bounds must consist of a single character" pos

  let range_bounds_must_be_of_the_same_type pos =
    Errors.raise_spanned_error "range bounds must be of the same type" pos

  let variable_is_not_an_integer_constant name pos =
    Errors.raise_spanned_error
      (Format.asprintf
         "variable %s is not an integer constant and cannot be used here" name)
      pos

  let constant_forbidden_as_table pos =
    Errors.raise_spanned_error "constant forbidden as table" pos

  let constant_cannot_have_an_attribut pos =
    Errors.raise_spanned_error "constant cannot have an attribut" pos

  let constant_cannot_have_a_size pos =
    Errors.raise_spanned_error "constant cannot have a size" pos

  let constant_forbidden_as_lvalue pos =
    Errors.raise_spanned_error "constant forbidden as lvalue" pos

  let constant_forbidden_as_arg pos =
    Errors.raise_spanned_error "constant forbidden as argument" pos

  let unknown_application app pos =
    let msg = Format.sprintf "application \"%s\" is unknown" app in
    Errors.raise_spanned_error msg pos

  let unknown_app_on_cmdline app =
    let msg = Format.sprintf "unknown application \"%s\" on command line" app in
    Errors.raise_error msg

  let application_already_defined app old_pos pos =
    let msg =
      Format.asprintf "application %s already defined %a" app
        Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let chaining_already_defined ch old_pos pos =
    let msg =
      Format.asprintf "chaining %s already defined %a" ch Pos.format_position
        old_pos
    in
    Errors.raise_spanned_error msg pos

  let unknown_chaining ch pos =
    let msg = Format.sprintf "chaining \"%s\" is unknown" ch in
    Errors.raise_spanned_error msg pos
end

(** Eliminates unselected applications *)

type apps_env = {
  selected_apps : unit StrMap.t;
  apps : (bool * Pos.t) StrMap.t;
  chains : (bool * Pos.t) StrMap.t;
}

let empty_apps_env (cli_apps : string list) : apps_env =
  let selected_apps =
    let add_app m a = StrMap.add a () m in
    List.fold_left add_app StrMap.empty cli_apps
  in
  { selected_apps; apps = StrMap.empty; chains = StrMap.empty }

let get_selected_apps (apps_env : apps_env)
    (apps : Mast.application Pos.marked StrMap.t) :
    Mast.application Pos.marked StrMap.t =
  let sel_app _ (a, apos) apps =
    match StrMap.find_opt a apps_env.apps with
    | Some (b, _) -> if b then StrMap.add a (a, apos) apps else apps
    | None -> Err.unknown_application a apos
  in
  StrMap.fold sel_app apps StrMap.empty

let get_selected_apps_list (apps_env : apps_env)
    (apps : Mast.application Pos.marked list) : Mast.application Pos.marked list
    =
  let sel_app apps (a, apos) =
    match StrMap.find_opt a apps_env.apps with
    | Some (b, _) -> if b then (a, apos) :: apps else apps
    | None -> Err.unknown_application a apos
  in
  List.rev (List.fold_left sel_app [] apps)

let get_selected_chains (apps_env : apps_env)
    (chains : Mast.chaining Pos.marked StrMap.t) :
    Mast.chaining Pos.marked StrMap.t =
  let sel_chain _ (ch, chpos) chains =
    match StrMap.find_opt ch apps_env.chains with
    | Some (b, _) -> if b then StrMap.add ch (ch, chpos) chains else chains
    | None -> Err.unknown_chaining ch chpos
  in
  StrMap.fold sel_chain chains StrMap.empty

(** Eliminates constants and loops *)
let check_apps_on_cmdline (apps_env : apps_env) : unit =
  let iter a _ =
    match StrMap.find_opt a apps_env.apps with
    | Some _ -> ()
    | None -> Err.unknown_app_on_cmdline a
  in
  StrMap.iter iter apps_env.selected_apps

let elim_unselected_apps (p : Mast.program) : Mast.program =
  let apps_env, prog =
    List.fold_left
      (fun (apps_env, prog) source_file ->
        let apps_env, prog_file =
          List.fold_left
            (fun (apps_env, prog_file) source_item ->
              let item, pos_item = source_item in
              match item with
              | Mast.Application (app, pos) -> (
                  match StrMap.find_opt app apps_env.apps with
                  | Some (_, old_pos) ->
                      Err.application_already_defined app old_pos pos
                  | None ->
                      let is_sel = StrMap.mem app apps_env.selected_apps in
                      let apps = StrMap.add app (is_sel, pos) apps_env.apps in
                      let apps_env = { apps_env with apps } in
                      let prog_file =
                        if is_sel then source_item :: prog_file else prog_file
                      in
                      (apps_env, prog_file))
              | Mast.Chaining (mch, mal) -> (
                  let ch, pos = mch in
                  match StrMap.find_opt ch apps_env.chains with
                  | Some (_, old_pos) ->
                      Err.chaining_already_defined ch old_pos pos
                  | None ->
                      let sel_apps = get_selected_apps_list apps_env mal in
                      let is_sel = sel_apps <> [] in
                      let chains =
                        StrMap.add ch (is_sel, pos) apps_env.chains
                      in
                      let apps_env = { apps_env with chains } in
                      let prog_file =
                        if is_sel then
                          let new_item =
                            (Mast.Chaining (mch, sel_apps), pos_item)
                          in
                          new_item :: prog_file
                        else prog_file
                      in
                      (apps_env, prog_file))
              | Mast.Rule rule ->
                  let rule_apps = get_selected_apps apps_env rule.rule_apps in
                  if StrMap.is_empty rule_apps then (apps_env, prog_file)
                  else
                    let rule_chainings =
                      get_selected_chains apps_env rule.rule_chainings
                    in
                    let rule =
                      { rule with Mast.rule_apps; Mast.rule_chainings }
                    in
                    let prog_file = (Mast.Rule rule, pos_item) :: prog_file in
                    (apps_env, prog_file)
              | Mast.Verification verif ->
                  let verif_apps =
                    get_selected_apps apps_env verif.verif_apps
                  in
                  if StrMap.is_empty verif_apps then (apps_env, prog_file)
                  else
                    let verif = { verif with Mast.verif_apps } in
                    let prog_file =
                      (Mast.Verification verif, pos_item) :: prog_file
                    in
                    (apps_env, prog_file)
              | Mast.Target target ->
                  let target_apps =
                    get_selected_apps apps_env target.target_apps
                  in
                  if StrMap.is_empty target_apps then (apps_env, prog_file)
                  else
                    let target = { target with Mast.target_apps } in
                    let prog_file =
                      (Mast.Target target, pos_item) :: prog_file
                    in
                    (apps_env, prog_file)
              | Mast.Function target ->
                  let target_apps =
                    get_selected_apps apps_env target.target_apps
                  in
                  if StrMap.is_empty target_apps then (apps_env, prog_file)
                  else
                    let target = { target with Mast.target_apps } in
                    let prog_file =
                      (Mast.Function target, pos_item) :: prog_file
                    in
                    (apps_env, prog_file)
              | VariableDecl _ | EventDecl _ | Error _ | Output _ | Func
              | VarCatDecl _ | RuleDomDecl _ | VerifDomDecl _ ->
                  (apps_env, source_item :: prog_file))
            (apps_env, []) source_file
        in
        (apps_env, List.rev prog_file :: prog))
      (empty_apps_env !Cli.application_names, [])
      p
  in
  check_apps_on_cmdline apps_env;
  List.rev prog

module ConstMap = StrMap

type const_context = float Pos.marked ConstMap.t

module ParamsMap = struct
  include CharMap

  let pp ?(sep = "; ") ?(pp_key = Format.pp_print_char) ?(assoc = "=")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

type loop_param_value = VarName of Mast.variable_name | RangeInt of int

type loop_context = (loop_param_value * int) ParamsMap.t

type loop_domain = (loop_param_value * int) list ParamsMap.t

let format_loop_param_value fmt (v : loop_param_value * int) =
  match fst v with
  | VarName v -> Format.fprintf fmt "%s" v
  | RangeInt i -> Format.fprintf fmt "%d" i

let format_loop_context fmt (ld : loop_context) =
  ParamsMap.pp format_loop_param_value fmt ld

let format_loop_domain fmt (ld : loop_domain) =
  ParamsMap.pp (Pp.list_comma format_loop_param_value) fmt ld

let add_const (name, name_pos) (cval, cval_pos) const_map =
  match ConstMap.find_opt name const_map with
  | Some (_, old_pos) -> Err.constant_already_defined old_pos name_pos
  | None -> (
      match cval with
      | Com.AtomLiteral (Com.Float f) ->
          ConstMap.add name (f, name_pos) const_map
      | Com.AtomVar (Mast.Normal const) -> (
          match ConstMap.find_opt const const_map with
          | Some (value, _) -> ConstMap.add name (value, name_pos) const_map
          | None -> Err.unknown_constant cval_pos)
      | _ -> assert false)

let expand_table_size (const_map : const_context) table_size =
  match table_size with
  | Some (Mast.SymbolSize c, size_pos) -> (
      match ConstMap.find_opt c const_map with
      | Some (f, _) ->
          let i = int_of_float f in
          if f = float i && i >= 0 then Some (Mast.LiteralSize i, size_pos)
          else Err.table_size_must_be_positive size_pos
      | None -> Err.unknown_constant size_pos)
  | _ -> table_size

let rec expand_variable (const_map : const_context) (loop_map : loop_context)
    (m_var : Mast.variable Pos.marked) : Mast.expression Pos.marked =
  let var, var_pos = m_var in
  match var with
  | Mast.Normal name -> (
      match ConstMap.find_opt name const_map with
      | Some (f, _) -> (Com.Literal (Float f), var_pos)
      | None -> (Com.Var var, var_pos))
  | Mast.Generic gen_name ->
      if List.length gen_name.Mast.parameters == 0 then
        expand_variable const_map loop_map
          (Mast.Normal gen_name.Mast.base, var_pos)
      else instantiate_params const_map loop_map gen_name.Mast.base var_pos

and check_var_name (var_name : string) (var_pos : Pos.t) : unit =
  for i = String.length var_name - 1 downto 0 do
    let p = var_name.[i] in
    if
      p <> '_'
      && (not ('0' <= p && p <= '9'))
      && Char.equal (Char.lowercase_ascii p) p
    then Err.unbounded_parameter_in_variable_name p var_pos
  done

and instantiate_params (const_map : const_context) (loop_map : loop_context)
    (var_name : string) (pos : Pos.t) : Mast.expression Pos.marked =
  match ParamsMap.choose_opt loop_map with
  | None ->
      check_var_name var_name pos;
      expand_variable const_map loop_map (Mast.Normal var_name, pos)
  | Some (param, (value, size)) ->
      let new_var_name =
        match value with
        | VarName value ->
            if size <> String.length value then
              Err.variable_name_does_not_match_computed_size pos;
            Re.Str.replace_first
              (Re.Str.regexp (Format.asprintf "%c" param))
              value var_name
        | RangeInt i ->
            let is = string_of_int i in
            let l = String.length is in
            if size < l then
              Err.integer_representation_exceeds_computed_size pos;
            let value =
              if l = size then is else String.make (size - l) '0' ^ is
            in
            Re.Str.replace_first
              (Re.Str.regexp (Format.asprintf "%c" param))
              value var_name
      in
      let loop_map = ParamsMap.remove param loop_map in
      instantiate_params const_map loop_map new_var_name pos

let merge_loop_context (loop_map : loop_context) (lmap : loop_context)
    (pos : Pos.t) : loop_context =
  ParamsMap.merge
    (fun param old_val new_val ->
      match (old_val, new_val) with
      | Some _, Some _ -> Err.same_parameter_in_nested_loops param pos
      | Some v, None | None, Some v -> Some v
      | None, None -> assert false)
    loop_map lmap

(** The M language has a weird and very annoying "feature", which is that you
    can define the following loop:

    {v sum(i=05,06,07:Xi) v}

    The legacy compiler enforce the following: All possible value of [i] must
    have their string representations have the same length. In such loops we can
    mix characters, strings and integers, the former two will define strictly
    that length while integer will settle on their shortest representation.

    In thes example above, [Xi] will become [X5] because there are no string or
    integer above 9 in the list of possible values. *)

type var_or_int_index = VarIndex of Mast.variable | IntIndex of int

(** The M language added a new feature in its 2017 edition : you can specify
    loop variable ranges bounds with constant variables. Because we need the
    actual value of the bounds to unroll everything, this function queries the
    const value in the context if needed. Otherwise, it might be a dynamic
    index. *)
let var_or_int_value (const_map : const_context)
    (m_atom : Mast.variable Com.atom Pos.marked) : var_or_int_index =
  match Pos.unmark m_atom with
  | Com.AtomVar v -> (
      let name = Mast.get_variable_name v in
      match ConstMap.find_opt name const_map with
      | Some (fvalue, _) -> IntIndex (int_of_float fvalue)
      | None -> VarIndex v)
  | Com.AtomLiteral (Com.Float f) -> IntIndex (int_of_float f)
  | Com.AtomLiteral Com.Undefined -> assert false

let var_or_int (m_atom : Mast.variable Com.atom Pos.marked) =
  let atom, atom_pos = m_atom in
  match atom with
  | Com.AtomVar (Normal v) -> VarName v
  | Com.AtomVar (Generic _) ->
      Err.generic_variable_not_allowed_in_left_part_of_loop atom_pos
  | Com.AtomLiteral (Com.Float f) -> RangeInt (int_of_float f)
  | Com.AtomLiteral Com.Undefined -> assert false

let loop_variables_size (lpvl : loop_param_value list) (pos : Pos.t) =
  let size_err p = Err.loop_variables_have_different_sizes p in
  let _fixed, size =
    List.fold_left
      (fun (fixed, size) (lpv : loop_param_value) ->
        match lpv with
        | VarName n ->
            let l = String.length n in
            if (fixed && size <> l) || ((not fixed) && size > l) then
              size_err pos;
            (true, l)
        | RangeInt i ->
            let l = String.length (Int.to_string i) in
            if fixed && size < l then size_err pos;
            (fixed, max l size))
      (false, 0) lpvl
  in
  size

(** Helper to make a list of integers from an integer range *)
let rec make_int_range_list (i1 : int) (i2 : int) : loop_param_value list =
  if i1 > i2 then []
  else
    let tl = make_int_range_list (i1 + 1) i2 in
    RangeInt i1 :: tl

let make_var_range_list (v1 : string) (v2 : string) : loop_param_value list =
  let rec aux c1 c2 =
    if c1 > c2 then []
    else
      let tl = aux (c1 + 1) c2 in
      VarName (String.make 1 (Char.chr c1)) :: tl
  in
  aux (Char.code v1.[0]) (Char.code v2.[0])

let make_range_list (l1 : Mast.variable Com.atom Pos.marked)
    (l2 : Mast.variable Com.atom Pos.marked) : loop_param_value list =
  let length_err p =
    Err.non_numeric_range_bounds_must_be_a_single_character p
  in
  match (var_or_int l1, var_or_int l2) with
  | RangeInt i1, RangeInt i2 -> make_int_range_list i1 i2
  | VarName v1, VarName v2 ->
      if String.length v1 <> 1 then length_err (Pos.get_position l1);
      if String.length v2 <> 1 then length_err (Pos.get_position l2);
      make_var_range_list v1 v2
  | _ -> Err.range_bounds_must_be_of_the_same_type (Pos.get_position l2)

(** From a loop domain of varying loop parameters, builds by cartesian product
    the list of all iterations that the loop will take, each time assigining a
    different combination of values to the loop parameters. *)
let rec iterate_all_combinations (ld : loop_domain) : loop_context list =
  try
    let param, values = ParamsMap.choose ld in
    match values with
    | [] -> []
    | [ hd ] ->
        let new_ld = ParamsMap.remove param ld in
        let all_contexts = iterate_all_combinations new_ld in
        if List.length all_contexts = 0 then [ ParamsMap.one param hd ]
        else List.map (fun c -> ParamsMap.add param hd c) all_contexts
    | hd :: tl ->
        let new_ld = ParamsMap.add param tl ld in
        let all_contexts_minus_hd_val_for_param =
          iterate_all_combinations new_ld
        in
        let new_ld = ParamsMap.add param [ hd ] ld in
        let all_context_with_hd_val_for_param =
          iterate_all_combinations new_ld
        in
        all_context_with_hd_val_for_param @ all_contexts_minus_hd_val_for_param
  with Not_found -> []

(** This function is the workhorse of loop unrolling : it takes a loop prefix
    containing the set of variables over which to iterate, and fabricates a
    combinator. This combinator takes a user-provided way of translating the
    loop body generically over the values of the iterated variables, and produce
    a list corresponding of the unrolled loop bodies expressions containing the
    iterated values.

    In OCaml terms, if you want [translate_loop_variables lvs f ctx], then you
    should define [f] by [let f = fun lc i ctx -> ...] and use {!val:
    merge_loop_ctx} inside [...] before translating the loop body. [lc] is the
    loop context, [i] the loop sequence index and [ctx] the translation context. *)

let expand_loop_variables (lvs : Mast.variable Com.loop_variables Pos.marked)
    (const_map : const_context) : (loop_context -> 'a) -> 'a list =
  let pos = Pos.get_position lvs in
  match Pos.unmark lvs with
  | Com.ValueSets lvs | Com.Ranges lvs ->
      let varying_domain =
        List.fold_left
          (fun domain (param, values) ->
            let values =
              List.flatten
                (List.map
                   (fun value ->
                     match value with
                     | Com.Single l -> [ var_or_int l ]
                     | Com.Range (l1, l2) -> make_range_list l1 l2
                     | Com.Interval (l1, l2) -> (
                         let lb = var_or_int_value const_map l1 in
                         let ub = var_or_int_value const_map l2 in
                         match (lb, ub) with
                         | VarIndex v, _ | _, VarIndex v ->
                             Err.variable_is_not_an_integer_constant
                               (Mast.get_variable_name v) pos
                         | IntIndex lb, IntIndex ub -> make_int_range_list lb ub
                         ))
                   values)
            in
            let sz = loop_variables_size values pos in
            let values = List.map (fun v -> (v, sz)) values in
            ParamsMap.add (Pos.unmark param) values domain)
          ParamsMap.empty lvs
      in
      let loop_map_list = iterate_all_combinations varying_domain in
      fun translator -> List.map translator loop_map_list

let rec expand_expression (const_map : const_context) (loop_map : loop_context)
    (m_expr : Mast.expression Pos.marked) : Mast.expression Pos.marked =
  let open Com in
  let expr, expr_pos = m_expr in
  match expr with
  | TestInSet (positive, e, values) ->
      let e' = expand_expression const_map loop_map e in
      let values' =
        List.map
          (fun set_value ->
            match set_value with
            | VarValue set_var -> (
                match expand_variable const_map loop_map set_var with
                | Literal (Float f), var_pos -> FloatValue (f, var_pos)
                | Var var, var_pos -> VarValue (var, var_pos)
                | _ -> assert false)
            | FloatValue _ | Interval _ -> set_value)
          values
      in
      (TestInSet (positive, e', values'), expr_pos)
  | Comparison (op, e1, e2) ->
      let e1' = expand_expression const_map loop_map e1 in
      let e2' = expand_expression const_map loop_map e2 in
      (Comparison (op, e1', e2'), expr_pos)
  | Binop (op, e1, e2) ->
      let e1' = expand_expression const_map loop_map e1 in
      let e2' = expand_expression const_map loop_map e2 in
      (Binop (op, e1', e2'), expr_pos)
  | Unop (op, e) ->
      let e' = expand_expression const_map loop_map e in
      (Unop (op, e'), expr_pos)
  | Index (t, i) ->
      let t' =
        match expand_variable const_map loop_map t with
        | Var v, v_pos -> (v, v_pos)
        | Literal (Float _), v_pos -> Err.constant_forbidden_as_table v_pos
        | _ -> assert false
      in
      let i' = expand_expression const_map loop_map i in
      (Index (t', i'), expr_pos)
  | Conditional (e1, e2, e3_opt) ->
      let e1' = expand_expression const_map loop_map e1 in
      let e2' = expand_expression const_map loop_map e2 in
      let e3_opt' =
        match e3_opt with
        | Some e3 -> Some (expand_expression const_map loop_map e3)
        | None -> None
      in
      (Conditional (e1', e2', e3_opt'), expr_pos)
  | FuncCall (f_name, args) ->
      let args' =
        List.map (fun arg -> expand_expression const_map loop_map arg) args
      in
      (FuncCall (f_name, args'), expr_pos)
  | FuncCallLoop (f_name, lvs, e) ->
      let loop_context_provider = expand_loop_variables lvs const_map in
      let translator lmap =
        let loop_map =
          merge_loop_context loop_map lmap (Pos.get_position lvs)
        in
        expand_expression const_map loop_map e
      in
      let args' = loop_context_provider translator in
      (FuncCall (f_name, args'), expr_pos)
  | Literal _ -> m_expr
  | Var v -> expand_variable const_map loop_map (v, expr_pos)
  | Loop (lvs, e) ->
      let loop_context_provider = expand_loop_variables lvs const_map in
      let translator lmap =
        let loop_map =
          merge_loop_context loop_map lmap (Pos.get_position lvs)
        in
        expand_expression const_map loop_map e
      in
      let loop_exprs = loop_context_provider translator in
      List.fold_left
        (fun res loop_expr ->
          (Binop ((Or, expr_pos), res, loop_expr), expr_pos))
        (Literal (Float 0.0), expr_pos)
        loop_exprs
  | Attribut (var, a) -> (
      match expand_variable const_map loop_map var with
      | Var v, v_pos -> (Attribut ((v, v_pos), a), expr_pos)
      | Literal (Float _), v_pos -> Err.constant_cannot_have_an_attribut v_pos
      | _ -> assert false)
  | EventField (e, f, i) ->
      let e' = expand_expression const_map loop_map e in
      (EventField (e', f, i), expr_pos)
  | Size var -> (
      match expand_variable const_map loop_map var with
      | Var v, v_pos -> (Size (v, v_pos), expr_pos)
      | Literal (Float _), v_pos -> Err.constant_cannot_have_a_size v_pos
      | _ -> assert false)
  | NbCategory _ | NbAnomalies | NbDiscordances | NbInformatives | NbBloquantes
    ->
      m_expr

let expand_formula (const_map : const_context)
    (prev : Mast.variable Com.formula Pos.marked list)
    (m_form : Mast.variable Com.formula Pos.marked) :
    Mast.variable Com.formula Pos.marked list =
  let form, form_pos = m_form in
  match form with
  | Com.SingleFormula (VarDecl (v, idx, e)) ->
      let v' =
        match expand_variable const_map ParamsMap.empty v with
        | Com.Var v, v_pos -> (v, v_pos)
        | Com.Literal (Com.Float _), v_pos ->
            Err.constant_forbidden_as_lvalue v_pos
        | _ -> assert false
      in
      let idx' = Option.map (expand_expression const_map ParamsMap.empty) idx in
      let e' = expand_expression const_map ParamsMap.empty e in
      (Com.SingleFormula (VarDecl (v', idx', e')), form_pos) :: prev
  | Com.SingleFormula (EventFieldDecl (idx, f, i, e)) ->
      let idx' = expand_expression const_map ParamsMap.empty idx in
      let e' = expand_expression const_map ParamsMap.empty e in
      (Com.SingleFormula (EventFieldDecl (idx', f, i, e')), form_pos) :: prev
  | Com.MultipleFormulaes (lvs, VarDecl (v, idx, e)) ->
      let loop_context_provider = expand_loop_variables lvs const_map in
      let translator loop_map =
        let v' =
          match expand_variable const_map loop_map v with
          | Com.Var v, v_pos -> (v, v_pos)
          | Com.Literal (Com.Float _), v_pos ->
              Err.constant_forbidden_as_lvalue v_pos
          | _ -> assert false
        in
        let idx' = Option.map (expand_expression const_map loop_map) idx in
        let e' = expand_expression const_map loop_map e in
        (Com.SingleFormula (VarDecl (v', idx', e')), form_pos)
      in
      let res = loop_context_provider translator in
      List.rev res @ prev
  | Com.MultipleFormulaes (lvs, EventFieldDecl (idx, f, i, e)) ->
      let loop_context_provider = expand_loop_variables lvs const_map in
      let translator loop_map =
        let idx' = expand_expression const_map loop_map idx in
        let e' = expand_expression const_map loop_map e in
        (Com.SingleFormula (EventFieldDecl (idx', f, i, e')), form_pos)
      in
      let res = loop_context_provider translator in
      List.rev res @ prev

let rec expand_instruction (const_map : const_context)
    (prev : Mast.instruction Pos.marked list)
    (m_instr : Mast.instruction Pos.marked) : Mast.instruction Pos.marked list =
  let instr, instr_pos = m_instr in
  match instr with
  | Com.Affectation m_form ->
      let m_forms = expand_formula const_map [] m_form in
      List.fold_left
        (fun res f -> (Com.Affectation f, instr_pos) :: res)
        prev m_forms
  | Com.IfThenElse (expr, ithen, ielse) ->
      let expr' = expand_expression const_map ParamsMap.empty expr in
      let ithen' = expand_instructions const_map ithen in
      let ielse' = expand_instructions const_map ielse in
      (Com.IfThenElse (expr', ithen', ielse'), instr_pos) :: prev
  | Com.WhenDoElse (wdl, ed) ->
      let map (expr, dl, pos) =
        let expr' = expand_expression const_map ParamsMap.empty expr in
        let dl' = expand_instructions const_map dl in
        (expr', dl', pos)
      in
      let wdl' = List.map map wdl in
      let ed' = Pos.map_under_mark (expand_instructions const_map) ed in
      (Com.WhenDoElse (wdl', ed'), instr_pos) :: prev
  | Com.Print (std, pr_args) ->
      let pr_args' =
        List.map
          (fun arg ->
            match Pos.unmark arg with
            | Com.PrintEventName (expr, f, i) ->
                let expr' = expand_expression const_map ParamsMap.empty expr in
                (Com.PrintEventName (expr', f, i), Pos.get_position arg)
            | Com.PrintEventAlias (expr, f, i) ->
                let expr' = expand_expression const_map ParamsMap.empty expr in
                (Com.PrintEventAlias (expr', f, i), Pos.get_position arg)
            | Com.PrintIndent expr ->
                let expr' = expand_expression const_map ParamsMap.empty expr in
                (Com.PrintIndent expr', Pos.get_position arg)
            | Com.PrintExpr (expr, mi, ma) ->
                let expr' = expand_expression const_map ParamsMap.empty expr in
                (Com.PrintExpr (expr', mi, ma), Pos.get_position arg)
            | Com.PrintString _ | Com.PrintName _ | Com.PrintAlias _ -> arg)
          pr_args
      in
      (Com.Print (std, pr_args'), instr_pos) :: prev
  | Com.Iterate (name, vars, var_params, instrs) ->
      let var_params' =
        List.map
          (fun (cats, expr) ->
            let expr' = expand_expression const_map ParamsMap.empty expr in
            (cats, expr'))
          var_params
      in
      let instrs' = expand_instructions const_map instrs in
      (Com.Iterate (name, vars, var_params', instrs'), instr_pos) :: prev
  | Com.Iterate_values (name, var_intervals, instrs) ->
      let var_intervals' =
        List.map
          (fun (e0, e1, step) ->
            let e0' = expand_expression const_map ParamsMap.empty e0 in
            let e1' = expand_expression const_map ParamsMap.empty e1 in
            let step' = expand_expression const_map ParamsMap.empty step in
            (e0', e1', step'))
          var_intervals
      in
      let instrs' = expand_instructions const_map instrs in
      (Com.Iterate_values (name, var_intervals', instrs'), instr_pos) :: prev
  | Com.Restore (vars, var_params, evts, instrs) ->
      let var_params' =
        List.map
          (fun (v, c, e) ->
            let e' = expand_expression const_map ParamsMap.empty e in
            (v, c, e'))
          var_params
      in
      let evts' = List.map (expand_expression const_map ParamsMap.empty) evts in
      let instrs' = expand_instructions const_map instrs in
      (Com.Restore (vars, var_params', evts', instrs'), instr_pos) :: prev
  | Com.ArrangeEvents (sort, filter, instrs) ->
      let sort' =
        match sort with
        | Some (var0, var1, expr) ->
            let expr' = expand_expression const_map ParamsMap.empty expr in
            Some (var0, var1, expr')
        | None -> None
      in
      let filter' =
        match filter with
        | Some (var, expr) ->
            let expr' = expand_expression const_map ParamsMap.empty expr in
            Some (var, expr')
        | None -> None
      in
      let instrs' = expand_instructions const_map instrs in
      (Com.ArrangeEvents (sort', filter', instrs'), instr_pos) :: prev
  | Com.VerifBlock instrs ->
      let instrs' = expand_instructions const_map instrs in
      (Com.VerifBlock instrs', instr_pos) :: prev
  | Com.ComputeTarget (tn, targs) ->
      let map var =
        match expand_variable const_map ParamsMap.empty var with
        | Com.Var v, v_pos -> (v, v_pos)
        | Com.Literal (Com.Float _), v_pos ->
            Err.constant_forbidden_as_arg v_pos
        | _ -> assert false
      in
      let targs' = List.map map targs in
      (Com.ComputeTarget (tn, targs'), instr_pos) :: prev
  | Com.ComputeVerifs _ | Com.ComputeDomain _ | Com.ComputeChaining _
  | Com.RaiseError _ | Com.CleanErrors | Com.ExportErrors | Com.FinalizeErrors
    ->
      (instr, instr_pos) :: prev

and expand_instructions (const_map : const_context)
    (instrs : Mast.instruction Pos.marked list) :
    Mast.instruction Pos.marked list =
  List.fold_left (expand_instruction const_map) [] (List.rev instrs)

let elim_constants_and_loops (p : Mast.program) : Mast.program =
  let _, expanded_prog =
    List.fold_left
      (fun (const_map, prog) source_file ->
        let const_map, prog_file =
          List.fold_left
            (fun (const_map, prog_file) source_item ->
              let item, pos_item = source_item in
              match item with
              | Mast.VariableDecl var_decl -> (
                  match var_decl with
                  | Mast.ConstVar (m_name, m_cval) ->
                      let const_map = add_const m_name m_cval const_map in
                      (const_map, prog_file)
                  | Mast.ComputedVar (cvar, pos_cvar) ->
                      let comp_table =
                        expand_table_size const_map cvar.Mast.comp_table
                      in
                      let var_decl' =
                        Mast.ComputedVar
                          ({ cvar with Mast.comp_table }, pos_cvar)
                      in
                      let prog_file =
                        (Mast.VariableDecl var_decl', pos_item) :: prog_file
                      in
                      (const_map, prog_file)
                  | _ -> (const_map, source_item :: prog_file))
              | Mast.Rule rule ->
                  let rule_tmp_vars =
                    StrMap.map
                      (fun (name, tsz) ->
                        (name, expand_table_size const_map tsz))
                      rule.Mast.rule_tmp_vars
                  in
                  let rule_formulaes =
                    expand_instructions const_map rule.Mast.rule_formulaes
                  in
                  let rule' =
                    { rule with Mast.rule_tmp_vars; Mast.rule_formulaes }
                  in
                  let prog_file = (Mast.Rule rule', pos_item) :: prog_file in
                  (const_map, prog_file)
              | Mast.Verification verif ->
                  let verif_conditions =
                    List.map
                      (fun (cond, cond_pos) ->
                        let verif_cond_expr =
                          expand_expression const_map ParamsMap.empty
                            cond.Mast.verif_cond_expr
                        in
                        ({ cond with Mast.verif_cond_expr }, cond_pos))
                      verif.Mast.verif_conditions
                  in
                  let verif' = { verif with Mast.verif_conditions } in
                  let prog_file =
                    (Mast.Verification verif', pos_item) :: prog_file
                  in
                  (const_map, prog_file)
              | Mast.Target target ->
                  let target_tmp_vars =
                    StrMap.map
                      (fun (name, tsz) ->
                        (name, expand_table_size const_map tsz))
                      target.Mast.target_tmp_vars
                  in
                  let target_prog =
                    expand_instructions const_map target.Mast.target_prog
                  in
                  let target' =
                    { target with Mast.target_tmp_vars; Mast.target_prog }
                  in
                  let prog_file =
                    (Mast.Target target', pos_item) :: prog_file
                  in
                  (const_map, prog_file)
              | Mast.Function target ->
                  let target_tmp_vars =
                    StrMap.map
                      (fun (name, tsz) ->
                        (name, expand_table_size const_map tsz))
                      target.Mast.target_tmp_vars
                  in
                  let target_prog =
                    expand_instructions const_map target.Mast.target_prog
                  in
                  let target' =
                    { target with Mast.target_tmp_vars; Mast.target_prog }
                  in
                  let prog_file =
                    (Mast.Function target', pos_item) :: prog_file
                  in
                  (const_map, prog_file)
              | _ -> (const_map, source_item :: prog_file))
            (const_map, []) source_file
        in
        (const_map, List.rev prog_file :: prog))
      (ConstMap.empty, []) p
  in
  List.rev expanded_prog

(** Main preprocessor function *)
let proceed (p : Mast.program) : Mast.program =
  p |> elim_unselected_apps |> elim_constants_and_loops

(* Screugneugneuh ! *)
let _ = ignore (format_loop_context, format_loop_domain)
