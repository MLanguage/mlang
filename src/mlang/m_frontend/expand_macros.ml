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
end

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
  ParamsMap.pp (Format_mast.pp_print_list_comma format_loop_param_value) fmt ld

let add_const (name, name_pos) (cval, cval_pos) const_map =
  match ConstMap.find_opt name const_map with
  | Some (_, old_pos) -> Err.constant_already_defined old_pos name_pos
  | None -> (
      match cval with
      | Mast.Float f -> ConstMap.add name (f, name_pos) const_map
      | Mast.Variable (Mast.Normal const) -> (
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
      | Some (f, _) -> (Mast.Literal (Float f), var_pos)
      | None -> (Mast.Literal (Variable var), var_pos))
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
    (m_litt : Mast.literal Pos.marked) : var_or_int_index =
  match Pos.unmark m_litt with
  | Mast.Variable v -> (
      let name = Mast.get_variable_name v in
      match ConstMap.find_opt name const_map with
      | Some (fvalue, _) -> IntIndex (int_of_float fvalue)
      | None -> VarIndex v)
  | Mast.Float f -> IntIndex (int_of_float f)
  | Mast.Undefined -> assert false

let var_or_int (m_lit : Mast.literal Pos.marked) =
  let lit, lit_pos = m_lit in
  match lit with
  | Mast.Float f -> RangeInt (int_of_float f)
  | Mast.Variable (Normal v) -> VarName v
  | Mast.Variable (Generic _) ->
      Err.generic_variable_not_allowed_in_left_part_of_loop lit_pos
  | Mast.Undefined -> assert false

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

let make_range_list (l1 : Mast.literal Pos.marked)
    (l2 : Mast.literal Pos.marked) : loop_param_value list =
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
        if List.length all_contexts = 0 then [ ParamsMap.singleton param hd ]
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

let expand_loop_variables (lvs : Mast.loop_variables Pos.marked)
    (const_map : const_context) : (loop_context -> 'a) -> 'a list =
  let pos = Pos.get_position lvs in
  match Pos.unmark lvs with
  | Mast.ValueSets lvs | Mast.Ranges lvs ->
      let varying_domain =
        List.fold_left
          (fun domain (param, values) ->
            let values =
              List.flatten
                (List.map
                   (fun value ->
                     match value with
                     | Mast.Single l -> [ var_or_int l ]
                     | Mast.Range (l1, l2) -> make_range_list l1 l2
                     | Mast.Interval (l1, l2) -> (
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
  let expr, expr_pos = m_expr in
  match expr with
  | Mast.TestInSet (positive, e, values) ->
      let e' = expand_expression const_map loop_map e in
      let values' =
        List.map
          (fun set_value ->
            match set_value with
            | Mast.VarValue set_var -> (
                match expand_variable const_map loop_map set_var with
                | Mast.Literal (Float f), var_pos -> Mast.FloatValue (f, var_pos)
                | Mast.Literal (Variable var), var_pos ->
                    Mast.VarValue (var, var_pos)
                | _ -> assert false)
            | Mast.FloatValue _ | Mast.Interval _ -> set_value)
          values
      in
      (Mast.TestInSet (positive, e', values'), expr_pos)
  | Mast.Comparison (op, e1, e2) ->
      let e1' = expand_expression const_map loop_map e1 in
      let e2' = expand_expression const_map loop_map e2 in
      (Mast.Comparison (op, e1', e2'), expr_pos)
  | Mast.Binop (op, e1, e2) ->
      let e1' = expand_expression const_map loop_map e1 in
      let e2' = expand_expression const_map loop_map e2 in
      (Mast.Binop (op, e1', e2'), expr_pos)
  | Mast.Unop (op, e) ->
      let e' = expand_expression const_map loop_map e in
      (Mast.Unop (op, e'), expr_pos)
  | Mast.Index (t, i) ->
      let t' =
        match expand_variable const_map loop_map t with
        | Mast.Literal (Variable v), v_pos -> (v, v_pos)
        | Mast.Literal (Float _), v_pos -> Err.constant_forbidden_as_table v_pos
        | _ -> assert false
      in
      let i' = expand_expression const_map loop_map i in
      (Mast.Index (t', i'), expr_pos)
  | Mast.Conditional (e1, e2, e3_opt) ->
      let e1' = expand_expression const_map loop_map e1 in
      let e2' = expand_expression const_map loop_map e2 in
      let e3_opt' =
        match e3_opt with
        | Some e3 -> Some (expand_expression const_map loop_map e3)
        | None -> None
      in
      (Mast.Conditional (e1', e2', e3_opt'), expr_pos)
  | Mast.FunctionCall (f_name, args) ->
      let args' = expand_func_args const_map loop_map args in
      (Mast.FunctionCall (f_name, args'), expr_pos)
  | Mast.Literal l -> (
      match l with
      | Mast.Variable var -> expand_variable const_map loop_map (var, expr_pos)
      | _ -> m_expr)
  | Mast.Loop (lvs, e) ->
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
          (Mast.Binop ((Mast.Or, expr_pos), res, loop_expr), expr_pos))
        (Mast.Literal (Float 0.0), expr_pos)
        loop_exprs
  | Mast.Attribut (var, a) -> (
      match expand_variable const_map loop_map var with
      | Mast.Literal (Variable v), v_pos ->
          (Mast.Attribut ((v, v_pos), a), expr_pos)
      | Mast.Literal (Float _), v_pos ->
          Err.constant_cannot_have_an_attribut v_pos
      | _ -> assert false)
  | Mast.Size var -> (
      match expand_variable const_map loop_map var with
      | Mast.Literal (Variable v), v_pos -> (Mast.Size (v, v_pos), expr_pos)
      | Mast.Literal (Float _), v_pos -> Err.constant_cannot_have_a_size v_pos
      | _ -> assert false)
  | Mast.NbCategory _ | Mast.NbAnomalies | Mast.NbDiscordances
  | Mast.NbInformatives | Mast.NbBloquantes ->
      m_expr

and expand_func_args (const_map : const_context) (loop_map : loop_context)
    (args : Mast.func_args) : Mast.func_args =
  match args with
  | Mast.ArgList args ->
      let args' =
        List.map (fun arg -> expand_expression const_map loop_map arg) args
      in
      Mast.ArgList args'
  | Mast.LoopList (lvs, e) ->
      let loop_context_provider = expand_loop_variables lvs const_map in
      let translator lmap =
        let loop_map =
          merge_loop_context loop_map lmap (Pos.get_position lvs)
        in
        expand_expression const_map loop_map e
      in
      Mast.ArgList (loop_context_provider translator)

(** Translates lvalues into the assigning variable as well as the type of
    assignment *)
let expand_lvalue (const_map : const_context) (loop_map : loop_context)
    (m_lval : Mast.lvalue Pos.marked) : Mast.lvalue Pos.marked =
  let lval, lval_pos = m_lval in
  let var =
    match expand_variable const_map loop_map lval.Mast.var with
    | Mast.Literal (Variable v), v_pos -> (v, v_pos)
    | Mast.Literal (Float _), v_pos -> Err.constant_forbidden_as_lvalue v_pos
    | _ -> assert false
  in
  let index =
    Option.map (expand_expression const_map loop_map) lval.Mast.index
  in
  (Mast.{ var; index }, lval_pos)

let expand_formula (const_map : const_context)
    (prev : Mast.formula Pos.marked list) (m_form : Mast.formula Pos.marked) :
    Mast.formula Pos.marked list =
  let form, form_pos = m_form in
  match form with
  | Mast.SingleFormula f ->
      let lvalue = expand_lvalue const_map ParamsMap.empty f.Mast.lvalue in
      let formula =
        expand_expression const_map ParamsMap.empty f.Mast.formula
      in
      (Mast.SingleFormula { lvalue; formula }, form_pos) :: prev
  | Mast.MultipleFormulaes (lvs, f) ->
      (* Format.eprintf "%a\n\n" Format_mast.format_formula form;*)
      let loop_context_provider = expand_loop_variables lvs const_map in
      let translator loop_map =
        let lvalue = expand_lvalue const_map loop_map f.Mast.lvalue in
        let formula = expand_expression const_map loop_map f.Mast.formula in
        (Mast.SingleFormula { lvalue; formula }, form_pos)
      in
      let res = loop_context_provider translator in
      (* List.iter (fun (f, _) -> Format.eprintf "res %a\n"
         Format_mast.format_formula f) res; Format.eprintf "\n";*)
      List.rev res @ prev

let expand_formulaes (const_map : const_context)
    (forms : Mast.formula Pos.marked list) : Mast.formula Pos.marked list =
  List.fold_left (expand_formula const_map) [] (List.rev forms)

let rec expand_instruction (const_map : const_context)
    (prev : Mast.instruction Pos.marked list)
    (m_instr : Mast.instruction Pos.marked) : Mast.instruction Pos.marked list =
  let instr, instr_pos = m_instr in
  match instr with
  | Mast.Formula m_form ->
      let m_forms = expand_formula const_map [] m_form in
      List.fold_left
        (fun res f -> (Mast.Formula f, instr_pos) :: res)
        prev m_forms
  | Mast.IfThenElse (expr, ithen, ielse) ->
      let expr' = expand_expression const_map ParamsMap.empty expr in
      let ithen' = expand_instructions const_map ithen in
      let ielse' = expand_instructions const_map ielse in
      (Mast.IfThenElse (expr', ithen', ielse'), instr_pos) :: prev
  | Mast.Print (std, pr_args) ->
      let pr_args' =
        List.map
          (fun arg ->
            match Pos.unmark arg with
            | Mast.PrintIndent expr ->
                let expr' = expand_expression const_map ParamsMap.empty expr in
                (Mast.PrintIndent expr', Pos.get_position arg)
            | Mast.PrintExpr (expr, mi, ma) ->
                let expr' = expand_expression const_map ParamsMap.empty expr in
                (Mast.PrintExpr (expr', mi, ma), Pos.get_position arg)
            | Mast.PrintString _ | Mast.PrintName _ | Mast.PrintAlias _ -> arg)
          pr_args
      in
      (Mast.Print (std, pr_args'), instr_pos) :: prev
  | Mast.Iterate (name, cats, expr, instrs) ->
      let expr' = expand_expression const_map ParamsMap.empty expr in
      let instrs' = expand_instructions const_map instrs in
      (Mast.Iterate (name, cats, expr', instrs'), instr_pos) :: prev
  | Mast.Restore (vars, instrs) ->
      let instrs' = expand_instructions const_map instrs in
      (Mast.Restore (vars, instrs'), instr_pos) :: prev
  | Mast.VerifBlock instrs ->
      let instrs' = expand_instructions const_map instrs in
      (Mast.VerifBlock instrs', instr_pos) :: prev
  | Mast.ComputeVerifs _ | Mast.ComputeDomain _ | Mast.ComputeChaining _
  | Mast.ComputeTarget _ | Mast.RaiseError _ | Mast.CleanErrors
  | Mast.ExportErrors | Mast.FinalizeErrors ->
      (instr, instr_pos) :: prev

and expand_instructions (const_map : const_context)
    (instrs : Mast.instruction Pos.marked list) :
    Mast.instruction Pos.marked list =
  List.fold_left (expand_instruction const_map) [] (List.rev instrs)

(** Eliminates constants and loops *)
let proceed (p : Mast.program) : Mast.program =
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
                  let rule_formulaes =
                    expand_formulaes const_map rule.Mast.rule_formulaes
                  in
                  let rule' = { rule with Mast.rule_formulaes } in
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
              | _ -> (const_map, source_item :: prog_file))
            (const_map, []) source_file
        in
        (const_map, List.rev prog_file :: prog))
      (ConstMap.empty, []) p
  in
  List.rev expanded_prog

(* Screugneugneuh ! *)
let _ = ignore (format_loop_context, format_loop_domain)
