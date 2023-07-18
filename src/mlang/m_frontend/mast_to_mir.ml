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

(** Program input/output *)
type io_status =
  | Input
  | Output
  | Regular  (** Computed from other variables but not output *)

type var_decl_data = {
  var_decl_typ : Mast.value_typ option;
  var_decl_is_table : int option;
  var_decl_descr : string option;
  var_decl_io : io_status;
  var_pos : Pos.t;
}
(** Intermediate container for variable declaration info *)

module ConstMap = StrMap

(** {2 Loop translation context} *)

module ParamsMap = struct
  include CharMap

  let pp ?(sep = "; ") ?(pp_key = Format.pp_print_char) ?(assoc = "=")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

type loop_param_value = VarName of Mast.variable_name | RangeInt of int

let format_loop_param_value fmt (v : loop_param_value * int) =
  match fst v with
  | VarName v -> Format.fprintf fmt "%s" v
  | RangeInt i -> Format.fprintf fmt "%d" i

type loop_context = (loop_param_value * int) ParamsMap.t
(** Maps loop variables to their values ; the integer represents the expected
    length of the string representation of the value *)

type loop_domain = (loop_param_value * int) list ParamsMap.t
(** Loops can have multiple loop parameters *)

let _format_loop_context fmt (ld : loop_context) =
  ParamsMap.pp format_loop_param_value fmt ld

let _format_loop_domain fmt (ld : loop_domain) =
  ParamsMap.pp (Format_mast.pp_print_list_comma format_loop_param_value) fmt ld

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

(** Helper to make a list of integers from an integer range *)
let rec make_int_range_list (i1 : int) (i2 : int) : loop_param_value list =
  if i1 > i2 then []
  else
    let tl = make_int_range_list (i1 + 1) i2 in
    RangeInt i1 :: tl

(** {2 General translation context} *)

type translating_context = {
  table_definition : bool;
      (** [true] if translating an expression susceptible to contain a generic
          table index *)
  idmap : Mir.idmap;  (** Current string-to-{!type: Mir.Variable.t} mapping *)
  lc : loop_context option;  (** Current loop translation context *)
  const_map : float Pos.marked ConstMap.t;
      (** Mapping from constant variables to their value *)
  exec_number : Mir.execution_number;
      (** Number of the rule of verification condition being translated *)
}
(** This context will be passed along during the translation *)

let dummy_exec_number (pos : Pos.t) : Mir.execution_number =
  { Mir.rule_number = -1; Mir.seq_number = 0; pos }

(** When entering a loop, you are provided with a new loop context that you have
    to integrate to the general context with this function. The [position]
    argument is used to print an error message in case of the same loop
    parameters used in nested loops. *)
let merge_loop_ctx (ctx : translating_context) (new_lc : loop_context)
    (pos : Pos.t) : translating_context =
  match ctx.lc with
  | None -> { ctx with lc = Some new_lc }
  | Some old_lc ->
      let merged_lc =
        ParamsMap.merge
          (fun param old_val new_val ->
            match (old_val, new_val) with
            | Some _, Some _ ->
                Errors.raise_spanned_error
                  (Format.asprintf
                     "Same loop parameter %c used in two nested loop contexts"
                     param)
                  pos
            | Some v, None | None, Some v -> Some v
            | None, None -> assert false
            (* should not happen *))
          old_lc new_lc
      in
      { ctx with lc = Some merged_lc }

(** Helper to compute the max SSA candidate in a list *)
let rec list_max_execution_number (l : Mir.Variable.t list) : Mir.Variable.t =
  match l with
  | [] -> raise Not_found
  | [ v ] -> v
  | v :: rest ->
      let max_rest = list_max_execution_number rest in
      if
        Mir.(
          compare_execution_number v.Mir.Variable.execution_number
            max_rest.Mir.Variable.execution_number)
        < 0
      then max_rest
      else v

(** Given a list of candidates for an SSA variable query, returns the correct
    one: the maximum in the same rule or if no candidates in the same rule, the
    maximum in other rules. *)
let find_var_among_candidates (exec_number : Mir.execution_number)
    (l : Mir.Variable.t list) : Mir.Variable.t =
  let same_rule =
    List.filter
      (fun var ->
        var.Mir.Variable.execution_number.Mir.rule_number
        = exec_number.Mir.rule_number)
      l
  in
  if List.length same_rule = 0 then list_max_execution_number l
  else list_max_execution_number same_rule

(** Implementation of legacy hack to use TGV variables as reusable local
    variables *)
let is_vartmp (v : Mir.Variable.t) =
  let vartmp_pattern = "VARTMP" in
  try
    String.sub (Pos.unmark v.Mir.name) 0 (String.length vartmp_pattern)
    |> String.equal vartmp_pattern
  with Invalid_argument _ -> false

let get_var_from_name (d : Mir.Variable.t list Pos.VarNameToID.t)
    (name : Mast.variable_name Pos.marked) (exec_number : Mir.execution_number)
    (is_lvalue : bool) : Mir.Variable.t =
  try
    let same_name = Pos.VarNameToID.find (Pos.unmark name) d in
    let candidate_list =
      List.filter
        (fun var ->
          Mir.is_candidate_valid var.Mir.Variable.execution_number exec_number
            is_lvalue)
        same_name
    in
    (* If there is no candidate in the same rule and more one than more
       candidate outside of the (-1) declaration stub that have to different
       rule numbers, then we throw an error because it breaks an M invariant *)
    (if
     (not is_lvalue)
     && not
          (List.exists
             (fun var ->
               var.Mir.Variable.execution_number.rule_number
               = exec_number.rule_number)
             candidate_list)
    then
     let rules_containing_candidates : Mir.Variable.t IntMap.t =
       List.fold_left
         (fun acc var ->
           if var.Mir.Variable.execution_number.rule_number <> -1 then
             IntMap.add var.Mir.Variable.execution_number.rule_number var acc
           else acc)
         IntMap.empty candidate_list
     in
     if IntMap.cardinal rules_containing_candidates > 2 then
       Errors.print_spanned_warning
         "A variable is used with multiple candidates for its previous \
          definition. Please initialize the variable in the rule before using \
          it."
         (Pos.get_position name));
    (* If the above check passes, we select the right candidate *)
    find_var_among_candidates exec_number candidate_list
  with Not_found ->
    Errors.raise_spanned_error
      (Format.asprintf "variable %s has not been declared" (Pos.unmark name))
      (Pos.get_position name)

(** Same but also take into account variables defined in the same execution unit *)
let get_var_from_name_lax (d : Mir.Variable.t list Pos.VarNameToID.t)
    (name : Mast.variable_name Pos.marked) (exec_number : Mir.execution_number)
    (using_var_in_def : bool) : Mir.Variable.t =
  try
    let same_name = Pos.VarNameToID.find (Pos.unmark name) d in
    find_var_among_candidates exec_number
      (List.filter
         (fun var ->
           Mir.is_candidate_valid var.Mir.Variable.execution_number exec_number
             using_var_in_def
           || Mir.same_execution_number var.Mir.Variable.execution_number
                exec_number)
         same_name)
  with Not_found ->
    Errors.raise_spanned_error
      (Format.asprintf "variable %s has not been declared" (Pos.unmark name))
      (Pos.get_position name)

(**{1 Translation}*)

(**{2 Loops}*)

type var_or_int_index = VarIndex of Mast.variable | IntIndex of int

(** The M language added a new feature in its 2017 edition : you can specify
    loop variable ranges bounds with constant variables. Because we need the
    actual value of the bounds to unroll everything, this function queries the
    const value in the context if needed. Otherwise, it might be a dynamic
    index. *)
let var_or_int_value (ctx : translating_context) (l : Mast.literal Pos.marked) :
    var_or_int_index =
  match Pos.unmark l with
  | Mast.Variable v -> (
      try
        (* We look up the value of the variable, which may be const *)
        let name = Mast.get_variable_name v in
        IntIndex (ConstMap.find name ctx.const_map |> Pos.unmark |> int_of_float)
      with Not_found -> VarIndex v)
  | Mast.Float f -> IntIndex (int_of_float f)

let loop_variables_size (lpvl : loop_param_value list) (pos : Pos.t) =
  let size_err p =
    Errors.raise_spanned_error "loop variables have different sizes" p
  in
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

let var_or_int (l : Mast.literal Pos.marked) =
  match Pos.unmark l with
  | Mast.Float f -> RangeInt (int_of_float f)
  | Mast.Variable (Normal v) -> VarName v
  | Mast.Variable (Generic _) ->
      Errors.raise_spanned_error
        "generic variables not allowed in left part of loop"
        (Pos.get_position l)

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
    Errors.raise_spanned_error
      "non-numeric range bounds must consist of a single character" p
  in
  match (var_or_int l1, var_or_int l2) with
  | RangeInt i1, RangeInt i2 -> make_int_range_list i1 i2
  | VarName v1, VarName v2 ->
      if String.length v1 <> 1 then length_err (Pos.get_position l1);
      if String.length v2 <> 1 then length_err (Pos.get_position l2);
      make_var_range_list v1 v2
  | _ ->
      Errors.raise_spanned_error "range bounds must be of the same type"
        (Pos.get_position l2)

(** This function is the workhorse of loop unrolling : it takes a loop prefix
    containing the set of variables over which to iterate, and fabricates a
    combinator. This combinator takes auser-provided way of translating the loop
    body generically over the values of the iterated variables, and produce a
    list corresponding of the unrolled loop bodies expressions containing the
    iterated values.

    In OCaml terms, if you want [translate_loop_variables lvs f ctx], then you
    should define [f] by [let f = fun lc i ctx -> ...] and use {!val:
    merge_loop_ctx} inside [...] before translating the loop body. [lc] is the
    loop context, [i] the loop sequence index and [ctx] the translation context. *)

let translate_loop_variables (lvs : Mast.loop_variables Pos.marked)
    (ctx : translating_context) : (loop_context -> int -> 'a) -> 'a list =
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
                         let lb = var_or_int_value ctx l1 in
                         let ub = var_or_int_value ctx l2 in
                         match (lb, ub) with
                         | VarIndex v, _ | _, VarIndex v ->
                             Errors.raise_spanned_error
                               (Format.asprintf
                                  "variable %s is not an integer constant and \
                                   cannot be used here"
                                  (Mast.get_variable_name v))
                               pos
                         | IntIndex lb, IntIndex ub -> make_int_range_list lb ub
                         ))
                   values)
            in
            let sz = loop_variables_size values pos in
            let values = List.map (fun v -> (v, sz)) values in
            ParamsMap.add (Pos.unmark param) values domain)
          ParamsMap.empty lvs
      in
      let loop_ctx = iterate_all_combinations varying_domain in
      fun translator ->
        let _, t_list =
          List.fold_left
            (fun (i, t_list) lc ->
              let new_t = translator lc i in
              (i + 1, new_t :: t_list))
            (0, []) loop_ctx
        in
        List.rev t_list

(**{2 Variables}*)

(** Variables are tricky to translate; indeed, we have unrolled all the loops,
    and generic variables depend on the loop parameters. We have to interrogate
    the loop context for the current values of the loop parameter and then
    replace *inside the string* the loop parameter by its value to produce the
    new variable. *)

let get_var (d : Mir.Variable.t list Pos.VarNameToID.t)
    (exec_number : Mir.execution_number) (name : Mast.variable_name Pos.marked)
    (is_lvalue : bool) (lax : bool) : Mir.expression =
  if lax then Mir.Var (get_var_from_name_lax d name exec_number is_lvalue)
  else Mir.Var (get_var_from_name d name exec_number is_lvalue)

(** The M language has a weird and very annoying "feature", which is that you
    can define the following loop:

    {v sum(i=05,06,07:Xi) v}

    The legacy compiler enforce the following: All possible value of [i] must
    have their string representations have the same length. In such loops we can
    mix characters, strings and integers, the former two will define strictly
    that length while integer will settle on their shortest representation.

    In thes example above, [Xi] will become [X5] because there are no string or
    integer above 9 in the list of possible values. *)

(**{2 Preliminary passes}*)

(** Gets constant values. Done in a separate pass because constant variables are
    substituted everywhere by their defined value. *)
let get_constants (p : Mast.program) : float Pos.marked ConstMap.t =
  List.fold_left
    (fun const_map source_file ->
      List.fold_left
        (fun const_map source_file_item ->
          match Pos.unmark source_file_item with
          | Mast.VariableDecl var_decl -> (
              match var_decl with
              | Mast.ConstVar (marked_name, cval) -> (
                  try
                    let _old_const, old_pos =
                      ConstMap.find (Pos.unmark marked_name) const_map
                    in
                    Cli.var_info_print
                      "Dropping declaration of constant variable %s %a because \
                       constant was previously defined %a"
                      (Pos.unmark marked_name) Pos.format_position
                      (Pos.get_position marked_name)
                      Pos.format_position old_pos;
                    const_map
                  with Not_found -> (
                    match Pos.unmark cval with
                    | Mast.Float f ->
                        ConstMap.add (Pos.unmark marked_name)
                          (f, Pos.get_position marked_name)
                          const_map
                    | _ -> const_map))
              | _ -> const_map)
          | _ -> const_map)
        const_map source_file)
    ConstMap.empty p

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
                  | Some (_decl, pos) ->
                      Cli.warning_print
                        "Category \"%s\" defined more than once:@;\
                         Already defined %a"
                        (String.concat " "
                           (Format_mast.format_var_type normalized_decl.var_type
                           :: List.map Pos.unmark normalized_decl.var_category))
                        Pos.format_position pos
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

let get_var_category_map (p : Mast.program) :
    Pos.t StrMap.t Pos.marked Mir.CatVarMap.t =
  List.fold_left
    (fun cats source_file ->
      List.fold_left
        (fun cats source_file_item ->
          match Pos.unmark source_file_item with
          | Mast.VarCatDecl (catDecl, posDecl) -> (
              let attributes =
                List.fold_left
                  (fun res (str, pos) ->
                    match StrMap.find_opt str res with
                    | None -> StrMap.add str pos res
                    | Some posAttr ->
                        Errors.raise_spanned_error
                          (Format.asprintf
                             "attribute \"%s\" defined more than once:@;\
                              Already defined %a" str Pos.format_position
                             posAttr)
                          pos)
                  StrMap.empty catDecl.var_attributes
              in
              let add_cat cat cats =
                match Mir.CatVarMap.find_opt cat cats with
                | None -> Mir.CatVarMap.add cat (attributes, posDecl) cats
                | Some (_, pos) ->
                    Errors.raise_spanned_error
                      (Format.asprintf
                         "Category \"%a\" defined more than once:@;\
                          Already defined %a" Mir.pp_cat_variable cat
                         Pos.format_position pos)
                      posDecl
              in
              match catDecl.var_type with
              | Mast.Input ->
                  let id = StrSet.from_marked_list catDecl.var_category in
                  add_cat (Mir.CatInput id) cats
              | Mast.Computed ->
                  let base = Mir.CatCompSet.singleton Base in
                  let givenBack = Mir.CatCompSet.singleton GivenBack in
                  let baseAndGivenBack = base |> Mir.CatCompSet.add GivenBack in
                  cats
                  |> add_cat (Mir.CatComputed Mir.CatCompSet.empty)
                  |> add_cat (Mir.CatComputed base)
                  |> add_cat (Mir.CatComputed givenBack)
                  |> add_cat (Mir.CatComputed baseAndGivenBack))
          | _ -> cats)
        cats source_file)
    Mir.CatVarMap.empty p

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
    (categories : Mast.var_category_decl Pos.marked list)
    (const_map : float Pos.marked ConstMap.t) :
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
                      match
                        ConstMap.find_opt (Pos.unmark var_name) const_map
                      with
                      | opt ->
                          let old_pos, kind =
                            match opt with
                            | Some (_f, old_pos) -> (old_pos, "constant")
                            | None ->
                                ( Pos.get_position
                                    (List.hd
                                       (Pos.VarNameToID.find
                                          (Pos.unmark var_name) idmap))
                                      .Mir.Variable.name,
                                  "variable" )
                          in
                          Cli.var_info_print
                            "Dropping declaration of %s %a because %s was \
                             previously defined %a"
                            (Pos.unmark var_name) Pos.format_position
                            (Pos.get_position var_name)
                            kind Pos.format_position old_pos;
                          (vars, idmap, errors, out_list)
                    with Not_found -> (
                      match var_decl with
                      | Mast.ComputedVar cvar ->
                          let cvar = Pos.unmark cvar in
                          let category =
                            Mast.computed_category
                            :: List.map Pos.unmark cvar.comp_category
                          in
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
                              (dummy_exec_number
                                 (Pos.get_position cvar.Mast.comp_name))
                              ~attributes:cvar.comp_attributes ~category
                              ~cats:(Mir.CatVarSet.singleton cat)
                              ~origin:None
                              ~is_table:(Pos.unmark_option cvar.Mast.comp_table)
                          in
                          let new_var_data =
                            {
                              var_decl_typ =
                                Pos.unmark_option cvar.Mast.comp_typ;
                              var_decl_is_table =
                                Pos.unmark_option cvar.Mast.comp_table;
                              var_decl_descr =
                                Some (Pos.unmark cvar.Mast.comp_description);
                              var_decl_io = Regular;
                              var_pos = Pos.get_position source_file_item;
                            }
                          in
                          let new_vars =
                            Mir.VariableMap.add new_var new_var_data vars
                          in
                          let new_idmap =
                            Pos.VarNameToID.add
                              (Pos.unmark cvar.Mast.comp_name)
                              [ new_var ] idmap
                          in
                          let new_out_list =
                            if cvar.Mast.comp_is_givenback then
                              cvar.Mast.comp_name :: out_list
                            else out_list
                          in
                          (new_vars, new_idmap, errors, new_out_list)
                      | Mast.InputVar ivar ->
                          let ivar = Pos.unmark ivar in
                          let category =
                            Mast.input_category
                            :: List.map Pos.unmark ivar.input_category
                          in
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
                              (dummy_exec_number
                                 (Pos.get_position ivar.Mast.input_name))
                              ~attributes:ivar.input_attributes ~origin:None
                              ~category
                              ~cats:(Mir.CatVarSet.singleton cat)
                              ~is_table:None
                            (* Input variables also have a low order *)
                          in
                          let new_var_data =
                            {
                              var_decl_typ =
                                begin
                                  match
                                    Pos.unmark_option ivar.Mast.input_typ
                                  with
                                  | Some x -> Some x
                                  | None ->
                                      if
                                        List.exists
                                          (fun t ->
                                            String.equal Mast.income_category
                                              (Pos.unmark t))
                                          ivar.input_category
                                      then Some Mast.Real
                                      else None
                                end;
                              var_decl_is_table = None;
                              var_decl_descr =
                                Some (Pos.unmark ivar.Mast.input_description);
                              var_decl_io = Input;
                              var_pos = Pos.get_position source_file_item;
                            }
                          in
                          let new_vars =
                            Mir.VariableMap.add new_var new_var_data vars
                          in
                          let new_idmap =
                            Pos.VarNameToID.add
                              (Pos.unmark ivar.Mast.input_name)
                              [ new_var ] idmap
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
          let out_var =
            (get_var_from_name_lax idmap out_name
               (dummy_exec_number (Pos.get_position out_name)))
              false
          in
          let data = Mir.VariableMap.find out_var vars in
          Mir.VariableMap.add out_var { data with var_decl_io = Output } vars
        with Not_found -> assert false
        (* should not happen *))
      vars out_list
  in
  (vars, errors, idmap)

(**{2 SSA construction}*)

(** Call it with [translate_variable ctx var ~is_lvalue ~lax]. SSA is all about
    assigning the correct variable assignment instance when a variable is used
    somewhere. That is why [translate_variable] needs the
    [ctx.execution_number]. [ctx.lc] is the loop context and the thing that
    complicates this function the most: variables used inside loops have loop
    parameters that have to be instantiated to give a normal variable name.
    [var] is the main argument that you want to translate. [lax] is a special
    argument for SSA construction that, if sets to [true], allows
    [translate_variable] to return a variable assigned exactly at
    [ctx.exec_number] (otherwise it always return a variable) in another rule or
    before in the same rule. *)
let rec translate_variable (ctx : translating_context)
    (var : Mast.variable Pos.marked) ~(is_lvalue : bool) ~(lax : bool) :
    Mir.expression Pos.marked =
  match Pos.unmark var with
  | Mast.Normal name -> begin
      match ConstMap.find_opt name ctx.const_map with
      | Some (f, _pos) -> Pos.same_pos_as (Mir.Literal (Float f)) var
      | None ->
          Pos.same_pos_as
            (get_var ctx.idmap ctx.exec_number (Pos.same_pos_as name var)
               is_lvalue lax)
            var
    end
  | Mast.Generic gen_name -> (
      if List.length gen_name.Mast.parameters == 0 then
        translate_variable ctx
          (Pos.same_pos_as (Mast.Normal gen_name.Mast.base) var)
          ~is_lvalue ~lax
      else
        match ctx.lc with
        | None ->
            Errors.raise_spanned_error
              "variable contains loop parameters but is not used inside a loop \
               context"
              (Pos.get_position var)
        | Some _lc ->
            instantiate_generic_variables_parameters ctx gen_name is_lvalue
              (Pos.get_position var) lax)

(** The following function deal with the "trying all cases" pragma *)
and instantiate_generic_variables_parameters (ctx : translating_context)
    (gen_name : Mast.variable_generic_name) (is_lvalue : bool) (pos : Pos.t)
    (lax : bool) : Mir.expression Pos.marked =
  instantiate_generic_variables_parameters_aux ctx gen_name.Mast.base is_lvalue
    pos lax

and instantiate_generic_variables_parameters_aux (ctx : translating_context)
    (var_name : string) (is_lvalue : bool) (pos : Pos.t) (lax : bool) :
    Mir.expression Pos.marked =
  (* [ctx.lc] should not be None here *)
  match ParamsMap.choose_opt (Option.get ctx.lc) with
  | None -> translate_variable ctx (Mast.Normal var_name, pos) ~is_lvalue ~lax
  | Some (param, (value, size)) ->
      let new_var_name =
        match value with
        | VarName value ->
            if size <> String.length value then
              Errors.raise_spanned_error
                "variable name does not match computed size" pos;
            Re.Str.replace_first
              (Re.Str.regexp (Format.asprintf "%c" param))
              value var_name
        | RangeInt i ->
            let is = string_of_int i in
            let l = String.length is in
            if size < l then
              Errors.raise_spanned_error
                "integer representation exceeds computed size" pos;
            let value =
              if l = size then is else String.make (size - l) '0' ^ is
            in
            Re.Str.replace_first
              (Re.Str.regexp (Format.asprintf "%c" param))
              value var_name
      in
      let ctx = { ctx with lc = Option.map (ParamsMap.remove param) ctx.lc } in
      instantiate_generic_variables_parameters_aux ctx new_var_name is_lvalue
        pos lax

let duplicate_var (var : Mir.Variable.t) (exec_number : Mir.execution_number)
    (idmap : Mir.idmap) : Mir.Variable.t =
  let origin =
    if is_vartmp var then None
    else
      match Pos.VarNameToID.find (Pos.unmark var.name) idmap with
      | [] ->
          Errors.raise_error "Tried to duplicate a variable without declaration"
      | v :: _ -> (
          match v.Mir.Variable.origin with None -> Some v | Some v -> Some v)
    (* invariant : every variables with the same name have the same origin
       (itself have None), with the exception of [VARTMP]s which are used as
       local variables *)
  in
  Mir.Variable.new_var var.name None var.descr exec_number
    ~attributes:var.attributes ~origin ~category:var.category ~cats:var.cats
    ~is_table:var.is_table

(** Linear pass that fills [idmap] with all the variable assignments along with
    their execution number. *)
let get_var_redefinitions (p : Mast.program) (idmap : Mir.idmap)
    (const_map : float Pos.marked ConstMap.t) : Mir.idmap =
  let idmap =
    List.fold_left
      (fun (idmap : Mir.idmap) source_file ->
        List.fold_left
          (fun (idmap : Mir.idmap) source_file_item ->
            match Pos.unmark source_file_item with
            | Mast.Rule r ->
                let rule_number = Pos.unmark r.Mast.rule_number in
                if not (belongs_to_iliad_app r.Mast.rule_applications) then
                  idmap
                else
                  fst
                    (List.fold_left
                       (fun (idmap, seq_number) formula ->
                         let exec_number =
                           {
                             Mir.rule_number;
                             Mir.seq_number;
                             Mir.pos = Pos.get_position formula;
                           }
                         in
                         let ctx =
                           {
                             idmap;
                             lc = None;
                             const_map;
                             table_definition = false;
                             exec_number;
                           }
                         in
                         match Pos.unmark formula with
                         | Mast.SingleFormula f ->
                             let lvar =
                               match
                                 Pos.unmark
                                   (translate_variable ctx
                                      (Pos.unmark f.Mast.lvalue).Mast.var
                                      ~is_lvalue:true ~lax:false)
                               with
                               | Mir.Var var -> var
                               | _ -> assert false
                               (* should not happen *)
                             in
                             let new_var =
                               duplicate_var lvar exec_number idmap
                             in
                             let new_idmap =
                               Pos.VarNameToID.add
                                 (Pos.unmark lvar.Mir.Variable.name)
                                 (new_var
                                 :: Pos.VarNameToID.find
                                      (Pos.unmark lvar.Mir.Variable.name)
                                      idmap)
                                 idmap
                             in
                             (new_idmap, seq_number + 1)
                         | Mast.MultipleFormulaes (lvs, f) ->
                             let loop_context_provider =
                               translate_loop_variables lvs ctx
                             in
                             let translator lc idx =
                               let exec_number =
                                 {
                                   exec_number with
                                   Mir.seq_number = seq_number + idx;
                                 }
                               in
                               let ctx =
                                 { ctx with lc = Some lc; exec_number }
                               in
                               let lvar =
                                 match
                                   Pos.unmark
                                     (translate_variable ctx
                                        (Pos.unmark f.Mast.lvalue).Mast.var
                                        ~is_lvalue:false ~lax:false)
                                 with
                                 | Mir.Var var -> var
                                 | _ -> assert false
                                 (* should not happen *)
                               in
                               let new_var =
                                 duplicate_var lvar exec_number idmap
                               in
                               (Pos.unmark lvar.Mir.Variable.name, new_var)
                             in
                             let new_var_defs =
                               loop_context_provider translator
                             in
                             List.fold_left
                               (fun (idmap, seq_number) (var_name, var_def) ->
                                 ( Pos.VarNameToID.add var_name
                                     (var_def
                                     :: Pos.VarNameToID.find var_name idmap)
                                     idmap,
                                   seq_number + 1 ))
                               (idmap, seq_number) new_var_defs)
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
      let var =
        translate_variable ctx (Pos.same_pos_as v i) ~is_lvalue:false ~lax:false
      in
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
  | x ->
      Errors.raise_spanned_error
        (Format.asprintf "unknown function %s" x)
        (Pos.get_position f_name)

let rec translate_expression (ctx : translating_context)
    (f : Mast.expression Pos.marked) : Mir.expression Pos.marked =
  let expr =
    match Pos.unmark f with
    | Mast.TestInSet (positive, e, values) ->
        let new_e = translate_expression ctx e in
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
                        translate_variable ctx set_var ~is_lvalue:false
                          ~lax:false )
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
        let new_e1 = translate_expression ctx e1 in
        let new_e2 = translate_expression ctx e2 in
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
        let new_e1 = translate_expression ctx e1 in
        let new_e2 = translate_expression ctx e2 in
        Mir.Binop (op, new_e1, new_e2)
    | Mast.Unop (op, e) ->
        let new_e = translate_expression ctx e in
        Mir.Unop (op, new_e)
    | Mast.Index (t, i) ->
        let t_var = translate_variable ctx t ~is_lvalue:false ~lax:false in
        let new_i = translate_table_index ctx i in
        Mir.Index
          ( (match Pos.unmark t_var with
            | Mir.Var v -> Pos.same_pos_as v t_var
            | _ -> assert false (* should not happen *)),
            new_i )
    | Mast.Conditional (e1, e2, e3) ->
        let new_e1 = translate_expression ctx e1 in
        let new_e2 = translate_expression ctx e2 in
        let new_e3 =
          match e3 with
          | Some e3 -> translate_expression ctx e3
          | None -> Pos.same_pos_as (Mir.Literal Mir.Undefined) e2
          (* the absence of a else branch for a ternary operators can yield an
             undefined term *)
        in
        Mir.Conditional (new_e1, new_e2, new_e3)
    | Mast.FunctionCall (f_name, args) ->
        let f_correct = translate_function_name f_name in
        let new_args = translate_func_args ctx args in
        Mir.FunctionCall (f_correct, new_args)
    | Mast.Literal l -> (
        match l with
        | Mast.Variable var ->
            let new_var =
              translate_variable ctx (Pos.same_pos_as var f) ~is_lvalue:false
                ~lax:false
            in
            Pos.unmark new_var
        | Mast.Float f -> Mir.Literal (Mir.Float f))
    (* These loops correspond to "pour un i dans ...: ... so it's OR "*)
    | Mast.Loop (lvs, e) ->
        let loop_context_provider = translate_loop_variables lvs ctx in
        let translator lc _ =
          let new_ctx = merge_loop_ctx ctx lc (Pos.get_position lvs) in
          translate_expression new_ctx e
        in
        let loop_exprs = loop_context_provider translator in
        List.fold_left
          (fun acc loop_expr ->
            Mir.Binop
              (Pos.same_pos_as Mast.Or e, Pos.same_pos_as acc e, loop_expr))
          (Mir.Literal Mir.false_literal) loop_exprs
  in
  Pos.same_pos_as expr f

(** Mutually recursive with {!val: translate_expression} *)
and translate_func_args (ctx : translating_context) (args : Mast.func_args) :
    Mir.expression Pos.marked list =
  match args with
  | Mast.ArgList args -> List.map (fun arg -> translate_expression ctx arg) args
  | Mast.LoopList (lvs, e) ->
      let loop_context_provider = translate_loop_variables lvs ctx in
      let translator lc _ =
        let new_ctx = merge_loop_ctx ctx lc (Pos.get_position lvs) in
        translate_expression new_ctx e
      in
      loop_context_provider translator

(** {2 Translation of source file items}*)

(** Helper type to indicate the kind of variable assignment *)
type index_def = NoIndex | ConstIndex of int | DynamicIndex of Mir.variable

let translate_index_def (ctx : translating_context)
    ((v, pos) : Mast.variable Pos.marked) : translating_context * index_def =
  match var_or_int_value ctx (Mast.Variable v, pos) with
  | IntIndex i -> (ctx, ConstIndex i)
  | VarIndex v -> (
      match translate_variable ctx (v, pos) ~is_lvalue:true ~lax:true with
      | Mir.Var v, _ -> (ctx, DynamicIndex v)
      | _ -> assert false)

(** Translates lvalues into the assigning variable as well as the type of
    assignment *)
let translate_lvalue (ctx : translating_context) (lval : Mast.lvalue Pos.marked)
    : translating_context * Mir.Variable.t * index_def =
  let var =
    match
      Pos.unmark
        (translate_variable ctx (Pos.unmark lval).Mast.var ~is_lvalue:true
           ~lax:true)
    with
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
      | Mast.SymbolIndex (Mast.Generic _ as v) ->
          let mir_v =
            translate_variable ctx (Pos.same_pos_as v ti) ~is_lvalue:true
              ~lax:false
          in
          let ctx, index_def =
            match Pos.unmark mir_v with
            | Mir.Var v ->
                translate_index_def ctx
                  (Pos.same_pos_as
                     (Mast.Normal (Pos.unmark v.Mir.Variable.name))
                     ti)
            | Mir.Literal (Float f) -> (ctx, ConstIndex (int_of_float f))
            | _ -> assert false
            (* should not happen*)
          in
          (ctx, var, index_def))
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

(** Main toplevel declaration translator that adds a variable definition to the
    MIR program *)
let add_var_def (var_data : Mir.variable_data Mir.VariableMap.t)
    (var_lvalue : Mir.Variable.t) (var_expr : Mir.expression Pos.marked)
    (def_kind : index_def) (var_decl_data : var_decl_data Mir.VariableMap.t)
    (idmap : Mir.idmap) : Mir.variable_data Mir.VariableMap.t =
  let var_at_declaration =
    List.find
      (fun var ->
        Mir.(
          same_execution_number var.Mir.Variable.execution_number
            (dummy_exec_number (Pos.get_position var_expr))))
      (Pos.VarNameToID.find (Pos.unmark var_lvalue.name) idmap)
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
  let vdata =
    let var_io =
      match decl_data.var_decl_io with
      | Input -> Mir.Input
      | Regular -> Mir.Regular
      | Output -> Mir.Output
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
              Mir.var_io;
            }
        | DynamicIndex v ->
            {
              Mir.var_definition =
                Mir.TableVar (size, Mir.IndexGeneric (v, var_expr));
              Mir.var_typ;
              Mir.var_io;
            })
    | None ->
        if def_kind = NoIndex then
          {
            Mir.var_definition = Mir.SimpleVar var_expr;
            Mir.var_typ;
            Mir.var_io;
          }
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
                (* should not happen since we already looked into idmap to get
                   the var value from its name *) );
            ]
  in
  Mir.VariableMap.add var_lvalue vdata var_data

let get_domains (cat_str : string)
    (get_item : Mast.source_file_item -> ('a Mast.domain_decl * 'b) option)
    (p : Mast.program) : 'b Mir.domain Mast.DomainIdMap.t =
  let fold_items (domains, synonyms, by_default) marked_item =
    match get_item (Pos.unmark marked_item) with
    | Some (decl, dom_data) ->
        let dom_names = Mast.DomainIdSet.from_marked_list_list decl.dom_names in
        let dom_id = Mast.DomainIdSet.min_elt dom_names in
        let domain =
          Mir.
            {
              dom_id;
              dom_names;
              dom_by_default = decl.dom_by_default;
              dom_min = Mast.DomainIdSet.from_marked_list_list decl.dom_parents;
              dom_max = Mast.DomainIdSet.empty;
              dom_data;
            }
        in
        let domains = Mast.DomainIdMap.add dom_id domain domains in
        let fold syn sl =
          let name = Mast.DomainId.from_marked_list (Pos.unmark sl) in
          if Mast.DomainIdMap.mem name syn then
            let msg =
              Format.sprintf "there is already a %s domain with this name"
                cat_str
            in
            Errors.raise_spanned_error msg (Pos.get_position sl)
          else Mast.DomainIdMap.add name dom_id syn
        in
        let synonyms = List.fold_left fold synonyms decl.dom_names in
        let by_default =
          if decl.dom_by_default then
            match by_default with
            | None -> Some dom_id
            | _ ->
                let msg =
                  Format.sprintf "there is already a default %s domain" cat_str
                in
                Errors.raise_spanned_error msg (Pos.get_position marked_item)
          else by_default
        in
        (domains, synonyms, by_default)
    | None -> (domains, synonyms, by_default)
  in
  let fold_sources doms source = List.fold_left fold_items doms source in
  let domains, synonyms, by_default =
    List.fold_left fold_sources
      (Mast.DomainIdMap.empty, Mast.DomainIdMap.empty, None)
      p
  in
  let get_dom id dom =
    Mast.DomainIdMap.find (Mast.DomainIdMap.find id synonyms) dom
  in
  let domains =
    let rec set_min id dom (visiting, visited, doms) =
      if Mast.DomainIdSet.mem id visited then (visiting, visited, doms)
      else if Mast.DomainIdSet.mem id visiting then
        Errors.raise_error
          (Format.sprintf "there is a loop in the %s domain hierarchy" cat_str)
      else
        let visiting = Mast.DomainIdSet.add id visiting in
        let visiting, visited, doms =
          let parentMap =
            let fold parentId map =
              let parentDom = get_dom parentId doms in
              let parentId = parentDom.Mir.dom_id in
              Mast.DomainIdMap.add parentId parentDom map
            in
            Mast.DomainIdSet.fold fold dom.Mir.dom_min Mast.DomainIdMap.empty
          in
          Mast.DomainIdMap.fold set_min parentMap (visiting, visited, doms)
        in
        let dom_min =
          let fold parentId res =
            let parentDom = get_dom parentId doms in
            let parentId = parentDom.Mir.dom_id in
            Mast.DomainIdSet.singleton parentId
            |> Mast.DomainIdSet.union parentDom.Mir.dom_min
            |> Mast.DomainIdSet.union res
          in
          Mast.DomainIdSet.fold fold dom.Mir.dom_min Mast.DomainIdSet.empty
        in
        let dom = Mir.{ dom with dom_min } in
        let doms = Mast.DomainIdMap.add id dom doms in
        let visiting = Mast.DomainIdSet.remove id visiting in
        let visited = Mast.DomainIdSet.add id visited in
        (visiting, visited, doms)
    in
    let init = (Mast.DomainIdSet.empty, Mast.DomainIdSet.empty, domains) in
    let _, _, domains = Mast.DomainIdMap.fold set_min domains init in
    domains
  in
  let domains =
    let set_max id dom doms =
      let fold minId doms =
        let minDom = Mast.DomainIdMap.find minId doms in
        let dom_max = Mast.DomainIdSet.add id minDom.Mir.dom_max in
        let minDom = Mir.{ minDom with dom_max } in
        Mast.DomainIdMap.add minId minDom doms
      in
      Mast.DomainIdSet.fold fold dom.Mir.dom_min doms
    in
    Mast.DomainIdMap.fold set_max domains domains
  in
  let domains =
    match by_default with
    | Some def_id ->
        let fold _ dom doms =
          let foldName name doms = Mast.DomainIdMap.add name dom doms in
          Mast.DomainIdSet.fold foldName dom.Mir.dom_names doms
        in
        Mast.DomainIdMap.empty
        |> Mast.DomainIdMap.fold fold domains
        |> Mast.DomainIdMap.add Mast.DomainId.empty (get_dom def_id domains)
    | None ->
        Errors.raise_error
          (Format.sprintf "there are no default %s domain" cat_str)
  in
  (* let _ = let iter id dom = let pp_ss fmt ss = let iter s = Format.fprintf
     fmt "<%s> " s in Mast.DomainId.iter iter ss in let pp_sss fmt sss = let
     iter ss = Format.fprintf fmt "%a, " pp_ss ss in Mast.DomainIdSet.iter iter
     sss in Format.printf "XXX %a\n: min: %a\n: max: %a\n" pp_ss id pp_sss
     dom.Mir.dom_min pp_sss dom.Mir.dom_max in Mast.DomainIdMap.iter iter
     domains; exit 0 in *)
  domains

let get_rule_domains (p : Mast.program) : Mir.rule_domain Mast.DomainIdMap.t =
  let get_item = function
    | Mast.RuleDomDecl decl ->
        let dom_data =
          { Mir.rdom_computable = decl.Mast.dom_data.rdom_computable }
        in
        Some (decl, dom_data)
    | _ -> None
  in
  get_domains "rule" get_item p

let get_rule_chains (domains : Mir.rule_domain Mast.DomainIdMap.t)
    (p : Mast.program) : Mir.rule_domain Mast.ChainingMap.t =
  let fold_rules chains marked_item =
    match Pos.unmark marked_item with
    | Mast.Rule r when r.rule_chaining <> None ->
        let ch_name, ch_pos = Option.get r.rule_chaining in
        let rule_domain =
          let dom_id =
            Mast.DomainId.from_marked_list (Pos.unmark r.rule_tag_names)
          in
          Mast.DomainIdMap.find dom_id domains
        in
        let ch_dom =
          match Mast.ChainingMap.find_opt ch_name chains with
          | Some dom -> dom
          | None -> rule_domain
        in
        let rdom_is_min =
          Mast.DomainIdSet.mem rule_domain.dom_id ch_dom.dom_min
        in
        let rdom_is_max =
          Mast.DomainIdSet.mem rule_domain.dom_id ch_dom.dom_max
        in
        let rdom_is_eq = rule_domain.dom_id = ch_dom.dom_id in
        if rdom_is_min || rdom_is_max || rdom_is_eq then
          if not rdom_is_min then
            Mast.ChainingMap.add ch_name rule_domain chains
          else chains
        else
          let msg = "chaining incompatible with rule domain" in
          Errors.raise_spanned_error msg ch_pos
    | _ -> chains
  in
  let fold_sources chains source = List.fold_left fold_rules chains source in
  List.fold_left fold_sources Mast.ChainingMap.empty p

let cats_variable_from_decl_list cats l =
  let rec aux res = function
    | [] -> res
    | Mast.AuthInput id :: t ->
        let vcat = Mir.CatInput (StrSet.from_marked_list (Pos.unmark id)) in
        aux (Mir.CatVarSet.add vcat res) t
    | Mast.AuthComputed id :: t -> begin
        match Pos.unmark id with
        | [] ->
            let res =
              res |> Mir.CatVarSet.add (Mir.CatComputed Mir.CatCompSet.empty)
            in
            aux res t
        | [ ("base", _) ] ->
            let base = Mir.CatCompSet.singleton Mir.Base in
            let res = res |> Mir.CatVarSet.add (Mir.CatComputed base) in
            aux res t
        | [ ("base", _); ("*", _) ] ->
            let base = Mir.CatCompSet.singleton Base in
            let baseAndGivenBack = base |> Mir.CatCompSet.add GivenBack in
            let res =
              res
              |> Mir.CatVarSet.add (Mir.CatComputed base)
              |> Mir.CatVarSet.add (Mir.CatComputed baseAndGivenBack)
            in
            aux res t
        | [ ("restituee", _) ] ->
            let givenBack = Mir.CatCompSet.singleton GivenBack in
            let res = Mir.CatVarSet.add (Mir.CatComputed givenBack) res in
            aux res t
        | [ ("restituee", _); ("*", _) ] ->
            let givenBack = Mir.CatCompSet.singleton GivenBack in
            let baseAndGivenBack = givenBack |> Mir.CatCompSet.add Base in
            let res =
              res
              |> Mir.CatVarSet.add (Mir.CatComputed givenBack)
              |> Mir.CatVarSet.add (Mir.CatComputed baseAndGivenBack)
            in
            aux res t
        | [ ("base", _); ("restituee", _) ] | [ ("restituee", _); ("base", _) ]
          ->
            let baseAndGivenBack =
              Mir.CatCompSet.singleton Base |> Mir.CatCompSet.add GivenBack
            in
            let res =
              res |> Mir.CatVarSet.add (Mir.CatComputed baseAndGivenBack)
            in
            aux res t
        | [ ("*", _) ] ->
            let base = Mir.CatCompSet.singleton Base in
            let givenBack = Mir.CatCompSet.singleton GivenBack in
            let baseAndGivenBack = base |> Mir.CatCompSet.add GivenBack in
            let res =
              res
              |> Mir.CatVarSet.add (Mir.CatComputed Mir.CatCompSet.empty)
              |> Mir.CatVarSet.add (Mir.CatComputed base)
              |> Mir.CatVarSet.add (Mir.CatComputed givenBack)
              |> Mir.CatVarSet.add (Mir.CatComputed baseAndGivenBack)
            in
            aux res t
        | _ ->
            Errors.raise_spanned_error "unlnown calculated variable category"
              (Pos.get_position id)
      end
    | Mast.AuthAll :: t ->
        let res =
          Mir.CatVarMap.fold (fun c _ r -> Mir.CatVarSet.add c r) cats res
        in
        aux res t
  in
  aux Mir.CatVarSet.empty l

let get_verif_domains (cats : 'a Mir.CatVarMap.t) (p : Mast.program) :
    Mir.verif_domain Mast.DomainIdMap.t =
  let get_item = function
    | Mast.VerifDomDecl decl ->
        let catSet =
          cats_variable_from_decl_list cats decl.Mast.dom_data.vdom_auth
        in
        let dom_data =
          {
            Mir.vdom_auth = catSet;
            Mir.vdom_auto_cc = decl.Mast.dom_data.vdom_auto_cc;
          }
        in
        Some (decl, dom_data)
    | _ -> None
  in
  get_domains "verif" get_item p

(** Main translation pass that deal with regular variable definition; returns a
    map whose keys are the variables being defined (with the execution number
    corresponding to the place where it is defined) and whose values are the
    expressions corresponding to the definitions. *)
let get_rules_and_var_data (idmap : Mir.idmap)
    (var_decl_data : var_decl_data Mir.VariableMap.t)
    (const_map : float Pos.marked ConstMap.t) (p : Mast.program) :
    (Mir.Variable.t list
    * Mir.rov_id Pos.marked
    * string Pos.marked list Pos.marked
    * Mast.chaining Pos.marked option)
    Mir.RuleMap.t
    * Mir.variable_data Mir.VariableMap.t =
  List.fold_left
    (fun (rule_data, var_data) source_file ->
      List.fold_left
        (fun (rule_data, var_data) source_file_item ->
          match Pos.unmark source_file_item with
          | Mast.Rule r ->
              let rule_number = Pos.unmark r.Mast.rule_number in
              if not (belongs_to_iliad_app r.Mast.rule_applications) then
                (rule_data, var_data)
              else
                let rule_vars, var_data, _ =
                  List.fold_left
                    (fun (rule_vars, var_data, seq_number) formula ->
                      let ctx =
                        {
                          idmap;
                          lc = None;
                          const_map;
                          table_definition = false;
                          exec_number =
                            {
                              Mir.rule_number;
                              Mir.seq_number;
                              Mir.pos = Pos.get_position formula;
                            };
                        }
                      in
                      match Pos.unmark formula with
                      | Mast.SingleFormula f ->
                          let ctx, var_lvalue, def_kind =
                            translate_lvalue ctx f.Mast.lvalue
                          in
                          let rule_vars = var_lvalue :: rule_vars in
                          let var_expr =
                            translate_expression ctx f.Mast.formula
                          in
                          let var_data =
                            add_var_def var_data var_lvalue var_expr def_kind
                              var_decl_data idmap
                          in
                          (rule_vars, var_data, seq_number + 1)
                      | Mast.MultipleFormulaes (lvs, f) ->
                          let loop_context_provider =
                            translate_loop_variables lvs ctx
                          in
                          let translator lc idx =
                            let new_ctx =
                              {
                                ctx with
                                lc = Some lc;
                                exec_number =
                                  {
                                    ctx.exec_number with
                                    Mir.seq_number = seq_number + idx;
                                  };
                              }
                            in
                            let new_ctx, var_lvalue, def_kind =
                              translate_lvalue new_ctx f.Mast.lvalue
                            in
                            let var_expr =
                              translate_expression new_ctx f.Mast.formula
                            in
                            (var_lvalue, var_expr, def_kind)
                          in
                          let data_to_add = loop_context_provider translator in
                          List.fold_left
                            (fun (rule_vars, var_data, seq_number)
                                 (var_lvalue, var_expr, def_kind) ->
                              let var_data =
                                add_var_def var_data var_lvalue var_expr
                                  def_kind var_decl_data idmap
                              in
                              let rule_vars = var_lvalue :: rule_vars in
                              (rule_vars, var_data, seq_number + 1))
                            (rule_vars, var_data, seq_number)
                            data_to_add)
                    ([], var_data, 0) r.Mast.rule_formulaes
                in
                let rule_number =
                  Pos.map_under_mark (fun n -> Mir.RuleID n) r.rule_number
                in
                let rule =
                  ( List.rev rule_vars,
                    rule_number,
                    r.rule_tag_names,
                    r.rule_chaining )
                in
                ( Mir.RuleMap.add (Pos.unmark rule_number) rule rule_data,
                  var_data )
          | _ -> (rule_data, var_data))
        (rule_data, var_data) source_file)
    (Mir.RuleMap.empty, Mir.VariableMap.empty)
    (List.rev p)

(** At this point [var_data] contains the definition data for all the times a
    variable is defined. However the M language deals with undefined variable,
    so for each variable we have to insert a dummy definition corresponding to
    the declaration and whose value and whose value is found in the TGV at the
    beginning of the execution *)
let add_dummy_definitions_for_variable_declarations
    (var_data : Mir.variable_data Mir.VariableMap.t)
    (var_decl_data : var_decl_data Mir.VariableMap.t) (idmap : Mir.idmap) :
    Mir.variable_data Mir.VariableMap.t =
  Mir.VariableMap.fold
    (fun var decl (var_data : Mir.variable_data Mir.VariableMap.t) ->
      begin
        match decl.var_decl_io with
        | Input -> ()
        | Output | Regular ->
            if
              List.for_all Mir.is_dummy_variable
                (Pos.VarNameToID.find (Pos.unmark var.Mir.Variable.name) idmap)
            then
              Cli.var_info_print
                "variable %s declared %a is never defined in the application"
                (Pos.unmark var.Mir.Variable.name)
                Pos.format_position
                (Pos.get_position var.Mir.Variable.name)
      end;
      Mir.VariableMap.add var
        {
          Mir.var_definition = Mir.InputVar;
          Mir.var_typ =
            translate_value_typ
              (match decl.var_decl_typ with
              | None -> None
              | Some typ -> Some (Pos.same_pos_as typ var.name));
          Mir.var_io = Mir.Input;
        }
        var_data)
    var_decl_data var_data

let get_conds (cats : 'a Mir.CatVarMap.t) (error_decls : Mir.Error.t list)
    (const_map : float Pos.marked ConstMap.t) (idmap : Mir.idmap)
    (p : Mast.program) :
    Mir.verif_domain Mast.DomainIdMap.t * Mir.condition_data Mir.VariableMap.t =
  let verif_domains = get_verif_domains cats p in
  let conds =
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
                        match
                          Mast.DomainIdMap.find_opt vdom_id verif_domains
                        with
                        | Some vdom -> vdom
                        | None ->
                            Errors.raise_spanned_error "Unknown verif domain"
                              (Pos.get_position verif.verif_tag_names)
                      in
                      let e =
                        translate_expression
                          {
                            idmap;
                            lc = None;
                            const_map;
                            table_definition = false;
                            exec_number =
                              {
                                Mir.rule_number;
                                Mir.seq_number = 0;
                                Mir.pos = Pos.get_position verif_cond;
                              };
                          }
                          (Pos.unmark verif_cond).Mast.verif_cond_expr
                      in
                      let category =
                        (* Verifications are maped to a dummy variable, we use
                           it to store all the subtypes of variables appearing
                           in its expression to avoid going through it later
                           when we sort verifications chains out *)
                        Mir.fold_expr_var
                          (fun subtypes var ->
                            List.fold_left
                              (fun subtypes st ->
                                if List.mem st subtypes then subtypes
                                else st :: subtypes)
                              subtypes var.Mir.category)
                          [] (Pos.unmark e)
                      in
                      let cond_cats =
                        Mir.fold_expr_var
                          (fun subtypes (var : Mir.variable) ->
                            Mir.CatVarSet.fold
                              (fun c res ->
                                if
                                  Mir.CatVarSet.mem c
                                    cond_domain.dom_data.vdom_auth
                                then Mir.CatVarSet.add c res
                                else
                                  Errors.raise_error
                                    (Format.asprintf
                                       "forbidden variable \"%s\" of category \
                                        \"%a\" in verif %d of domain \"%a\""
                                       (Pos.unmark var.Mir.name)
                                       Mir.pp_cat_variable c rule_number
                                       (Mast.DomainId.pp ()) cond_domain.dom_id))
                              var.Mir.cats subtypes)
                          Mir.CatVarSet.empty (Pos.unmark e)
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
                              (fun v ->
                                Mir.get_max_var_sorted_by_execution_number
                                  Mir.sort_by_lowest_exec_number (Pos.unmark v)
                                  idmap)
                              err_var )
                        with Not_found ->
                          Errors.raise_error
                            (Format.asprintf "undeclared error %s %a"
                               (Pos.unmark err_name) Pos.format_position
                               (Pos.get_position err_name))
                      in
                      let dummy_var =
                        Mir.Variable.new_var
                          (Pos.same_pos_as
                             (Format.sprintf "verification_condition_%d"
                                (Mir.Variable.fresh_id ()))
                             e)
                          None
                          (Pos.same_pos_as
                             (let () =
                                Pos.format_position Format.str_formatter
                                  (Pos.get_position e)
                              in
                              Format.flush_str_formatter ())
                             e)
                          {
                            Mir.rule_number;
                            Mir.seq_number = 0;
                            Mir.pos = Pos.get_position verif_cond;
                          }
                          ~attributes:[] ~origin:None ~category ~cats:cond_cats
                          ~is_table:None
                      in
                      ( Mir.VariableMap.add dummy_var
                          Mir.
                            {
                              cond_number =
                                Pos.same_pos_as (VerifID rule_number)
                                  verif.verif_number;
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
      Mir.VariableMap.empty p
  in
  (verif_domains, conds)

let translate (p : Mast.program) : Mir.program =
  let const_map = get_constants p in
  let var_category_decls = get_var_categories p in
  let var_category_map = get_var_category_map p in
  let _ =
    Mir.CatVarMap.pp
      (fun fmt (attrs, _) -> StrMap.pp (fun _ _ -> ()) fmt attrs)
      Format.std_formatter var_category_map
  in
  let var_decl_data, error_decls, idmap =
    get_variables_decl p var_category_decls const_map
  in
  let idmap = get_var_redefinitions p idmap const_map in
  let rule_domains = get_rule_domains p in
  let rule_domain_by_default =
    Mast.DomainIdMap.find Mast.DomainId.empty rule_domains
  in
  let rule_chains = get_rule_chains rule_domains p in
  let rule_data, var_data =
    get_rules_and_var_data idmap var_decl_data const_map p
  in
  let var_data =
    add_dummy_definitions_for_variable_declarations var_data var_decl_data idmap
  in
  let rules, rule_vars =
    Mir.RuleMap.fold
      (fun rule_id (rule_vars, rule_number, rule_tag_names, rule_chaining)
           (rules, vars) ->
        let domain_id =
          Mast.DomainId.from_marked_list (Pos.unmark rule_tag_names)
        in
        let rule_domain =
          match Mast.DomainIdMap.find_opt domain_id rule_domains with
          | Some domain -> domain
          | None ->
              Errors.raise_spanned_error "unknown rule domain"
                (Pos.get_position rule_tag_names)
        in
        let rule_chain =
          match rule_chaining with
          | None -> None
          | Some mch ->
              let ch_name = Pos.unmark mch in
              Some (ch_name, Mast.ChainingMap.find ch_name rule_chains)
        in
        let rule_vars, vars =
          List.fold_left
            (fun (rule_vars, vars) var ->
              ( (var.Mir.Variable.id, Mir.VariableMap.find var var_data)
                :: rule_vars,
                Mir.VariableDict.add var vars ))
            ([], vars) (List.rev rule_vars)
        in
        let rule_data =
          Mir.{ rule_domain; rule_chain; rule_vars; rule_number }
        in
        (Mir.RuleMap.add rule_id rule_data rules, vars))
      rule_data
      (Mir.RuleMap.empty, Mir.VariableDict.empty)
  in
  let var_data, orphans =
    Mir.VariableMap.fold
      (fun var data (var_dict, orphans) ->
        let orphans =
          if Mir.VariableDict.mem var rule_vars then orphans
          else (var.Mir.Variable.id, data) :: orphans
        in
        (Mir.VariableDict.add var var_dict, orphans))
      var_data
      (Mir.VariableDict.empty, [])
  in
  let rules =
    Mir.RuleMap.add Mir.initial_undef_rule_id
      Mir.
        {
          rule_domain = rule_domain_by_default;
          rule_chain = None;
          rule_vars = orphans;
          rule_number = (RuleID 0, Pos.no_pos);
        }
      rules
  in
  let verif_domains, conds =
    get_conds var_category_map error_decls const_map idmap p
  in
  Mir.
    {
      program_var_categories = var_category_map;
      program_rule_domains = rule_domains;
      program_verif_domains = verif_domains;
      program_chainings = rule_chains;
      program_vars = var_data;
      program_rules = rules;
      program_conds = conds;
      program_idmap = idmap;
      program_exec_passes = [];
    }
