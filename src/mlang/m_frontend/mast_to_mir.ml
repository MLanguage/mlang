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

module ConstMap = Map.Make (String)

(** {2 Loop translation context} *)

module ParamsMap = struct
  include Map.Make (Char)

  let map_printer value_printer fmt map =
    Format.fprintf fmt "{ %a }"
      (fun fmt ->
        iter (fun k v -> Format.fprintf fmt "%c=%a; " k value_printer v))
      map
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
  ParamsMap.map_printer format_loop_param_value fmt ld

let _format_loop_domain fmt (ld : loop_domain) =
  ParamsMap.map_printer
    (Format_mast.pp_print_list_comma format_loop_param_value)
    fmt ld

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
  | v :: rest -> (
      let max_rest = list_max_execution_number rest in
      match
        Mir.(
          max_exec_number v.Mir.Variable.execution_number
            max_rest.Mir.Variable.execution_number)
      with
      | Mir.Left -> v
      | Mir.Right -> max_rest)

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

module IntMap = Map.Make (Int)

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

(** The M language added a new feature in its 2017 edition : you can specify
    loop variable ranges bounds with constant variables. Because we need the
    actual value of the bounds to unroll everything, this function queries the
    const value in the context if needed. *)
let var_or_int_value (ctx : translating_context) (l : Mast.literal Pos.marked) :
    int =
  match Pos.unmark l with
  | Mast.Variable v -> (
      try
        (* We look up the value of the variable, which has to be const *)
        let name = Mast.get_variable_name v in
        ConstMap.find name ctx.const_map |> Pos.unmark |> int_of_float
      with Not_found ->
        Errors.raise_spanned_error
          (Format.asprintf
             "variable %s is not an integer constant and cannot be used here"
             (Mast.get_variable_name v))
          (Pos.get_position l))
  | Mast.Float f -> int_of_float f

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
      fun translator ->
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
                       | Mast.Interval (l1, l2) ->
                           make_int_range_list (var_or_int_value ctx l1)
                             (var_or_int_value ctx l2))
                     values)
              in
              let sz = loop_variables_size values pos in
              let values = List.map (fun v -> (v, sz)) values in
              ParamsMap.add (Pos.unmark param) values domain)
            ParamsMap.empty lvs
        in
        let _, t_list =
          List.fold_left
            (fun (i, t_list) lc ->
              let new_t = translator lc i in
              (i + 1, new_t :: t_list))
            (0, [])
            (iterate_all_combinations varying_domain)
        in
        List.rev t_list

(**{2 Variables}*)

(** Variables are tricky to translate; indeed, we have unrolled all the loops,
    and generic variables depend on the loop parameters. We have to interrogate
    the loop context for the current values of the loop parameter and then
    replace *inside the string* the loop parameter by its value to produce the
    new variable. *)

(** A variable whose name is [X] should be translated as the generic table index
    expression. However sometimes there is a variable called [X] (yes...) so if
    there is no loop in the context we return the variable [X] for ["X"]. *)
let get_var_or_x (d : Mir.Variable.t list Pos.VarNameToID.t)
    (exec_number : Mir.execution_number) (name : Mast.variable_name Pos.marked)
    (is_lvalue : bool) (in_table : bool) (lax : bool) : Mir.expression =
  if Pos.unmark name = "X" && in_table then Mir.GenericTableIndex
  else if lax then Mir.Var (get_var_from_name_lax d name exec_number is_lvalue)
  else Mir.Var (get_var_from_name d name exec_number is_lvalue)

(** The M language has a weird and very annoying "feature", which is that you
    can define the following loop:

    {v sum(i=05,06,07:Xi) v}

    In this example, what is [Xi] supposed to become ? [X05] ? [X5] ? The answer
    in the actual M codebase is: it depends. Indeed, sometimes [X05] is defines,
    sometimes it is [X5], and sometimes the [i=05,...] will have trailing
    zeroes, and sometimes not. So we have to try all combinations of trailing
    zeroes and find one where everything is correctly defined.

    It is unclear why this behavior is accepted by the M language. Maybe it has
    to do with the way a string to integer function works inside the official
    interpreter... *)

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

let belongs_to_iliad_app (r : Mast.application Pos.marked list) : bool =
  List.exists (fun app -> Pos.unmark app = "iliad") r

(** Retrieves variable declaration data. Done in a separate pass because wen
    don't want to deal with sorting the dependencies between files or inside
    files. *)
let get_variables_decl (p : Mast.program)
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
                      | Some (_f, old_pos) ->
                          Cli.var_info_print
                            "Dropping declaration of %s %a because constant \
                             was previously defined %a"
                            (Pos.unmark var_name) Pos.format_position
                            (Pos.get_position var_name)
                            Pos.format_position old_pos;
                          (vars, idmap, errors, out_list)
                      | None ->
                          let old_var =
                            List.hd
                              (Pos.VarNameToID.find (Pos.unmark var_name) idmap)
                          in
                          Cli.var_info_print
                            "Dropping declaration of %s %a because variable \
                             was previously defined %a"
                            (Pos.unmark var_name) Pos.format_position
                            (Pos.get_position var_name)
                            Pos.format_position
                            (Pos.get_position old_var.Mir.Variable.name);
                          (vars, idmap, errors, out_list)
                    with Not_found -> (
                      match var_decl with
                      | Mast.ComputedVar cvar ->
                          let cvar = Pos.unmark cvar in
                          let attrs =
                            ( Pos.same_pos_as "calculee" cvar.Mast.comp_name,
                              Pos.same_pos_as (Mast.Float 1.)
                                cvar.Mast.comp_name )
                            :: cvar.comp_attributes
                          in
                          let attrs =
                            if
                              List.exists
                                (fun x -> Pos.unmark x = Mast.Base)
                                cvar.Mast.comp_subtyp
                            then
                              ( Pos.same_pos_as "base" cvar.Mast.comp_name,
                                Pos.same_pos_as (Mast.Float 1.)
                                  cvar.Mast.comp_name )
                              :: attrs
                            else attrs
                          in
                          let new_var =
                            Mir.Variable.new_var cvar.Mast.comp_name None
                              cvar.Mast.comp_description
                              (dummy_exec_number
                                 (Pos.get_position cvar.Mast.comp_name))
                              ~attributes:attrs ~is_income:false ~origin:None
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
                            if
                              List.exists
                                (fun x ->
                                  match Pos.unmark x with
                                  | Mast.GivenBack -> true
                                  | Mast.Base -> false)
                                cvar.Mast.comp_subtyp
                            then cvar.Mast.comp_name :: out_list
                            else out_list
                          in
                          (new_vars, new_idmap, errors, new_out_list)
                      | Mast.InputVar ivar ->
                          let ivar = Pos.unmark ivar in
                          let new_var =
                            Mir.Variable.new_var ivar.Mast.input_name
                              (Some (Pos.unmark ivar.Mast.input_alias))
                              ivar.Mast.input_description
                              (dummy_exec_number
                                 (Pos.get_position ivar.Mast.input_name))
                              ~attributes:ivar.input_attributes ~origin:None
                              ~is_income:
                                (Pos.unmark ivar.input_subtyp = Mast.Income)
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
                                  | None -> (
                                      match
                                        Pos.unmark ivar.Mast.input_subtyp
                                      with
                                      | Mast.Income -> Some Mast.Real
                                      | _ -> None)
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

(** Call it with
    [translate_variable idmap exec_number table_definition lc var lax]. SSA is
    all about assigning the correct variable assignment instance when a variable
    is used somewhere. That is why [translate_variable] needs the
    [execution_number]. [table_definition] is needed because the M language has
    a special ["X"] variable (generic table index) that has to be distinguished
    from a variable simply named ["X"]. [lc] is the loop context and the thing
    that complicates this function the most: variables used inside loops have
    loop parameters that have to be instantiated to give a normal variable name.
    [var] is the main argument that you want to translate. [lax] is a special
    argument for SSA construction that, if sets to [true], allows
    [translate_variable] to return a variable assigned exactly at [exec_number]
    (otherwise it always return a variable) in another rule or before in the
    same rule. *)
let rec translate_variable (idmap : Mir.idmap)
    (exec_number : Mir.execution_number)
    (const_map : float Pos.marked ConstMap.t) (table_definition : bool)
    (lc : loop_context option) (var : Mast.variable Pos.marked)
    (is_lvalue : bool) (lax : bool) : Mir.expression Pos.marked =
  match Pos.unmark var with
  | Mast.Normal name -> begin
      match ConstMap.find_opt name const_map with
      | Some (f, _pos) -> Pos.same_pos_as (Mir.Literal (Float f)) var
      | None ->
          Pos.same_pos_as
            (get_var_or_x idmap exec_number (Pos.same_pos_as name var) is_lvalue
               table_definition lax)
            var
    end
  | Mast.Generic gen_name -> (
      if List.length gen_name.Mast.parameters == 0 then
        translate_variable idmap exec_number const_map table_definition lc
          (Pos.same_pos_as (Mast.Normal gen_name.Mast.base) var)
          is_lvalue lax
      else
        match lc with
        | None ->
            Errors.raise_spanned_error
              "variable contains loop parameters but is not used inside a loop \
               context"
              (Pos.get_position var)
        | Some lc ->
            instantiate_generic_variables_parameters idmap exec_number const_map
              table_definition lc gen_name is_lvalue (Pos.get_position var) lax)

(** The following function deal with the "trying all cases" pragma *)
and instantiate_generic_variables_parameters (idmap : Mir.idmap)
    (exec_number : Mir.execution_number)
    (const_map : float Pos.marked ConstMap.t) (table_definition : bool)
    (lc : loop_context) (gen_name : Mast.variable_generic_name)
    (is_lvalue : bool) (pos : Pos.t) (lax : bool) : Mir.expression Pos.marked =
  instantiate_generic_variables_parameters_aux idmap exec_number const_map
    table_definition lc gen_name.Mast.base is_lvalue pos lax

and instantiate_generic_variables_parameters_aux (idmap : Mir.idmap)
    (exec_number : Mir.execution_number)
    (const_map : float Pos.marked ConstMap.t) (table_definition : bool)
    (lc : loop_context) (var_name : string) (is_lvalue : bool) (pos : Pos.t)
    (lax : bool) : Mir.expression Pos.marked =
  match ParamsMap.choose_opt lc with
  | None ->
      translate_variable idmap exec_number const_map table_definition (Some lc)
        (Mast.Normal var_name, pos)
        is_lvalue lax
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
      instantiate_generic_variables_parameters_aux idmap exec_number const_map
        table_definition
        (ParamsMap.remove param lc)
        new_var_name is_lvalue pos lax

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
    ~attributes:var.attributes ~origin ~is_income:var.is_income
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
                let rule_number = Mast.rule_number r.Mast.rule_name in
                if not (belongs_to_iliad_app r.Mast.rule_applications) then
                  idmap
                else
                  fst
                    (List.fold_left
                       (fun (idmap, seq_number) formula ->
                         match Pos.unmark formula with
                         | Mast.SingleFormula f ->
                             let exec_number =
                               {
                                 Mir.rule_number;
                                 Mir.seq_number;
                                 Mir.pos = Pos.get_position f.Mast.lvalue;
                               }
                             in
                             let lvar =
                               match
                                 Pos.unmark
                                   (translate_variable idmap exec_number
                                      const_map
                                      ((Pos.unmark f.Mast.lvalue).Mast.index
                                     <> None)
                                      None (Pos.unmark f.Mast.lvalue).Mast.var
                                      true false)
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
                             let exec_number =
                               {
                                 Mir.rule_number;
                                 Mir.seq_number;
                                 Mir.pos = Pos.get_position f.Mast.lvalue;
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
                               let lvar =
                                 match
                                   Pos.unmark
                                     (translate_variable idmap exec_number
                                        const_map
                                        ((Pos.unmark f.Mast.lvalue).Mast.index
                                       <> None)
                                        (Some lc)
                                        (Pos.unmark f.Mast.lvalue).Mast.var
                                        false false)
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
        translate_variable ctx.idmap ctx.exec_number ctx.const_map
          ctx.table_definition ctx.lc (Pos.same_pos_as v i) false false
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
  Pos.same_pos_as
    (match Pos.unmark f with
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
                        translate_variable ctx.idmap ctx.exec_number
                          ctx.const_map ctx.table_definition ctx.lc set_var
                          false false )
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
                     Pos.same_pos_as equal_test f,
                     or_chain ))
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
        let new_e1 = translate_expression ctx e1 in
        let new_e2 = translate_expression ctx e2 in
        Mir.Binop (op, new_e1, new_e2)
    | Mast.Unop (op, e) ->
        let new_e = translate_expression ctx e in
        Mir.Unop (op, new_e)
    | Mast.Index (t, i) ->
        let t_var =
          translate_variable ctx.idmap ctx.exec_number ctx.const_map
            ctx.table_definition ctx.lc t false false
        in
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
              translate_variable ctx.idmap ctx.exec_number ctx.const_map
                ctx.table_definition ctx.lc (Pos.same_pos_as var f) false false
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
          (Mir.Literal Mir.false_literal) loop_exprs)
    f

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
type index_def = NoIndex | SingleIndex of int | GenericIndex

(** Translates lvalues into the assigning variable as well as the type of
    assignment *)
let translate_lvalue (ctx : translating_context) (lval : Mast.lvalue Pos.marked)
    : translating_context * Mir.Variable.t * index_def =
  let var =
    match
      Pos.unmark
        (translate_variable ctx.idmap ctx.exec_number ctx.const_map
           ctx.table_definition ctx.lc (Pos.unmark lval).Mast.var true true)
    with
    | Mir.Var (var : Mir.Variable.t) -> var
    | _ -> assert false
    (* should not happen *)
  in
  match (Pos.unmark lval).Mast.index with
  | Some ti -> (
      match Pos.unmark ti with
      | Mast.SymbolIndex (Mast.Normal "X") ->
          ({ ctx with table_definition = true }, var, GenericIndex)
      | Mast.LiteralIndex i -> (ctx, var, SingleIndex i)
      | Mast.SymbolIndex (Mast.Normal _ as v) ->
          let i = var_or_int_value ctx (Pos.same_pos_as (Mast.Variable v) ti) in
          (ctx, var, SingleIndex i)
      | Mast.SymbolIndex (Mast.Generic _ as v) ->
          let mir_v =
            translate_variable ctx.idmap ctx.exec_number ctx.const_map
              ctx.table_definition ctx.lc (Pos.same_pos_as v ti) true false
          in
          let i =
            match Pos.unmark mir_v with
            | Mir.Var v ->
                var_or_int_value ctx
                  (Pos.same_pos_as
                     (Mast.Variable
                        (Mast.Normal (Pos.unmark v.Mir.Variable.name)))
                     ti)
            | Mir.Literal (Float f) -> int_of_float f
            | _ -> assert false
            (* should not happen*)
          in
          (ctx, var, SingleIndex i))
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
      (match decl_data.var_decl_typ with
      | Some x -> Some (x, decl_data.var_pos)
      | None -> None)
  in
  Mir.VariableMap.add var_lvalue
    (try
       let io =
         match decl_data.var_decl_io with
         | Input -> Mir.Input
         | Regular -> Mir.Regular
         | Output -> Mir.Output
       in
       if def_kind = NoIndex then
         {
           Mir.var_definition = Mir.SimpleVar var_expr;
           Mir.var_typ;
           Mir.var_io = io;
         }
       else
         match decl_data.var_decl_is_table with
         | Some size -> (
             match def_kind with
             | NoIndex -> assert false (* should not happen*)
             | SingleIndex i ->
                 {
                   Mir.var_definition =
                     Mir.TableVar
                       (size, Mir.IndexTable (Mir.IndexMap.singleton i var_expr));
                   Mir.var_typ;
                   Mir.var_io = io;
                 }
             | GenericIndex ->
                 {
                   Mir.var_definition =
                     Mir.TableVar (size, Mir.IndexGeneric var_expr);
                   Mir.var_typ;
                   Mir.var_io = io;
                 })
         | None ->
             Errors.raise_multispanned_error
               (Format.asprintf
                  "variable %s is defined as a table but has been declared as \
                   a non-table"
                  (Pos.unmark var_lvalue.Mir.Variable.name))
               [
                 (Some "variable definition", Pos.get_position var_expr);
                 ( Some "variable declaration",
                   (Mir.VariableMap.find var_lvalue var_decl_data).var_pos );
               ]
     with Not_found ->
       assert false
       (* should not happen *)
       (* should not happen since we already looked into idmap to get the var
          value from its name *))
    var_data

(** Main translation pass that deal with regular variable definition; returns a
    map whose keys are the variables being defined (with the execution number
    corresponding to the place where it is defined) and whose values are the
    expressions corresponding to the definitions. *)
let get_rules_and_var_data (idmap : Mir.idmap)
    (var_decl_data : var_decl_data Mir.VariableMap.t)
    (const_map : float Pos.marked ConstMap.t) (p : Mast.program) :
    (Mir.Variable.t list * Mast.rule_name) Mir.RuleMap.t
    * Mir.variable_data Mir.VariableMap.t =
  List.fold_left
    (fun (rule_data, var_data) source_file ->
      List.fold_left
        (fun (rule_data, var_data) source_file_item ->
          match Pos.unmark source_file_item with
          | Mast.Rule r ->
              let rule_number = Mast.rule_number r.Mast.rule_name in
              if not (belongs_to_iliad_app r.Mast.rule_applications) then
                (rule_data, var_data)
              else
                let rule_vars, var_data, _ =
                  List.fold_left
                    (fun (rule_vars, var_data, seq_number) formula ->
                      match Pos.unmark formula with
                      | Mast.SingleFormula f ->
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
                                  Mir.pos = Pos.get_position f.Mast.lvalue;
                                };
                            }
                          in
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
                                  Mir.pos = Pos.get_position f.Mast.lvalue;
                                };
                            }
                          in
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
                                    Mir.rule_number;
                                    Mir.seq_number = seq_number + idx;
                                    Mir.pos = Pos.get_position f.Mast.lvalue;
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
                let rule = (List.rev rule_vars, r.rule_name) in
                (Mir.RuleMap.add (Mir.fresh_rule_id ()) rule rule_data, var_data)
          | Mast.VariableDecl (Mast.ConstVar _) ->
              (* constant variables occurences are substituted by their
                 values *)
              (rule_data, var_data)
          | Mast.VariableDecl (Mast.InputVar (var, pos)) ->
              let var =
                get_var_from_name_lax idmap var.Mast.input_name
                  (dummy_exec_number pos) false
              in
              let var_decl =
                try Mir.VariableMap.find var var_decl_data
                with Not_found -> assert false
                (* should not happen *)
              in
              let typ =
                translate_value_typ
                  (match var_decl.var_decl_typ with
                  | Some x -> Some (x, Pos.get_position var.Mir.Variable.name)
                  | None -> None)
              in
              let var_data =
                Mir.VariableMap.add var
                  {
                    Mir.var_definition = Mir.InputVar;
                    Mir.var_typ = typ;
                    Mir.var_io = Mir.Input;
                  }
                  var_data
              in
              (rule_data, var_data)
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
      let add_var var decl var_data =
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
          var_data
      in
      (* The variable has not been defined in a rule *)
      match decl.var_decl_io with
      | Input -> add_var var decl var_data
      | Output | Regular ->
          if
            List.for_all Mir.is_dummy_variable
              (Pos.VarNameToID.find (Pos.unmark var.Mir.Variable.name) idmap)
          then
            Cli.var_info_print
              "variable %s declared %a is never defined in the application"
              (Pos.unmark var.Mir.Variable.name)
              Pos.format_position
              (Pos.get_position var.Mir.Variable.name);
          add_var var decl var_data)
    var_decl_data var_data

let get_conds (error_decls : Mir.Error.t list)
    (const_map : float Pos.marked ConstMap.t) (idmap : Mir.idmap)
    (p : Mast.program) : Mir.condition_data Mir.VariableMap.t =
  List.fold_left
    (fun conds source_file ->
      List.fold_left
        (fun conds source_file_item ->
          match Pos.unmark source_file_item with
          | Mast.Verification verif
            when belongs_to_iliad_app verif.Mast.verif_applications ->
              let rule_number = Mast.verification_number verif.verif_name in
              List.fold_left
                (fun conds verif_cond ->
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
                  let err =
                    let err_name, err_var =
                      (Pos.unmark verif_cond).Mast.verif_cond_error
                    in
                    try
                      ( List.find
                          (fun e ->
                            Pos.unmark e.Mir.Error.name = Pos.unmark err_name)
                          error_decls,
                        Option.map
                          (fun v ->
                            List.sort
                              (fun v w ->
                                -Mir.compare_execution_number
                                   v.Mir.Variable.execution_number
                                   w.Mir.Variable.execution_number)
                              (Pos.VarNameToID.find (Pos.unmark v) idmap)
                            |> List.hd)
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
                      ~attributes:[] ~origin:None ~is_income:false
                      ~is_table:None
                  in
                  Mir.VariableMap.add dummy_var
                    { Mir.cond_expr = e; Mir.cond_error = err }
                    conds)
                conds verif.Mast.verif_conditions
          | _ -> conds)
        conds source_file)
    Mir.VariableMap.empty p

let remove_corrective_rules (p : Mast.program) : Mast.program =
  List.map
    (fun source_file ->
      List.filter
        (fun source_file_item ->
          match Pos.unmark source_file_item with
          | Mast.Rule rule ->
              not
                (List.exists
                   (fun part -> Pos.unmark part = "corrective")
                   rule.rule_name)
          | _ -> true)
        source_file)
    p

let translate (p : Mast.program) : Mir.program =
  let p = remove_corrective_rules p in
  let const_map = get_constants p in
  let var_decl_data, error_decls, idmap = get_variables_decl p const_map in
  let idmap = get_var_redefinitions p idmap const_map in
  let rule_data, var_data =
    get_rules_and_var_data idmap var_decl_data const_map p
  in
  let var_data =
    add_dummy_definitions_for_variable_declarations var_data var_decl_data idmap
  in
  let rules, rule_vars =
    Mir.RuleMap.fold
      (fun rule_id (rule_vars, rule_name) (rules, vars) ->
        let rule_vars, vars =
          List.fold_left
            (fun (rule_vars, vars) var ->
              ( (var.Mir.Variable.id, Mir.VariableMap.find var var_data)
                :: rule_vars,
                Mir.VariableDict.add var vars ))
            ([], vars) (List.rev rule_vars)
        in
        let rule_number, rule_tags =
          Mir.rule_number_and_tags_of_rule_name rule_name
        in
        ( Mir.RuleMap.add rule_id Mir.{ rule_vars; rule_number; rule_tags } rules,
          vars ))
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
      Mir.{ rule_vars = orphans; rule_number = (0, Pos.no_pos); rule_tags = [] }
      rules
  in
  let conds = get_conds error_decls const_map idmap p in
  {
    Mir.program_vars = var_data;
    Mir.program_rules = rules;
    Mir.program_conds = conds;
    Mir.program_idmap = idmap;
    Mir.program_exec_passes = [];
  }
