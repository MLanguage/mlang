(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** {!module: Mast} to {!module: Mir} translation of M programs. *)

(** {1 Translation context} *)

(** {2 Variable declarations}*)

(** Program input/output *)
type io_status =
  | Input
  | Output
  | Constant
  | Regular  (** Computed from other variables but not output *)

type var_decl_data = {
  var_decl_typ : Mast.value_typ option;
  var_decl_is_table : int option;
  var_decl_descr : string option;
  var_decl_io : io_status;
  var_pos : Pos.t;
}
(** Intermediate container for variable declaration info *)

(** {2 Loop translation context} *)

(** The M language has a strange way of doing loops. We translate them by unrolling; but for that we
    need a context to hold the loop parameters, which consists of a mapping from characters to
    integers or other characters. *)

(** Map whose keys are loop parameters *)
module ParamsMap = struct
  include Map.Make (Char)

  let map_printer value_printer fmt map =
    Format.fprintf fmt "{ %a }"
      (fun fmt -> iter (fun k v -> Format.fprintf fmt "%c=%a; " k value_printer v))
      map
end

(** The values of the map can be either strings of integers *)
type loop_param_value = VarName of Mast.variable_name | RangeInt of int

let format_loop_param_value fmt (v : loop_param_value) =
  match v with VarName v -> Format.fprintf fmt "%s" v | RangeInt i -> Format.fprintf fmt "%d" i

type loop_context = loop_param_value ParamsMap.t
(** This is the context when iterating a loop : for each loop parameter, we have access to the
    current value of this loop parameter in this iteration. *)

type loop_domain = loop_param_value list ParamsMap.t
(** Loops can have multiple loop parameters *)

let format_loop_context fmt (ld : loop_context) =
  ParamsMap.map_printer format_loop_param_value fmt ld

let format_loop_domain fmt (ld : loop_domain) =
  ParamsMap.map_printer (Format_mast.pp_print_list_comma format_loop_param_value) fmt ld

(** From a loop domain of varying loop parameters, builds by cartesian product the list of all
    iterations that the loop will take, each time assigining a different combination of values to
    the loop parameters. *)
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
        let all_contexts_minus_hd_val_for_param = iterate_all_combinations new_ld in
        let new_ld = ParamsMap.add param [ hd ] ld in
        let all_context_with_hd_val_for_param = iterate_all_combinations new_ld in
        all_context_with_hd_val_for_param @ all_contexts_minus_hd_val_for_param
  with Not_found -> []

(** Helper to make a list of integers from a range *)
let rec make_range_list (i1 : int) (i2 : int) : loop_param_value list =
  if i1 > i2 then []
  else
    let tl = make_range_list (i1 + 1) i2 in
    RangeInt i1 :: tl

(** {2 General translation context} *)

type translating_context = {
  table_definition : bool;
      (** [true] if translating an expression susceptible to contain a generic table index *)
  idmap : Mir.idmap;  (** Current string-to-{!type: Mir.Variable.t} mapping *)
  lc : loop_context option;  (** Current loop translation context *)
  int_const_values : int Mir.VariableMap.t;  (** Mapping from constant variables to their value *)
  exec_number : Mir.execution_number;
      (** Number of the rule of verification condition being translated *)
  current_lvalue : Mast.variable_name;
}
(** This context will be passed along during the translation *)

(** Dummy execution number used for variable declarations *)
let dummy_exec_number (pos : Pos.t) : Mir.execution_number =
  { Mir.rule_number = -1; Mir.seq_number = 0; pos }

(** When entering a loop, you are provided with a new loop context that you have to integrate to the
    general context with this function. The [position] argument is used to print an error message in
    case of the same loop parameters used in nested loops. *)
let merge_loop_ctx (ctx : translating_context) (new_lc : loop_context) (pos : Pos.t) :
    translating_context =
  match ctx.lc with
  | None -> { ctx with lc = Some new_lc }
  | Some old_lc ->
      let merged_lc =
        ParamsMap.merge
          (fun param old_val new_val ->
            match (old_val, new_val) with
            | Some _, Some _ ->
                Errors.raise_spanned_error
                  (Format.asprintf "Same loop parameter %c used in two nested loop contexts" param)
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
        Mir.(max_exec_number v.Mir.Variable.execution_number max_rest.Mir.Variable.execution_number)
      with
      | Mir.Left -> v
      | Mir.Right -> max_rest)

(** Given a list of candidates for an SSA variable query, returns the correct one: the maximum in
    the same rule or if no candidates in the same rule, the maximum in other rules. *)
let find_var_among_candidates (exec_number : Mir.execution_number) (l : Mir.Variable.t list) :
    Mir.Variable.t =
  let same_rule =
    List.filter
      (fun var -> var.Mir.Variable.execution_number.Mir.rule_number = exec_number.Mir.rule_number)
      l
  in
  if List.length same_rule = 0 then list_max_execution_number l
  else list_max_execution_number same_rule

(** Queries a [type: Mir.variable.t] from an [type:idmap] mapping, the name of a variable and the
    rule number from which the variable is requested. Returns the variable with the same name and
    highest rule number that is below the current rule number from where this variable is requested *)
let get_var_from_name (d : Mir.Variable.t list Pos.VarNameToID.t)
    (name : Mast.variable_name Pos.marked) (exec_number : Mir.execution_number)
    (using_var_in_def : bool) : Mir.Variable.t =
  try
    let same_name = Pos.VarNameToID.find (Pos.unmark name) d in
    find_var_among_candidates exec_number
      (List.filter
         (fun var ->
           Mir.is_candidate_valid var.Mir.Variable.execution_number exec_number using_var_in_def)
         same_name)
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
           Mir.is_candidate_valid var.Mir.Variable.execution_number exec_number using_var_in_def
           || Mir.same_execution_number var.Mir.Variable.execution_number exec_number)
         same_name)
  with Not_found ->
    Errors.raise_spanned_error
      (Format.asprintf "variable %s has not been declared" (Pos.unmark name))
      (Pos.get_position name)

(**{1 Translation}*)

(**{2 Loops}*)

(** The M language added a new feature in its 2017 edition : you can specify loop variable ranges
    bounds with constant variables. Because we need the actual value of the bounds to unroll
    everything, this function queries the const value in the context if needed. *)
let var_or_int_value (ctx : translating_context) (l : Mast.literal Pos.marked) : int =
  match Pos.unmark l with
  | Mast.Variable v -> (
      try
        (* We look up the value of the variable, which has to be const *)
        let name = Mast.get_variable_name v in
        let using_var_in_def = name = ctx.current_lvalue in
        Mir.VariableMap.find
          (get_var_from_name ctx.idmap (Pos.same_pos_as name l) ctx.exec_number using_var_in_def)
          ctx.int_const_values
      with Not_found ->
        Errors.raise_spanned_error
          (Format.asprintf "variable %s is not an integer constant and cannot be used here"
             (Mast.get_variable_name v))
          (Pos.get_position l))
  | Mast.Float f -> int_of_float f

(** This function is the workhorse of loop unrolling : it takes a loop prefix containing the set of
    variables over which to iterate, and fabricates a combinator. This combinator takes
    auser-provided way of translating the loop body generically over the values of the iterated
    variables, and produce a list corresponding of the unrolled loop bodies expressions containing
    the iterated values.

    In OCaml terms, if you want [translate_loop_variables lvs f ctx], then you should define [f] by
    [let f = fun lc i ctx -> ...] and use {!val: merge_loop_ctx} inside [...] before translating the
    loop body. [lc] is the loop context, [i] the loop sequence index and [ctx] the translation
    context. *)
let translate_loop_variables (lvs : Mast.loop_variables Pos.marked) (ctx : translating_context) :
    (loop_context -> int -> 'a) -> 'a list =
  match Pos.unmark lvs with
  | Mast.ValueSets lvs ->
      fun translator ->
        let varying_domain =
          List.fold_left
            (fun domain (param, values) ->
              let values =
                List.flatten
                  (List.map
                     (fun value ->
                       match value with
                       | Mast.VarParam v -> [ VarName (Pos.unmark v) ]
                       | Mast.IntervalLoop (i1, i2) ->
                           make_range_list (var_or_int_value ctx i1) (var_or_int_value ctx i2))
                     values)
              in
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
  | Mast.Ranges lvs ->
      fun translator ->
        let varying_domain =
          List.fold_left
            (fun domain (param, values) ->
              let values =
                List.map
                  (fun value ->
                    match value with
                    | Mast.VarParam v -> [ VarName (Pos.unmark v) ]
                    | Mast.IntervalLoop (i1, i2) ->
                        make_range_list (var_or_int_value ctx i1) (var_or_int_value ctx i2))
                  values
              in
              ParamsMap.add (Pos.unmark param) (List.flatten values) domain)
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

(** Variables are tricky to translate; indeed, we have unrolled all the loops, and generic variables
    depend on the loop parameters. We have to interrogate the loop context for the current values of
    the loop parameter and then replace *inside the string* the loop parameter by its value to
    produce the new variable. *)

(** A variable whose name is [X] should be translated as the generic table index expression. However
    sometimes there is a variable called [X] (yes...) so if there is no loop in the context we
    return the variable [X] for ["X"]. *)
let get_var_or_x (d : Mir.Variable.t list Pos.VarNameToID.t) (exec_number : Mir.execution_number)
    (name : Mast.variable_name Pos.marked) (current_lvalue : Mast.variable_name) (in_table : bool)
    (lax : bool) : Mir.expression =
  if Pos.unmark name = "X" && in_table then Mir.GenericTableIndex
  else if lax then
    Mir.Var (get_var_from_name_lax d name exec_number (current_lvalue = Pos.unmark name))
  else Mir.Var (get_var_from_name d name exec_number (current_lvalue = Pos.unmark name))

(** The M language has a weird and very annoying "feature", which is that you can define the
    following loop:

    {v sum(i=05,06,07:Xi) v}

    In this example, what is [Xi] supposed to become ? [X05] ? [X5] ? The answer in the actual M
    codebase is: it depends. Indeed, sometimes [X05] is defines, sometimes it is [X5], and sometimes
    the [i=05,...] will have trailing zeroes, and sometimes not. So we have to try all combinations
    of trailing zeroes and find one where everything is correctly defined.

    It is unclear why this behavior is accepted by the M language. Maybe it has to do with the way a
    string to integer function works inside the official interpreter... *)

(** To try everything, we have to cover all cases concerning trailing zeroes *)
type zero_padding = ZPNone | ZPAdd | ZPRemove

let format_zero_padding (zp : zero_padding) : string =
  match zp with
  | ZPNone -> "no zero padding"
  | ZPAdd -> "add zero padding"
  | ZPRemove -> "remove zero padding"

(**{2 Preliminary passes}*)

(** Gets constant variables declaration data and values. Done in a separate pass because constant
    variables can be used in loop ranges bounds. *)
let get_constants (p : Mast.program) :
    var_decl_data Mir.VariableMap.t * Mir.idmap * int Mir.VariableMap.t =
  let vars, idmap, int_const_list =
    List.fold_left
      (fun (vars, (idmap : Mir.idmap), int_const_list) source_file ->
        List.fold_left
          (fun (vars, (idmap : Mir.idmap), int_const_list) source_file_item ->
            match Pos.unmark source_file_item with
            | Mast.VariableDecl var_decl -> (
                match var_decl with
                | Mast.ConstVar (marked_name, cval) -> (
                    try
                      let old_var = List.hd (Pos.VarNameToID.find (Pos.unmark marked_name) idmap) in
                      Cli.var_info_print
                        "Dropping declaration of constant variable %s %a because variable was \
                         previously defined %a"
                        (Pos.unmark old_var.Mir.Variable.name)
                        Pos.format_position (Pos.get_position marked_name) Pos.format_position
                        (Pos.get_position old_var.Mir.Variable.name);
                      (vars, idmap, int_const_list)
                    with Not_found ->
                      let new_var =
                        Mir.Variable.new_var marked_name None
                          (Pos.same_pos_as "constant" marked_name)
                          (dummy_exec_number (Pos.get_position marked_name))
                          ~attributes:[] ~is_income:false ~is_table:None
                        (* a constant variable does not have attributes *)
                      in
                      let new_var_data =
                        {
                          var_decl_typ = None;
                          var_decl_is_table = None;
                          var_decl_descr = None;
                          var_decl_io = Constant;
                          var_pos = Pos.get_position source_file_item;
                        }
                      in
                      let new_vars = Mir.VariableMap.add new_var new_var_data vars in
                      let new_idmap =
                        Pos.VarNameToID.add (Pos.unmark marked_name) [ new_var ] idmap
                      in
                      let new_int_const_list =
                        match Pos.unmark cval with
                        | Mast.Float f -> (new_var, int_of_float f) :: int_const_list
                        | _ -> int_const_list
                      in
                      (new_vars, new_idmap, new_int_const_list))
                | _ -> (vars, idmap, int_const_list))
            | _ -> (vars, idmap, int_const_list))
          (vars, idmap, int_const_list) source_file)
      (Mir.VariableMap.empty, Pos.VarNameToID.empty, [])
      p
  in
  let int_const_vals : int Mir.VariableMap.t =
    List.fold_left
      (fun out (var, i) -> Mir.VariableMap.add var i out)
      Mir.VariableMap.empty int_const_list
  in
  (vars, idmap, int_const_vals)

let belongs_to_iliad_app (r : Mast.application Pos.marked list) : bool =
  List.exists (fun app -> Pos.unmark app = "iliad") r

(** Retrieves variable declaration data. Done in a separate pass because wen don't want to deal with
    sorting the dependencies between files or inside files. *)
let get_variables_decl (p : Mast.program) (vars : var_decl_data Mir.VariableMap.t)
    (idmap : Mir.idmap) : var_decl_data Mir.VariableMap.t * Mir.Error.t list * Mir.idmap =
  let vars, idmap, errors, out_list =
    List.fold_left
      (fun (vars, (idmap : Mir.idmap), errors, out_list) source_file ->
        List.fold_left
          (fun (vars, (idmap : Mir.idmap), errors, out_list) source_file_item ->
            match Pos.unmark source_file_item with
            | Mast.VariableDecl var_decl -> (
                match var_decl with
                | Mast.ComputedVar cvar -> (
                    let cvar = Pos.unmark cvar in
                    (* First we check if the variable has not been declared a first time *)
                    try
                      let old_var =
                        List.hd (Pos.VarNameToID.find (Pos.unmark cvar.Mast.comp_name) idmap)
                      in
                      Cli.var_info_print
                        "Dropping declaration of %s %a because variable was previously defined %a"
                        (Pos.unmark old_var.Mir.Variable.name)
                        Pos.format_position
                        (Pos.get_position cvar.Mast.comp_name)
                        Pos.format_position
                        (Pos.get_position old_var.Mir.Variable.name);
                      (vars, idmap, errors, out_list)
                    with Not_found ->
                      let attrs =
                        ( Pos.same_pos_as "calculee" cvar.Mast.comp_name,
                          Pos.same_pos_as (Mast.Float 1.) cvar.Mast.comp_name )
                        :: cvar.comp_attributes
                      in
                      let attrs =
                        if List.exists (fun x -> Pos.unmark x = Mast.Base) cvar.Mast.comp_subtyp
                        then
                          ( Pos.same_pos_as "base" cvar.Mast.comp_name,
                            Pos.same_pos_as (Mast.Float 1.) cvar.Mast.comp_name )
                          :: attrs
                        else attrs
                      in
                      let new_var =
                        Mir.Variable.new_var cvar.Mast.comp_name None cvar.Mast.comp_description
                          (dummy_exec_number (Pos.get_position cvar.Mast.comp_name))
                          ~attributes:attrs ~is_income:false
                          ~is_table:(Pos.unmark_option cvar.Mast.comp_table)
                      in
                      let new_var_data =
                        {
                          var_decl_typ = Pos.unmark_option cvar.Mast.comp_typ;
                          var_decl_is_table = Pos.unmark_option cvar.Mast.comp_table;
                          var_decl_descr = Some (Pos.unmark cvar.Mast.comp_description);
                          var_decl_io = Regular;
                          var_pos = Pos.get_position source_file_item;
                        }
                      in
                      let new_vars = Mir.VariableMap.add new_var new_var_data vars in
                      let new_idmap =
                        Pos.VarNameToID.add (Pos.unmark cvar.Mast.comp_name) [ new_var ] idmap
                      in
                      let new_out_list =
                        if
                          List.exists
                            (fun x ->
                              match Pos.unmark x with Mast.GivenBack -> true | Mast.Base -> false)
                            cvar.Mast.comp_subtyp
                        then cvar.Mast.comp_name :: out_list
                        else out_list
                      in
                      (new_vars, new_idmap, errors, new_out_list))
                | Mast.InputVar ivar -> (
                    let ivar = Pos.unmark ivar in
                    try
                      let old_var =
                        List.hd (Pos.VarNameToID.find (Pos.unmark ivar.Mast.input_name) idmap)
                      in
                      Cli.var_info_print
                        "Dropping declaration of %s %a because variable was previously defined %a"
                        (Pos.unmark old_var.Mir.Variable.name)
                        Pos.format_position
                        (Pos.get_position ivar.Mast.input_name)
                        Pos.format_position
                        (Pos.get_position old_var.Mir.Variable.name);
                      (vars, idmap, errors, out_list)
                    with Not_found ->
                      let new_var =
                        Mir.Variable.new_var ivar.Mast.input_name
                          (Some (Pos.unmark ivar.Mast.input_alias))
                          ivar.Mast.input_description
                          (dummy_exec_number (Pos.get_position ivar.Mast.input_name))
                          ~attributes:ivar.input_attributes
                          ~is_income:(Pos.unmark ivar.input_subtyp = Mast.Income)
                          ~is_table:None
                        (* Input variables also have a low order *)
                      in
                      let new_var_data =
                        {
                          var_decl_typ =
                            begin
                              match Pos.unmark_option ivar.Mast.input_typ with
                              | Some x -> Some x
                              | None -> (
                                  match Pos.unmark ivar.Mast.input_subtyp with
                                  | Mast.Income -> Some Mast.Real
                                  | _ -> None)
                            end;
                          var_decl_is_table = None;
                          var_decl_descr = Some (Pos.unmark ivar.Mast.input_description);
                          var_decl_io = Input;
                          var_pos = Pos.get_position source_file_item;
                        }
                      in
                      let new_vars = Mir.VariableMap.add new_var new_var_data vars in
                      let new_idmap =
                        Pos.VarNameToID.add (Pos.unmark ivar.Mast.input_name) [ new_var ] idmap
                      in
                      (new_vars, new_idmap, errors, out_list))
                | Mast.ConstVar (_, _) ->
                    (vars, idmap, errors, out_list) (* already treated before *))
            | Mast.Output out_name -> (vars, idmap, errors, out_name :: out_list)
            | Mast.Error err ->
                let err =
                  Mir.Error.new_error err.Mast.error_name
                    (Pos.same_pos_as
                       (String.concat ":" (List.map (fun s -> Pos.unmark s) err.Mast.error_descr))
                       err.Mast.error_name)
                    (Pos.unmark err.error_typ)
                in
                (vars, idmap, err :: errors, out_list)
            | _ -> (vars, idmap, errors, out_list))
          (vars, idmap, errors, out_list) source_file)
      (vars, (idmap : Mir.idmap), [], [])
      p
  in
  let vars : var_decl_data Mir.VariableMap.t =
    List.fold_left
      (fun vars out_name ->
        try
          let out_var =
            get_var_from_name_lax idmap out_name
              (dummy_exec_number (Pos.get_position out_name))
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

(** Call it with [translate_variable idmap exec_number table_definition lc var lax]. SSA is all
    about assigning the correct variable assignment instance when a variable is used somewhere. That
    is why [translate_variable] needs the [execution_number]. [table_definition] is needed because
    the M language has a special ["X"] variable (generic table index) that has to be distinguished
    from a variable simply named ["X"]. [lc] is the loop context and the thing that complicates this
    function the most: variables used inside loops have loop parameters that have to be instantiated
    to give a normal variable name. [var] is the main argument that you want to translate. [lax] is
    a special argument for SSA construction that, if sets to [true], allows [translate_variable] to
    return a variable assigned exactly at [exec_number] (otherwise it always return a variable) in
    another rule or before in the same rule. *)
let rec translate_variable (idmap : Mir.idmap) (exec_number : Mir.execution_number)
    (table_definition : bool) (lc : loop_context option) (var : Mast.variable Pos.marked)
    (current_lvalue : Mast.variable_name) (lax : bool) : Mir.expression Pos.marked =
  match Pos.unmark var with
  | Mast.Normal name ->
      Pos.same_pos_as
        (get_var_or_x idmap exec_number (Pos.same_pos_as name var) current_lvalue table_definition
           lax)
        var
  | Mast.Generic gen_name -> (
      if List.length gen_name.Mast.parameters == 0 then
        translate_variable idmap exec_number table_definition lc
          (Pos.same_pos_as (Mast.Normal gen_name.Mast.base) var)
          current_lvalue lax
      else
        match lc with
        | None ->
            Errors.raise_spanned_error
              "variable contains loop parameters but is not used inside a loop context"
              (Pos.get_position var)
        | Some _ ->
            instantiate_generic_variables_parameters idmap exec_number table_definition lc gen_name
              current_lvalue (Pos.get_position var) lax)

(** The following function deal with the "trying all cases" pragma *)
and instantiate_generic_variables_parameters (idmap : Mir.idmap)
    (exec_number : Mir.execution_number) (table_definition : bool) (lc : loop_context option)
    (gen_name : Mast.variable_generic_name) (current_lvalue : Mast.variable_name) (pos : Pos.t)
    (lax : bool) : Mir.expression Pos.marked =
  instantiate_generic_variables_parameters_aux idmap exec_number table_definition lc
    gen_name.Mast.base current_lvalue ZPNone pos lax

and instantiate_generic_variables_parameters_aux (idmap : Mir.idmap)
    (exec_number : Mir.execution_number) (table_definition : bool) (lc : loop_context option)
    (var_name : string) (current_lvalue : Mast.variable_name) (pad_zero : zero_padding)
    (pos : Pos.t) (lax : bool) : Mir.expression Pos.marked =
  try
    match
      ParamsMap.choose_opt
        (match lc with
        | None ->
            Errors.raise_spanned_error
              "variable contains loop parameters but is not used inside a loop context" pos
        | Some lc -> lc)
    with
    | None ->
        translate_variable idmap exec_number table_definition lc (Mast.Normal var_name, pos)
          current_lvalue lax
    | Some (param, value) ->
        (* The [pad_zero] parameter is here because RangeInt variables are sometimes defined with a
           trailing 0 before the index. So the algorithm first tries to find a definition without
           the trailing 0, and if it fails it tries with a trailing 0. *)
        let new_var_name =
          match value with
          | VarName value ->
              (* Sometimes the DGFiP code inserts a extra 0 to the value they want to replace when
                 it is already there... So we need to remove it here. *)
              let value =
                match pad_zero with
                | ZPNone -> value
                | ZPRemove -> string_of_int (int_of_string value)
                | ZPAdd -> "0" ^ value
              in
              Re.Str.replace_first (Re.Str.regexp (Format.asprintf "%c" param)) value var_name
          | RangeInt i ->
              let value =
                match pad_zero with
                | ZPNone -> string_of_int i
                | ZPRemove -> string_of_int i
                | ZPAdd -> "0" ^ string_of_int i
              in
              Re.Str.replace_first (Re.Str.regexp (Format.asprintf "%c" param)) value var_name
        in
        instantiate_generic_variables_parameters_aux idmap exec_number table_definition
          (Some
             (ParamsMap.remove param
                (match lc with
                | None ->
                    Errors.raise_spanned_error
                      "variable contains loop parameters but is not used inside a loop context" pos
                | Some lc -> lc)))
          new_var_name current_lvalue pad_zero pos lax
  with
  | err
  when match lc with
       | None ->
           Errors.raise_spanned_error
             "variable contains loop parameters but is not used inside a loop context" pos
       | Some lc -> ParamsMap.cardinal lc > 0
  ->
    let new_pad_zero =
      match pad_zero with ZPNone -> ZPRemove | ZPRemove -> ZPAdd | _ -> raise err
    in
    instantiate_generic_variables_parameters_aux idmap exec_number table_definition lc var_name
      current_lvalue new_pad_zero pos lax

(** Linear pass that fills [idmap] with all the variable assignments along with their execution
    number. *)
let get_var_redefinitions (p : Mast.program) (idmap : Mir.idmap)
    (int_const_vals : int Mir.VariableMap.t) : Mir.idmap =
  let idmap =
    List.fold_left
      (fun (idmap : Mir.idmap) source_file ->
        List.fold_left
          (fun (idmap : Mir.idmap) source_file_item ->
            match Pos.unmark source_file_item with
            | Mast.Rule r ->
                let rule_number = Mast.rule_number r.Mast.rule_name in
                if not (belongs_to_iliad_app r.Mast.rule_applications) then idmap
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
                                      ((Pos.unmark f.Mast.lvalue).Mast.index <> None)
                                      None (Pos.unmark f.Mast.lvalue).Mast.var ""
                                      (* dummy current lvalue *) false)
                               with
                               | Mir.Var var -> var
                               | _ -> assert false
                               (* should not happen *)
                             in
                             let new_var =
                               Mir.Variable.new_var lvar.Mir.Variable.name None
                                 lvar.Mir.Variable.descr exec_number
                                 ~attributes:lvar.Mir.Variable.attributes
                                 ~is_income:lvar.Mir.Variable.is_income
                                 ~is_table:lvar.Mir.Variable.is_table
                             in
                             let new_idmap =
                               Pos.VarNameToID.add
                                 (Pos.unmark lvar.Mir.Variable.name)
                                 (new_var
                                 :: Pos.VarNameToID.find (Pos.unmark lvar.Mir.Variable.name) idmap)
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
                                 int_const_values = int_const_vals;
                                 table_definition = false;
                                 exec_number;
                                 current_lvalue = "" (* dummy current lvalue *);
                               }
                             in
                             let loop_context_provider = translate_loop_variables lvs ctx in
                             let translator lc idx =
                               let exec_number =
                                 { exec_number with Mir.seq_number = seq_number + idx }
                               in
                               let lvar =
                                 match
                                   Pos.unmark
                                     (translate_variable idmap exec_number
                                        ((Pos.unmark f.Mast.lvalue).Mast.index <> None)
                                        (Some lc) (Pos.unmark f.Mast.lvalue).Mast.var
                                        ctx.current_lvalue false)
                                 with
                                 | Mir.Var var -> var
                                 | _ -> assert false
                                 (* should not happen *)
                               in
                               let new_var =
                                 Mir.Variable.new_var lvar.Mir.Variable.name None
                                   lvar.Mir.Variable.descr exec_number
                                   ~attributes:lvar.Mir.Variable.attributes
                                   ~is_income:lvar.Mir.Variable.is_income
                                   ~is_table:lvar.Mir.Variable.is_table
                               in
                               (Pos.unmark lvar.Mir.Variable.name, new_var)
                             in
                             let new_var_defs = loop_context_provider translator in
                             List.fold_left
                               (fun (idmap, seq_number) (var_name, var_def) ->
                                 ( Pos.VarNameToID.add var_name
                                     (var_def :: Pos.VarNameToID.find var_name idmap)
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

let translate_table_index (ctx : translating_context) (i : Mast.table_index Pos.marked) :
    Mir.expression Pos.marked =
  match Pos.unmark i with
  | Mast.LiteralIndex i' -> Pos.same_pos_as (Mir.Literal (Mir.Float (float_of_int i'))) i
  | Mast.SymbolIndex v ->
      let var =
        translate_variable ctx.idmap ctx.exec_number ctx.table_definition ctx.lc
          (Pos.same_pos_as v i) ctx.current_lvalue false
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
      Errors.raise_spanned_error (Format.asprintf "unknown function %s" x) (Pos.get_position f_name)

(** Main translation function for expressions *)
let rec translate_expression (ctx : translating_context) (f : Mast.expression Pos.marked) :
    Mir.expression Pos.marked =
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
                        translate_variable ctx.idmap ctx.exec_number ctx.table_definition ctx.lc
                          set_var ctx.current_lvalue false )
                | Mast.FloatValue i ->
                    Mir.Comparison
                      ( Pos.same_pos_as Mast.Eq i,
                        Pos.same_pos_as local_var_expr e,
                        Pos.same_pos_as (Mir.Literal (Mir.Float (Pos.unmark i))) i )
                | Mast.Interval (bn, en) ->
                    if Pos.unmark bn > Pos.unmark en then
                      Errors.raise_spanned_error "wrong interval bounds" (Pos.get_position bn)
                    else
                      Mir.Binop
                        ( Pos.same_pos_as Mast.And bn,
                          Pos.same_pos_as
                            (Mir.Comparison
                               ( Pos.same_pos_as Mast.Gte bn,
                                 Pos.same_pos_as local_var_expr e,
                                 Pos.same_pos_as
                                   (Mir.Literal (Mir.Float (float_of_int (Pos.unmark bn))))
                                   bn ))
                            bn,
                          Pos.same_pos_as
                            (Mir.Comparison
                               ( Pos.same_pos_as Mast.Lte en,
                                 Pos.same_pos_as local_var_expr e,
                                 Pos.same_pos_as
                                   (Mir.Literal (Mir.Float (float_of_int (Pos.unmark en))))
                                   en ))
                            en )
              in
              Pos.same_pos_as
                (Mir.Binop (Pos.same_pos_as Mast.Or f, Pos.same_pos_as equal_test f, or_chain))
                f)
            (Pos.same_pos_as (Mir.Literal Mir.Undefined) f)
            values
        in
        let or_chain =
          if not positive then Pos.same_pos_as (Mir.Unop (Mast.Not, or_chain)) or_chain
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
          translate_variable ctx.idmap ctx.exec_number ctx.table_definition ctx.lc t
            ctx.current_lvalue false
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
          (* the absence of a else branch for a ternary operators can yield an undefined term *)
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
              translate_variable ctx.idmap ctx.exec_number ctx.table_definition ctx.lc
                (Pos.same_pos_as var f) ctx.current_lvalue false
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
            Mir.Binop (Pos.same_pos_as Mast.Or e, Pos.same_pos_as acc e, loop_expr))
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

(** Translates lvalues into the assigning variable as well as the type of assignment *)
let translate_lvalue (ctx : translating_context) (lval : Mast.lvalue Pos.marked) :
    translating_context * Mir.Variable.t * index_def =
  let var =
    match
      Pos.unmark
        (translate_variable ctx.idmap ctx.exec_number ctx.table_definition ctx.lc
           (Pos.unmark lval).Mast.var "" (* dummy lvalue since we are translating the lvalue *) true)
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
          let mvg_v =
            translate_variable ctx.idmap ctx.exec_number ctx.table_definition ctx.lc
              (Pos.same_pos_as v ti)
              (Pos.unmark var.Mir.Variable.name)
              false
          in
          let i =
            var_or_int_value ctx
              (Pos.same_pos_as
                 (Mast.Variable
                    (match Pos.unmark mvg_v with
                    | Mir.Var v -> Mast.Normal (Pos.unmark v.Mir.Variable.name)
                    | _ -> assert false (* should not happen*)))
                 ti)
          in
          ({ ctx with current_lvalue = Pos.unmark var.Mir.Variable.name }, var, SingleIndex i))
  | None -> ({ ctx with current_lvalue = Pos.unmark var.Mir.Variable.name }, var, NoIndex)

(** Date types are not supported *)
let translate_value_typ (typ : Mast.value_typ Pos.marked option) : Mir.typ option =
  match typ with
  | Some (Mast.Boolean, _) -> Some Mir.Real
  (* Indeed, the BOOLEEN annotations are useless because they feed it to functions that expect
     reals *)
  | Some (Mast.Real, _) -> Some Mir.Real
  | Some (_, _) -> Some Mir.Real
  | None -> None

(** Main toplevel declaration translator that adds a variable definition to the MVG program *)
let add_var_def (var_data : Mir.variable_data Mir.VariableMap.t) (var_lvalue : Mir.Variable.t)
    (var_expr : Mir.expression Pos.marked) (def_kind : index_def)
    (var_decl_data : var_decl_data Mir.VariableMap.t) (idmap : Mir.idmap) :
    Mir.variable_data Mir.VariableMap.t =
  let var_at_declaration =
    List.find
      (fun var ->
        Mir.(
          same_execution_number var.Mir.Variable.execution_number
            (dummy_exec_number (Pos.get_position var_expr))))
      (Pos.VarNameToID.find (Pos.unmark var_lvalue.name) idmap)
  in
  let decl_data =
    try Mir.VariableMap.find var_at_declaration var_decl_data with Not_found -> assert false
    (* should not happen *)
  in
  let var_typ =
    translate_value_typ
      (match decl_data.var_decl_typ with Some x -> Some (x, decl_data.var_pos) | None -> None)
  in
  Mir.VariableMap.add var_lvalue
    (try
       let io =
         match decl_data.var_decl_io with
         | Input -> Mir.Input
         | Constant | Regular -> Mir.Regular
         | Output -> Mir.Output
       in
       if def_kind = NoIndex then
         { Mir.var_definition = Mir.SimpleVar var_expr; Mir.var_typ; Mir.var_io = io }
       else
         match decl_data.var_decl_is_table with
         | Some size -> (
             match def_kind with
             | NoIndex -> assert false (* should not happen*)
             | SingleIndex i ->
                 {
                   Mir.var_definition =
                     Mir.TableVar (size, Mir.IndexTable (Mir.IndexMap.singleton i var_expr));
                   Mir.var_typ;
                   Mir.var_io = io;
                 }
             | GenericIndex ->
                 {
                   Mir.var_definition = Mir.TableVar (size, Mir.IndexGeneric var_expr);
                   Mir.var_typ;
                   Mir.var_io = io;
                 })
         | None ->
             Errors.raise_multispanned_error
               (Format.asprintf
                  "variable %s is defined as a table but has been declared as a non-table"
                  (Pos.unmark var_lvalue.Mir.Variable.name))
               [
                 (Some "variable definition", Pos.get_position var_expr);
                 ( Some "variable declaration",
                   (Mir.VariableMap.find var_lvalue var_decl_data).var_pos );
               ]
     with Not_found ->
       assert false
       (* should not happen *)
       (* should not happen since we already looked into idmap to get the var value from its name *))
    var_data

(** Main translation pass that deal with regular variable definition; returns a map whose keys are
    the variables being defined (with the execution number corresponding to the place where it is
    defined) and whose values are the expressions corresponding to the definitions. *)
let get_var_data (idmap : Mir.idmap) (var_decl_data : var_decl_data Mir.VariableMap.t)
    (int_const_vals : int Mir.VariableMap.t) (p : Mast.program) :
    Mir.variable_data Mir.VariableMap.t =
  let out =
    List.fold_left
      (fun var_data source_file ->
        List.fold_left
          (fun var_data source_file_item ->
            match Pos.unmark source_file_item with
            | Mast.Rule r ->
                let rule_number = Mast.rule_number r.Mast.rule_name in
                if not (belongs_to_iliad_app r.Mast.rule_applications) then var_data
                else
                  fst
                    (List.fold_left
                       (fun (var_data, seq_number) formula ->
                         match Pos.unmark formula with
                         | Mast.SingleFormula f ->
                             let ctx =
                               {
                                 idmap;
                                 lc = None;
                                 int_const_values = int_const_vals;
                                 table_definition = false;
                                 exec_number =
                                   {
                                     Mir.rule_number;
                                     Mir.seq_number;
                                     Mir.pos = Pos.get_position f.Mast.lvalue;
                                   };
                                 current_lvalue =
                                   "" (* dummy, will be filled by [translate_lvalue] *);
                               }
                             in
                             let ctx, var_lvalue, def_kind = translate_lvalue ctx f.Mast.lvalue in
                             let var_expr = translate_expression ctx f.Mast.formula in
                             ( add_var_def var_data var_lvalue var_expr def_kind var_decl_data idmap,
                               seq_number + 1 )
                         | Mast.MultipleFormulaes (lvs, f) ->
                             let ctx =
                               {
                                 idmap;
                                 lc = None;
                                 int_const_values = int_const_vals;
                                 table_definition = false;
                                 exec_number =
                                   {
                                     Mir.rule_number;
                                     Mir.seq_number;
                                     Mir.pos = Pos.get_position f.Mast.lvalue;
                                   };
                                 current_lvalue = "";
                               }
                             in
                             let loop_context_provider = translate_loop_variables lvs ctx in
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
                               let var_expr = translate_expression new_ctx f.Mast.formula in
                               (var_lvalue, var_expr, def_kind)
                             in
                             let data_to_add = loop_context_provider translator in
                             List.fold_left
                               (fun (var_data, seq_number) (var_lvalue, var_expr, def_kind) ->
                                 ( add_var_def var_data var_lvalue var_expr def_kind var_decl_data
                                     idmap,
                                   seq_number + 1 ))
                               (var_data, seq_number) data_to_add)
                       (var_data, 0) r.Mast.rule_formulaes)
            | Mast.VariableDecl (Mast.ConstVar (name, lit)) ->
                let var =
                  get_var_from_name_lax idmap name (dummy_exec_number (Pos.get_position name)) false
                in
                add_var_def var_data var
                  (Pos.same_pos_as
                     (Mir.Literal
                        (match Pos.unmark lit with
                        | Mast.Variable var ->
                            Errors.raise_spanned_error
                              (Format.asprintf
                                 "const variable %a cannot be defined by using another variable"
                                 Format_mast.format_variable var)
                              (Pos.get_position source_file_item)
                        | Mast.Float f -> Mir.Float f))
                     lit)
                  NoIndex var_decl_data idmap
            | Mast.VariableDecl (Mast.InputVar (var, pos)) ->
                let var =
                  get_var_from_name_lax idmap var.Mast.input_name (dummy_exec_number pos) false
                in
                let var_decl =
                  try Mir.VariableMap.find var var_decl_data with Not_found -> assert false
                  (* should not happen *)
                in
                let typ =
                  translate_value_typ
                    (match var_decl.var_decl_typ with
                    | Some x -> Some (x, Pos.get_position var.Mir.Variable.name)
                    | None -> None)
                in
                Mir.VariableMap.add var
                  { Mir.var_definition = Mir.InputVar; Mir.var_typ = typ; Mir.var_io = Mir.Input }
                  var_data
            | _ -> var_data)
          var_data source_file)
      Mir.VariableMap.empty (List.rev p)
  in
  out

(** At this point [var_data] contains the definition data for all the times a variable is defined.
    However the M language deals with undefined variable, so for each variable we have to insert a
    dummy definition corresponding to the declaration and whose value is [Undefined]. *)
let add_dummy_definition_for_variable_declaration (var_data : Mir.variable_data Mir.VariableMap.t)
    (var_decl_data : var_decl_data Mir.VariableMap.t) (idmap : Mir.idmap) :
    Mir.variable_data Mir.VariableMap.t =
  Mir.VariableMap.fold
    (fun var decl (var_data : Mir.variable_data Mir.VariableMap.t) ->
      (* The variable has not been defined in a rule *)
      match decl.var_decl_io with
      | Output | Regular -> (
          if
            List.for_all
              (fun var' -> var'.Mir.Variable.execution_number.Mir.rule_number = -1)
              (Pos.VarNameToID.find (Pos.unmark var.Mir.Variable.name) idmap)
          then
            Cli.var_info_print "variable %s declared %a is never defined in the application"
              (Pos.unmark var.Mir.Variable.name)
              Pos.format_position
              (Pos.get_position var.Mir.Variable.name);
          (* This is the case where the variable is not defined. *)
          let io =
            match decl.var_decl_io with
            | Output -> Mir.Output
            | Regular | Constant -> Mir.Regular
            | Input -> assert false
            (* should not happen *)
          in
          (* We insert the Undef expr *)
          match decl.var_decl_is_table with
          | Some size ->
              Mir.VariableMap.add var
                {
                  Mir.var_definition =
                    Mir.TableVar
                      ( size,
                        Mir.IndexGeneric
                          (Pos.same_pos_as (Mir.Literal Mir.Undefined) var.Mir.Variable.name) );
                  Mir.var_typ =
                    translate_value_typ
                      (match decl.var_decl_typ with
                      | None -> None
                      | Some typ -> Some (Pos.same_pos_as typ var.name));
                  Mir.var_io = io;
                }
                var_data
          | None ->
              Mir.VariableMap.add var
                {
                  Mir.var_definition =
                    Mir.SimpleVar
                      (Pos.same_pos_as (Mir.Literal Mir.Undefined) var.Mir.Variable.name);
                  Mir.var_typ =
                    translate_value_typ
                      (match decl.var_decl_typ with
                      | None -> None
                      | Some typ -> Some (Pos.same_pos_as typ var.name));
                  Mir.var_io = io;
                }
                var_data)
      | Input ->
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
      | Constant -> var_data
      (* the variable's definition has already been inserted in [var_data] *))
    var_decl_data var_data

(** Returns a map whose keys are dummy variables and whose values are the verification conditions. *)
let get_conds (error_decls : Mir.Error.t list) (idmap : Mir.idmap) (p : Mast.program) :
    Mir.condition_data Mir.VariableMap.t =
  List.fold_left
    (fun conds source_file ->
      List.fold_left
        (fun conds source_file_item ->
          match Pos.unmark source_file_item with
          | Mast.Verification verif when belongs_to_iliad_app verif.Mast.verif_applications ->
              let rule_number = Mast.verification_number verif.verif_name in
              List.fold_left
                (fun conds verif_cond ->
                  let e =
                    translate_expression
                      {
                        idmap;
                        lc = None;
                        int_const_values = Mir.VariableMap.empty;
                        table_definition = false;
                        exec_number =
                          {
                            Mir.rule_number;
                            Mir.seq_number = 0;
                            Mir.pos = Pos.get_position verif_cond;
                          };
                        current_lvalue =
                          ""
                          (* dummy, we are in a verif condition so you can't refer to it inside *);
                      }
                      (Pos.unmark verif_cond).Mast.verif_cond_expr
                  in
                  let errs =
                    List.map
                      (fun err_name ->
                        try
                          Some
                            (List.find
                               (fun e -> Pos.unmark e.Mir.Error.name = Pos.unmark err_name)
                               error_decls)
                        with Not_found ->
                          Cli.var_info_print "undeclared error %s %a" (Pos.unmark err_name)
                            Pos.format_position (Pos.get_position err_name);
                          None)
                      (Pos.unmark verif_cond).Mast.verif_cond_errors
                  in
                  if
                    List.length errs > 0
                    && List.for_all
                         (fun err ->
                           match err with
                           | None -> false (* if the error is undefined then we take it anyway *)
                           | Some err -> err.Mir.Error.typ <> Mast.Anomaly)
                         errs
                  then
                    (* If all errors raised by this verification condition are informative, we don't
                       need it! *)
                    conds
                  else
                    let dummy_var =
                      Mir.Variable.new_var
                        (Pos.same_pos_as
                           (Format.sprintf "verification_condition_%d" (Mir.Variable.fresh_id ()))
                           e)
                        None
                        (Pos.same_pos_as
                           (let () =
                              Pos.format_position Format.str_formatter (Pos.get_position e)
                            in
                            Format.flush_str_formatter ())
                           e)
                        {
                          Mir.rule_number;
                          Mir.seq_number = 0;
                          Mir.pos = Pos.get_position verif_cond;
                        }
                        ~attributes:[] ~is_income:false ~is_table:None
                    in
                    Mir.VariableMap.add dummy_var
                      {
                        Mir.cond_expr = e;
                        Mir.cond_errors =
                          List.map
                            (fun x ->
                              match x with Some x -> x | None -> assert false
                              (* should not happen *))
                            (List.filter
                               (fun x -> match x with Some _ -> true | None -> false)
                               errs);
                      }
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
              not (List.exists (fun part -> Pos.unmark part = "corrective") rule.rule_name)
          | _ -> true)
        source_file)
    p

(** Main translation function from the M AST to the M Variable Graph. This function performs 6
    linear passes on the input code:

    - [remove_corrective_rules] removes all the rules that are not necessary for the computation of
      the "primitive" income tax;
    - [get_constants] gets the value of all constant variables, the values of which are needed to
      compute certain loop bounds;
    - [get_variables_decl] retrieves the declarations of all other variables and errors;
    - [get_var_redefinitions] incorporates into [idmap] all definitions inside rules along with
      their execution number;
    - [get_var_data] is the workhorse pass that translates all the expressions corresponding to the
      definitions;
    - [add_dummy_definition_for_variable_declaration] adds [Undefined] definitions placeholder for
      all variables declarations;
    - [get_errors_conds] parses the verification conditions definitions. *)
let translate (p : Mast.program) : Mir.program =
  let p = remove_corrective_rules p in
  let var_decl_data, idmap, int_const_vals = get_constants p in
  let var_decl_data, error_decls, idmap = get_variables_decl p var_decl_data idmap in
  let idmap = get_var_redefinitions p idmap int_const_vals in
  let var_data = get_var_data idmap var_decl_data int_const_vals p in
  let var_data = add_dummy_definition_for_variable_declaration var_data var_decl_data idmap in
  let conds = get_conds error_decls idmap p in
  {
    Mir.program_vars = var_data;
    Mir.program_conds = conds;
    Mir.program_idmap = idmap;
    Mir.program_exec_passes = [];
  }
