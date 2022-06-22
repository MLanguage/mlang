(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr> Raphaël Monat <raphael.monat@lip6.fr>

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

(** Main data structure for M analysis *)

(**{1 Variables} *)

(** Variables are first-class objects *)

type execution_number = {
  rule_number : int;
      (** Written in the name of the rule or verification condition *)
  seq_number : int;  (** Index in the sequence of the definitions in the rule *)
  pos : Pos.t;
}
(** To determine in which order execute the different variable assigment we have
    to record their position in the graph. *)

let compare_execution_number (n1 : execution_number) (n2 : execution_number) :
    int =
  if n1.rule_number = n2.rule_number then compare n1.seq_number n2.seq_number
  else compare n1.rule_number n2.rule_number

type max_result =
  | Left
  | Right  (** Operator used to select the most preferable variable to choose *)

let max_exec_number (left : execution_number) (right : execution_number) :
    max_result =
  if left.rule_number > right.rule_number then Left
  else if left.rule_number < right.rule_number then Right
  else if left.seq_number > right.seq_number then Left
  else if left.seq_number < right.seq_number then Right
  else Left

(** This is the operator used to determine the if a candidate definition is
    valid at a given point *)
let is_candidate_valid (candidate : execution_number)
    (current : execution_number) (is_lvalue : bool) : bool =
  if is_lvalue then
    (* This is the case where we are using variable [VAR] while defining [VAR]:
       i.e we are querying the left hand side of the assignation. The valid
       definitions here are either the declaration or earlier definitions in the
       same rules. *)
    candidate.rule_number = -1
    || candidate.rule_number = current.rule_number
       && candidate.seq_number < current.seq_number
  else
    (* In this case, we are using [FOO] in the definition of [BAR]. Then valid
       definitions of [FOO] include all that are in different rules or earlier
       definition in the same rule. *)
    candidate.rule_number <> current.rule_number
    || candidate.seq_number < current.seq_number

(** This is the operator used to find a particular variable in the [idmap] *)
let same_execution_number (en1 : execution_number) (en2 : execution_number) :
    bool =
  en1.rule_number = en2.rule_number && en1.seq_number = en2.seq_number

type variable_id = int
(** Each variable has an unique ID *)

type variable_subtype =
  | Context
  | Family
  | Penality
  | Income
  | Base
  | GivenBack
  | Computed
  | Input

let subtypes_of_decl (var_decl : Mast.variable_decl) : variable_subtype list =
  match var_decl with
  | ConstVar _ -> []
  | ComputedVar cv ->
      let subtypes =
        List.map
          (fun subtyp ->
            match (Pos.unmark subtyp : Mast.computed_typ) with
            | Base -> Base
            | GivenBack -> GivenBack)
          (Pos.unmark cv).comp_subtyp
      in
      Computed :: subtypes
  | InputVar iv ->
      let iv = Pos.unmark iv in
      let subtypes =
        match (Pos.unmark iv.input_subtyp : Mast.input_variable_subtype) with
        | Context -> [ Context ]
        | Family -> [ Family ]
        | Penality -> [ Penality ]
        | Income -> [ Income ]
      in
      let subtypes =
        if iv.input_given_back then GivenBack :: subtypes else subtypes
      in
      Input :: subtypes

type variable = {
  name : string Pos.marked;  (** The position is the variable declaration *)
  execution_number : execution_number;
      (** The number associated with the rule of verification condition in which
          the variable is defined *)
  alias : string option;  (** Input variable have an alias *)
  id : variable_id;
  descr : string Pos.marked;
      (** Description taken from the variable declaration *)
  attributes :
    (Mast.input_variable_attribute Pos.marked * Mast.literal Pos.marked) list;
  origin : variable option;
      (** If the variable is an SSA duplication, refers to the original
          (declared) variable *)
  subtypes : variable_subtype list;
  is_table : int option;
}

module Variable = struct
  type id = variable_id

  type t = variable = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    execution_number : execution_number;
        (** The number associated with the rule of verification condition in
            which the variable is defined *)
    alias : string option;  (** Input variable have an alias *)
    id : variable_id;
    descr : string Pos.marked;
        (** Description taken from the variable declaration *)
    attributes :
      (Mast.input_variable_attribute Pos.marked * Mast.literal Pos.marked) list;
    origin : variable option;
        (** If the variable is an SSA duplication, refers to the original
            (declared) variable *)
    subtypes : variable_subtype list;
    is_table : int option;
  }

  let fresh_id : unit -> id =
    let counter : int ref = ref 0 in
    fun () ->
      let v = !counter in
      counter := !counter + 1;
      v

  let new_var (name : string Pos.marked) (alias : string option)
      (descr : string Pos.marked) (execution_number : execution_number)
      ~(attributes :
         (Mast.input_variable_attribute Pos.marked * Mast.literal Pos.marked)
         list) ~(origin : t option) ~(subtypes : variable_subtype list)
      ~(is_table : int option) : t =
    {
      name;
      id = fresh_id ();
      descr;
      alias;
      execution_number;
      attributes;
      origin;
      subtypes;
      is_table;
    }

  let compare (var1 : t) (var2 : t) = compare var1.id var2.id
end

(** Local variables don't appear in the M source program but can be introduced
    by let bindings when translating to MIR. They should be De Bruijn indices
    but instead are unique globals identifiers out of laziness. *)

type local_variable = { id : int }

module LocalVariable = struct
  type t = local_variable = { id : int }

  let counter : int ref = ref 0

  let fresh_id () : int =
    let v = !counter in
    counter := !counter + 1;
    v

  let new_var () : t = { id = fresh_id () }

  let compare (var1 : t) (var2 : t) = compare var1.id var2.id
end

(** Type of MIR values *)
type typ = Real

type literal = Float of float | Undefined

let false_literal = Float 0.

let true_literal = Float 1.

(** MIR only supports a restricted set of functions *)
type func =
  | SumFunc  (** Sums the arguments *)
  | AbsFunc  (** Absolute value *)
  | MinFunc  (** Minimum of a list of values *)
  | MaxFunc  (** Maximum of a list of values *)
  | GtzFunc  (** Greater than zero (strict) ? *)
  | GtezFunc  (** Greater or equal than zero ? *)
  | NullFunc  (** Equal to zero ? *)
  | ArrFunc  (** Round to nearest integer *)
  | InfFunc  (** Truncate to integer *)
  | PresentFunc  (** Different than zero ? *)
  | Multimax  (** ??? *)
  | Supzero  (** ??? *)

(** MIR expressions are simpler than M; there are no loops or syntaxtic sugars.
    Because M lets you define conditional without an else branch although it is
    an expression-based language, we include an [Error] constructor to which the
    missing else branch is translated to.

    Because translating to MIR requires a lot of unrolling and expansion, we
    introduce a [LocalLet] construct to avoid code duplication. *)

type 'variable expression_ =
  | Unop of (Mast.unop[@opaque]) * 'variable expression_ Pos.marked
  | Comparison of
      (Mast.comp_op[@opaque]) Pos.marked
      * 'variable expression_ Pos.marked
      * 'variable expression_ Pos.marked
  | Binop of
      (Mast.binop[@opaque]) Pos.marked
      * 'variable expression_ Pos.marked
      * 'variable expression_ Pos.marked
  | Index of 'variable Pos.marked * 'variable expression_ Pos.marked
  | Conditional of
      'variable expression_ Pos.marked
      * 'variable expression_ Pos.marked
      * 'variable expression_ Pos.marked
  | FunctionCall of (func[@opaque]) * 'variable expression_ Pos.marked list
  | Literal of (literal[@opaque])
  | Var of 'variable
  | LocalVar of (LocalVariable.t[@opaque])
  | Error
  | LocalLet of
      (LocalVariable.t[@opaque])
      * 'variable expression_ Pos.marked
      * 'variable expression_ Pos.marked

type expression = variable expression_

let rec map_expr_var (f : 'v -> 'v2) (e : 'v expression_) : 'v2 expression_ =
  let map = Pos.map_under_mark (map_expr_var f) in
  match (e :> 'v expression_) with
  | Unop (op, e) -> Unop (op, map e)
  | Comparison (op, e1, e2) -> Comparison (op, map e1, map e2)
  | Binop (op, e1, e2) -> Binop (op, map e1, map e2)
  | Index ((v, pos), e) -> Index ((f v, pos), map e)
  | Conditional (e1, e2, e3) -> Conditional (map e1, map e2, map e3)
  | FunctionCall (func, es) -> FunctionCall (func, List.map map es)
  | Var v -> Var (f v)
  | LocalLet (v, e1, e2) -> LocalLet (v, map e1, map e2)
  | Literal l -> Literal l
  | LocalVar v -> LocalVar v
  | Error -> Error

let rec fold_expr_var (f : 'a -> 'v -> 'a) (acc : 'a) (e : 'v expression_) : 'a
    =
  let fold acc e = fold_expr_var f acc (Pos.unmark e) in
  match (e :> 'v expression_) with
  | Unop (_, e) -> fold acc e
  | Comparison (_, e1, e2) | Binop (_, e1, e2) | LocalLet (_, e1, e2) ->
      fold (fold acc e1) e2
  | Index ((v, _), e) -> fold (f acc v) e
  | Conditional (e1, e2, e3) -> fold (fold (fold acc e1) e2) e3
  | FunctionCall (_, es) -> List.fold_left fold acc es
  | Var v -> f acc v
  | Literal _ | LocalVar _ | Error -> acc

(** MIR programs are just mapping from variables to their definitions, and make
    a massive use of [VariableMap]. *)
module VariableMap = struct
  include Map.Make (Variable)

  let map_printer key_printer value_printer fmt map =
    Format.fprintf fmt "{ %a }"
      (fun fmt ->
        iter (fun k v ->
            Format.fprintf fmt "%a ~> %a, " key_printer k value_printer v))
      map
end

(* module VariableDictMap = Map.Make (struct
 *   type t = Variable.id
 * 
 *   let compare = compare
 * end)
 * 
 * type variable_dict = variable VariableDictMap.t *)

(** Variable dictionary, act as a set but refered by keys *)
module VariableDict = Dict.Make (struct
  type t = Variable.id

  type elt = Variable.t

  let key_of_elt v = v.Variable.id

  let compare = compare
end)

module VariableSet = Set.Make (Variable)

module LocalVariableMap = struct
  include Map.Make (LocalVariable)

  let map_printer value_printer fmt map =
    Format.fprintf fmt "{ %a }"
      (fun fmt ->
        iter (fun var v ->
            Format.fprintf fmt "%d ~> %a, " var.id value_printer v))
      map
end

(** This map is used to store the definitions of all the cells of a table
    variable that is not not defined generically *)
module IndexMap = struct
  include Map.Make (struct
    type t = int

    let compare = compare
  end)

  let map_printer value_printer fmt map =
    Format.fprintf fmt "{ %a }"
      (fun fmt ->
        iter (fun k v -> Format.fprintf fmt "%d ~> %a, " k value_printer v))
      map
end

type 'variable index_def =
  | IndexTable of
      ('variable expression_ Pos.marked IndexMap.t[@name "index_map"])
  | IndexGeneric of 'variable * 'variable expression_ Pos.marked

(** The definitions here are modeled closely to the source M language. One could
    also adopt a more lambda-calculus-compatible model with functions used to
    model tables. *)
type 'variable variable_def_ =
  | SimpleVar of 'variable expression_ Pos.marked
  | TableVar of int * 'variable index_def
  | InputVar

let map_var_def_var (f : 'v -> 'v2) (vdef : 'v variable_def_) :
    'v2 variable_def_ =
  let map_expr = Pos.map_under_mark (map_expr_var f) in
  match vdef with
  | InputVar -> InputVar
  | SimpleVar e -> SimpleVar (map_expr e)
  | TableVar (i, idef) ->
      let idef =
        match idef with
        | IndexTable idx_map -> IndexTable (IndexMap.map map_expr idx_map)
        | IndexGeneric (v, e) -> IndexGeneric (f v, map_expr e)
      in
      TableVar (i, idef)

type variable_def = variable variable_def_

type io = Input | Output | Regular

type 'variable variable_data_ = {
  var_definition : 'variable variable_def_;
  var_typ : typ option;
      (** The typing info here comes from the variable declaration in the source
          program *)
  var_io : io;
}

type variable_data = variable variable_data_

type rule_id = RuleID of int | VerifID of int

let num_of_rule_or_verif_id = function RuleID n | VerifID n -> n

let fresh_rule_num =
  let count = ref 0 in
  fun () ->
    let n = !count in
    incr count;
    n

(** Special rule id for initial definition of variables *)
let initial_undef_rule_id = RuleID (-1)

type rule_data = {
  rule_vars : (Variable.id * variable_data) list;
  rule_number : rule_id Pos.marked;
  rule_tags : Mast.chain_tag list;
}

module RuleMap = Map.Make (struct
  type t = rule_id

  let compare = compare
end)

module TagMap = Map.Make (struct
  type t = Mast.chain_tag

  let compare t1 t2 =
    match (t1, t2) with
    | Mast.Custom s1, Mast.Custom s2 -> String.compare s1 s2
    | _ -> Stdlib.compare t1 t2
end)

(**{1 Verification conditions}*)

type error_descr = {
  kind : string Pos.marked;
  major_code : string Pos.marked;
  minor_code : string Pos.marked;
  description : string Pos.marked;
  isisf : string Pos.marked;
}
(** Errors are first-class objects *)

type error = {
  name : string Pos.marked;  (** The position is the variable declaration *)
  id : int;  (** Each variable has an unique ID *)
  descr : error_descr;  (** Description taken from the variable declaration *)
  typ : Mast.error_typ;
}

module Error = struct
  type descr = error_descr = {
    kind : string Pos.marked;
    major_code : string Pos.marked;
    minor_code : string Pos.marked;
    description : string Pos.marked;
    isisf : string Pos.marked;
  }

  type t = error = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    id : int;  (** Each variable has an unique ID *)
    descr : error_descr;  (** Description taken from the variable declaration *)
    typ : Mast.error_typ;
  }

  let counter : int ref = ref 0

  let fresh_id () : int =
    let v = !counter in
    counter := !counter + 1;
    v

  let mast_error_desc_to_ErrorDesc (error : Mast.error_) =
    {
      kind = List.nth error.error_descr 0;
      major_code = List.nth error.error_descr 1;
      minor_code = List.nth error.error_descr 2;
      description = List.nth error.error_descr 3;
      isisf =
        (match List.nth_opt error.error_descr 4 with
        | Some s -> s
        | None -> ("", Pos.no_pos));
    }

  let new_error (name : string Pos.marked) (error : Mast.error_)
      (error_typ : Mast.error_typ) : t =
    {
      name;
      id = fresh_id ();
      descr = error |> mast_error_desc_to_ErrorDesc;
      typ = error_typ;
    }

  let err_descr_string (err : t) =
    Pos.same_pos_as
      (String.concat ":"
         [
           err.descr.kind |> Pos.unmark;
           err.descr.major_code |> Pos.unmark;
           err.descr.minor_code |> Pos.unmark;
           err.descr.description |> Pos.unmark;
           err.descr.isisf |> Pos.unmark;
         ])
      err.name

  let compare (var1 : t) (var2 : t) = compare var1.id var2.id
end

type 'variable condition_data_ = {
  cond_number : rule_id Pos.marked;
  cond_expr : 'variable expression_ Pos.marked;
  cond_error : (Error.t[@opaque]) * 'variable option;
  cond_tags : Mast.chain_tag Pos.marked list;
}

let map_cond_data_var (f : 'v -> 'v2) (cond : 'v condition_data_) :
    'v2 condition_data_ =
  {
    cond_number = cond.cond_number;
    cond_expr = Pos.map_under_mark (map_expr_var f) cond.cond_expr;
    cond_error =
      (let e, v = cond.cond_error in
       (e, Option.map f v));
    cond_tags = cond.cond_tags;
  }

type condition_data = variable condition_data_

type idmap = Variable.t list Pos.VarNameToID.t
(** We translate string variables into first-class unique {!type:
    Mir.Variable.t}, so we need to keep a mapping between the two. A name is
    mapped to a list of variables because variables can be redefined in
    different rules *)

type exec_pass = { exec_pass_set_variables : literal Pos.marked VariableMap.t }

type program = {
  program_vars : VariableDict.t;
      (** A static register of all variables that can be used during a
          calculation *)
  program_rules : rule_data RuleMap.t;
      (** Definitions of variables, some may be removed during optimization
          passes *)
  program_conds : condition_data VariableMap.t;
      (** Conditions are affected to dummy variables *)
  program_idmap : idmap;
  program_exec_passes : exec_pass list;
}

(** {1 Helpers}*)

(** Throws an error in case of alias not found *)
let find_var_name_by_alias (p : program) (alias : string Pos.marked) : string =
  let v =
    VariableDict.fold
      (fun v acc ->
        match (acc, v.Variable.alias) with
        | Some _, _ | None, None -> acc
        | None, Some v_alias ->
            if v_alias = Pos.unmark alias then Some (Pos.unmark v.Variable.name)
            else None)
      p.program_vars None
  in
  match v with
  | Some v -> v
  | None ->
      Errors.raise_spanned_error
        (Format.asprintf "alias not found: %s" (Pos.unmark alias))
        (Pos.get_position alias)

let sort_by_lowest_exec_number v1 v2 =
  -compare_execution_number v1.Variable.execution_number
     v2.Variable.execution_number
(* here the minus sign is to have the "meaningful" execution numbers first, and
   the declarative execution number last *)

let sort_by_highest_exec_number v1 v2 =
  compare v1.Variable.execution_number v2.Variable.execution_number

let get_var_sorted_by_execution_number (p : program) (name : string) sort :
    Variable.t =
  let vars = Pos.VarNameToID.find name p.program_idmap |> List.sort sort in
  match vars with [] -> raise Not_found | hd :: _ -> hd

let find_var_by_name (p : program) (name : string Pos.marked) : Variable.t =
  try
    get_var_sorted_by_execution_number p (Pos.unmark name)
      sort_by_lowest_exec_number
  with Not_found -> (
    try
      let name = find_var_name_by_alias p name in
      get_var_sorted_by_execution_number p name sort_by_highest_exec_number
    with Not_found ->
      Errors.raise_spanned_error "unknown variable" (Pos.get_position name))

(** Explores the rules to find rule and variable data *)
let find_var_definition (p : program) (var : Variable.t) :
    rule_data * variable_data =
  (* using exceptions to cut short exploration *)
  let exception Found_rule of rule_data * variable_data in
  let exception Found_var of variable_data in
  try
    RuleMap.iter
      (fun _ rule_data ->
        try
          List.iter
            (fun (vid, def) -> if var.id = vid then raise (Found_var def))
            rule_data.rule_vars
        with Found_var def -> raise (Found_rule (rule_data, def)))
      p.program_rules;
    raise Not_found
  with Found_rule (rule, var) -> (rule, var)

let map_vars (f : Variable.t -> variable_data -> variable_data) (p : program) :
    program =
  let program_rules =
    RuleMap.map
      (fun rule_data ->
        let rule_vars =
          List.map
            (fun (vid, def) ->
              let var = VariableDict.find vid p.program_vars in
              (vid, f var def))
            rule_data.rule_vars
        in
        { rule_data with rule_vars })
      p.program_rules
  in
  { p with program_rules }

let fold_vars (f : Variable.t -> variable_data -> 'a -> 'a) (p : program)
    (acc : 'a) : 'a =
  RuleMap.fold
    (fun _ rule_data acc ->
      List.fold_left
        (fun acc (vid, def) ->
          let var = VariableDict.find vid p.program_vars in
          f var def acc)
        acc rule_data.rule_vars)
    p.program_rules acc

let is_dummy_variable (var : Variable.t) : bool =
  var.execution_number.rule_number = -1

let find_vars_by_io (p : program) (io_to_find : io) : VariableDict.t =
  fold_vars
    (fun var var_data acc ->
      if
        var_data.var_io = io_to_find
        && var
           = get_var_sorted_by_execution_number p (Pos.unmark var.name)
               sort_by_lowest_exec_number
      then VariableDict.add var acc
      else acc)
    p VariableDict.empty
