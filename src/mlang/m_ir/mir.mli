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

type cat_computed = Base | GivenBack

module CatCompSet : SetExt.T with type elt = cat_computed

type cat_variable = CatInput of StrSet.t | CatComputed of CatCompSet.t

val pp_cat_variable : Format.formatter -> cat_variable -> unit

val compare_cat_variable : cat_variable -> cat_variable -> int

module CatVarSet : SetExt.T with type elt = cat_variable

module CatVarMap : MapExt.T with type key = cat_variable

type cat_variable_loc = LocCalculated | LocBase | LocInput

type cat_variable_data = {
  id : cat_variable;
  id_str : string;
  id_int : int;
  loc : cat_variable_loc;
  pos : Pos.t;
  attributs : Pos.t StrMap.t;
}

type variable_id = string
(** Each variable has an unique ID *)

module Variable : sig
  type id = variable_id

  type t = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    alias : string Pos.marked option;  (** Input variable have an alias *)
    id : variable_id;
    descr : string Pos.marked;
        (** Description taken from the variable declaration *)
    attributes : int Pos.marked StrMap.t;
    cats : cat_variable option;
    typ : Mast.value_typ option;
    is_table : int option;
    is_temp : bool;
    is_it : bool;
  }

  val new_var :
    string Pos.marked ->
    string Pos.marked option ->
    string Pos.marked ->
    attributes:int Pos.marked StrMap.t ->
    cats:cat_variable option ->
    typ:Mast.value_typ option ->
    is_table:int option ->
    is_temp:bool ->
    is_it:bool ->
    t

  val compare : t -> t -> int
end

type local_variable = { id : int }

(** Type of MIR values *)
type typ = Real

type literal = Float of float | Undefined

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
  | VerifNumber
  | ComplNumber

(** MIR expressions are simpler than M; there are no loops or syntaxtic sugars.

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
  | LocalVar of local_variable
  | LocalLet of
      local_variable
      * 'variable expression_ Pos.marked
      * 'variable expression_ Pos.marked
  | NbCategory of CatVarSet.t
  | Attribut of string Pos.marked * 'variable * string Pos.marked
  | Size of 'variable
  | NbAnomalies
  | NbDiscordances
  | NbInformatives
  | NbBloquantes

type expression = Variable.t expression_

module VariableMap : MapExt.T with type key = Variable.t
(** MIR programs are just mapping from variables to their definitions, and make
    a massive use of [VariableMap]. *)

module VariableSet : SetExt.T with type elt = Variable.t

module LocalVariableMap : sig
  include MapExt.T with type key = local_variable
end

module IndexMap : IntMap.T

type 'variable index_def =
  | IndexTable of
      ('variable expression_ Pos.marked IndexMap.t[@name "index_map"])
  | IndexGeneric of 'variable * 'variable expression_ Pos.marked

type 'variable variable_def_ =
  | SimpleVar of 'variable expression_ Pos.marked
  | TableVar of int * 'variable index_def
  | InputVar

type variable_def = Variable.t variable_def_

type 'variable variable_data_ = {
  var_definition : 'variable variable_def_;
  var_typ : typ option;
      (** The typing info here comes from the variable declaration in the source
          program *)
}

type variable_data = Variable.t variable_data_

type rov_id = RuleID of int | VerifID of int

module RuleMap : MapExt.T with type key = rov_id

module TargetMap : StrMap.T

type 'a domain = {
  dom_id : Mast.DomainId.t Pos.marked;
  dom_names : Pos.t Mast.DomainIdMap.t;
  dom_by_default : bool;
  dom_min : Mast.DomainIdSet.t;
  dom_max : Mast.DomainIdSet.t;
  dom_rov : IntSet.t;
  dom_data : 'a;
  dom_used : int Pos.marked option;
}

type rule_domain_data = { rdom_computable : bool }

type rule_domain = rule_domain_data domain

type 'variable print_arg =
  | PrintString of string
  | PrintName of string Pos.marked * Variable.t
  | PrintAlias of string Pos.marked * Variable.t
  | PrintIndent of 'variable expression_ Pos.marked
  | PrintExpr of 'variable expression_ Pos.marked * int * int

(** Errors are first-class objects *)

type error = {
  name : string Pos.marked;  (** The position is the variable declaration *)
  kind : string Pos.marked;
  major_code : string Pos.marked;
  minor_code : string Pos.marked;
  description : string Pos.marked;
  isisf : string Pos.marked;
  typ : Mast.error_typ;
}

module Error : sig
  type t = error = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    kind : string Pos.marked;
    major_code : string Pos.marked;
    minor_code : string Pos.marked;
    description : string Pos.marked;
    isisf : string Pos.marked;
    typ : Mast.error_typ;
  }

  val new_error : string Pos.marked -> Mast.error_ -> Mast.error_typ -> error

  val err_descr_string : t -> string Pos.marked

  val compare : t -> t -> int
end

type instruction =
  | Affectation of variable_id * variable_data
  | IfThenElse of
      expression * instruction Pos.marked list * instruction Pos.marked list
  | ComputeTarget of string Pos.marked
  | VerifBlock of instruction Pos.marked list
  | Print of Mast.print_std * Variable.t print_arg Pos.marked list
  | Iterate of
      variable_id
      * CatVarSet.t
      * expression Pos.marked
      * instruction Pos.marked list
  | Restore of
      Pos.t VariableMap.t
      * (Variable.t * CatVarSet.t * expression Pos.marked) list
      * instruction Pos.marked list
  | RaiseError of error * string option
  | CleanErrors
  | ExportErrors
  | FinalizeErrors

type rule_data = {
  rule_apps : Pos.t StrMap.t;
  rule_domain : rule_domain;
  rule_chain : (string * rule_domain) option;
  rule_vars : instruction Pos.marked list;
  rule_number : rov_id Pos.marked;
}

type target_data = {
  target_name : string Pos.marked;
  target_file : string option;
  target_apps : string Pos.marked StrMap.t;
  target_tmp_vars : (Variable.t * Pos.t * int option) StrMap.t;
  target_prog : instruction Pos.marked list;
}

type verif_domain_data = { vdom_auth : CatVarSet.t; vdom_verifiable : bool }

type verif_domain = verif_domain_data domain

type 'variable condition_data_ = {
  cond_seq_id : int;
  cond_number : rov_id Pos.marked;
  cond_domain : verif_domain;
  cond_expr : 'variable expression_ Pos.marked;
  cond_error : error * 'variable option;
  cond_cats : int CatVarMap.t;
}

type condition_data = Variable.t condition_data_

type program = {
  program_safe_prefix : string;
  program_applications : Pos.t StrMap.t;
  program_var_categories : cat_variable_data CatVarMap.t;
  program_rule_domains : rule_domain Mast.DomainIdMap.t;
  program_verif_domains : verif_domain Mast.DomainIdMap.t;
  program_vars : Variable.t StrMap.t;
      (** A static register of all variables that can be used during a
          calculation *)
  program_targets : target_data TargetMap.t;
}

(** Local variables don't appear in the M source program but can be introduced
    by let bindings when translating to MIR. They should be De Bruijn indices
    but instead are unique globals identifiers out of laziness. *)
module LocalVariable : sig
  type t = local_variable = { id : int }

  val new_var : unit -> t

  val compare : t -> t -> int
end

val false_literal : literal

val true_literal : literal

val num_of_rule_or_verif_id : rov_id -> int

val find_var_name_by_alias : program -> string Pos.marked -> string

val map_expr_var : ('v -> 'v2) -> 'v expression_ -> 'v2 expression_

val fold_expr_var : ('a -> 'v -> 'a) -> 'a -> 'v expression_ -> 'a

val map_var_def_var : ('v -> 'v2) -> 'v variable_def_ -> 'v2 variable_def_

val map_cond_data_var : ('v -> 'v2) -> 'v condition_data_ -> 'v2 condition_data_

val cond_cats_to_set : int CatVarMap.t -> CatVarSet.t

val find_var_definition : program -> Variable.t -> rule_data * variable_data

val fresh_rule_num : unit -> int

val initial_undef_rule_id : rov_id

val find_var_by_name : program -> string Pos.marked -> Variable.t
(** Get a variable for a given name or alias, because of SSA multiple variables
    share a name or alias. If an alias is provided, the variable returned is
    that with the lowest execution number. When a name is provided, then the
    variable with the highest execution number is returned. *)

val mast_to_catvar :
  'a CatVarMap.t -> string Pos.marked list Pos.marked -> cat_variable

val expand_functions : program -> program
(** Calls [expand_functions_expr] on the whole program *)
