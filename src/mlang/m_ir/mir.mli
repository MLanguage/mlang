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
    cats : Com.cat_variable option;
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
    cats:Com.cat_variable option ->
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

type set_value = Variable.t Com.set_value

type expression = Variable.t Com.expression_

module VariableMap : MapExt.T with type key = Variable.t
(** MIR programs are just mapping from variables to their definitions, and make
    a massive use of [VariableMap]. *)

module VariableSet : SetExt.T with type elt = Variable.t

module LocalVariableMap : sig
  include MapExt.T with type key = local_variable
end

module IndexMap : IntMap.T

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
  | PrintIndent of 'variable Com.expression_ Pos.marked
  | PrintExpr of 'variable Com.expression_ Pos.marked * int * int

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
  | Affectation of
      variable_id * (int * expression Pos.marked) option * expression Pos.marked
  | IfThenElse of
      expression * instruction Pos.marked list * instruction Pos.marked list
  | ComputeTarget of string Pos.marked
  | VerifBlock of instruction Pos.marked list
  | Print of Mast.print_std * Variable.t print_arg Pos.marked list
  | Iterate of
      variable_id
      * Com.CatVarSet.t
      * expression Pos.marked
      * instruction Pos.marked list
  | Restore of
      Pos.t VariableMap.t
      * (Variable.t * Com.CatVarSet.t * expression Pos.marked) list
      * instruction Pos.marked list
  | RaiseError of error * string option
  | CleanErrors
  | ExportErrors
  | FinalizeErrors

type target_data = {
  target_name : string Pos.marked;
  target_file : string option;
  target_apps : string Pos.marked StrMap.t;
  target_tmp_vars : (Variable.t * Pos.t * int option) StrMap.t;
  target_prog : instruction Pos.marked list;
}

type verif_domain_data = { vdom_auth : Com.CatVarSet.t; vdom_verifiable : bool }

type verif_domain = verif_domain_data domain

type program = {
  program_safe_prefix : string;
  program_applications : Pos.t StrMap.t;
  program_var_categories : Com.cat_variable_data Com.CatVarMap.t;
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

val false_literal : Com.literal

val true_literal : Com.literal

val find_var_name_by_alias : program -> string Pos.marked -> string

val map_expr_var : ('v -> 'v2) -> 'v Com.expression_ -> 'v2 Com.expression_

val fold_expr_var : ('a -> 'v -> 'a) -> 'a -> 'v Com.expression_ -> 'a

val cond_cats_to_set : int Com.CatVarMap.t -> Com.CatVarSet.t

val find_var_by_name : program -> string Pos.marked -> Variable.t
(** Get a variable for a given name or alias, because of SSA multiple variables
    share a name or alias. If an alias is provided, the variable returned is
    that with the lowest execution number. When a name is provided, then the
    variable with the highest execution number is returned. *)

val mast_to_catvar :
  'a Com.CatVarMap.t -> string Pos.marked list Pos.marked -> Com.cat_variable

val expand_functions : program -> program
(** Calls [expand_functions_expr] on the whole program *)
