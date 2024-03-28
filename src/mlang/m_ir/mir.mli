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

type loc_tgv = {
  loc_id : string;
  loc_cat : Com.CatVar.loc;
  loc_idx : int;
  loc_int : int;
}

type loc =
  | LocTgv of string * loc_tgv
  | LocTmp of string * int
  | LocIt of string * int

val set_loc_int : loc -> int -> loc

val set_loc_tgv : loc -> Com.CatVar.loc -> int -> loc

module Var : sig
  type id = string

  type tgv = {
    alias : string Pos.marked option;  (** Input variable have an alias *)
    descr : string Pos.marked;
        (** Description taken from the variable declaration *)
    attrs : int Pos.marked StrMap.t;
    cat : Com.CatVar.t;
    typ : Mast.value_typ option;
  }

  type scope = Tgv of tgv | Temp | It

  type t = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    id : id;
    is_table : int option;
    is_given_back : bool;
    loc : loc;
    scope : scope;
  }

  val tgv : t -> tgv

  val alias : t -> string Pos.marked option

  val alias_str : t -> string

  val descr : t -> string Pos.marked

  val descr_str : t -> string

  val attrs : t -> int Pos.marked StrMap.t

  val cat : t -> Com.CatVar.t

  val loc_tgv : t -> loc_tgv

  val loc_int : t -> int

  val is_temp : t -> bool

  val is_it : t -> bool

  val init_loc : loc_tgv

  val new_tgv :
    name:string Pos.marked ->
    is_table:int option ->
    is_given_back:bool ->
    alias:string Pos.marked option ->
    descr:string Pos.marked ->
    attrs:int Pos.marked StrMap.t ->
    cat:Com.CatVar.t ->
    typ:Mast.value_typ option ->
    t

  val new_temp :
    name:string Pos.marked -> is_table:int option -> loc_int:int -> t

  val new_it : name:string Pos.marked -> is_table:int option -> loc_int:int -> t

  val compare : t -> t -> int
end

type local_variable = { id : int }

(** Type of MIR values *)
type typ = Real

type set_value = Var.t Com.set_value

type expression = Var.t Com.expression

module VariableMap : MapExt.T with type key = Var.t
(** MIR programs are just mapping from variables to their definitions, and make
    a massive use of [VariableMap]. *)

module VariableSet : SetExt.T with type elt = Var.t

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

type instruction = Var.t Com.instruction

type m_instruction = instruction Pos.marked

type target_data = {
  target_name : string Pos.marked;
  target_file : string option;
  target_apps : string Pos.marked StrMap.t;
  target_tmp_vars : (Var.t * Pos.t * int option) StrMap.t;
  target_nb_tmps : int;
  target_sz_tmps : int;
  target_nb_its : int;
  target_prog : m_instruction list;
}

type verif_domain_data = {
  vdom_auth : Pos.t Com.CatVar.Map.t;
  vdom_verifiable : bool;
}

type verif_domain = verif_domain_data domain

type stats = {
  nb_calculated : int;
  nb_base : int;
  nb_input : int;
  nb_vars : int;
  nb_all_tmps : int;
  nb_all_its : int;
  sz_calculated : int;
  sz_base : int;
  sz_input : int;
  sz_vars : int;
  sz_all_tmps : int;
}

type program = {
  program_safe_prefix : string;
  program_applications : Pos.t StrMap.t;
  program_var_categories : Com.CatVar.data Com.CatVar.Map.t;
  program_rule_domains : rule_domain Mast.DomainIdMap.t;
  program_verif_domains : verif_domain Mast.DomainIdMap.t;
  program_vars : Var.t StrMap.t;
      (** A static register of all variables that can be used during a
          calculation *)
  program_targets : target_data TargetMap.t;
  program_main_target : string;
  program_stats : stats;
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

val map_expr_var : ('v -> 'v2) -> 'v Com.expression -> 'v2 Com.expression

val fold_expr_var : ('a -> 'v -> 'a) -> 'a -> 'v Com.expression -> 'a

val cond_cats_to_set : int Com.CatVar.Map.t -> Com.CatVar.Set.t

val find_var_by_name : program -> string Pos.marked -> Var.t
(** Get a variable for a given name or alias, because of SSA multiple variables
    share a name or alias. If an alias is provided, the variable returned is
    that with the lowest execution number. When a name is provided, then the
    variable with the highest execution number is returned. *)

val expand_functions : program -> program
(** Calls [expand_functions_expr] on the whole program *)
