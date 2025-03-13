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

type set_value = Com.Var.t Com.set_value

type expression = Com.Var.t Com.expression

type m_expression = expression Pos.marked

type instruction = (Com.Var.t, Com.Error.t) Com.instruction

type m_instruction = instruction Pos.marked

type target = (Com.Var.t, Com.Error.t) Com.target

type stats = {
  nb_calculated : int;
  nb_base : int;
  nb_input : int;
  nb_vars : int;
  nb_all_tmps : int;
  nb_all_refs : int;
  sz_calculated : int;
  sz_base : int;
  sz_input : int;
  sz_vars : int;
  sz_all_tmps : int;
  nb_all_tables : int;
  sz_all_tables : int;
  table_map : Com.Var.t IntMap.t;
}

type program = {
  program_safe_prefix : string;
  program_applications : Pos.t StrMap.t;
  program_var_categories : Com.CatVar.data Com.CatVar.Map.t;
  program_rule_domains : Com.rule_domain Com.DomainIdMap.t;
  program_verif_domains : Com.verif_domain Com.DomainIdMap.t;
  program_vars : Com.Var.t StrMap.t;
  program_alias : string Pos.marked StrMap.t;
  program_event_fields : Com.event_field StrMap.t;
  program_event_field_idxs : string IntMap.t;
  program_rules : string IntMap.t;
  program_verifs : string IntMap.t;
  program_chainings : string StrMap.t;
  program_errors : Com.Error.t StrMap.t;
  program_functions : target StrMap.t;
  program_targets : target StrMap.t;
  program_main_target : string;
  program_stats : stats;
}

val find_var_name_by_alias : program -> string Pos.marked -> string

val find_var_by_name : program -> string Pos.marked -> Com.Var.t
(** Get a variable for a given name or alias, because of SSA multiple variables
    share a name or alias. If an alias is provided, the variable returned is
    that with the lowest execution number. When a name is provided, then the
    variable with the highest execution number is returned. *)

val expand_functions : program -> program
(** Calls [expand_functions_expr] on the whole program *)
