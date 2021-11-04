(* Copyright (C) 2019-2021-2020 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr>
   Raphaël Monat <raphael.monat@lip6.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

type rule_id = Mir.rule_id

module RuleMap = Mir.RuleMap

type rule = { rule_id : rule_id; rule_name : string; rule_stmts : stmt list }

and stmt = stmt_kind Pos.marked

and stmt_kind =
  | SAssign of Mir.Variable.t * Mir.variable_data
  | SConditional of Mir.expression * stmt list * stmt list
  | SVerif of Mir.condition_data
  | SRuleCall of rule_id

type program = {
  rules : rule RuleMap.t;
  statements : stmt list;
  idmap : Mir.idmap;
  mir_program : Mir.program;
  outputs : unit Mir.VariableMap.t;
}

val squish_statements : program -> int -> string -> program

val get_all_statements : program -> stmt list

val count_instructions : program -> int

val get_assigned_variables : program -> Mir.VariableDict.t

val get_local_variables : program -> unit Mir.LocalVariableMap.t

val get_locals_size : program -> int

val remove_empty_conditionals : stmt list -> stmt list
