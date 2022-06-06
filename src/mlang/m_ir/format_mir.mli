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

val format_execution_number : Format.formatter -> Mir.execution_number -> unit

val format_execution_number_short :
  Format.formatter -> Mir.execution_number -> unit

val format_typ : Format.formatter -> Mir.typ -> unit

val format_subtype : Format.formatter -> Mir.variable_subtype -> unit

val format_func : Format.formatter -> Mir.func -> unit

val format_literal : Format.formatter -> Mir.literal -> unit

val format_expression : Format.formatter -> Mir.expression -> unit

val format_variable_def : Format.formatter -> Mir.variable_def -> unit

val format_variable_data : Format.formatter -> Mir.variable_data -> unit

val format_variables :
  Format.formatter -> Mir.variable_data Mir.VariableMap.t -> unit

val format_error : Format.formatter -> Mir.Error.t -> unit

val format_precondition : Format.formatter -> Mir.condition_data -> unit

val format_program_rules :
  Format.formatter -> Mir.VariableDict.t -> Mir.rule_data Mir.RuleMap.t -> unit

val format_program_conds :
  Format.formatter -> Mir.condition_data Mir.VariableMap.t -> unit

val format_program : Format.formatter -> Mir.program -> unit

val format_variable : Format.formatter -> Mir.Variable.t -> unit

val format_io : Format.formatter -> Mir.io -> unit
