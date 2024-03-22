(* Copyright (C) 2019-2021-2020 Inria, contributors: Denis Merigoux
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

type program = {
  targets : Mir.target_data Mir.TargetMap.t;
  main_function : string;
  mir_program : Mir.program;
}

val main_statements : program -> Mir.m_instruction list

val format_program : Format.formatter -> program -> unit

val remove_empty_conditionals : Mir.m_instruction list -> Mir.m_instruction list

val get_used_variables_ :
  Mir.expression Pos.marked -> Mir.VariableSet.t -> Mir.VariableSet.t

val get_used_variables : Mir.expression Pos.marked -> Mir.VariableSet.t
