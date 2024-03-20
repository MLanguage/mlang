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

type stmt = Mir.Variable.t Com.m_instruction

type target_function = {
  file : string option;
  tmp_vars : (Mir.Variable.t * Pos.t * int option) StrMap.t;
  stmts : stmt list;
}

type program = {
  targets : target_function Mir.TargetMap.t;
  main_function : string;
  mir_program : Mir.program;
}

val main_statements : program -> stmt list

val format_program : Format.formatter -> program -> unit

val remove_empty_conditionals : stmt list -> stmt list

val get_used_variables_ :
  Mir.expression Pos.marked -> Mir.VariableSet.t -> Mir.VariableSet.t

val get_used_variables : Mir.expression Pos.marked -> Mir.VariableSet.t
