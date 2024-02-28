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

val get_used_variables_ :
  Mir.expression Pos.marked -> Mir.VariableDict.t -> Mir.VariableDict.t
(** Add all the sucessors of [lvar] in the graph that are used by [e] *)

val get_used_variables : Mir.expression Pos.marked -> Mir.VariableDict.t
(** Calls the previous function with an empty dict *)

type rule_execution_order = Mir.rov_id list

val get_var_dependencies :
  ?strict:bool ->
  Mir.program ->
  rule_execution_order ->
  Mir.variable ->
  Mir.variable list
