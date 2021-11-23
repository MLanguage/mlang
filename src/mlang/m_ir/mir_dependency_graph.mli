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

(** Defines the dependency graph of an M program *)

module RG : Graph.Sig.G
(** Dependency graph for the rules of the M program. Each node corresponds to a
    rule, each edge to variables use. The edges in the graph go from input to
    outputs. *)

val get_used_variables_ :
  Mir.expression Pos.marked -> Mir.VariableDict.t -> Mir.VariableDict.t
(** Add all the sucessors of [lvar] in the graph that are used by [e] *)

val get_used_variables : Mir.expression Pos.marked -> Mir.VariableDict.t
(** Calls the previous function with an empty dict *)

val create_rules_dependency_graph :
  Mir.program -> Mir.rule_id Mir.VariableMap.t -> RG.t

val check_for_cycle : RG.t -> Mir.program -> bool -> bool
(** Outputs [true] and a warning in case of cycles. *)

type rule_execution_order = Mir.rule_id list

val get_rules_execution_order : RG.t -> rule_execution_order
