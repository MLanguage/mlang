(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux
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

type block_id = int

module BlockMap : Map.S with type key = block_id

type stmt = stmt_kind Pos.marked

and stmt_kind =
  | SAssign of Bir.variable * Bir.variable_data
  | SConditional of Bir.expression * block_id * block_id * block_id
  | SVerif of Bir.condition_data
  | SGoto of block_id
  | SRuleCall of Bir.rov_id * string Pos.marked * stmt list
  | SFunctionCall of Bir.function_name * Mir.variable list

type block = stmt list

type program = {
  blocks : block BlockMap.t;
  entry_block : block_id;
  exit_block : block_id;
  idmap : Mir.idmap;
  mir_program : Mir.program;
  outputs : unit Bir.VariableMap.t;
  main_function : Bir.function_name;
}

val count_instr : program -> int

module CFG : Graph.Sig.P with type V.t = int

val get_cfg : program -> CFG.t

module Topological : sig
  val fold : (CFG.V.t -> 'a -> 'a) -> CFG.t -> 'a -> 'a

  val iter : (CFG.V.t -> unit) -> CFG.t -> unit
end

module Dominators : Graph.Dominator.S with type vertex = int and type t = CFG.t

module Paths : sig
  type path_checker = Graph.Path.Check(CFG).path_checker

  val create : CFG.t -> path_checker

  val check_path : path_checker -> CFG.V.t -> CFG.V.t -> bool
end

module Reachability : sig
  val analyze : (CFG.V.t -> bool) -> CFG.t -> CFG.V.t -> bool
end
