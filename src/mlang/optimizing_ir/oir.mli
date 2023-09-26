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

module BlockMap : MapExt.T with type key = block_id

type stmt = stmt_kind Pos.marked

and stmt_kind =
  | SAssign of Bir.variable * Bir.variable_def
  | SConditional of Bir.expression * block_id * block_id * block_id
  | SVerif of Bir.condition_data
  | SGoto of block_id
  | SRovCall of Bir.rov_id
  | SFunctionCall of Bir.function_name * Mir.Variable.t list
  | SPrint of Mast.print_std * Bir.variable Mir.print_arg list
  | SIterate of
      Bir.variable * Mir.CatVarSet.t * Bir.expression * block_id * block_id
  | SRestore of
      Bir.VariableSet.t
      * (Bir.variable * Mir.CatVarSet.t * Bir.expression) list
      * block_id
      * block_id

type block = stmt list

type cfg = {
  blocks : block BlockMap.t;
  entry_block : block_id;
  exit_block : block_id;
}

type mpp_function = { cfg : cfg; is_verif : bool }

type target_function = {
  tmp_vars : (Bir.variable * Pos.t * int option) StrMap.t;
  cfg : cfg;
  is_verif : bool;
}

type rov_code = Rule of cfg | Verif of cfg

type rov = { id : Bir.rov_id; name : string Pos.marked; code : rov_code }

type program = {
  mpp_functions : mpp_function Bir.FunctionMap.t;
  targets : target_function Mir.TargetMap.t;
  rules_and_verifs : rov Bir.ROVMap.t;
  idmap : Mir.idmap;
  mir_program : Mir.program;
  outputs : unit Bir.VariableMap.t;
  main_function : Bir.function_name;
  context : Bir.program_context option;
}

val map_program_cfgs : (cfg -> cfg) -> program -> program

val count_instr : program -> int

module CFG : Graph.Sig.P with type V.t = int

val get_cfg : cfg -> CFG.t

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
