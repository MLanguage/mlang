(* Copyright (C) 2020 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

type block_id = int

module BlockMap = Map.Make (Int)

type stmt = stmt_kind Pos.marked

and stmt_kind =
  | SAssign of Mir.Variable.t * Mir.variable_data
  | SConditional of Mir.expression * block_id * block_id * block_id
      (** The first two block ids are the true and false branch, the third is the join point after *)
  | SVerif of Mir.condition_data
  | SGoto of block_id

type block = stmt list

type program = {
  blocks : block BlockMap.t;
  entry_block : block_id;
  idmap : Mir.idmap;
  mir_program : Mir.program;
  outputs : unit Mir.VariableMap.t;
}

let count_instr (p : program) : int =
  BlockMap.fold
    (fun _ block acc ->
      List.fold_left
        (fun acc s ->
          match Pos.unmark s with
          | SConditional _ | SAssign _ | SVerif _ -> acc + 1
          | SGoto _ -> acc)
        acc block)
    p.blocks 0

module CFG = Graph.Persistent.Digraph.ConcreteBidirectional (struct
  type t = block_id

  let hash v = v

  let compare v1 v2 = compare v1 v2

  let equal v1 v2 = v1 = v2
end)

let get_cfg (p : program) : CFG.t =
  let g = CFG.empty in
  BlockMap.fold
    (fun (id : block_id) (block : block) (g : CFG.t) ->
      let g = CFG.add_vertex g id in
      List.fold_left
        (fun g stmt ->
          match Pos.unmark stmt with
          | SGoto next_b -> CFG.add_edge g id next_b
          | SConditional (_, next_b1, next_b2, _) ->
              let g = CFG.add_edge g id next_b1 in
              CFG.add_edge g id next_b2
          | _ -> g)
        g block)
    p.blocks g

module Topological = Graph.Topological.Make (CFG)
module Dominators = Graph.Dominator.Make (CFG)

module Reachability =
  Graph.Fixpoint.Make
    (CFG)
    (struct
      type vertex = CFG.E.vertex

      type edge = CFG.E.t

      type g = CFG.t

      type data = bool

      let direction = Graph.Fixpoint.Forward

      let equal = ( = )

      let join = ( || )

      let analyze _ x = x
    end)
