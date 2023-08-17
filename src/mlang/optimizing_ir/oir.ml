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

module BlockMap = IntMap

type stmt = stmt_kind Pos.marked

and stmt_kind =
  | SAssign of Bir.variable * Bir.variable_def
  | SConditional of Bir.expression * block_id * block_id * block_id
      (** The first two block ids are the true and false branch, the third is
          the join point after *)
  | SVerif of Bir.condition_data
  | SGoto of block_id
  | SRovCall of Bir.rov_id
  | SFunctionCall of Bir.function_name * Mir.variable list

type block = stmt list

type cfg = {
  blocks : block BlockMap.t;
  entry_block : block_id;
  exit_block : block_id;
}

type mpp_function = { cfg : cfg; is_verif : bool }

type rov_code = Rule of cfg | Verif of cfg

type rov = { id : Bir.rov_id; name : string Pos.marked; code : rov_code }

type program = {
  mpp_functions : mpp_function Bir.FunctionMap.t;
  rules_and_verifs : rov Bir.ROVMap.t;
  idmap : Mir.idmap;
  mir_program : Mir.program;
  outputs : unit Bir.VariableMap.t;
  main_function : Bir.function_name;
  context : Bir.program_context option;
}

let map_program_cfgs (f : cfg -> cfg) (p : program) : program =
  let mpp_functions =
    Bir.FunctionMap.map
      (fun func -> { func with cfg = f func.cfg })
      p.mpp_functions
  in
  let rules_and_verifs =
    Bir.ROVMap.map
      (fun rov ->
        {
          rov with
          code =
            (match rov.code with
            | Rule cfg -> Rule (f cfg)
            | Verif cfg -> Verif (f cfg));
        })
      p.rules_and_verifs
  in
  {
    mpp_functions;
    rules_and_verifs;
    idmap = p.idmap;
    mir_program = p.mir_program;
    outputs = p.outputs;
    main_function = p.main_function;
    context = p.context;
  }

let count_instr (p : program) : int =
  let aux acc stmts =
    List.fold_left
      (fun acc s ->
        match Pos.unmark s with
        | SConditional _ | SAssign _ | SVerif _ | SRovCall _ | SFunctionCall _
          ->
            acc + 1
        | SGoto _ -> acc)
      acc stmts
  in
  let count =
    Bir.FunctionMap.fold
      (fun _ { cfg; _ } acc ->
        BlockMap.fold (fun _ block acc -> aux acc block) cfg.blocks acc)
      p.mpp_functions 0
  in
  Bir.ROVMap.fold
    (fun _ { code; _ } acc ->
      match code with
      | Rule cfg | Verif cfg ->
          BlockMap.fold (fun _ block acc -> aux acc block) cfg.blocks acc)
    p.rules_and_verifs count

module CFG = Graph.Persistent.Digraph.ConcreteBidirectional (struct
  type t = block_id

  let hash v = v

  let compare v1 v2 = compare v1 v2

  let equal v1 v2 = v1 = v2
end)

let get_cfg (cfg : cfg) : CFG.t =
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
    cfg.blocks g

module Topological = Graph.Topological.Make (CFG)
module Dominators = Graph.Dominator.Make (CFG)
module Paths = Graph.Path.Check (CFG)

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
