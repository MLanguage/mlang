(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** Intepretation has to be done following the topological order, but cycles complicate that. We
    indeed have to partition the graph into its strongly connected components and create a new graph
    where the nodes are the SCC and there is an edge between [scc1] and [scc2] if a variable in
    [scc1] uses a variable from [scc2].

    Then, we can execute the program following topological order on this new graph. If a SCC has
    more than one variable, then the computation is done in multiple passes, each new pass taking
    the previous outputs as inputs for all the varaibles in the SCC. *)

open Mir
module ExecutionOrder = Graph.Topological.Make (Dependency.DepGraph)

type execution_order = Variable.t list
(** Each map is the set of variables defined circularly in this strongly connected component *)

let get_execution_order (dep_graph : Dependency.DepGraph.t) : execution_order =
  List.rev (ExecutionOrder.fold (fun var exec_order -> var :: exec_order) dep_graph [])

(** This custom visitor uses [get_execution_order] to visit all the expressions in the program in
    the correct order. *)
class ['self] program_iter =
  object (self : 'self)
    inherit [_] variable_data_iter

    inherit [_] condition_data_iter [@@warning "-7"]

    method visit_program env (this : Mir.program) =
      let dep_graph = Dependency.create_dependency_graph this in
      let exec_order = get_execution_order dep_graph in
      List.iter
        (fun var ->
          try
            let data = VariableMap.find var this.program_vars in
            self#visit_variable_data env data
          with Not_found -> (
            try
              let cond = VariableMap.find var this.program_conds in
              self#visit_condition_data env cond
            with Not_found -> assert false ))
        exec_order
  end

let fold_on_vars (f : Variable.t -> 'a -> 'a) (dep_graph : Dependency.DepGraph.t) (init : 'a) : 'a =
  let exec_order = get_execution_order dep_graph in
  List.fold_left (fun acc v -> f v acc) init exec_order
