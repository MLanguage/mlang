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

open Mvg

type scc_id = int

module ExecutionGraph = Graph.Imperative.Digraph.ConcreteBidirectional (struct
  type t = int (* these are the strongly connected components identifiers *)

  let hash v = v

  let compare v1 v2 = compare v1 v2

  let equal v1 v2 = v1 = v2
end)

type execution_scc_graph = {
  execution_scc_graph : ExecutionGraph.t;
  execution_scc_graph_contents : unit VariableMap.t array;
}

let create_execution_scc_graph (p : Mvg.program) : execution_scc_graph =
  let dep_graph = Dependency.create_dependency_graph p in
  let nb_scc, scc_assignment = Dependency.SCC.scc dep_graph in
  let interp_order =
    {
      execution_scc_graph = ExecutionGraph.create ();
      execution_scc_graph_contents = Array.make nb_scc VariableMap.empty;
    }
  in
  Dependency.DepGraph.iter_vertex
    (fun var ->
      let succs_var = Dependency.DepGraph.succ dep_graph var in
      let var_scc = scc_assignment var in
      interp_order.execution_scc_graph_contents.(var_scc) <-
        VariableMap.add var () interp_order.execution_scc_graph_contents.(var_scc);
      List.iter
        (fun succ ->
          ExecutionGraph.add_edge interp_order.execution_scc_graph var_scc (scc_assignment succ))
        succs_var)
    dep_graph;
  interp_order

module ExecutionOrder = Graph.Topological.Make (ExecutionGraph)

type execution_order = unit VariableMap.t list
(** Each map is the set of variables defined circularly in this strongly connected component *)

let get_execution_order (p : Mvg.program) : execution_order =
  let execution_graph = create_execution_scc_graph p in
  List.rev
    (ExecutionOrder.fold
       (fun scc_id exec_order ->
         let new_scc = execution_graph.execution_scc_graph_contents.(scc_id) in
         new_scc :: exec_order)
       execution_graph.execution_scc_graph [])

(** This custom visitor uses [get_execution_order] to visit all the expressions in the program in
    the correct order. *)
class ['self] program_iter =
  object (self : 'self)
    inherit [_] variable_data_iter

    inherit [_] condition_data_iter [@@warning "-7"]

    method visit_program env this =
      let exec_order = get_execution_order this in
      List.iter
        (fun scc ->
          VariableMap.iter
            (fun var () ->
              try
                let data = VariableMap.find var this.program_vars in
                self#visit_variable_data env data
              with Not_found -> (
                try
                  let cond = VariableMap.find var this.program_conds in
                  self#visit_condition_data env cond
                with Not_found -> assert false ))
            scc)
        exec_order
  end

let fold_on_vars (f : Variable.t -> 'a -> 'a) (p : Mvg.program) (init : 'a) : 'a =
  let exec_order = get_execution_order p in
  List.fold_left (fun acc scc -> Mvg.VariableMap.fold (fun v () a -> f v a) scc acc) init exec_order
