(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by the DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-C license and that you accept its terms.
*)

(**
   Intepretation has to be done following the topological order, but cycles complicate that.
   We indeed have to partition the graph into its strongly connected components and create a
   new graph where the nodes are the SCC and there is an edge between [scc1] and [scc2] if a variable
   in [scc1] uses a variable from [scc2].

   Then, we can execute the program following topological order on this new graph. If a SCC has more
   than one variable, then the computation is done in multiple passes, each new pass taking the
   previous outputs as inputs for all the varaibles in the SCC.
*)

open Mvg

type scc_id = int

module ExecutionGraph = Graph.Imperative.Digraph.ConcreteBidirectional(struct
    type t = int (** these are the strongly connected components identifiers *)
    let hash v = v
    let compare v1 v2 = compare v1 v2
    let equal v1 v2 = v1 = v2
  end)

type execution_scc_graph = {
  execution_scc_graph : ExecutionGraph.t;
  execution_scc_graph_contents: unit VariableMap.t array
}

let create_execution_scc_graph (p : Mvg.program) : execution_scc_graph =
  let dep_graph = Dependency.create_dependency_graph p in
  let (nb_scc, scc_assignment) = Dependency.SCC.scc dep_graph in
  let interp_order = {
    execution_scc_graph = ExecutionGraph.create ();
    execution_scc_graph_contents = Array.make nb_scc VariableMap.empty;
  } in
  Dependency.DepGraph.iter_vertex (fun var ->
      let succs_var = Dependency.DepGraph.succ dep_graph var in
      let var_scc = scc_assignment var in
      interp_order.execution_scc_graph_contents.(var_scc) <- VariableMap.add var ()
          interp_order.execution_scc_graph_contents.(var_scc);
      List.iter (fun succ ->
          ExecutionGraph.add_edge interp_order.execution_scc_graph var_scc (scc_assignment succ)
        ) succs_var
    ) dep_graph;
  interp_order

module ExecutionOrder = Graph.Topological.Make(ExecutionGraph)

(** Each map is the set of variables defined circularly in this strongly connected component *)
type execution_order = unit VariableMap.t list

let get_execution_order (p: Mvg.program) : execution_order =
  let execution_graph = create_execution_scc_graph p in
  List.rev (ExecutionOrder.fold (fun scc_id exec_order ->
      let new_scc = execution_graph.execution_scc_graph_contents.(scc_id) in
      new_scc::exec_order
    ) execution_graph.execution_scc_graph [])

let fold_on_vars (f: Variable.t -> 'a -> 'a) (p: Mvg.program)  (init : 'a) : 'a =
  let exec_order = get_execution_order p in
  List.fold_left (fun acc scc ->
      Mvg.VariableMap.fold (fun v () a-> f v a) scc acc) init exec_order
