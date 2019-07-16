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

(** Defines the dependency graph of an M program *)

(** Each node corresponds to a variable, each edge to a variable use.
    The edges in the graph go from output to inputs. *)
module DepGraph = Graph.Persistent.Digraph.ConcreteBidirectional(struct
    type t = Mvg.Variable.t
    let hash v = v.Mvg.Variable.id
    let compare v1 v2 = compare v1.Mvg.Variable.id v2.Mvg.Variable.id
    let equal v1 v2 = v1.Mvg.Variable.id = v2.Mvg.Variable.id
  end)

(** Add all the sucessors of [lvar] in the graph that are used by [e] *)
let rec add_usages (lvar: Mvg.Variable.t) (e: Mvg.expression Ast.marked) (acc: DepGraph.t) : DepGraph.t =
  let acc = DepGraph.add_vertex acc lvar in
  let add_edge acc var lvar =
    DepGraph.add_edge acc var lvar
  in
  match Ast.unmark e with
  | Mvg.Comparison (_, e1, e2) | Mvg.Binop (_, e1, e2 )
  | Mvg.LocalLet (_, e1, e2) ->
    let acc = add_usages lvar e1 acc in
    let acc = add_usages lvar e2 acc in
    acc
  | Mvg.Unop (_, e) ->
    add_usages lvar e acc
  | Mvg.Index ((var,_ ), e) ->
    let acc = add_edge acc var lvar in
    let acc = add_usages lvar e acc in
    acc
  | Mvg.Conditional (e1, e2, e3) ->
    let acc = add_usages lvar e1 acc in
    let acc = add_usages lvar e2 acc in
    let acc = add_usages lvar e3 acc in
    acc
  | Mvg.FunctionCall (_, args) ->
    List.fold_left (fun acc arg ->
        add_usages lvar arg acc) acc args
  | Mvg.LocalVar _
  | Mvg.Literal _
  | Mvg.GenericTableIndex
  | Mvg.Error -> acc
  | Mvg.Var var ->
    add_edge acc var lvar

(** The dependency graph also includes nodes for the conditions to be checked at execution *)
let create_dependency_graph (p: Mvg.program) : DepGraph.t =
  let g = Mvg.VariableMap.fold (fun var def acc ->
      match def.Mvg.var_definition with
      | Mvg.InputVar -> DepGraph.add_vertex acc var
      | Mvg.SimpleVar e -> add_usages var e acc
      | Mvg.TableVar (_, def) -> begin match def with
          | Mvg.IndexGeneric e -> add_usages var e acc
          | Mvg.IndexTable es -> Mvg.IndexMap.fold (fun _ e acc ->
              add_usages var e acc
            ) es acc
        end
    ) p.program_vars DepGraph.empty in
  Mvg.VariableMap.fold (fun cond_var cond acc ->
      add_usages cond_var cond.Mvg.cond_expr acc
    ) p.program_conds g

let program_when_printing : Mvg.program option ref = ref None

(** The graph is output in the Dot format *)
module Dot = Graph.Graphviz.Dot(struct
    include DepGraph (* use the graph module from above *)

    let edge_attributes _ = [`Color 0xffa366]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = begin match !program_when_printing with
      | None -> []
      | Some p ->
        let input_color = 0x66b5ff in
        let output_color = 0xE6E600 in
        let cond_color = 0x666633 in
        let regular_color = 0x8585ad in
        let text_color = 0xf2f2f2 in
        try
          let var_data =
            Mvg.VariableMap.find v p.program_vars

          in
          match var_data.Mvg.var_io with
          | Mvg.Input -> [
              `Fillcolor input_color; `Shape `Box; `Style `Filled; `Fontcolor text_color;
              `Label (Printf.sprintf "%s\n%s"
                        (match v.Mvg.Variable.alias with Some s -> s | None -> Ast.unmark v.Mvg.Variable.name)
                        (Ast.unmark v.Mvg.Variable.descr)
                     )
            ]
          | Mvg.Regular -> [
              `Fillcolor regular_color; `Style `Filled; `Shape `Box; `Fontcolor text_color;
              `Label (Printf.sprintf "%s\n%s"
                        (Ast.unmark v.Mvg.Variable.name)
                        (Ast.unmark v.Mvg.Variable.descr)
                     )
            ]
          | Mvg.Output -> [
              `Fillcolor output_color; `Shape `Box; `Style `Filled; `Fontcolor text_color;
              `Label (Printf.sprintf "%s\n%s"
                        (Ast.unmark v.Mvg.Variable.name)
                        (Ast.unmark v.Mvg.Variable.descr)
                     )
            ]
        with
        | Not_found ->
          let _ = Mvg.VariableMap.find v p.program_conds in
          [
            `Fillcolor cond_color; `Shape `Box; `Style `Filled; `Fontcolor text_color;
            `Label (Printf.sprintf "%s\n%s"
                      (Ast.unmark v.Mvg.Variable.name)
                      (Ast.unmark v.Mvg.Variable.descr)
                   )
          ]
    end
    let vertex_name v = "\"" ^ Ast.unmark v.Mvg.Variable.name ^ "\""
    let default_vertex_attributes _ = []
    let graph_attributes _ = [`Bgcolor 0x00001a]
  end)

module DepgGraphOper = Graph.Oper.P(DepGraph)

let print_dependency_graph (filename: string) (graph: DepGraph.t) (p: Mvg.program): unit =
  let file = open_out_bin filename in
  (* let graph = DepgGraphOper.transitive_reduction graph in *)
  program_when_printing:= Some p;
  Cli.debug_print (Printf.sprintf
                     "Writing variables dependency graph to %s (%d variables)"
                     filename
                     (DepGraph.nb_vertex graph));
  if !Cli.debug_flag then
    Dot.output_graph file graph;
  close_out file

(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)
module SCC = Graph.Components.Make(DepGraph)

(**
   Outputs [true] and a warning in case of cycles.
*)
let check_for_cycle (g: DepGraph.t) (p: Mvg.program) (print_debug: bool) : bool =
  (* if there is a cycle, there will be an strongly connected component of cardinality > 1 *)
  let sccs = SCC.scc_list g in
  if List.length sccs < DepGraph.nb_vertex g then begin
    let sccs = List.filter (fun scc -> List.length scc > 1) sccs in
    let cycles_strings = ref [] in
    let dir = "variable_cycles" in
    begin try Unix.mkdir dir 0o750 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end;
    if !Cli.print_cycles_flag && print_debug then begin
      List.iteri (fun i scc ->
          let new_g = DepGraph.fold_vertex (fun vertex new_g ->
              if List.mem vertex scc then
                new_g
              else
                DepGraph.remove_vertex new_g vertex
            ) g g in
          let filename = Printf.sprintf "%s/strongly_connected_component_%d.dot" dir i in
          print_dependency_graph filename new_g p;
          cycles_strings := (Printf.sprintf "The following variables are defined circularly: %s\n\
                                             The dependency graph of this circular definition has been written to %s"
                               (String.concat " <-> "
                                  (List.map
                                     (fun var -> Ast.unmark var.Mvg.Variable.name)
                                     scc))
                               filename
                            )::!cycles_strings;
        ) sccs;
      let oc = open_out (dir ^ "/variable_cycles.txt") in
      Printf.fprintf oc "%s" (String.concat "\n\n" !cycles_strings);
      close_out oc
    end;
    true
  end else false

module OutputToInputReachability = Graph.Fixpoint.Make(DepGraph)
    (struct
      type vertex = DepGraph.E.vertex
      type edge = DepGraph.E.t
      type g = DepGraph.t
      type data = bool
      let direction = Graph.Fixpoint.Backward
      let equal = (=)
      let join = (||)
      let analyze _ = (fun _ -> true) (* TODO: figure out why x -> x is not working ??? *)
    end)

module Constability = Graph.Fixpoint.Make(DepGraph)
    (struct
      type vertex = DepGraph.E.vertex
      type edge = DepGraph.E.t
      type g = DepGraph.t
      type data = bool
      let direction = Graph.Fixpoint.Backward
      let equal = (=)
      let join = (&&)
      let analyze _ = (fun x -> x)
    end)

module TopologicalOrder = Graph.Topological.Make(DepGraph)
