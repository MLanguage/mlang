(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

open Verifisc_utils

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
let rec add_usages (lvar: Mvg.Variable.t) (e: Mvg.expression Pos.marked) (acc: DepGraph.t) : DepGraph.t =
  let acc = DepGraph.add_vertex acc lvar in
  let add_edge acc var lvar =
    DepGraph.add_edge acc var lvar
  in
  match Pos.unmark e with
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
  (* FIXME: for ocamlgraph to work, output nodes should not have any successors... *)
  let g = DepGraph.fold_vertex (fun (var : Mvg.Variable.t) (g :DepGraph.t) ->
      match (Mvg.VariableMap.find_opt var p.program_vars) with
      | None -> g
      | Some data -> if data.Mvg.var_io = Mvg.Output then
          DepGraph.fold_succ (fun (succ: Mvg.Variable.t) (g: DepGraph.t) -> DepGraph.remove_edge g var succ) g var g
        else g
    ) g g in
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
                        (match v.Mvg.Variable.alias with Some s -> s | None -> Pos.unmark v.Mvg.Variable.name)
                        (Pos.unmark v.Mvg.Variable.descr)
                     )
            ]
          | Mvg.Regular -> [
              `Fillcolor regular_color; `Style `Filled; `Shape `Box; `Fontcolor text_color;
              `Label (Printf.sprintf "%s\n%s"
                        (Pos.unmark v.Mvg.Variable.name)
                        (Pos.unmark v.Mvg.Variable.descr)
                     )
            ]
          | Mvg.Output -> [
              `Fillcolor output_color; `Shape `Box; `Style `Filled; `Fontcolor text_color;
              `Label (Printf.sprintf "%s\n%s"
                        (Pos.unmark v.Mvg.Variable.name)
                        (Pos.unmark v.Mvg.Variable.descr)
                     )
            ]
        with
        | Not_found ->
          let _ = Mvg.VariableMap.find v p.program_conds in
          [
            `Fillcolor cond_color; `Shape `Box; `Style `Filled; `Fontcolor text_color;
            `Label (Printf.sprintf "%s\n%s"
                      (Pos.unmark v.Mvg.Variable.name)
                      (Pos.unmark v.Mvg.Variable.descr)
                   )
          ]
    end
    let vertex_name v = "\"" ^ Pos.unmark v.Mvg.Variable.name ^ "\""
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
                                     (fun var -> Pos.unmark var.Mvg.Variable.name)
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
      let analyze _ = fun x -> x
    end)

module TopologicalOrder = Graph.Topological.Make(DepGraph)
