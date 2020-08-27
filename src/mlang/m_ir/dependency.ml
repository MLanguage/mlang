(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** Defines the dependency graph of an M program *)

(** Each node corresponds to a variable, each edge to a variable use. The edges in the graph go from
    output to inputs. *)
module DepGraph = Graph.Persistent.Digraph.ConcreteBidirectional (struct
  type t = Mir.Variable.t

  let hash v = v.Mir.Variable.id

  let compare v1 v2 = compare v1.Mir.Variable.id v2.Mir.Variable.id

  let equal v1 v2 = v1.Mir.Variable.id = v2.Mir.Variable.id
end)

(** Add all the sucessors of [lvar] in the graph that are used by [e] *)
let rec get_used_variables (e : Mir.expression Pos.marked) (acc : unit Mir.VariableMap.t) :
    unit Mir.VariableMap.t =
  match Pos.unmark e with
  | Mir.Comparison (_, e1, e2) | Mir.Binop (_, e1, e2) | Mir.LocalLet (_, e1, e2) ->
      let acc = get_used_variables e1 acc in
      let acc = get_used_variables e2 acc in
      acc
  | Mir.Unop (_, e) -> get_used_variables e acc
  | Mir.Index ((var, _), e) ->
      let acc = Mir.VariableMap.add var () acc in
      let acc = get_used_variables e acc in
      acc
  | Mir.Conditional (e1, e2, e3) ->
      let acc = get_used_variables e1 acc in
      let acc = get_used_variables e2 acc in
      let acc = get_used_variables e3 acc in
      acc
  | Mir.FunctionCall (_, args) ->
      List.fold_left (fun acc arg -> get_used_variables arg acc) acc args
  | Mir.LocalVar _ | Mir.Literal _ | Mir.GenericTableIndex | Mir.Error -> acc
  | Mir.Var var -> Mir.VariableMap.add var () acc

let add_usages (lvar : Mir.Variable.t) (e : Mir.expression Pos.marked) (acc : DepGraph.t) :
    DepGraph.t =
  let acc = DepGraph.add_vertex acc lvar in
  let add_edge acc var lvar = DepGraph.add_edge acc var lvar in
  let usages = get_used_variables e Mir.VariableMap.empty in
  Mir.VariableMap.fold (fun var _ acc -> add_edge acc var lvar) usages acc

(** The dependency graph also includes nodes for the conditions to be checked at execution *)
let create_dependency_graph (p : Mir.program) : DepGraph.t =
  let g =
    Mir.VariableMap.fold
      (fun var def acc ->
        match def.Mir.var_definition with
        | Mir.InputVar -> DepGraph.add_vertex acc var
        | Mir.SimpleVar e -> add_usages var e acc
        | Mir.TableVar (_, def) -> (
            match def with
            | Mir.IndexGeneric e -> add_usages var e acc
            | Mir.IndexTable es -> Mir.IndexMap.fold (fun _ e acc -> add_usages var e acc) es acc ))
      p.program_vars DepGraph.empty
  in
  (* FIXME: for ocamlgraph to work, output nodes should not have any successors... *)
  let g =
    DepGraph.fold_vertex
      (fun (var : Mir.Variable.t) (g : DepGraph.t) ->
        match Mir.VariableMap.find_opt var p.program_vars with
        | None -> g
        | Some data ->
            if data.Mir.var_io = Mir.Output then
              DepGraph.fold_succ
                (fun (succ : Mir.Variable.t) (g : DepGraph.t) -> DepGraph.remove_edge g var succ)
                g var g
            else g)
      g g
  in
  Mir.VariableMap.fold
    (fun cond_var cond acc -> add_usages cond_var cond.Mir.cond_expr acc)
    p.program_conds g

let program_when_printing : Mir.program option ref = ref None

(** The graph is output in the Dot format *)
module Dot = Graph.Graphviz.Dot (struct
  include DepGraph (* use the graph module from above *)

  let edge_attributes _ = [ `Color 0xffa366 ]

  let default_edge_attributes _ = []

  let get_subgraph _ = None

  let vertex_attributes v =
    match !program_when_printing with
    | None -> []
    | Some p -> (
        let input_color = 0x66b5ff in
        let output_color = 0xE6E600 in
        let cond_color = 0x666633 in
        let regular_color = 0x8585ad in
        let text_color = 0xf2f2f2 in
        try
          let var_data = Mir.VariableMap.find v p.program_vars in
          match var_data.Mir.var_io with
          | Mir.Input ->
              [
                `Fillcolor input_color;
                `Shape `Box;
                `Style `Filled;
                `Fontcolor text_color;
                `Label
                  (Format.asprintf "%s\n%s"
                     ( match v.Mir.Variable.alias with
                     | Some s -> s
                     | None -> Pos.unmark v.Mir.Variable.name )
                     (Pos.unmark v.Mir.Variable.descr));
              ]
          | Mir.Regular ->
              [
                `Fillcolor regular_color;
                `Style `Filled;
                `Shape `Box;
                `Fontcolor text_color;
                `Label
                  (Format.asprintf "%s\n%s" (Pos.unmark v.Mir.Variable.name)
                     (Pos.unmark v.Mir.Variable.descr));
              ]
          | Mir.Output ->
              [
                `Fillcolor output_color;
                `Shape `Box;
                `Style `Filled;
                `Fontcolor text_color;
                `Label
                  (Format.asprintf "%s\n%s" (Pos.unmark v.Mir.Variable.name)
                     (Pos.unmark v.Mir.Variable.descr));
              ]
        with Not_found ->
          let _ = Mir.VariableMap.find v p.program_conds in
          [
            `Fillcolor cond_color;
            `Shape `Box;
            `Style `Filled;
            `Fontcolor text_color;
            `Label
              (Format.asprintf "%s\n%s" (Pos.unmark v.Mir.Variable.name)
                 (Pos.unmark v.Mir.Variable.descr));
          ] )

  let vertex_name v = "\"" ^ Pos.unmark v.Mir.Variable.name ^ "\""

  let default_vertex_attributes _ = []

  let graph_attributes _ = [ `Bgcolor 0x00001a ]
end)

module DepgGraphOper = Graph.Oper.P (DepGraph)

let print_dependency_graph (filename : string) (graph : DepGraph.t) (p : Mir.program) : unit =
  let file = open_out_bin filename in
  (* let graph = DepgGraphOper.transitive_reduction graph in *)
  program_when_printing := Some p;
  Cli.debug_print "Writing variables dependency graph to %s (%d variables)" filename
    (DepGraph.nb_vertex graph);
  if !Cli.debug_flag then Dot.output_graph file graph;
  close_out file

module SCC = Graph.Components.Make (DepGraph)
(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)

(** Outputs [true] and a warning in case of cycles. *)
let check_for_cycle (g : DepGraph.t) (p : Mir.program) (print_debug : bool) : bool =
  (* if there is a cycle, there will be an strongly connected component of cardinality > 1 *)
  let sccs = SCC.scc_list g in
  if List.length sccs < DepGraph.nb_vertex g then begin
    let sccs = List.filter (fun scc -> List.length scc > 1) sccs in
    let cycles_strings = ref [] in
    let dir = "variable_cycles" in
    begin
      try Unix.mkdir dir 0o750 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end;
    if !Cli.print_cycles_flag && print_debug then begin
      List.iteri
        (fun i scc ->
          let new_g =
            DepGraph.fold_vertex
              (fun vertex new_g ->
                if List.mem vertex scc then new_g else DepGraph.remove_vertex new_g vertex)
              g g
          in
          let filename = Format.asprintf "%s/strongly_connected_component_%d.dot" dir i in
          print_dependency_graph filename new_g p;
          cycles_strings :=
            Format.asprintf
              "The following variables are defined circularly: %s\n\
               The dependency graph of this circular definition has been written to %s"
              (String.concat " <-> " (List.map (fun var -> Pos.unmark var.Mir.Variable.name) scc))
              filename
            :: !cycles_strings)
        sccs;
      let oc = open_out (dir ^ "/variable_cycles.txt") in
      Format.fprintf
        (Format.formatter_of_out_channel oc)
        "%s"
        (String.concat "\n\n" !cycles_strings);
      close_out oc
    end;
    true
  end
  else false

module OutputToInputReachability =
  Graph.Fixpoint.Make
    (DepGraph)
    (struct
      type vertex = DepGraph.E.vertex

      type edge = DepGraph.E.t

      type g = DepGraph.t

      type data = bool

      let direction = Graph.Fixpoint.Backward

      let equal = ( = )

      let join = ( || )

      let analyze _ x = x
    end)

module TopologicalOrder = Graph.Topological.Make (DepGraph)
