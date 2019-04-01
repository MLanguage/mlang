(*
  Copyright 2018 Denis Merigoux and INRIA

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

module DepGraph = Graph.Persistent.Graph.Concrete(struct
    type t = Cfg.Variable.t
    let hash v = v.Cfg.Variable.id
    let compare v1 v2 = compare v1.Cfg.Variable.id v2.Cfg.Variable.id
    let equal v1 v2 = v1.Cfg.Variable.id = v2.Cfg.Variable.id
  end)

let rec add_usages (lvar: Cfg.Variable.t) (e: Cfg.expression Ast.marked) (acc: DepGraph.t) : DepGraph.t =
  match Ast.unmark e with
  | Cfg.Comparison (_, e1, e2) | Cfg.Binop (_, e1, e2 )
  | Cfg.LocalLet (_, e1, e2) ->
    let acc = add_usages lvar e1 acc in
    let acc = add_usages lvar e2 acc in
    acc
  | Cfg.Unop (_, e) ->
    add_usages lvar e acc
  | Cfg.Index ((var,_ ), e) ->
    let acc = DepGraph.add_edge acc lvar var in
    let acc = add_usages lvar e acc in
    acc
  | Cfg.Conditional (e1, e2, e3) ->
    let acc = add_usages lvar e1 acc in
    let acc = add_usages lvar e2 acc in
    let acc = add_usages lvar e3 acc in
    acc
  | Cfg.FunctionCall (_, args) ->
    List.fold_left (fun acc arg ->
        add_usages lvar arg acc) acc args
  | Cfg.LocalVar _
  | Cfg.Literal _
  | Cfg.GenericTableIndex
  | Cfg.Error -> acc
  | Cfg.Var var -> DepGraph.add_edge acc lvar var


let create_dependency_graph (p: Cfg.program) : DepGraph.t =
  Cfg.VariableMap.fold (fun var def acc ->
      match def.Cfg.var_definition with
      | Cfg.InputVar -> acc
      | Cfg.SimpleVar e -> add_usages var e acc
      | Cfg.TableVar (_, def) -> begin match def with
          | Cfg.IndexGeneric e -> add_usages var e acc
          | Cfg.IndexTable es -> Cfg.IndexMap.fold (fun _ e acc ->
              add_usages var e acc
            ) es acc
        end
    ) p DepGraph.empty


module Dot = Graph.Graphviz.Dot(struct
    include DepGraph (* use the graph module from above *)

    let edge_attributes _ = [`Color 4711]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes _ = [`Shape `Box]
    let vertex_name v = Ast.unmark v.Cfg.Variable.name
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let print_dependency_graph (filename: string) (graph: DepGraph.t): unit =
  let file = open_out_bin filename in
  Cli.debug_print (Printf.sprintf "Writing variables dependency graph to %s" filename);
  if !Cli.debug_flag then
    Dot.output_graph file graph;
  close_out file
