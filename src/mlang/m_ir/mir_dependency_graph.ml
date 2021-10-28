(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

module RG =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = Mir.rule_id

      let hash v = v

      let compare = compare

      let equal = ( = )
    end)
    (struct
      type t = string

      let compare = String.compare

      let default = ""
    end)

(** Add all the sucessors of [lvar] in the graph that are used by [e] *)
let rec get_used_variables_ (e : Mir.expression Pos.marked) (acc : Mir.VariableDict.t) :
    Mir.VariableDict.t =
  match Pos.unmark e with
  | Mir.Comparison (_, e1, e2) | Mir.Binop (_, e1, e2) | Mir.LocalLet (_, e1, e2) ->
      let acc = get_used_variables_ e1 acc in
      let acc = get_used_variables_ e2 acc in
      acc
  | Mir.Unop (_, e) -> get_used_variables_ e acc
  | Mir.Index ((var, _), e) ->
      let acc = Mir.VariableDict.add var acc in
      let acc = get_used_variables_ e acc in
      acc
  | Mir.Conditional (e1, e2, e3) ->
      let acc = get_used_variables_ e1 acc in
      let acc = get_used_variables_ e2 acc in
      let acc = get_used_variables_ e3 acc in
      acc
  | Mir.FunctionCall (_, args) ->
      List.fold_left (fun acc arg -> get_used_variables_ arg acc) acc args
  | Mir.LocalVar _ | Mir.Literal _ | Mir.GenericTableIndex | Mir.Error -> acc
  | Mir.Var var -> Mir.VariableDict.add var acc

let get_used_variables (e : Mir.expression Pos.marked) : Mir.VariableDict.t =
  get_used_variables_ e Mir.VariableDict.empty

let get_def_used_variables (def : Mir.variable_def) : Mir.VariableDict.t =
  match def with
  | Mir.InputVar -> Mir.VariableDict.empty
  | Mir.SimpleVar e -> get_used_variables e
  | Mir.TableVar (_, def) -> (
      match def with
      | Mir.IndexGeneric e -> get_used_variables e
      | Mir.IndexTable es ->
          Mir.IndexMap.fold
            (fun _ e acc -> Mir.VariableDict.union acc (get_used_variables e))
            es Mir.VariableDict.empty)

let create_rules_dependency_graph (program : Mir.program)
    (vars_to_rules : Mir.rule_id Mir.VariableMap.t) : RG.t =
  Mir.RuleMap.fold
    (fun rule_id { Mir.rule_vars; _ } g ->
      List.fold_left
        (fun g (_vid, def) ->
          let succs = get_def_used_variables def.Mir.var_definition in
          let var_deps =
            Mir.VariableDict.fold
              (fun succ var_deps ->
                let rsucc = Mir.VariableMap.find succ vars_to_rules in
                let vsuccs =
                  try Mir.RuleMap.find rsucc var_deps with Not_found -> Mir.VariableDict.empty
                in
                Mir.RuleMap.add rsucc (Mir.VariableDict.add succ vsuccs) var_deps)
              succs Mir.RuleMap.empty
          in
          Mir.RuleMap.fold
            (fun succ vsuccs g ->
              try
                let label =
                  Format.fprintf Format.str_formatter "%a"
                    (Format.pp_print_list ~pp_sep:Format.pp_print_space Format_mir.format_variable)
                    (Mir.VariableDict.bindings vsuccs |> List.map snd);
                  Format.flush_str_formatter ()
                in
                let edge = RG.E.create rule_id label succ in
                RG.add_edge_e g edge
              with Not_found -> g)
            var_deps g)
        g rule_vars)
    program.program_rules RG.empty

module SCC = Graph.Components.Make (RG)
(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)

let check_for_cycle (g : RG.t) (p : Mir.program) (print_debug : bool) : bool =
  (* if there is a cycle, there will be an strongly connected component of cardinality > 1 *)
  let sccs = SCC.scc_list g in
  if List.length sccs < RG.nb_vertex g then begin
    let sccs = List.filter (fun scc -> List.length scc > 1) sccs in
    let cycles_strings = ref [] in
    if (not !Cli.no_print_cycles_flag) && print_debug then begin
      List.iter
        (fun scc ->
          let edges =
            let loop = scc @ [ List.hd scc ] in
            let rec get_edges acc = function
              | [] | [ _ ] -> acc
              | v1 :: v2 :: vtx -> get_edges ((v1, RG.find_edge g v1 v2) :: acc) (v2 :: vtx)
            in
            get_edges [] loop |> List.rev
          in
          cycles_strings :=
            Format.asprintf "The following rules contain circular definitions:\n%s\n"
              (String.concat "\n^\n|\nv\n"
                 (List.map
                    (fun (rule_id, edge) ->
                      let rule = Mir.RuleMap.find rule_id p.Mir.program_rules in
                      Format.asprintf "%d with vars: %s" (Pos.unmark rule.rule_number)
                        (RG.E.label edge))
                    edges))
            :: !cycles_strings)
        sccs;
      Format.eprintf "%s" (String.concat "\n\n" !cycles_strings)
    end;
    true
  end
  else false

module RuleExecutionOrder = Graph.Topological.Make (RG)

type rule_execution_order = Mir.rule_id list

let get_rules_execution_order (dep_graph : RG.t) : rule_execution_order =
  RuleExecutionOrder.fold (fun var exec_order -> var :: exec_order) dep_graph []
