(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
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

module RG =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = Mir.rov_id

      let hash v = Mir.num_of_rule_or_verif_id v (* no verif here anyway *)

      let compare = compare

      let equal = ( = )
    end)
    (struct
      type t = string

      let compare = String.compare

      let default = ""
    end)

(** Add all the sucessors of [lvar] in the graph that are used by [e] *)
let rec get_used_variables_ (e : Mir.expression Pos.marked)
    (acc : Mir.VariableDict.t) : Mir.VariableDict.t =
  match Pos.unmark e with
  | Mir.Comparison (_, e1, e2) | Mir.Binop (_, e1, e2) | Mir.LocalLet (_, e1, e2)
    ->
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
  | Mir.LocalVar _ | Mir.Literal _ | Mir.Error -> acc
  | Mir.Var var -> Mir.VariableDict.add var acc

let get_used_variables (e : Mir.expression Pos.marked) : Mir.VariableDict.t =
  get_used_variables_ e Mir.VariableDict.empty

let get_def_used_variables (def : Mir.variable_def) : Mir.VariableDict.t =
  match def with
  | Mir.InputVar -> Mir.VariableDict.empty
  | Mir.SimpleVar e -> get_used_variables e
  | Mir.TableVar (_, def) -> (
      match def with
      | Mir.IndexGeneric (v, e) -> Mir.VariableDict.add v (get_used_variables e)
      | Mir.IndexTable es ->
          Mir.IndexMap.fold
            (fun _ e acc -> Mir.VariableDict.union acc (get_used_variables e))
            es Mir.VariableDict.empty)

let create_rules_dependency_graph (chain_rules : Mir.rule_data Mir.RuleMap.t)
    (vars_to_rules : Mir.rov_id Mir.VariableMap.t) : RG.t =
  Mir.RuleMap.fold
    (fun rule_id { Mir.rule_vars; _ } g ->
      let g = RG.add_vertex g rule_id in
      List.fold_left
        (fun g (_vid, def) ->
          let succs = get_def_used_variables def.Mir.var_definition in
          let var_deps =
            Mir.VariableDict.fold
              (fun succ var_deps ->
                try
                  let rsucc = Mir.VariableMap.find succ vars_to_rules in
                  let vsuccs =
                    try Mir.RuleMap.find rsucc var_deps
                    with Not_found -> Mir.VariableDict.empty
                  in
                  Mir.RuleMap.add rsucc
                    (Mir.VariableDict.add succ vsuccs)
                    var_deps
                with Not_found -> var_deps)
              succs Mir.RuleMap.empty
          in
          Mir.RuleMap.fold
            (fun succ vsuccs g ->
              try
                if rule_id = succ then g
                else
                  let label =
                    Format.fprintf Format.str_formatter "  %a"
                      (Format.pp_print_list
                         ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n  ")
                         Format_mir.format_variable)
                      (Mir.VariableDict.bindings vsuccs |> List.map snd);
                    Format.flush_str_formatter ()
                  in
                  let edge = RG.E.create rule_id label succ in
                  RG.add_edge_e g edge
              with Not_found -> g)
            var_deps g)
        g rule_vars)
    chain_rules RG.empty

module VG = Graph.Persistent.Digraph.ConcreteBidirectional (struct
  type t = Mir.variable_id

  let hash v = v

  let compare = compare

  let equal = ( = )
end)

module SCC = Graph.Components.Make (RG)
(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)

let check_for_cycle (g : RG.t) (p : Mir.program) (print_debug : bool) : bool =
  (* Find a cycle within a list of strongly connected components. Depth first
     traversal *)
  let cycle_within g vtx =
    let exception Found of (RG.V.t * RG.E.t) list in
    (* Find an already passed vertex. Return all vertexes passed from that
       point *)
    let find_seen_suffix v seen =
      let rec aux seen suffix =
        match seen with
        | [] -> None
        | (v', e) :: _ when v = v' -> Some ((v', e) :: suffix)
        | s :: seen -> aux seen (s :: suffix)
      in
      aux seen []
    in
    (* DFS. Never returns, raise an exception on cycle find *)
    let rec aux seen v =
      match find_seen_suffix v seen with
      | Some seen -> raise (Found seen)
      | None ->
          let succs = RG.succ g v |> List.filter (fun x -> List.mem x vtx) in
          List.iter
            (fun succ ->
              let seen = (v, RG.find_edge g v succ) :: seen in
              aux seen succ)
            succs
    in
    try
      aux [] (List.hd vtx);
      assert false
    with Found cycle -> cycle
  in
  (* if there is a cycle, there will be an strongly connected component of
     cardinality > 1 *)
  let sccs = SCC.scc_list g in
  if List.length sccs < RG.nb_vertex g then begin
    let sccs = List.filter (fun scc -> List.length scc > 1) sccs in
    let cycles_strings = ref [] in
    if (not !Cli.no_print_cycles_flag) && print_debug then begin
      List.iter
        (fun scc ->
          let edges = cycle_within g scc in
          cycles_strings :=
            Format.asprintf
              "The following rules contain circular definitions:\n%s\n"
              (String.concat "\n"
                 (let rule =
                    Mir.RuleMap.find (List.hd edges |> fst) p.program_rules
                  in
                  string_of_int
                    (Mir.num_of_rule_or_verif_id (Pos.unmark rule.rule_number))
                  :: List.map
                       (fun (rule_id, edge) ->
                         let rule = Mir.RuleMap.find rule_id p.program_rules in
                         Format.asprintf "depends on %d through vars: {%s}"
                           (Mir.num_of_rule_or_verif_id
                              (Pos.unmark rule.rule_number))
                           (RG.E.label edge))
                       edges))
            :: !cycles_strings)
        sccs;
      Format.eprintf "%s" (String.concat "\n\n" !cycles_strings)
    end;
    true
  end
  else false

type rule_execution_order = Mir.rov_id list

module Traversal = Graph.Traverse.Dfs (RG)

let pull_rules_dependencies (g : RG.t) (rules : Mir.rov_id list) :
    RG.t * rule_execution_order =
  let order =
    List.fold_left
      (fun exec_order rule_id ->
        let new_deps =
          Traversal.fold_component
            (fun dep new_deps ->
              if List.mem dep exec_order then
                (* We already added this dependency from another rule, and thus
                   all its own dependencies *)
                new_deps
              else dep :: new_deps)
            exec_order g rule_id
        in
        (* Here's a subtlety, we don't now in which order the rules given are,
           only that their should not be any cycles. So we can have three cases:
           The current rule we pull dependencies of is unrelated to previous
           ones, this is trivialy correct. Or, the current rule is already a
           dependency of previous ones, then nothing will be added, as seen
           above. Or finally, the current rule has dependencies in common with
           previous rules, in which case we know them to be already added, so to
           be topologically ordered we have to put the new dependencies at the
           end, it is safe since there is no cycles. *)
        exec_order @ new_deps)
      [] rules
  in
  let subgraph =
    RG.fold_vertex
      (fun rule_id subg ->
        if List.mem rule_id order then subg else RG.remove_vertex subg rule_id)
      g g
  in
  (subgraph, order)

let create_vars_dependency_graph (p : Mir.program)
    (chain_rules : rule_execution_order) : VG.t =
  List.fold_left
    (fun g rule_id ->
      let rule = Mir.RuleMap.find rule_id p.program_rules in
      List.fold_left
        (fun g (vid, vdef) ->
          let deps = get_def_used_variables vdef.Mir.var_definition in
          Mir.VariableDict.fold
            (fun var g -> VG.add_edge g vid var.Mir.id)
            deps g)
        g rule.rule_vars)
    VG.empty chain_rules

let get_var_dependencies ?(strict = true) (p : Mir.program)
    (rules : rule_execution_order) (var : Mir.variable) : Mir.variable list =
  let depg = create_vars_dependency_graph p rules in
  let vid = var.Mir.id in
  if not (VG.mem_vertex depg var.Mir.id) then
    Errors.raise_error "This variable is not defined in this chain.";
  let module Traversal = Graph.Traverse.Dfs (VG) in
  Traversal.fold_component
    (fun did deps ->
      if (not strict) || VG.out_degree depg did = 0 then
        let var = Mir.VariableDict.find did p.program_vars in
        var :: deps
      else deps)
    [] depg vid

module RuleExecutionOrder = Graph.Topological.Make (RG)

let get_rules_execution_order (dep_graph : RG.t) : rule_execution_order =
  RuleExecutionOrder.fold (fun var exec_order -> var :: exec_order) dep_graph []
