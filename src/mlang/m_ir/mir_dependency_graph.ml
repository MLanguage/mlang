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
  | Mir.LocalVar _ | Mir.Literal _ | Mir.Error | Mir.NbCategory _
  | Mir.Attribut _ | Mir.Size _ | Mir.NbAnomalies | Mir.NbDiscordances
  | Mir.NbInformatives | Mir.NbBloquantes ->
      acc
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

module VG = Graph.Persistent.Digraph.ConcreteBidirectional (struct
  type t = Mir.variable_id

  let hash v = v

  let compare = compare

  let equal = ( = )
end)

type rule_execution_order = Mir.rov_id list

let create_vars_dependency_graph (_p : Mir.program)
    (chain_rules : rule_execution_order) : VG.t =
  List.fold_left
    (fun g rule_id ->
      let rule = Mir.RuleMap.find rule_id Mir.RuleMap.empty in
      List.fold_left
        (fun g instr ->
          match Pos.unmark instr with
          | Mir.Affectation (vid, vdef) ->
              let deps = get_def_used_variables vdef.Mir.var_definition in
              Mir.VariableDict.fold
                (fun var g -> VG.add_edge g vid var.Mir.id)
                deps g
          | _ -> assert false
          (* never used *))
        g rule.Mir.rule_vars)
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
