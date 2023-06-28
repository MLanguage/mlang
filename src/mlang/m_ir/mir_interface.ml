(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr> Raphaël Monat <raphael.monat@lip6.fr>

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

open Mir

let reset_all_outputs (p : program) : program =
  map_vars
    (fun var var_data ->
      match var_data.var_io with
      | Input ->
          {
            var_data with
            var_io = Input;
            var_definition =
              (match var_data.var_definition with
              | InputVar | SimpleVar _ -> var_data.var_definition
              | TableVar _ ->
                  Errors.raise_spanned_error
                    (Format.asprintf
                       "Defining a\n\
                       \             variable input for a table variable %s is \
                        not supported"
                       (Pos.unmark var.Variable.name))
                    (Pos.get_position var.Variable.name));
          }
      | _ ->
          {
            var_data with
            var_io = Regular;
            var_definition =
              (match var_data.var_definition with
              | InputVar -> assert false
              | SimpleVar old -> SimpleVar old
              | TableVar (size, old) -> TableVar (size, old));
          })
    p

type chain_order = {
  dep_graph : Mir_dependency_graph.RG.t;
  execution_order : Mir.rov_id list;
}

type full_program = {
  program : Mir.program;
  domains_orders : chain_order StrSetMap.t;
  chainings_orders : chain_order StrMap.t;
}

let to_full_program (program : program) : full_program =
  let domains_orders =
    StrSetMap.fold
      (fun dom_id _ domains_orders ->
        let vars_to_rules, chain_rules =
          Mir.RuleMap.fold
            (fun rov_id rule (vars, rules) ->
              let rule_domain = rule.rule_domain in
              let is_max = StrSetSet.mem dom_id rule_domain.rdom.dom_max in
              let is_eq = rule_domain.rdom.dom_id = dom_id in
              let is_not_rule_0 = Pos.unmark rule.rule_number <> RuleID 0 in
              if is_not_rule_0 && (is_max || is_eq) then
                ( List.fold_left
                    (fun vars (vid, _def) ->
                      let var = VariableDict.find vid program.program_vars in
                      VariableMap.add var rov_id vars)
                    vars rule.rule_vars,
                  RuleMap.add rov_id rule rules )
              else (vars, rules))
            program.program_rules
            (VariableMap.empty, RuleMap.empty)
        in
        let dep_graph =
          Mir_dependency_graph.create_rules_dependency_graph chain_rules
            vars_to_rules
        in
        let execution_order =
          Mir_dependency_graph.get_rules_execution_order dep_graph
        in
        StrSetMap.add dom_id { dep_graph; execution_order } domains_orders)
      program.program_rule_domains StrSetMap.empty
  in
  let chainings_orders =
    let chainings_roots =
      StrMap.map
        (fun chain_dom ->
          let dep_graph =
            (StrSetMap.find chain_dom.rdom.dom_id domains_orders).dep_graph
          in
          (dep_graph, []))
        program.program_chainings
    in
    let chainings_roots =
      RuleMap.fold
        (fun rov_id rule chainings_roots ->
          match rule.rule_chain with
          | Some (chain_id, _) ->
              let g, rs = StrMap.find chain_id chainings_roots in
              StrMap.add chain_id (g, rov_id :: rs) chainings_roots
          | None -> chainings_roots)
        program.program_rules chainings_roots
    in
    StrMap.fold
      (fun chain_id (dep_graph, chain_roots) chainings_orders ->
        let dep_graph, execution_order =
          Mir_dependency_graph.pull_rules_dependencies dep_graph chain_roots
        in
        StrMap.add chain_id { dep_graph; execution_order } chainings_orders)
      chainings_roots StrMap.empty
  in
  { program; domains_orders; chainings_orders }

let output_var_dependencies (p : full_program) (order : chain_order)
    (var : Mir.variable) =
  let deps =
    Mir_dependency_graph.get_var_dependencies p.program order.execution_order
      var
  in
  List.iter
    (fun (var : variable) ->
      Printf.printf "%s (%s)\n" (Pos.unmark var.name)
        (Option.value ~default:"" var.alias))
    deps
