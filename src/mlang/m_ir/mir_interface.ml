(* Copyright (C) 2019 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr> Raphël Monat
   <raphael.monat@lip6.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Mir

let reset_all_outputs (p : program) : program =
  {
    p with
    program_vars =
      VariableMap.mapi
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
                           \             variable input for a table variable %s is not supported"
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
        p.program_vars;
  }

type full_program = {
  dep_graph : Mir_dependency_graph.G.t;
  main_execution_order : Mir.rule_id list;
  rules_execution_order : Mir_dependency_graph.execution_order Mir.RuleMap.t;
  vars_execution_order : Mir_dependency_graph.execution_order;
  program : Mir.program;
}

let to_full_program (program : program) : full_program =
  let dep_graph = Mir_dependency_graph.create_program_dependency_graph program in
  let vars_to_rules, rules_execution_order =
    Mir.RuleMap.fold
      (fun rule_id { rule_vars; _ } (conv, orders) ->
        let conv =
          List.fold_left (fun conv var -> VariableMap.add var rule_id conv) conv rule_vars
        in
        let orders =
          let vars =
            List.fold_left
              (fun vars var -> VariableMap.add var (VariableMap.find var program.program_vars) vars)
              VariableMap.empty rule_vars
          in
          let order =
            Mir_dependency_graph.create_vars_dependency_graph vars
            |> Mir_dependency_graph.get_execution_order
          in
          Mir.RuleMap.add rule_id order orders
        in
        (conv, orders))
      program.program_rules
      (VariableMap.empty, Mir.RuleMap.empty)
  in
  let main_execution_order =
    Mir_dependency_graph.create_rules_dependency_graph program vars_to_rules dep_graph
    |> Mir_dependency_graph.get_rules_execution_order
  in
  let vars_execution_order =
    List.concat_map
      (fun rule_id -> Mir.RuleMap.find rule_id rules_execution_order)
      main_execution_order
  in
  { program; dep_graph; main_execution_order; rules_execution_order; vars_execution_order }
