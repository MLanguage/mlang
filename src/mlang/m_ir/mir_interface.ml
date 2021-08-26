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
    p

type full_program = {
  dep_graph : Mir_dependency_graph.RG.t;
  main_execution_order : Mir.rule_id list;
  vars_execution_order : Mir_dependency_graph.execution_order;
  program : Mir.program;
}

let to_full_program (program : program) : full_program =
  let vars_to_rules, rules_execution_order =
    Mir.RuleMap.fold
      (fun rule_id { rule_vars; _ } (conv, orders) ->
        let conv, order =
          List.fold_left
            (fun (conv, order) (vid, _def) ->
              let var = VariableDict.find vid program.program_vars in
              let conv = VariableMap.add var rule_id conv in
              (conv, var :: order))
            (conv, []) rule_vars
        in
        let order = List.rev order in
        let orders = Mir.RuleMap.add rule_id order orders in
        (conv, orders))
      program.program_rules
      (VariableMap.empty, Mir.RuleMap.empty)
  in
  let dep_graph = Mir_dependency_graph.create_rules_dependency_graph program vars_to_rules in
  let main_execution_order = Mir_dependency_graph.get_rules_execution_order dep_graph in
  let vars_execution_order =
    List.concat_map
      (fun rule_id -> Mir.RuleMap.find rule_id rules_execution_order)
      main_execution_order
  in
  { program; dep_graph; main_execution_order; vars_execution_order }
