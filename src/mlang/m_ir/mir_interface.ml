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
  execution_order : Mir.rule_id list;
}

type full_program = {
  program : Mir.program;
  chains_orders : chain_order Mir.TagMap.t;
}

let to_full_program (program : program) (chains : Mast.chain_tag list) :
    full_program =
  let chains_orders =
    List.fold_left
      (fun chains tag ->
        let vars_to_rules, chain_rules =
          Mir.RuleMap.fold
            (fun rule_id rule (vars, rules) ->
              if Mast.are_tags_part_of_chain rule.rule_tags tag then
                ( List.fold_left
                    (fun vars (vid, _def) ->
                      let var = VariableDict.find vid program.program_vars in
                      VariableMap.add var rule_id vars)
                    vars rule.rule_vars,
                  RuleMap.add rule_id rule rules )
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
        Mir.TagMap.add tag { dep_graph; execution_order } chains)
      Mir.TagMap.empty chains
  in
  { program; chains_orders }
