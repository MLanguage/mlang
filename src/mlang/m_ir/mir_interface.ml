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
  let chains_orders, _ =
    List.fold_left
      (fun (chains, seen_customs) tag ->
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
        let customs, _ =
          RuleMap.fold
            (fun rule_id rule (customs, in_primcorr) ->
              List.fold_left
                (fun (customs, in_primcorr) tag ->
                  match tag with
                  | Mast.Custom _ -> (
                      let ipc =
                        Mast.are_tags_part_of_chain rule.rule_tags Mast.PrimCorr
                      in
                      if in_primcorr && not ipc then
                        Errors.raise_error
                          "Custom chain must be attributed to rules with all \
                           the exact same tagging."
                      else
                        match TagMap.find_opt tag customs with
                        | Some rs ->
                            ( TagMap.add tag (rule_id :: rs) customs,
                              ipc || in_primcorr )
                        | None ->
                            ( TagMap.add tag [ rule_id ] customs,
                              ipc || in_primcorr ))
                  | _ -> (customs, in_primcorr))
                (customs, in_primcorr) rule.rule_tags)
            chain_rules (TagMap.empty, false)
        in
        let customs =
          TagMap.map
            (fun rules ->
              Mir_dependency_graph.pull_rules_dependencies dep_graph rules)
            customs
        in
        let seen_customs =
          TagMap.merge
            (fun custom_tag seen curr ->
              match (seen, curr) with
              | None, None -> None
              | Some _, None -> seen
              | None, Some _ -> Some tag
              | Some s, Some _ -> (
                  match (s, tag) with
                  | Mast.Primitif, Mast.Corrective
                  | Mast.Corrective, Mast.Primitif ->
                      (* ignore this case *) seen
                  | _ ->
                      let custom_tag =
                        match custom_tag with
                        | Mast.Custom s -> s
                        | _ -> assert false
                      in
                      Errors.raise_error
                        (Format.asprintf
                           "Rules with custom chain %s found with incompatible \
                            tags %a and %a."
                           custom_tag Format_mast.format_chain_tag s
                           Format_mast.format_chain_tag tag)))
            seen_customs customs
        in
        let chains =
          TagMap.fold
            (fun tag (dep_graph, execution_order) chains ->
              TagMap.add tag { dep_graph; execution_order } chains)
            customs
            (Mir.TagMap.add tag { dep_graph; execution_order } chains)
        in
        (chains, seen_customs))
      (Mir.TagMap.empty, Mir.TagMap.empty)
      chains
  in
  { program; chains_orders }

let output_var_dependencies (p : full_program) (chain : Mast.chain_tag)
    (var : Mir.variable) =
  let chain = TagMap.find chain p.chains_orders in
  let deps =
    Mir_dependency_graph.get_var_dependencies p.program chain.execution_order
      var
  in
  List.iter
    (fun (var : variable) ->
      Printf.printf "%s (%s)\n" (Pos.unmark var.name)
        (Option.value ~default:"" var.alias))
    deps
