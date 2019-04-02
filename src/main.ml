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

open Lexer
open Lexing
open Cli

let parse_cli_args () =
  (** Code block to retrieve and parse command-line arguments. *)
  let speclist = Arg.align [
      ("--verify", Arg.Set verify_flag,
       " Vérifie que les conditions sont valables dans tous les cas");
      ("--debug", Arg.Set debug_flag,
       " Affiche des informations de débuggage");
      ("--dep_graph_file", Arg.Set_string dep_graph_file,
       " Fichier où écrire le graphe de dépendance (par défault dep_graph.dot)")
    ]
  in let usage_msg =
       "M parser"
  in
  let anon_func (file: string) : unit =
    source_files := file::!source_files
  in Arg.parse speclist anon_func usage_msg

let main () =
  parse_cli_args ();
  Cli.debug_print "Reading files...";
  let program = ref [] in
  List.iter (fun source_file ->
      let (filebuf, input) = if source_file <> "" then
          let input = open_in source_file in
          (Lexing.from_channel input, Some input)
        else if source_file <> "" then
          (Lexing.from_string source_file, None)
        else
          failwith "Il faut spécifier un fichier !"
      in
      Cli.debug_print (Printf.sprintf "Parsing %s" source_file);
      let filebuf = {filebuf with
                     lex_curr_p = { filebuf.lex_curr_p with
                                    pos_fname = Filename.basename source_file
                                  }
                    }
      in
      try
        Parse_utils.current_file := source_file;
        let commands = Parser.source_file token filebuf in
        program := commands::!program
      with
      | Errors.LexingError msg | Errors.ParsingError msg ->
        error_print msg
      | Parser.Error -> begin
          error_print
            (Printf.sprintf "Lexer error in file %s at position %s"
               (!Parse_utils.current_file)
               (Errors.print_lexer_position filebuf.lex_curr_p));
          begin match input with
            | Some input -> close_in input
            | None -> ()
          end;
          exit (-1)
        end
    ) !source_files;
  try
    let program = Ast_to_cfg.translate !program in
    Cli.debug_print "Typechecking...";
    let typing_info = Typechecker.typecheck program in
    ignore (typing_info);
    Cli.debug_print "Analysing dependencies...";
    let dep_graph = Dependency.create_dependency_graph program in
    Dependency.print_dependency_graph (!dep_graph_file ^ "_before_optimization.dot")  dep_graph;
    Dependency.check_for_cycle dep_graph;
    Cli.debug_print (Printf.sprintf "Optimizing program with %d variables..." (Cfg.VariableMap.cardinal program));
    let unused_variables = Dependency.get_unused_variables dep_graph program in
    Cli.debug_print (Printf.sprintf "Removing %d unused variables..." (Cfg.VariableMap.cardinal unused_variables));
    let program = Cfg.VariableMap.filter (fun var _ -> not (Cfg.VariableMap.mem var unused_variables)) program in
    let program : Cfg.program ref = ref program in
    let nb_inlined_vars : int ref = ref max_int in
    while (0 < !nb_inlined_vars) do
      let dep_graph = Dependency.create_dependency_graph !program in
      let single_use_vars = Dependency.single_use_vars dep_graph in
      let to_inline_vars = Cfg.VariableMap.filter (fun var _ -> match (Cfg.VariableMap.find var !program).Cfg.var_io with
          | Cfg.Input | Cfg.Output -> false
          | Cfg.Regular -> true
        ) single_use_vars in
      nb_inlined_vars := Cfg.VariableMap.cardinal to_inline_vars;
      if !nb_inlined_vars > 0 then begin
        Cli.debug_print (Printf.sprintf "Inlining %d variables..." !nb_inlined_vars);
        let new_program = Inlining.inline_vars to_inline_vars !program in
        program := new_program;
      end
    done;
    let program = !program in
    Cli.debug_print
      (Printf.sprintf "Program variables count down to %d!"
         (Cfg.VariableMap.cardinal program));
    let dep_graph = Dependency.create_dependency_graph program in
    Dependency.print_dependency_graph (!dep_graph_file ^ "_after_optimization.dot") dep_graph;
    ignore program
  with
  | Errors.TypeError e ->
    error_print (Errors.format_typ_error e); exit 1
  | Errors.Unimplemented (msg,pos) ->
    error_print (Printf.sprintf "unimplemented for expression %s (code %s)" (Format_ast.format_position pos) msg)

let _ = main ()
