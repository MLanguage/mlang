(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Lexing
open Lexer

(** Entry function for the executable. Returns a negative number in case of error. *)
let driver (files : string list) (application : string) (debug : bool) (display_time : bool)
    (dep_graph_file : string) (print_cycles : bool) (optimize : bool) (backend : string)
    (function_spec : string) (output : string option) (real_precision : int)
    (run_all_tests : string option) (run_test : string option) (year : int) =
  Cli.set_all_arg_refs files application debug display_time dep_graph_file print_cycles optimize
    backend function_spec output real_precision run_all_tests run_test year;
  try
    Cli.debug_print "Reading files...";
    let program = ref [] in
    if List.length !Cli.source_files = 0 then
      raise (Errors.ArgumentError "please provide at least one M source file");
    let current_progress, finish = Cli.create_progress_bar "Parsing" in
    List.iter
      (fun source_file ->
        let filebuf, input =
          if source_file <> "" then
            let input = open_in source_file in
            (Lexing.from_channel input, Some input)
          else if source_file <> "" then (Lexing.from_string source_file, None)
          else failwith "You have to specify at least one file!"
        in
        current_progress source_file;
        let filebuf =
          {
            filebuf with
            lex_curr_p = { filebuf.lex_curr_p with pos_fname = Filename.basename source_file };
          }
        in
        try
          Parse_utils.current_file := source_file;
          let commands = Parser.source_file token filebuf in
          program := commands :: !program
        with
        | Errors.LexingError msg | Errors.ParsingError msg -> Cli.error_print "%s" msg
        | Parser.Error ->
            Cli.error_print "Lexer error in file %s at position %a\n" !Parse_utils.current_file
              Errors.print_lexer_position filebuf.lex_curr_p;
            begin
              match input with
              | Some input -> close_in input
              | None -> ()
            end;
            Cmdliner.Term.exit_status (`Ok 2))
      !Cli.source_files;
    finish "completed!";
    let application = if !Cli.application = "" then None else Some !Cli.application in
    let program = Ast_to_mvg.translate !program application in
    Cli.debug_print "Expanding function definitions...";
    let program = Functions.expand_functions program in
    Cli.debug_print "Typechecking...";
    let program = Typechecker.typecheck program in
    Cli.debug_print "Checking for circular variable definitions...";
    let dep_graph = Dependency.create_dependency_graph program in
    ignore (Dependency.check_for_cycle dep_graph program true);
    if !Cli.run_all_tests <> None then
      let tests : string = match !Cli.run_all_tests with Some s -> s | _ -> assert false in
      Test.check_all_tests program tests
    else if !Cli.run_test <> None then begin
      Interpreter.repl_debug := true;
      let test : string = match !Cli.run_test with Some s -> s | _ -> assert false in
      Test.check_test program test
    end
    else begin
      Cli.debug_print "Extracting the desired function from the whole program...";
      let mvg_func = Interface.read_function_from_spec program in
      let program = Interface.fit_function program mvg_func in
      let program = if !Cli.optimize then Optimization.optimize program dep_graph else program in
      let dep_graph = Dependency.create_dependency_graph program in
      if String.lowercase_ascii !Cli.backend = "interpreter" then begin
        Cli.debug_print "Interpreting the program...";
        let program =
          {
            Interpreter.ip_program = program;
            ip_utils =
              {
                Interpreter.utilities_dep_graph = dep_graph;
                Interpreter.utilities_execution_order =
                  Execution_order.get_execution_order dep_graph;
              };
          }
        in
        let f = Interface.make_function_from_program program in
        let results = f (Interface.read_inputs_from_stdin mvg_func) in
        Interface.print_output mvg_func results;
        Interpreter.repl_debugguer results program.ip_program
      end
      else if
        String.lowercase_ascii !Cli.backend = "python"
        || String.lowercase_ascii !Cli.backend = "autograd"
      then begin
        Cli.debug_print "Compiling the program to Python...";
        if !Cli.output_file = "" then
          raise (Errors.ArgumentError "an output file must be defined with --output");
        Mvg_to_python.generate_python_program program dep_graph !Cli.output_file;
        Cli.result_print
          "Generated Python function from requested set of inputs and outputs, results written to %s\n"
          !Cli.output_file
      end
      else if String.lowercase_ascii !Cli.backend = "java" then begin
        Cli.debug_print "Compiling the program to Java...";
        if !Cli.output_file = "" then
          raise (Errors.ArgumentError "an output file must be defined with --output");
        Mvg_to_java.generate_java_program program dep_graph !Cli.output_file;
        Cli.result_print
          "Generated Java function from requested set of inputs and outputs, results written to %s\n"
          !Cli.output_file
      end
      else if String.lowercase_ascii !Cli.backend = "clojure" then begin
        Cli.debug_print "Compiling the program to Clojure...";
        if !Cli.output_file = "" then
          raise (Errors.ArgumentError "an output file must be defined with --output");
        Mvg_to_clojure.generate_clj_program program dep_graph !Cli.output_file;
        Cli.result_print
          "Generated Clojure function from requested set of inputs and outputs, results written to \
           %s\n"
          !Cli.output_file
      end
      else raise (Errors.ArgumentError (Format.asprintf "unknown backend (%s)" !Cli.backend))
    end
  with
  | Errors.TypeError e ->
      Cli.error_print "%a\n" Errors.format_typ_error e;
      Cmdliner.Term.exit_status (`Ok 2)
  | Errors.Unimplemented msg ->
      Cli.error_print "unimplemented (%s)\n" msg;
      Cmdliner.Term.exit ~term_err:Cmdliner.Term.exit_status_internal_error (`Ok ())
  | Errors.ArgumentError msg ->
      Cli.error_print "Command line argument error: %s\n" msg;
      Cmdliner.Term.exit ~term_err:Cmdliner.Term.exit_status_cli_error (`Ok ())

let main () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (Cli.mlang_t driver, Cli.info)
