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
open Mlexer

(** Entry function for the executable. Returns a negative number in case of error. *)
let driver (files : string list) (application : string) (debug : bool) (display_time : bool)
    (dep_graph_file : string) (print_cycles : bool) (optimize : bool) (backend : string)
    (function_spec : string option) (mpp_file : string option) (output : string option)
    (real_precision : int) (run_all_tests : string option) (run_test : string option) (year : int) =
  Cli.set_all_arg_refs files application debug display_time dep_graph_file print_cycles optimize
    backend function_spec mpp_file output real_precision run_all_tests run_test year;
  try
    Cli.debug_print "Reading files...";
    let m_program = ref [] in
    if List.length !Cli.source_files = 0 then
      Errors.raise_error "please provide at least one M source file";
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
          { filebuf with lex_curr_p = { filebuf.lex_curr_p with pos_fname = source_file } }
        in
        try
          let commands = Mparser.source_file token filebuf in
          m_program := commands :: !m_program
        with Mparser.Error ->
          begin
            match input with
            | Some input -> close_in input
            | None -> ()
          end;
          Errors.raise_spanned_error "M syntax error"
            (Parse_utils.mk_position (filebuf.lex_start_p, filebuf.lex_curr_p)))
      !Cli.source_files;
    finish "completed!";
    let application = if !Cli.application = "" then None else Some !Cli.application in
    Cli.debug_print "Elaborating...";
    let m_program = Mast_to_mvg.translate !m_program application in
    let full_m_program = Mir_interface.to_full_program m_program in
    let full_m_program = Mir_typechecker.expand_functions full_m_program in
    Cli.debug_print "Typechecking...";
    let full_m_program = Mir_typechecker.typecheck full_m_program in
    Cli.debug_print "Checking for circular variable definitions...";
    ignore
      (Mir_dependency_graph.check_for_cycle full_m_program.dep_graph full_m_program.program true);
    let mpp = Option.get @@ Mpp_frontend.process mpp_file full_m_program in
    let m_program = Mir_interface.reset_all_outputs full_m_program.program in
    let full_m_program = Mir_interface.to_full_program m_program in
    Cli.debug_print "Creating combined program suitable for execution...";
    let combined_program = Mpp_ir_to_bir.create_combined_program full_m_program mpp in
    Cli.debug_print "Parsed mpp file %s" (Option.get mpp_file);
    if !Cli.run_all_tests <> None then
      let tests : string = match !Cli.run_all_tests with Some s -> s | _ -> assert false in
      Test_interpreter.check_all_tests combined_program full_m_program.execution_order tests
    else if !Cli.run_test <> None then begin
      Bir_interpreter.repl_debug := true;
      let test : string = match !Cli.run_test with Some s -> s | _ -> assert false in
      Test_interpreter.check_test combined_program full_m_program.execution_order test;
      Cli.result_print "Test passed!"
    end
    else begin
      Cli.debug_print "Extracting the desired function from the whole program...";
      let mvg_func = Mir_interface.read_function_from_spec full_m_program.program in
      let full_m_program =
        if !Cli.optimize then full_m_program (* todo: reinstate optimizations *) else full_m_program
      in
      if String.lowercase_ascii !Cli.backend = "interpreter" then begin
        Cli.debug_print "Interpreting the program...";
        let f = Mir_interface.make_function_from_program full_m_program in
        let results = f (Mir_interface.read_inputs_from_stdin mvg_func) in
        Mir_interface.print_output mvg_func results;
        Bir_interpreter.repl_debugguer results full_m_program.program
      end
      else if
        String.lowercase_ascii !Cli.backend = "python"
        || String.lowercase_ascii !Cli.backend = "autograd"
      then begin
        Cli.debug_print "Compiling the program to Python...";
        if !Cli.output_file = "" then
          Errors.raise_error "an output file must be defined with --output";
        Bir_to_python.generate_python_program combined_program !Cli.output_file;
        Cli.result_print
          "Generated Python function from requested set of inputs and outputs, results written to %s\n"
          !Cli.output_file
      end
      else if String.lowercase_ascii !Cli.backend = "java" then begin
        Cli.debug_print "Compiling the program to Java...";
        if !Cli.output_file = "" then
          Errors.raise_error "an output file must be defined with --output";
        Bir_to_java.generate_java_program full_m_program.program full_m_program.dep_graph
          !Cli.output_file;
        Cli.result_print
          "Generated Java function from requested set of inputs and outputs, results written to %s\n"
          !Cli.output_file
      end
      else if String.lowercase_ascii !Cli.backend = "clojure" then begin
        Cli.debug_print "Compiling the program to Clojure...";
        if !Cli.output_file = "" then
          Errors.raise_error "an output file must be defined with --output";
        Bir_to_clojure.generate_clj_program full_m_program.program full_m_program.dep_graph
          !Cli.output_file;
        Cli.result_print
          "Generated Clojure function from requested set of inputs and outputs, results written to \
           %s\n"
          !Cli.output_file
      end
      else Errors.raise_error (Format.asprintf "unknown backend %s" !Cli.backend)
    end
  with Errors.StructuredError (msg, pos, kont) ->
    Cli.error_print "%a\n" Errors.format_structured_error (msg, pos);
    (match kont with None -> () | Some kont -> kont ());
    exit (-1)

let main () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (Cli.mlang_t driver, Cli.info)
