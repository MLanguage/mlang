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
let driver (files : string list) (debug : bool) (var_info_debug : string list) (display_time : bool)
    (dep_graph_file : string) (print_cycles : bool) (backend : string option)
    (function_spec : string option) (mpp_file : string) (output : string option)
    (run_all_tests : string option) (run_test : string option) (mpp_function : string)
    (optimize : bool) (optimize_unsafe_float : bool) (code_coverage : bool)
    (precision : string option) (test_error_margin : float option) =
  Cli.set_all_arg_refs files debug var_info_debug display_time dep_graph_file print_cycles output
    optimize_unsafe_float;
  try
    Cli.debug_print "Reading M files...";
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
    Cli.debug_print "Elaborating...";
    let m_program = Mast_to_mvg.translate !m_program in
    let full_m_program = Mir_interface.to_full_program m_program in
    let full_m_program = Mir_typechecker.expand_functions full_m_program in
    Cli.debug_print "Typechecking...";
    let full_m_program = Mir_typechecker.typecheck full_m_program in
    Cli.debug_print "Checking for circular variable definitions...";
    ignore
      (Mir_dependency_graph.check_for_cycle full_m_program.dep_graph full_m_program.program true);
    let mpp = Mpp_frontend.process mpp_file full_m_program in
    let m_program = Mir_interface.reset_all_outputs full_m_program.program in
    let full_m_program = Mir_interface.to_full_program m_program in
    Cli.debug_print "Creating combined program suitable for execution...";
    let combined_program = Mpp_ir_to_bir.create_combined_program full_m_program mpp mpp_function in
    let value_sort =
      let precision = Option.get precision in
      if precision = "double" then Bir_interpreter.RegularFloat
      else
        let mpfr_regex = Re.Pcre.regexp "^mpfr(\\d+)$" in
        if Re.Pcre.pmatch ~rex:mpfr_regex precision then
          let mpfr_prec = Re.Pcre.get_substring (Re.Pcre.exec ~rex:mpfr_regex precision) 1 in
          Bir_interpreter.MPFR (int_of_string mpfr_prec)
        else if precision = "interval" then Bir_interpreter.Interval
        else
          let bigint_regex = Re.Pcre.regexp "^fixed(\\d+)$" in
          if Re.Pcre.pmatch ~rex:bigint_regex precision then
            let fixpoint_prec =
              Re.Pcre.get_substring (Re.Pcre.exec ~rex:bigint_regex precision) 1
            in
            Bir_interpreter.BigInt (int_of_string fixpoint_prec)
          else if precision = "mpq" then Bir_interpreter.Rational
          else Errors.raise_error (Format.asprintf "Unkown precision option: %s" precision)
    in
    if run_all_tests <> None then begin
      if code_coverage && optimize then
        Errors.raise_error
          "Code coverage and program optimizations cannot be enabled together when running a test \
           suite, check your command-line options";
      let tests : string = match run_all_tests with Some s -> s | _ -> assert false in
      Test_interpreter.check_all_tests combined_program tests optimize code_coverage value_sort
        (Option.get test_error_margin)
    end
    else if run_test <> None then begin
      Bir_interpreter.repl_debug := true;
      if code_coverage then
        Cli.warning_print "The code coverage flag is ignored when running a single test";
      let test : string = match run_test with Some s -> s | _ -> assert false in
      ignore
        (Test_interpreter.check_test combined_program test optimize false value_sort
           (Option.get test_error_margin));
      Cli.result_print "Test passed!"
    end
    else begin
      Cli.debug_print "Extracting the desired function from the whole program...";
      let spec_file =
        match function_spec with
        | None ->
            Errors.raise_error "function specification file is not specified using --function_spec"
        | Some f -> f
      in
      let function_spec = Bir_interface.read_function_from_spec combined_program spec_file in
      let combined_program, _ =
        Bir_interface.adapt_program_to_function combined_program function_spec
      in
      let combined_program =
        if optimize then begin
          Cli.debug_print "Translating to CFG form for optimizations...";
          let oir_program = Bir_to_oir.bir_program_to_oir combined_program in
          Cli.debug_print "Optimizing...";
          let oir_program = Oir_optimizations.optimize oir_program in
          Cli.debug_print "Translating back to AST...";
          let combined_program = Bir_to_oir.oir_program_to_bir oir_program in
          combined_program
        end
        else combined_program
      in
      match backend with
      | Some backend ->
          if String.lowercase_ascii backend = "interpreter" then begin
            Cli.debug_print "Interpreting the program...";
            let inputs = Bir_interface.read_inputs_from_stdin function_spec in
            let print_output =
              Bir_interpreter.evaluate_program function_spec combined_program inputs 0 value_sort
            in
            print_output ()
          end
          else if String.lowercase_ascii backend = "python" then begin
            Cli.debug_print "Compiling the codebase to Python...";
            if !Cli.output_file = "" then
              Errors.raise_error "an output file must be defined with --output";
            Bir_to_python.generate_python_program combined_program function_spec !Cli.output_file;
            Cli.debug_print "Result written to %s" !Cli.output_file
          end
          else if String.lowercase_ascii backend = "c" then begin
            Cli.debug_print "Compiling the codebase to C...";
            if !Cli.output_file = "" then
              Errors.raise_error "an output file must be defined with --output";
            Bir_to_c.generate_c_program combined_program function_spec !Cli.output_file;
            Cli.debug_print "Result written to %s" !Cli.output_file
          end
          else Errors.raise_error (Format.asprintf "Unknown backend: %s" backend)
      | None -> Errors.raise_error "No backend specified!"
    end
  with Errors.StructuredError (msg, pos, kont) ->
    Cli.error_print "Error: %a" Errors.format_structured_error (msg, pos);
    (match kont with None -> () | Some kont -> kont ());
    exit (-1)

let main () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (Cli.mlang_t driver, Cli.info)
