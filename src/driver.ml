(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by the DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-C license and that you accept its terms.
*)

open Lexing
open Lexer

(** Entry function for the executable. Returns a negative number in case of error. *)
let driver
    (files: string list)
    (application: string)
    (debug: bool)
    (display_time: bool)
    (dep_graph_file: string)
    (print_cycles: bool)
    (optimize: bool)
    (backend: string)
    (function_spec: string)
    (output: string option)
    (number_of_passes: int)
  =
  Cli.set_all_arg_refs
    files
    application
    debug
    display_time
    dep_graph_file
    print_cycles
    optimize
    backend
    function_spec
    output
    number_of_passes;
  try
    Cli.debug_print "Reading files...";
    let program = ref [] in
    if List.length !Cli.source_files = 0 then
      raise (Errors.ArgumentError "please provide at least one M source file");
    let current_progress, finish = Cli.create_progress_bar "Parsing" in
    List.iter (fun source_file ->
        let (filebuf, input) = if source_file <> "" then
            let input = open_in source_file in
            (Lexing.from_channel input, Some input)
          else if source_file <> "" then
            (Lexing.from_string source_file, None)
          else
            failwith "You have to specify at least one file!"
        in
        current_progress source_file;
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
          Cli.error_print msg
        | Parser.Error -> begin
            Cli.error_print
              (Printf.sprintf "Lexer error in file %s at position %s"
                 (!Parse_utils.current_file)
                 (Errors.print_lexer_position filebuf.lex_curr_p));
            begin match input with
              | Some input -> close_in input
              | None -> ()
            end;
            Cmdliner.Term.exit_status (`Ok 2);
          end
      ) !Cli.source_files;
    finish "completed!";
    let application = if !Cli.application = "" then None else Some !Cli.application in
    let program =
      Ast_to_mvg.translate !program application
    in
    Cli.debug_print "Extracting the desired function from the whole program...";
    let mvg_func = Interface.read_function_from_spec program in
    let program = Interface.fit_function program mvg_func in
    Cli.debug_print ("Expanding function definitions...");
    let program = Functions.expand_functions program in
    Cli.debug_print "Typechecking...";
    let typing, program = Typechecker.typecheck program in
    Cli.debug_print "Checking for circular variable definitions...";
    let dep_graph = Dependency.create_dependency_graph program in
    ignore (Dependency.check_for_cycle dep_graph program true);



    let program = if !Cli.optimize then Optimize.optimize program else program in
    (* Noundef.check program; *)

    (* Mvg.VariableMap.iter (fun var (ty, bool) ->
     *     if Mvg.VariableMap.mem var program.program_vars then
     *       Cli.debug_print (Format.sprintf "%s -> %s\n" (Ast.unmark var.name) (Mvg.show_typ ty)))
     *   typing.Typechecker.typ_info_var; *)

    begin if String.lowercase_ascii !Cli.backend = "z3" then
        Z3_driver.translate_and_launch_query program typing
      else if String.lowercase_ascii !Cli.backend = "specifisc" then begin
        Cli.debug_print "Translating the M program to Specifisc";
        ignore (Mvg_to_specifisc.translate_program program typing)
      end else if String.lowercase_ascii !Cli.backend = "interpreter" then begin
        Cli.debug_print "Interpreting the program...";
        let f = Interface.make_function_from_program program !Cli.number_of_passes in
        let results = f (Interface.read_inputs_from_stdin mvg_func) in
        Interface.print_output mvg_func results;
        Interpreter.repl_debugguer results program
      end else if
        String.lowercase_ascii !Cli.backend = "python" ||
        String.lowercase_ascii !Cli.backend = "autograd"
      then begin
        Cli.debug_print "Compiling the program to Python...";
        if !Cli.output_file = "" then
          raise (Errors.ArgumentError "an output file must be defined with --output");
        Mvg_to_python.generate_python_program program !Cli.output_file !Cli.number_of_passes;
        Cli.result_print (Printf.sprintf "Generated Python function from requested set of inputs and outputs, results written to %s" !Cli.output_file)
      end else
        raise (Errors.ArgumentError (Printf.sprintf "unknown backend (%s)" !Cli.backend))
    end
  with
  | Errors.TypeError e ->
    Cli.error_print (Errors.format_typ_error e); Cmdliner.Term.exit_status (`Ok 2)
  | Errors.Unimplemented (msg) ->
    Cli.error_print (Printf.sprintf "unimplemented (%s)"  msg); Cmdliner.Term.exit ~term_err:Cmdliner.Term.exit_status_internal_error (`Ok ())
  | Errors.ArgumentError msg ->
    Cli.error_print (Printf.sprintf "Command line argument error: %s" msg); Cmdliner.Term.exit ~term_err:Cmdliner.Term.exit_status_cli_error (`Ok ())
  | Errors.UnsupportedBySpecifisc msg ->
    Cli.error_print (Printf.sprintf "Unsupported by Specifisc: %s" msg); Cmdliner.Term.exit_status (`Ok 3)

let main () =
  Cmdliner.Term.exit @@ Cmdliner.Term.eval (Cli.verifisc_t driver, Cli.info)
