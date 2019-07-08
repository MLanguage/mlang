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

open Verifisc
open Lexing
open Lexer


(** Entry function for the executable. Returns a negative number in case of error. *)
let main () : int =
  Cli.parse_cli_args ();
  Cli.debug_print "Reading files...";
  let program = ref [] in
  List.iter (fun source_file ->
      let (filebuf, input) = if source_file <> "" then
          let input = open_in source_file in
          (Lexing.from_channel input, Some input)
        else if source_file <> "" then
          (Lexing.from_string source_file, None)
        else
          failwith "You have to specify at least one file!"
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
          exit (-1)
        end
    ) !Cli.source_files;
  try
    let program, idmap, var_defs_not_in_app =
      Ast_to_mvg.translate !program (if !Cli.application = "" then None else Some !Cli.application)
    in

    Cli.debug_print ("Expanding function definitions...");
    let program = Functions.expand_functions program in

    Cli.debug_print "Typechecking...";
    let typing_info, program = Typechecker.typecheck program in

    Cli.debug_print "Checking for circular variable definitions...";
    let dep_graph = Dependency.create_dependency_graph program in
    Dependency.print_dependency_graph (!Cli.dep_graph_file ^ "_before_optimization.dot") dep_graph program;
    let program = Dependency.check_for_cycle dep_graph program in

    let program =
      Dependency.try_and_fix_undefined_dependencies dep_graph program var_defs_not_in_app
    in

    let dep_graph = Dependency.create_dependency_graph program in
    Cli.debug_print "Analysing the dependencies of the program...";
    let unused_variables = Dependency.get_unused_variables dep_graph program in
    Cli.debug_print (Printf.sprintf "Removing %d unused variables..." (Mvg.VariableMap.cardinal unused_variables));
    let program = {
      program with
      Mvg.program_vars =
        Mvg.VariableMap.filter
          (fun var _ -> not (Mvg.VariableMap.mem var unused_variables))
          program.program_vars
    } in

    let program = if !Cli.optimize then Optimize.optimize program typing_info idmap else program in
    let dep_graph = Dependency.create_dependency_graph program in
    Dependency.print_dependency_graph (!Cli.dep_graph_file ^ "_after_optimization.dot") dep_graph program;

    Cli.debug_print "Interpreting the program...";

    let input_values = Interface.sample_test_case program typing_info in
    let results = Interpreter.evaluate_program program dep_graph idmap input_values in
    Interface.print_output program idmap results;

    exit 0

  with
  | Errors.TypeError e ->
    Cli.error_print (Errors.format_typ_error e); exit 1
  | Errors.Unimplemented (msg) ->
    Cli.error_print (Printf.sprintf "unimplemented (%s)"  msg); -3

let _ = main ()
