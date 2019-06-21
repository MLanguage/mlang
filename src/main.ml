(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by thge DGFiP.

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

open Lexer
open Lexing
open Cli

(** Entry function for the executable. Returns a negative number in case of error. *)
let main () : int =
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
    let program, idmap, var_defs_not_in_app =
      Ast_to_mvg.translate !program (if !application = "" then None else Some !application)
    in

    Cli.debug_print ("Expanding function definitions...");
    let program = Functions.expand_functions program in

    Cli.debug_print "Typechecking...";
    let typing_info, program = Typechecker.typecheck program in

    (* Cli.warning_print @@ Printf.sprintf "Result: %s\n" (Typechecker.show_typ_info typing_info); *)

    Cli.debug_print "Analysing dependencies...";
    let dep_graph = Dependency.create_dependency_graph program in
    Dependency.print_dependency_graph (!dep_graph_file ^ "_before_optimization.dot") dep_graph program;
    if not !no_cycles_check_flag then
      Dependency.check_for_cycle dep_graph program;

    let program =
      Dependency.try_and_fix_undefined_dependencies dep_graph program var_defs_not_in_app
    in
    let dep_graph = Dependency.create_dependency_graph program in

    let correctly_defined_outputs =
      Dependency.correctly_defined_outputs dep_graph program
    in

    let correctly_defined_output_variables =
      List.map
        (fun (v, _) -> Format_mvg.format_variable v ^ " | Type: " ^
                       (Format_mvg.format_typ
                          (fst (Mvg.VariableMap.find v typing_info.Typechecker.typ_info_var))))
        (Mvg.VariableMap.bindings correctly_defined_outputs)
    in

    if Mvg.VariableMap.cardinal correctly_defined_outputs = 0 then begin
      raise (Errors.TypeError (
          Errors.Variable
            ("there are no correctly defined output variables (see warnings for undefined variables)."
            )))
    end;
    let defined_output_variables = "defined_output_variables.txt" in
    Cli.debug_print @@ Printf.sprintf "Correctly defined output variables: %d, see %s for list."
      (List.length correctly_defined_output_variables)
      defined_output_variables;
    if !Cli.debug_flag then begin
      let oc = open_out defined_output_variables in
      Printf.fprintf oc "%s"
        (String.concat "\n" correctly_defined_output_variables);
      close_out oc
    end;

    let program = Dependency.requalify_outputs program correctly_defined_outputs in

    Cli.debug_print (Printf.sprintf "Optimizing program with %d variables..." (Mvg.VariableMap.cardinal program));
    let unused_variables = Dependency.get_unused_variables dep_graph program in
    Cli.debug_print (Printf.sprintf "Removing %d unused variables..." (Mvg.VariableMap.cardinal unused_variables));
    let program = Mvg.VariableMap.filter (fun var _ -> not (Mvg.VariableMap.mem var unused_variables)) program in

    (* Printf.printf "Program: %s\n" (Format_mvg.format_program program); *)
    Cli.debug_print (Printf.sprintf "Propagating constants variables...");
    let dep_graph = Dependency.create_dependency_graph program in
    let program = Constant_propagation.propagate_constants dep_graph program in

    let program : Mvg.program ref = ref program in
    let typing_info : Typechecker.typ_info ref = ref typing_info in
    let nb_inlined_vars : int ref = ref max_int in
    while (0 < !nb_inlined_vars) do
      let dep_graph = Dependency.create_dependency_graph !program in
      let single_use_vars = Dependency.single_use_vars dep_graph in
      let to_inline_vars = Mvg.VariableMap.filter
          (fun var _ -> try
              match (Mvg.VariableMap.find var !program).Mvg.var_io with
              | Mvg.Input | Mvg.Output -> false
              | Mvg.Regular -> true
            with
            | Not_found -> false (* TODO: figure out why it's happening *)
          ) single_use_vars in
      nb_inlined_vars := Mvg.VariableMap.cardinal to_inline_vars;
      if !nb_inlined_vars > 0 then begin
        Cli.debug_print (Printf.sprintf "Inlining %d variables..." !nb_inlined_vars);
        let (new_program, new_typing_info) =
          Inlining.inline_vars to_inline_vars !typing_info !program
        in
        program := new_program;
        typing_info := new_typing_info;
      end
    done;
    let program = !program in
    let typing_info = !typing_info in

    Cli.debug_print (Printf.sprintf "Propagating constants variables...");
    let dep_graph = Dependency.create_dependency_graph program in
    let program = Constant_propagation.propagate_constants dep_graph program in

    let dep_graph = Dependency.create_dependency_graph program in
    let unused_variables = Dependency.get_unused_variables dep_graph program in
    Cli.debug_print (Printf.sprintf "Removing %d unused variables..." (Mvg.VariableMap.cardinal unused_variables));
    let program = Mvg.VariableMap.filter (fun var _ -> not (Mvg.VariableMap.mem var unused_variables)) program in

    Cli.debug_print (Printf.sprintf "Partially evaluating expressions...");
    let program = Constant_propagation.partially_evaluate program in
    Cli.debug_print
      (Printf.sprintf "Program variables count down to %d!"
         (Mvg.VariableMap.cardinal program));


    let dep_graph = Dependency.create_dependency_graph program in
    Dependency.print_dependency_graph (!dep_graph_file ^ "_after_optimization.dot") dep_graph program;

    let minimal_input_variables =
      List.map
        (fun (v, _) -> Format_mvg.format_variable v ^ " | Type: " ^
                       (Format_mvg.format_typ
                          (fst (Mvg.VariableMap.find v typing_info.Typechecker.typ_info_var))))
        (Mvg.VariableMap.bindings
           (Mvg.VariableMap.filter (fun _ var_data -> var_data.Mvg.var_io = Mvg.Input) program)
        )
    in
    Cli.debug_print @@ Printf.sprintf "Number of input variables needed: %d."
      (List.length minimal_input_variables);

    ignore (exit 0);

    Cli.debug_print (Printf.sprintf "The program so far:\n%s\n" (Format_mvg.format_program program));
    Cli.debug_print (Printf.sprintf "Translating the program into a Z3 query...");
    let mvg = [("model", "true"); ("timeout", (string_of_int (1000 * 30)))] in
    let ctx = (Z3.mk_context mvg) in
    let s = Z3.Solver.mk_solver ctx None in
    let typing_info = Z3_repr.find_bitvec_repr program dep_graph typing_info in
    Cli.debug_print @@ Printf.sprintf "repr_info_var: %s\nrepr_info_local_var: %s\n"
      (Mvg.VariableMap.show Z3_repr.show_repr typing_info.Z3_repr.repr_info_var)
      (Mvg.LocalVariableMap.show Z3_repr.show_repr typing_info.Z3_repr.repr_info_local_var);
    let z3_program = Mvg_to_z3.translate_program program dep_graph typing_info ctx s in
    let t0 = Sys.time () in
    Cli.debug_print
      (Printf.sprintf
         "The Z3 query will contain %d different variables"
         (Mvg.VariableMap.cardinal z3_program.Z3_repr.repr_data_var +
          Mvg.LocalVariableMap.cardinal z3_program.Z3_repr.repr_data_local_var)
      );
    match Z3.Solver.check s [] with
    | Z3.Solver.UNSATISFIABLE -> Cli.result_print "Z3 found that the constraints are unsatisfiable!"; -1
    | Z3.Solver.UNKNOWN -> Cli.result_print "Z3 didn't find an answer..."; -2
    | Z3.Solver.SATISFIABLE ->
      let t1 = Sys.time () in
      Cli.result_print "Z3 found an answer!";
      let filename = "results.json" in
      Cli.result_print (Printf.sprintf "The values of all variables are written in %s" filename);
      let file = open_out filename in
      Printf.fprintf file "%s" (Format_z3.format_z3_program z3_program.Z3_repr.repr_data_var ctx s);
      Cli.result_print
        (Printf.sprintf
           "The query took %f seconds to execute. Here are some statistics about it:\n%s"
           (t1 -. t0)
           (Z3.Statistics.to_string (Z3.Solver.get_statistics s))
        ); 0
  with
  | Errors.TypeError e ->
    error_print (Errors.format_typ_error e); exit 1
  | Errors.Unimplemented (msg,pos) ->
    error_print (Printf.sprintf "unimplemented for expression %s (%s)" (Format_ast.format_position pos) msg); -3

let _ = main ()
