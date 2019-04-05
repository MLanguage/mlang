(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by thge DGFiP.

This software is governed by the CeCILL-B license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-B
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
knowledge of the CeCILL-B license and that you accept its terms.
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
    let program = Ast_to_cfg.translate !program in
    Cli.debug_print "Typechecking...";
    let typing_info = Typechecker.typecheck program in
    typing_info;
    Cli.debug_print "Analysing dependencies...";
    let dep_graph = Dependency.create_dependency_graph program in
    Dependency.print_dependency_graph (!dep_graph_file ^ "_before_optimization.dot")  dep_graph;
    Dependency.check_for_cycle dep_graph;
    Cli.debug_print (Printf.sprintf "Optimizing program with %d variables..." (Cfg.VariableMap.cardinal program));
    Cli.debug_print ("Expanding function definitions...");
    let program = Functions.expand_functions program in
    Cli.debug_print (Printf.sprintf "Propagating constants variables...");
    let program = Constant_propagation.propagate_constants dep_graph program in
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
    Cli.debug_print (Printf.sprintf "Partially evaluating expressions...");
    let program = Constant_propagation.partially_evaluate program in
    Cli.debug_print
      (Printf.sprintf "Program variables count down to %d!"
         (Cfg.VariableMap.cardinal program));
    let dep_graph = Dependency.create_dependency_graph program in
    Dependency.print_dependency_graph (!dep_graph_file ^ "_after_optimization.dot") dep_graph;
    Cli.debug_print (Printf.sprintf "Translating the program into a Z3 query...");
    let cfg = [("model", "true"); ("timeout", (string_of_int (1000 * 30)))] in
    let ctx = (Z3.mk_context cfg) in
    let s = Z3.Solver.mk_solver ctx None in
    let z3_program = Cfg_to_z3.translate_program program typing_info ctx s in
    match Z3.Solver.check s [] with
    | Z3.Solver.UNSATISFIABLE -> Cli.result_print "Z3 found that the constraints are unsatisfiable!"
    | Z3.Solver.UNKNOWN -> Cli.result_print "Z3 didn't find an answer..."
    | Z3.Solver.SATISFIABLE ->
      Cli.result_print "Z3 found an answer!";
      let filename = "results.json" in
      Cli.result_print (Printf.sprintf "The values of all variables are written in %s" filename);
      let file = open_out filename in
      Printf.fprintf file "%s" (Format_z3.format_z3_program z3_program s)
  with
  | Errors.TypeError e ->
    error_print (Errors.format_typ_error e); exit 1
  | Errors.Unimplemented (msg,pos) ->
    error_print (Printf.sprintf "unimplemented for expression %s (code %s)" (Format_ast.format_position pos) msg)

let _ = main ()
