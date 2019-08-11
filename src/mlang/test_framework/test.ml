(*
Copyright Inria, contributors:
  Raphël Monat <raphael.monat@lip6.fr> (2019)

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

open Mvg
open Tast


let process program =
  let idmap = program.program_idmap in
  List.fold_left (fun acc (var, value) ->
      let v =
        try Ast_to_mvg.list_max_execution_number @@ Pos.VarNameToID.find var idmap
        with Not_found ->
          let n = Mvg.find_var_name_by_alias program var in
          Ast_to_mvg.list_max_execution_number @@ Pos.VarNameToID.find n idmap
      in
      VariableMap.add v (Mvg.Int value) acc
    ) VariableMap.empty


let parse_file test_name =
  let input = open_in test_name in
  let filebuf = Lexing.from_channel input in
  let filebuf = {filebuf with
                 lex_curr_p = { filebuf.lex_curr_p with
                                pos_fname = Filename.basename test_name
                              }
                } in
  let f = try
      Some (Tparser.test_file Tlexer.token filebuf)
    with
    | Errors.LexingError msg | Errors.ParsingError msg ->
      close_in input;
      Cli.error_print msg;
      Cmdliner.Term.exit_status (`Ok 2);
      None
    | Tparser.Error -> begin
        Cli.error_print
          (Printf.sprintf "Lexer error in file %s at position %s"
             test_name
             (Errors.print_lexer_position filebuf.lex_curr_p));
        close_in input;
        Cmdliner.Term.exit_status (`Ok 2);
        None
      end in
  match f with
  | Some f -> f
  | None -> assert false

let to_ast_literal value : Ast.literal =
  match value with
  | I i -> Int i
  | F f -> Float f

let to_mvg_function (program:Mvg.program) (t: test_file) : Interface.mvg_function =
  let func_variable_inputs = VariableMap.empty in
  let func_constant_inputs =
    Interface.const_var_set_from_list program
      (List.map (fun (var, value) ->
           (* Cli.debug_print (Printf.sprintf "input %s" var); *)
           let mexpr : Ast.expression Pos.marked = Literal (to_ast_literal value), Pos.no_pos in
           var, mexpr
         )
          t.ep)
  in
  let func_outputs =(* VariableMap.empty in *)
    Interface.var_set_from_variable_name_list program (List.map fst t.rp) in
  (* some output variables are actually input, so we don't declare any for now *)
  let func_conds =
    VariableMap.empty in
  (* Interface.translate_cond program.program_idmap
   *   (List.map (fun (var, value) ->
   *        Ast.Comparison ((Eq, Pos.no_pos),
   *                    (Literal (Variable (Normal var)), Pos.no_pos),
   *                    (Literal (to_ast_literal value), Pos.no_pos)),
   *                    Pos.no_pos) t.rp) in *)
  { func_variable_inputs; func_constant_inputs; func_outputs; func_conds }


let check_test (p: Mvg.program) (test_name: string) =
  Cli.debug_print (Printf.sprintf "Parsing %s..." test_name);
  let t = parse_file test_name in
  Cli.debug_print (Printf.sprintf "Running test %s..." t.nom);
  let f = to_mvg_function p t in
  Cli.debug_print (Printf.sprintf "Executing program");
  let p = Interface.fit_function p f in
  let _ =  Interpreter.evaluate_program p VariableMap.empty 3 in
  ()

let check_all_tests (p:Mvg.program) =
  let arr = Sys.readdir "tests/" in
  Array.sort (fun f1 f2 ->
      Pervasives.compare (Unix.stat ("tests/" ^ f1)).st_size (Unix.stat ("tests/" ^ f2)).st_size) arr;
  Array.iter (fun name -> check_test p ("tests/" ^ name)) arr
