(*
Copyright Inria, contributors:
  Raphël Monat <raphael.monat@lip6.fr> (2019)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

open Mvg
open Tast

let parse_file (test_name:string) : test_file =
  Parse_utils.current_file := test_name;
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

let to_ast_literal (value:Tast.literal) : Ast.literal =
  match value with
  | I i -> Int i
  | F f -> Float f

let to_mvg_function (program:Mvg.program) (t: test_file) : Interface.mvg_function =
  let func_variable_inputs = VariableMap.empty in
  let func_constant_inputs =
    Interface.const_var_set_from_list program
      (List.map (fun (var, value, pos) ->
           let mexpr : Ast.expression Pos.marked = Literal (to_ast_literal value), pos in
           var, mexpr
         )
          t.ep)
  in
  let func_outputs = VariableMap.empty in
  (* Interface.var_set_from_variable_name_list program (List.map fst t.rp) in *)
  (* some output variables are actually input, so we don't declare any for now *)
  let func_conds =
    Interface.translate_cond program.program_idmap
      (List.map (fun (var, value, pos) ->
           Ast.Comparison ((Eq, pos),
                           (Literal (Variable (Normal var)), pos),
                           (Literal (to_ast_literal value), pos)),
           pos) t.rp) in
  { func_variable_inputs; func_constant_inputs; func_outputs; func_conds }


let check_test (p: Mvg.program) (test_name: string) =
  Cli.debug_print (Printf.sprintf "Parsing %s..." test_name);
  let t = parse_file test_name in
  Cli.debug_print (Printf.sprintf "Running test %s..." t.nom);
  let f = to_mvg_function p t in
  Cli.debug_print (Printf.sprintf "Executing program");
  let p = Interface.fit_function p f in
  let _ =  Interpreter.evaluate_program p VariableMap.empty 5 in
  ()

let check_all_tests (p:Mvg.program) (test_dir: string) =
  let arr = Sys.readdir test_dir in
  Interpreter.exit_on_rte := false;
  (* sort by increasing size, hoping that small files = simple tests *)
  Array.sort (fun f1 f2 ->
      Pervasives.compare (Unix.stat (test_dir ^ f1)).st_size (Unix.stat (test_dir ^ f2)).st_size) arr;
  Cli.debug_flag := false;
  Cli.warning_flag := false;
  let process = fun name ->
      try
        check_test p (test_dir ^ name);
        Cli.debug_print (Printf.sprintf "Success on %s!" name)
      with Interpreter.RuntimeError (e, _) ->
        Cli.debug_print @@ Interpreter.format_runtime_error e in
  Array.iter process arr
