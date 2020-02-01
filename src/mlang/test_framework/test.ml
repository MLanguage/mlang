(* Copyright Inria, contributors: Raphël Monat <raphael.monat@lip6.fr> (2019)

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Mvg
open Tast

let parse_file (test_name : string) : test_file =
  Parse_utils.current_file := test_name;
  let input = open_in test_name in
  let filebuf = Lexing.from_channel input in
  let filebuf =
    {
      filebuf with
      lex_curr_p = { filebuf.lex_curr_p with pos_fname = Filename.basename test_name };
    }
  in
  let f =
    try Some (Tparser.test_file Tlexer.token filebuf) with
    | Errors.LexingError msg | Errors.ParsingError msg ->
        close_in input;
        Cli.error_print "%s" msg;
        Cmdliner.Term.exit_status (`Ok 2);
        None
    | Tparser.Error ->
        Cli.error_print "Lexer error in file %s at position %a\n" test_name
          Errors.print_lexer_position filebuf.lex_curr_p;
        close_in input;
        Cmdliner.Term.exit_status (`Ok 2);
        None
  in
  match f with Some f -> f | None -> assert false

let to_ast_literal (value : Tast.literal) : Ast.literal =
  match value with I i -> Float (float_of_int i) | F f -> Float f

let to_mvg_function (program : Mvg.program) (t : test_file) :
    Interface.mvg_function * condition_data VariableMap.t =
  let func_variable_inputs = VariableMap.empty in
  let func_constant_inputs =
    Interface.const_var_set_from_list program
      (List.map
         (fun (var, value, pos) ->
           let mexpr : Ast.expression Pos.marked = (Literal (to_ast_literal value), pos) in
           (var, mexpr))
         t.ep)
  in
  let func_outputs = VariableMap.empty in
  (* Interface.var_set_from_variable_name_list program (List.map fst t.rp) in *)
  (* some output variables are actually input, so we don't declare any for now *)
  let func_conds =
    Interface.translate_cond program.program_idmap
      (List.map
         (fun (var, value, pos) ->
           ( Ast.Comparison
               ( (Eq, pos),
                 (Literal (Variable (Normal var)), pos),
                 (Literal (to_ast_literal value), pos) ),
             pos ))
         t.rp)
  in
  ( {
      func_variable_inputs;
      func_constant_inputs;
      func_outputs;
      func_conds = VariableMap.empty;
      func_exec_passes = None;
    },
    func_conds )

let check_test (p : Mvg.program) (typing : Typechecker.typ_info) (test_name : string) =
  Cli.debug_print "Parsing %s..." test_name;
  let t = parse_file test_name in
  Cli.debug_print "Running test %s..." t.nom;
  let f, test_conds = to_mvg_function p t in
  Cli.debug_print "Executing program";
  let p = Interface.fit_function p f in
  let ctx, p = Interpreter.evaluate_program p typing VariableMap.empty !Cli.number_of_passes in
  try
    VariableMap.iter
      (fun _ cond ->
        let result = Interpreter.evaluate_expr ctx p cond.cond_expr Boolean in
        match result with
        | Bool true ->
            raise
              (Interpreter.RuntimeError
                 ( Interpreter.ConditionViolated
                     ( cond.cond_errors,
                       cond.cond_expr,
                       [
                         ( match Pos.unmark cond.cond_expr with
                         | Unop (Ast.Not, (Comparison ((Ast.Eq, _), (Var var, _), (_, _)), _)) ->
                             (var, VariableMap.find var ctx.ctx_vars)
                         | _ -> assert false );
                         (* should not happen *)
                       ] ),
                   ctx ))
        | _ -> ())
      test_conds
  with Interpreter.RuntimeError (e, ctx) ->
    if !Interpreter.exit_on_rte then begin
      Cli.error_print "%a" Interpreter.format_runtime_error e;
      flush_all ();
      flush_all ();
      if !Interpreter.repl_debug then Interpreter.repl_debugguer ctx p;
      exit 1
    end
    else raise (Interpreter.RuntimeError (e, ctx))

let check_all_tests (p : Mvg.program) (typing : Typechecker.typ_info) (test_dir : string) =
  let arr = Sys.readdir test_dir in
  Interpreter.exit_on_rte := false;
  (* sort by increasing size, hoping that small files = simple tests *)
  Array.sort compare arr;
  (* (fun f1 f2 ->
   *   compare (Unix.stat (test_dir ^ f1)).st_size (Unix.stat (test_dir ^ f2)).st_size) arr; *)
  Cli.warning_flag := false;
  Cli.display_time := false;
  let _, finish = Cli.create_progress_bar "Testing files" in
  let process name (successes, failures) =
    try
      Cli.debug_flag := false;
      check_test p typing (test_dir ^ name);
      Cli.debug_flag := true;
      (name :: successes, failures) (* Cli.debug_print "Success on %s" name *)
    with Interpreter.RuntimeError (ConditionViolated (_, expr, bindings), _) -> (
      Cli.debug_flag := true;
      match (bindings, Pos.unmark expr) with
      | ( [ (v, Interpreter.SimpleVar l1) ],
          Unop (Not, (Comparison ((Ast.Eq, _), _, (Literal l2, _)), _)) ) ->
          (* Cli.debug_print "Failure on %s, var = %s, got = %a, expected = %a" name varname
             Format_mvg.format_literal l1 Format_mvg.format_literal l2; *)
          let errs_varname = try VariableMap.find v failures with Not_found -> [] in
          (successes, VariableMap.add v ((name, l1, l2) :: errs_varname) failures)
      | _ -> assert false )
  in
  (* Cli.debug_print @@ Interpreter.format_runtime_error e in *)
  let s, f =
    Parmap.parfold ~chunksize:10 process (Parmap.A arr) ([], VariableMap.empty)
      (fun (old_s, old_f) (new_s, new_f) ->
        (new_s @ old_s, VariableMap.union (fun _ x1 x2 -> Some (x1 @ x2)) old_f new_f))
  in
  finish "done!";
  Cli.debug_print "%d successes, on: %s" (List.length s) (String.concat ", " (List.sort compare s));
  Cli.debug_print "Failures:";
  let f_l =
    List.sort
      (fun (_, i) (_, i') -> -compare (List.length i) (List.length i'))
      (VariableMap.bindings f)
  in
  List.iter
    (fun (var, infos) ->
      Cli.debug_print "\t%s, %d errors in files %s" (Pos.unmark var.Variable.name)
        (List.length infos)
        (String.concat ", " (List.map (fun (n, _, _) -> n) (List.sort compare infos))))
    f_l
