(* Copyright Inria, contributors: Raphël Monat <raphael.monat@lip6.fr> (2019)

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Mir
open Test_ast

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
    try Some (Test_parser.test_file Test_lexer.token filebuf) with
    | Errors.LexingError msg | Errors.ParsingError msg ->
        close_in input;
        Cli.error_print "%s" msg;
        Cmdliner.Term.exit_status (`Ok 2);
        None
    | Test_parser.Error ->
        Cli.error_print "Lexer error in file %s at position %a\n" test_name
          Errors.print_lexer_position filebuf.lex_curr_p;
        close_in input;
        Cmdliner.Term.exit_status (`Ok 2);
        None
  in
  match f with
  | Some f ->
      close_in input;
      f
  | None -> assert false

let to_ast_literal (value : Test_ast.literal) : Mast.literal =
  match value with I i -> Float (float_of_int i) | F f -> Float f

let find_var_of_name (p : Mir.program) (name : string) : Variable.t =
  try
    List.hd
      (List.sort
         (fun v1 v2 -> compare v1.Mir.Variable.execution_number v2.Mir.Variable.execution_number)
         (Pos.VarNameToID.find name p.program_idmap))
  with Not_found ->
    let name = find_var_name_by_alias p name in
    List.hd
      (List.sort
         (fun v1 v2 -> compare v1.Mir.Variable.execution_number v2.Mir.Variable.execution_number)
         (Pos.VarNameToID.find name p.program_idmap))

let to_mvg_function_and_inputs (program : Mir.program) (t : test_file) :
    Mir_interface.mvg_function * condition_data VariableMap.t * Mir.literal VariableMap.t =
  let func_variable_inputs, input_file =
    List.fold_left
      (fun (fv, in_f) (var, value, _) ->
        let var = find_var_of_name program var in
        let lit = match value with I i -> Float (float_of_int i) | F f -> Float f in
        (VariableMap.add var () fv, VariableMap.add var lit in_f))
      (VariableMap.empty, VariableMap.empty)
      t.ep
  in
  let func_constant_inputs = VariableMap.empty in
  let func_outputs = VariableMap.empty in
  (* some output variables are actually input, so we don't declare any for now *)
  let func_conds =
    Mir_interface.translate_cond program.program_idmap
      (List.map
         (fun (var, value, pos) ->
           (* we allow a difference of 0 between the control value and the result *)
           let first_exp =
             ( Mast.Comparison
                 ( (Lte, pos),
                   ( Mast.Binop
                       ( (Mast.Sub, pos),
                         (Literal (Variable (Normal var)), pos),
                         (Literal (to_ast_literal value), pos) ),
                     pos ),
                   (Literal (Float 0.), pos) ),
               pos )
           in
           let second_exp =
             ( Mast.Comparison
                 ( (Gte, pos),
                   ( Mast.Binop
                       ( (Mast.Sub, pos),
                         (Literal (Variable (Normal var)), pos),
                         (Literal (to_ast_literal value), pos) ),
                     pos ),
                   (Literal (Float 0.), pos) ),
               pos )
           in
           (Mast.Binop ((Mast.And, pos), first_exp, second_exp), pos))
         t.rp)
  in
  ( {
      func_variable_inputs;
      func_constant_inputs;
      func_outputs;
      func_conds = VariableMap.empty;
      func_exec_passes = None;
    },
    func_conds,
    input_file )

let add_test_conds_usage_to_outputs (p : Mir_interface.full_program)
    (test_conds : condition_data VariableMap.t) : Mir_interface.full_program =
  let outputs =
    VariableMap.fold
      (fun _ test_cond acc ->
        let vars_used_by_test =
          Mir_dependency_graph.get_used_variables test_cond.cond_expr VariableMap.empty
        in
        VariableMap.fold
          (fun used_var _ acc -> VariableMap.add used_var () acc)
          vars_used_by_test acc)
      test_conds VariableMap.empty
  in
  let program =
    {
      p.program with
      program_vars =
        VariableMap.mapi
          (fun var data ->
            if VariableMap.mem var outputs then
              match data.Mir.var_io with
              | Input | Output -> data
              | Regular -> { data with var_io = Output }
            else data)
          p.program.program_vars;
    }
  in
  { p with program }

let check_test (p : Mir.program) mpp (test_name : string) =
  Cli.debug_print "Parsing %s..." test_name;
  let t = parse_file test_name in
  Cli.debug_print "Running test %s..." t.nom;
  let f, test_conds, input_file = to_mvg_function_and_inputs p t in
  Cli.debug_print "Executing program";
  let p = Mir_interface.fit_function p f in
  let dep_graph = Mir_dependency_graph.create_dependency_graph p in
  let exec_order = Mir_dependency_graph.get_execution_order dep_graph in
  let p =
    {
      Mir_interface.program = p;
      Mir_interface.dep_graph;
      Mir_interface.execution_order = exec_order;
    }
  in
  let p = add_test_conds_usage_to_outputs p test_conds in
  let combined_program = Mpp_ir_to_bir.create_combined_program p mpp in
  (* Cli.debug_print "Combined Program (w/o verif conds):@.%a@." Format_mir.format_new_program
     combined_program; *)
  let ctx =
    Bir_interpreter.evaluate_program combined_program input_file
      (Bir_interpreter.empty_ctx p.program)
  in
  let test_cond_list = VariableMap.bindings test_conds in
  let execution_order_list : (Variable.t * int) list =
    List.mapi (fun i var -> (var, i)) (Mir_dependency_graph.get_execution_order p.dep_graph)
  in
  let execution_order_map : int VariableMap.t =
    List.fold_left
      (fun acc (var, i) -> VariableMap.add var i acc)
      VariableMap.empty execution_order_list
  in
  (* We sort the control variables according to execution order so that we are able to catch the
     "first" mistakes first. *)
  let exec_order_compare ((var1, _) : Variable.t * condition_data)
      ((var2, _) : Variable.t * condition_data) : int =
    try
      let pos1 = VariableMap.find var1 execution_order_map in
      let pos2 = VariableMap.find var2 execution_order_map in
      compare pos1 pos2
    with Not_found -> 0
  in
  try
    List.iter
      (fun (_, cond) ->
        let result = Bir_interpreter.evaluate_expr ctx p.program cond.cond_expr in
        match result with
        | Float f when f <> 0. ->
            raise
              (Bir_interpreter.RuntimeError
                 ( Bir_interpreter.ConditionViolated
                     ( cond.cond_errors,
                       cond.cond_expr,
                       [
                         ( match Pos.unmark cond.cond_expr with
                         | Unop
                             ( Mast.Not,
                               ( Mir.Binop
                                   ( (Mast.And, _),
                                     ( Comparison
                                         ( (Mast.Lte, _),
                                           (Mir.Binop ((Mast.Sub, _), (Var var, _), _), _),
                                           (_, _) ),
                                       _ ),
                                     _ ),
                                 _ ) ) ->
                             (var, VariableMap.find var ctx.ctx_vars)
                         | _ -> assert false );
                         (* should not happen *)
                       ] ),
                   ctx ))
        | _ -> ())
      (List.sort exec_order_compare test_cond_list)
  with Bir_interpreter.RuntimeError (e, ctx) ->
    if !Bir_interpreter.exit_on_rte then begin
      Cli.error_print "%a" Bir_interpreter.format_runtime_error e;
      flush_all ();
      flush_all ();
      if !Bir_interpreter.repl_debug then Bir_interpreter.repl_debugguer ctx p.program;
      exit 1
    end
    else raise (Bir_interpreter.RuntimeError (e, ctx))

let check_all_tests (p : Mir.program) mpp (test_dir : string) =
  let arr = Sys.readdir test_dir in
  let arr =
    Array.of_list
    @@ List.filter (fun x -> not @@ Sys.is_directory (test_dir ^ "/" ^ x)) (Array.to_list arr)
  in
  Bir_interpreter.exit_on_rte := false;
  (* sort by increasing size, hoping that small files = simple tests *)
  Array.sort compare arr;
  Cli.warning_flag := false;
  Cli.display_time := false;
  let _, finish = Cli.create_progress_bar "Testing files" in
  let process name (successes, failures) =
    try
      Cli.debug_flag := false;
      check_test p mpp (test_dir ^ name);
      Cli.debug_flag := true;
      (name :: successes, failures)
    with
    | Bir_interpreter.RuntimeError (ConditionViolated (err, expr, bindings), _) -> (
        Cli.debug_flag := true;
        match (bindings, Pos.unmark expr) with
        | ( [ (v, Bir_interpreter.SimpleVar l1) ],
            Unop
              ( Mast.Not,
                ( Mir.Binop
                    ( (Mast.And, _),
                      ( Comparison
                          ((Mast.Lte, _), (Mir.Binop ((Mast.Sub, _), _, (Literal l2, _)), _), (_, _)),
                        _ ),
                      _ ),
                  _ ) ) ) ->
            let errs_varname = try VariableMap.find v failures with Not_found -> [] in
            (successes, VariableMap.add v ((name, l1, l2) :: errs_varname) failures)
        | _ ->
            Cli.error_print "Test %s incorrect (error%s %a raised)@." name
              (if List.length err > 1 then "s" else "")
              (Format.pp_print_list Format.pp_print_string)
              (List.map (fun x -> Pos.unmark x.Error.name) err);
            (successes, failures) )
    | Errors.TypeError t ->
        Cli.error_print "Type error in %s (%a), case not taken into account@." name
          Errors.format_typ_error t;
        (successes, failures)
  in
  let s, f =
    Parmap.parfold ~chunksize:5 process (Parmap.A arr) ([], VariableMap.empty)
      (fun (old_s, old_f) (new_s, new_f) ->
        (new_s @ old_s, VariableMap.union (fun _ x1 x2 -> Some (x1 @ x2)) old_f new_f))
  in
  finish "done!";
  Cli.warning_flag := true;
  Cli.display_time := true;
  Cli.result_print "Test results: %d successes in files: %s@." (List.length s)
    (String.concat ", " (List.sort compare s));

  let f_l =
    List.sort
      (fun (_, i) (_, i') -> -compare (List.length i) (List.length i'))
      (VariableMap.bindings f)
  in
  if List.length f_l = 0 then Cli.result_print "No failures!@."
  else begin
    Cli.warning_print "Failures:@.";
    List.iter
      (fun (var, infos) ->
        Cli.error_print "\t%s, %d errors in files %s@." (Pos.unmark var.Variable.name)
          (List.length infos)
          (String.concat ", " (List.map (fun (n, _, _) -> n) (List.sort compare infos))))
      f_l
  end
