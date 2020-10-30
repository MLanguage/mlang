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
  let input = open_in test_name in
  let filebuf = Lexing.from_channel input in
  let filebuf = { filebuf with lex_curr_p = { filebuf.lex_curr_p with pos_fname = test_name } } in
  let f =
    try Test_parser.test_file Test_lexer.token filebuf with
    | Errors.StructuredError e ->
        close_in input;
        raise (Errors.StructuredError e)
    | Test_parser.Error ->
        close_in input;
        Errors.raise_spanned_error "Test syntax error"
          (Parse_utils.mk_position (filebuf.lex_start_p, filebuf.lex_curr_p))
  in
  close_in input;
  f

let to_ast_literal (value : Test_ast.literal) : Mast.literal =
  match value with I i -> Float (float_of_int i) | F f -> Float f

let find_var_of_name (p : Mir.program) (name : string Pos.marked) : Variable.t =
  try
    List.hd
      (List.sort
         (fun v1 v2 -> compare v1.Mir.Variable.execution_number v2.Mir.Variable.execution_number)
         (Pos.VarNameToID.find (Pos.unmark name) p.program_idmap))
  with Not_found ->
    let name = find_var_name_by_alias p name in
    List.hd
      (List.sort
         (fun v1 v2 -> compare v1.Mir.Variable.execution_number v2.Mir.Variable.execution_number)
         (Pos.VarNameToID.find name p.program_idmap))

let to_mvg_function_and_inputs (program : Bir.program) (t : test_file) :
    Bir_interface.bir_function * Mir.literal VariableMap.t =
  let func_variable_inputs, input_file =
    List.fold_left
      (fun (fv, in_f) (var, value, pos) ->
        let var = find_var_of_name program.mir_program (var, pos) in
        let lit = match value with I i -> Float (float_of_int i) | F f -> Float f in
        (VariableMap.add var () fv, VariableMap.add var lit in_f))
      (VariableMap.empty, VariableMap.empty)
      t.ep
  in
  let func_constant_inputs = VariableMap.empty in
  let func_outputs = VariableMap.empty in
  (* some output variables are actually input, so we don't declare any for now *)
  let func_conds =
    Bir_interface.translate_cond program.idmap
      (List.map
         (fun (var, value, pos) ->
           (* we allow a difference of 0.000001 between the control value and the result *)
           let first_exp =
             ( Mast.Comparison
                 ( (Lte, pos),
                   ( Mast.Binop
                       ( (Mast.Sub, pos),
                         (Literal (Variable (Normal var)), pos),
                         (Literal (to_ast_literal value), pos) ),
                     pos ),
                   (Literal (Float 0.000001), pos) ),
               pos )
           in
           let second_exp =
             ( Mast.Comparison
                 ( (Lte, pos),
                   ( Mast.Binop
                       ( (Mast.Sub, pos),
                         (Literal (to_ast_literal value), pos),
                         (Literal (Variable (Normal var)), pos) ),
                     pos ),
                   (Literal (Float 0.000001), pos) ),
               pos )
           in
           (Mast.Binop ((Mast.And, pos), first_exp, second_exp), pos))
         t.rp)
  in
  ({ func_variable_inputs; func_constant_inputs; func_outputs; func_conds }, input_file)

let add_test_conds_to_combined_program (p : Bir.program) (conds : condition_data VariableMap.t) :
    Bir.program =
  (* because evaluate_program redefines everything each time, we have to make sure that the
     redefinitions of our constant inputs are removed from the main list of statements *)
  let new_stmts =
    List.filter_map
      (fun stmt ->
        match Pos.unmark stmt with
        | Bir.SAssign (var, var_data) -> (
            let new_var_data =
              {
                var_data with
                var_io = Regular;
                var_definition =
                  ( match var_data.var_definition with
                  | InputVar -> SimpleVar (Pos.same_pos_as (Literal Undefined) var.Variable.name)
                  | SimpleVar old -> SimpleVar old
                  | TableVar (size, old) -> TableVar (size, old) );
              }
            in
            match new_var_data.var_definition with
            | InputVar -> None
            | _ -> Some (Pos.same_pos_as (Bir.SAssign (var, new_var_data)) stmt) )
        | _ -> Some stmt)
      p.Bir.statements
  in
  let conditions_stmts =
    VariableMap.fold
      (fun _ cond stmts -> (Bir.SVerif cond, Pos.get_position cond.cond_expr) :: stmts)
      conds []
  in
  { p with Bir.statements = new_stmts @ conditions_stmts }

let check_test (combined_program : Bir.program) (test_name : string) (optimize : bool)
    (code_coverage : bool) (value_sort : Bir_interpreter.value_sort) :
    Bir_instrumentation.code_coverage_result =
  Cli.debug_print "Parsing %s..." test_name;
  let t = parse_file test_name in
  Cli.debug_print "Running test %s..." t.nom;
  let f, input_file = to_mvg_function_and_inputs combined_program t in
  Cli.debug_print "Executing program";
  let combined_program, code_loc_offset =
    Bir_interface.adapt_program_to_function combined_program f
  in
  let combined_program = add_test_conds_to_combined_program combined_program f.func_conds in
  (* Cli.debug_print "Combined Program (w/o verif conds):@.%a@." Format_bir.format_program
     combined_program; *)
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
  if code_coverage then Bir_instrumentation.code_coverage_init ();
  ignore
    (Bir_interpreter.evaluate_program combined_program
       (Bir_interpreter.update_ctx_with_inputs Bir_interpreter.empty_vanilla_ctx input_file)
       (-code_loc_offset) value_sort);
  if code_coverage then Bir_instrumentation.code_coverage_result ()
  else Bir_instrumentation.empty_code_coverage_result

type test_failures = (string * Mir.literal * Mir.literal) list Mir.VariableMap.t

type process_acc = string list * test_failures * Bir_instrumentation.code_coverage_acc

type coverage_kind =
  | NotCovered
  | Covered
  | CoveredOneDef of Bir_interpreter.var_literal
  | CoveredOneDefAndUndefined of Bir_interpreter.var_literal

let check_all_tests (p : Bir.program) (test_dir : string) (optimize : bool)
    (code_coverage_activated : bool) (value_sort : Bir_interpreter.value_sort) =
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
  let process (name : string) ((successes, failures, code_coverage_acc) : process_acc) : process_acc
      =
    try
      Cli.debug_flag := false;
      let code_coverage_result =
        check_test p (test_dir ^ name) optimize code_coverage_activated value_sort
      in
      Cli.debug_flag := true;
      let code_coverage_acc =
        Bir_instrumentation.merge_code_coverage_single_results_with_acc code_coverage_result
          code_coverage_acc
      in
      (name :: successes, failures, code_coverage_acc)
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
            Cli.error_print "Test %s incorrect (error on variable %s)" name
              (Pos.unmark v.Variable.name);
            let errs_varname = try VariableMap.find v failures with Not_found -> [] in
            ( successes,
              VariableMap.add v ((name, l1, l2) :: errs_varname) failures,
              code_coverage_acc )
        | _ ->
            Cli.error_print "Test %s incorrect (error%s %a raised)" name
              (if List.length err > 1 then "s" else "")
              (Format.pp_print_list Format.pp_print_string)
              (List.map (fun x -> Pos.unmark x.Error.name) err);
            (successes, failures, code_coverage_acc) )
    | Errors.StructuredError (msg, pos, kont) ->
        Cli.error_print "Error in test %s: %a" name Errors.format_structured_error (msg, pos);
        (match kont with None -> () | Some kont -> kont ());
        (successes, failures, code_coverage_acc)
  in
  let s, f, code_coverage =
    Parmap.parfold ~chunksize:5 process (Parmap.A arr) ([], VariableMap.empty, VariableMap.empty)
      (fun (old_s, old_f, old_code_coverage) (new_s, new_f, new_code_coverage) ->
        ( new_s @ old_s,
          VariableMap.union (fun _ x1 x2 -> Some (x1 @ x2)) old_f new_f,
          Bir_instrumentation.merge_code_coverage_acc old_code_coverage new_code_coverage ))
  in
  finish "done!";
  Cli.warning_flag := true;
  Cli.display_time := true;
  Cli.result_print "Test results: %d successes" (List.length s);

  let f_l =
    List.sort
      (fun (_, i) (_, i') -> -compare (List.length i) (List.length i'))
      (VariableMap.bindings f)
  in
  if List.length f_l = 0 then Cli.result_print "No failures!"
  else begin
    Cli.warning_print "Failures:";
    List.iter
      (fun (var, infos) ->
        Cli.error_print "\t%s, %d errors in files %s" (Pos.unmark var.Variable.name)
          (List.length infos)
          (String.concat ", " (List.map (fun (n, _, _) -> n) (List.sort compare infos))))
      f_l
  end;
  if code_coverage_activated then begin
    let all_code_locs = Bir_instrumentation.get_code_locs p in
    let all_code_locs_with_coverage =
      Bir_instrumentation.CodeLocationMap.mapi
        (fun code_loc var ->
          match Mir.VariableMap.find_opt var code_coverage with
          | None -> NotCovered
          | Some used_code_locs -> (
              match Bir_instrumentation.CodeLocationMap.find_opt code_loc used_code_locs with
              | None -> NotCovered
              | Some def -> (
                  match def with
                  | Bir_instrumentation.OnlyOneDef def -> CoveredOneDef def
                  | Bir_instrumentation.OnlyOneDefAndUndefined def -> CoveredOneDefAndUndefined def
                  | Bir_instrumentation.MultipleDefs -> Covered ) ))
        all_code_locs
    in
    let all_code_locs_num =
      Bir_instrumentation.CodeLocationMap.cardinal all_code_locs_with_coverage
    in
    let not_covered, one_value, one_value_or_undefined, covered =
      Bir_instrumentation.CodeLocationMap.fold
        (fun _ cov (not_covered, one_value, one_value_or_undefined, covered) ->
          match cov with
          | NotCovered -> (not_covered + 1, one_value, one_value_or_undefined, covered)
          | CoveredOneDef _ -> (not_covered, one_value + 1, one_value_or_undefined, covered)
          | CoveredOneDefAndUndefined _ ->
              (not_covered, one_value, one_value_or_undefined + 1, covered)
          | Covered -> (not_covered, one_value, one_value_or_undefined, covered + 1))
        all_code_locs_with_coverage (0, 0, 0, 0)
    in
    Cli.warning_print "Some code locations are not covered properly by this set of test runs.";
    Cli.warning_print "The estimated code coverage is:";
    Cli.warning_print "-> assigmnents never covered: %.3f%%"
      (float_of_int not_covered /. float_of_int all_code_locs_num *. 100.);
    Cli.warning_print "-> assigmnents covered by only one value (possibly undefined): %.3f%%"
      (float_of_int one_value /. float_of_int all_code_locs_num *. 100.);
    Cli.warning_print "-> assigmnents covered by only one value different from undefined: %.3f%%"
      (float_of_int one_value_or_undefined /. float_of_int all_code_locs_num *. 100.);
    Cli.warning_print
      "-> assigmnents covered by only two or more values different from undefined: %.3f%%"
      (float_of_int covered /. float_of_int all_code_locs_num *. 100.)
  end
