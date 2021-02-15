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

let to_mvg_function_and_inputs (program : Bir.program) (t : test_file) (test_error_margin : float) :
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
           (* sometimes test outputs mention aliases so we have to catch thos two using the line
              below*)
           let var = Pos.unmark (find_var_of_name program.mir_program (var, pos)).Variable.name in
           (* we allow a difference of 0.000001 between the control value and the result *)
           let first_exp =
             ( Mast.Comparison
                 ( (Lte, pos),
                   ( Mast.Binop
                       ( (Mast.Sub, pos),
                         (Literal (Variable (Normal var)), pos),
                         (Literal (to_ast_literal value), pos) ),
                     pos ),
                   (Literal (Float test_error_margin), pos) ),
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
                   (Literal (Float test_error_margin), pos) ),
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
    (code_coverage : bool) (value_sort : Bir_interpreter.value_sort) (test_error_margin : float) :
    Bir_instrumentation.code_coverage_result =
  Cli.debug_print "Parsing %s..." test_name;
  let t = parse_file test_name in
  Cli.debug_print "Running test %s..." t.nom;
  let f, input_file = to_mvg_function_and_inputs combined_program t test_error_margin in
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
  let _print_outputs =
    Bir_interpreter.evaluate_program f combined_program input_file (-code_loc_offset) value_sort
  in
  if code_coverage then Bir_instrumentation.code_coverage_result ()
  else Bir_instrumentation.empty_code_coverage_result

type test_failures = (string * Mir.literal * Mir.literal) list Mir.VariableMap.t

type process_acc = string list * test_failures * Bir_instrumentation.code_coverage_acc

type coverage_kind = NotCovered | Covered of int  (** The int is the number of different values *)

module IntMap = Map.Make (Int)

let incr_int_key (m : int IntMap.t) (key : int) : int IntMap.t =
  match IntMap.find_opt key m with None -> IntMap.add key 0 m | Some i -> IntMap.add key (i + 1) m

let check_all_tests (p : Bir.program) (test_dir : string) (optimize : bool)
    (code_coverage_activated : bool) (value_sort : Bir_interpreter.value_sort)
    (test_error_margin : float) =
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
    let report_violated_condition_error (bindings : (Variable.t * Mir.literal) option)
        (expr : Mir.expression Pos.marked) (err : Error.t list) =
      Cli.debug_flag := true;
      match (bindings, Pos.unmark expr) with
      | ( Some (v, l1),
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
          (successes, VariableMap.add v ((name, l1, l2) :: errs_varname) failures, code_coverage_acc)
      | _ ->
          Cli.error_print "Test %s incorrect (error%s %a raised)" name
            (if List.length err > 1 then "s" else "")
            (Format.pp_print_list Format.pp_print_string)
            (List.map (fun x -> Pos.unmark x.Error.name) err);
          (successes, failures, code_coverage_acc)
    in
    try
      Cli.debug_flag := false;
      let code_coverage_result =
        check_test p (test_dir ^ name) optimize code_coverage_activated value_sort test_error_margin
      in
      Cli.debug_flag := true;
      let code_coverage_acc =
        Bir_instrumentation.merge_code_coverage_single_results_with_acc code_coverage_result
          code_coverage_acc
      in
      (name :: successes, failures, code_coverage_acc)
    with
    | Bir_interpreter.RegularFloatInterpreter.RuntimeError ((ConditionViolated _ as cv), _) ->
        let expr, err, bindings =
          match cv with
          | Bir_interpreter.RegularFloatInterpreter.ConditionViolated (err, expr, bindings) -> (
              ( expr,
                err,
                match bindings with
                | [ (v, Bir_interpreter.RegularFloatInterpreter.SimpleVar l1) ] ->
                    Some (v, Bir_interpreter.RegularFloatInterpreter.value_to_literal l1)
                | _ -> None ) )
          | _ -> assert false
          (* should not happen *)
        in
        report_violated_condition_error bindings expr err
    | Bir_interpreter.MPFRInterpreter.RuntimeError ((ConditionViolated _ as cv), _) ->
        let expr, err, bindings =
          match cv with
          | Bir_interpreter.MPFRInterpreter.ConditionViolated (err, expr, bindings) -> (
              ( expr,
                err,
                match bindings with
                | [ (v, Bir_interpreter.MPFRInterpreter.SimpleVar l1) ] ->
                    Some (v, Bir_interpreter.MPFRInterpreter.value_to_literal l1)
                | _ -> None ) )
          | _ -> assert false
          (* should not happen *)
        in
        report_violated_condition_error bindings expr err
    | Bir_interpreter.BigIntInterpreter.RuntimeError ((ConditionViolated _ as cv), _) ->
        let expr, err, bindings =
          match cv with
          | Bir_interpreter.BigIntInterpreter.ConditionViolated (err, expr, bindings) -> (
              ( expr,
                err,
                match bindings with
                | [ (v, Bir_interpreter.BigIntInterpreter.SimpleVar l1) ] ->
                    Some (v, Bir_interpreter.BigIntInterpreter.value_to_literal l1)
                | _ -> None ) )
          | _ -> assert false
          (* should not happen *)
        in
        report_violated_condition_error bindings expr err
    | Bir_interpreter.IntervalInterpreter.RuntimeError ((ConditionViolated _ as cv), _) ->
        let expr, err, bindings =
          match cv with
          | Bir_interpreter.IntervalInterpreter.ConditionViolated (err, expr, bindings) -> (
              ( expr,
                err,
                match bindings with
                | [ (v, Bir_interpreter.IntervalInterpreter.SimpleVar l1) ] ->
                    Some (v, Bir_interpreter.IntervalInterpreter.value_to_literal l1)
                | _ -> None ) )
          | _ -> assert false
          (* should not happen *)
        in
        report_violated_condition_error bindings expr err
    | Bir_interpreter.RationalInterpreter.RuntimeError ((ConditionViolated _ as cv), _) ->
        let expr, err, bindings =
          match cv with
          | Bir_interpreter.RationalInterpreter.ConditionViolated (err, expr, bindings) -> (
              ( expr,
                err,
                match bindings with
                | [ (v, Bir_interpreter.RationalInterpreter.SimpleVar l1) ] ->
                    Some (v, Bir_interpreter.RationalInterpreter.value_to_literal l1)
                | _ -> None ) )
          | _ -> assert false
          (* should not happen *)
        in
        report_violated_condition_error bindings expr err
    | Bir_interpreter.IntervalInterpreter.RuntimeError
        (Bir_interpreter.IntervalInterpreter.StructuredError (msg, pos, kont), _)
    | Bir_interpreter.BigIntInterpreter.RuntimeError
        (Bir_interpreter.BigIntInterpreter.StructuredError (msg, pos, kont), _)
    | Bir_interpreter.MPFRInterpreter.RuntimeError
        (Bir_interpreter.MPFRInterpreter.StructuredError (msg, pos, kont), _)
    | Bir_interpreter.RegularFloatInterpreter.RuntimeError
        (Bir_interpreter.RegularFloatInterpreter.StructuredError (msg, pos, kont), _)
    | Bir_interpreter.RationalInterpreter.RuntimeError
        (Bir_interpreter.RationalInterpreter.StructuredError (msg, pos, kont), _)
    | Errors.StructuredError (msg, pos, kont) ->
        Cli.error_print "Error in test %s: %a" name Errors.format_structured_error (msg, pos);
        (match kont with None -> () | Some kont -> kont ());
        (successes, failures, code_coverage_acc)
    | Bir_interpreter.IntervalInterpreter.RuntimeError (_, _)
    | Bir_interpreter.BigIntInterpreter.RuntimeError (_, _)
    | Bir_interpreter.MPFRInterpreter.RuntimeError (_, _)
    | Bir_interpreter.RegularFloatInterpreter.RuntimeError (_, _)
    | Bir_interpreter.RationalInterpreter.RuntimeError (_, _) ->
        Cli.error_print "Runtime error in test %s" name;
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
              | Some def -> Covered (Bir_instrumentation.VarLiteralSet.cardinal def) ))
        all_code_locs
    in
    let all_code_locs_num =
      Bir_instrumentation.CodeLocationMap.cardinal all_code_locs_with_coverage
    in
    let number_of_values_to_number_of_statements =
      Bir_instrumentation.CodeLocationMap.fold
        (fun _ cov number_of_values_to_number_of_statements ->
          match cov with
          | NotCovered -> incr_int_key number_of_values_to_number_of_statements 0
          | Covered i -> incr_int_key number_of_values_to_number_of_statements i)
        all_code_locs_with_coverage IntMap.empty
    in
    Cli.result_print "Here is the estimated code coverage of this set of test runs, broke down";
    Cli.result_print "by the number of values statements are covered with:";
    let number_of_values_to_number_of_statements =
      List.sort
        (fun x y -> compare (fst x) (fst y))
        (IntMap.bindings number_of_values_to_number_of_statements)
    in
    let number_of_values_to_number_of_statements =
      let rec build_list (i : int) (input : (int * int) list) =
        match input with
        | [] -> []
        | (i', n) :: tl ->
            if i' = i then (i', n) :: build_list (i + 1) tl else (i, 0) :: build_list (i + 1) input
      in
      build_list 0 number_of_values_to_number_of_statements
    in
    let number_zero, number_one, number_two_or_more =
      match number_of_values_to_number_of_statements with
      | (0, number_zero) :: (1, number_one) :: rest ->
          (number_zero, number_one, List.fold_left (fun acc (_, n) -> acc + n) 0 rest)
      | _ -> assert false
    in
    let number_of_values_to_number_of_statements =
      [ ("zero", number_zero); ("one", number_one); ("two or more", number_two_or_more) ]
    in
    List.iter
      (fun (number_of_values, number_of_statements) ->
        Cli.result_print "%s values → %d (%s of statements)"
          (ANSITerminal.sprintf [ ANSITerminal.blue ] "%s" number_of_values)
          number_of_statements
          (ANSITerminal.sprintf [ ANSITerminal.blue ] "%.4f%%"
             (float_of_int number_of_statements /. float_of_int all_code_locs_num *. 100.)))
      number_of_values_to_number_of_statements
  end
