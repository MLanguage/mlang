(* Copyright Inria, contributors: Raphaël Monat <raphael.monat@lip6.fr> (2019)

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

open Irj_include

let convert_pos (pos : Irj_ast.pos) =
  Pos.make_position pos.pos_filename pos.pos_loc

(* enforces type compatibility (the type Irj_ast.pos is defined in exactly the
   same way as Pos.t) *)

let find_var_of_name (p : Mir.program) (name : string Pos.marked) :
    Mir.Variable.t =
  try
    List.hd
      (List.sort
         (fun v1 v2 ->
           compare v1.Mir.Variable.execution_number
             v2.Mir.Variable.execution_number)
         (Pos.VarNameToID.find (Pos.unmark name) p.program_idmap))
  with Not_found ->
    let name = Mir.find_var_name_by_alias p name in
    List.hd
      (List.sort
         (fun v1 v2 ->
           compare v1.Mir.Variable.execution_number
             v2.Mir.Variable.execution_number)
         (Pos.VarNameToID.find name p.program_idmap))

let to_MIR_function_and_inputs (program : Bir.program) (t : Irj_ast.irj_file) :
    float StrMap.t * StrSet.t * Mir.literal Bir.VariableMap.t =
  let input_file =
    let ancsded =
      find_var_of_name program.mir_program ("V_ANCSDED", Pos.no_pos)
      |> Bir.(var_from_mir default_tgv)
    in
    let ancsded_val =
      Mir.Float (float_of_int (Option.get !Cli.income_year + 1))
    in
    List.fold_left
      (fun in_f (var, value, pos) ->
        let var =
          find_var_of_name program.mir_program (var, convert_pos pos)
          |> Bir.(var_from_mir default_tgv)
        in
        let lit =
          match value with
          | Irj_ast.I i -> Mir.Float (float_of_int i)
          | F f -> Float f
        in
        Bir.VariableMap.add var lit in_f)
      (Bir.VariableMap.singleton ancsded ancsded_val)
      t.prim.entrees
  in
  let expectedVars =
    let fold res (var, value, _pos) =
      let fVal = match value with Irj_ast.I i -> float i | Irj_ast.F f -> f in
      StrMap.add var fVal res
    in
    List.fold_left fold StrMap.empty t.prim.resultats_attendus
  in
  let expectedAnos =
    let fold res ano = StrSet.add ano res in
    List.fold_left fold StrSet.empty (List.map fst t.prim.controles_attendus)
  in
  (expectedVars, expectedAnos, input_file)

exception InterpError of int

let check_test (combined_program : Bir.program) (test_name : string)
    (optimize : bool) (code_coverage : bool) (value_sort : Cli.value_sort)
    (round_ops : Cli.round_ops) : Bir_instrumentation.code_coverage_result =
  Cli.debug_print "Parsing %s..." test_name;
  let t = Irj_file.parse_file test_name in
  Cli.debug_print "Running test %s..." t.nom;
  let expVars, expAnos, input_file =
    to_MIR_function_and_inputs combined_program t
  in
  Cli.debug_print "Executing program";
  (* Cli.debug_print "Combined Program (w/o verif conds):@.%a@."
     Format_bir.format_program combined_program; *)
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
  let varMap, anoSet =
    Bir_interpreter.evaluate_program combined_program input_file value_sort
      round_ops
  in
  let check_vars exp vars =
    let test_error_margin = 0.01 in
    let fold var f nb =
      let f' =
        match StrMap.find_opt var vars with Some (Some f') -> f' | _ -> 0.0
      in
      if abs_float (f -. f') > test_error_margin then (
        Cli.error_print "KO | %s expected: %f - evaluated: %f" var f f';
        nb + 1)
      else nb
    in
    StrMap.fold fold exp 0
  in
  let check_anos exp rais =
    let missAnos = StrSet.diff exp rais in
    let unexAnos = StrSet.diff rais exp in
    StrSet.iter (Cli.error_print "KO | missing error: %s") missAnos;
    StrSet.iter (Cli.error_print "KO | unexpected error: %s") unexAnos;
    StrSet.cardinal missAnos + StrSet.cardinal unexAnos
  in
  let nbErrs = check_vars expVars varMap + check_anos expAnos anoSet in
  if nbErrs > 0 then raise (InterpError nbErrs);
  if code_coverage then Bir_instrumentation.code_coverage_result ()
  else Bir_instrumentation.empty_code_coverage_result

type process_acc =
  string list * int StrMap.t * Bir_instrumentation.code_coverage_acc

type coverage_kind =
  | NotCovered
  | Covered of int  (** The int is the number of different values *)

let incr_int_key (m : int IntMap.t) (key : int) : int IntMap.t =
  match IntMap.find_opt key m with
  | None -> IntMap.add key 0 m
  | Some i -> IntMap.add key (i + 1) m

let check_all_tests (p : Bir.program) (test_dir : string) (optimize : bool)
    (code_coverage_activated : bool) (value_sort : Cli.value_sort)
    (round_ops : Cli.round_ops) (filter_function : string -> bool) =
  let arr = Sys.readdir test_dir in
  let arr =
    Array.of_list
    @@ List.filter filter_function
    @@ List.filter
         (fun x -> not @@ Sys.is_directory (test_dir ^ "/" ^ x))
         (Array.to_list arr)
  in
  Bir_interpreter.exit_on_rte := false;
  (* sort by increasing size, hoping that small files = simple tests *)
  Array.sort compare arr;
  Cli.warning_flag := false;
  Cli.display_time := false;
  let _, finish = Cli.create_progress_bar "Testing files" in
  let process (name : string)
      ((successes, failures, code_coverage_acc) : process_acc) : process_acc =
    let module Interp = (val Bir_interpreter.get_interp value_sort round_ops
                           : Bir_interpreter.S)
    in
    try
      Cli.debug_flag := false;
      let code_coverage_result =
        check_test p (test_dir ^ name) optimize code_coverage_activated
          value_sort round_ops
      in
      Cli.debug_flag := true;
      let code_coverage_acc =
        Bir_instrumentation.merge_code_coverage_single_results_with_acc
          code_coverage_result code_coverage_acc
      in
      Cli.result_print "%s" name;
      (name :: successes, failures, code_coverage_acc)
    with
    | InterpError nbErr ->
        (successes, StrMap.add name nbErr failures, code_coverage_acc)
    | Errors.StructuredError (msg, pos, kont) ->
        Cli.error_print "Error in test %s: %a" name
          Errors.format_structured_error (msg, pos);
        (match kont with None -> () | Some kont -> kont ());
        (successes, failures, code_coverage_acc)
    | Interp.RuntimeError (run_error, _) -> (
        match run_error with
        | Interp.ConditionViolated (_err, (expr, pos), _bindings) ->
            let msg = Format.asprintf "%a" Format_bir.format_expression expr in
            Cli.error_print "Error in test %s: %a" name
              Errors.format_structured_error
              (msg, [ (None, pos) ]);
            (successes, failures, code_coverage_acc)
        | Interp.StructuredError (msg, pos, kont) ->
            Cli.error_print "Error in test %s: %a" name
              Errors.format_structured_error (msg, pos);
            (match kont with None -> () | Some kont -> kont ());
            (successes, failures, code_coverage_acc)
        | Interp.ErrorValue (msg, pos) ->
            Cli.error_print "Runtime error in test %s: ErrorValue (%s, %a)" name
              msg Pos.format_position pos;
            (successes, failures, code_coverage_acc)
        | Interp.FloatIndex (msg, pos) ->
            Cli.error_print "Runtime error in test %s: FloatIndex (%s, %a)" name
              msg Pos.format_position pos;
            (successes, failures, code_coverage_acc)
        | Interp.IndexOutOfBounds (msg, pos) ->
            Cli.error_print
              "Runtime error in test %s: IndexOutOfBounds (%s, %a)" name msg
              Pos.format_position pos;
            (successes, failures, code_coverage_acc)
        | Interp.IncorrectOutputVariable (msg, pos) ->
            Cli.error_print
              "Runtime error in test %s: IncorrectOutputVariable (%s, %a)" name
              msg Pos.format_position pos;
            (successes, failures, code_coverage_acc)
        | Interp.UnknownInputVariable (msg, pos) ->
            Cli.error_print
              "Runtime error in test %s: UnknownInputVariable (%s, %a)" name msg
              Pos.format_position pos;
            (successes, failures, code_coverage_acc)
        | Interp.NanOrInf (msg, (_, pos)) ->
            Cli.error_print "Runtime error in test %s: NanOrInf (%s, %a)" name
              msg Pos.format_position pos;
            (successes, failures, code_coverage_acc)
        | Interp.RaisedError (mir_err, _so, _pos) ->
            Cli.error_print "Runtime error in test %s: %s)" name
              (Pos.unmark (Mir.Error.err_descr_string mir_err));
            (successes, failures, code_coverage_acc))
    | Irj_include.Irj_ast.TestParsingError (msg, pos) as e ->
        Cli.error_print "Parsing error: %s %a" msg Pos.format_position
          (convert_pos pos);
        raise e
    | e ->
        Cli.error_print "Uncatched exception: %s" (Printexc.to_string e);
        raise e
  in
  let s, f, code_coverage =
    Parmap.parfold ~chunksize:5 process (Parmap.A arr)
      ([], StrMap.empty, Bir.VariableMap.empty)
      (fun (old_s, old_f, old_code_coverage) (new_s, new_f, new_code_coverage)
      ->
        ( new_s @ old_s,
          StrMap.union (fun _ x1 x2 -> Some (x1 + x2)) old_f new_f,
          Bir_instrumentation.merge_code_coverage_acc old_code_coverage
            new_code_coverage ))
  in
  finish "done!";
  Cli.warning_flag := true;
  Cli.display_time := true;
  Cli.result_print "Test results: %d successes" (List.length s);

  if StrMap.cardinal f = 0 then Cli.result_print "No failures!"
  else (
    Cli.warning_print "Failures:";
    StrMap.iter
      (fun name nbErr -> Cli.error_print "\t%d errors in files %s" nbErr name)
      f);
  if code_coverage_activated then begin
    let all_code_locs = Bir_instrumentation.get_code_locs p in
    let all_code_locs_with_coverage =
      Bir_instrumentation.CodeLocationMap.mapi
        (fun code_loc var ->
          match Bir.VariableMap.find_opt var code_coverage with
          | None -> NotCovered
          | Some used_code_locs -> (
              match
                Bir_instrumentation.CodeLocationMap.find_opt code_loc
                  used_code_locs
              with
              | None -> NotCovered
              | Some def ->
                  Covered (Bir_instrumentation.VarLiteralSet.cardinal def)))
        all_code_locs
    in
    let all_code_locs_num =
      Bir_instrumentation.CodeLocationMap.cardinal all_code_locs_with_coverage
    in
    let number_of_values_to_number_of_statements =
      Bir_instrumentation.CodeLocationMap.fold
        (fun _ cov number_of_values_to_number_of_statements ->
          match cov with
          | NotCovered ->
              incr_int_key number_of_values_to_number_of_statements 0
          | Covered i -> incr_int_key number_of_values_to_number_of_statements i)
        all_code_locs_with_coverage IntMap.empty
    in
    Cli.result_print
      "Here is the estimated code coverage of this set of test runs, broke down";
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
            if i' = i then (i', n) :: build_list (i + 1) tl
            else (i, 0) :: build_list (i + 1) input
      in
      build_list 0 number_of_values_to_number_of_statements
    in
    let number_zero, number_one, number_two_or_more =
      match number_of_values_to_number_of_statements with
      | (0, number_zero) :: (1, number_one) :: rest ->
          ( number_zero,
            number_one,
            List.fold_left (fun acc (_, n) -> acc + n) 0 rest )
      | _ -> assert false
    in
    let number_of_values_to_number_of_statements =
      [
        ("zero", number_zero);
        ("one", number_one);
        ("two or more", number_two_or_more);
      ]
    in
    List.iter
      (fun (number_of_values, number_of_statements) ->
        Cli.result_print "%s values → %d (%s of statements)"
          (ANSITerminal.sprintf [ ANSITerminal.blue ] "%s" number_of_values)
          number_of_statements
          (ANSITerminal.sprintf [ ANSITerminal.blue ] "%.4f%%"
             (float_of_int number_of_statements
             /. float_of_int all_code_locs_num
             *. 100.)))
      number_of_values_to_number_of_statements
  end
