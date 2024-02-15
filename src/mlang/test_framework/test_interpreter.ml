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

let to_ast_literal (value : Irj_ast.literal) : Mast.literal =
  match value with I i -> Float (float_of_int i) | F f -> Float f

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
    Bir_interface.bir_function * Mir.literal Bir.VariableMap.t =
  let func_variable_inputs, input_file =
    let ancsded =
      find_var_of_name program.mir_program ("V_ANCSDED", Pos.no_pos)
      |> Bir.(var_from_mir default_tgv)
    in
    let ancsded_val =
      Mir.Float (float_of_int (Option.get !Cli.income_year + 1))
    in
    List.fold_left
      (fun (fv, in_f) (var, value, pos) ->
        let var =
          find_var_of_name program.mir_program (var, convert_pos pos)
          |> Bir.(var_from_mir default_tgv)
        in
        let lit =
          match value with
          | Irj_ast.I i -> Mir.Float (float_of_int i)
          | F f -> Float f
        in
        (Bir.VariableMap.add var () fv, Bir.VariableMap.add var lit in_f))
      ( Bir.VariableMap.singleton ancsded (),
        Bir.VariableMap.singleton ancsded ancsded_val )
      t.prim.entrees
  in
  let func_constant_inputs = Bir.VariableMap.empty in
  let func_outputs = Bir.VariableMap.empty in
  (* some output variables are actually input, so we don't declare any for
     now *)
  let func_conds =
    Bir_interface.translate_external_conditions
      program.mir_program.program_var_categories program.idmap
      (List.map
         (fun (var, value, pos) ->
           (* sometimes test outputs mention aliases so we have to catch thos
              two using the line below*)
           let var =
             Pos.unmark
               (find_var_of_name program.mir_program (var, convert_pos pos))
                 .Mir.Variable.name
           in
           (* we allow a difference of 0.000001 between the control value and
              the result *)
           let test_error_margin = 0.01 in
           let first_exp =
             ( Mast.Comparison
                 ( (Lt, convert_pos pos),
                   ( Mast.Binop
                       ( (Mast.Sub, convert_pos pos),
                         (Literal (Variable (Normal var)), convert_pos pos),
                         (Literal (to_ast_literal value), convert_pos pos) ),
                     convert_pos pos ),
                   (Literal (Float test_error_margin), convert_pos pos) ),
               convert_pos pos )
           in
           let second_exp =
             ( Mast.Comparison
                 ( (Lt, convert_pos pos),
                   ( Mast.Binop
                       ( (Mast.Sub, convert_pos pos),
                         (Literal (to_ast_literal value), convert_pos pos),
                         (Literal (Variable (Normal var)), convert_pos pos) ),
                     convert_pos pos ),
                   (Literal (Float test_error_margin), convert_pos pos) ),
               convert_pos pos )
           in
           ( Mast.Binop ((Mast.And, convert_pos pos), first_exp, second_exp),
             convert_pos pos ))
         t.prim.resultats_attendus)
  in
  let func_errors =
    List.sort_uniq compare (List.map fst t.prim.controles_attendus)
  in
  ( {
      func_variable_inputs;
      func_constant_inputs;
      func_outputs;
      func_conds;
      func_errors;
    },
    input_file )

let check_test (combined_program : Bir.program) (test_name : string)
    (optimize : bool) (code_coverage : bool) (value_sort : Cli.value_sort)
    (round_ops : Cli.round_ops) : Bir_instrumentation.code_coverage_result =
  Cli.debug_print "Parsing %s..." test_name;
  let t = Irj_file.parse_file test_name in
  Cli.debug_print "Running test %s..." t.nom;
  let f, input_file = to_MIR_function_and_inputs combined_program t in
  Cli.debug_print "Executing program";
  let combined_program, code_loc_offset =
    Bir_interface.adapt_program_to_function combined_program f
  in
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
  let _print_outputs, sorted_anos =
    Bir_interpreter.evaluate_program f combined_program input_file
      (-code_loc_offset) value_sort round_ops
  in
  let rec check_errors nbUnex nbMiss exp rais =
    match (exp, rais) with
    | ee :: el, (re, _) :: rl ->
        let ren = Pos.unmark re.Mir.name in
        if ee < ren then (
          Cli.error_print "Missing error: %s" ee;
          check_errors nbUnex (nbMiss + 1) el rais)
        else if ren < ee then (
          Cli.error_print "Unexpected error: %s" ren;
          check_errors (nbUnex + 1) nbMiss exp rl)
        else (
          Cli.debug_print "Raised error: %s" ee;
          check_errors nbUnex nbMiss el rl)
    | ee :: el, [] ->
        Cli.error_print "Missing error: %s" ee;
        check_errors nbUnex (nbMiss + 1) el []
    | [], (re, _) :: rl ->
        let ren = Pos.unmark re.Mir.name in
        Cli.error_print "Unexpected error: %s" ren;
        check_errors (nbUnex + 1) nbMiss [] rl
    | [], [] ->
        if nbUnex + nbMiss > 0 then
          let msg =
            if nbMiss = 0 then
              Format.sprintf "%d unexpected error%s" nbUnex
                (if nbUnex > 1 then "s" else "")
            else if nbUnex = 0 then
              Format.sprintf "%d missing error%s" nbMiss
                (if nbMiss > 1 then "s" else "")
            else
              Format.sprintf "%d unexpected error%s, %d missing error%s" nbUnex
                (if nbUnex > 1 then "s" else "")
                nbMiss
                (if nbMiss > 1 then "s" else "")
          in
          raise (Errors.StructuredError (msg, [], None))
  in
  check_errors 0 0 f.func_errors sorted_anos;
  if code_coverage then Bir_instrumentation.code_coverage_result ()
  else Bir_instrumentation.empty_code_coverage_result

type test_failures = (string * Mir.literal * Mir.literal) list Bir.VariableMap.t

type process_acc =
  string list * test_failures * Bir_instrumentation.code_coverage_acc

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
    let report_violated_condition_error
        (bindings : (Bir.variable * Mir.literal) option)
        (expr : Bir.expression Pos.marked) (err : Mir.Error.t) =
      Cli.debug_flag := true;
      match (bindings, Pos.unmark expr) with
      | ( Some (v, l1),
          Unop
            ( Mast.Not,
              ( Mir.Binop
                  ( (Mast.And, _),
                    ( Comparison
                        ( (Mast.Lt, _),
                          (Mir.Binop ((Mast.Sub, _), _, (Literal l2, _)), _),
                          (_, _) ),
                      _ ),
                    _ ),
                _ ) ) ) ->
          Cli.error_print "Test %s incorrect (error on variable %s: %a != %a)"
            name
            (Pos.unmark (Bir.var_to_mir v).Mir.Variable.name)
            Format_mir.format_literal l1 Format_mir.format_literal l2;
          let errs_varname =
            try Bir.VariableMap.find v failures with Not_found -> []
          in
          ( successes,
            Bir.VariableMap.add v ((name, l1, l2) :: errs_varname) failures,
            code_coverage_acc )
      | _ ->
          Cli.error_print "Test %s incorrect (error %s raised)" name
            (Pos.unmark err.Mir.Error.name);
          (successes, failures, code_coverage_acc)
    in
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
    | Errors.StructuredError (msg, pos, kont) ->
        Cli.error_print "Error in test %s: %a" name
          Errors.format_structured_error (msg, pos);
        (match kont with None -> () | Some kont -> kont ());
        ignore (successes, failures, code_coverage_acc);
        failwith "Stop"
    | Interp.RuntimeError (run_error, _) -> (
        match run_error with
        | Interp.ConditionViolated _ as cv ->
            let expr, err, bindings =
              match cv with
              | Interp.ConditionViolated (err, expr, bindings) -> (
                  ( expr,
                    err,
                    match bindings with
                    | [ (v, Interp.SimpleVar l1) ] ->
                        Some (v, Interp.value_to_literal l1)
                    | _ -> None ))
              | _ -> assert false
              (* should not happen *)
            in
            report_violated_condition_error bindings expr err
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
      ([], Bir.VariableMap.empty, Bir.VariableMap.empty)
      (fun (old_s, old_f, old_code_coverage) (new_s, new_f, new_code_coverage)
      ->
        ( new_s @ old_s,
          Bir.VariableMap.union (fun _ x1 x2 -> Some (x1 @ x2)) old_f new_f,
          Bir_instrumentation.merge_code_coverage_acc old_code_coverage
            new_code_coverage ))
  in
  finish "done!";
  Cli.warning_flag := true;
  Cli.display_time := true;
  Cli.result_print "Test results: %d successes" (List.length s);

  let f_l =
    List.sort
      (fun (_, i) (_, i') -> -compare (List.length i) (List.length i'))
      (Bir.VariableMap.bindings f)
  in
  if List.length f_l = 0 then Cli.result_print "No failures!"
  else begin
    Cli.warning_print "Failures:";
    List.iter
      (fun (var, infos) ->
        Cli.error_print "\t%s, %d errors in files %s"
          (Pos.unmark (Bir.var_to_mir var).Mir.Variable.name)
          (List.length infos)
          (String.concat ", "
             (List.map (fun (n, _, _) -> n) (List.sort compare infos))))
      f_l
  end;
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
