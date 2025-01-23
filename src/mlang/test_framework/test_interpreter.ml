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

let find_var_of_name (p : Mir.program) (name : string Pos.marked) : Com.Var.t =
  try StrMap.find (Pos.unmark name) p.program_vars
  with Not_found ->
    let name = Mir.find_var_name_by_alias p name in
    StrMap.find name p.program_vars

let to_MIR_function_and_inputs (program : Mir.program) (t : Irj_ast.irj_file) :
    (Com.literal Com.Var.Map.t * float StrMap.t * StrSet.t)
    * ((Com.literal, Com.Var.t) Com.event_value IntMap.t list
      * float StrMap.t
      * StrSet.t)
      option =
  let inputVars =
    let ancsded = find_var_of_name program ("V_ANCSDED", Pos.no_pos) in
    let ancsded_val = Com.Float (float_of_int (!Cli.income_year + 1)) in
    List.fold_left
      (fun in_f ((var, var_pos), (value, _value_pos)) ->
        let var = find_var_of_name program (var, var_pos) in
        let lit =
          match value with
          | Irj_ast.I i -> Com.Float (float i)
          | F f -> Com.Float f
        in
        Com.Var.Map.add var lit in_f)
      (Com.Var.Map.one ancsded ancsded_val)
      t.prim.entrees
  in
  let eventsList rappels =
    let from_var vn =
      let name =
        match StrMap.find_opt vn program.program_alias with
        | Some m_name -> Pos.unmark m_name
        | None -> vn
      in
      match StrMap.find_opt name program.program_vars with
      | Some var -> var
      | None ->
          Cli.error_print "Variable inconnue: %s" vn;
          raise (Errors.StructuredError ("Fichier de test incorrect", [], None))
    in
    let fromDirection = function
      | "R" -> Com.Float 0.0
      | "C" -> Com.Float 1.0
      | "M" -> Com.Float 2.0
      | "P" -> Com.Float 3.0
      | s ->
          Cli.error_print "Sens du rappel: %s, devrait être parmi R, C, M et P"
            s;
          raise (Errors.StructuredError ("Fichier de test incorrect", [], None))
    in
    let fromPenalty = function
      | None -> Com.Float 0.0 (* None *)
      | Some p when 0 <= p && p <= 99 -> Com.Float (float p)
      | Some p ->
          Cli.error_print "Code de pénalité: %d, devrait être entre 0 et 99" p;
          raise (Errors.StructuredError ("Fichier de test incorrect", [], None))
    in
    let fromBaseTl = function
      | Some f -> Com.Float (float f)
      | None -> Com.Undefined
    in
    let from_2042_rect = function
      | None -> Com.Float 0.0 (* None *)
      | Some 0 -> Com.Float 0.0
      | Some 1 -> Com.Float 1.0
      | Some r ->
          Cli.error_print
            "Indicateur de déclaration rectificative: %d, devrait être 0 ou 1" r;
          raise (Errors.StructuredError ("Fichier de test incorrect", [], None))
    in
    let toEvent (rappel : Irj_ast.rappel) =
      IntMap.empty
      |> IntMap.add 0 (Com.Numeric (Com.Float (float rappel.event_nb)))
      |> IntMap.add 1 (Com.Numeric (Com.Float (float rappel.rappel_nb)))
      |> IntMap.add 2 (Com.RefVar (from_var rappel.variable_code))
      |> IntMap.add 3 (Com.Numeric (Com.Float (float rappel.change_value)))
      |> IntMap.add 4 (Com.Numeric (fromDirection rappel.direction))
      |> IntMap.add 5 (Com.Numeric (fromPenalty rappel.penalty_code))
      |> IntMap.add 6 (Com.Numeric (fromBaseTl rappel.base_tolerance_legale))
      |> IntMap.add 7 (Com.Numeric (Com.Float (float rappel.month_year)))
      |> IntMap.add 8 (Com.Numeric (from_2042_rect rappel.decl_2042_rect))
    in
    List.map toEvent rappels
  in
  let expectedVars vars_init =
    let fold res ((var, _), (value, _)) =
      let fVal = match value with Irj_ast.I i -> float i | Irj_ast.F f -> f in
      StrMap.add var fVal res
    in
    List.fold_left fold StrMap.empty vars_init
  in
  let expectedAnos anos_init =
    let fold res ano = StrSet.add ano res in
    List.fold_left fold StrSet.empty (List.map fst anos_init)
  in
  let prim =
    ( inputVars,
      expectedVars t.prim.resultats_attendus,
      expectedAnos t.prim.controles_attendus )
  in
  let corr =
    match t.rapp with
    | None -> None
    | Some rapp ->
        Some
          ( eventsList rapp.entrees_rappels,
            expectedVars rapp.resultats_attendus,
            expectedAnos rapp.controles_attendus )
  in
  (prim, corr)

exception InterpError of int

let check_test (program : Mir.program) (test_name : string)
    (value_sort : Cli.value_sort) (round_ops : Cli.round_ops) : unit =
  Cli.debug_print "Parsing %s..." test_name;
  let t = Irj_file.parse_file test_name in
  Cli.debug_print "Running test %s..." t.nom;
  let (inputVars, expVars, expAnos), evtDatas =
    to_MIR_function_and_inputs program t
  in
  let events =
    match evtDatas with None -> [] | Some (events, _, _) -> events
  in
  Cli.debug_print "Executing program";
  (* Cli.debug_print "Combined Program (w/o verif conds):@.%a@."
     Format_bir.format_program program; *)
  let varMap, anoSet =
    Mir_interpreter.evaluate_program program inputVars events value_sort
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
  if nbErrs > 0 then raise (InterpError nbErrs)

type process_acc = string list * int StrMap.t

let check_all_tests (p : Mir.program) (test_dir : string)
    (value_sort : Cli.value_sort) (round_ops : Cli.round_ops)
    (filter_function : string -> bool) =
  let arr = Sys.readdir test_dir in
  let arr =
    Array.of_list
    @@ List.filter filter_function
    @@ List.filter
         (fun x -> not @@ Sys.is_directory (test_dir ^ "/" ^ x))
         (Array.to_list arr)
  in
  Mir_interpreter.exit_on_rte := false;
  (* sort by increasing size, hoping that small files = simple tests *)
  Array.sort compare arr;
  Cli.warning_flag := false;
  Cli.display_time := false;
  (* let _, finish = Cli.create_progress_bar "Testing files" in*)
  let process (name : string) ((successes, failures) : process_acc) :
      process_acc =
    let module Interp = (val Mir_interpreter.get_interp value_sort round_ops
                           : Mir_interpreter.S)
    in
    try
      Cli.debug_flag := false;
      check_test p (test_dir ^ name) value_sort round_ops;
      Cli.debug_flag := true;
      Cli.result_print "%s" name;
      (name :: successes, failures)
    with
    | InterpError nbErr -> (successes, StrMap.add name nbErr failures)
    | Errors.StructuredError (msg, pos, kont) ->
        Cli.error_print "Error in test %s: %a" name
          Errors.format_structured_error (msg, pos);
        (match kont with None -> () | Some kont -> kont ());
        (successes, failures)
    | Interp.RuntimeError (run_error, _) -> (
        match run_error with
        | Interp.StructuredError (msg, pos, kont) ->
            Cli.error_print "Error in test %s: %a" name
              Errors.format_structured_error (msg, pos);
            (match kont with None -> () | Some kont -> kont ());
            (successes, failures)
        | Interp.NanOrInf (msg, (_, pos)) ->
            Cli.error_print "Runtime error in test %s: NanOrInf (%s, %a)" name
              msg Pos.format_position pos;
            (successes, failures))
    | e ->
        Cli.error_print "Uncatched exception: %s" (Printexc.to_string e);
        raise e
  in
  let s, f =
    Parmap.parfold ~chunksize:5 process (Parmap.A arr) ([], StrMap.empty)
      (fun (old_s, old_f) (new_s, new_f) ->
        (new_s @ old_s, StrMap.union (fun _ x1 x2 -> Some (x1 + x2)) old_f new_f))
  in
  (* finish "done!"; *)
  Cli.warning_flag := true;
  Cli.display_time := true;
  Cli.result_print "Test results: %d successes" (List.length s);

  if StrMap.cardinal f = 0 then Cli.result_print "No failures!"
  else (
    Cli.warning_print "Failures:";
    StrMap.iter
      (fun name nbErr -> Cli.error_print "\t%d errors in files %s" nbErr name)
      f)
