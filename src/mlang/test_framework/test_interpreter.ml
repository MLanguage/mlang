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
  with Not_found -> (
    let name = Mir.find_var_name_by_alias p name in
    try StrMap.find name p.program_vars
    with Not_found ->
      Cli.error_print "Variable inconnue: %s" name;
      raise (Errors.StructuredError ("Fichier de test incorrect", [], None)))

type instance = {
  label : string;
  vars : Com.literal Com.Var.Map.t;
  events : (Com.literal, Com.Var.t) Com.event_value StrMap.t list;
  expectedVars : float StrMap.t;
  expectedAnos : StrSet.t;
}

let to_MIR_function_and_inputs (program : Mir.program) (t : Irj_ast.irj_file) :
    instance list =
  let add_var name value map =
    try
      let var = find_var_of_name program (Pos.without name) in
      Com.Var.Map.add var (Com.Float value) map
    with _ -> map
  in
  let vars =
    let map_init =
      Com.Var.Map.empty
      |> add_var "V_ANCSDED" (float (!Config.income_year + 1))
      |> add_var "V_MILLESIME" (float !Config.income_year)
    in
    List.fold_left
      (fun in_f (Pos.Mark (var, _var_pos), Pos.Mark (value, _value_pos)) ->
        add_var var (match value with Irj_ast.I i -> float i | F f -> f) in_f)
      map_init t.prim.entrees
  in
  let eventsList rappels =
    let from_var vn =
      match StrMap.find_opt vn program.program_alias with
      | Some var -> Com.RefVar var
      | None -> (
          match StrMap.find_opt vn program.program_vars with
          | Some var -> Com.RefVar var
          | None ->
              Cli.error_print "Variable inconnue: %s" vn;
              let msg = "Fichier de test incorrect" in
              raise (Errors.StructuredError (msg, [], None)))
    in
    let fromDirection = function
      | "R" -> Com.Numeric (Com.Float 0.0)
      | "M" -> Com.Numeric (Com.Float 1.0)
      | "P" -> Com.Numeric (Com.Float 2.0)
      | "C" -> Com.Numeric (Com.Float 3.0)
      | s ->
          Cli.error_print "Sens du rappel: %s, devrait être parmi R, C, M et P"
            s;
          raise (Errors.StructuredError ("Fichier de test incorrect", [], None))
    in
    let toNum p = Com.Numeric (Com.Float (float p)) in
    let optToNum op = toNum (Option.value ~default:0 op) in
    let toEvent (rappel : Irj_ast.rappel) =
      StrMap.empty
      |> StrMap.add "numero" (toNum rappel.event_nb)
      |> StrMap.add "rappel" (toNum rappel.rappel_nb)
      |> StrMap.add "code" (from_var rappel.variable_code)
      |> StrMap.add "montant" (toNum rappel.change_value)
      |> StrMap.add "sens" (fromDirection rappel.direction)
      |> StrMap.add "penalite" (optToNum rappel.penalty_code)
      |> StrMap.add "base_tl" (optToNum rappel.base_tolerance_legale)
      |> StrMap.add "date" (toNum rappel.month_year)
      |> StrMap.add "2042_rect" (optToNum rappel.decl_2042_rect)
      |> StrMap.add "anc_penalite" (optToNum None)
      |> StrMap.add "id_evt" (optToNum None)
      |> StrMap.add "strate" (optToNum None)
    in
    List.map toEvent rappels
  in
  let expVars vars_init =
    let fold res (Pos.Mark (var, _), Pos.Mark (value, _)) =
      let fVal = match value with Irj_ast.I i -> float i | Irj_ast.F f -> f in
      StrMap.add var fVal res
    in
    List.fold_left fold StrMap.empty vars_init
  in
  let expAnos anos_init =
    let fold res ano = StrSet.add ano res in
    List.fold_left fold StrSet.empty (List.map Pos.unmark anos_init)
  in
  match t.rapp with
  | None ->
      let expectedVars = expVars t.prim.resultats_attendus in
      let expectedAnos = expAnos t.prim.controles_attendus in
      [ { label = "primitif"; vars; events = []; expectedVars; expectedAnos } ]
  | Some rapp ->
      let vars = add_var "MODE_CORR" 1.0 vars in
      let events = eventsList rapp.entrees_rappels in
      let expectedVars = expVars rapp.resultats_attendus in
      let expectedAnos = expAnos rapp.controles_attendus in
      [ { label = "correctif"; vars; events; expectedVars; expectedAnos } ]

exception InterpError of int

let check_test (program : Mir.program) (test_name : string)
    (value_sort : Config.value_sort) (round_ops : Config.round_ops)
    (ign_vars : StrSet.t) : unit =
  let check_vars exp vars =
    let test_error_margin = 0.01 in
    let fold vname f nb =
      if StrSet.mem vname ign_vars then (
        Cli.warning_print "OK | %s ignoree" vname;
        nb)
      else
        match StrMap.find_opt vname program.program_vars with
        | Some var ->
            if Com.Var.is_tgv var then
              if Com.Var.is_given_back var then
                let f' =
                  match Com.Var.Map.find_opt var vars with
                  | Some (Com.Float f') -> f'
                  | _ -> 0.0
                in
                if abs_float (f -. f') > test_error_margin then (
                  Cli.error_print "KO | %s attendue: %f - evaluee: %f" vname f
                    f';
                  nb + 1)
                else nb
              else (
                Cli.warning_print "OK | %s ignoree car non-restituee" vname;
                nb)
            else (
              Cli.warning_print "Variable inconnue dans le TGV: %s" vname;
              nb)
        | None ->
            Cli.warning_print "Variable inconnue: %s" vname;
            nb
    in
    StrMap.fold fold exp 0
  in
  let check_anos exp errSet =
    let rais =
      let fold e res = StrSet.add (Pos.unmark e.Com.Error.name) res in
      Com.Error.Set.fold fold errSet StrSet.empty
    in
    let missAnos = StrSet.diff exp rais in
    let unexAnos = StrSet.diff rais exp in
    StrSet.iter (Cli.error_print "KO | erreur manquante: %s") missAnos;
    StrSet.iter (Cli.error_print "KO | erreur inattendue: %s") unexAnos;
    StrSet.cardinal missAnos + StrSet.cardinal unexAnos
  in
  let dbg_warning = !Config.warning_flag in
  let dbg_time = !Config.display_time in
  Config.warning_flag := false;
  Config.display_time := false;
  Cli.debug_print "Parsing %s..." test_name;
  let t = Irj_file.parse_file test_name in
  Cli.debug_print "Running test %s..." t.nom;
  let insts = to_MIR_function_and_inputs program t in
  let rec check = function
    | [] -> ()
    | inst :: insts ->
        Cli.debug_print "Executing program %s" inst.label;
        (* Cli.debug_print "Combined Program (w/o verif conds):@.%a@."
           Format_bir.format_program program; *)
        let varMap, anoSet =
          Mir_interpreter.evaluate_program program inst.vars inst.events
            value_sort round_ops
        in
        let nbErrs =
          check_vars inst.expectedVars varMap
          + check_anos inst.expectedAnos anoSet
        in
        if nbErrs <= 0 then (
          Cli.debug_print "OK!";
          check insts)
        else (
          Cli.debug_print "KO!";
          raise (InterpError nbErrs))
  in
  check insts;
  Config.warning_flag := dbg_warning;
  Config.display_time := dbg_time

let ignored_vars_set (p : Mir.program) (sl : string list) =
  let str_list = [ "^\\("; String.concat "\\|" sl; "\\)$" ] in
  let regexp = Str.regexp @@ String.concat "" str_list in
  let fold vn _ set =
    if Str.string_match regexp vn 0 then StrSet.add vn set else set
  in
  StrSet.empty
  |> StrMap.fold fold p.program_vars
  |> StrMap.fold fold p.program_alias

let ignored_vars_list = [ "NBPT"; "RETX.*"; "NATMAJ.*"; "TL_.*" ]

type process_acc = string list * int StrMap.t

let check_all_tests (p : Mir.program) (test_dir : string)
    (value_sort : Config.value_sort) (round_ops : Config.round_ops)
    (filter_function : string -> bool) =
  let arr =
    test_dir |> Sys.readdir |> Array.to_list
    |> List.filter (fun x -> not @@ Sys.is_directory (test_dir ^ "/" ^ x))
    |> List.filter filter_function
    |> List.sort String.compare |> Array.of_list
  in
  let ign_vars = ignored_vars_set p ignored_vars_list in
  Mir_interpreter.exit_on_rte := false;
  let dbg_warning = !Config.warning_flag in
  let dbg_time = !Config.display_time in
  Config.warning_flag := false;
  Config.display_time := false;
  (* let _, finish = Config.create_progress_bar "Testing files" in*)
  let process (name : string) ((successes, failures) : process_acc) :
      process_acc =
    let module Interp = (val Mir_interpreter.get_interp value_sort round_ops
                           : Mir_interpreter.S)
    in
    try
      Config.debug_flag := false;
      check_test p (test_dir ^ name) value_sort round_ops ign_vars;
      Config.debug_flag := true;
      Cli.result_print "%s" name;
      (name :: successes, failures)
    with
    | InterpError nbErr ->
        Cli.error_print "%s" name;
        (successes, StrMap.add name nbErr failures)
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
        | Interp.NanOrInf (msg, Pos.Mark (_, pos)) ->
            Cli.error_print "Runtime error in test %s: NanOrInf (%s, %a)" name
              msg Pos.format pos;
            (successes, failures))
    | e ->
        Cli.error_print "Uncatched exception: %s" (Printexc.to_string e);
        raise e
  in
  let s, f =
    Parmap.parfold ~chunksize:5 process (Parmap.A arr) ([], StrMap.empty)
      (fun (old_s, old_f) (new_s, new_f) ->
        (new_s @ old_s, StrMap.union (fun _ x1 x2 -> Some (x1 + x2)) old_f new_f))
    (*
    Array.fold_left (fun acc name -> process name acc) ([], StrMap.empty) arr
    *)
  in
  (* finish "done!"; *)
  Config.warning_flag := dbg_warning;
  Config.display_time := dbg_time;
  Cli.result_print "Test results: %d successes" (List.length s);
  if StrMap.cardinal f = 0 then Cli.result_print "No failures!"
  else (
    Cli.warning_print "Failures:";
    StrMap.iter
      (fun name nbErr -> Cli.error_print "\t%d errors in files %s" nbErr name)
      f)

let check_one_test (p : Mir.program) (name : string)
    (value_sort : Config.value_sort) (round_ops : Config.round_ops) =
  let ign_vars = ignored_vars_set p ignored_vars_list in
  Mir_interpreter.exit_on_rte := false;
  (* sort by increasing size, hoping that small files = simple tests *)
  let dbg_warning = !Config.warning_flag in
  let dbg_time = !Config.display_time in
  Config.warning_flag := false;
  Config.display_time := false;
  (* let _, finish = Config.create_progress_bar "Testing files" in*)
  let is_ok =
    let module Interp = (val Mir_interpreter.get_interp value_sort round_ops
                           : Mir_interpreter.S)
    in
    try
      Config.debug_flag := false;
      check_test p name value_sort round_ops ign_vars;
      Config.debug_flag := true;
      Cli.result_print "%s" name;
      None
    with
    | InterpError nbErr -> Some nbErr
    | Errors.StructuredError (msg, pos, kont) ->
        Cli.error_print "Error in test %s: %a" name
          Errors.format_structured_error (msg, pos);
        (match kont with None -> () | Some kont -> kont ());
        Some 0
    | Interp.RuntimeError (run_error, _) -> (
        match run_error with
        | Interp.StructuredError (msg, pos, kont) ->
            Cli.error_print "Error in test %s: %a" name
              Errors.format_structured_error (msg, pos);
            (match kont with None -> () | Some kont -> kont ());
            Some 0
        | Interp.NanOrInf (msg, Pos.Mark (_, pos)) ->
            Cli.error_print "Runtime error in test %s: NanOrInf (%s, %a)" name
              msg Pos.format pos;
            Some 0)
    | e ->
        Cli.error_print "Uncatched exception: %s" (Printexc.to_string e);
        raise e
  in
  (* finish "done!"; *)
  Config.warning_flag := dbg_warning;
  Config.display_time := dbg_time;
  match is_ok with
  | None -> Cli.result_print "No failure!"
  | Some 0 -> Cli.error_print "Unexpected failure"
  | Some nbErr -> Cli.error_print "Failure: %d errors in file %s" nbErr name
