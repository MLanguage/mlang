(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

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

open Backend_compilers
open Irj_utils
open Lexing
open M_ir
open M_frontend
open Mlexer

exception Exit

(* The legacy compiler plays a nasty trick on us, that we have to reproduce:
   rule 1 is modified to add assignments to APPLI_XXX variables according to the
   target application (OCEANS, BATCH and ILIAD). *)
let patch_rule_1 (backend : Config.backend) (dgfip_flags : Dgfip_options.flags)
    (program : Mast.program) : Mast.program =
  let open Mast in
  let var_exists name =
    List.exists
      (List.exists (fun m_item ->
           match Pos.unmark m_item with
           | VariableDecl (ComputedVar m_cv) ->
               Pos.unmark (Pos.unmark m_cv).comp_name = name
           | VariableDecl (InputVar m_iv) ->
               Pos.unmark (Pos.unmark m_iv).input_name = name
           | _ -> false))
      program
  in
  let mk_assign name value l =
    if var_exists name then
      let m_access =
        Pos.without (Com.VarAccess (None, Pos.without (Com.Normal name)))
      in
      let litt = Com.Literal (Com.Float (if value then 1.0 else 0.0)) in
      let cmd = Com.SingleFormula (VarDecl (m_access, Pos.without litt)) in
      Pos.without cmd :: l
    else l
  in
  let oceans, batch, iliad =
    match backend with
    | Dgfip_c ->
        (dgfip_flags.flg_cfir, dgfip_flags.flg_gcos, dgfip_flags.flg_iliad)
    | UnknownBackend -> (false, false, true)
  in
  List.map
    (List.map (fun m_item ->
         match Pos.unmark m_item with
         | Rule r when Pos.unmark r.rule_number = 1 ->
             let fl =
               List.map
                 (fun f -> Pos.same (Com.Affectation f) f)
                 ([]
                 |> mk_assign "APPLI_OCEANS" oceans
                 |> mk_assign "APPLI_BATCH" batch
                 |> mk_assign "APPLI_ILIAD" iliad)
             in
             let r' = { r with rule_formulaes = r.rule_formulaes @ fl } in
             Pos.same (Rule r') m_item
         | _ -> m_item))
    program

let parse () =
  let current_progress, finish = Cli.create_progress_bar "Parsing" in

  let parse filebuf source_file =
    current_progress source_file;
    let lex_curr_p = { filebuf.lex_curr_p with pos_fname = source_file } in
    let filebuf = { filebuf with lex_curr_p } in
    match Mparser.source_file token filebuf with
    | commands -> commands
    | exception Mparser.Error ->
        Errors.raise_spanned_error "M syntax error"
          (Parse_utils.mk_position (filebuf.lex_start_p, filebuf.lex_curr_p))
  in

  let parse_file source_file =
    let input = open_in source_file in
    let filebuf = Lexing.from_channel input in
    try
      parse filebuf source_file
      (* We're catching exceptions to properly close the input channel *)
    with Errors.StructuredError _ as e ->
      close_in input;
      raise e
  in

  let parse_m_dgfip m_program =
    if !Config.without_dgfip_m then m_program
    else
      let parse_internal str =
        let filebuf = Lexing.from_string str in
        let source_file = Dgfip_m.internal_m in
        parse filebuf source_file
      in
      let decs = parse_internal Dgfip_m.declarations in
      let events = parse_internal Dgfip_m.event_declaration in
      events :: decs :: m_program
  in

  let parse_m_files m_program =
    let parse_file_progress source_file =
      current_progress source_file;
      parse_file source_file
    in
    (*FIXME: use a fold here *)
    let prog =
      List.map parse_file_progress @@ Config.get_files !Config.source_files
    in
    List.rev prog @ m_program
  in

  let m_program =
    [] |> parse_m_dgfip |> parse_m_files |> List.rev
    |> patch_rule_1 !Config.backend !Config.dgfip_flags
  in
  finish "completed!";
  m_program

let run_single_test m_program test =
  Mir_interpreter.repl_debug := true;
  Test_interpreter.check_one_test m_program test !Config.value_sort
    !Config.round_ops;
  Cli.result_print "Test passed!"

let run_multiple_tests m_program tests =
  let filter_function =
    match !Config.dgfip_test_filter with
    | false -> fun _ -> true
    | true -> ( fun x -> match x.[0] with 'A' .. 'Z' -> true | _ -> false)
  in
  Test_interpreter.check_all_tests m_program tests !Config.value_sort
    !Config.round_ops filter_function

let extract m_program =
  Cli.debug_print "Extracting the desired function from the whole program...";
  match !Config.backend with
  | Config.Dgfip_c ->
      Cli.debug_print "Compiling the codebase to DGFiP C...";
      if !Config.output_file = "" then
        Errors.raise_error "an output file must be defined with --output";
      Dgfip_gen_files.generate_auxiliary_files !Config.dgfip_flags m_program;
      Bir_to_dgfip_c.generate_c_program !Config.dgfip_flags m_program
        !Config.output_file;
      Cli.debug_print "Result written to %s" !Config.output_file
  | UnknownBackend -> Errors.raise_error "No backend specified!"

let driver () =
  try
    Cli.debug_print "Reading M files...";
    let m_program = parse () in
    Cli.debug_print "Elaborating...";
    let m_program = Expander.proceed m_program in
    let m_program = Validator.proceed !Config.mpp_function m_program in
    let m_program = Mast_to_mir.translate m_program in
    let m_program = Mir.expand_functions m_program in
    Cli.debug_print "Creating combined program suitable for execution...";
    match !Config.execution_mode with
    | SingleTest test -> run_single_test m_program test
    | MultipleTests tests -> run_multiple_tests m_program tests
    | Extraction -> extract m_program
  with Errors.StructuredError (msg, pos_list, kont) as e ->
    Cli.error_print "%a" Errors.format_structured_error (msg, pos_list);
    (match kont with None -> () | Some kont -> kont ());
    raise e

let set_opts (files : string list) (application_names : string list)
    (without_dgfip_m : bool) (debug : bool) (var_info_debug : string list)
    (display_time : bool) (print_cycles : bool) (backend : string option)
    (output : string option) (run_tests : string option)
    (dgfip_test_filter : bool) (run_test : string option)
    (mpp_function : string option) (optimize_unsafe_float : bool)
    (precision : string option) (roundops : string option)
    (comparison_error_margin : float option) (income_year : int)
    (m_clean_calls : bool) (dgfip_options : string list option) =
  Config.set_opts ~files ~application_names ~without_dgfip_m ~debug
    ~var_info_debug ~display_time ~print_cycles ~backend ~output ~run_tests
    ~dgfip_test_filter ~run_test ~mpp_function ~optimize_unsafe_float ~precision
    ~roundops ~comparison_error_margin ~income_year ~m_clean_calls
    ~dgfip_options

let run () =
  let eval_cli =
    Cmdliner.Cmd.eval_value @@ Cmdliner.Cmd.v Cli.info (Cli.mlang_t set_opts)
  in
  match eval_cli with
  | Ok `Help | Ok `Version | Ok (`Ok `Displayed_dgfip_help) -> ()
  | Ok (`Ok `Run) -> driver ()
  | Ok (`Ok (`Error m)) -> Errors.raise_error m
  | Error `Exn ->
      Errors.raise_error
        "Uncaught exception while reading command line arguments"
  | Error `Parse -> Errors.raise_error "Parsing command line arguments failed"
  | Error `Term -> Errors.raise_error "Term evaluation error"

let main () =
  try run ()
  with Errors.StructuredError (msg, pos_list, kont) as e ->
    Cli.error_print "%a" Errors.format_structured_error (msg, pos_list);
    (match kont with None -> () | Some kont -> kont ());
    raise e
