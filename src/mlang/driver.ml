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

open Lexing
open Mlexer

let process_dgfip_options (backend : string option)
    (dgfip_options : string list option) =
  match backend with
  | Some backend when String.lowercase_ascii backend = "dgfip_c" -> begin
      match dgfip_options with
      | None ->
          Errors.raise_error
            "when using the DGFiP backend, DGFiP options MUST be provided"
      | Some options -> begin
          match Dgfip_options.process_dgfip_options options with
          | None ->
              Errors.raise_error "parsing of DGFiP options failed, aborting"
          | Some flags -> flags
        end
    end
  | _ -> Dgfip_options.default_flags

(* The legacy compiler plays a nasty trick on us, that we have to reproduce:
   rule 1 is modified to add assignments to APPLI_XXX variables according to the
   target application (OCEANS, BATCH and ILIAD). *)
let patch_rule_1 (backend : string option) (dgfip_flags : Dgfip_options.flags)
    (source_file : Mast.source_file) =
  let open Mast in
  let mk_assign var val_ =
    let v = if val_ then 1.0 else 0.0 in
    ( Com.SingleFormula
        ((Normal var, Pos.no_pos), None, (Literal (Float v), Pos.no_pos)),
      Pos.no_pos )
  in
  let oceans, batch, iliad =
    match backend with
    | Some backend when String.lowercase_ascii backend = "dgfip_c" ->
        (dgfip_flags.flg_cfir, dgfip_flags.flg_gcos, dgfip_flags.flg_iliad)
    | _ -> (false, false, true)
  in
  List.map
    (fun item ->
      match Pos.unmark item with
      | Rule r when Pos.unmark r.rule_number = 1 ->
          let fl =
            List.map
              (fun f -> Pos.same_pos_as (Com.Affectation f) f)
              [
                mk_assign "APPLI_OCEANS" oceans;
                mk_assign "APPLI_BATCH" batch;
                mk_assign "APPLI_ILIAD" iliad;
              ]
          in
          ( Rule { r with rule_formulaes = r.rule_formulaes @ fl },
            Pos.get_position item )
      | _ -> item)
    source_file

(** Entry function for the executable. Returns a negative number in case of
    error. *)
let driver (files : string list) (application_names : string list)
    (without_dgfip_m : bool) (debug : bool) (var_info_debug : string list)
    (display_time : bool) (dep_graph_file : string) (print_cycles : bool)
    (backend : string option) (output : string option)
    (run_all_tests : string option) (dgfip_test_filter : bool)
    (run_test : string option) (mpp_function : string)
    (optimize_unsafe_float : bool) (precision : string option)
    (roundops : string option) (comparison_error_margin : float option)
    (income_year : int option) (m_clean_calls : bool)
    (dgfip_options : string list option) =
  if income_year = None then
    Errors.raise_error "income year missing (--income-year YEAR)";
  let value_sort =
    let precision = Option.get precision in
    if precision = "double" then Cli.RegularFloat
    else
      let mpfr_regex = Re.Pcre.regexp "^mpfr(\\d+)$" in
      if Re.Pcre.pmatch ~rex:mpfr_regex precision then
        let mpfr_prec =
          Re.Pcre.get_substring (Re.Pcre.exec ~rex:mpfr_regex precision) 1
        in
        Cli.MPFR (int_of_string mpfr_prec)
      else if precision = "interval" then Cli.Interval
      else
        let bigint_regex = Re.Pcre.regexp "^fixed(\\d+)$" in
        if Re.Pcre.pmatch ~rex:bigint_regex precision then
          let fixpoint_prec =
            Re.Pcre.get_substring (Re.Pcre.exec ~rex:bigint_regex precision) 1
          in
          Cli.BigInt (int_of_string fixpoint_prec)
        else if precision = "mpq" then Cli.Rational
        else
          Errors.raise_error
            (Format.asprintf "Unkown precision option: %s" precision)
  in
  let round_ops =
    let roundops = Option.get roundops in
    if roundops = "default" then Cli.RODefault
    else if roundops = "multi" then Cli.ROMulti
    else
      let mf_regex = Re.Pcre.regexp "^mainframe(\\d+)$" in
      if Re.Pcre.pmatch ~rex:mf_regex roundops then
        let mf_long_size =
          Re.Pcre.get_substring (Re.Pcre.exec ~rex:mf_regex roundops) 1
        in
        match int_of_string mf_long_size with
        | (32 | 64) as sz -> Cli.ROMainframe sz
        | _ ->
            Errors.raise_error
              (Format.asprintf "Invalid long size for mainframe: %s"
                 mf_long_size)
      else
        Errors.raise_error
          (Format.asprintf "Unkown roundops option: %s" roundops)
  in
  Cli.set_all_arg_refs files application_names without_dgfip_m debug
    var_info_debug display_time dep_graph_file print_cycles output
    optimize_unsafe_float m_clean_calls comparison_error_margin income_year
    value_sort round_ops;
  let dgfip_flags = process_dgfip_options backend dgfip_options in
  try
    Cli.debug_print "Reading M files...";
    let current_progress, finish = Cli.create_progress_bar "Parsing" in
    let m_program = ref [] in
    if not without_dgfip_m then (
      let filebuf = Lexing.from_string Dgfip_m.declarations in
      current_progress Dgfip_m.internal_m;
      let filebuf =
        {
          filebuf with
          lex_curr_p =
            { filebuf.lex_curr_p with pos_fname = Dgfip_m.internal_m };
        }
      in
      try
        let commands = Mparser.source_file token filebuf in
        m_program := commands :: !m_program
      with Mparser.Error ->
        Errors.raise_error
          (Format.sprintf "M\n       syntax error in %s" Dgfip_m.internal_m));
    if List.length !Cli.source_files = 0 then
      Errors.raise_error "please provide at least one M source file";
    List.iter
      (fun source_file ->
        let filebuf, input =
          if source_file <> "" then
            let input = open_in source_file in
            (Lexing.from_channel input, input)
          else failwith "You have to specify at least one file!"
        in
        current_progress source_file;
        let filebuf =
          {
            filebuf with
            lex_curr_p = { filebuf.lex_curr_p with pos_fname = source_file };
          }
        in
        try
          let commands = Mparser.source_file token filebuf in
          let commands = patch_rule_1 backend dgfip_flags commands in
          m_program := commands :: !m_program
        with Mparser.Error ->
          close_in input;
          Errors.raise_spanned_error "M syntax error"
            (Parse_utils.mk_position (filebuf.lex_start_p, filebuf.lex_curr_p)))
      !Cli.source_files;
    m_program := List.rev !m_program;
    finish "completed!";
    Cli.debug_print "Elaborating...";
    let m_program = Mast_to_mir.translate !m_program mpp_function in
    let m_program = Mir.expand_functions m_program in
    Cli.debug_print "Creating combined program suitable for execution...";
    if run_all_tests <> None then
      let tests : string =
        match run_all_tests with Some s -> s | _ -> assert false
      in
      let filter_function =
        match dgfip_test_filter with
        | false -> fun _ -> true
        | true -> ( fun x -> match x.[0] with 'A' .. 'Z' -> true | _ -> false)
      in
      Test_interpreter.check_all_tests m_program tests value_sort round_ops
        filter_function
    else if run_test <> None then begin
      Mir_interpreter.repl_debug := true;
      let test : string =
        match run_test with Some s -> s | _ -> assert false
      in
      ignore (Test_interpreter.check_test m_program test value_sort round_ops);
      Cli.result_print "Test passed!"
    end
    else begin
      Cli.debug_print
        "Extracting the desired function from the whole program...";
      match backend with
      | Some backend ->
          if String.lowercase_ascii backend = "dgfip_c" then begin
            Cli.debug_print "Compiling the codebase to DGFiP C...";
            if !Cli.output_file = "" then
              Errors.raise_error "an output file must be defined with --output";
            Dgfip_gen_files.generate_auxiliary_files dgfip_flags m_program;
            Bir_to_dgfip_c.generate_c_program dgfip_flags m_program
              !Cli.output_file;
            Cli.debug_print "Result written to %s" !Cli.output_file
          end
          else
            Errors.raise_error (Format.asprintf "Unknown backend: %s" backend)
      | None -> Errors.raise_error "No backend specified!"
    end
  with Errors.StructuredError (msg, pos, kont) ->
    Cli.error_print "%a" Errors.format_structured_error (msg, pos);
    (match kont with None -> () | Some kont -> kont ());
    exit (-1)

let main () =
  exit @@ Cmdliner.Cmd.eval @@ Cmdliner.Cmd.v Cli.info (Cli.mlang_t driver)
