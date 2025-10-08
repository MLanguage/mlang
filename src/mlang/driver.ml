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
open M_ir
open M_frontend

exception Exit

let process_dgfip_options (backend : Config.backend)
    ~(application_names : string list) (dgfip_options : string list option) =
  match backend with
  | Dgfip_c -> begin
      match dgfip_options with
      | None ->
          Cli.error_print
            "when using the DGFiP backend, DGFiP options MUST be provided";
          raise Exit
      | Some options -> begin
          match
            Dgfip_options.process_dgfip_options ~application_names options
          with
          | None ->
              Cli.error_print "parsing of DGFiP options failed, aborting";
              raise Exit
          | Some flags -> flags
        end
    end
  | UnknownBackend -> Config.Dgfip_options.default_flags

let set_opts (files : string list) (application_names : string list)
    (without_dgfip_m : bool) (debug : bool) (var_info_debug : string list)
    (display_time : bool) (print_cycles : bool) (backend : string option)
    (output : string option) (run_tests : string option)
    (dgfip_test_filter : bool) (run_test : string option)
    (mpp_function : string) (optimize_unsafe_float : bool)
    (precision : string option) (roundops : string option)
    (comparison_error_margin : float option) (income_year : int)
    (m_clean_calls : bool) (dgfip_options : string list option) =
  let value_sort =
    let precision = Option.get precision in
    if precision = "double" then Config.RegularFloat
    else
      let mpfr_regex = Re.Pcre.regexp "^mpfr(\\d+)$" in
      if Re.Pcre.pmatch ~rex:mpfr_regex precision then
        let mpfr_prec =
          Re.Pcre.get_substring (Re.Pcre.exec ~rex:mpfr_regex precision) 1
        in
        Config.MPFR (int_of_string mpfr_prec)
      else if precision = "interval" then Config.Interval
      else
        let bigint_regex = Re.Pcre.regexp "^fixed(\\d+)$" in
        if Re.Pcre.pmatch ~rex:bigint_regex precision then
          let fixpoint_prec =
            Re.Pcre.get_substring (Re.Pcre.exec ~rex:bigint_regex precision) 1
          in
          Config.BigInt (int_of_string fixpoint_prec)
        else if precision = "mpq" then Config.Rational
        else
          Errors.raise_error
            (Format.asprintf "Unkown precision option: %s" precision)
  in
  let round_ops =
    match roundops with
    | Some "default" -> Config.RODefault
    | Some "multi" -> Config.ROMulti
    | Some roundops ->
        let mf_regex = Re.Pcre.regexp "^mainframe(\\d+)$" in
        if Re.Pcre.pmatch ~rex:mf_regex roundops then
          let mf_long_size =
            Re.Pcre.get_substring (Re.Pcre.exec ~rex:mf_regex roundops) 1
          in
          match int_of_string mf_long_size with
          | (32 | 64) as sz -> Config.ROMainframe sz
          | _ ->
              Errors.raise_error
                (Format.asprintf "Invalid long size for mainframe: %s"
                   mf_long_size)
        else
          Errors.raise_error
            (Format.asprintf "Unknown roundops option: %s" roundops)
    | None -> Errors.raise_error @@ Format.asprintf "Unspecified roundops@."
  in
  let backend =
    match backend with Some "dgfip_c" -> Config.Dgfip_c | _ -> UnknownBackend
  in
  let execution_mode =
    match (run_tests, run_test) with
    | Some s, _ -> Config.MultipleTests s
    | None, Some s -> Config.SingleTest s
    | None, None -> Config.Extraction
  in
  let files =
    match List.length files with
    | 0 -> Errors.raise_error "please provide at least one M source file"
    | _ -> Config.NonEmpty files
  in
  let dgfip_flags =
    process_dgfip_options backend ~application_names dgfip_options
  in
  Config.set_all_arg_refs files application_names without_dgfip_m debug
    var_info_debug display_time print_cycles output optimize_unsafe_float
    m_clean_calls comparison_error_margin income_year value_sort round_ops
    backend dgfip_test_filter mpp_function dgfip_flags execution_mode

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
    let progress_bar = Cli.create_progress_bar "Parsing" in
    let files = Config.get_files !Config.source_files in
    let m_program = Parsing.parse files progress_bar in
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

let main () =
  let opt_code =
    Cmdliner.Cmd.eval @@ Cmdliner.Cmd.v Cli.info (Cli.mlang_t set_opts)
  in
  match opt_code with 0 -> driver () | i -> exit i
