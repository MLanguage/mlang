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
          | Ok (`Ok flags) -> flags
          | Ok _ -> assert false
          | Error _ ->
              Cli.error_print "parsing of DGFiP options failed, aborting";
              raise Exit
        end
    end
  | UnknownBackend -> Dgfip_options.default_flags

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

let set_opts (files : string list) (application_names : string list)
    (without_dgfip_m : bool) (debug : bool) (var_info_debug : string list)
    (display_time : bool) (print_cycles : bool) (backend : string option)
    (output : string option) (run_tests : string option)
    (dgfip_test_filter : bool) (run_test : string option)
    (mpp_function : string option) (optimize_unsafe_float : bool)
    (precision : string option) (roundops : string option)
    (comparison_error_margin : float option) (income_year : int)
    (m_clean_calls : bool) (dgfip_options : string list option)
    (no_nondet_display : bool) (plain_output : bool) (trace : bool)
    (trace_output_file : string option) =
  begin match (trace, trace_output_file) with
  | false, Some _ ->
      Cli.warning_print
        "trace_output_file has been given, but tracing has not been set."
  | _, _ -> ()
  end;
  Config.set_opts ~files ~application_names ~without_dgfip_m ~debug
    ~var_info_debug ~display_time ~print_cycles ~backend ~output ~run_tests
    ~dgfip_test_filter ~run_test ~mpp_function ~optimize_unsafe_float ~precision
    ~roundops ~comparison_error_margin ~income_year ~m_clean_calls
    ~dgfip_options ~no_nondet_display ~plain_output ~trace ~trace_output_file

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
