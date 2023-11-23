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

(** Command-line interface helpers *)

(**{1 Command line arguments}*)

(**{2 Argument parsing}*)

(** The command line interface is declared using {!module Cmdliner} *)

open Cmdliner

let files =
  Arg.(
    non_empty & pos_all file []
    & info [] ~docv:"FILES" ~doc:"M files to be compiled")

let debug =
  Arg.(value & flag & info [ "debug"; "d" ] ~doc:"Prints debug information")

let var_info_debug =
  Arg.(
    value & opt_all string []
    & info [ "var_info_debug" ]
        ~doc:"Prints debug information for variables passed as arguments")

let display_time =
  Arg.(
    value & flag
    & info [ "display_time"; "t" ]
        ~doc:"Displays timing information (use with --debug)")

let dep_graph_file =
  let doc =
    "Name of the file where the variable dependency graph should be output \
     (use with --debug)"
  in
  Arg.(
    value & opt file "dep_graph.dot"
    & info [ "dep_graph_file"; "g" ] ~docv:"DEP_GRAPH" ~doc)

let no_print_cycles =
  let doc = "If set, disable the eventual circular dependencies repport" in
  Arg.(value & flag & info [ "no_print_cycles"; "c" ] ~doc)

let optimize =
  let doc =
    "Applies dead code removal and partial evaluation to the generated code"
  in
  Arg.(value & flag & info [ "optimize"; "O" ] ~doc)

let optimize_unsafe_float =
  let doc =
    "Activate unsafe floating point optimizations (such as x * 0 ~> 0)"
  in
  Arg.(value & flag & info [ "fast-math" ] ~doc)

let backend =
  Arg.(
    value
    & opt (some string) None
    & info [ "backend"; "b" ] ~docv:"BACKEND"
        ~doc:"Backend selection: interpreter, Python, C, dgfip_c")

let function_spec =
  Arg.(
    value
    & opt (some file) None
    & info [ "function_spec"; "f" ] ~docv:"SPEC"
        ~doc:
          "M function specification file (extension .m_spec).$(i, SPEC) should \
           define the expected inputs, outputs and constant values. This \
           information will be used to select the relevant computational rules \
           from the M code corpus. If no function_spec is specified, all \
           available inputs and outputs are used for the calculation.")

let mpp_file =
  Arg.(
    required
    & opt (some file) None
    & info [ "mpp_file" ] ~docv:"MPP_FILE"
        ~doc:"M++ preprocessor file (extension .mpp)")

let mpp_function =
  Arg.(
    required
    & opt (some string) None
    & info [ "mpp_function" ] ~docv:"MPP_FUNCTION" ~doc:"M++ file main function")

let output =
  Arg.(
    value
    & opt (some string) None
    & info [ "output"; "o" ] ~docv:"OUTPUT"
        ~doc:
          "$(i, OUTPUT) is the file that will contain the extracted function \
           (for compiler backends)")

let run_all_tests =
  Arg.(
    value
    & opt (some file) None
    & info [ "run_all_tests"; "R" ] ~docv:"TESTS"
        ~doc:"Run all tests in folder specified folder")

let dgfip_test_filter =
  let doc =
    "Filter test files on filename: keep only tests beginning by an uppercase \
     character (use with --run-al-tests)"
  in
  Arg.(value & flag & info [ "dgfip_test_filter" ] ~doc)

let run_test =
  Arg.(
    value
    & opt (some file) None
    & info [ "run_test"; "r" ] ~docv:"TESTS"
        ~doc:"Run specific test passed as argument")

let code_coverage =
  Arg.(
    value & flag
    & info [ "code_coverage" ]
        ~doc:
          "Instruments the interpreter to retrieve the code coverage (use with \
           --run_all_tests)")

let precision =
  Arg.(
    value
    & opt (some string) (Some "double")
    & info [ "precision"; "p" ] ~docv:"PRECISION"
        ~doc:
          "Precision of the interpreter: double, mpfr<n> (where n > 0 it the \
           bit size of the multi-precision floats), fixed<n> (where n > 0 is \
           the fixpoint precision), interval (64-bits IEEE754 floats, with up \
           and down rounding mode), mpq (multi-precision rationals) . Default \
           is double")

let roundops =
  Arg.(
    value
    & opt (some string) (Some "default")
    & info [ "roundops" ] ~docv:"ROUNDOPS"
        ~doc:
          "Rounding operations to use in the interpreter: default, multi, \
           mainframe<n> (where n is the size in bits of the long type to \
           simulate). Each corresponds to the behavior of the legacy DGFiP \
           code in different environments: default when running on a regular \
           PC, multi when running in a multithread context, and mainframe when \
           running on a mainframe. In this case, the size of the long type has \
           to be specified; it can be either 32 or 64.")

let test_error_margin =
  Arg.(
    value
    & opt (some float) (Some 0.)
    & info [ "test_error_margin" ] ~docv:"ERROR_MARGIN"
        ~doc:
          "Margin of error tolerated when executing tests, as a float. Default \
           0.")

let m_clean_calls =
  Arg.(
    value & flag
    & info
        [ "clean_between_m_calls" ]
        ~doc:
          "Clean the value of computed variables between two m calls (to check \
           that there is no hidden state kept between two calls)")

let dgfip_options =
  Arg.(
    value
    & opt (some (list string)) None
    & info [ "dgfip_options" ]
        ~doc:
          "Specify DGFiP options (use --dgfip_options=--help to display DGFiP \
           specific options)")

let var_dependencies =
  Arg.(
    value
    & opt (some (pair string string)) None
    & info [ "var_dependencies" ]
        ~doc:"Output list of dependencies of the given variable")

let mlang_t f =
  Term.(
    const f $ files $ debug $ var_info_debug $ display_time $ dep_graph_file
    $ no_print_cycles $ backend $ function_spec $ mpp_file $ output
    $ run_all_tests $ dgfip_test_filter $ run_test $ mpp_function $ optimize
    $ optimize_unsafe_float $ code_coverage $ precision $ roundops
    $ test_error_margin $ m_clean_calls $ dgfip_options $ var_dependencies)

let info =
  let doc =
    "Intepreter and compiler for M, the language created by the French \
     Direction Generale des Finances Publiques (DGFiP)."
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "The M language is used by the DGFiP to encode the rules describing \
         the computation of the French income tax. An M program consists in \
         several *.m files in no particular order. $(tname) will parse all the \
         rules contained in those files that correspond to a particular \
         application tag. Then, it will extract from this set of rules an \
         user-specified function, than can be interpreted with a command-line \
         prompt or compiled to a function in the language of your choice.";
      `S Manpage.s_authors;
      `P "Denis Merigoux <denis.merigoux@inria.fr>";
      `P "Raphael Monat <raphael.monat@lip6.fr>";
      `S Manpage.s_examples;
      `P "Typical usage:";
      `Pre
        "mlang -a iliad -f query.m_spec -b interpreter \
         ir-calcul/sources2018m_6_3/*.m";
      `S Manpage.s_bugs;
      `P "Please file bug reports at https://github.com/MLanguage/mlang/issues";
    ]
  in
  let exits =
    Cmd.Exit.defaults
    @ [
        Cmd.Exit.info ~doc:"on M parsing error." 1;
        Cmd.Exit.info ~doc:"on M typechecking error." 2;
      ]
  in
  Cmd.info "mlang"
    ~version:
      (match Build_info.V1.version () with
      | None -> "n/a"
      | Some v -> Build_info.V1.Version.to_string v)
    ~doc ~exits ~man

type value_sort =
  | RegularFloat
  | MPFR of int (* bitsize of the floats *)
  | BigInt of int (* precision of the fixed point *)
  | Interval
  | Rational

type round_ops = RODefault | ROMulti | ROMainframe of int
(* size of type long, either 32 or 64 *)

let source_files : string list ref = ref []

let dep_graph_file : string ref = ref "dep_graph.dot"

let verify_flag = ref false

let debug_flag = ref false

let var_info_flag = ref false

let var_info_debug = ref []

let warning_flag = ref true

let no_print_cycles_flag = ref false

let display_time = ref false

let output_file = ref ""

let optimize_unsafe_float = ref false

let m_clean_calls = ref false

let value_sort = ref RegularFloat

let round_ops = ref RODefault

let set_all_arg_refs (files_ : string list) (debug_ : bool)
    (var_info_debug_ : string list) (display_time_ : bool)
    (dep_graph_file_ : string) (no_print_cycles_ : bool)
    (output_file_ : string option) (optimize_unsafe_float_ : bool)
    (m_clean_calls_ : bool) (value_sort_ : value_sort) (round_ops_ : round_ops)
    =
  source_files := files_;
  debug_flag := debug_;
  var_info_debug := var_info_debug_;
  var_info_flag := !var_info_debug <> [];
  display_time := display_time_;
  dep_graph_file := dep_graph_file_;
  no_print_cycles_flag := no_print_cycles_;
  optimize_unsafe_float := optimize_unsafe_float_;
  m_clean_calls := m_clean_calls_;
  value_sort := value_sort_;
  round_ops := round_ops_;
  match output_file_ with None -> () | Some o -> output_file := o

(**{1 Terminal formatting}*)

let concat_with_line_depending_prefix_and_suffix (prefix : int -> string)
    (suffix : int -> string) (ss : string list) =
  match ss with
  | hd :: rest ->
      let out, _ =
        List.fold_left
          (fun (acc, i) s ->
            ( (acc ^ prefix i ^ s
              ^ if i = List.length ss - 1 then "" else suffix i),
              i + 1 ))
          ((prefix 0 ^ hd ^ if 0 = List.length ss - 1 then "" else suffix 0), 1)
          rest
      in
      out
  | [] -> prefix 0

let add_prefix_to_each_line (s : string) (prefix : int -> string) =
  concat_with_line_depending_prefix_and_suffix
    (fun i -> prefix i)
    (fun _ -> "\n")
    (String.split_on_char '\n' s)

(**{2 Markers}*)

(** Prints [\[INFO\]] in blue on the terminal standard output *)
let var_info_marker () =
  ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.blue ] "[VAR INFO] "

let time : float ref = ref (Unix.gettimeofday ())

let initial_time : float ref = ref (Unix.gettimeofday ())

let time_marker () =
  let new_time = Unix.gettimeofday () in
  let old_time = !time in
  time := new_time;
  let delta = (new_time -. old_time) *. 1000. in
  if delta > 100. then
    ANSITerminal.printf
      [ ANSITerminal.Bold; ANSITerminal.black ]
      "[TIME] %.0f ms\n" delta

let format_with_style (styles : ANSITerminal.style list)
    (str : ('a, unit, string) format) =
  if true (* can depend on a stylr flag *) then ANSITerminal.sprintf styles str
  else Printf.sprintf str

(** Prints [\[DEBUG\]] in purple on the terminal standard output as well as
    timing since last debug *)
let debug_marker (f_time : bool) =
  if f_time then time_marker ();
  ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.magenta ] "[DEBUG] "

(** Prints [\[ERROR\]] in red on the terminal error output *)
let error_marker () =
  ANSITerminal.eprintf [ ANSITerminal.Bold; ANSITerminal.red ] "[ERROR] "

(** Prints [\[WARNING\]] in yellow on the terminal standard output *)
let warning_marker () =
  ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.yellow ] "[WARNING] "

(** Prints [\[RESULT\]] in green on the terminal standard output *)
let result_marker () =
  ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.green ] "[RESULT] "

let clocks =
  Array.of_list [ "🕛"; "🕐"; "🕑"; "🕒"; "🕓"; "🕔"; "🕕"; "🕖"; "🕗"; "🕘"; "🕙"; "🕚" ]

(** Prints [\[🕛\]] in blue on the terminal standard output *)
let clock_marker i =
  let new_time = Unix.gettimeofday () in
  let initial_time = !initial_time in
  let delta = new_time -. initial_time in
  ANSITerminal.printf
    [ ANSITerminal.Bold; ANSITerminal.blue ]
    "[%s  %.1f s] "
    clocks.(i mod Array.length clocks)
    delta

(**{2 Printers}*)

let debug_print ?(endline = "\n") kont =
  ANSITerminal.erase ANSITerminal.Eol;
  if !debug_flag then
    Format.kasprintf
      (fun str ->
        Format.printf "%a%s%s@?"
          (fun _ -> debug_marker)
          !display_time str endline)
      kont
  else Format.ifprintf Format.std_formatter kont

let var_info_print kont =
  ANSITerminal.erase ANSITerminal.Eol;
  if !var_info_flag then
    Format.kasprintf
      (fun str -> Format.printf "%a%s@." (fun _ -> var_info_marker) () str)
      kont
  else Format.ifprintf Format.std_formatter kont

let error_print kont =
  ANSITerminal.erase ANSITerminal.Eol;
  Format.kasprintf
    (fun str -> Format.eprintf "%a%s@." (fun _ -> error_marker) () str)
    kont

let create_progress_bar (task : string) : (string -> unit) * (string -> unit) =
  let step_ticks = 5 in
  let ticks = ref 0 in
  let msg = ref task in
  let stop = ref false in
  let timer () =
    while true do
      if !stop then Thread.exit ();
      ticks := !ticks + 1;
      clock_marker (!ticks / step_ticks);
      Format.printf "%s" !msg;
      flush_all ();
      flush_all ();
      ANSITerminal.erase ANSITerminal.Below;
      ANSITerminal.move_bol ();
      Unix.sleepf 0.05
    done
  in
  let _ = Thread.create timer () in
  ( (fun current_progress_msg ->
      msg := Format.sprintf "%s: %s" task current_progress_msg),
    fun finish_msg ->
      stop := true;
      debug_marker false;
      Format.printf "%s: %s" task finish_msg;
      ANSITerminal.erase ANSITerminal.Below;
      ANSITerminal.move_bol ();
      Format.printf "\n";
      time_marker () )

let warning_print kont =
  ANSITerminal.erase ANSITerminal.Eol;
  if !warning_flag then
    Format.kasprintf
      (fun str -> Format.printf "%a%s@." (fun _ -> warning_marker) () str)
      kont
  else Format.ifprintf Format.std_formatter kont

let result_print kont =
  ANSITerminal.erase ANSITerminal.Eol;
  Format.kasprintf
    (fun str -> Format.printf "%a%s@." (fun _ -> result_marker) () str)
    kont
