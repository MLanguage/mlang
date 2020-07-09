(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** Command-line interface helpers *)

(**{1 Command line arguments}*)

(**{2 Argument parsing}*)

(** The command line interface is declared using {!module Cmdliner} *)

open Cmdliner

let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES" ~doc:"M files to be compiled")

let debug = Arg.(value & flag & info [ "debug"; "d" ] ~doc:"Prints debug information")

let display_time =
  Arg.(
    value & flag
    & info [ "display_time"; "t" ] ~doc:"Displays timing information (use with --debug)")

let dep_graph_file =
  let doc =
    "Name of the file where the variable dependency graph should be output (use with --debug)"
  in
  Arg.(value & opt file "dep_graph.dot" & info [ "dep_graph_file"; "g" ] ~docv:"DEP_GRAPH" ~doc)

let print_cycles =
  let doc =
    "If set, the eventual circular dependencies in variables definition willbe output to the \
     \"variable_cycles\" directory"
  in
  Arg.(value & flag & info [ "print_cycles"; "c" ] ~doc)

let optimize =
  Arg.(
    value & flag & info [ "optimize"; "O" ] ~doc:"Enables the optimizations passes on the M program")

let backend =
  Arg.(
    required
    & opt (some string) None
    & info [ "backend"; "b" ] ~docv:"BACKEND"
        ~doc:"Backend selection: interpreter, python, java, clojure")

let function_spec =
  Arg.(
    value
    & opt (some file) None
    & info [ "function_spec"; "f" ] ~docv:"SPEC"
        ~doc:
          "M function specification file (extension .m_spec).$(i, SPEC) should define the expected \
           inputs, outputs and constant values. This information will be used to select the \
           relevant computational rules from the M code corpus.")

let output =
  Arg.(
    value
    & opt (some string) None
    & info [ "output"; "o" ] ~docv:"OUTPUT"
        ~doc:
          "$(i, OUTPUT) is the file that will contain the extracted function (for compiler \
           backends)")

let real_precision =
  Arg.(
    value & opt int 100
    & info [ "real_precision"; "p" ] ~docv:"PRECISION"
        ~doc:
          "Z3 only deals with integer arithmetic, while M supports floating point values. This \
           parameter lets you choose the level of precision you want for Z3 computations, which is \
           equal to 1/$(i, PRECISION).")

let run_all_tests =
  Arg.(
    value
    & opt (some file) None
    & info [ "run_all_tests"; "R" ] ~docv:"TESTS" ~doc:"Run all tests in folder specified folder")

let run_test =
  Arg.(
    value
    & opt (some file) None
    & info [ "run_test"; "r" ] ~docv:"TESTS" ~doc:"Run specific test passed as argument")

let year = Arg.(value & opt int 2018 & info [ "year" ] ~docv:"FILES" ~doc:"year of the M program")

let mlang_t f =
  Term.(
    const f $ files $ debug $ display_time $ dep_graph_file $ print_cycles $ optimize $ backend
    $ function_spec $ output $ real_precision $ run_all_tests $ run_test $ year)

let info =
  let doc =
    "Intepreter and compiler for M, the language created by the French Direction Generale des \
     Finances Publiques (DGFiP)."
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "The M language is used by the DGFiP to encode the rules describing the computation of the \
         French income tax. An M program consists in several *.m files in no particular order. \
         $(tname) will parse all the rules contained in those files that correspond to a \
         particular application tag. Then, it will extract from this set of rules an \
         user-specified function, than can be interpreted with a command-line prompt or compiled \
         to a function in the language of your choice.";
      `S Manpage.s_authors;
      `P "Denis Merigoux <denis.merigoux@inria.fr>";
      `P "Raphael Monat <raphael.monat@lip6.fr>";
      `S Manpage.s_examples;
      `P "Typical usage:";
      `Pre "mlang -a iliad -f query.m_spec -b interpreter ir-calcul/sources2017m_6_10/*.m";
      `S Manpage.s_bugs;
      `P "Please file bug reports at https://gitlab.inria.fr/verifisc/mlang/issues";
    ]
  in
  let exits =
    Term.default_exits
    @ [
        Term.exit_info ~doc:"on M parsing error." 1;
        Term.exit_info ~doc:"on M typechecking error." 2;
      ]
  in
  Term.info "mlang"
    ~version:
      ( match Build_info.V1.version () with
      | None -> "n/a"
      | Some v -> Build_info.V1.Version.to_string v )
    ~doc ~exits ~man

(**{2 Flags and parameters}*)

(** M source files to be compiled *)
let source_files : string list ref = ref []

(** Prefix for dependency graph output files *)
let dep_graph_file : string ref = ref "dep_graph.dot"

(** Use Z3 to check if verif rules hold all the time *)
let verify_flag = ref false

(** Prints debug information *)
let debug_flag = ref false

(** Print infomation about variables declared, defined ou used incorrectly *)
let var_info_flag = ref false

(** Print warning info *)
let warning_flag = ref true

(** Dump circular definitions of variables *)
let print_cycles_flag = ref false

(** Displays timing information *)
let display_time = ref false

(** Run the optimisations on the M variable graph *)
let optimize = ref false

let output_file = ref ""

let function_spec = ref None

let real_precision = ref 100

let backend = ref "python"

let run_all_tests : string option ref = ref None

let run_test : string option ref = ref None

let year : int ref = ref 2018

let set_all_arg_refs (files_ : string list) (debug_ : bool) (display_time_ : bool)
    (dep_graph_file_ : string) (print_cycles_ : bool) (optimize_ : bool) (backend_ : string)
    (function_spec_ : string option) (output_ : string option) (real_precision_ : int)
    (run_all_tests_ : string option) (run_test_ : string option) (year_ : int) =
  source_files := files_;
  debug_flag := debug_;
  display_time := display_time_;
  dep_graph_file := dep_graph_file_;
  print_cycles_flag := print_cycles_;
  optimize := optimize_;
  backend := backend_;
  function_spec := function_spec_;
  real_precision := real_precision_;
  (output_file :=
     match output_ with
     | Some o -> o
     | None ->
         if backend_ = "interpreter" then ""
         else raise (Errors.ArgumentError ("--output flag must be set for the backend " ^ backend_)));
  run_all_tests := run_all_tests_;
  run_test := run_test_;
  year := year_

(**{1 Terminal formatting}*)

(**{2 Markers}*)

(** Prints [\[INFO\]] in blue on the terminal standard output *)
let var_info_marker () = ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.blue ] "[VAR INFO] "

let time : float ref = ref (Unix.gettimeofday ())

let initial_time : float ref = ref (Unix.gettimeofday ())

let time_marker () =
  let new_time = Unix.gettimeofday () in
  let old_time = !time in
  time := new_time;
  let delta = (new_time -. old_time) *. 1000. in
  if delta > 100. then
    ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.black ] "[TIME] %.0f ms\n" delta

(** Prints [\[DEBUG\]] in purple on the terminal standard output as well as timing since last debug *)
let debug_marker (f_time : bool) =
  if f_time then time_marker ();
  ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.magenta ] "[DEBUG] "

(** Prints [\[ERROR\]] in red on the terminal error output *)
let error_marker () = ANSITerminal.eprintf [ ANSITerminal.Bold; ANSITerminal.red ] "[ERROR] "

(** Prints [\[WARNING\]] in yellow on the terminal standard output *)
let warning_marker () = ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.yellow ] "[WARNING] "

(** Prints [\[RESULT\]] in green on the terminal standard output *)
let result_marker () = ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.green ] "[RESULT] "

let clocks =
  Array.of_list
    [
      "🕛"; "🕐"; "🕑"; "🕒"; "🕓"; "🕔"; "🕕"; "🕖"; "🕗"; "🕘"; "🕙"; "🕚";
    ]

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

(** All the printers below print their argument after the correct marker *)

let debug_print ?(endline = "\n") kont =
  if !debug_flag then
    Format.kasprintf
      (fun str -> Format.printf "%a%s%s@?" (fun _ -> debug_marker) !display_time str endline)
      kont
  else Format.ifprintf Format.std_formatter kont

let var_info_print kont =
  if !var_info_flag then
    Format.kasprintf (fun str -> Format.printf "%a%s@?" (fun _ -> var_info_marker) () str) kont
  else Format.ifprintf Format.std_formatter kont

let error_print kont =
  Format.kasprintf (fun str -> Format.eprintf "%a%s@?" (fun _ -> error_marker) () str) kont

(** Returns two functions: the first one, [current_progress], has to be called during the progress
    loop and the other one, [finish], has to be called at the end of the progressive task. *)
let create_progress_bar (task : string) : (string -> unit) * (string -> unit) =
  let step_ticks = 5 in
  let ticks = ref 0 in
  let msg = ref task in
  let stop = ref false in
  let timer () =
    while true do
      if !stop then Thread.exit ();
      clock_marker (!ticks / step_ticks);
      ticks := !ticks + 1;
      Format.printf "%s" !msg;
      ANSITerminal.erase ANSITerminal.Below;
      ANSITerminal.move_bol ();
      Unix.sleepf 0.05
    done
  in
  let _ = Thread.create timer () in
  ( (fun current_progress_msg -> msg := Format.sprintf "%s: %s" task current_progress_msg),
    fun finish_msg ->
      stop := true;
      debug_marker false;
      Format.printf "%s: %s" task finish_msg;
      ANSITerminal.erase ANSITerminal.Below;
      ANSITerminal.move_bol ();
      Format.printf "\n";
      time_marker () )

let warning_print kont =
  if !warning_flag then
    Format.kasprintf (fun str -> Format.printf "%a%s@?" (fun _ -> warning_marker) () str) kont
  else Format.ifprintf Format.std_formatter kont

let result_print kont =
  Format.kasprintf (fun str -> Format.printf "%a%s@?" (fun _ -> result_marker) () str) kont
