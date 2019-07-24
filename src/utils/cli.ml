(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by the DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-C license and that you accept its terms.
*)

(** Command-line interface helpers *)

(**{1 Command line arguments }*)

(**{2 Argument parsing }*)

(** The command line interface is declared using {!module Cmdliner}  *)


open Cmdliner

let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES" ~doc:"M files to be compiled")

let application = Arg.(
    required &
    opt (some string) None &
    info ["application"; "a"]
      ~docv:"APPLICATION"
      ~doc:"Name of the M application to select rules from : iliad, batch, bareme..."
  )

let debug = Arg.(value & flag & info ["debug"; "d"] ~doc:"Prints debug information")

let display_time = Arg.(
    value &
    flag &
    info ["display_time"; "t"] ~doc:"Displays timing information (use with --debug)")

let dep_graph_file =
  let doc = "Name of the file where the variable dependency graph should be output \
             (use with --debug)" in
  Arg.(
    value &
    opt file "dep_graph.dot" &
    info ["dep_graph_file"; "g"] ~docv:"DEP_GRAPH" ~doc
  )

let print_cycles =
  let doc = "If set, the eventual circular dependencies in variables definition will\
             be output to the \"variable_cycles\" directory" in
  Arg.(
    value &
    flag &
    info ["print_cycles"; "c"] ~doc
  )

let optimize =
  Arg.(
    value &
    flag &
    info ["optimize"; "O"] ~doc:"Optimize the program by partial evaluation"
  )

let backend =
  Arg.(
    required &
    opt (some string) None &
    info ["backend"; "b"]
      ~docv:"BACKEND"
      ~doc:"Backend selection: interpreter, python or z3"
  )

let function_spec =
  Arg.(
    required &
    opt (some file) None &
    info ["function_spec"; "f"]
      ~docv:"SPEC"
      ~doc:"M function specification file (extension .m_spec).\
            $(i, SPEC) should define the expected inputs, outputs and \
            constant values. This information will be used to select the \
            relevant computational rules from the M code corpus."
  )

let output =
  Arg.(
    value &
    opt (some string) None &
    info ["output"; "o"]
      ~docv:"OUTPUT"
      ~doc:"$(i, OUTPUT) is the file that will contain the extracted function \
            (for compiler backends)"
  )

let number_of_passes =
  Arg.(
    value &
    opt int 1 &
    info ["number_of_passes"; "n"] ~docv:"PASSES"
      ~doc:"M programs can contain variables defined circularly. In this \
            case, the value computed by the program depends on an arbitrary \
            number of execution passes that you provide with $(i,PASSES)"
  )



let verifisc_t f =
  Term.(
    const f $
    files $
    application $
    debug $
    display_time $
    dep_graph_file $
    print_cycles $
    optimize $
    backend $
    function_spec $
    output $
    number_of_passes
  )

let info =
  let doc =  "Intepreter and compiler for M, the language created by the French \
              Direction Generale des Finances Publiques (DGFiP)."
  in
  let man = [
    `S Manpage.s_description;
    `P "The M language is used by the DGFiP to encode the rules describing the computation \
        of the French income tax. An M program consists in several *.m files in no particular \
        order. $(tname) will parse all the rules contained in those files that correspond to a \
        particular application tag. Then, it will extract from this set of rules an \
        user-specified function, than can be interpreted with a command-line prompt or compiled \
        to a function in the language of your choice.";
    `S Manpage.s_authors;
    `P "Denis Merigoux <denis.merigoux@inria.fr>";
    `P "Raphael Monat <raphael.monat@lip6.fr>";
    `S Manpage.s_examples;
    `P "Typical usage:";
    `Pre "verifisc -a iliad -f query.m_spec -b interpreter ir-calcul/sources2017m_6_10/*.m";
    `S Manpage.s_bugs;
    `P "Please file bug reports at https://gitlab.inria.fr/verifisc/verifisc-m/issues" ]
  in
  let exits = Term.default_exits @ [
      Term.exit_info ~doc:"on M parsing error." 1;
      Term.exit_info ~doc:"on M typechecking error." 2;
    ] in
  Term.info "verifisc" ~version:(match Build_info.V1.version () with
      | None -> "n/a"
      | Some v -> Build_info.V1.Version.to_string v) ~doc ~exits ~man

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

(** Dump circular definitions of variables *)
let print_cycles_flag = ref false

(** Name of application to consider (drops all the rules not corresponding to it) *)
let application = ref ""

(** Displays timing information *)
let display_time = ref false

(** Run the optimisations on the M variable graph *)
let optimize = ref false

let output_file = ref ""

let function_spec = ref ""

let number_of_passes = ref 1

let backend = ref "python"

let set_all_arg_refs
    (files_: string list)
    (application_: string)
    (debug_: bool)
    (display_time_: bool)
    (dep_graph_file_: string)
    (print_cycles_: bool)
    (optimize_: bool)
    (backend_: string)
    (function_spec_: string)
    (output_: string option)
    (number_of_passes_: int)
  =
  source_files := files_;
  application := application_;
  debug_flag := debug_;
  display_time := display_time_;
  dep_graph_file := dep_graph_file_;
  print_cycles_flag := print_cycles_;
  optimize := optimize_;
  backend := backend_;
  function_spec := function_spec_;
  output_file := begin match output_ with
    | Some o -> o
    | None -> if backend_ = "interpreter" then "" else
        raise (Errors.ArgumentError ("--output flag must be set for the backend " ^ backend_))
  end;
  number_of_passes := number_of_passes_

(**{1 Terminal formatting }*)

(**{2 Markers}*)

(** Prints [[INFO]] in blue on the terminal standard output *)
let var_info_marker () = ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.blue] "[VAR INFO] "

let time : float ref = ref (Unix.gettimeofday ())

(** Prints [[DEBUG]] in purple on the terminal standard output as well as timing since last debug *)
let debug_marker () =
  if !display_time then begin
    let new_time = Unix.gettimeofday () in
    let old_time = !time in
    time := new_time;
    let delta = (new_time -. old_time) *. 1000. in
    if delta > 100. then begin
      ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.black]
        "[TIME] ";
      Printf.printf "%.0f ms\n"
        delta
    end
  end;
  ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.magenta] "[DEBUG] "

(** Prints [[ERROR]] in red on the terminal error output *)
let error_marker () = ANSITerminal.eprintf [ANSITerminal.Bold; ANSITerminal.red] "[ERROR] "

(** Prints [[WARNING]] in yellow on the terminal standard output *)
let warning_marker () = ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.yellow] "[WARNING] "

(** Prints [[RESULT]] in green on the terminal standard output *)
let result_marker () = ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.green] "[RESULT] "

(**{2 Printers}*)

(** All the printers below print their argument after the correct marker *)

let debug_print (s: string) =
  if !debug_flag then begin
    debug_marker ();
    Printf.printf "%s\n" s;
    flush stdout;
    flush stdout
  end

let var_info_print (s: string) =
  if !var_info_flag then begin
    var_info_marker ();
    Printf.printf "%s\n" s;
    flush stdout;
    flush stdout
  end

let error_print (s: string) =
  error_marker ();
  Printf.eprintf "%s\n" s;
  flush stdout;
  flush stdout

let warning_print (s: string) =
  warning_marker ();
  Printf.printf "%s\n" s;
  flush stdout;
  flush stdout

let result_print (s: string) =
  result_marker ();
  Printf.printf "%s\n" s;
  flush stdout;
  flush stdout
