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

(**{2 Argument parsing }*)

(** {!module Arg} function that specifies command-line arguments parsing *)
let parse_cli_args () =
  (* Code block to retrieve and parse command-line arguments. *)
  let speclist = Arg.align [
      ("--application", Arg.Set_string application,
       " Nom de l'application (jette toutes les règles ne comportant pas cette mention)");
      ("--debug", Arg.Set debug_flag,
       " Affiche des informations de débuggage");
      ("--display_time", Arg.Set display_time,
       " Affiche le temps passé entre chaque information donnée par --debug");
      ("--dep_graph_file", Arg.Set_string dep_graph_file,
       " Nom du fichier où écrire le graphe de dépendance avec --debug");
      ("--print_cycles", Arg.Set print_cycles_flag,
       " Affiche les cycles de définition dans les variables");
      ("--optimize", Arg.Set optimize,
       "Optimise le programme (inlining, propagation des constantes, élimination du code mort)");
      ("--var_info", Arg.Set var_info_flag,
       " Affiche des informations sur les variables du programmes mal définies");
      ("--verify", Arg.Set verify_flag,
       " Vérifie que les conditions sont valables dans tous les cas");
      ("--backend", Arg.Set_string backend,
       " Défini le backend (Python, Z3, Interpreteur)");
      ("--function_spec", Arg.Set_string function_spec,
       " Fichier de spécification des entrées et sorties voulues");
      ("--output", Arg.Set_string output_file,
       " Nom du fichier de sortie pour le résultat de la compilation");
      ("--number_of_passes", Arg.Set_int number_of_passes,
       " Nombre de passes d'exécution pour les variables définies circulairement (défault 1)")
    ]
  in let usage_msg =
       "Parser and compiler for M, the language used by DGFiP to encode fiscal rules."
  in
  let anon_func (file: string) : unit =
    source_files := file::!source_files
  in Arg.parse speclist anon_func usage_msg

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
