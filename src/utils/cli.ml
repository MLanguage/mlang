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
let dep_graph_file : string ref = ref "dep_graph"

(** Use Z3 to check if verif rules hold all the time *)
let verify_flag = ref false

(** Prints debug information *)
let debug_flag = ref false

(** Print infomation about variables declared, defined ou used incorrectly *)
let var_info_flag = ref false

(** Don't check for cycles in the variables depencency graph *)
let no_cycles_check_flag = ref false

(** Name of application to consider (drops all the rules not corresponding to it) *)
let application = ref ""

(** Wheter given back variables should be marked as program output *)
let flag_output_given_back = ref false

(** Output variable *)
let output_variable = ref ""

(** Run the optimisations on the M variable graph *)
let optimize = ref false

(**{2 Argument parsing }*)

(** {!module Arg} function that specifies command-line arguments parsing *)
let parse_cli_args () =
  (* Code block to retrieve and parse command-line arguments. *)
  let speclist = Arg.align [
      ("--application", Arg.Set_string application,
       " Nom de l'application (jette toutes les règles ne comportant pas cette mention)");
      ("--debug", Arg.Set debug_flag,
       " Affiche des informations de débuggage");
      ("--dep_graph_file", Arg.Set_string dep_graph_file,
       " Préfixe pour le fichier où écrire le graphe de dépendance avec --debug (par défault \"dep_graph\")");
      ("--given_back_output", Arg.Set flag_output_given_back,
       "Marque les variables \"restituées\" comme des \"sortie()\" du programme");
      ("--no_cycles_check", Arg.Set no_cycles_check_flag,
       " Ne vérifie pas l'absence de définitions circulaires (peut causer une boucle infinie à l'interprétation)");
      ("--optimize", Arg.Set optimize,
       "Optimise le programme (inlining, propagation des constantes, élimination du code mort)");
      ("--output", Arg.Set_string output_variable,
       "Marque une variable particulière comme sortie du programme (elle sera alors la seule sortie)");
      ("--var_info", Arg.Set var_info_flag,
       " Affiche des informations sur les variables du programmes mal définies");
      ("--verify", Arg.Set verify_flag,
       " Vérifie que les conditions sont valables dans tous les cas");
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

(** Prints [[DEBUG]] in purple on the terminal standard output *)
let debug_marker () = ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.magenta] "[DEBUG] "

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
