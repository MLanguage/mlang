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

(**{2 Command line arguments parsing}*)

val mlang_t :
  (string list ->
  bool ->
  bool ->
  string list ->
  bool ->
  string ->
  bool ->
  string option ->
  string option ->
  string ->
  string option ->
  string option ->
  bool ->
  string option ->
  string ->
  bool ->
  bool ->
  bool ->
  string option ->
  string option ->
  float option ->
  bool ->
  string list option ->
  (string * string) option ->
  'a) ->
  'a Cmdliner.Term.t
(** Mlang binary command-line arguments parsing function *)

val info : Cmdliner.Cmd.info
(** Command-line man page for --help *)

(**{2 Flags and parameters}*)

(** According on the [value_sort], a specific interpreter will be called with
    the right kind of floating-point value *)
type value_sort =
  | RegularFloat
  | MPFR of int  (** bitsize of the floats *)
  | BigInt of int  (** precision of the fixed point *)
  | Interval
  | Rational

(** Rounding operations to use in the interpreter. They correspond to the
    rounding operations used by the DGFiP calculator in different execution
    contexts.

    - RODefault: rounding operations used in the PC/single-thread context
    - ROMulti: rouding operations used in the PC/multi-thread context
    - ROMainframe rounding operations used in the mainframe context *)
type round_ops =
  | RODefault
  | ROMulti
  | ROMainframe of int  (** size of type long, either 32 or 64 *)

val source_files : string list ref
(** M source files to be compiled *)

val dep_graph_file : string ref
(** Prefix for dependency graph output files *)

val verify_flag : bool ref
(** Use Z3 to check if verif rules hold all the time *)

val debug_flag : bool ref
(** Prints debug information *)

val var_info_flag : bool ref
(** Print infomation about variables declared, defined ou used incorrectly *)

val var_info_debug : string list ref
(** Prints even more information but only about some variables members of a list *)

val warning_flag : bool ref
(** Print warning info *)

val no_print_cycles_flag : bool ref
(** Dump circular definitions of variables *)

val display_time : bool ref
(** Displays timing information *)

val output_file : string ref
(** Output file *)

val optimize_unsafe_float : bool ref
(** Activate unsafe floating point optimizations *)

val m_clean_calls : bool ref
(** Clean regular variables between M calls *)

val value_sort : value_sort ref

val round_ops : round_ops ref

val set_all_arg_refs :
  (* files *) string list ->
  (* without_dgfip_m *) bool ->
  (* debug *) bool ->
  (* var_info_debug *) string list ->
  (* display_time *) bool ->
  (* dep_graph_file *) string ->
  (* prints_cycles *) bool ->
  (* output_file *) string option ->
  (* optimize_unsafe_float *) bool ->
  (* m_clean_call *) bool ->
  value_sort ->
  round_ops ->
  unit

val add_prefix_to_each_line : string -> (int -> string) -> string
(** [add_prefix_to_each_line msg prefix] will print msg but each line with line
    number [i] starts with the string [prefix i]*)

(**{2 Printers}*)

val format_with_style :
  ANSITerminal.style list -> ('a, unit, string) format -> 'a

(** All the printers below print their argument after the correct marker *)

val var_info_print : ('a, Format.formatter, unit, unit) format4 -> 'a

val debug_print :
  ?endline:string -> ('a, Format.formatter, unit, unit) format4 -> 'a

val warning_print : ('a, Format.formatter, unit, unit) format4 -> 'a

val error_print : ('a, Format.formatter, unit, unit) format4 -> 'a

val result_print : ('a, Format.formatter, unit, unit) format4 -> 'a

val create_progress_bar : string -> (string -> unit) * (string -> unit)
(** Returns two functions: the first one, [current_progress], has to be called
    during the progress loop and the other one, [finish], has to be called at
    the end of the progressive task. *)
