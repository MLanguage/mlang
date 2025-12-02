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
  string list ->
  bool ->
  bool ->
  string list ->
  bool ->
  bool ->
  string option ->
  string option ->
  string option ->
  bool ->
  string option ->
  string ->
  bool ->
  string option ->
  string option ->
  float option ->
  int ->
  bool ->
  string list option ->
  'a) ->
  'a Cmdliner.Term.t
(** Mlang binary command-line arguments parsing function *)

val info : Cmdliner.Cmd.info
(** Command-line man page for --help *)

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
