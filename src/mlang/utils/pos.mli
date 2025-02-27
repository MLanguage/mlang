(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** A position in the source code is a file, as well as begin and end location
    of the form col:line *)

(** {2 Source code position} *)

type t

val make_position : string -> Lexing.position * Lexing.position -> t

val make_position_between : t -> t -> t

val format_position_short : Format.formatter -> t -> unit

val format_position_gnu : Format.formatter -> t -> unit
(** Respects https://www.gnu.org/prep/standards/standards.html#Formatting-Error-Messages *)

val format_position : Format.formatter -> t -> unit

type 'a marked = 'a * t
(** Everything related to the source code should keep its t stored, to improve
    error messages *)

val no_pos : t
(** Placeholder t *)

val mark : t -> 'a -> 'a marked

val unmark : 'a marked -> 'a

val get : 'a marked -> t

val map_under_mark : ('a -> 'b) -> 'a marked -> 'b marked

val same_pos_as : 'a -> 'b marked -> 'a marked

val unmark_option : 'a marked option -> 'a option

val get_start_line : t -> int

val get_start_column : t -> int

val get_end_line : t -> int

val get_end_column : t -> int

val get_file : t -> string

val indent_number : string -> int

val retrieve_loc_text : t -> string
(** Given a source code position, retrieves the content of the code by acessing
    the file and reading its text *)
