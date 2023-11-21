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

(** Helpers for parsing *)

(** {1 Frontend variable names}*)

(** A parsed variable can be a regular variable or an integer literal *)
type parse_val = ParseVar of Mast.variable | ParseInt of int

val mk_position : Lexing.position * Lexing.position -> Pos.t

val parse_variable :
  Lexing.position * Lexing.position -> string -> Mast.variable
(** Checks whether the variable contains parameters *)

val parse_variable_name : Lexing.position * Lexing.position -> string -> string
(** Checks whether the string is entirely capitalized *)

val parse_parameter : Lexing.position * Lexing.position -> string -> char

val parse_string : string -> string
(** Removes the quotes *)

val parse_variable_or_int :
  Lexing.position * Lexing.position -> string -> parse_val

val parse_table_index :
  Lexing.position * Lexing.position -> string -> Mast.table_index
(** Table index can be integer or [X], the generic table index variable *)

val parse_table_size : string -> Mast.table_size

val parse_func_name : 'a -> string -> string

(**{1 Literal parsing}*)

val parse_int : Lexing.position * Lexing.position -> string -> int
(** Checks whether is it actually an integer*)

val parse_literal : Lexing.position * Lexing.position -> string -> Mast.literal

val parse_const_value : string -> Mast.literal
