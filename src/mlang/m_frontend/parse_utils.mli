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
type parse_val = ParseVar of Com.var_name | ParseInt of int

val mk_position : Lexing.position * Lexing.position -> Pos.t

val parse_variable : Lexing.position * Lexing.position -> string -> Com.var_name
(** Checks whether the variable contains parameters *)

val parse_variable_name : Lexing.position * Lexing.position -> string -> string
(** Checks whether the string is entirely capitalized *)

val parse_parameter : Lexing.position * Lexing.position -> string -> char

val parse_variable_or_int :
  Lexing.position * Lexing.position -> string -> parse_val

val parse_table_size : string -> Mast.table_size

val parse_func_name : 'a -> string -> string

(**{1 Literal parsing}*)

val parse_int : Lexing.position * Lexing.position -> string -> int
(** Checks whether is it actually an integer*)

val parse_literal : Lexing.position * Lexing.position -> string -> Com.literal

val parse_atom :
  Lexing.position * Lexing.position -> string -> Com.m_var_name Com.atom

val parse_to_atom : parse_val -> Pos.t -> Com.m_var_name Com.atom

val parse_function_name : string Pos.marked -> Com.func Pos.marked

val parse_index_format : string Pos.marked -> string Pos.marked

val parse_if_then_etc :
  (Mast.expression Pos.marked option * Mast.instruction Pos.marked list * Pos.t)
  list ->
  Mast.instruction

val parse_when_do_etc :
  (Mast.expression Pos.marked * Mast.instruction Pos.marked list * Pos.t) list
  * Mast.instruction Pos.marked list Pos.marked ->
  Mast.instruction

type target_header =
  | Target_apps of Mast.application Pos.marked list
  | Target_input_arg of string Pos.marked list
  | Target_tmp_vars of
      (string Pos.marked * Mast.table_size Pos.marked option) list
  | Function_result of string Pos.marked

val parse_target_or_function_header :
  string Pos.marked ->
  bool ->
  target_header Pos.marked list ->
  Mast.application Pos.marked StrMap.t
  * string Pos.marked list
  * (string Pos.marked * Mast.table_size Pos.marked option) list
  * string Pos.marked option
