(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** A parsed variable can be a regular variable or an integer literal *)
type parse_val = ParseVar of Mast.variable | ParseInt of int

val mk_position : Lexing.position * Lexing.position -> Pos.t

val parse_variable : Lexing.position * Lexing.position -> string -> Mast.variable

val parse_variable_name : Lexing.position * Lexing.position -> string -> string

val parse_string : string -> string

val parse_int : Lexing.position * Lexing.position -> string -> int

val parse_literal : Lexing.position * Lexing.position -> string -> Mast.literal

val parse_variable_or_int : Lexing.position * Lexing.position -> string -> parse_val

val parse_table_index : Lexing.position * Lexing.position -> string -> Mast.table_index

val parse_func_name : 'a -> string -> string
