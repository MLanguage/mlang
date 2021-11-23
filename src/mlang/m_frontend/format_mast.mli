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

val format_comp_op : Format.formatter -> Mast.comp_op -> unit

val format_binop : Format.formatter -> Mast.binop -> unit

val format_unop : Format.formatter -> Mast.unop -> unit

val format_variable : Format.formatter -> Mast.variable -> unit

val format_rule_name : Format.formatter -> Mast.rule_name -> unit

val format_source_file : Format.formatter -> Mast.source_file -> unit

val pp_print_list_endline :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

val pp_print_list_comma :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

val pp_unmark : ('a -> 'b -> 'c) -> 'a -> 'b Pos.marked -> 'c
