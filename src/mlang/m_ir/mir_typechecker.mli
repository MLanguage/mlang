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

(** This modules defines M's static semantic. There is only one type: float. The
    typechecking is mostly about differentiating table from non-tables variables *)

(* The typechecker returns a new program because it defines missing table
   entries as "undefined" *)
val typecheck : Mir_interface.full_program -> Mir_interface.full_program

val expand_functions_expr :
  'var Mir.expression_ Pos.marked -> 'var Mir.expression_ Pos.marked
(** Most functions are just syntactic sugar for operations expressible with the
    rest of the language, so we expand these. *)

val expand_functions : Mir_interface.full_program -> Mir_interface.full_program
(** Calls [expand_functions_expr] on the whole program *)
