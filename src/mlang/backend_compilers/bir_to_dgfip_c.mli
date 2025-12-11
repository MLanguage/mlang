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

(** This module with a single entry point generates C files from a
    {!Bir.program}. *)

(** The optimized code generation for M code, which represents the vast majority
    of the output, is built in {!DecoupledExpr}. *)

val generate_c_program :
  Config.Dgfip_options.flags -> Mir.program -> (* filename *) string -> unit
