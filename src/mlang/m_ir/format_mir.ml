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

open Mir

let format_typ fmt (t : typ) =
  Format.pp_print_string fmt (match t with Real -> "real")

let format_variable fmt (var : Var.t) =
  Format.fprintf fmt "%s" (Pos.unmark var.name)

let format_expression = Com.format_expression format_variable

let format_error fmt (err : Com.Error.t) =
  Format.fprintf fmt "erreur %s (%a)" (Pos.unmark err.name) Com.Error.pp_descr
    err

let format_variable fmt (v : Var.t) =
  Format.fprintf fmt "%s: %s" (Pos.unmark v.name) (Var.descr_str v)
