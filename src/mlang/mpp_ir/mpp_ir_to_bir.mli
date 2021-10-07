(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr> Raphël
   Monat <raphael.monat@lip6.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

val create_combined_program :
  Mir_interface.full_program ->
  Mpp_ir.mpp_program ->
  (* function to extract *) string ->
  Bir.program
