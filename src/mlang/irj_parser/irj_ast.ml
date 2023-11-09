(* Copyright Inria, contributors: Raphaël Monat <raphael.monat@lip6.fr> (2019)
   Mathieu Durero <mathieu.durero@dgfip.finances.gouv.fr> (2023)

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

type literal = I of int | F of float

type var_value = string * literal * Pos.t

type var_values = var_value list

type errors = (string * Pos.t) list

type rappels =
  (string * string * var_value * string * string * string * string * string)
  list

type prim_data_block = {
  entrees : var_values;
  controles_attendus : errors;
  resultats_attendus : var_values;
}

type corr_data_block = {
  entrees_rappels : rappels;
  controles_attendus : errors;
  resultats_attendus : var_values;
}

type irj_file = {
  nom : string;
  prim : prim_data_block;
  rapp : corr_data_block option;
}
