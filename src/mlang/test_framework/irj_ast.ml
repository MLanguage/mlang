(* Copyright Inria, contributors: Raphaël Monat <raphael.monat@lip6.fr> (2019)
   Mathieu Durero <mathieu.durero@dgfip.finances.gouv.fr>, David Declerck (2023)

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

type pos = {
  pos_filename : string;
  pos_loc : Lexing.position * Lexing.position;
}

let mk_position sloc =
  { pos_filename = (fst sloc).Lexing.pos_fname; pos_loc = sloc }

exception TestParsingError of (string * pos)
(* duplication of some of the utils *)

type literal = I of int | F of float

type var_value = string * literal * pos

(* type var_values = var_value list *)

type calc_error = string * pos

(* type calc_errors = calc_error list *)

(* type rappel = string * string * var_value * string * string * string * string
   * string *)
type rappel = {
  event_nb : int;
  rappel_nb : int;
  variable_code : string;
  change_value : int;
  direction : string;
  (* R, C, M, P *)
  penalty_code : int option;
  (* 0 - 99 *)
  base_tolerance_legale : int option;
  month_year : int;
  (* MMYYYY *)
  decl_2042_rect : int option;
  (* 0 or 1 *)
  pos : pos;
}

type prim_data_block = {
  entrees : var_value list;
  controles_attendus : calc_error list;
  resultats_attendus : var_value list;
}

type corr_data_block = {
  entrees_rappels : rappel list;
  controles_attendus : calc_error list;
  resultats_attendus : var_value list;
}

type irj_file = {
  nom : string;
  prim : prim_data_block;
  rapp : corr_data_block option;
      (* corr : prim_data_block option; *)
      (*corr is for old correctif form from primitif files, rapp is for the
        actual one in correctif files*)
}
