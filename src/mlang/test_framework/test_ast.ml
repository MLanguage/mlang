(* Copyright Inria, contributors: Raphaël Monat <raphael.monat@lip6.fr> (2019)

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

type value =
  | Int of int
  | Float of float

type discord = (string * Pos.t)

type input = (string * value * Pos.t)

type rappel = {
  num_evt: int;
  num_rap: int;
  variable: string;
  value: int;
  sens: string; (* R, C, M, P *)
  penalite: int option; (* 0 - 99 *)
  base_tl: int option;
  date_inr: int; (* MMYYYY *)
  ind20: int option; (* 0 or 1 *)
  pos: Pos.t
}

type test_file = {
  nom : string;
  ep : input list;
  cp : discord list;
  rp : input list;
  corr : (input list * discord list * input list) option;
  rapp : (rappel list * discord list * input list) option;
}
