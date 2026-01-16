(* This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

val proceed : Mast.program -> Mast.program
(** Expands the program by:
    - keeping elements that only belong to the selected applications defined in
      {!Utils.Config.application_names};
    - inlining constant's values and removes their definition;
    - unrolling loops on variable names (such as "sum(i=05,06,07:Xi)" that
      becomes "sum(x05, X06, X07)". *)
