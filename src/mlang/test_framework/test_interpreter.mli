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

val check_test :
  Mir.program ->
  (* test file name *) string ->
  Cli.value_sort ->
  Cli.round_ops ->
  unit

val check_all_tests :
  Mir.program ->
  string ->
  Cli.value_sort ->
  Cli.round_ops ->
  (string -> bool) ->
  unit
(** Similar to [check_test] but tests a whole folder full of test files *)

val check_one_test :
  Mir.program -> string -> Cli.value_sort -> Cli.round_ops -> unit
