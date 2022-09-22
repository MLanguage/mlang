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
  Bir.program ->
  (* test file name *) string ->
  (* optimize *) bool ->
  (* code coverage *) bool ->
  Cli.value_sort ->
  Cli.round_ops ->
  (* test_error margin *) float ->
  Bir_instrumentation.code_coverage_result
(** [check_test test_file optimize code_coverage value_sort round_ops
    test_error_margin]
    runs the BIR interpreter using float kind [value_sort] and rounding
    operations [round_ops] on a given [test_file]. A margin or error of
    [test_error_margin] is tolerated between the computed values and the
    expected values. [optimize] and [code_coverage] are flags that trigger
    respectively compiler optimizations and code coverage instrumentation for
    the interpreter run. *)

val check_all_tests :
  Bir.program ->
  string ->
  bool ->
  bool ->
  Cli.value_sort ->
  Cli.round_ops ->
  float ->
  unit
(** Similar to [check_test] but tests a whole folder full of test files *)
