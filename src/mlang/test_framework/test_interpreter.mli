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
(** [check_test program test_file value_sort round_ops] runs a single test case.

    It parses the [test_file], executes the [program] with the inputs specified
    in the file, and compares the final variable states and generated errors
    against the expected outcomes. It prints "OK!" on success or "KO!" with
    details on failure.

    @param program The `Mir` program to be tested.
    @param test_file The path to the `.irj` test file.
    @param value_sort The interpretation mode (e.g., symbolic or concrete).
    @param round_ops The rounding operations to use for floating-point arithmetic.
    @raise InterpError if the test fails with a specific number of errors.
    @raise Errors.StructuredError for parsing or setup errors in the test file. *)

val check_all_tests :
  Mir.program ->
  string ->
  Cli.value_sort ->
  Cli.round_ops ->
  (string -> bool) ->
  unit
(** [check_all_tests program test_dir value_sort round_ops filter] runs all
    test files in a given directory.

    It reads all files in [test_dir], applies the [filter] function to select
    which files to run, and then executes each one in parallel. It aggregates
    and prints a summary of successes and failures.

    @param program The `Mir` program to be tested.
    @param test_dir The directory containing the `.irj` test files.
    @param value_sort The interpretation mode.
    @param round_ops The rounding operations to use.
    @param filter A function that returns [true] for filenames that should be
                  included in the test run. *)

val check_one_test :
  Mir.program -> string -> Cli.value_sort -> Cli.round_ops -> unit
(** [check_one_test program test_file value_sort round_ops] runs a single
    test file and reports its success or failure.

    This is a wrapper around [check_test] intended for running a specific test
    case individually. It catches exceptions and prints a summary of the outcome.

    @param program The `Mir` program to be tested.
    @param test_file The path to the `.irj` test file.
    @param value_sort The interpretation mode.
    @param round_ops The rounding operations to use. *)
