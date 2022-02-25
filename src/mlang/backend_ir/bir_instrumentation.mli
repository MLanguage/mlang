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

(** Instrumentation of the interpreter to computer code coverage *)

(** {1 Code coverage for a single run}*)

module CodeLocationMap : Map.S with type key = Bir_interpreter.code_location

type code_coverage_result =
  Bir_interpreter.var_literal CodeLocationMap.t Bir.VariableMap.t
(** For each variable, and for each code location where it is assigned, we
    record the value it has been assigned to during an interpreter run *)

val empty_code_coverage_result : code_coverage_result

(** The code coverage is stateful, it has to be initialized before the run with
    [code_coverage_init] and you can retrieve its results after with
    [code_coverage_result]. *)

val code_coverage_init : unit -> unit

val code_coverage_result : unit -> code_coverage_result

(** {1 Code coverage for multiple runs}*)

(** Code coverage is best measured for multiple runs of the interpreter on a set
    of test files. *)

module VarLiteralSet : Set.S with type elt = Bir_interpreter.var_literal

type code_coverage_map_value = VarLiteralSet.t

type code_coverage_acc =
  code_coverage_map_value CodeLocationMap.t Bir.VariableMap.t
(** The accumulated coverage is the set of distinct values a particular variable
    assignment has received in the tests runs so far *)

val merge_code_coverage_single_results_with_acc :
  code_coverage_result -> code_coverage_acc -> code_coverage_acc
(** [merge_code_coverage_single_results_with_acc result acc] merges the code
    coverage results of a single run [result] with the accumulated results over
    the tests so far [acc] *)

val merge_code_coverage_acc :
  code_coverage_acc -> code_coverage_acc -> code_coverage_acc
(** Merges two partial code coverage accumulator into a single, bigger one *)

(** {1 Code locations}*)

type code_locs = Bir.variable CodeLocationMap.t

val get_code_locs : Bir.program -> code_locs
(** Returns all code locations in a program *)
