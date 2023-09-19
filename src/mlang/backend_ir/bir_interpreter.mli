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

(** Interpretation of BIR programs *)

(**{1 Program values}*)

(* Type of the values being passed around in the interpreter **)
type var_literal =
  | SimpleVar of Mir.literal
  | TableVar of int * Mir.literal array

(**{1 Instrumentation of the interpreter}*)

(** The BIR interpreter can be instrumented to record which program locations
    have been executed. *)

(** Representation of each program location segment *)
type code_location_segment =
  | InsideBlock of int
  | ConditionalBranch of bool
  | InsideRule of Bir.rov_id
  | InsideFunction of Bir.function_name

val format_code_location_segment :
  Format.formatter -> code_location_segment -> unit

type code_location = code_location_segment list
(** A program location is simply the path inside the program *)

val format_code_location : Format.formatter -> code_location -> unit

val assign_hook :
  (Bir.variable -> (unit -> var_literal) -> code_location -> unit) ref
(** The instrumentation of the interpreter is done through this reference. The
    function that you assign to this reference will be called each time a
    variable assignment is executed *)

val exit_on_rte : bool ref
(** If set to true, the interpreter exits the whole process in case of runtime
    error *)

val repl_debug : bool ref
(** If set to true, prints the REPL debugger in case of runtime error *)

(** {1 The interpreter functor}*)

(** The intepreter is parametrized by the kind of floating-point values used for
    the execution *)

(** Signature of the modules produced by the functor *)
module type S = sig
  type custom_float
  (** Comes from the instantiation of the functor by a kind of floating-point
      value *)

  (** Functor-specific program values *)
  type value = Number of custom_float | Undefined

  val format_value : Format.formatter -> value -> unit

  val format_value_prec : int -> int -> Format.formatter -> value -> unit

  (** Functor-specific variable values *)
  type var_value = SimpleVar of value | TableVar of int * value array

  val format_var_value : Format.formatter -> var_value -> unit

  val format_var_value_prec :
    int -> int -> Format.formatter -> var_value -> unit

  val format_var_value_with_var :
    Format.formatter -> Bir.variable * var_value -> unit

  type ctx = {
    ctx_local_vars : value Pos.marked Mir.LocalVariableMap.t;
    ctx_vars : var_value Bir.VariableMap.t;
  }
  (** Interpretation context *)

  val empty_ctx : ctx

  val literal_to_value : Mir.literal -> value

  val var_literal_to_var_value : var_literal -> var_value

  val value_to_literal : value -> Mir.literal

  val var_value_to_var_literal : var_value -> var_literal

  val update_ctx_with_inputs : ctx -> Mir.literal Bir.VariableMap.t -> ctx

  (** Interpreter runtime errors *)
  type run_error =
    | ErrorValue of string * Pos.t
    | FloatIndex of string * Pos.t
    | IndexOutOfBounds of string * Pos.t
    | IncorrectOutputVariable of string * Pos.t
    | UnknownInputVariable of string * Pos.t
    | ConditionViolated of
        Mir.Error.t
        * Bir.expression Pos.marked
        * (Bir.variable * var_value) list
    | NanOrInf of string * Bir.expression Pos.marked
    | StructuredError of
        (string * (string option * Pos.t) list * (unit -> unit) option)

  exception RuntimeError of run_error * ctx

  val replace_undefined_with_input_variables :
    Mir.program -> Mir.VariableDict.t -> Mir.program
  (** Before execution of the program, replaces the [undefined] stubs for input
      variables by their true input value *)

  val print_output : Bir_interface.bir_function -> ctx -> unit

  val raise_runtime_as_structured : run_error -> ctx -> Mir.program -> 'a
  (** Raises a runtime error with a formatted error message and context *)

  val evaluate_expr : ctx -> Mir.program -> Bir.expression Pos.marked -> value

  val evaluate_program : Bir.program -> ctx -> int -> ctx
end

module FloatDefInterp : S
(** The different interpreters, which combine a representation of numbers and
    rounding operations. The first part of the name corresponds to the
    representation of numbers, and is one of the following:

    - Float: "regular" IEE754 floating point numbers
    - MPFR: arbitrary precision floating-point numbers using MPFR
    - BigInt: fixed-point numbers
    - Intv: intervals of two IEEE754 floating-point numbers
    - Rat: rationals

    The second part indicates the rounding operations to use, and is one of the
    following:

    - Def: use the default rounding operations, those of the PC/single-thread
      context
    - Multi: use the rouding operations of the PC/multi-thread context
    - Mf: use the rounding operations of the mainframe context *)

module FloatMultInterp : S

module FloatMfInterp : S

module MPFRDefInterp : S

module MPFRMultInterp : S

module MPFRMfInterp : S

module BigIntDefInterp : S

module BigIntMultInterp : S

module BigIntMfInterp : S

module IntvDefInterp : S

module IntvMultInterp : S

module IntvMfInterp : S

module RatDefInterp : S

module RatMultInterp : S

module RatMfInterp : S

(** {1 Generic interpretation API}*)

val get_interp : Cli.value_sort -> Cli.round_ops -> (module S)

val evaluate_program :
  Bir_interface.bir_function ->
  Bir.program ->
  Mir.literal Bir.VariableMap.t ->
  int ->
  Cli.value_sort ->
  Cli.round_ops ->
  unit ->
  unit
(** Main interpreter function *)

val evaluate_expr :
  Mir.program ->
  Bir.expression Pos.marked ->
  Cli.value_sort ->
  Cli.round_ops ->
  Mir.literal
(** Interprets only an expression *)
