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

(**{1 Instrumentation of the interpreter}*)

(** The BIR interpreter can be instrumented to record which program locations
    have been executed. *)

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

  type ctx_tmp_var = { mutable var : Com.Var.t; mutable value : value }

  type ctx_ref_var = {
    mutable var : Com.Var.t;
    mutable var_space : Com.variable_space;
    mutable ref_var : Com.Var.t;
    mutable org : int;
  }

  type print_ctx = { mutable indent : int; mutable is_newline : bool }

  type ctx_var_space = {
    input : value Array.t;
    computed : value Array.t;
    base : value Array.t;
  }

  type ctx = {
    ctx_prog : Mir.program;
    mutable ctx_target : Mir.target;
    mutable ctx_var_space : int;
    ctx_var_spaces : ctx_var_space Array.t;
    ctx_tmps : ctx_tmp_var Array.t;
    mutable ctx_tmps_org : int;
    ctx_ref : ctx_ref_var Array.t;
    mutable ctx_ref_org : int;
    ctx_tab_map : Com.Var.t Array.t;
    ctx_pr_out : print_ctx;
    ctx_pr_err : print_ctx;
    mutable ctx_anos : (Com.Error.t * string option) list;
    mutable ctx_old_anos : StrSet.t;
    mutable ctx_nb_anos : int;
    mutable ctx_nb_discos : int;
    mutable ctx_nb_infos : int;
    mutable ctx_nb_bloquantes : int;
    mutable ctx_finalized_anos : (Com.Error.t * string option) list;
    mutable ctx_exported_anos : (Com.Error.t * string option) list;
    mutable ctx_events : (value, Com.Var.t) Com.event_value Array.t Array.t list;
  }
  (** Interpretation context *)

  val empty_ctx : Mir.program -> ctx

  val literal_to_value : Com.literal -> value

  val value_to_literal : value -> Com.literal

  val update_ctx_with_inputs : ctx -> Com.literal Com.Var.Map.t -> unit

  val update_ctx_with_events :
    ctx -> (Com.literal, Com.Var.t) Com.event_value StrMap.t list -> unit

  (** Interpreter runtime errors *)
  type run_error =
    | NanOrInf of string * Mir.expression Pos.marked
    | StructuredError of
        (string * (string option * Pos.t) list * (unit -> unit) option)

  exception RuntimeError of run_error * ctx

  val raise_runtime_as_structured : run_error -> 'a
  (** Raises a runtime error with a formatted error message and context *)

  val compare_numbers : Com.comp_op -> custom_float -> custom_float -> bool
  (** Returns the comparison between two numbers in the rounding and precision
      context of the interpreter. *)

  val evaluate_expr : ctx -> Mir.expression Pos.marked -> value

  val evaluate_program : ctx -> unit
end

module FloatDefInterp :
  S with type custom_float = Mir_number.RegularFloatNumber.t
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

module FloatMultInterp :
  S with type custom_float = Mir_number.RegularFloatNumber.t

module FloatMfInterp :
  S with type custom_float = Mir_number.RegularFloatNumber.t

module MPFRDefInterp : S with type custom_float = Mir_number.MPFRNumber.t

module MPFRMultInterp : S with type custom_float = Mir_number.MPFRNumber.t

module MPFRMfInterp : S with type custom_float = Mir_number.MPFRNumber.t

module BigIntDefInterp : S

module BigIntMultInterp : S

module BigIntMfInterp : S

module IntvDefInterp : S with type custom_float = Mir_number.IntervalNumber.t

module IntvMultInterp : S with type custom_float = Mir_number.IntervalNumber.t

module IntvMfInterp : S with type custom_float = Mir_number.IntervalNumber.t

module RatDefInterp : S with type custom_float = Mir_number.RationalNumber.t

module RatMultInterp : S with type custom_float = Mir_number.RationalNumber.t

module RatMfInterp : S with type custom_float = Mir_number.RationalNumber.t

(** {1 Generic interpretation API}*)

val get_interp : Cli.value_sort -> Cli.round_ops -> (module S)

val evaluate_program :
  Mir.program ->
  Com.literal Com.Var.Map.t ->
  (Com.literal, Com.Var.t) Com.event_value StrMap.t list ->
  Cli.value_sort ->
  Cli.round_ops ->
  Com.literal Com.Var.Map.t * Com.Error.Set.t
(** Main interpreter function *)

val evaluate_expr :
  Mir.program ->
  Mir.expression Pos.marked ->
  Cli.value_sort ->
  Cli.round_ops ->
  Com.literal
(** Interprets only an expression *)
