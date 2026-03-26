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

open M_ir

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
  module N : Number.S

  module Tracer : Tracers.S

  type value = N.t Types.value

  type ctx_tmp_var = N.t Context.ctx_tmp_var

  type ctx_var_space = N.t Context.ctx_var_space

  type ctx = (N.t, Tracer.ctx) Context.t

  exception RuntimeError of Types.run_error * ctx

  val empty_ctx :
    ?dbg_info:Dbg_info.t ->
    ?inputs:Com.literal Com.Var.Map.t ->
    ?events:(Com.literal, Com.Var.t) Com.event_value StrMap.t list ->
    Mir.program ->
    ctx

  val format_value : Format.formatter -> value -> unit

  val format_value_prec : int -> int -> Format.formatter -> value -> unit

  val get_dbg_info : ctx -> Dbg_info.t option

  val raise_runtime_as_structured : Types.run_error -> 'a

  val evaluate_expr : ctx -> Mir.expression Pos.marked -> value

  val evaluate_program : ctx -> unit
end

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

module Runner : sig
  module NoTracing : sig
    module FloatDefInterp : S

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
  end

  module WithTracing : sig
    module FloatDefInterp : S

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
  end
end

(** {1 Generic interpretation API}*)

val get_interp :
  Config.value_sort -> Config.round_ops -> trace:bool -> (module S)

val evaluate_program :
  ?dbg_info:Dbg_info.t ->
  Mir.program ->
  Com.literal Com.Var.Map.t ->
  (Com.literal, Com.Var.t) Com.event_value StrMap.t list ->
  Config.value_sort ->
  Config.round_ops ->
  Com.literal Com.Var.Map.t * Com.Error.Set.t * Dbg_info.t option
(** Main interpreter function *)

val evaluate_expr :
  ?dbg_info:Dbg_info.t ->
  Mir.program ->
  Mir.expression Pos.marked ->
  Config.value_sort ->
  Config.round_ops ->
  Com.literal
(** Interprets only an expression *)
