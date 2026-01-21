(** Interpretation engine. *)

val exit_on_rte : bool ref

(** {2 Generic evaluation functions} *)

val evaluate_program :
  p:M_ir.Mir.program ->
  inputs:M_ir.Com.literal M_ir.Com.Var.Map.t ->
  events:(M_ir.Com.literal, M_ir.Com.Var.t) M_ir.Com.event_value StrMap.t list ->
  sort:Config.value_sort ->
  round_ops:Config.round_ops ->
  M_ir.Com.literal M_ir.Com.Var.Map.t * M_ir.Com.Error.Set.t
(** Evaluates a whole program and returns the given back variables, as well as
    the set of anomalies. The evaluation engine is selected from [sort] and
    [roundops]. *)

val evaluate_expr :
  p:M_ir.Mir.program ->
  e:M_ir.Mir.expression Pos.marked ->
  sort:Config.value_sort ->
  round_ops:Config.round_ops ->
  M_ir.Com.literal
(** Evaluates a single expression. The evaluation engine is selected from [sort]
    and [roundops]. *)

(** {2 Generic module type} *)

module type S = sig
  type custom_float

  type ctx = custom_float Types.ctx

  exception InternalRuntimeError of Types.run_error * ctx

  (** {2 M Evaluation} *)

  val evaluate_expr :
    ctx -> M_ir.Mir.expression Pos.marked -> custom_float Types.value
  (** Evaluates an expression. *)

  val evaluate_program : ctx -> unit
  (** Evaluates a whole program. Proper initialisation of inputs and events is
      required before calling this function (through [update_ctx_with_inputs]
      and [update_ctx_with_events]. *)

  (** {2 Helpers} *)

  (** These helpers are here for compatibility with {!module: Context}. *)

  val literal_to_value : M_ir.Com.literal -> custom_float Types.value

  val value_to_literal : custom_float Types.value -> M_ir.Com.literal

  val literal_event_to_value_event :
    (M_ir.Com.literal, M_ir.Com.Var.t) M_ir.Com.event_value ->
    (custom_float Types.value, M_ir.Com.Var.t) M_ir.Com.event_value
end

(** {2 Engine builder} *)

(** Builds an intepretation engine from a number interface
    ({!module: M_ir.Mir_number}) and a rounding strategy
    ({!module: M_ir.Mir_roundops}). *)
module Make (N : Number.S) : S with type custom_float = N.t

(** {2 Engines} *)

(** These modules are instanes of Make with modules defined in
    {!module: M_ir.Mir_number} and {!module: M_ir.Mir_roundops}. *)

module FloatDefInterp : S with type custom_float = float
(** Float with default rounding strategy. *)

module FloatMultInterp : S with type custom_float = float
(** Float with multithread rounding strategy. *)

module FloatMfInterp : S with type custom_float = float
(** Float with mainframe rounding strategy. *)

module MPFRDefInterp : S with type custom_float = Mpfrf.t
(** Multiple-precision floating-point with default rounding strategy. *)

module MPFRMultInterp : S with type custom_float = Mpfrf.t
(** Multiple-precision floating-point with multithread rounding strategy. *)

module MPFRMfInterp : S with type custom_float = Mpfrf.t
(** Multiple-precision floating-point with mainframe rounding strategy. *)

module BigIntDefInterp : S with type custom_float = Mpzf.t
(** Multiple precision integer arithmetic with default rounding strategy. *)

module BigIntMultInterp : S with type custom_float = Mpzf.t
(** Multiple precision integer arithmetic with multihtread rounding strategy. *)

module BigIntMfInterp : S with type custom_float = Mpzf.t
(** Multiple precision integer arithmetic with mainframe rounding strategy. *)

module IntvDefInterp : S with type custom_float = M_ir.Mir_number.interval
(** Multiple-precision floating-point intervals with default rounding strategy. *)

module IntvMultInterp : S with type custom_float = M_ir.Mir_number.interval
(** Multiple-precision floating-point intervals with multithread rounding strategy. *)

module IntvMfInterp : S with type custom_float = M_ir.Mir_number.interval
(** Multiple-precision floating-point intervals with mainframe rounding strategy. *)

module RatDefInterp : S with type custom_float = Mpqf.t
(** Multiple-precision rationals with default rounding strategy. *)

module RatMultInterp : S with type custom_float = Mpqf.t
(** Multiple-precision rationals with multithread rounding strategy. *)

module RatMfInterp : S with type custom_float = Mpqf.t
(** Multiple-precision rationals with mainframe rounding strategy. *)
