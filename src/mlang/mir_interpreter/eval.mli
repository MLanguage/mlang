(** Interpretation engine. *)

val exit_on_rte : bool ref

(** {2 Engine builder} *)

(** Builds an intepretation engine from a number interface
    ({!module: M_ir.Mir_number}) and a rounding strategy
    ({!module: M_ir.Mir_roundops}). *)
module Make
    (N : M_ir.Mir_number.NumberInterface)
    (RF : M_ir.Mir_roundops.RoundOpsFunctor) :
  Types.S with type custom_float = N.t

(** {2 Engines} *)

(** These modules are instanes of Make with modules defined in
    {!module: M_ir.Mir_number} and {!module: M_ir.Mir_roundops}. *)

module FloatDefInterp : Types.S

module FloatMultInterp : Types.S

module FloatMfInterp : Types.S

module MPFRDefInterp : Types.S

module MPFRMultInterp : Types.S

module MPFRMfInterp : Types.S

module BigIntDefInterp : Types.S

module BigIntMultInterp : Types.S

module BigIntMfInterp : Types.S

module IntvDefInterp : Types.S

module IntvMultInterp : Types.S

module IntvMfInterp : Types.S

module RatDefInterp : Types.S

module RatMultInterp : Types.S

module RatMfInterp : Types.S

val evaluate_program :
  p:M_ir.Mir.program ->
  inputs:M_ir.Com.literal M_ir.Com.Var.Map.t ->
  events:(M_ir.Com.literal, M_ir.Com.Var.t) M_ir.Com.event_value StrMap.t list ->
  sort:Config.value_sort ->
  round_ops:Config.round_ops ->
  M_ir.Com.literal M_ir.Com.Var.Map.t * M_ir.Com.Error.Set.t
(** Evaluates a whole program and returns the given back variables, as
    well as the set of anomalies.
    The evaluation engine is selected from [sort] and [roundops]. *)

val evaluate_expr :
  p:M_ir.Mir.program ->
  e:M_ir.Mir.expression Pos.marked ->
  sort:Config.value_sort ->
  round_ops:Config.round_ops ->
  M_ir.Com.literal
(** Evaluates a single expression.
    The evaluation engine is selected from [sort] and [roundops]. *)
