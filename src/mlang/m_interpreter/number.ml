open M_ir

module BigIntPrecision = struct
  let scaling_factor_bits = ref 64
end

module MainframeLongSize = struct
  let max_long = ref Int64.max_int
end

module type S = sig
  include Mir_number.NumberInterface

  include Mir_roundops.RoundOpsInterface with type t := t

  val to_literal : t Types.value -> Com.literal

  val of_literal : Com.literal -> t Types.value

  val format_value : Format.formatter -> t Types.value -> unit

  val format_value_prec :
    int -> int -> Format.formatter -> t Types.value -> unit
end

module Make
    (N : M_ir.Mir_number.NumberInterface)
    (RF : Mir_roundops.RoundOpsFunctor) =
struct
  include N
  include RF (N)

  let to_literal (l : t Types.value) : Com.literal =
    match l with
    | Undefined -> Com.Undefined
    | Number f -> Com.Float (to_float f)

  let of_literal (l : Com.literal) : t Types.value =
    match l with
    | Com.Undefined -> Undefined
    | Com.Float f -> Number (of_float f)

  let format_value (fmt : Format.formatter) (x : N.t Types.value) =
    match x with
    | Undefined -> Com.format_literal fmt Com.Undefined
    | Number x -> N.format_t fmt x

  let format_value_prec (mi : int) (ma : int) (fmt : Format.formatter)
      (x : N.t Types.value) =
    match x with
    | Undefined -> Com.format_literal fmt Com.Undefined
    | Number x -> N.format_prec_t mi ma fmt x
end

module FloatDef =
  Make (Mir_number.RegularFloatNumber) (Mir_roundops.DefaultRoundOps)
module FloatMult =
  Make (Mir_number.RegularFloatNumber) (Mir_roundops.DefaultRoundOps)
module FloatMf =
  Make
    (Mir_number.RegularFloatNumber)
    (Mir_roundops.MainframeRoundOps (MainframeLongSize))
module MPFRDef = Make (Mir_number.MPFRNumber) (Mir_roundops.DefaultRoundOps)
module MPFRMult = Make (Mir_number.MPFRNumber) (Mir_roundops.MultiRoundOps)
module MPFRMf =
  Make
    (Mir_number.MPFRNumber)
    (Mir_roundops.MainframeRoundOps (MainframeLongSize))
module BigIntDef =
  Make
    (Mir_number.BigIntFixedPointNumber
       (BigIntPrecision))
       (Mir_roundops.DefaultRoundOps)
module BigIntMult =
  Make
    (Mir_number.BigIntFixedPointNumber
       (BigIntPrecision))
       (Mir_roundops.MultiRoundOps)
module BigIntMf =
  Make
    (Mir_number.BigIntFixedPointNumber
       (BigIntPrecision))
       (Mir_roundops.MainframeRoundOps (MainframeLongSize))
module IntvDef = Make (Mir_number.IntervalNumber) (Mir_roundops.DefaultRoundOps)
module IntvMult = Make (Mir_number.IntervalNumber) (Mir_roundops.MultiRoundOps)
module IntvMf =
  Make
    (Mir_number.IntervalNumber)
    (Mir_roundops.MainframeRoundOps (MainframeLongSize))
module RatDef = Make (Mir_number.RationalNumber) (Mir_roundops.DefaultRoundOps)
module RatMult = Make (Mir_number.RationalNumber) (Mir_roundops.MultiRoundOps)
module RatMf =
  Make
    (Mir_number.RationalNumber)
    (Mir_roundops.MainframeRoundOps (MainframeLongSize))

let setup_precision (sort : Config.value_sort) (roundops : Config.round_ops) :
    unit =
  begin match sort with
  | MPFR prec -> Mpfr.set_default_prec prec
  | BigInt prec -> BigIntPrecision.scaling_factor_bits := prec
  | Interval -> Mpfr.set_default_prec 64
  | _ -> ()
  end;
  match roundops with
  | ROMainframe long_size ->
      let max_long =
        if long_size = 32 then Int64.of_int32 Int32.max_int
        else if long_size = 64 then Int64.max_int
        else assert false
        (* checked when parsing command line *)
      in
      MainframeLongSize.max_long := max_long
  | _ -> ()
