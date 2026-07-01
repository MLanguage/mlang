(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2026 DGFiP - INRIA                                      *)
(*                                                                            *)
(* Ce programme est distribué sous la licence CeCILL-C: vous pouvez le        *)
(* redistribuer et/ou le modifier sous les contraintes de celle-ci.           *)
(*                                                                            *)
(* L'accessibilité au code source et les droits de copie, de modification et  *)
(* de redistribution qui découlent de ce contrat ont pour contrepartie de     *)
(* n'offrir aux utilisateurs qu'une garantie limitée et de ne faire peser sur *)
(* l'auteur du logiciel, le titulaire des droits patrimoniaux et les          *)
(* concédants successifs qu'une responsabilité restreinte.                    *)
(*                                                                            *)
(******************************************************************************)

(** This module type merges the {!module M_ir.Mir_number} and
    {!module M_ir.Mir_roundops} interface to provide a unique module for
    manipulating values. *)
module type S = sig
  include M_ir.Mir_number.NumberInterface

  include M_ir.Mir_roundops.RoundOpsInterface with type t := t

  val to_literal : t Types.value -> M_ir.Com.literal
  (** Translates a value into an M literal *)

  val of_literal : M_ir.Com.literal -> t Types.value
  (** Translates a M literal into a value *)

  val format_value : Format.formatter -> t Types.value -> unit
  (** Pretty printer for a value *)

  val format_value_prec :
    int -> int -> Format.formatter -> t Types.value -> unit
  (** Pretty printer for a value, with min/max precision *)
end

module Make
    (N : M_ir.Mir_number.NumberInterface)
    (RF : M_ir.Mir_roundops.RoundOpsFunctor) : S with type t = N.t

module FloatDef : S with type t = float
(** Float with default rounding strategy. *)

module FloatMult : S with type t = float
(** Float with multithread rounding strategy. *)

module FloatMf : S with type t = float
(** Float with mainframe rounding strategy. *)

module MPFRDef : S with type t = Mpfrf.t
(** Multiple-precision floating-point with default rounding strategy. *)

module MPFRMult : S with type t = Mpfrf.t
(** Multiple-precision floating-point with multithread rounding strategy. *)

module MPFRMf : S with type t = Mpfrf.t
(** Multiple-precision floating-point with mainframe rounding strategy. *)

module BigIntDef : S with type t = Mpzf.t
(** Multiple-precision floating-point with mainframe rounding strategy. *)

module BigIntMult : S with type t = Mpzf.t
(** Multiple precision integer arithmetic with multihtread rounding strategy. *)

module BigIntMf : S with type t = Mpzf.t
(** Multiple precision integer arithmetic with mainframe rounding strategy. *)

module IntvDef : S with type t = M_ir.Mir_number.interval
(** Multiple-precision floating-point intervals with default rounding strategy.
*)

module IntvMult : S with type t = M_ir.Mir_number.interval
(** Multiple-precision floating-point intervals with multithread rounding
    strategy. *)

module IntvMf : S with type t = M_ir.Mir_number.interval
(** Multiple-precision floating-point intervals with mainframe rounding
    strategy. *)

module RatDef : S with type t = Mpqf.t
(** Multiple-precision rationals with default rounding strategy. *)

module RatMult : S with type t = Mpqf.t
(** Multiple-precision rationals with multithread rounding strategy. *)

module RatMf : S with type t = Mpqf.t
(** Multiple-precision rationals with mainframe rounding strategy. *)

val setup_precision : Config.value_sort -> Config.round_ops -> unit
(** Initializes the precision and the value sort for the different libraries
    used for calculations. *)
