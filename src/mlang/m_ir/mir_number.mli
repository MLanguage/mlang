(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2021 - 2026 DGFiP - INRIA                               *)
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

type interval = { down : Mpfrf.t; up : Mpfrf.t }

module type NumberInterface = sig
  type t

  val format_t : Format.formatter -> t -> unit

  val format_prec_t : int -> int -> Format.formatter -> t -> unit

  val abs : t -> t

  val floor : t -> t

  val ceil : t -> t

  val of_int : Int64.t -> t

  val to_int : t -> Int64.t

  val of_float : float -> t

  val to_float : t -> float

  val zero : unit -> t

  val one : unit -> t

  val ( =. ) : t -> t -> bool

  val ( >=. ) : t -> t -> bool

  val ( >. ) : t -> t -> bool

  val ( <. ) : t -> t -> bool

  val ( <=. ) : t -> t -> bool

  val ( +. ) : t -> t -> t

  val ( -. ) : t -> t -> t

  val ( /. ) : t -> t -> t

  val ( *. ) : t -> t -> t

  val ( %. ) : t -> t -> t

  val min : t -> t -> t

  val max : t -> t -> t

  val is_nan_or_inf : t -> bool

  val is_zero : t -> bool

  val compare : ?epsilon:float -> Com.comp_op -> t -> t -> bool
  (** Returns the comparison between two numbers in the precision context of the
      current configuration. *)
end

module RegularFloatNumber : NumberInterface with type t = float

val mpfr_floor : Mpfrf.t -> Mpfrf.t

module MPFRNumber : NumberInterface with type t = Mpfrf.t

module IntervalNumber : NumberInterface with type t = interval

module RationalNumber : NumberInterface with type t = Mpqf.t

module BigIntFixedPointNumber : functor
  (_ : sig
     val scaling_factor_bits : int ref
   end)
  -> NumberInterface with type t = Mpzf.t
