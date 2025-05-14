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

  val of_float_input : float -> t

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

  val min : t -> t -> t

  val max : t -> t -> t

  val is_nan_or_inf : t -> bool

  val is_zero : t -> bool
end

module RegularFloatNumber : NumberInterface

val mpfr_floor : Mpfrf.t -> Mpfrf.t

module MPFRNumber : NumberInterface

module IntervalNumber : NumberInterface

module RationalNumber : NumberInterface

module BigIntFixedPointNumber : functor
  (_ : sig
     val scaling_factor_bits : int ref
   end)
  -> NumberInterface
