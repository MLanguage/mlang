(* Copyright (C) 2020 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

module type NumberInterface = sig
  type t

  val format_t : Format.formatter -> t -> unit

  val modf : t -> t * t

  val copysign : t -> t -> t

  val of_int : int -> t

  val to_int : t -> int
  (** Warning: lossy *)

  val of_float : float -> t

  val to_float : t -> float
  (** Warning: lossy *)

  val zero : t

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

module RegularFloatReal : NumberInterface = struct
  type t = float

  let format_t fmt f = Format.fprintf fmt "%f" f

  let modf x = modf x

  let copysign x y = copysign x y

  let of_int i = float_of_int i

  let to_int f = int_of_float f

  let of_float f = f

  let to_float f = f

  let zero = 0.

  let one () = 1.

  let ( =. ) x y = x = y

  let ( >=. ) x y = x >= y

  let ( >. ) x y = x > y

  let ( <. ) x y = x < y

  let ( <=. ) x y = x <= y

  let ( +. ) x y = x +. y

  let ( -. ) x y = x -. y

  let ( /. ) x y = x /. y

  let ( *. ) x y = x *. y

  let min x y = min x y

  let max x y = max x y

  let is_nan_or_inf x = not (Float.is_finite x)

  let is_zero x = x = 0.
end

module MPFRReal : NumberInterface = struct
  type t = Mpfrf.t

  let format_t fmt f = Format.fprintf fmt "%a" Mpfrf.print f

  let modf x =
    let x = Mpfrf.to_mpfr x in
    let frac_part = Mpfr.init () in
    let int_part = Mpfr.init () in
    ignore (Mpfr.modf int_part frac_part x Near);
    (Mpfrf.of_mpfr frac_part, Mpfrf.of_mpfr int_part)

  let copysign x y =
    match (Mpfrf.sgn x, Mpfrf.sgn y) with
    | 0, _ -> x
    | sx, sy when (sx > 0 && sy > 0) || (sx < 0 && sy < 0) || sy = 0 -> x
    | _ -> Mpfrf.sub (Mpfrf.of_int 0 Near) x Near

  let of_int i = Mpfrf.of_int i Near

  let to_int f = int_of_float (Mpfrf.to_float f)

  let of_float f = Mpfrf.of_float f Near

  let to_float f = Mpfrf.to_float ~round:Near f

  let zero = Mpfrf.of_int 0 Near

  let one () = Mpfrf.of_int 1 Near

  let ( =. ) x y = Mpfrf.cmp x y = 0

  let ( >=. ) x y = Mpfrf.cmp x y >= 0

  let ( >. ) x y = Mpfrf.cmp x y > 0

  let ( <. ) x y = Mpfrf.cmp x y < 0

  let ( <=. ) x y = Mpfrf.cmp x y <= 0

  let ( +. ) x y = Mpfrf.add x y Near

  let ( -. ) x y = Mpfrf.sub x y Near

  let ( /. ) x y = Mpfrf.div x y Near

  let ( *. ) x y = Mpfrf.mul x y Near

  let min x y = if x >. y then y else x

  let max x y = if x >. y then x else y

  let is_zero x = x =. zero

  let is_nan_or_inf x = not (Mpfrf.number_p x)
end

module IntervalReal : NumberInterface = struct
  let epsilon = Float.succ @@ Float.succ @@ Float.succ 0.

  type t = Interval_crlibm.t

  let format_t fmt f =
    Format.fprintf fmt "[%.30f;%.30f]" f.Interval_crlibm.low f.Interval_crlibm.high

  let modf x =
    let fpart_low, ipart_low = modf (Interval_crlibm.I.low x) in
    let fpart_high, ipart_high = modf (Interval_crlibm.I.high x) in
    try
      ( Interval_crlibm.I.v (min fpart_low fpart_high) (max fpart_low fpart_high),
        Interval_crlibm.I.v (min ipart_low ipart_high) (max ipart_low ipart_high) )
    with Invalid_argument msg ->
      Errors.raise_error
        (Format.asprintf "Error during interval modf computation of %a, %s" format_t x msg)

  let copysign x y =
    match (Interval_crlibm.I.sgn x, Interval_crlibm.I.sgn y) with
    | sx, _ when Interval_crlibm.I.equal sx Interval_crlibm.I.zero -> x
    | sx, sy
      when Interval_crlibm.I.(sx > zero && sy > zero)
           || Interval_crlibm.I.(sx < zero && sy < zero)
           || Interval_crlibm.I.(equal sy zero) ->
        x
    | _ -> Interval_crlibm.I.( ~- ) x

  let of_int i = Interval_crlibm.I.of_int i

  let to_int f =
    let i_low = int_of_float (Float.round (Interval_crlibm.I.low f)) in
    let i_high = int_of_float (Float.round (Interval_crlibm.I.high f)) in
    if i_low = i_high then i_low
    else
      Errors.raise_error
        (Format.asprintf "converting %a to int, too much imprecision (low: %d, high: %d)!" format_t
           f i_low i_high)

  let of_float (f : float) = Interval_crlibm.I.v (f -. epsilon) (f +. epsilon)

  let to_float f = (Interval_crlibm.I.low f +. Interval_crlibm.I.high f) /. 2.

  let zero = Interval_crlibm.I.v (0. -. epsilon) (0. +. epsilon)

  let one () = Interval_crlibm.I.v (1. -. epsilon) (1. +. epsilon)

  let ( =. ) x y = Interval_crlibm.I.equal x y

  let ( >=. ) x y = Interval_crlibm.I.( >= ) x y

  let ( >. ) x y = Interval_crlibm.I.( > ) x y

  let ( <. ) x y = Interval_crlibm.I.( < ) x y

  let ( <=. ) x y = Interval_crlibm.I.( <= ) x y

  let ( +. ) x y = Interval_crlibm.I.( + ) x y

  let ( -. ) x y = Interval_crlibm.I.( - ) x y

  let ( /. ) x y = Interval_crlibm.I.( / ) x y

  let ( *. ) x y = Interval_crlibm.I.( * ) x y

  let min x y = Interval_crlibm.I.min x y

  let max x y = Interval_crlibm.I.max x y

  let is_zero x = Interval_crlibm.I.subset zero x

  let is_nan_or_inf x = Interval_crlibm.I.is_entire x
end

module BigIntFixedPointReal (P : sig
  val bit_size_of_int : int ref
end) : NumberInterface = struct
  type t = Mpzf.t

  let precision_modulo () =
    (* 2 ** P.bit_size_of_int *)
    let result = Mpz.init () in
    Mpz.pow_ui result (Mpzf.of_int 2) !P.bit_size_of_int;
    Mpzf.of_mpz result

  let format_t fmt (f : t) =
    Format.fprintf fmt "%f"
      (Mpfrf.to_float
         (Mpfrf.div (Mpfrf.of_mpz f Near) (Mpfrf.of_mpz (precision_modulo ()) Near) Near))

  let modf x =
    let int_part, frac_part = Mpzf.tdiv_qr x (precision_modulo ()) in
    let int_part = Mpzf.mul int_part (precision_modulo ()) in
    (frac_part, int_part)

  let copysign x y =
    match (Mpzf.sgn x, Mpzf.sgn y) with
    | 0, _ -> x
    | sx, sy when (sx > 0 && sy > 0) || (sx < 0 && sy < 0) || sy = 0 -> x
    | _ -> Mpzf.sub (Mpzf.of_int 0) x

  let of_int i = Mpzf.mul (Mpzf.of_int i) (precision_modulo ())

  let to_int f =
    let s = Mpzf.to_float (Mpzf.tdiv_q f (precision_modulo ())) in
    int_of_float s

  let of_float (f : float) : t =
    let frac_part, int_part = Float.modf f in
    let frac_part_scaled = frac_part *. Mpzf.to_float (precision_modulo ()) in
    Mpzf.add (Mpzf.of_float frac_part_scaled)
      (Mpzf.mul (Mpzf.of_float int_part) (precision_modulo ()))

  let to_float f =
    let frac_part, int_part = modf f in
    Mpzf.to_float (Mpzf.tdiv_q int_part (precision_modulo ()))
    +. (Mpzf.to_float frac_part /. Mpzf.to_float (precision_modulo ()))

  let zero = Mpzf.of_int 0

  let one () = Mpzf.mul (Mpzf.of_int 1) (precision_modulo ())

  let ( =. ) x y = Mpzf.cmp x y = 0

  let ( >=. ) x y = Mpzf.cmp x y >= 0

  let ( >. ) x y = Mpzf.cmp x y > 0

  let ( <. ) x y = Mpzf.cmp x y < 0

  let ( <=. ) x y = Mpzf.cmp x y <= 0

  let ( +. ) x y = Mpzf.add x y

  let ( -. ) x y = Mpzf.sub x y

  let ( /. ) x y = Mpzf.tdiv_q (Mpzf.mul x (precision_modulo ())) y

  let ( *. ) x y = Mpzf.tdiv_q (Mpzf.mul x y) (precision_modulo ())

  let is_zero x = x =. zero

  let min x y = if x >. y then y else x

  let max x y = if x >. y then x else y

  let is_nan_or_inf _ = false
end
