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
  (** Warning: lossy *)

  val of_float : float -> t

  val of_float_input : float -> t

  val to_float : t -> float
  (** Warning: lossy *)

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

module RegularFloatNumber : NumberInterface = struct
  type t = float

  let format_t fmt f = Format.fprintf fmt "%f" f

  let format_prec_t mi ma fmt f =
    let s = Format.sprintf "%.*f" ma f in
    try
      let v = String.index_from s 0 '.' in
      let b = Bytes.of_string s in
      Bytes.set b v ',';
      let rec aux i =
        let c = Bytes.get b i in
        if (i - v > mi && c = '0') || c = ',' then aux (i - 1)
        else Bytes.to_string (Bytes.sub b 0 (i + 1))
      in
      Format.fprintf fmt "%s" (aux (Bytes.length b - 1))
    with _ -> Format.fprintf fmt "%s" s

  let abs x = Float.abs x

  let floor x = Float.floor x

  let ceil x = Float.ceil x

  let of_int i = Int64.to_float i

  let to_int f = Int64.of_float f

  let of_float f = f

  let of_float_input f = f

  let to_float f = f

  let zero () = 0.

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

let mpfr_abs (x : Mpfrf.t) : Mpfrf.t =
  let out = Mpfr.init2 (Mpfr.get_prec x) in
  ignore (Mpfr.abs out x Mpfr.Near);
  Mpfrf.of_mpfr out

let mpfr_floor (x : Mpfrf.t) : Mpfrf.t =
  let out = Mpfr.init () in
  ignore (Mpfr.floor out x);
  Mpfrf.of_mpfr out

let mpfr_ceil (x : Mpfrf.t) : Mpfrf.t =
  let out = Mpfr.init () in
  ignore (Mpfr.ceil out x);
  Mpfrf.of_mpfr out

module MPFRNumber : NumberInterface = struct
  type t = Mpfrf.t

  let rounding : Mpfr.round = Near

  let format_t fmt f = Format.fprintf fmt "%a" Mpfrf.print f

  let format_prec_t _mi _ma fmt f = format_t fmt f

  let abs (x : t) : t = mpfr_abs x

  let floor (x : t) : t = mpfr_floor x

  let ceil (x : t) : t = mpfr_ceil x

  let of_int i = Mpfrf.of_int (Int64.to_int i) rounding

  let to_int f = Int64.of_float (Mpfrf.to_float f)

  let of_float f = Mpfrf.of_float f rounding

  let of_float_input f = Mpfrf.of_float f rounding

  let to_float f = Mpfrf.to_float ~round:rounding f

  let zero () = Mpfrf.of_int 0 rounding

  let one () = Mpfrf.of_int 1 rounding

  let ( =. ) x y = Mpfrf.cmp x y = 0

  let ( >=. ) x y = Mpfrf.cmp x y >= 0

  let ( >. ) x y = Mpfrf.cmp x y > 0

  let ( <. ) x y = Mpfrf.cmp x y < 0

  let ( <=. ) x y = Mpfrf.cmp x y <= 0

  let ( +. ) x y = Mpfrf.add x y rounding

  let ( -. ) x y = Mpfrf.sub x y rounding

  let ( /. ) x y = Mpfrf.div x y rounding

  let ( *. ) x y = Mpfrf.mul x y rounding

  let min x y = if x >. y then y else x

  let max x y = if x >. y then x else y

  let is_zero x = x =. zero ()

  let is_nan_or_inf x = not (Mpfrf.number_p x)
end

module IntervalNumber : NumberInterface = struct
  type t = { down : Mpfrf.t; up : Mpfrf.t }

  let v (x : Mpfrf.t) (y : Mpfrf.t) : t = { down = x; up = y }

  let format_t fmt f =
    Format.fprintf fmt "[%a;%a]" Mpfrf.print f.down Mpfrf.print f.up

  let format_prec_t _mi _ma fmt f = format_t fmt f

  let abs x =
    let id = mpfr_abs x.down in
    let iu = mpfr_abs x.up in
    v id iu

  let floor x =
    let id = mpfr_floor x.down in
    let iu = mpfr_floor x.up in
    v id iu

  let ceil x =
    let id = mpfr_ceil x.down in
    let iu = mpfr_ceil x.up in
    v id iu

  let of_int i =
    v (Mpfrf.of_int (Int64.to_int i) Down) (Mpfrf.of_int (Int64.to_int i) Up)

  let of_float (f : float) = v (Mpfrf.of_float f Down) (Mpfrf.of_float f Up)

  let of_float_input (f : float) =
    v (Mpfrf.of_float f Down) (Mpfrf.of_float f Up)

  let to_float (f : t) : float =
    let fd = Mpfrf.to_float ~round:Down f.down in
    let fu = Mpfrf.to_float ~round:Up f.up in
    if fd = fu then fd
    else
      let prec_diff = fu -. fd in
      let digits = 1 - (Float.to_int @@ Float.log10 prec_diff) in
      Errors.raise_error
        (Format.asprintf
           "Tried to convert interval to float, got two different bounds: \
            [%.*f;%.*f]"
           digits fd digits fu)

  let to_int (f : t) : Int64.t = Int64.of_float (to_float f)

  let zero () = v (Mpfrf.of_int 0 Down) (Mpfrf.of_int 0 Up)

  let one () = v (Mpfrf.of_int 1 Down) (Mpfrf.of_int 1 Up)

  let ( =. ) x y =
    let outd = Mpfrf.cmp x.down y.down = 0 in
    let outu = Mpfrf.cmp x.up y.up = 0 in
    if outd = outu then outu
    else
      Errors.raise_error
        (Format.asprintf "Tried to compare %a = %a but got inconsistent results"
           format_t x format_t y)

  let ( >=. ) x y =
    let outd = Mpfrf.cmp x.down y.down >= 0 in
    let outu = Mpfrf.cmp x.up y.up >= 0 in
    if outd = outu then outu
    else
      Errors.raise_error
        (Format.asprintf
           "Tried to compare %a >= %a but got inconsistent results" format_t x
           format_t y)

  let ( >. ) x y =
    let outd = Mpfrf.cmp x.down y.down > 0 in
    let outu = Mpfrf.cmp x.up y.up > 0 in
    if outd = outu then outu
    else
      Errors.raise_error
        (Format.asprintf "Tried to compare %a > %a but got inconsistent results"
           format_t x format_t y)

  let ( <. ) x y =
    let outd = Mpfrf.cmp x.down y.down < 0 in
    let outu = Mpfrf.cmp x.up y.up < 0 in
    if outd = outu then outu
    else
      Errors.raise_error
        (Format.asprintf "Tried to compare %a < %a but got inconsistent results"
           format_t x format_t y)

  let ( <=. ) x y =
    let outd = Mpfrf.cmp x.down y.down <= 0 in
    let outu = Mpfrf.cmp x.up y.up <= 0 in
    if outd = outu then outu
    else
      Errors.raise_error
        (Format.asprintf
           "Tried to compare %a <= %a but got inconsistent results" format_t x
           format_t y)

  let ( +. ) x y = v (Mpfrf.add x.down y.down Down) (Mpfrf.add x.up y.up Up)

  let ( -. ) x y = v (Mpfrf.sub x.down y.down Down) (Mpfrf.sub x.up y.up Up)

  let ( /. ) x y = v (Mpfrf.div x.down y.down Down) (Mpfrf.div x.up y.up Up)

  let ( *. ) x y = v (Mpfrf.mul x.down y.down Down) (Mpfrf.mul x.up y.up Up)

  let min x y = if x >. y then y else x

  let max x y = if x >. y then x else y

  let is_zero x = x =. zero ()

  let is_nan_or_inf x = not (Mpfrf.number_p x.down && Mpfrf.number_p x.up)
end

module RationalNumber : NumberInterface = struct
  type t = Mpqf.t

  let format_t fmt f = Mpqf.print fmt f

  let format_prec_t _mi _ma fmt f = format_t fmt f

  let abs x = Mpqf.abs x

  let floor x =
    let num = Mpqf.get_num x in
    let dem = Mpqf.get_den x in
    Mpqf.of_mpz (Mpzf.fdiv_q num dem)

  let ceil x =
    let num = Mpqf.get_num x in
    let dem = Mpqf.get_den x in
    Mpqf.of_mpz (Mpzf.cdiv_q num dem)

  let of_int i = Mpqf.of_int (Int64.to_int i)

  let to_int f = Int64.of_float (Mpqf.to_float f)

  let of_float f = Mpqf.of_float f

  let of_float_input f = Mpqf.of_float f

  let to_float f = Mpqf.to_float f

  let zero () = Mpqf.of_int 0

  let one () = Mpqf.of_int 1

  let ( =. ) x y = Mpqf.equal x y

  let ( >=. ) x y = Mpqf.cmp x y >= 0

  let ( >. ) x y = Mpqf.cmp x y > 0

  let ( <. ) x y = Mpqf.cmp x y < 0

  let ( <=. ) x y = Mpqf.cmp x y <= 0

  let ( +. ) x y = Mpqf.add x y

  let ( -. ) x y = Mpqf.sub x y

  let ( /. ) x y = Mpqf.div x y

  let ( *. ) x y = Mpqf.mul x y

  let min x y = if x >. y then y else x

  let max x y = if x >. y then x else y

  let is_zero x = x =. zero ()

  let is_nan_or_inf (x : t) =
    let max = Mpz.init () in
    Mpz.pow_ui max (Mpz.of_int 2) 128;
    let min = Mpzf.sub (Mpzf.of_int 0) max in
    Mpzf.cmp (Mpqf.get_num x) max > 0
    || Mpzf.cmp (Mpqf.get_den x) max > 0
    || Mpzf.cmp (Mpqf.get_num x) min < 0
    || Mpzf.cmp (Mpqf.get_den x) min < 0
end

module BigIntFixedPointNumber (P : sig
  val scaling_factor_bits : int ref
end) : NumberInterface = struct
  type t = Mpzf.t

  let precision_modulo () =
    (* 2 ** P.bit_size_of_int *)
    let result = Mpz.init () in
    Mpz.pow_ui result (Mpzf.of_int 2) !P.scaling_factor_bits;
    Mpzf.of_mpz result

  let format_t fmt (f : t) =
    Format.fprintf fmt "%f"
      (Mpfrf.to_float
         (Mpfrf.div (Mpfrf.of_mpz f Near)
            (Mpfrf.of_mpz (precision_modulo ()) Near)
            Near))

  let format_prec_t _mi _ma fmt (f : t) = format_t fmt f

  let modf x =
    let int_part, frac_part = Mpzf.tdiv_qr x (precision_modulo ()) in
    let int_part = Mpzf.mul int_part (precision_modulo ()) in
    (frac_part, int_part)

  let abs x = Mpzf.abs x

  let floor x =
    let prec_mod = precision_modulo () in
    Mpzf.mul (Mpzf.fdiv_q x prec_mod) prec_mod

  let ceil x =
    let prec_mod = precision_modulo () in
    Mpzf.mul (Mpzf.cdiv_q x prec_mod) prec_mod

  let of_int i = Mpzf.mul (Mpzf.of_int (Int64.to_int i)) (precision_modulo ())

  let to_int f =
    let s = Mpzf.to_float (Mpzf.tdiv_q f (precision_modulo ())) in
    Int64.of_float s

  let of_float (f : float) : t =
    let frac_part, int_part = Float.modf f in
    let frac_part_scaled = frac_part *. Mpzf.to_float (precision_modulo ()) in
    Mpzf.add
      (Mpzf.of_float frac_part_scaled)
      (Mpzf.mul (Mpzf.of_float int_part) (precision_modulo ()))

  let of_float_input (f : float) : t = of_float f

  let to_float f =
    let frac_part, int_part = modf f in
    Mpzf.to_float (Mpzf.tdiv_q int_part (precision_modulo ()))
    +. (Mpzf.to_float frac_part /. Mpzf.to_float (precision_modulo ()))

  let zero () = Mpzf.of_int 0

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

  let is_zero x = x =. zero ()

  let min x y = if x >. y then y else x

  let max x y = if x >. y then x else y

  let is_nan_or_inf _ = false
end
