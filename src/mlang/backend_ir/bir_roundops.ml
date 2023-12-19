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

let epsilon = 0.000001

module type RoundOpsInterface = sig
  type t

  val truncatef : t -> t

  val roundf : t -> t
end

module type RoundOpsFunctor = functor (N : Bir_number.NumberInterface) ->
  RoundOpsInterface with type t = N.t

module DefaultRoundOps (N : Bir_number.NumberInterface) :
  RoundOpsInterface with type t = N.t = struct
  type t = N.t

  let truncatef (x : N.t) : N.t = N.floor N.(x +. N.of_float epsilon)

  (* Careful : rounding in M is done with this arbitrary behavior. We can't use
     copysign here because [x < zero] is critical to have the correct behavior
     on -0 *)
  let roundf (x : N.t) =
    N.of_int
      (N.to_int
         N.(
           x
           +. N.of_float
                (if N.(x < zero ()) then Float.sub (-0.5) epsilon
                else Float.add 0.5 epsilon)))
end

module MultiRoundOps (N : Bir_number.NumberInterface) :
  RoundOpsInterface with type t = N.t = struct
  type t = N.t

  let truncatef (x : N.t) : N.t = N.floor N.(x +. N.of_float 0.000001)

  let roundf (x : N.t) =
    let n_0_5 = N.of_float 0.5 in
    let n_100000_0 = N.of_float 100000.0 in
    let v1 = N.floor x in
    let v2 = N.(N.floor (((x -. v1) *. n_100000_0) +. n_0_5) /. n_100000_0) in
    N.floor N.(v1 +. v2 +. n_0_5)
end

module MainframeRoundOps (L : sig
  val max_long : Int64.t ref
end)
(N : Bir_number.NumberInterface) : RoundOpsInterface with type t = N.t = struct
  type t = N.t

  let floor_g (x : N.t) : N.t =
    if N.abs x <= N.of_int !L.max_long then N.floor x else x

  let ceil_g (x : N.t) : N.t =
    if N.abs x <= N.of_int !L.max_long then N.ceil x else x

  let truncatef (x : N.t) : N.t = floor_g N.(x +. N.of_float epsilon)

  (* Careful : rounding in M is done with this arbitrary behavior. We can't use
     copysign here because [x < zero] is critical to have the correct behavior
     on -0 *)
  let roundf (x : N.t) =
    if N.(x < zero ()) then ceil_g N.(x -. N.of_float (Float.add 0.5 epsilon))
    else floor_g N.(x +. N.of_float (Float.add 0.5 epsilon))
end
