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

module type RoundOpsInterface = sig
  type t

  val truncatef : t -> t

  val roundf : t -> t
end

module type RoundOpsFunctor = functor (N : Mir_number.NumberInterface) ->
  RoundOpsInterface with type t = N.t

module DefaultRoundOps (N : Mir_number.NumberInterface) :
  RoundOpsInterface with type t = N.t = struct
  type t = N.t

  let epsilon = !Cli.comparison_error_margin

  let truncatef (x : N.t) : N.t = N.floor N.(x +. N.of_float epsilon)

  (* Careful : rounding in M is done with this arbitrary behavior. We can't use
     copysign here because [x < zero] is critical to have the correct behavior
     on -0 *)
  let roundf (x : N.t) =
    let e = N.of_float (0.5 +. (epsilon *. 50.)) in
    if N.(x < zero ()) then N.ceil N.(x -. e) else N.floor N.(x +. e)
end

module MultiRoundOps (N : Mir_number.NumberInterface) :
  RoundOpsInterface with type t = N.t = struct
  type t = N.t

  let epsilon = !Cli.comparison_error_margin

  let truncatef (x : N.t) : N.t = N.floor N.(x +. N.of_float epsilon)

  let roundf (x : N.t) =
    let e = N.of_float (0.5 +. (epsilon *. 50.)) in
    if N.(x < zero ()) then N.ceil N.(x -. e) else N.floor N.(x +. e)
end

module MainframeRoundOps (L : sig
  val max_long : Int64.t ref
end)
(N : Mir_number.NumberInterface) : RoundOpsInterface with type t = N.t = struct
  type t = N.t

  let epsilon = !Cli.comparison_error_margin

  let floor_g (x : N.t) : N.t =
    if N.abs x <= N.of_int !L.max_long then N.floor x else x

  let ceil_g (x : N.t) : N.t =
    if N.abs x <= N.of_int !L.max_long then N.ceil x else x

  let truncatef (x : N.t) : N.t = floor_g N.(x +. N.of_float epsilon)

  (* Careful : rounding in M is done with this arbitrary behavior. We can't use
     copysign here because [x < zero] is critical to have the correct behavior
     on -0 *)
  let roundf (x : N.t) =
    let e = N.of_float (0.5 +. (epsilon *. 50.)) in
    if N.(x < zero ()) then ceil_g N.(x -. e) else floor_g N.(x +. e)
end
