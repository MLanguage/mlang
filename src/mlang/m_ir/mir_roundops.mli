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

(** Rounding operations to use in the interpreter *)
module type RoundOpsInterface = sig
  type t

  val truncatef : t -> t

  val roundf : t -> t
end

(** The actual implementation of rounding operations depends on the chosen
    representation of numbers, hence we need a functor *)
module type RoundOpsFunctor = functor (N : Mir_number.NumberInterface) ->
  RoundOpsInterface with type t = N.t

module DefaultRoundOps : RoundOpsFunctor
(** Default rounding operations: those used in the PC/single-thread context *)

module MultiRoundOps : RoundOpsFunctor
(** Multithread rounding operations: those used in the PC/multi-thread context
*)

(** Mainframe rounding operations: those used in the mainframe context. As the
    behavior depends on the sie of the `long` type, this size must be given as
    an argument (and should be either 32 or 64). *)
module MainframeRoundOps : functor
  (L : sig
     val max_long : Int64.t ref
   end)
  -> RoundOpsFunctor
