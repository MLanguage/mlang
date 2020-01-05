(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

module type Type = sig
  type t

  val equal : t -> t -> bool

  val compare : t -> t -> int
end

module type S = sig
  type elt

  type t

  val create : elt -> t
  (** [create i] creates a new node with element [i] *)

  val find : t -> elt
  (** [find n] finds the representative of node [n] *)

  val union : t -> t -> unit
  (** [union n1 n2] merges nodes [n1] and [n2], performing path compression and union by rank along
      the way. *)
end

module Make (Eq : Type) : S with type elt = Eq.t
