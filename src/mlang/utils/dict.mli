module type S = sig
  type key

  type elt

  type t

  val bindings : t -> (key * elt) list

  val add : elt -> t -> t

  val empty : t

  val find : key -> t -> elt

  val mem : elt -> t -> bool

  val union : t -> t -> t

  val inter : t -> t -> t

  val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b

  val singleton : elt -> t

  val filter : (key -> elt -> bool) -> t -> t

  val for_all : (elt -> bool) -> t -> bool
end

module Make : functor
  (I : sig
     type t

     type elt

     val key_of_elt : elt -> t

     val compare : t -> t -> int
   end)
  -> S with type key = I.t and type elt = I.elt
