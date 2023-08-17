module type T = sig
  include Set.S

  val from_list : elt list -> t

  val from_marked_list : elt Pos.marked list -> t

  val pp :
    ?sep:string ->
    ?pp_elt:(Format.formatter -> elt -> unit) ->
    unit ->
    Format.formatter ->
    t ->
    unit
end

module Make : functor (Ord : Set.OrderedType) -> T with type elt = Ord.t
