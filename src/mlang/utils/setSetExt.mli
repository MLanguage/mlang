module type T = sig
  type base_elt

  include SetExt.T

  val from_list_list : base_elt list list -> t

  val from_marked_list_list : base_elt Pos.marked list Pos.marked list -> t

  val pp :
    ?sep1:string ->
    ?sep2:string ->
    ?pp_elt:(Format.formatter -> base_elt -> unit) ->
    unit ->
    Format.formatter ->
    t ->
    unit
end

module Make : functor (SetElt : SetExt.T) ->
  T with type base_elt = SetElt.elt and type elt = SetElt.t
