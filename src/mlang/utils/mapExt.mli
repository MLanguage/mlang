module type T = sig
  include Map.S

  val from_assoc_list : (key * 'a) list -> 'a t

  val pp :
    ?sep:string ->
    ?pp_key:(Format.formatter -> key -> unit) ->
    ?assoc:string ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit
end

module Make : functor (Ord : Set.OrderedType) -> T with type key = Ord.t
