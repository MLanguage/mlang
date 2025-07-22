module type T = sig
  include Map.S

  val card : 'a t -> int

  val one : key -> 'a -> 'a t

  val from_assoc_list : (key * 'a) list -> 'a t

  val union_fst : 'a t -> 'a t -> 'a t

  val union_snd : 'a t -> 'a t -> 'a t

  val pp :
    ?sep:string ->
    ?pp_key:(Pp.t -> key -> unit) ->
    ?assoc:string ->
    (Pp.t -> 'a -> unit) ->
    Pp.t ->
    'a t ->
    unit

  val pp_keys :
    ?sep:string -> ?pp_key:(Pp.t -> key -> unit) -> unit -> Pp.t -> 'a t -> unit
end

module Make : functor (Ord : Set.OrderedType) -> T with type key = Ord.t
