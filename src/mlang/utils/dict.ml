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

module Make (I : sig
  type t

  type elt

  val key_of_elt : elt -> t

  val compare : t -> t -> int
end) =
struct
  module DictMap = MapExt.Make (I)

  type key = I.t

  type elt = I.elt

  type t = I.elt DictMap.t

  let find = DictMap.find

  let filter = DictMap.filter

  let empty = DictMap.empty

  let bindings = DictMap.bindings

  let singleton v = DictMap.singleton (I.key_of_elt v) v

  let add v t = DictMap.add (I.key_of_elt v) v t

  let mem v t = DictMap.mem (I.key_of_elt v) t

  let fold f t acc = DictMap.fold (fun _ v acc -> f v acc) t acc

  let union t1 t2 = DictMap.union (fun _ v _ -> Some v) t1 t2

  let inter t1 t2 =
    DictMap.merge
      (fun _ v1 v2 -> match (v1, v2) with Some _, Some _ -> v1 | _ -> None)
      t1 t2

  let for_all f t = DictMap.for_all (fun _ v -> f v) t
end
