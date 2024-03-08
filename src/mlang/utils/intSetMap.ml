include MapExt.Make (IntSet)

module type T = MapExt.T with type key = IntSet.t

let pp ?(sep = ", ") ?(pp_key = IntSet.pp ()) ?(assoc = " => ")
    (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
    (map : 'a t) : unit =
  pp ~sep ~pp_key ~assoc pp_val fmt map
