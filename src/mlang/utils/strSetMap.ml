include MapExt.Make (StrSet)

module type T = MapExt.T with type key = StrSet.t

let pp ?(sep = ", ") ?(pp_key = StrSet.pp ()) ?(assoc = " => ")
    (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
    (map : 'a t) : unit =
  pp ~sep ~pp_key ~assoc pp_val fmt map
