include MapExt.Make (String)

module type T = MapExt.T with type key = string

let pp ?(sep = "; ") ?(pp_key = Format.pp_print_string) ?(assoc = " => ")
    (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
    (map : 'a t) : unit =
  pp ~sep ~pp_key ~assoc pp_val fmt map

let keySet t = fold (fun k _ s -> StrSet.add k s) t StrSet.empty
