include MapExt.Make (Int)

module type T = MapExt.T with type key = int

let pp ?(sep = "; ") ?(pp_key = Format.pp_print_int) ?(assoc = " => ")
    (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
    (map : 'a t) : unit =
  pp ~sep ~pp_key ~assoc pp_val fmt map
