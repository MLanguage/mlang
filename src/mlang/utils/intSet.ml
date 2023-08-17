module type T = SetExt.T with type elt = int

include SetExt.Make (Int)

let pp ?(sep = " ") ?(pp_elt = Format.pp_print_int) (_ : unit)
    (fmt : Format.formatter) (set : t) : unit =
  pp ~sep ~pp_elt () fmt set
