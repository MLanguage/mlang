module type T = SetExt.T with type elt = String.t

include SetExt.Make (String)

let pp ?(sep = " ") ?(pp_elt = Format.pp_print_string) (_ : unit)
    (fmt : Format.formatter) (set : t) : unit =
  pp ~sep ~pp_elt () fmt set
