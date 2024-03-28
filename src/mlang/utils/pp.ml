type t = Format.formatter

let nil _ _ = ()

let string = Format.pp_print_string

let option pp_elt fmt opt = Format.pp_print_option pp_elt fmt opt

let list sep pp_elt fmt l =
  let pp_sep fmt () = Format.fprintf fmt sep in
  Format.pp_print_list ~pp_sep pp_elt fmt l

let list_endline pp_elt fmt l = list "@\n" pp_elt fmt l

let list_comma pp_elt fmt l = list ", " pp_elt fmt l

let list_space pp_elt fmt l = list " " pp_elt fmt l

let unmark pp_elt fmt e = pp_elt fmt (Pos.unmark e)
