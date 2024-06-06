type t = Format.formatter

let fpr fmt form_str = Format.fprintf fmt form_str

let spr form_str = Format.asprintf form_str

let pr form_str = Format.printf form_str

let epr form_str =
  let cont fmt = Format.fprintf fmt "@?" in
  Format.kfprintf cont Format.err_formatter form_str

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
