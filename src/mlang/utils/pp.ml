(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2024 - 2026 DGFiP - INRIA                               *)
(*                                                                            *)
(* Ce programme est distribué sous la licence CeCILL-C: vous pouvez le        *)
(* redistribuer et/ou le modifier sous les contraintes de celle-ci.           *)
(*                                                                            *)
(* L'accessibilité au code source et les droits de copie, de modification et  *)
(* de redistribution qui découlent de ce contrat ont pour contrepartie de     *)
(* n'offrir aux utilisateurs qu'une garantie limitée et de ne faire peser sur *)
(* l'auteur du logiciel, le titulaire des droits patrimoniaux et les          *)
(* concédants successifs qu'une responsabilité restreinte.                    *)
(*                                                                            *)
(******************************************************************************)

type t = Format.formatter

let fpr fmt form_str = Format.fprintf fmt form_str

let spr form_str = Format.asprintf form_str

let pr form_str = Format.printf form_str

let epr form_str =
  let cont fmt = Format.fprintf fmt "@?" in
  Format.kfprintf cont Format.err_formatter form_str

let nil _ _ = ()

let string = Format.pp_print_string

let int = Format.pp_print_int

let float = Format.pp_print_float

let option pp_elt fmt opt = Format.pp_print_option pp_elt fmt opt

let list sep pp_elt fmt l =
  let pp_sep fmt () = Format.fprintf fmt sep in
  Format.pp_print_list ~pp_sep pp_elt fmt l

let list_endline pp_elt fmt l = list "@\n" pp_elt fmt l

let list_comma pp_elt fmt l = list ", " pp_elt fmt l

let list_space pp_elt fmt l = list " " pp_elt fmt l

let unmark pp_elt fmt e = pp_elt fmt (Pos.unmark e)
