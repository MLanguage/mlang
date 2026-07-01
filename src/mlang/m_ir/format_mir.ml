(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2019 - 2026 DGFiP - INRIA                               *)
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

let format_variable fmt (var : Com.Var.t) =
  Format.fprintf fmt "%s" (Pos.unmark var.name)

let format_expression = Com.format_expression format_variable

let format_error fmt (err : Com.Error.t) =
  Format.fprintf fmt "erreur %s (%a)" (Pos.unmark err.name) Com.Error.pp_descr
    err

let format_variable fmt (v : Com.Var.t) =
  Format.fprintf fmt "%s: %s" (Pos.unmark v.name) (Com.Var.descr_str v)
