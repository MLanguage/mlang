(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2023 - 2026 DGFiP - INRIA                               *)
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

include SetSetExt.Make (StrSet)

module type T = SetSetExt.T with type base_elt = string and type elt = StrSet.t

let pp ?(sep1 = ", ") ?(sep2 = " ") ?(pp_elt = Format.pp_print_string)
    (_ : unit) (fmt : Format.formatter) (setSet : t) : unit =
  pp ~sep1 ~sep2 ~pp_elt () fmt setSet
