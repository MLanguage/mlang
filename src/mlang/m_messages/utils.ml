(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2026 DGFiP - INRIA                                      *)
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

let pp_cycle pp ppf cycle =
  Format.pp_print_list
    ~pp_sep:(fun fmt _ -> Format.fprintf fmt " -> ")
    pp ppf cycle

let selected_lang ~default =
  match String.lowercase_ascii @@ Sys.getenv "LANG" with
  | "f" | "fr" | "francais" | "french" -> `Francais
  | "a" | "an" | "anglais" | "e" | "en" | "english" -> `English
  | _ | (exception Not_found) -> default
