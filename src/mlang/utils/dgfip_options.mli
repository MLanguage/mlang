(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2025 - 2026 DGFiP - INRIA                               *)
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

type flags = {
  annee_revenu : int;
  flg_correctif : bool;
  flg_iliad : bool;
  flg_pro : bool;
  flg_cfir : bool;
  flg_gcos : bool;
  flg_tri_ebcdic : bool;
  flg_short : bool;
  flg_register : bool;
  flg_optim_min_max : bool;
  flg_extraction : bool;
  flg_genere_libelle_restituee : bool;
  flg_controle_separe : bool;
  flg_controle_immediat : bool;
  flg_overlays : bool;
  flg_colors : bool;
  flg_ticket : bool;
  flg_trace : bool;
  flg_debug : bool;
  nb_debug_c : int;
  xflg : bool;
}

val default_flags : flags

val handler :
  application_names:string list ->
  int ->
  bool ->
  bool ->
  int option ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  int ->
  bool ->
  bool ->
  bool ->
  bool ->
  flags

val process_dgfip_options :
  application_names:string list ->
  string list ->
  (flags Cmdliner.Cmd.eval_ok, Cmdliner.Cmd.eval_error) result
