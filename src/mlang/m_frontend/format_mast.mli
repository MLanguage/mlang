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

val format_var_type : Mast.var_type -> string

val format_variable : Pp.t -> Com.m_var_name -> unit

val format_rule_domain : Pp.t -> Mast.rule_domain_decl -> unit

val format_verif_domain : Pp.t -> Mast.verif_domain_decl -> unit

val format_source_file_item : Pp.t -> Mast.source_file_item -> unit

val format_source_file : Pp.t -> Mast.source_file -> unit
