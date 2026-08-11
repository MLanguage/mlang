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

val proceed : Mast.program -> Mast.program
(** Expands the program by:
    - keeping elements that only belong to the selected applications defined in
      {!Utils.Config.application_names};
    - inlining constant's values and removes their definition;
    - unrolling loops on variable names (such as "sum(i=05,06,07:Xi)" that
      becomes "sum(x05, X06, X07)". *)
