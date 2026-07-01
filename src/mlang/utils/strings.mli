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

(* val sanitize_str : string * Pos.t -> string *)
(** DGFiP sources are encoded in iso-8859-1 which is not compatible with some
    backend compilers such as Java and Python, this function transforms illegal
    characters with a space. - not useful anymore (for now) *)

val concat_int : string -> string -> int -> string

val sanitize_c_str : string -> string

val compare_default : string -> string -> int

val compare_ebcdic : string -> string -> int

val starts_with : prefix:string -> string -> bool

val ends_with : suffix:string -> string -> bool
