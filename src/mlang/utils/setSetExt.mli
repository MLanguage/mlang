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

module type T = sig
  type base_elt

  include SetExt.T

  val from_list_list : base_elt list list -> t

  val from_marked_list_list : base_elt Pos.marked list Pos.marked list -> t

  val pp :
    ?sep1:string ->
    ?sep2:string ->
    ?pp_elt:(Format.formatter -> base_elt -> unit) ->
    unit ->
    Format.formatter ->
    t ->
    unit
end

module Make : functor (SetElt : SetExt.T) ->
  T with type base_elt = SetElt.elt and type elt = SetElt.t
