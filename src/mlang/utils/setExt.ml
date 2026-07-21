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
  include Set.S

  val card : t -> int

  val one : elt -> t

  val from_list : elt list -> t

  val from_marked_list : elt Pos.marked list -> t

  val pp :
    ?sep:string ->
    ?pp_elt:(Format.formatter -> elt -> unit) ->
    unit ->
    Format.formatter ->
    t ->
    unit
end

module Make =
functor
  (Ord : Set.OrderedType)
  ->
  struct
    include Set.Make (Ord)

    let card = cardinal

    let one = singleton

    let from_list (l : elt list) : t =
      let fold set elt = add elt set in
      List.fold_left fold empty l

    let from_marked_list (l : elt Pos.marked list) : t =
      let fold set elt = add (Pos.unmark elt) set in
      List.fold_left fold empty l

    let pp ?(sep = " ") ?(pp_elt = Pp.nil) (_ : unit) (fmt : Pp.t) (set : t) :
        unit =
      let foldSet elt first =
        let _ =
          if first then Format.fprintf fmt "%a" pp_elt elt
          else Format.fprintf fmt "%s%a" sep pp_elt elt
        in
        false
      in
      ignore (fold foldSet set true)
  end
