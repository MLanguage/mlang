(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2022 - 2026 DGFiP - INRIA                               *)
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

(** Rounding operations to use in the interpreter *)
module type RoundOpsInterface = sig
  type t

  val truncatef : t -> t

  val roundf : t -> t
end

(** The actual implementation of rounding operations depends on the chosen
    representation of numbers, hence we need a functor *)
module type RoundOpsFunctor = functor (N : Mir_number.NumberInterface) ->
  RoundOpsInterface with type t = N.t

module DefaultRoundOps : RoundOpsFunctor
(** Default rounding operations: those used in the PC/single-thread context *)

module MultiRoundOps : RoundOpsFunctor
(** Multithread rounding operations: those used in the PC/multi-thread context
*)

(** Mainframe rounding operations: those used in the mainframe context. As the
    behavior depends on the sie of the `long` type, this size must be given as
    an argument (and should be either 32 or 64). *)
module MainframeRoundOps : functor
  (_ : sig
     val max_long : Int64.t ref
   end)
  -> RoundOpsFunctor
