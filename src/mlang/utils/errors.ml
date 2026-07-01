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

type raised_in = Validator

exception
  BlockingError of { raised_in : raised_in; error_message : Ppf.structured_msg }

exception StructuredError of (Ppf.structured_msg * (unit -> unit) option)

let raise_blocking_error ~raised_in ~msg =
  raise (BlockingError { raised_in; error_message = msg })

let raise_structured_error ?(kont : (unit -> unit) option)
    (msg : Ppf.structured_msg) =
  raise (StructuredError (msg, kont))

let raise_multispanned_error_with_continuation (msg : string)
    (spans : (string option * Pos.t) list) (kont : (unit -> unit) option) : 'a =
  raise_structured_error (Ppf.make msg ~spans) ?kont

let raise_multispanned_error (msg : string)
    (spans : (string option * Pos.t) list) =
  raise_multispanned_error_with_continuation msg spans None

let raise_spanned_error (msg : string) ?(span_msg : string option)
    (span : Pos.t) : 'a =
  raise_multispanned_error msg [ (span_msg, span) ]

let raise_error (msg : string) : 'a = raise_multispanned_error msg []

let raise_spanned_error_with_continuation (msg : string)
    ?(span_msg : string option) (span : Pos.t) (kont : unit -> unit) : 'a =
  raise_multispanned_error_with_continuation msg
    [ (span_msg, span) ]
    (Some kont)
