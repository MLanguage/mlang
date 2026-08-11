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

(** {2 M messages} *)

(** This module provides multiple-language logging utils and defines the several
    printers for mlang (for now, Warnings and Errors). The environment variable
    'LANG' is used to define the messages language, which redirect to the actual
    translation modules ({!module M_messages.En} for English messages,
    {!module M_messages.Fr} for french). The module is automatically selected at
    runtime and included in this module, so refering to this module is
    equivalent to refering to the corresponding translation module. *)

module Types = Types

include Types.LANG

val select_parse_error_message : code:int -> string -> string
(** When failing, the parser raises a [Mparser.Error] with an integer error
    code. Each code is associated to a error message in the
    [M_frontend.Syntax_messages] module. Note that this module does not define
    textual error messages, but strings under the format
    ["Lang:name_of_the_method"]. This function fetches the string associated to
    the error code and, if it corresponds to properly formatted string, returns
    the associated translated error message. *)

(* val print_validator_error : *)
(*   M_frontend.Validator.Err.t -> Pos.t -> Log.structured_msg *)
(* (\** Prints the string representation of a validator error case. *\) *)
