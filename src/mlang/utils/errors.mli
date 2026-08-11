(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2021 - 2026 DGFiP - INRIA                               *)
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

(** {2 Structured error}*)

(** {1 Error Handling and Reporting}

    This module provides a centralized way to create, format, and report
    structured errors and warnings. It is designed to produce informative,
    user-friendly diagnostics by associating messages with specific source code
    locations. *)

(** {2 Types and Exceptions} *)

type raised_in = Validator

exception
  BlockingError of { raised_in : raised_in; error_message : Ppf.structured_msg }
(** An error that stops the execution of mlang. It carries:
    - the location where the exeption was raised;
    - the associated (strucuted) error message. *)

exception StructuredError of (Ppf.structured_msg * (unit -> unit) option)
(** A generic exception for all structured errors. It carries:
    - A main error message (string).
    - A list of associated source code locations ([Pos.t]), each with an
      optional descriptive message.
    - An optional thunk (continuation) to be executed after the error is caught
      and displayed. *)

(** {2 Error Raising Functions} *)

(** These are helper functions designed to simplify the process of raising a
    [StructuredError] exception with different kinds of information. *)

val raise_blocking_error : raised_in:raised_in -> msg:Ppf.structured_msg -> 'a
(** Raises a Blocking_error. *)

val raise_error : string -> 'a
(** Raises a simple [StructuredError] with only a main message and no associated
    source code locations or continuation.

    @param msg The main error message.
    @raise StructuredError
      Always raises this exception with an empty list of locations. *)

val raise_spanned_error : string -> ?span_msg:string -> Pos.t -> 'a
(** Raises a [StructuredError] that points to a single source code location.

    @param msg The main error message.
    @param ?span_msg
      An optional message specific to the provided location ([span]).
    @param span The source code position ([Pos.t]) where the error occurred.
    @raise StructuredError
      Raises the exception with the provided message and a single-element list
      containing the span and its optional message. *)

val raise_multispanned_error : string -> (string option * Pos.t) list -> 'a
(** Raises a [StructuredError] that points to multiple source code locations,
    each with its own optional message.

    @param msg The main error message.
    @param spans
      A list of tuples, where each contains an optional location-specific
      message and a source code position ([Pos.t]).
    @raise StructuredError
      Raises the exception with the provided message and list of spans. *)

val raise_spanned_error_with_continuation :
  string -> ?span_msg:string -> Pos.t -> (unit -> unit) -> 'a
(** Raises a [StructuredError] for a single source location and includes a
    continuation thunk. This thunk is a function that can be executed by the
    error handler after the error message is displayed.

    @param msg The main error message.
    @param ?span_msg An optional message specific to the provided location.
    @param span The source code position ([Pos.t]) of the error.
    @param kont
      A function of type [unit -> unit] to be executed after error reporting.
    @raise StructuredError
      Raises the exception including the message, the span, and the
      continuation. *)

val raise_multispanned_error_with_continuation :
  string -> (string option * Pos.t) list -> (unit -> unit) option -> 'a
(** Raises a [StructuredError] for a several source locations and includes a
    continuation thunk. This thunk is a function that can be executed by the
    error handler after the error message is displayed.

    @param msg The main error message.
    @param spans
      T A list of tuples, where each contains an optional location-specific
      message and a source code position ([Pos.t]).
    @param kont
      An optional function of type [unit -> unit] to be executed after error
      reporting.
    @raise StructuredError
      Raises the exception including the message, the span, and the
      continuation. *)

val raise_structured_error : ?kont:(unit -> unit) -> Ppf.structured_msg -> 'a
(** Raises a [StructuredError] for a structured message and includes a
    continuation thunk. This thunk is a function that can be executed by the
    error handler after the error message is displayed.

    @param kont
      An optional function of type [unit -> unit] to be executed after error
      reporting.
    @param msg The structured error message.
    @raise StructuredError
      Raises the exception including the message, the span, and the
      continuation. *)
