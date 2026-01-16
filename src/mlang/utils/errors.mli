(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** {2 Structured error}*)

(** {1 Error Handling and Reporting}

    This module provides a centralized way to create, format, and report
    structured errors and warnings. It is designed to produce informative,
    user-friendly diagnostics by associating messages with specific source code
    locations. *)

(** {2 Types and Exceptions} *)

exception
  StructuredError of
    (string * (string option * Pos.t) list * (unit -> unit) option)
(** The primary exception for all structured errors. It carries:
    - A main error message (string).
    - A list of associated source code locations ([Pos.t]), each with an
      optional descriptive message.
    - An optional thunk (continuation) to be executed after the error is caught
      and displayed. *)

(** {2 Formatting Functions} *)

val format_structured_error :
  Format.formatter -> string * (string option * Pos.t) list -> unit
(** [format_structured_error ppf (msg, pos)] formats a structured error for
    **human-readable** console output. It first prints the main message,
    followed by the code snippets corresponding to each source position,
    retrieved via [Pos.retrieve_loc_text]. If a position has an associated
    message, that message is printed just before its code snippet.

    @param ppf The OCaml [Format] module's formatter.
    @param error_data
      A tuple containing the main message and the list of associated locations.
*)

val format_structured_error_gnu_format :
  Format.formatter -> string * (string option * Pos.t) list -> unit
(** [format_structured_error_gnu_format ppf (msg, pos_list)] formats an error
    according to **GNU standards** (`file:line:col: message`). For each
    location, it prints the formatted position, the main error message, and any
    location-specific message.

    @param ppf The OCaml [Format] module's formatter.
    @param error_data
      A tuple containing the main message and the list of associated locations.
*)

(** {2 Error Raising Functions} *)

(** These are helper functions designed to simplify the process of raising a
    [StructuredError] exception with different kinds of information. *)

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

(** {2 Warning Functions} *)

(** These functions print formatted warnings to the console without raising an
    exception. They use the same formatting logic as the error functions. *)

val print_spanned_warning : string -> ?span_msg:string -> Pos.t -> unit
(** Prints a warning message associated with a single source code location. The
    output is formatted for human readability using [format_structured_error].

    @param msg The main warning message.
    @param ?span_msg An optional message specific to the provided location.
    @param span The source code position ([Pos.t]) of the warning. *)

val print_multispanned_warning : string -> (string option * Pos.t) list -> unit
(** Prints a warning message associated with multiple source code locations. The
    output is formatted for human readability using [format_structured_error].

    @param msg The main warning message.
    @param spans
      A list of tuples, each containing an optional message and a source
      position. *)
