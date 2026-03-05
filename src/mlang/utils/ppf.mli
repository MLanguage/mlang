(** {2 Structured messages}*)

(** {1 Message Handling and Reporting}

    This module provides a centralized way to create, format, and report
    structured errors and warnings. It is designed to produce informative,
    user-friendly diagnostics by associating messages with specific source code
    locations. *)

(** {2 Types and Exceptions} *)

type structured_msg = { msg : string; spans : (string option * Pos.t) list }
(** The primary type for all structured messages. It carries:
    - A main error message ([msg]).
    - A list of associated source code locations ([Pos.t]), each with an
      optional descriptive message. *)

val make : ?spans:(string option * Pos.t) list -> string -> structured_msg

val fmake :
  ?spans:(string option * Pos.t) list ->
  ('a, Format.formatter, unit, structured_msg) format4 ->
  'a

(** {2 Formatting Functions} *)

module type S = sig
  val debug_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val var_info_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val error_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val warning_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val result_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val format : Format.formatter -> structured_msg -> unit

  val create_progress_bar : string -> (string -> unit) * (string -> unit)
  (** Returns two functions: the first one, [current_progress], has to be called
      during the progress loop and the other one, [finish], has to be called at
      the end of the progressive task. *)
end

module ANSITerminal : S

module GNU : S

val debug_print : ('a, Format.formatter, unit, unit) format4 -> 'a

val error_print : ('a, Format.formatter, unit, unit) format4 -> 'a

val warning_print : ('a, Format.formatter, unit, unit) format4 -> 'a

val result_print : ('a, Format.formatter, unit, unit) format4 -> 'a

val create_progress_bar : string -> (string -> unit) * (string -> unit)

val format_structured_message : Format.formatter -> structured_msg -> unit
