open M_ir

(** A value representation: either undefined or a number. *)
type 'a value = Number of 'a | Undefined

(** The different kinds of errors the interpretor may raise. *)
type run_error =
  | NanOrInf of string * Mir.expression Pos.marked
  | StructuredError of (Ppf.structured_msg * (unit -> unit) option)
