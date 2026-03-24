open M_ir

type 'a value = Number of 'a | Undefined

type print_ctx = { mutable indent : int; mutable is_newline : bool }

type run_error =
  | NanOrInf of string * Mir.expression Pos.marked
  | StructuredError of
      (string * (string option * Pos.t) list * (unit -> unit) option)
