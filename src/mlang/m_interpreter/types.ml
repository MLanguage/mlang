open M_ir

type 'a value = Number of 'a | Undefined

type run_error =
  | NanOrInf of string * Mir.expression Pos.marked
  | StructuredError of (Ppf.structured_msg * (unit -> unit) option)
