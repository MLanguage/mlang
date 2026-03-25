open M_ir

type 'a value = Number of 'a | Undefined

type run_error =
  | NanOrInf of string * Mir.expression Pos.marked
  | StructuredError of
      (string * (string option * Pos.t) list * (unit -> unit) option)
