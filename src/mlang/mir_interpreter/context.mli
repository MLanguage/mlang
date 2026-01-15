val empty_ctx :
  ?inputs:'a Types.value M_ir.Com.Var.Map.t ->
  ?events:('a Types.value, M_ir.Com.Var.t) M_ir.Com.event_value StrMap.t list ->
  M_ir.Mir.program ->
  'a Types.ctx
(** [empty_ctx ?inputs ?events p]

    Creates a fresh context for executing the program [p] or expressions within
    the context of [p] (for example, with variables declared in [p].
    Parameters [inputs] and [events] are required for interpreting the whole
    program. *)
