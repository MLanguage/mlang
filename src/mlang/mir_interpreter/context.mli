module type S = sig
  type custom_float

  val get_var_space :
    custom_float Types.ctx -> M_ir.Com.var_space -> M_ir.Com.variable_space
  (** Returns the variable space of a given variable. *)

  val get_var :
    custom_float Types.ctx ->
    M_ir.Com.var_space ->
    M_ir.Com.Var.t ->
    M_ir.Com.variable_space * M_ir.Com.Var.t * int
  (** Returns the variable identifier and its space, with an offset integer.
      This offset integer is 0 for TGV variables *)

  val get_var_tab :
    custom_float Types.ctx -> M_ir.Com.Var.t -> int -> M_ir.Com.Var.t
  (** [get_var_tab ctx vs v i] Each cell of a table is a separate variable. This
      function returns the variable representing the cell [i] in table [v].
      Fails if the variable in argument is not a table. *)

  val get_var_value_org :
    custom_float Types.ctx ->
    M_ir.Com.variable_space ->
    M_ir.Com.Var.t ->
    int ->
    custom_float Types.value
  (** *)

  val get_var_value :
    custom_float Types.ctx ->
    M_ir.Com.var_space ->
    M_ir.Com.Var.t ->
    custom_float Types.value

  val get_var_value_tab :
    custom_float Types.ctx ->
    M_ir.Com.var_space ->
    M_ir.Com.Var.t ->
    int ->
    custom_float Types.value

  val set_var_ref :
    custom_float Types.ctx ->
    M_ir.Com.Var.t ->
    M_ir.Com.variable_space ->
    M_ir.Com.Var.t ->
    int ->
    unit

  val get_access_value :
    eval:
      (custom_float Types.ctx ->
      M_ir.Com.Var.t M_ir.Com.m_expression ->
      custom_float Types.value) ->
    custom_float Types.ctx ->
    M_ir.Com.Var.t M_ir.Com.access ->
    custom_float Types.value

  val get_access_var :
    eval:
      (custom_float Types.ctx ->
      M_ir.Com.Var.t M_ir.Com.m_expression ->
      custom_float Types.value) ->
    custom_float Types.ctx ->
    M_ir.Com.Var.t M_ir.Com.access ->
    (M_ir.Com.variable_space * M_ir.Com.Var.t * int) option

  val set_var_value_org :
    custom_float Types.ctx ->
    M_ir.Com.variable_space ->
    M_ir.Com.Var.t ->
    int ->
    custom_float Types.value ->
    unit

  val set_var_value :
    custom_float Types.ctx ->
    M_ir.Com.var_space ->
    M_ir.Com.Var.t ->
    custom_float Types.value ->
    unit

  val set_var_value_tab :
    custom_float Types.ctx ->
    M_ir.Com.var_space ->
    M_ir.Com.Var.t ->
    int ->
    custom_float Types.value ->
    unit

  val set_access :
    eval:
      (custom_float Types.ctx ->
      M_ir.Com.Var.t M_ir.Com.m_expression ->
      custom_float Types.value) ->
    custom_float Types.ctx ->
    M_ir.Com.Var.t M_ir.Com.access ->
    custom_float Types.value ->
    unit
end

val empty_ctx :
  ?inputs:'a Types.value M_ir.Com.Var.Map.t ->
  ?events:('a Types.value, M_ir.Com.Var.t) M_ir.Com.event_value StrMap.t list ->
  M_ir.Mir.program ->
  'a Types.ctx
(** [empty_ctx ?inputs ?events p]

    Creates a fresh context for executing the program [p] or expressions within
    the context of [p] (for example, with variables declared in [p]. Parameters
    [inputs] and [events] are required for interpreting the whole program. *)

module Make (N : M_ir.Mir_number.NumberInterface) :
  S with type custom_float := N.t
