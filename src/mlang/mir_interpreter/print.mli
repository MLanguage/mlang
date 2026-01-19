module Make
    (N : M_ir.Mir_number.NumberInterface)
    (_ : Context.S with type custom_float := N.t) : sig
  val fresh : M_ir.Com.print_std -> N.t Types.ctx -> N.t Types.pctx

  val flush : N.t Types.pctx -> unit

  val value : N.t Types.pctx -> int -> int -> N.t Types.value -> unit

  val string : N.t Types.pctx -> string -> unit

  val access :
    eval:
      (N.t Types.ctx -> M_ir.Com.Var.t M_ir.Com.m_expression -> N.t Types.value) ->
    N.t Types.pctx ->
    M_ir.Com.print_info ->
    M_ir.Com.Var.t M_ir.Com.access ->
    unit

  val indent : N.t Types.pctx -> N.t Types.value -> unit
end
