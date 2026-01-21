module Make (N : M_ir.Mir_number.NumberInterface) : sig
  val fresh : M_ir.Com.print_std -> N.t Types.ctx -> N.t Types.pctx
  (** Returns a fresh printing context. *)

  val flush : N.t Types.pctx -> unit
  (** Flushes the used formatter. *)

  val value : N.t Types.pctx -> int -> int -> N.t Types.value -> unit
  (** [value pctx mi ma v]
      
      Prints [v] with a precision between [mi] and [ma]. *)

  val string : N.t Types.pctx -> string -> unit
  (** Prints a string. *)

  val access :
    N.t Types.pctx ->
    M_ir.Com.print_info ->
    M_ir.Com.variable_space ->
    M_ir.Com.Var.t ->
    unit
  (** Prints a variable name/alias within its variable space. *)

  val indent : N.t Types.pctx -> N.t Types.value -> unit
  (** Sets a new indentation in the current printing context. *)
end
