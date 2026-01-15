module Make
    (N : M_ir.Mir_number.NumberInterface)
    (R : M_ir.Mir_roundops.RoundOpsInterface with type t = N.t) : sig
  val arr : N.t Types.value -> N.t Types.value

  val inf : N.t Types.value -> N.t Types.value

  val present : 'a Types.value -> N.t Types.value

  val supzero : N.t Types.value -> N.t Types.value

  val abs : N.t Types.value -> N.t Types.value

  val min : N.t Types.value -> N.t Types.value -> N.t Types.value

  val max : N.t Types.value -> N.t Types.value -> N.t Types.value

  val multimax :
    N.t Types.value ->
    [ `Table of N.t Types.value list | `Var of N.t Types.value ] ->
    N.t Types.value

  val nb_events : 'a Types.ctx -> N.t Types.value
end
