(** Implementation of several function calls as described in
    {{:../../../../../fonctions.html}the functions documentation}.

    Note: the interpreter implementation is supposed to be the legitimate
    specification. If there are inconsistencies between the documentation or the
    C code and the actual behavior of the interprer, the interpreter is the
    reference. *)

module Make (N : Number.S) : sig
  val arr : N.t Types.value -> N.t Types.value
  (** Implements the 'arr' call (rounding). *)

  val inf : N.t Types.value -> N.t Types.value
  (** Implements the 'inf' call (truncate). *)

  val present : 'a Types.value -> N.t Types.value
  (** Implements the 'present' call that checks if the value xis not equal to
      undefined. *)

  val supzero : N.t Types.value -> N.t Types.value
  (** Implements the 'supzero' call, which returns undefined for strictly
      negative values or the argunment otherwise. *)

  val abs : N.t Types.value -> N.t Types.value
  (** Implements the 'abs' call, calculating the absolute value of its argument.
  *)

  val min : N.t Types.value -> N.t Types.value -> N.t Types.value
  (** Implements the 'min' call, returning the minimum between two values. *)

  val max : N.t Types.value -> N.t Types.value -> N.t Types.value
  (** Implements the 'max' call, returning the maximum between two values. *)

  val multimax : N.t Types.value -> N.t Types.value list -> N.t Types.value
  (** Implements the 'multimax' call, returning the max value of a subtable. *)

  val nb_events : (N.t, _) Context.t -> N.t Types.value
  (** Implements the 'nb_events' call, returning the number of currently defined
      events. *)
end
