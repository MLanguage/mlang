type atom
(** An atomic proposition. *)

module AtomMap : Map.S with type key = atom

(** Abstract representation of boolean expressions used to represent definition
    tests. They should only be used for this and not for M boolean conditions
    (that are 3-valued boolean values). *)
type def_expr =
  | DEand of def_expr list
  | DEor of def_expr list
  | DEnot of def_expr
  | DEatom of atom

(** Module signature of expressions whose atoms are generic expressions. Generic
    expressions are replaced by atoms, and a map keeps the correspondance
    between expressions and atoms. If the same expression is used twice, it will
    be represented by the same atom. *)
module type S = sig
  type expr
  (** Generic expressions. *)

  type t
  (** Abstract boolean expression. *)

  val defalse : t
  (** Reprents the [false] litteral. *)

  val detrue : t
  (** Reprents the [true] litteral. *)

  val deand : t list -> t
  (** AND operator. Removes duplicates, applies basic boolean simplifications.
  *)

  val deor : t list -> t
  (** OR operator. Removes duplicates, applies basic boolean simplifications. *)

  val denot : t -> t
  (** NOT operator. Applies basic boolean simplifications. *)

  val devar : expr -> t
  (** An atomic expression. *)

  val deite : t -> t -> t -> t
  (** [deite c t e] Shortcut for [(c /\ t) \/ ((not c) /\ e)]. *)

  val get_expr : t -> def_expr
  (** Returns the raw definition expression. *)

  val get_assoc : t -> expr AtomMap.t
  (** Computes a map linking atoms to expressions. *)
end

module Make (OrderedExprs : sig
  type t

  val compare : t -> t -> int
end) : S with type expr = OrderedExprs.t
