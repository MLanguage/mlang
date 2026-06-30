type local_var = private Anon | Refered of int

(** States the kind/type of a variable/expression. *)
type dflag =
  | Def  (** For pure boolean expressions, used for definition check *)
  | Val  (** For arithmetical expressions, used for actual calculations *)
  | VarInfo  (** For variable data *)
  | VarSpace  (** For var spaces *)

(** Constuctors used for building C instructions. *)
type t =
  | True
  | False
  | Lit of float
  | M of Com.var_space * Com.Var.t * dflag
  | Local of local_var
  | And of t * t
  | Or of t * t
  | Not of t
  | Minus of t
  | Plus of t * t
  | Sub of t * t
  | Mult of t * t
  | Div of t * t
  | Modulo of t * t
  | Comp of string * t * t
  | Fun of string * t list
  | Varinfo of Dgfip_varid.varinfo
  | Varinfo_tab of Dgfip_varid.varinfo * t * t
  | Varinfo_field of t * t * string
  | Varspace_current of Com.var_space
  | Varspace_of of Com.var_space * Dgfip_varid.varinfo
  | Typ of Com.value_typ
  | Instr of string
  | Direct of t
  | Ite of t * t * t
  | It0 of t * t
  | Let_local of local_var * t * t

val irdata : t
(** A shortcut for representing the irdata construction
    ([Direct Instr "irdata"]) *)

val compare : t -> t -> int

(** {2 Local variables} *)

val anon : local_var

val locals_from_m : unit -> local_var * local_var
(** Return a couple of local variable from a MIR one, for defineness and
    valuation in this order. *)

val new_local : unit -> local_var
(** Create a fresh local variable *)
