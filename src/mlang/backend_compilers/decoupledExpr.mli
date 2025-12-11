val generate_variable :
  ?def_flag:bool -> ?trace_flag:bool -> Com.var_space -> Com.Var.t -> string

type dflag = Def | Val

(** {1 Low-level M computation} *)

(** This module distinguish defineness and valuation computations of M
    expressions so they can be expressed independantly and more thoroughtly
    optimized. Definition of such expression follows as such:

    - Express the computation of definess and valuation independantly through
      the use of constructors in {!section:constr}
    - Sub-expressions can be used to build up the M expression tree (see
      {!expression_composition})
    - A fully defined expression can be processed into a optimized value that
      can be printed ({!build_expression}) *)

(** {2 Local variables} *)

type local_var
(** Variable local to the computed expression *)

val locals_from_m : unit -> local_var * local_var
(** Return a couple of local variable from a MIR one, for defineness and
    valuation in this order. *)

val new_local : unit -> local_var
(** Create a fresh local variable *)

(** {2:constr Expression constructors} *)

(** These are the smart constructors used to build expressions. In effect, they
    represent the language in which computations are defined. *)

(** They are used as traditional nestable sum-type constructors. An example:

    {v 0.5 * (x + y) v}

    would turn into

    {[
      mult (lit 0.5) (plus (local_var x) (local_var y))
    ]}

    where [x] and [y] are previously defined {!local_var}s *)

type constr
(** Constructed decoupled expression *)

val dtrue : constr
(** True value *)

val dfalse : constr
(** False value *)

val lit : float -> constr
(** Float literal *)

val m_var : Com.var_space -> Com.Var.t -> dflag -> constr
(** Value from TGV. [m_var v off df] represents an access to the TGV variable
    [v] with [df] to read defineness or valuation. [off] is the access type for
    M array, and should be [None] most of the time. For array access, see
    {!access}. *)

val let_local : local_var -> constr -> constr -> constr
(** Local let-binding. [let_local v defining_expr body_expr] is akin to OCaml
    [let v = defining_expr in body_expr] *)

val local_var : local_var -> constr
(** Access local variable value *)

val dand : constr -> constr -> constr
(** Boolean and *)

val dor : constr -> constr -> constr
(** Boolean or *)

val dnot : constr -> constr
(** Boolean not *)

val minus : constr -> constr
(** Negate value *)

val plus : constr -> constr -> constr
(** Float addition *)

val sub : constr -> constr -> constr
(** Float substraction *)

val mult : constr -> constr -> constr
(** Float multiplication *)

val div : constr -> constr -> constr
(** Float division. Care to guard for division by zero as it is not intrisectly
    guarranteed *)

val modulo : constr -> constr -> constr
(** Float modulo. Care to guard for modulo by zero as it is not intrisectly
    guarranteed *)

val comp : string -> constr -> constr -> constr
(** Comparison operation. The operator is given as C-style string literal *)

val dfun : string -> constr list -> constr
(** Function call *)

val dinstr : string -> constr
(** Direct instruction *)

val ddirect : constr -> constr
(** Direct instruction, not pushed *)

val ite : constr -> constr -> constr -> constr
(** Functionnal if-the-else construction. [ite cond_expr then_expr else_expr] is
    akin to [if cond_expr then then_expr else else_expr] *)

(** {2 Decoupled expressions} *)

(** While {!constr} is the expression language for decoupled values, the
    following represents complete and optimized expressions for M computations
*)

type expression_composition = {
  set_vars : (dflag * string * constr) list;
  def_test : constr;
  value_comp : constr;
}
(** Representation of an M computation in construction. [def_test] for the
    defineness flag, and [value_comp] for the actual valuation. *)

val build_transitive_composition :
  ?safe_def:bool -> expression_composition -> expression_composition
(** Refine an expression composition to enfore M invariants. Mainly the fact
    that undefined value are valuated to zero. [value_comp] of the argument is
    expected to be defined assuming the expression {i is} defined. [safe_def],
    which defaults to [false], can be set when the defined [value_comp]
    computation will evaluate to zero if [def_test] do, allowing the guard to be
    optimized away. *)

type t
(** Decoupled expression type. Closed representation of a computation. *)

val is_always_true : t -> bool
(** Tells if the expression [t] reprensents a value statically different to zero
*)

type local_decls
(** Representation of local variables existing in an expression *)

val build_expression :
  expression_composition -> local_decls * (dflag * string * t) list * t * t
(** Crush {!constr} values into closed expressions {!t} *)

val format_local_declarations : Format.formatter -> local_decls -> unit

val format_assign :
  Dgfip_options.flags -> string -> Format.formatter -> t -> unit

val format_set_vars :
  Dgfip_options.flags -> Format.formatter -> (dflag * string * t) list -> unit
