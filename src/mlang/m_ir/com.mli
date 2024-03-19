type cat_computed = Base | GivenBack

module CatCompSet : SetExt.T with type elt = cat_computed

type cat_variable = CatInput of StrSet.t | CatComputed of CatCompSet.t

module CatVarSet : SetExt.T with type elt = cat_variable

module CatVarMap : MapExt.T with type key = cat_variable

type cat_variable_loc = LocCalculated | LocBase | LocInput

type cat_variable_data = {
  id : cat_variable;
  id_str : string;
  id_int : int;
  loc : cat_variable_loc;
  pos : Pos.t;
  attributs : Pos.t StrMap.t;
}

type literal = Float of float | Undefined

(** The M language has an extremely odd way to specify looping. Rather than
    having first-class local mutable variables whose value change at each loop
    iteration, the M language prefers to use the changing loop parameter to
    instantiate the variable names inside the loop. For instance,

    {v somme(i=1..10:Xi) v}

    should evaluate to the sum of variables [X1], [X2], etc. Parameters can be
    number or characters and there can be multiple of them. We have to store all
    this information. *)

(** Values that can be substituted for loop parameters *)
type 'v atom = AtomVar of 'v | AtomLiteral of literal

type 'v set_value_loop =
  | Single of 'v atom Pos.marked
  | Range of 'v atom Pos.marked * 'v atom Pos.marked
  | Interval of 'v atom Pos.marked * 'v atom Pos.marked

type 'v loop_variable = char Pos.marked * 'v set_value_loop list
(** A loop variable is the character that should be substituted in variable
    names inside the loop plus the set of value to substitute. *)

(** There are two kind of loop variables declaration, but they are semantically
    the same though they have different concrete syntax. *)
type 'v loop_variables =
  | ValueSets of 'v loop_variable list
  | Ranges of 'v loop_variable list

(** Unary operators *)
type unop = Not | Minus

(** Binary operators *)
type binop = And | Or | Add | Sub | Mul | Div

(** Comparison operators *)
type comp_op = Gt | Gte | Lt | Lte | Eq | Neq

type 'v set_value =
  | FloatValue of float Pos.marked
  | VarValue of 'v Pos.marked
  | Interval of int Pos.marked * int Pos.marked

type func =
  | SumFunc  (** Sums the arguments *)
  | AbsFunc  (** Absolute value *)
  | MinFunc  (** Minimum of a list of values *)
  | MaxFunc  (** Maximum of a list of values *)
  | GtzFunc  (** Greater than zero (strict) ? *)
  | GtezFunc  (** Greater or equal than zero ? *)
  | NullFunc  (** Equal to zero ? *)
  | ArrFunc  (** Round to nearest integer *)
  | InfFunc  (** Truncate to integer *)
  | PresentFunc  (** Different than zero ? *)
  | Multimax  (** ??? *)
  | Supzero  (** ??? *)
  | VerifNumber
  | ComplNumber

type 'v expression_ =
  | TestInSet of bool * 'v m_expression_ * 'v set_value list
      (** Test if an expression is in a set of value (or not in the set if the
          flag is set to [false]) *)
  | Unop of unop * 'v m_expression_
  | Comparison of comp_op Pos.marked * 'v m_expression_ * 'v m_expression_
  | Binop of binop Pos.marked * 'v m_expression_ * 'v m_expression_
  | Index of 'v Pos.marked * 'v m_expression_
  | Conditional of 'v m_expression_ * 'v m_expression_ * 'v m_expression_ option
  | FuncCall of func Pos.marked * 'v m_expression_ list
  | FuncCallLoop of
      func Pos.marked * 'v loop_variables Pos.marked * 'v m_expression_
  | Literal of literal
  | Var of 'v
  | Loop of 'v loop_variables Pos.marked * 'v m_expression_
      (** The loop is prefixed with the loop variables declarations *)
  | NbCategory of CatVarSet.t Pos.marked
  | Attribut of 'v Pos.marked * string Pos.marked
  | Size of 'v Pos.marked
  | NbAnomalies
  | NbDiscordances
  | NbInformatives
  | NbBloquantes

and 'v m_expression_ = 'v expression_ Pos.marked

val pp_cat_variable : Pp.t -> cat_variable -> unit

val compare_cat_variable : cat_variable -> cat_variable -> int

val format_literal : Pp.t -> literal -> unit

val format_atom : (Pp.t -> 'v -> unit) -> Pp.t -> 'v atom -> unit

val format_loop_variables :
  (Pp.t -> 'v -> unit) -> Pp.t -> 'v loop_variables -> unit

val format_unop : Pp.t -> unop -> unit

val format_binop : Pp.t -> binop -> unit

val format_comp_op : Pp.t -> comp_op -> unit

val format_set_value : (Pp.t -> 'v -> unit) -> Pp.t -> 'v set_value -> unit

val format_func : Pp.t -> func -> unit

val format_expression : (Pp.t -> 'v -> unit) -> Pp.t -> 'v expression_ -> unit
