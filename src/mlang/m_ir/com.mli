module CatVar : sig
  type t = Input of StrSet.t | Computed of { is_base : bool }

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int

  module Set : SetExt.T with type elt = t

  module Map : MapExt.T with type key = t

  type loc = LocCalculated | LocBase | LocInput

  type data = {
    id : t;
    id_str : string;
    id_int : int;
    loc : loc;
    pos : Pos.t;
    attributs : Pos.t StrMap.t;
  }
end

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

type 'v expression =
  | TestInSet of bool * 'v m_expression * 'v set_value list
      (** Test if an expression is in a set of value (or not in the set if the
          flag is set to [false]) *)
  | Unop of unop * 'v m_expression
  | Comparison of comp_op Pos.marked * 'v m_expression * 'v m_expression
  | Binop of binop Pos.marked * 'v m_expression * 'v m_expression
  | Index of 'v Pos.marked * 'v m_expression
  | Conditional of 'v m_expression * 'v m_expression * 'v m_expression option
  | FuncCall of func Pos.marked * 'v m_expression list
  | FuncCallLoop of
      func Pos.marked * 'v loop_variables Pos.marked * 'v m_expression
  | Literal of literal
  | Var of 'v
  | Loop of 'v loop_variables Pos.marked * 'v m_expression
      (** The loop is prefixed with the loop variables declarations *)
  | NbCategory of Pos.t CatVar.Map.t
  | Attribut of 'v Pos.marked * string Pos.marked
  | Size of 'v Pos.marked
  | NbAnomalies
  | NbDiscordances
  | NbInformatives
  | NbBloquantes

and 'v m_expression = 'v expression Pos.marked

module Error : sig
  type typ = Anomaly | Discordance | Information

  val compare_typ : typ -> typ -> int

  type t = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    kind : string Pos.marked;
    major_code : string Pos.marked;
    minor_code : string Pos.marked;
    description : string Pos.marked;
    isisf : string Pos.marked;
    typ : typ;
  }

  val pp_descr : Pp.t -> t -> unit

  val compare : t -> t -> int
end

type print_std = StdOut | StdErr

type 'v print_arg =
  | PrintString of string
  | PrintName of 'v Pos.marked
  | PrintAlias of 'v Pos.marked
  | PrintIndent of 'v m_expression
  | PrintExpr of 'v m_expression * int * int

type 'v instruction =
  | Affectation of 'v * (int * 'v m_expression) option * 'v m_expression
  | IfThenElse of
      'v m_expression * 'v m_instruction list * 'v m_instruction list
  | ComputeTarget of string Pos.marked
  | VerifBlock of 'v m_instruction list
  | Print of print_std * 'v print_arg Pos.marked list
  | Iterate of 'v * Pos.t CatVar.Map.t * 'v m_expression * 'v m_instruction list
  | Restore of
      'v list
      * ('v * Pos.t CatVar.Map.t * 'v m_expression) list
      * 'v m_instruction list
  | RaiseError of Error.t * string Pos.marked option
  | CleanErrors
  | ExportErrors
  | FinalizeErrors

and 'v m_instruction = 'v instruction Pos.marked

val format_literal : Pp.t -> literal -> unit

val format_atom : (Pp.t -> 'v -> unit) -> Pp.t -> 'v atom -> unit

val format_loop_variables :
  (Pp.t -> 'v -> unit) -> Pp.t -> 'v loop_variables -> unit

val format_unop : Pp.t -> unop -> unit

val format_binop : Pp.t -> binop -> unit

val format_comp_op : Pp.t -> comp_op -> unit

val format_set_value : (Pp.t -> 'v -> unit) -> Pp.t -> 'v set_value -> unit

val format_func : Pp.t -> func -> unit

val format_expression : (Pp.t -> 'v -> unit) -> Pp.t -> 'v expression -> unit

val format_print_arg : (Pp.t -> 'v -> unit) -> Pp.t -> 'v print_arg -> unit

val format_instruction : (Pp.t -> 'v -> unit) -> Pp.t -> 'v instruction -> unit

val format_instructions :
  (Pp.t -> 'v -> unit) -> Pp.t -> 'v m_instruction list -> unit
