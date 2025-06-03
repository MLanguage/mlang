module CatVar : sig
  type t = Input of StrSet.t | Computed of { is_base : bool }

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int

  module Set : SetExt.T with type elt = t

  module Map : sig
    include MapExt.T with type key = t

    val from_string_list : string Pos.marked list Pos.marked -> Pos.t t
  end

  type loc = LocInput | LocComputed | LocBase

  val pp_loc : Format.formatter -> loc -> unit

  module LocSet : SetExt.T with type elt = loc

  module LocMap : MapExt.T with type key = loc

  type data = {
    id : t;
    id_str : string;
    id_int : int;
    loc : loc;
    pos : Pos.t;
    attributs : Pos.t StrMap.t;
  }
end

(** Here are all the types a value can have. Date types don't seem to be used at
    all though. *)
type value_typ =
  | Boolean
  | DateYear
  | DateDayMonthYear
  | DateMonth
  | Integer
  | Real

type loc_tgv = {
  loc_cat : CatVar.loc;
  loc_idx : int;
  loc_tab_idx : int;
  loc_cat_id : CatVar.t;
  loc_cat_str : string;
  loc_cat_idx : int;
}

type loc_tmp = { loc_idx : int; loc_tab_idx : int; loc_cat_idx : int }

type loc =
  | LocTgv of string * loc_tgv
  | LocTmp of string * loc_tmp
  | LocRef of string * int

module Var : sig
  type id = int

  type tgv = {
    table : t Array.t option;
    alias : string Pos.marked option;  (** Input variable have an alias *)
    descr : string Pos.marked;
        (** Description taken from the variable declaration *)
    attrs : int Pos.marked StrMap.t;
    cat : CatVar.t;
    is_given_back : bool;
    typ : value_typ option;
  }

  and scope = Tgv of tgv | Temp of t Array.t option | Ref

  and t = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    id : id;
    loc : loc;
    scope : scope;
  }

  val tgv : t -> tgv

  val name : t -> string Pos.marked

  val name_str : t -> string

  val get_table : t -> t Array.t option

  val is_table : t -> bool

  val set_table : t -> t Array.t option -> t

  val cat_var_loc : t -> CatVar.loc

  val size : t -> int

  val alias : t -> string Pos.marked option

  val alias_str : t -> string

  val descr : t -> string Pos.marked

  val descr_str : t -> string

  val attrs : t -> int Pos.marked StrMap.t

  val cat : t -> CatVar.t

  val is_given_back : t -> bool

  val loc_tgv : t -> loc_tgv

  val loc_cat_idx : t -> int

  val set_loc_tgv_idx : t -> CatVar.data -> int -> t

  val set_loc_tmp_idx : t -> int -> t

  val loc_idx : t -> int

  val set_loc_idx : t -> int -> t

  val loc_tab_idx : t -> int

  val set_loc_tab_idx : t -> int -> t

  val is_tgv : t -> bool

  val is_temp : t -> bool

  val is_ref : t -> bool

  val init_loc : CatVar.t -> loc_tgv

  val new_tgv :
    name:string Pos.marked ->
    table:t Array.t option ->
    is_given_back:bool ->
    alias:string Pos.marked option ->
    descr:string Pos.marked ->
    attrs:int Pos.marked StrMap.t ->
    cat:CatVar.t ->
    typ:value_typ option ->
    t

  val new_temp : name:string Pos.marked -> table:t Array.t option -> t

  val new_ref : name:string Pos.marked -> t

  val new_arg : name:string Pos.marked -> t

  val new_res : name:string Pos.marked -> t

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int

  module Set : SetExt.T with type elt = t

  module Map : sig
    include MapExt.T with type key = t
  end

  (* val compare_name_ref : (string -> string -> int) ref

     val compare_name : string -> string -> int*)
end

type event_field = { name : string Pos.marked; index : int; is_var : bool }

type ('n, 'v) event_value = Numeric of 'n | RefVar of 'v

module DomainId : StrSet.T

module DomainIdSet :
  SetSetExt.T with type base_elt = string and type elt = DomainId.t

module DomainIdMap : MapExt.T with type key = DomainId.t

type 'a domain = {
  dom_id : DomainId.t Pos.marked;
  dom_names : Pos.t DomainIdMap.t;
  dom_by_default : bool;
  dom_min : DomainIdSet.t;
  dom_max : DomainIdSet.t;
  dom_rov : IntSet.t;
  dom_data : 'a;
  dom_used : int Pos.marked option;
}

type rule_domain_data = { rdom_computable : bool }

type rule_domain = rule_domain_data domain

type verif_domain_data = {
  vdom_auth : Pos.t CatVar.Map.t;
  vdom_verifiable : bool;
}

type verif_domain = verif_domain_data domain

type variable_space = {
  vs_id : int;
  vs_name : string Pos.marked;
  vs_cats : CatVar.loc Pos.marked CatVar.LocMap.t;
  vs_by_default : bool;
}

type literal = Float of float | Undefined

(** Unary operators *)
type unop = Not | Minus

(** Binary operators *)
type binop = And | Or | Add | Sub | Mul | Div | Mod

(** Comparison operators *)
type comp_op = Gt | Gte | Lt | Lte | Eq | Neq

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
  | NbEvents
  | Func of string

(** The M language has an extremely odd way to specify looping. Rather than
    having first-class local mutable variables whose value change at each loop
    iteration, the M language prefers to use the changing loop parameter to
    instantiate the variable names inside the loop. For instance,

    {v somme(i=1..10:Xi) v}

    should evaluate to the sum of variables [X1], [X2], etc. Parameters can be
    number or characters and there can be multiple of them. We have to store all
    this information. *)

type var_name_generic = { base : string; parameters : char list }
(** For generic variables, we record the list of their lowercase parameters *)

(** A variable is either generic (with loop parameters) or normal *)
type var_name = Normal of string | Generic of var_name_generic

type m_var_name = var_name Pos.marked

type ('s, 'v) access =
  | VarAccess of 's * 'v
  | TabAccess of 's * 'v * ('s, 'v) m_expression
  | ConcAccess of 's * m_var_name * string Pos.marked * ('s, 'v) m_expression
  | FieldAccess of ('s, 'v) m_expression * string Pos.marked * int

and ('s, 'v) m_access = ('s, 'v) access Pos.marked

(** Values that can be substituted for loop parameters *)
and 'v atom = AtomVar of 'v | AtomLiteral of literal

and 'v set_value_loop =
  | Single of 'v atom Pos.marked
  | Range of 'v atom Pos.marked * 'v atom Pos.marked
  | Interval of 'v atom Pos.marked * 'v atom Pos.marked

and 'v loop_variable = char Pos.marked * 'v set_value_loop list
(** A loop variable is the character that should be substituted in variable
    names inside the loop plus the set of value to substitute. *)

(** There are two kind of loop variables declaration, but they are semantically
    the same though they have different concrete syntax. *)
and 'v loop_variables =
  | ValueSets of 'v loop_variable list
  | Ranges of 'v loop_variable list

and ('s, 'v) set_value =
  | FloatValue of float Pos.marked
  | VarValue of ('s, 'v) m_access
  | IntervalValue of int Pos.marked * int Pos.marked

and ('s, 'v) expression =
  | TestInSet of bool * ('s, 'v) m_expression * ('s, 'v) set_value list
      (** Test if an expression is in a set of value (or not in the set if the
          flag is set to [false]) *)
  | Unop of unop * ('s, 'v) m_expression
  | Comparison of
      comp_op Pos.marked * ('s, 'v) m_expression * ('s, 'v) m_expression
  | Binop of binop Pos.marked * ('s, 'v) m_expression * ('s, 'v) m_expression
  | Conditional of
      ('s, 'v) m_expression
      * ('s, 'v) m_expression
      * ('s, 'v) m_expression option
  | FuncCall of func Pos.marked * ('s, 'v) m_expression list
  | FuncCallLoop of
      func Pos.marked * 'v loop_variables Pos.marked * ('s, 'v) m_expression
  | Literal of literal
  | Var of ('s, 'v) access
  | Loop of 'v loop_variables Pos.marked * ('s, 'v) m_expression
      (** The loop is prefixed with the loop variables declarations *)
  | NbCategory of Pos.t CatVar.Map.t
  | Attribut of ('s, 'v) m_access * string Pos.marked
  | Size of ('s, 'v) m_access
  | IsVariable of ('s, 'v) m_access * string Pos.marked
  | NbAnomalies
  | NbDiscordances
  | NbInformatives
  | NbBloquantes

and ('s, 'v) m_expression = ('s, 'v) expression Pos.marked

module Error : sig
  type typ = Anomaly | Discordance | Information

  val compare_typ : typ -> typ -> int

  type t = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    famille : string Pos.marked;
    code_bo : string Pos.marked;
    sous_code : string Pos.marked;
    libelle : string Pos.marked;
    is_isf : string Pos.marked;
    typ : typ;
  }

  val pp_descr : Pp.t -> t -> unit

  val pp : Pp.t -> t -> unit

  val compare : t -> t -> int

  module Set : SetExt.T with type elt = t

  module Map : sig
    include MapExt.T with type key = t
  end
end

type print_std = StdOut | StdErr

type print_info = Name | Alias

type ('s, 'v) print_arg =
  | PrintString of string
  | PrintAccess of print_info * ('s, 'v) access Pos.marked
  | PrintIndent of ('s, 'v) m_expression
  | PrintExpr of ('s, 'v) m_expression * int * int

(** In the M language, you can define multiple variables at once. This is the
    way they do looping since the definition can depend on the loop variable
    value (e.g [Xi] can depend on [i]). *)

type 'v formula_loop = 'v loop_variables Pos.marked

type ('s, 'v) formula_decl =
  | VarDecl of ('s, 'v) access Pos.marked * ('s, 'v) m_expression
  | EventFieldRef of ('s, 'v) m_expression * string Pos.marked * int * 'v

type ('s, 'v) formula =
  | SingleFormula of ('s, 'v) formula_decl
  | MultipleFormulaes of 'v formula_loop * ('s, 'v) formula_decl

type ('s, 'v, 'e) instruction =
  | Affectation of ('s, 'v) formula Pos.marked
  | IfThenElse of
      ('s, 'v) m_expression
      * ('s, 'v, 'e) m_instruction list
      * ('s, 'v, 'e) m_instruction list
  | WhenDoElse of
      (('s, 'v) m_expression * ('s, 'v, 'e) m_instruction list * Pos.t) list
      * ('s, 'v, 'e) m_instruction list Pos.marked
  | ComputeDomain of string Pos.marked list Pos.marked
  | ComputeChaining of string Pos.marked
  | ComputeVerifs of string Pos.marked list Pos.marked * ('s, 'v) m_expression
  | ComputeTarget of string Pos.marked * 'v list
  | VerifBlock of ('s, 'v, 'e) m_instruction list
  | Print of print_std * ('s, 'v) print_arg Pos.marked list
  | Iterate of
      'v
      * 'v list
      * (Pos.t CatVar.Map.t * ('s, 'v) m_expression) list
      * ('s, 'v, 'e) m_instruction list
  | Iterate_values of
      'v
      * (('s, 'v) m_expression * ('s, 'v) m_expression * ('s, 'v) m_expression)
        list
      * ('s, 'v, 'e) m_instruction list
  | Restore of
      'v list
      * ('v * Pos.t CatVar.Map.t * ('s, 'v) m_expression) list
      * ('s, 'v) m_expression list
      * ('v * ('s, 'v) m_expression) list
      * ('s, 'v, 'e) m_instruction list
  | ArrangeEvents of
      ('v * 'v * ('s, 'v) m_expression) option
      * ('v * ('s, 'v) m_expression) option
      * ('s, 'v) m_expression option
      * ('s, 'v, 'e) m_instruction list
  | RaiseError of 'e Pos.marked * string Pos.marked option
  | CleanErrors
  | ExportErrors
  | FinalizeErrors

and ('s, 'v, 'e) m_instruction = ('s, 'v, 'e) instruction Pos.marked

type ('s, 'v, 'e) target = {
  target_name : string Pos.marked;
  target_file : string option;
  target_apps : string Pos.marked StrMap.t;
  target_args : 'v list;
  target_result : 'v option;
  target_tmp_vars : 'v StrMap.t;
  target_nb_tmps : int;
  target_sz_tmps : int;
  target_nb_refs : int;
  target_prog : ('s, 'v, 'e) m_instruction list;
}

val target_is_function : ('s, 'v, 'e) target -> bool

val expr_map_var :
  ('s -> 't) -> ('v -> 'w) -> ('s, 'v) expression -> ('t, 'w) expression

val m_expr_map_var :
  ('s -> 't) -> ('v -> 'w) -> ('s, 'v) m_expression -> ('t, 'w) m_expression

val instr_map_var :
  ('s -> 't) ->
  ('v -> 'w) ->
  ('e -> 'f) ->
  ('s, 'v, 'e) instruction ->
  ('t, 'w, 'f) instruction

val m_instr_map_var :
  ('s -> 't) ->
  ('v -> 'w) ->
  ('e -> 'f) ->
  ('s, 'v, 'e) m_instruction ->
  ('t, 'w, 'f) m_instruction

val get_var_name : var_name -> string

val get_normal_var : var_name -> string

val format_value_typ : Pp.t -> value_typ -> unit

val format_literal : Pp.t -> literal -> unit

val format_atom : (Pp.t -> 'v -> unit) -> Pp.t -> 'v atom -> unit

val format_loop_variables :
  (Pp.t -> 'v -> unit) -> Pp.t -> 'v loop_variables -> unit

val format_unop : Pp.t -> unop -> unit

val format_binop : Pp.t -> binop -> unit

val format_comp_op : Pp.t -> comp_op -> unit

val format_set_value :
  (Pp.t -> 's -> unit) ->
  (Pp.t -> 'v -> unit) ->
  (Pp.t -> ('s, 'v) expression -> unit) ->
  Pp.t ->
  ('s, 'v) set_value ->
  unit

val format_func : Pp.t -> func -> unit

val format_expression :
  (Pp.t -> 's -> unit) ->
  (Pp.t -> 'v -> unit) ->
  Pp.t ->
  ('s, 'v) expression ->
  unit

val format_print_arg :
  (Pp.t -> 's -> unit) ->
  (Pp.t -> 'v -> unit) ->
  Pp.t ->
  ('s, 'v) print_arg ->
  unit

val format_formula :
  (Pp.t -> 's -> unit) ->
  (Pp.t -> 'v -> unit) ->
  Pp.t ->
  ('s, 'v) formula ->
  unit

val format_instruction :
  (Pp.t -> 's -> unit) ->
  (Pp.t -> 'v -> unit) ->
  (Pp.t -> 'e -> unit) ->
  Pp.t ->
  ('s, 'v, 'e) instruction ->
  unit

val format_instructions :
  (Pp.t -> 's -> unit) ->
  (Pp.t -> 'v -> unit) ->
  (Pp.t -> 'e -> unit) ->
  Pp.t ->
  ('s, 'v, 'e) m_instruction list ->
  unit
