module CatVar : sig
  type t = Input of StrSet.t | Computed of { is_base : bool }

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int

  module Set : SetExt.T with type elt = t

  module Map : sig
    include MapExt.T with type key = t

    val from_string_list : string Pos.marked list Pos.marked -> Pos.t t
  end

  type loc = LocComputed | LocBase | LocInput

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

type variable_space = {
  vs_id : int;
  vs_name : string Pos.marked;
  vs_cats : CatVar.loc Pos.marked CatVar.LocMap.t;
  vs_by_default : bool;
}

type verif_domain = verif_domain_data domain

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

type var_space = (m_var_name * int) option

type 'v access =
  | VarAccess of var_space * 'v
  | TabAccess of var_space * 'v * 'v m_expression
  | FieldAccess of var_space * 'v m_expression * string Pos.marked * int

and 'v m_access = 'v access Pos.marked

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

and 'v set_value =
  | FloatValue of float Pos.marked
  | VarValue of 'v m_access
  | IntervalValue of int Pos.marked * int Pos.marked

and 'v expression =
  | TestInSet of bool * 'v m_expression * 'v set_value list
      (** Test if an expression is in a set of value (or not in the set if the
          flag is set to [false]) *)
  | Unop of unop * 'v m_expression
  | Comparison of comp_op Pos.marked * 'v m_expression * 'v m_expression
  | Binop of binop Pos.marked * 'v m_expression * 'v m_expression
  | Conditional of 'v m_expression * 'v m_expression * 'v m_expression option
  | FuncCall of func Pos.marked * 'v m_expression list
  | FuncCallLoop of
      func Pos.marked * 'v loop_variables Pos.marked * 'v m_expression
  | Literal of literal
  | Var of 'v access
  | Loop of 'v loop_variables Pos.marked * 'v m_expression
      (** The loop is prefixed with the loop variables declarations *)
  | NbCategory of Pos.t CatVar.Map.t
  | Attribut of 'v m_access * string Pos.marked
  | Size of 'v m_access
  | IsVariable of 'v m_access * string Pos.marked
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

type 'v print_arg =
  | PrintString of string
  | PrintAccess of print_info * 'v m_access
  | PrintIndent of 'v m_expression
  | PrintExpr of 'v m_expression * int * int

(** In the M language, you can define multiple variables at once. This is the
    way they do looping since the definition can depend on the loop variable
    value (e.g [Xi] can depend on [i]). *)

type 'v formula_loop = 'v loop_variables Pos.marked

type 'v formula_decl =
  | VarDecl of 'v access Pos.marked * 'v m_expression
  | EventFieldRef of 'v m_expression * string Pos.marked * int * 'v

type 'v formula =
  | SingleFormula of 'v formula_decl
  | MultipleFormulaes of 'v formula_loop * 'v formula_decl

type ('v, 'e) instruction =
  | Affectation of 'v formula Pos.marked
  | IfThenElse of
      'v m_expression
      * ('v, 'e) m_instruction list
      * ('v, 'e) m_instruction list
  | WhenDoElse of
      ('v m_expression * ('v, 'e) m_instruction list * Pos.t) list
      * ('v, 'e) m_instruction list Pos.marked
  | ComputeDomain of string Pos.marked list Pos.marked * var_space
  | ComputeChaining of string Pos.marked * var_space
  | ComputeVerifs of
      string Pos.marked list Pos.marked * 'v m_expression * var_space
  | ComputeTarget of string Pos.marked * 'v m_access list * var_space
  | VerifBlock of ('v, 'e) m_instruction list
  | Print of print_std * 'v print_arg Pos.marked list
  | Iterate of
      'v
      * 'v m_access list
      * (Pos.t CatVar.Map.t * 'v m_expression) list
      * ('v, 'e) m_instruction list
  | Iterate_values of
      'v
      * ('v m_expression * 'v m_expression * 'v m_expression) list
      * ('v, 'e) m_instruction list
  | Restore of
      'v m_access list
      * ('v * Pos.t CatVar.Map.t * 'v m_expression * var_space) list
      * 'v m_expression list
      * ('v * 'v m_expression) list
      * ('v, 'e) m_instruction list
  | ArrangeEvents of
      ('v * 'v * 'v m_expression) option
      * ('v * 'v m_expression) option
      * 'v m_expression option
      * ('v, 'e) m_instruction list
  | RaiseError of 'e Pos.marked * string Pos.marked option
  | CleanErrors
  | ExportErrors
  | FinalizeErrors

and ('v, 'e) m_instruction = ('v, 'e) instruction Pos.marked

type ('v, 'e) target = {
  target_name : string Pos.marked;
  target_file : string option;
  target_apps : string Pos.marked StrMap.t;
  target_args : 'v list;
  target_result : 'v option;
  target_tmp_vars : 'v StrMap.t;
  target_nb_tmps : int;
  target_sz_tmps : int;
  target_nb_refs : int;
  target_prog : ('v, 'e) m_instruction list;
}

val target_is_function : ('v, 'e) target -> bool

val expr_map_var : ('v -> 'w) -> 'v expression -> 'w expression

val m_expr_map_var : ('v -> 'w) -> 'v m_expression -> 'w m_expression

val instr_map_var :
  ('v -> 'w) -> ('e -> 'f) -> ('v, 'e) instruction -> ('w, 'f) instruction

val m_instr_map_var :
  ('v -> 'w) -> ('e -> 'f) -> ('v, 'e) m_instruction -> ('w, 'f) m_instruction

type var_usage = Read | Write | Info | DeclRef | ArgRef | DeclLocal | Macro

val expr_fold_var :
  (var_usage -> var_space -> 'v option -> 'a -> 'a) -> 'v expression -> 'a -> 'a

val m_expr_fold_var :
  (var_usage -> var_space -> 'v option -> 'a -> 'a) ->
  'v m_expression ->
  'a ->
  'a

val instr_fold_var :
  (var_usage -> var_space -> 'v option -> 'a -> 'a) ->
  ('v, 'e) instruction ->
  'a ->
  'a

val m_instr_fold_var :
  (var_usage -> var_space -> 'v option -> 'a -> 'a) ->
  ('v, 'e) m_instruction ->
  'a ->
  'a

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
  (Pp.t -> 'v -> unit) ->
  (Pp.t -> 'v expression -> unit) ->
  Pp.t ->
  'v set_value ->
  unit

val format_func : Pp.t -> func -> unit

val format_expression : (Pp.t -> 'v -> unit) -> Pp.t -> 'v expression -> unit

val format_print_arg : (Pp.t -> 'v -> unit) -> Pp.t -> 'v print_arg -> unit

val format_formula : (Pp.t -> 'v -> unit) -> Pp.t -> 'v formula -> unit

val format_instruction :
  (Pp.t -> 'v -> unit) ->
  (Pp.t -> 'e -> unit) ->
  Pp.t ->
  ('v, 'e) instruction ->
  unit

val format_instructions :
  (Pp.t -> 'v -> unit) ->
  (Pp.t -> 'e -> unit) ->
  Pp.t ->
  ('v, 'e) m_instruction list ->
  unit
