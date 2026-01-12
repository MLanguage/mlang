(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

(** {2 Categories} *)

(** High level representation of categories of variable.
    In practice, there only are two types of categories:
    * "calculee" (Input) variables, with several sub categories;
    * "saisie" (Computed) variable, that can either be base or non-base.

    Such representation allows to manipulate sets of variable (for example
    in iterations). *)
module CatVar : sig
  type t =
    | Input of StrSet.t
        (** An input variable category with its sub categories *)
    | Computed of { is_base : bool }
        (** A computed var with a flag marking if it is a base varible. *)

  val all_inputs : t
  (** A category containing all inputs. *)

  val is_input : t -> bool
  (** Returns [true] if the category in argument is an input variable category. *)

  val is_computed : t -> bool
  (** Returns [true] if the category in argument is a computed variable category. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer. *)

  val compare : t -> t -> int

  module Set : SetExt.T with type elt = t

  module Map : sig
    include MapExt.T with type key = t

    val from_string_list : string Pos.marked list Pos.marked -> Pos.t t
    (** [from_string_list l]

        From a category identifier [l] seen as a list of strings (for example
        [\["saisie"; "revenu"; "corrective"\]] for the category
        ["saisie revenu corrective"]),
        returns a map binding all [CatVar.t] values refered by [l] to the location
        of [l]. *)
  end

  type loc = LocComputed | LocBase | LocInput

  val pp_loc : Format.formatter -> loc -> unit

  module LocSet : SetExt.T with type elt = loc

  module LocMap : MapExt.T with type key = loc

  type data = {
    id : t;  (** The category *)
    id_str : string;  (** A unique string identifier for the category *)
    id_int : int;  (** A unique int identifier *)
    loc : loc;  (** Quick access to the category kind *)
    pos : Pos.t;  (** Variable category declaration *)
    attributs : Pos.t StrMap.t;  (** Attributes of the category *)
  }
  (** Data for a given category. Defined in the validator. *)
end

(** Here are all the types a value can have. Used for the M function [type]. *)
type value_typ =
  | Boolean
  | DateYear
  | DateDayMonthYear
  | DateMonth
  | Integer
  | Real

(** {2 Variables} *)

(** Note: TGV stands for "Tableau Général des Variables".
    This is where the variables are declared. *)

type loc_tgv = {
  loc_cat : CatVar.loc;  (** Its category kind  *)
  loc_idx : int;  (** TODO *)
  loc_tab_idx : int;  (** TODO *)
  loc_cat_id : CatVar.t;  (** Full details on its category *)
  loc_cat_str : string;  (** String representation of its category *)
  loc_cat_idx : int;  (** The variable's identifier in its category. *)
}
(** Location of a TGV variable in the compiler's context. *)

type loc_tmp = {
  loc_idx : int;  (** TODO *)
  loc_tab_idx : int;  (** TODO *)
  loc_cat_idx : int;  (** The variable's identifier in its category *)
}
(** Location of a temporary variable in the compiler's context. *)

(** The different kinds of variables; the 'string' is the variable's name. *)
type loc =
  | LocTgv of string * loc_tgv  (** A TGV variable *)
  | LocTmp of string * loc_tmp  (** A temporary variable *)
  | LocRef of string * int  (** A reference (the integer is the 'loc_idx') *)

module Var : sig
  type id = int

  (** Data on a TGV variable. *)
  type tgv = {
    table : t Array.t option;
        (** The array of cells if the variable is a table. *)
    alias : string Pos.marked option;  (** Input variable have an alias *)
    descr : string Pos.marked;
        (** Description taken from the variable declaration *)
    attrs : int Pos.marked StrMap.t;
        (** Variable's attributes and their values *)
    cat : CatVar.t;  (** Category *)
    is_given_back : bool;  (** Is the variable 'restituee'? *)
    typ : value_typ option;  (** Optional variable type *)
  }
  (** Exhaustive data on a TGV variable. *)

  (** Where can the variable be found? *)
  and scope =
    | Tgv of tgv  (** This variable belongs to the TGV. *)
    | Temp of t Array.t option
        (** This variable is temporary, maybe an array. *)
    | Ref  (** This references another variable. *)

  and t = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    id : id;
        (** A unique identifier, enforced by [Var.new_{tgv,temp,ref,arg,res}] *)
    loc : loc;
    scope : scope;  (** Data on the variable. *)
  }

  (** {2 Getters and setters} *)

  val tgv : t -> tgv
  (** Returns the tgv data of a variable; fails if it is not a TGV variable. *)

  val name : t -> string Pos.marked
  (** Returns a markzs variable name. *)

  val name_str : t -> string
  (** Same as [name] without the mark. *)

  val get_table : t -> t Array.t option
  (** Returns the table represented by the variable, if relevant.
      Returns [None] on references. *)

  val is_table : t -> bool
  (** Returns true if the variable represents a table. *)

  val set_table : t -> t Array.t option -> t
  (** Sets a table to the given variable. *)

  val cat_var_loc : t -> CatVar.loc
  (** Returns the category of a TGV variable; fails if it is not a TGV variable.  *)

  val size : t -> int
  (** Returns the size of a variable: the size of the array if it is a table; 1
      otherwise. *)

  val alias : t -> string Pos.marked option
  (** Returns the variable's alias if any, otherwise returns None.
      Aliases only are set for input variables. *)

  val alias_str : t -> string
  (** Same as [alias] without the mark, or "" if no alias was given. *)

  val descr : t -> string Pos.marked
  (** Returns the description of the variable; fails if it is not a TGV variable. *)

  val descr_str : t -> string
  (** Same as [descr] without the mark. *)

  val attrs : t -> int Pos.marked StrMap.t
  (** Returns the attributes of a TGV variable; fails if it is not a TGV variable. *)

  val cat : t -> CatVar.t
  (** Returns the category of a TGV variable; fails if it is not a TGV variable. *)

  val typ : t -> value_typ option
  (** Returns the type of a TGV variable; fails if it is not a TGV variable. *)

  val is_given_back : t -> bool
  (** Returns [true] if TGV variable in argument is 'restituee'; fails if it is
      not a TGV variable. *)

  val loc_tgv : t -> loc_tgv
  (** Returns the loc data of a TGV variable; fails if it not a TGV variable. *)

  val loc_cat_idx : t -> int

  val set_loc_tgv_idx : t -> CatVar.data -> int -> t

  val set_loc_tmp_idx : t -> int -> t

  val loc_idx : t -> int

  val set_loc_idx : t -> int -> t

  val loc_tab_idx : t -> int

  val set_loc_tab_idx : t -> int -> t

  val is_tgv : t -> bool
  (** Returns true if it is a TGV variable. *)

  val is_temp : t -> bool
  (** Returns true if it is a temporary variable. *)

  val is_ref : t -> bool
  (** Returns true if it is a reference. *)

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
  (** Creates a new tgv variable with a unique id. *)

  val new_temp : name:string Pos.marked -> table:t Array.t option -> t
  (** Creates a new temporary variable with a unique id. *)

  val new_ref : name:string Pos.marked -> t
  (** Creates a new reference with a unique id. *)

  val new_arg : name:string Pos.marked -> t
  (** Creates a new temporary variable for a function's argument. *)

  val new_res : name:string Pos.marked -> t
  (** Creates a new temporary variable for a function's result. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer. *)

  val compare : t -> t -> int

  module Set : SetExt.T with type elt = t

  module Map : sig
    include MapExt.T with type key = t
  end

  (* val compare_name_ref : (string -> string -> int) ref

     val compare_name : string -> string -> int*)
end

type event_field = {
  name : string Pos.marked;  (** The field name *)
  index : int;  (** Position of the event field. Set in the validator. *)
  is_var : bool;  (** Is a reference to a variable or a value? *)
}
(** Data on an event field. *)

type ('n, 'v) event_value =
  | Numeric of 'n
  | RefVar of 'v  (** The values of an event. *)

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

(** {2 Values and expressions} *)

(* Careful, the link may be unstable! *)
(** For a complete documentation on values and simple expressions semantics,
    check {{:../../../../../syntax.html#valeurs}the full documentation}. *)

(** A literal can either be a float value or undefined. *)
type literal = Float of float | Undefined

(** A case for switches (aiguillages). *)
type case = Default | Value of literal

(** Unary operators *)
type unop = Not | Minus

(** Binary operators. *)
type binop = And | Or | Add | Sub | Mul | Div | Mod

(** Comparison operators.
    Comparison always return 0 or 1 when the compared values are both
    defined; otherwise, it returns 'undefined'.

    Note that this is even true when comparing undefined with undefined:
    the proper way to test if a value is undefined is through the function
    'PresentFunc' (present). *)
type comp_op = Gt | Gte | Lt | Lte | Eq | Neq

(** Functions callable in M.
    There is only a subset of the whole M functions; some are translated
    into dedicated expressions at parsing (champ_evenement for example
    is directly transformed into an {!access} to a variable). *)
type func =
  | SumFunc  (** Sums the arguments *)
  | AbsFunc  (** Absolute value *)
  | MinFunc  (** Minimum of a list of values *)
  | MaxFunc  (** Maximum of a list of values *)
  | GtzFunc  (** Greater than zero (strict) ? *)
  | GtezFunc  (** Greater or equal than zero ? *)
  | NullFunc  (** Equal to zero *)
  | ArrFunc  (** Round to nearest integer *)
  | InfFunc  (** Truncate to integer *)
  | PresentFunc  (** Different than undefined *)
  | Multimax  (** Max of a subset of a table *)
  | Supzero  (** Identity if > 0, undefined otherwise *)
  | VerifNumber  (** -- unsupported -- *)
  | ComplNumber  (** -- unsupported -- *)
  | NbEvents  (** Total number of events *)
  | Func of string  (** A M function *)

(** The M language has an extremely odd way to specify looping. Rather than
    having first-class local mutable variables whose value change at each loop
    iteration, the M language prefers to use the changing loop parameter to
    instantiate the variable names inside the loop. For instance,

    {v somme(i=1..9:Xi) v}

    should evaluate to the sum of variables [X1], [X2], etc. Parameters can be
    number or characters and there can be multiple of them. We have to store all
    this information. *)

type var_name_generic = { base : string; parameters : char list }
(** For generic variables, we record the list of their lowercase parameters *)

(** A variable is either generic (with loop parameters) or normal *)
type var_name = Normal of string | Generic of var_name_generic

type m_var_name = var_name Pos.marked

type var_space = (m_var_name * int) option
(** The prefix of a variable that defines its space.
    No space is equivalent to the default space. *)

(** A generic representation of an access to a variable, read or write. *)
type 'v access =
  | VarAccess of var_space * 'v  (** Simple variable occurence *)
  | TabAccess of var_space * 'v * 'v m_expression
      (** Access to a cell of a table *)
  | FieldAccess of var_space * 'v m_expression * string Pos.marked * int
      (** Call to 'champ_evenement' *)

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

(** A set of values. *)
and 'v set_value =
  | FloatValue of float Pos.marked  (** A literal singleton *)
  | VarValue of 'v m_access  (** A variable singleton *)
  | IntervalValue of int Pos.marked * int Pos.marked  (** A literal interval *)

(** Basic M expressions. *)
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
  | Type of 'v m_access * value_typ Pos.marked
  | SameVariable of 'v m_access * 'v m_access
  | InDomain of 'v m_access * Pos.t CatVar.Map.t
  | NbAnomalies
  | NbDiscordances
  | NbInformatives
  | NbBloquantes

and 'v m_expression = 'v expression Pos.marked

(** Handling of errors. *)
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

(** {2 Printing in M} *)

(** The print function is treated as a dedicated instruction. *)

(** Where to print *)
type print_std = StdOut | StdErr

(** How to print a variable *)
type print_info = Name | Alias

(** Something to print *)
type 'v print_arg =
  | PrintString of string
  | PrintAccess of print_info * 'v m_access
  | PrintIndent of 'v m_expression
      (** Evaluates the expression and indents as much *)
  | PrintExpr of 'v m_expression * int * int
      (** Prints an expression with a minimal and maximal precision. *)

(** {2 Loops} *)

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

(** {2 Stopping} *)

type stop_kind =
  | SKApplication  (** Leave the whole application *)
  | SKTarget  (** Leave the current target *)
  | SKFun  (** Leave the current function *)
  | SKId of string option
      (** Leave the iterator with the selected var
    (or the current if [None]) *)

(** {2 Instructions} *)

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
      * (Pos.t CatVar.Map.t * 'v m_expression * var_space) list
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
  | Switch of ('v m_expression * (case list * ('v, 'e) m_instruction list) list)
  | RaiseError of 'e Pos.marked * string Pos.marked option
  | CleanErrors
  | CleanFinalizedErrors
  | ExportErrors
  | FinalizeErrors
  | Stop of stop_kind

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
(** A target is a list of instructions. They are very similar to rules,
    except targets are entrypoints of the M program. *)
(* TODO: target doc *)

(** {2 Utils} *)
(* TODO: doc *)

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

(** {2 Pretty printing functions} *)

val format_value_typ : Pp.t -> value_typ -> unit

val format_literal : Pp.t -> literal -> unit

val format_case : Pp.t -> case -> unit

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
