(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

(** Abstract Syntax Tree for M *)

(** {1 Abstract Syntax Tree} *)

(** This AST is very close to the concrete syntax. It features many elements
    that are just dropped in later phases of the compiler, but may be used by
    other DGFiP applications *)

(**{2 Names}*)

type application = string
(** Applications are rule annotations. The 3 main DGFiP applications are:
    - [batch]: deprecated, used to compute the income tax but not anymore;
    - [bareme]: seems to compute the income tax;
    - [iliad]: usage unkown, much bigger than [bareme]. *)

type chaining = string
(** "enchaineur" in the M source code, utility unknown *)

type func_name = string
(** Func names are just string for the moment *)

type error_name = string
(** Ununsed for now *)

(**{2 Literals}*)

type table_size =
  | LiteralSize of int  (** The table size is an integer literal. *)
  | SymbolSize of string  (** The table size is a named constant. *)

(** Returns the literal value of a table size. This function expects to take a
    LiteralSize value. *)
let get_table_size = function
  | LiteralSize i -> i
  | SymbolSize _ -> assert false

(** Same as [get_table_size] on an optional size. *)
let get_table_size_opt = function
  | Some (Pos.Mark (LiteralSize i, pos)) -> Some (Pos.mark i pos)
  | None -> None
  | Some (Pos.Mark (SymbolSize _, _)) -> assert false

(**{2 Expressions}*)

type var_category_id = string Pos.marked list Pos.marked

type set_value = Com.m_var_name Com.set_value

type expression = Com.m_var_name Com.expression

type m_expression = expression Pos.marked

(**{1 Toplevel clauses}*)

(**{2 Rules}*)

(** The rule is the main feature of the M language. It defines the expression of
    one or several variables. *)

type instruction = (Com.m_var_name, error_name) Com.instruction

type m_instruction = instruction Pos.marked

type rule = {
  rule_number : int Pos.marked;  (** The rule's unique identifier. *)
  rule_tag_names : string Pos.marked list Pos.marked;
      (** The rule's domains. *)
  rule_apps : application Pos.marked StrMap.t;
      (** The applications of this rule. The key and the marked bound value seem
          to be equal. *)
  (* TODO: simplify map *)
  rule_chainings : chaining Pos.marked StrMap.t;
      (** The chainings of this rule. The key and the marked bound value seem to
          be equal. *)
  (* TODO: simplify map *)
  rule_tmp_vars : (string Pos.marked * table_size Pos.marked option) list;
      (** The temporary variables of a rule. *)
  rule_formulaes : instruction Pos.marked list;  (** The rule's instructions. *)
}
(** A rule is a list of instruction associated to one or several domains and to
    a list of applications. *)

type target = {
  target_name : string Pos.marked;
  target_file : string option;
  target_apps : string Pos.marked StrMap.t;
  target_args : string Pos.marked list;
  target_result : string Pos.marked option;
  target_tmp_vars : (string Pos.marked * table_size Pos.marked option) list;
  target_prog : m_instruction list;
}
(** A target is a meta rule that can call rules. Targets are entrypoints of the
    M program. *)

type 'a domain_decl = {
  dom_names : string Pos.marked list Pos.marked list;
  dom_parents : string Pos.marked list Pos.marked list;
  dom_by_default : bool;
  dom_data : 'a;
}
(** The declaration of a generic domain. It will either be a rule domain (with
    'a = {!rule_domain_data}) or a verification domain ('a =
    {!verif_domain_data}). *)

type rule_domain_data = { rdom_computable : bool }

type rule_domain_decl = rule_domain_data domain_decl

(**{2 Variable declaration}*)

(** The M language has prototypes for declaring variables with types and various
    attributes. There are three kind of variables: input variables, computed
    variables and constant variables.

    Variable declaration is not application-specific, which is not coherent. *)

(**{3 Input variables}*)

type variable_attribute = string Pos.marked * int Pos.marked
(** Variables are defined in families (saisie, calculee, ...) which all define
    attributes. Each variable must assign a value for each of its attributes. *)

type input_variable = {
  input_name : string Pos.marked;  (** The unique identifier of the variable. *)
  input_category : string Pos.marked list;
      (** The variable's categories (which define the variable's attributes). *)
  input_attributes : variable_attribute list;  (** The variable's attributes. *)
  input_alias : string Pos.marked;
      (** The variable can be used by its name or its alias. *)
  input_is_givenback : bool;  (** Whether it is "restituee" or not. *)
  input_description : string Pos.marked;
      (** The documentation of the variable. *)
  input_typ : Com.value_typ Pos.marked option;
      (** An optional type for the variable. By default, it will be treated as a
          real. *)
}
(** An input variable (variables from the family "saisie", which is a keyword).
*)

type computed_variable = {
  comp_name : string Pos.marked;  (** The unique identifier for the variable. *)
  comp_table : table_size Pos.marked option;
      (** Size of the table if the variable is a table, [None] for non-table
          variables *)
  comp_attributes : variable_attribute list;  (** The variable's attributes. *)
  comp_category : string Pos.marked list;
      (** The variable's categories (which define the variable's attributes). *)
  comp_typ : Com.value_typ Pos.marked option;
      (** An optional type for the variable. By default, it will be treated as a
          real, but tests on types will always result to 0. *)
  comp_is_givenback : bool;  (** Whether it is "restituee" or not. *)
  comp_description : string Pos.marked;
      (** The documentation of the variable. *)
}
(** A computed variable (variables from the family "calculee", which is a
    keyword. They basically are all the variables that are not from the "saisie"
    family. *)

(** A variable declaration. *)
type variable_decl =
  | ComputedVar of computed_variable Pos.marked
  | ConstVar of string Pos.marked * Com.m_var_name Com.atom Pos.marked
      (** The literal is the constant value *)
  | InputVar of input_variable Pos.marked

type var_type = Input | Computed

type var_category_decl = {
  var_type : var_type;  (** Are variables inputs or computed? *)
  var_category : string Pos.marked list;  (** The subcategories. *)
  var_attributes : string Pos.marked list;
      (** The attributes for variables of this category. *)
}
(** A variable category (and subcategory) declaration. *)

(** {3 Standard categories} *)

(** The category for {!Input} variables. *)
let input_category = "saisie"

(** The category for {!Computed} variables. *)
let computed_category = "calculee"

let base_category = "base"

(** The category for given back variables. *)
let givenback_category = "restituee"

(**{2 Verification clauses}*)

(** These clauses are expression refering to the variables of the program. They
    are dynamically checked and trigger errors when false. *)

type verification_condition = {
  verif_cond_expr : expression Pos.marked;
  verif_cond_error : error_name Pos.marked * string Pos.marked option;
      (** A verification condition error can be associated to a variable *)
}

type verification = {
  verif_number : int Pos.marked;
  verif_tag_names : string Pos.marked list Pos.marked;
  verif_apps : application Pos.marked StrMap.t;
      (** Verification conditions are application-specific *)
  verif_conditions : verification_condition Pos.marked list;
}

type verif_domain_data = {
  vdom_auth : var_category_id list;
  vdom_verifiable : bool;
}

type verif_domain_decl = verif_domain_data domain_decl

type error_ = {
  error_name : error_name Pos.marked;
  error_typ : Com.Error.typ Pos.marked;
  error_descr : string Pos.marked list;
}

(**{1 M programs}*)

type source_file_item =
  | Application of application Pos.marked  (** Declares an application *)
  | Chaining of chaining Pos.marked * application Pos.marked list
      (** Declares a chaining with its applications *)
  | VariableDecl of variable_decl  (** Declares a variable *)
  | EventDecl of Com.event_field list  (** Declares an event *)
  | Function of target  (** Defines a function *)
  | Rule of rule  (** Defines a rule *)
  | Target of target  (** Defines a target *)
  | Verification of verification  (** Defines a verification *)
  | Error of error_  (** Declares an error *)
  | Output of string Pos.marked  (** Declares an output variable *)
  | Func  (** Declares a function, unused *)
  | VarCatDecl of var_category_decl Pos.marked
      (** Defines a variable category *)
  | RuleDomDecl of rule_domain_decl  (** Declares a rule domain *)
  | VerifDomDecl of verif_domain_decl  (** Declares a verification domain *)
  | VariableSpaceDecl of Com.variable_space  (** Declares a variable space *)

(* TODO: parse something here *)

type source_file = source_file_item Pos.marked list

type program = source_file list
