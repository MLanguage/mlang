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
(** Applications are rule annotations. The 3 main DGFiP applications seem to be:

    - [batch]: deprecated, used to compute the income tax but not anymore;
    - [bareme]: seems to compute the income tax;
    - [iliad]: usage unkown, much bigger than [bareme]. *)

module DomainId = StrSet
module DomainIdSet = StrSetSet
module DomainIdMap = StrSetMap

type chaining = string
(** "enchaineur" in the M source code, utility unknown *)

module ChainingSet = StrSet
module ChainingMap = StrMap

type variable_name = string
(** Variables are just strings *)

type func_name = string
(** Func names are just string for the moment *)

type variable_generic_name = { base : string; parameters : char list }
(** For generic variables, we record the list of their lowercase parameters *)

type error_name = string
(** Ununsed for now *)

(**{2 Literals}*)

(** A variable is either generic (with loop parameters) or normal *)
type variable = Normal of variable_name | Generic of variable_generic_name

type literal = Variable of variable | Float of float

(** A table index is used in expressions like [TABLE\[X\]], and can be
    variables, integer or the special [X] variable that stands for a "generic"
    index (to define table values as a function of the index). [X] is contained
    here in [SymbolIndex] because there can also be a variable named ["X"]... *)
type table_index = LiteralIndex of int | SymbolIndex of variable

type set_value =
  | FloatValue of float Pos.marked
  | VarValue of variable Pos.marked
  | Interval of int Pos.marked * int Pos.marked

(**{2 Loops}*)

(** The M language has an extremely odd way to specify looping. Rather than
    having first-class local mutable variables whose value change at each loop
    iteration, the M language prefers to use the changing loop parameter to
    instantiate the variable names inside the loop. For instance,

    {v somme(i=1..10:Xi) v}

    should evaluate to the sum of variables [X1], [X2], etc. Parameters can be
    number or characters and there can be multiple of them. We have to store all
    this information. *)

(** Values that can be substituted for loop parameters *)
type set_value_loop =
  | Single of literal Pos.marked
  | Range of literal Pos.marked * literal Pos.marked
  | Interval of literal Pos.marked * literal Pos.marked

type loop_variable = char Pos.marked * set_value_loop list
(** A loop variable is the character that should be substituted in variable
    names inside the loop plus the set of value to substitute. *)

(** There are two kind of loop variables declaration, but they are semantically
    the same though they have different concrete syntax. *)
type loop_variables =
  | ValueSets of loop_variable list
  | Ranges of loop_variable list

(**{2 Expressions}*)

(** Comparison operators *)
type comp_op = Gt | Gte | Lt | Lte | Eq | Neq

(** Binary operators *)
type binop = And | Or | Add | Sub | Mul | Div

let precedence = function
  | Add -> 2
  | Sub -> 2
  | Mul -> 1
  | Div -> 1
  | And -> 3
  | Or -> 4

let has_priority op op' = precedence op' < precedence op

let is_right_associative = function _ -> false

let is_left_associative = function Sub | Div -> true | _ -> false

(** Unary operators *)
type unop = Not | Minus

(** The main type of the M language *)
type expression =
  | TestInSet of bool * expression Pos.marked * set_value list
      (** Test if an expression is in a set of value (or not in the set if the
          flag is set to [false]) *)
  | Comparison of
      comp_op Pos.marked * expression Pos.marked * expression Pos.marked
      (** Compares two expressions and produce a boolean *)
  | Binop of binop Pos.marked * expression Pos.marked * expression Pos.marked
  | Unop of unop * expression Pos.marked
  | Index of variable Pos.marked * table_index Pos.marked
      (** Access a cell in a table *)
  | Conditional of
      expression Pos.marked
      * expression Pos.marked
      * expression Pos.marked option
      (** Classic conditional with an optional else clause ([None] only for
          verification conditions) *)
  | FunctionCall of func_name Pos.marked * func_args
  | Literal of literal
  | Loop of loop_variables Pos.marked * expression Pos.marked
      (** The loop is prefixed with the loop variables declarations *)

(** Functions can take a explicit list of argument or a loop expression that
    expands into a list *)
and func_args =
  | ArgList of expression Pos.marked list
  | LoopList of loop_variables Pos.marked * expression Pos.marked

(**{1 Toplevel clauses}*)

(**{2 Rules}*)

(** The rule is the main feature of the M language. It defines the expression of
    one or several variables. *)

type lvalue = {
  var : variable Pos.marked;
  index : table_index Pos.marked option; (* [None] if not a table *)
}
(** An lvalue (left value) is a variable being assigned. It can be a table or a
    non-table variable *)

type formula_decl = {
  lvalue : lvalue Pos.marked;
  formula : expression Pos.marked;
}

(** In the M language, you can define multiple variables at once. This is the
    way they do looping since the definition can depend on the loop variable
    value (e.g [Xi] can depend on [i]). *)
type formula =
  | SingleFormula of formula_decl
  | MultipleFormulaes of loop_variables Pos.marked * formula_decl

type rule = {
  rule_number : int Pos.marked;
  rule_tag_names : string Pos.marked list Pos.marked;
  rule_applications : application Pos.marked list;
  rule_chaining : chaining Pos.marked option;
  rule_formulaes : formula Pos.marked list;
      (** A rule can contain many variable definitions *)
}

type 'a domain_decl = {
  dom_names : string Pos.marked list Pos.marked list;
  dom_parents : string Pos.marked list Pos.marked list;
  dom_by_default : bool;
  dom_data : 'a;
}

type rule_domain_data = { rdom_computable : bool }

type rule_domain_decl = rule_domain_data domain_decl

(**{2 Variable declaration}*)

(** The M language has prototypes for declaring variables with types and various
    attributes. There are three kind of variables: input variables, computed
    variables and constant variables.

    Variable declaration is not application-specific, which is not coherent. *)

(**{3 Input variables}*)

type variable_attribute = string Pos.marked * literal Pos.marked

(** Here are all the types a value can have. Date types don't seem to be used at
    all though. *)
type value_typ =
  | Boolean
  | DateYear
  | DateDayMonthYear
  | DateMonth
  | Integer
  | Real

type input_variable = {
  input_name : variable_name Pos.marked;
  input_category : string Pos.marked list;
  input_attributes : variable_attribute list;
  input_alias : variable_name Pos.marked;  (** Unused for now *)
  input_is_givenback : bool;
  input_description : string Pos.marked;
  input_typ : value_typ Pos.marked option;
}

type computed_variable = {
  comp_name : variable_name Pos.marked;
  comp_table : int Pos.marked option;
      (** size of the table, [None] for non-table variables *)
  comp_attributes : variable_attribute list;
  comp_category : string Pos.marked list;
  comp_typ : value_typ Pos.marked option;
  comp_is_givenback : bool;
  comp_description : string Pos.marked;
}

type variable_decl =
  | ComputedVar of computed_variable Pos.marked
  | ConstVar of variable_name Pos.marked * literal Pos.marked
      (** The literal is the constant value *)
  | InputVar of input_variable Pos.marked

type var_type = Input | Computed

type var_category_decl = {
  var_type : var_type;
  var_category : string Pos.marked list;
  var_attributes : string Pos.marked list;
}

(* standard categories *)
let input_category = "saisie"

let computed_category = "calculee"

let base_category = "base"

let givenback_category = "restituee"

(**{2 Verification clauses}*)

(** These clauses are expression refering to the variables of the program. They
    seem to be dynamically checked and trigger errors when false. *)

type verification_condition = {
  verif_cond_expr : expression Pos.marked;
  verif_cond_error : error_name Pos.marked * variable_name Pos.marked option;
      (** A verification condition error can ba associated to a variable *)
}

type verification = {
  verif_number : int Pos.marked;
  verif_tag_names : string Pos.marked list Pos.marked;
  verif_applications : application Pos.marked list;
      (** Verification conditions are application-specific *)
  verif_conditions : verification_condition Pos.marked list;
}

type verif_auth_decl = string Pos.marked list Pos.marked

type verif_domain_data = { vdom_auth : verif_auth_decl list }

type verif_domain_decl = verif_domain_data domain_decl

type error_typ = Anomaly | Discordance | Information

let compare_error_type e1 e2 =
  match (e1, e2) with
  | Anomaly, (Discordance | Information) -> -1
  | (Discordance | Information), Anomaly -> 1
  | Information, Discordance -> -1
  | Discordance, Information -> 1
  | _ -> 0

type error_ = {
  error_name : error_name Pos.marked;
  error_typ : error_typ Pos.marked;
  error_descr : string Pos.marked list;
}

(**{1 M programs}*)

type source_file_item =
  | Application of application Pos.marked  (** Declares an application *)
  | Chaining of chaining * application Pos.marked list
      (** Unused, declares an "enchaineur" *)
  | VariableDecl of variable_decl
  | Rule of rule
  | Verification of verification
  | Error of error_  (** Declares an error *)
  | Output of variable_name Pos.marked  (** Declares an output variable *)
  | Function  (** Declares a function, unused *)
  | VarCatDecl of var_category_decl Pos.marked
  | RuleDomDecl of rule_domain_decl
  | VerifDomDecl of verif_domain_decl

(* TODO: parse something here *)

type source_file = source_file_item Pos.marked list

type program = source_file list

(**{1 Function specification AST}*)

type function_spec = {
  spec_inputs : variable_name Pos.marked list;
  spec_consts : (variable_name Pos.marked * expression Pos.marked) list;
  spec_outputs : variable_name Pos.marked list;
  spec_conditions : expression Pos.marked list;
}

(** {1 Helper functions} *)

let get_variable_name (v : variable) : string =
  match v with Normal s -> s | Generic s -> s.base
