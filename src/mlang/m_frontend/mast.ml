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

type chaining = string
(** "enchaineur" in the M source code, utility unknown *)

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

let get_normal_var = function Normal name -> name | Generic _ -> assert false

type table_size = LiteralSize of int | SymbolSize of string

let get_table_size = function
  | LiteralSize i -> i
  | SymbolSize _ -> assert false

let get_table_size_opt = function
  | Some (LiteralSize i, pos) -> Some (i, pos)
  | None -> None
  | Some (SymbolSize _, _) -> assert false

(**{2 Expressions}*)

type var_category_id = string Pos.marked list Pos.marked

type set_value = variable Com.set_value

type expression = variable Com.expression

type m_expression = expression Pos.marked

(**{1 Toplevel clauses}*)

(**{2 Rules}*)

(** The rule is the main feature of the M language. It defines the expression of
    one or several variables. *)

type instruction = (variable, error_name) Com.instruction

type m_instruction = instruction Pos.marked

type rule = {
  rule_number : int Pos.marked;
  rule_tag_names : string Pos.marked list Pos.marked;
  rule_apps : application Pos.marked StrMap.t;
  rule_chainings : chaining Pos.marked StrMap.t;
  rule_tmp_vars : (string Pos.marked * table_size Pos.marked option) StrMap.t;
  rule_formulaes : instruction Pos.marked list;
      (** A rule can contain many variable definitions *)
}

type target = {
  target_name : string Pos.marked;
  target_file : string option;
  target_apps : application Pos.marked StrMap.t;
  target_args : string Pos.marked list;
  target_result : string Pos.marked option;
  target_tmp_vars : (string Pos.marked * table_size Pos.marked option) StrMap.t;
  target_nb_tmps : int;
  target_sz_tmps : int;
  target_nb_refs : int;
  target_prog : instruction Pos.marked list;
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

type variable_attribute = string Pos.marked * int Pos.marked

type input_variable = {
  input_name : variable_name Pos.marked;
  input_category : string Pos.marked list;
  input_attributes : variable_attribute list;
  input_alias : variable_name Pos.marked;  (** Unused for now *)
  input_is_givenback : bool;
  input_description : string Pos.marked;
  input_typ : Com.value_typ Pos.marked option;
}

type computed_variable = {
  comp_name : variable_name Pos.marked;
  comp_table : table_size Pos.marked option;
      (** size of the table, [None] for non-table variables *)
  comp_attributes : variable_attribute list;
  comp_category : string Pos.marked list;
  comp_typ : Com.value_typ Pos.marked option;
  comp_is_givenback : bool;
  comp_description : string Pos.marked;
}

type variable_decl =
  | ComputedVar of computed_variable Pos.marked
  | ConstVar of variable_name Pos.marked * variable Com.atom Pos.marked
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
  | VariableDecl of variable_decl
  | Function of target
  | Rule of rule
  | Target of target
  | Verification of verification
  | Error of error_  (** Declares an error *)
  | Output of variable_name Pos.marked  (** Declares an output variable *)
  | Func  (** Declares a function, unused *)
  | VarCatDecl of var_category_decl Pos.marked
  | RuleDomDecl of rule_domain_decl
  | VerifDomDecl of verif_domain_decl

(* TODO: parse something here *)

type source_file = source_file_item Pos.marked list

type program = source_file list

(** {1 Helper functions} *)

let get_variable_name (v : variable) : string =
  match v with Normal s -> s | Generic s -> s.base
