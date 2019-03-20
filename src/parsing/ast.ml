type position = {
  pos_filename: string;
  pos_loc: (Lexing.position * Lexing.position)
}

let pp_position f (pos:position) : unit =
  let (s, e) = pos.pos_loc in
  Format.fprintf f "%s %d:%d--%d:%d"
    pos.pos_filename
    s.Lexing.pos_lnum (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
    e.Lexing.pos_lnum (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)

type 'a marked = ('a * position)
[@@deriving show]

let unmark ((x, _) : 'a marked) : 'a = x

type application = string
[@@deriving show]

type chaining = string
[@@deriving show]

type rule_name = string list
[@@deriving show]

type variable_name = string
[@@deriving show]

type func_name =
  | Unknown of string
[@@deriving show]

type variable_generic_name = {
  base: string;
  parameters: char list
}
[@@deriving show]

type variable =
  | Normal of variable_name
  | Generic of variable_generic_name
[@@deriving show]

type verification_name = string list
[@@deriving show]

type error_name = string
[@@deriving show]

type literal =
  | Variable of variable
  | Int of int
  | Float of float
[@@deriving show]

type table_index =
  | LiteralIndex of int
  | GenericIndex
  | SymbolIndex of variable
[@@deriving show]


type lvalue = {
  var: variable;
  index: table_index option
}
[@@deriving show]


type set_value =
  | VarValue of variable
  | Interval of int * int
[@@deriving show]

type comp_op =
  | Gt
  | Gte
  | Lt
  | Lte
  | Eq
  | Neq
[@@deriving show]

type binop =
  | And
  | Or
  | Add
  | Sub
  | Mul
  | Div
[@@deriving show]

type unop =
  | Not
  | Minus
[@@deriving show]

type loop_variable = variable * set_value list
[@@deriving show]

type loop_variables =
  | ValueSets of loop_variable list
  | Ranges of loop_variable list
[@@deriving show]

type expression =
  | TestInSet of bool * expression * set_value list
  | Comparison of comp_op * expression * expression
  | Binop of binop * expression * expression
  | Unop of unop * expression
  | Index of variable * table_index
  | Conditional of expression * expression * expression option
  | FunctionCall of func_name * func_args
  | Literal of literal
  | Loop of loop_variables * expression
[@@deriving show]

and func_args =
    | ArgList of expression list
  | LoopList of unit
[@@deriving show]

type formula_decl =
  {
    lvalue: lvalue;
    formula: expression;
  }
[@@deriving show]

type formula =
  | SingleFormula of formula_decl
  | MultipleFormulaes of loop_variables * formula_decl
[@@deriving show]

type rule = {
  rule_name: rule_name;
  rule_applications: application marked list;
  rule_chaining: chaining marked option;
  rule_formulaes: formula list;
}
[@@deriving show]

type computed_typ =
  | Base
  | GivenBack
[@@deriving show]

type input_variable_subtype =
  | Context
  | Family
  | Penality
  | Income
[@@deriving show]

type input_variable_attribute = string
[@@deriving show]

type value_typ =
  | Boolean
  | DateYear
  | DateDayMonthYear
  | DateMonth
  | Integer
  | Real
[@@deriving show]

type input_variable = {
  input_name: variable_name;
  input_subtyp: input_variable_subtype;
  input_attributes: (input_variable_attribute * literal) list;
  input_given_back: bool;
  input_alias: variable_name;
  input_description: string;
  input_typ: value_typ option;
}
[@@deriving show]

type computed_variable = {
  comp_name: variable;
  comp_table: int option; (* size of the table *)
  comp_subtyp: computed_typ list;
  comp_typ: value_typ option;
  comp_description: string;
}
[@@deriving show]

type variable_decl =
  | ComputedVar of computed_variable
  | ConstVar of variable_name * literal
  | InputVar of input_variable
[@@deriving show]

type verification_condition = {
  verif_cond_expr: expression;
  verif_cond_errors: error_name list
}
[@@deriving show]

type verification = {
  verif_name: verification_name;
  verif_applications: application marked list;
  verif_conditions: verification_condition list;
}
[@@deriving show]

type error_typ =
  | Anomaly
  | Discordance
  | Information
[@@deriving show]

type error_ = {
  error_name: error_name;
  error_typ: error_typ;
  error_descr: string list;
}
[@@deriving show]

type source_file_item =
  | Application of application marked
  | Chaining of chaining * application marked list
  | Variable of variable_decl
  | Rule of rule
  | Verification of verification
  | Error of error_
  | Output of variable_name
[@@deriving show]

type source_file = source_file_item marked list
[@@deriving show]
