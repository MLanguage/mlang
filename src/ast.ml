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

type lvalue = {
  var: variable;
  index: variable option
}
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
    index: table_index option;
    formula: expression;
  }
[@@deriving show]

type formula =
  | SingleFormula of formula_decl
  | MultipleFormulaes of loop_variables * formula_decl
[@@deriving show]

type rule = {
  name: rule_name;
  applications: application list;
  chaining: chaining option;
  formulaes: formula list;
}
[@@deriving show]

type source_file_item =
  | Application of unit
  | Chaining of unit
  | Variable of unit
  | Rule of rule
  | Verification of unit
  | Error of unit
  | Output of unit
[@@deriving show]

type source_file = source_file_item list
[@@deriving show]
