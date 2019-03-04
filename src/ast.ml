type application = string
[@@deriving show]

type chaining = string
[@@deriving show]

type rule_name = string list
[@@deriving show]

type variable_name = string
[@@deriving show]

type variable_generic_name = {
  base: string;
  parameters: char list
}
[@@deriving show]

type table_index =
  | LiteralIndex of int
  | GenericIndex
  | SymbolIndex of variable_name
[@@deriving show]

type expression = unit
[@@deriving show]

type formula_decl =
  {
    variable: variable_name;
    index: table_index option;
    formula: expression;
  }
[@@deriving show]

type formula =
  | SingleFormula of formula_decl
  | MultipleFormulaes of unit
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
