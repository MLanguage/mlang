type application = string

type chaining = string

type formula = unit

type rule = {
  name: string list;
  application: application list;
  chaining: chaining option;
  formulas: formula list;
}

type source_file_item =
  | Application of unit
  | Chaining of unit
  | Variable of unit
  | Rule of unit
  | Verification of unit
  | Error of unit
  | Output of unit

type source_file = source_file_item list
