(** Abstract Syntax Tree for M *)

(** {2 Source code position} *)

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

let no_pos : position =
  let zero_pos =
    { Lexing.pos_fname = ""; Lexing.pos_lnum = 0; Lexing.pos_cnum = 0; Lexing.pos_bol = 0 }
  in
  {
    pos_filename = "unknown position";
    pos_loc = (zero_pos, zero_pos)
  }

let unmark ((x, _) : 'a marked) : 'a = x

let get_position ((_,x) : 'a marked) : position = x

let map_under_mark (f: 'a -> 'b) ((x, y) :'a marked) : 'b marked =
  (f x, y)

let same_pos_as (x: 'a) ((_, y) : 'b marked) : 'a marked =
  (x,y)

(** {2 Abstract Syntax Tree } *)

type application = string
[@@deriving show]

type chaining = string
[@@deriving show]

type rule_name = string marked list
[@@deriving show]

type variable_name = string
[@@deriving show]

type func_name = string
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

type verification_name = string marked list
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
  index: table_index marked option
}
[@@deriving show]


type set_value =
  | IntValue of int marked
  | VarValue of variable marked
  | Interval of int marked * int marked
[@@deriving show]

type set_value_loop =
  | VarParam of variable_name marked
  | IntervalLoop of int marked * int marked
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

type loop_variable = char marked * set_value_loop list
[@@deriving show]

type loop_variables =
  | ValueSets of loop_variable list
  | Ranges of loop_variable list
[@@deriving show]

type expression =
  | TestInSet of bool * expression marked * set_value list
  | Comparison of comp_op marked * expression marked * expression marked
  | Binop of binop marked * expression marked * expression marked
  | Unop of unop  * expression marked
  | Index of variable marked * table_index marked
  | Conditional of
      expression marked * expression marked * expression marked option
  | FunctionCall of func_name marked * func_args
  | Literal of literal
  | Loop of loop_variables marked * expression marked
[@@deriving show]

and func_args =
    | ArgList of expression marked list
  | LoopList of loop_variables marked * expression marked
[@@deriving show]

type formula_decl =
  {
    lvalue: lvalue marked;
    formula: expression marked;
  }
[@@deriving show]

type formula =
  | SingleFormula of formula_decl
  | MultipleFormulaes of loop_variables marked * formula_decl
[@@deriving show]

type rule = {
  rule_name: rule_name;
  rule_applications: application marked list;
  rule_chaining: chaining marked option;
  rule_formulaes: formula marked list;
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
  input_name: variable_name marked;
  input_subtyp: input_variable_subtype marked;
  input_attributes:
    (input_variable_attribute marked * literal marked) list;
  input_given_back: bool;
  input_alias: variable_name marked;
  input_description: string marked;
  input_typ: value_typ marked option;
}
[@@deriving show]

type computed_variable = {
  comp_name: variable_name marked;
  comp_table: int marked option; (* size of the table *)
  comp_subtyp: computed_typ marked list;
  comp_typ: value_typ marked option;
  comp_description: string marked;
}
[@@deriving show]

type variable_decl =
  | ComputedVar of computed_variable marked
  | ConstVar of variable_name marked * literal marked
  | InputVar of input_variable marked
[@@deriving show]

type verification_condition = {
  verif_cond_expr: expression marked;
  verif_cond_errors: error_name marked list
}
[@@deriving show]

type verification = {
  verif_name: verification_name;
  verif_applications: application marked list;
  verif_conditions: verification_condition marked list;
}
[@@deriving show]

type error_typ =
  | Anomaly
  | Discordance
  | Information
[@@deriving show]

type error_ = {
  error_name: error_name marked;
  error_typ: error_typ marked;
  error_descr: string marked list;
}
[@@deriving show]

type source_file_item =
  | Application of application marked
  | Chaining of chaining * application marked list
  | Variable of variable_decl
  | Rule of rule
  | Verification of verification
  | Error of error_
  | Output of variable_name marked
[@@deriving show]

type source_file = source_file_item marked list
[@@deriving show]

type program = source_file list
[@@deriving show]


(** {2 Helper functions } *)

let get_variable_name (v: variable) : string = match v with
  | Normal s -> s
  | Generic s -> s.base

let unmark_option (x: 'a marked option) : 'a option = match x with
  | Some x -> Some (unmark x)
  | None -> None
