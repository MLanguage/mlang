(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by the DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-C license and that you accept its terms.
*)

(** Abstract Syntax Tree for M *)

(** {1 Source code position} *)

(** A position in the source code is a file, as well as begin and end location of the form col:line *)
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

(** Everything related to the source code should keep its position stored, to improve error messages *)
type 'a marked = ('a * position)
[@@deriving show]

(** Placeholder position *)
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

(** {1 Abstract Syntax Tree } *)


(**
   This AST is very close to the concrete syntax. It features many elements that are
   just dropped in later phases of the compiler, but may be used by other DGFiP applications
*)

(**{2 Names }*)

(** Applications are rule annotations. The 3 main DGFiP applications seem to be:
    {ul
    {li [batch]: deprecated, used to compute the income tax but not anymore; }
    {li [bareme]: seems to compute the income tax; }
    {li [iliad]: usage unkown, much bigger than [bareme]. }
    }
*)
type application = string
[@@deriving show]

(** "enchaineur" in the M source code, utility unknown *)
type chaining = string
[@@deriving show]

(** Rule can have multiple names. Currently unused in the compiler *)
type rule_name = string marked list
[@@deriving show]

(** Variables are just strings *)
type variable_name = string
[@@deriving show]

(** Func names are just string for the moment *)
type func_name = string
[@@deriving show]

(** For generic variables, we record the list of their lowercase parameters *)
type variable_generic_name = {
  base: string;
  parameters: char list
}
[@@deriving show]

(** Verification clauses can have multiple names, currently unused *)
type verification_name = string marked list
[@@deriving show]

(** Ununsed for now *)
type error_name = string
[@@deriving show]

(**{2 Literals }*)

(** A variable is either generic (with loop parameters) or normal *)
type variable =
  | Normal of variable_name
  | Generic of variable_generic_name
[@@deriving show]

type literal =
  | Variable of variable
  | Int of int
  | Float of float
[@@deriving show]

(**
   A table index is used in expressions like [TABLE[X]], and can be variables, integer or the special
   [X] variable that stands for a "generic" index (to define table values as a function of the index).
*)
type table_index =
  | LiteralIndex of int
  | GenericIndex
  | SymbolIndex of variable
[@@deriving show]

type set_value =
  | IntValue of int marked
  | VarValue of variable marked
  | Interval of int marked * int marked
[@@deriving show]

(**{2 Loops }*)

(**
   The M language has an extremely odd way to specify looping. Rather than having first-class
   local mutable variables whose value change at each loop iteration, the M language prefers to
    use the changing loop parameter to instantiate the variable names inside the loop. For instance,
   {v
somme(i=1..10:Xi)
    v}
   should evaluate to the sum of variables [X1], [X2], etc. Parameters can be number or characters and
   there can be multiple of them. We have to store all this information.
*)

(** Values that can be substituted for loop parameters *)
type set_value_loop =
  | VarParam of variable_name marked
  | IntervalLoop of literal marked * literal marked
[@@deriving show]

(**
   A loop variable is the character that should be substituted in variable names inside the loop
   plus the set of value to substitute.
*)
type loop_variable = char marked * set_value_loop list
[@@deriving show]

(**
   There are two kind of loop variables declaration, but they are semantically the same though
   they have different concrete syntax.
*)
type loop_variables =
  | ValueSets of loop_variable list
  | Ranges of loop_variable list
[@@deriving show]

(**{2 Expressions}*)

(** Comparison operators *)
type comp_op =
  | Gt
  | Gte
  | Lt
  | Lte
  | Eq
  | Neq
[@@deriving show]

(** Binary operators *)
type binop =
  | And
  | Or
  | Add
  | Sub
  | Mul
  | Div
[@@deriving show]

(** Unary operators *)
type unop =
  | Not
  | Minus
[@@deriving show]

(** The main type of the M language *)
type expression =
  | TestInSet of bool * expression marked * set_value list
  (**
     Test if an expression is in a set of value (or not in the set if the flag is set to [false])
  *)
  | Comparison of comp_op marked * expression marked * expression marked
  (**
     Compares two expressions and produce a boolean
  *)
  | Binop of binop marked * expression marked * expression marked
  | Unop of unop  * expression marked
  | Index of variable marked * table_index marked
  (**
     Access a cell in a table
  *)
  | Conditional of
      expression marked * expression marked * expression marked option
  (** Classic conditional with an optional else clause ([None] only for verification conditions) *)
  | FunctionCall of func_name marked * func_args
  | Literal of literal
  | Loop of loop_variables marked * expression marked
  (** The loop is prefixed with the loop variables declarations *)
[@@deriving show]

(** Functions can take a explicit list of argument or a loop expression that expands into a list *)
and func_args =
  | ArgList of expression marked list
  | LoopList of loop_variables marked * expression marked
[@@deriving show]

(**{1 Toplevel clauses }*)

(**{2 Rules }*)

(**
   The rule is the main feature of the M language. It defines the expression of one or several
   variables.
*)

(** An lvalue (left value) is a variable being assigned. It can be a table or a non-table variable *)
type lvalue = {
  var: variable marked;
  index: table_index marked option (* [None] if not a table *)
}
[@@deriving show]

type formula_decl =
  {
    lvalue: lvalue marked;
    formula: expression marked;
  }
[@@deriving show]

(**
   In the M language, you can define multiple variables at once. This is the way they do looping
   since the definition can depend on the loop variable value (e.g [Xi] can depend on [i]).
*)
type formula =
  | SingleFormula of formula_decl
  | MultipleFormulaes of loop_variables marked * formula_decl
[@@deriving show]

type rule = {
  rule_name: rule_name;
  rule_applications: application marked list;
  rule_chaining: chaining marked option;
  rule_formulaes: formula marked list; (** A rule can contain many variable definitions *)
}
[@@deriving show]

(**{2 Variable declaration }*)

(**
   The M language has prototypes for declaring variables with types and various attributes. There are
   three kind of variables: input variables, computed variables and constant variables.

   Variable declaration is not application-specific, which is not coherent.
*)

(**{3 Input variables }*)

(**
   Unused for now, except for typechecking: [Income] should be a real number corresponding to an amount of money
*)
type input_variable_subtype =
  | Context
  | Family
  | Penality
  | Income
[@@deriving show]

(** Attributes are unused for now *)
type input_variable_attribute = string
[@@deriving show]

(** Here are all the types a value can have. Date types don't seem to be used at all though. *)
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
  input_given_back: bool; (** An input variable given back ("restituee") means that it's also an output *)
  input_alias: variable_name marked; (** Unused for now *)
  input_description: string marked;
  input_typ: value_typ marked option;
}
[@@deriving show]

(** A [GivenBack] variable is an output of the program *)
type computed_typ =
  | Base
  | GivenBack
[@@deriving show]

type computed_variable = {
  comp_name: variable_name marked;
  comp_table: int marked option; (** size of the table, [None] for non-table variables *)
  comp_subtyp: computed_typ marked list;
  comp_typ: value_typ marked option;
  comp_description: string marked;
}
[@@deriving show]


type variable_decl =
  | ComputedVar of computed_variable marked
  | ConstVar of variable_name marked * literal marked (** The literal is the constant value *)
  | InputVar of input_variable marked
[@@deriving show]

(**{2 Verification clauses }*)

(**
   These clauses are expression refering to the variables of the program. They seem to be dynamically
   checked and trigger errors when false.
*)

type verification_condition = {
  verif_cond_expr: expression marked;
  verif_cond_errors: error_name marked list (** A verification condition can trigger multiple errors *)
}
[@@deriving show]

type verification = {
  verif_name: verification_name;
  verif_applications: application marked list; (** Verification conditions are application-specific *)
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

(**{1 M programs }*)

type source_file_item =
  | Application of application marked (** Declares an application *)
  | Chaining of chaining * application marked list (** Unused, declares an "enchaineur" *)
  | VariableDecl of variable_decl
  | Rule of rule
  | Verification of verification
  | Error of error_ (** Declares an error *)
  | Output of variable_name marked (** Declares an output variable *)
  | Function (** Declares a function, unused *) (* TODO: parse something here *)
[@@deriving show]

type source_file = source_file_item marked list
[@@deriving show]

type program = source_file list
[@@deriving show]

(**{1 Function specification AST }*)

type function_spec = {
  spec_inputs: variable_name list;
  spec_consts: (variable_name * expression marked) list;
  spec_outputs: variable_name list;
}


(** {1 Helper functions } *)

let get_variable_name (v: variable) : string = match v with
  | Normal s -> s
  | Generic s -> s.base

let unmark_option (x: 'a marked option) : 'a option = match x with
  | Some x -> Some (unmark x)
  | None -> None
