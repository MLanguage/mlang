(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

(**
   This module describes the core backend language for describing tax specifications : Specifisc.
   Programs in Specifisc only deal with boolean logic and integer arithmetic modulo 2^64. The
   language is imperative, each function consisting of variable definitions and constraints that
   should hold during the program execution.

   This language is meant for formal analysis of the tax specification.
*)

module Variable (_ : sig end) = struct
  type t = {
    name: string Ast.marked;
    id: int;
    descr: string Ast.marked;
  }

  let counter : int ref = ref 0

  let fresh_id () : int=
    let v = !counter in
    counter := !counter + 1;
    v

  let new_var
      (name: string Ast.marked)
      (descr: string Ast.marked)
    : t =
    {
      name; id = fresh_id (); descr;
    }

  let compare (var1 :t) (var2 : t) =
    compare var1.id var2.id
end

module BoolVariable = Variable ()
module BoolVariableMap = Map.Make(BoolVariable)
module IntVariable = Variable ()
module IntVariableMap = Map.Make(IntVariable)
module ArithmeticFunctionVariable = Variable ()
module ArithmeticFunctionVariableMap = Map.Make(ArithmeticFunctionVariable)
module FunctionVariable = Variable ()
module FunctionVariableMap = Map.Make(FunctionVariable)

type typ =
  | Int
  | Bool

type comparison_op = Lt | Lte | Gt | Gte | Neq | Eq

type logical_binop = And | Or

type arithmetic_binop = Add | Sub | Mul | Div

type logical_expression =
  | Comparison of comparison_op Ast.marked * arithmetic_expression Ast.marked * arithmetic_expression Ast.marked
  | LogicalBinop of logical_binop Ast.marked * logical_expression Ast.marked * logical_expression Ast.marked
  | LogicalNot of logical_expression Ast.marked
  | BoolLiteral of bool
  | BoolVar of BoolVariable.t

and arithmetic_expression =
  | ArithmeticBinop of arithmetic_binop Ast.marked * arithmetic_expression Ast.marked * arithmetic_expression Ast.marked
  | ArithmeticMinus of arithmetic_expression Ast.marked
  | Conditional of logical_expression Ast.marked * arithmetic_expression Ast.marked * arithmetic_expression Ast.marked
  | IntLiteral of Int64.t
  | IntVar of IntVariable.t
  | FunctionCall of ArithmeticFunctionVariable.t Ast.marked * arithmetic_expression Ast.marked list


(**
   Specifisc programs can feature custom arithmetic functions, as long as those have a defined
   specification in Z3.
*)


type arith_args_spec =
  | SpecFixed of int
  | SpecVariable  (* At least one *)

(** The specification is of the form [fun args ret -> ...] *)
type arith_func_z3_spec = Z3.Expr.expr list -> Z3.Expr.expr -> Z3.Expr.expr

type arith_func_interpreter_spec = Int64.t list -> Int64.t

type arithmetic_func = {
  arith_func_args_typ: arith_args_spec;
  arith_func_z3_spec: arith_func_z3_spec;
  arith_func_interpreter_spec: arith_func_interpreter_spec;
}

type command =
  | BoolDef of BoolVariable.t * logical_expression Ast.marked
  | IntDef of IntVariable.t * arithmetic_expression Ast.marked
  | Constraint of logical_expression Ast.marked

type variables = IntVariable.t list * BoolVariable.t list

type func = {
  body: command list;
  inputs: variables;
  outputs: variables;
}

type idmap_var =
  | IDBoolVar of BoolVariable.t
  | IDIntVar of IntVariable.t

type idmap = idmap_var list Mvg.VarNameToID.t

type program = {
  program_functions: func FunctionVariableMap.t;
  program_arith_functions : arithmetic_func ArithmeticFunctionVariableMap.t;
  program_mult_factor: int;
  program_idmap: idmap
}
