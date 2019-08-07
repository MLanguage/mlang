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
module IntVariable = Variable ()
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
type arith_func_spec = Z3.Expr.expr list -> Z3.Expr.expr -> Z3.Expr.expr

type arithmetic_func = {
  arith_func_args_typ: arith_args_spec;
  arith_func_spec: arith_func_spec
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

type program = {
  program_functions: func FunctionVariableMap.t;
  arith_functions : arithmetic_func ArithmeticFunctionVariableMap.t
}
