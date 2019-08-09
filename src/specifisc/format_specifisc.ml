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

open Specifisc

let format_typ (t: typ) : string = match t with
  | Int -> "integer"
  | Bool -> "boolean"

let format_comparison_op (op : comparison_op) : string = match op with
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  | Neq -> "!="
  | Eq -> "=="

let format_logical_binop (op: logical_binop) : string = match op with
  | And -> "&&"
  | Or -> "||"

let format_arithmetic_binop (op: arithmetic_binop) : string = match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let format_bool_var (b: BoolVariable.t) : string =
  Printf.sprintf "%s_%d"
    (Ast.unmark b.BoolVariable.name)
    b.BoolVariable.id

let format_int_var (b: IntVariable.t) : string =
  Printf.sprintf "%s_%d"
    (Ast.unmark b.IntVariable.name)
    b.IntVariable.id

let format_function_var (b: FunctionVariable.t) : string =
  Printf.sprintf "%s_%d"
    (Ast.unmark b.FunctionVariable.name)
    b.FunctionVariable.id

let rec format_logical_expression (e: logical_expression) : string = match e with
  | Comparison (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)"
      (format_arithmetic_expression (Ast.unmark e1))
      (format_comparison_op (Ast.unmark op))
      (format_arithmetic_expression (Ast.unmark e2))
  | LogicalBinop (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)"
      (format_logical_expression (Ast.unmark e1))
      (format_logical_binop (Ast.unmark op))
      (format_logical_expression (Ast.unmark e2))
  | LogicalNot e1 ->
    Printf.sprintf "!%s" (format_logical_expression (Ast.unmark e1))
  | BoolLiteral b -> string_of_bool b
  | BoolVar v -> format_bool_var v

and format_arithmetic_expression (e: arithmetic_expression) : string = match e with
  | ArithmeticBinop (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)"
      (format_arithmetic_expression (Ast.unmark e1))
      (format_arithmetic_binop (Ast.unmark op))
      (format_arithmetic_expression (Ast.unmark e2))
  | Conditional (e1, e2, e3) ->
    Printf.sprintf "(if %s then %s else %s)"
      (format_logical_expression (Ast.unmark e1))
      (format_arithmetic_expression (Ast.unmark e2))
      (format_arithmetic_expression (Ast.unmark e3))
  | ArithmeticMinus e1 ->
    Printf.sprintf "- %s" (format_arithmetic_expression (Ast.unmark e1))
  | IntLiteral i -> Int64.to_string i
  | IntVar v -> format_int_var v
  | _ -> raise (Errors.Unimplemented "specifisc formatting")

let format_command (c: command) : string = match c with
  | BoolDef (bv, e) ->
    Printf.sprintf "%s : bool := %s"
      (format_bool_var bv)
      (format_logical_expression (Ast.unmark e))
  | IntDef (iv, e) ->
    Printf.sprintf "%s : int := %s"
      (format_int_var iv)
      (format_arithmetic_expression (Ast.unmark e))
  | Constraint e ->
    Printf.sprintf "assert(%s)"
      (format_logical_expression (Ast.unmark e))

let format_func (f: func) : string =
  Printf.sprintf "function(%s, %s) -> %s, %s\n%s"
    (String.concat "," (List.map (fun v -> format_int_var v) (fst f.inputs)))
    (String.concat "," (List.map (fun v -> format_bool_var v) (snd f.inputs)))
    (String.concat "," (List.map (fun v -> format_int_var v) (fst f.outputs)))
    (String.concat "," (List.map (fun v -> format_bool_var v) (snd f.outputs)))
    (String.concat "\n" (List.map (fun c -> format_command c) f.body))

let format_program (p: program) : string =
  FunctionVariableMap.fold (fun fvar f acc ->
      acc ^ begin
        Printf.sprintf "%s ::= %s\n\n"
          (format_function_var fvar)
          (format_func f)
      end
    ) p.program_functions ""
