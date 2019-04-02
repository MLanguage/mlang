(*
  Copyright 2018 Denis Merigoux and INRIA

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

open Cfg

let format_typ (t: typ) : string = match t with
  | Integer -> "integer"
  | Real -> "real"
  | Boolean -> "boolean"

let format_func (f: func) : string = match f with
  | SumFunc -> "somme"
  | AbsFunc -> "abs"
  | MinFunc -> "min"
  | MaxFunc -> "max"
  | GtzFunc -> "positif"
  | GtezFunc -> "positif_ou_nul"
  | NullFunc -> "null"
  | ArrFunc -> "arr"
  | InfFunc -> "inf"
  | PresentFunc -> "present"

let format_literal (l: literal) : string = match l with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b

let rec format_expression (e: expression) : string = match e with
  | Comparison ((op, _), (e1, _), (e2, _)) ->
    (format_expression e1) ^ " " ^ (Format_ast.format_comp_op op) ^ "" ^ (format_expression e2)
  | Binop ((op, _), (e1, _), (e2, _)) ->
    (format_expression e1) ^ " " ^ (Format_ast.format_binop op) ^ "" ^ (format_expression e2)
  | Unop (op, (e, _)) ->
    (Format_ast.format_unop op) ^ " " ^ (format_expression e)
  | Conditional ((e1, _), (e2, _), (e3, _)) ->
    "si " ^ (format_expression e1) ^ " alors " ^ (format_expression e2) ^
    " sinon " ^ (format_expression e3)
  | FunctionCall(f, args) -> Printf.sprintf "%s(%s)"
                               (format_func f )
                               (String.concat "," (List.map (fun e -> format_expression (Ast.unmark e)) args))
  | Literal lit -> format_literal lit
  | Var var -> Ast.unmark var.Variable.name
  | LocalVar lvar -> "x" ^ (string_of_int lvar.LocalVariable.id)
  | GenericTableIndex -> "X"
  | Error -> "indéfini"
  | LocalLet (lvar, (e1, _), (e2, _)) ->
    "soit x" ^ (string_of_int lvar.LocalVariable.id) ^ "= "^
    (format_expression e1) ^ " dans " ^ (format_expression e2)
  | Index(var, i) ->
    Printf.sprintf "%s[%s]"
      (Ast.unmark (Ast.unmark var).Variable.name)
      (format_expression (Ast.unmark i))

let format_io (io: io) : string = match io with
  | Input -> "input"
  | Output -> "output"
  | Regular -> "regular"
