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

module Pos = Specifisc.Pos
open Mvg

let format_execution_number (exec_number: execution_number) : string =
  if exec_number.rule_number = -1 then
    Printf.sprintf "declaration, %s"
      (Pos.format_position exec_number.pos)
  else
    Printf.sprintf "rule %d, sequence index %d, %s"
      exec_number.rule_number
      exec_number.seq_number
      (Pos.format_position exec_number.pos)

let format_execution_number_short (exec_number: execution_number) : string =
  if exec_number.rule_number = -1 then "declaration"else
    Printf.sprintf "%d#%d"
      exec_number.rule_number
      exec_number.seq_number


let format_typ (t: typ) : string = match t with
  | Integer -> "integer"
  | Real -> "real"
  | Boolean -> "boolean"

let format_io (io: io) : string = match io with
  | Input -> "input"
  | Output -> "output"
  | Regular -> "regular"

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
  | Multimax -> "multimax"
  | Supzero -> "supzero"

let format_literal (l: literal) : string = match l with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Undefined -> "indéfini"

let rec format_expression (e: expression) : string = match e with
  | Comparison ((op, _), (e1, _), (e2, _)) ->
    "(" ^ (format_expression e1) ^ " " ^ (Format_ast.format_comp_op op) ^ " " ^ (format_expression e2) ^ ")"
  | Binop ((op, _), (e1, _), (e2, _)) ->
    "(" ^ (format_expression e1) ^ " " ^ (Format_ast.format_binop op) ^ " " ^ (format_expression e2) ^ ")"
  | Unop (op, (e, _)) ->
    (Format_ast.format_unop op) ^ " " ^ (format_expression e)
  | Conditional ((e1, _), (e2, _), (e3, _)) ->
    "(si " ^ (format_expression e1) ^ " alors " ^ (format_expression e2) ^
    " sinon " ^ (format_expression e3) ^ ")"
  | FunctionCall(f, args) -> Printf.sprintf "%s(%s)"
                               (format_func f )
                               (String.concat "," (List.map (fun e -> format_expression (Pos.unmark e)) args))
  | Literal lit -> format_literal lit
  | Var var ->
    Printf.sprintf "%s[%s]"
      (Pos.unmark var.Variable.name)
      (format_execution_number_short var.Variable.execution_number)
  | LocalVar lvar -> "t" ^ (string_of_int lvar.LocalVariable.id)
  | GenericTableIndex -> "X"
  | Error -> "erreur"
  | LocalLet (lvar, (e1, _), (e2, _)) ->
    "soit t" ^ (string_of_int lvar.LocalVariable.id) ^ " = ("^
    (format_expression e1) ^ ") dans " ^ (format_expression e2)
  | Index(var, i) ->
    Printf.sprintf "%s[%s]"
      (Pos.unmark (Pos.unmark var).Variable.name)
      (format_expression (Pos.unmark i))

let format_variable_def (def: variable_def) : string = match def  with
  | SimpleVar e -> format_expression (Pos.unmark e) ^ "\n"
  | InputVar -> "[User input]\n"
  | TableVar (_, IndexGeneric e) -> "X -> " ^ (format_expression (Pos.unmark e)) ^ "\n"
  | TableVar (_, IndexTable defs) -> IndexMap.fold (fun i e acc ->
      acc ^ (Printf.sprintf "%d -> %s\n" i (format_expression (Pos.unmark e)))
    ) defs ""

let format_program_vars (p: variable_data VariableMap.t) : string = VariableMap.fold (fun var def acc ->
    acc ^ (Printf.sprintf "Variable %s%s of type %s, io %s:\n%s"
             (Pos.unmark var.Variable.name)
             (match var.Variable.alias with Some x -> " (alias "^ x ^")" | None -> "")
             (match def.var_typ with | None -> "unknown" | Some t -> format_typ t)
             (format_io def.var_io)
             (format_variable_def def.var_definition))
  ) p ""

let format_error (e: Error.t) : string =
  Printf.sprintf "erreur %s (%s)"
    (Pos.unmark e.Error.name)
    (Pos.unmark e.Error.descr)

let format_precondition (precond: condition_data) : string =
  Printf.sprintf "Précondition : %s\nSinon %s"
    (format_expression (Pos.unmark precond.cond_expr))
    (String.concat "," (List.map (fun err -> format_error err) precond.cond_errors))

let format_program_conds (conds: condition_data VariableMap.t) : string =
  String.concat
    "\n"
    (List.map
       (fun (_, cond) -> format_precondition cond) (VariableMap.bindings conds)
    )
let format_program (p: program) : string =
  Printf.sprintf "%s\n\n%s" (format_program_vars p.program_vars) (format_program_conds p.program_conds)

let format_variable (v: Variable.t) : string =
  Printf.sprintf "%s: %s" (Pos.unmark v.Variable.name) (Pos.unmark v.Variable.descr)

let format_io (io: io) : string = match io with
  | Input -> "input"
  | Output -> "output"
  | Regular -> "regular"
