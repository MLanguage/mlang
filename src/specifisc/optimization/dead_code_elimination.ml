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


open Ast

type data = {
  used_bool_vars : unit BoolVariableMap.t;
  used_int_vars: unit IntVariableMap.t;
}

let empty_data (func: func) = {
  used_bool_vars = List.fold_left (fun acc var ->
      BoolVariableMap.add var () acc
    ) BoolVariableMap.empty (snd func.outputs);
  used_int_vars =  List.fold_left (fun acc var ->
      IntVariableMap.add var () acc
    ) IntVariableMap.empty (fst func.outputs);
}

let add_bool_use (var: BoolVariable.t) (data: data) : data =
  { data with
    used_bool_vars = BoolVariableMap.add var () data.used_bool_vars
  }

let add_int_use (var: IntVariable.t) (data: data) : data =
  { data with
    used_int_vars = IntVariableMap.add var () data.used_int_vars
  }

let rec process_bool_expr (e: logical_expression Pos.marked) (data: data) : data =
  match Pos.unmark e with
  | Comparison (_, e1, e2) ->
    let data = process_int_expr e1 data in
    let data = process_int_expr e2 data in
    data
  | LogicalBinop (_, e1, e2) ->
    let data = process_bool_expr e1 data in
    let data = process_bool_expr e2 data in
    data
  | LogicalNot e1 ->
    let data = process_bool_expr e1 data in
    data
  | BoolLiteral _ -> data
  | BoolVar v -> add_bool_use v data

and process_int_expr (e: arithmetic_expression Pos.marked) (data: data) : data =
  match Pos.unmark e with
  | ArithmeticBinop (_, e1, e2) ->
    let data = process_int_expr e1 data in
    let data = process_int_expr e2 data in
    data
  | ArithmeticMinus e1 ->
    let data = process_int_expr e1 data in
    data
  | Conditional (e1, e2, e3) ->
    let data = process_bool_expr e1 data in
    let data = process_int_expr e2 data in
    let data = process_int_expr e3 data in
    data
  | IntLiteral _ -> data
  | IntVar v -> add_int_use v data

let process_command (c: command) (data: data)
  : bool * data = match c with
  | BoolDef (var, e) ->
    let is_necessary = BoolVariableMap.mem var data.used_bool_vars in
    let data =
      if is_necessary then
        process_bool_expr e data
      else data
    in
    (is_necessary, data)
  | IntDef (var, e) ->
    let is_necessary = IntVariableMap.mem var data.used_int_vars in
    let data =
      if is_necessary then
        process_int_expr e data
      else data
    in
    (is_necessary, data)
  | Constraint e ->
    (true, process_bool_expr e data)

let optimize (p: program) : program =
  { p with
    program_functions = FunctionVariableMap.map (fun func ->
        { func with
          body =
            let data = empty_data func in
            let new_body, _ = List.fold_right (fun cmd (new_body, data) ->
                let is_necessary, data = process_command cmd data in
                (if is_necessary then cmd::new_body else new_body), data
              ) func.body ([], data)
            in
            new_body
        }
      ) p.program_functions
  }
