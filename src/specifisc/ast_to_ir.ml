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

let translate_arithmetic_expression
    (_: Ast.arithmetic_expression Pos.marked)
  : Ir.arithmetic_expression Pos.marked * Ir.command list =
  assert false

and translate_logical_expression
    (_: Ast.logical_expression Pos.marked)
  : Ir.logical_expression Pos.marked * Ir.command list =
  assert false

let translate_body (body: Ast.command list) : Ir.command list =
  let new_body = List.fold_left (fun new_body cmd ->
      match cmd with
      | Ast.BoolDef (bool_var, e) ->
        let new_e, new_cmds = translate_logical_expression e in
        (Ir.BoolDef (bool_var, new_e))::new_cmds@new_body
      | Ast.IntDef (int_var, e) ->
        let new_e, new_cmds = translate_arithmetic_expression e in
        (Ir.IntDef (int_var, new_e))::new_cmds@new_body
      | Ast.Constraint e ->
        let new_e, new_cmds = translate_logical_expression e in
        (Ir.Constraint new_e)::new_cmds@new_body
    ) [] body
  in
  List.rev new_body

let translate_program (p: Ast.program) : Ir.program =
  {
    Ir.program_idmap = p.Ast.program_idmap;
    Ir.program_mult_factor = p.Ast.program_mult_factor;
    Ir.program_functions = Ast.FunctionVariableMap.map (fun func ->
        {
          Ir.inputs = func.Ast.inputs;
          Ir.outputs = func.Ast.outputs;
          Ir.body = translate_body func.Ast.body
        }
      ) p.Ast.program_functions

  }
