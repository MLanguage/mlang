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
   Most functions are just syntactic sugar for operations expressible with the rest of
   the language, so we expand these.
*)

open Mvg

let rec expand_functions_expr (e: expression Ast.marked) : expression Ast.marked =
  match Ast.unmark e with
  | Comparison (op, e1, e2) ->
    let new_e1 = expand_functions_expr e1 in
    let new_e2 = expand_functions_expr e2 in
    Ast.same_pos_as (Comparison (op, new_e1, new_e2)) e
  | Binop (op, e1, e2) ->
    let new_e1 = expand_functions_expr e1 in
    let new_e2 = expand_functions_expr e2 in
    Ast.same_pos_as (Binop (op, new_e1, new_e2)) e
  | Unop (op, e1) ->
    let new_e1 = expand_functions_expr e1 in
    Ast.same_pos_as (Unop (op, new_e1)) e
  | Conditional (e1, e2, e3) ->
    let new_e1 = expand_functions_expr e1 in
    let new_e2 = expand_functions_expr e2 in
    let new_e3 = expand_functions_expr e3 in
    Ast.same_pos_as (Conditional (new_e1, new_e2, new_e3)) e
  | Index (var, e1) ->
    let new_e1 = expand_functions_expr e1 in
    Ast.same_pos_as (Index(var, new_e1)) e
  | Literal _ -> e
  | Var _ -> e
  | LocalVar _ -> e
  | GenericTableIndex -> e
  | Error -> e
  | LocalLet (lvar, e1, e2) ->
    let new_e1 = expand_functions_expr e1 in
    let new_e2 = expand_functions_expr e2 in
    Ast.same_pos_as (LocalLet(lvar, new_e1, new_e2)) e
  | FunctionCall (SumFunc, args) ->
    Ast.same_pos_as (
      List.fold_left (fun acc arg ->
          if acc = Error then
            Ast.unmark (expand_functions_expr arg)
          else
            Binop (Ast.same_pos_as Ast.Add e, Ast.same_pos_as acc e, expand_functions_expr arg)
        ) Error args
    ) e
  | FunctionCall (GtzFunc, [arg]) ->
    Ast.same_pos_as
      (Conditional (
          Ast.same_pos_as
            (Comparison (
                Ast.same_pos_as Ast.Gt e,
                expand_functions_expr arg,
                Ast.same_pos_as (Literal (Int 0)) e)
            ) e,
          Ast.same_pos_as (Literal (Int 1)) e,
          Ast.same_pos_as (Literal (Int 0)) e)
      ) e
  | FunctionCall (GtezFunc, [arg]) ->
    Ast.same_pos_as
      (Conditional (
          Ast.same_pos_as
            (Comparison (
                Ast.same_pos_as Ast.Gte e,
                expand_functions_expr arg,
                Ast.same_pos_as (Literal (Int 0)) e)
            ) e,
          Ast.same_pos_as (Literal (Int 1)) e,
          Ast.same_pos_as (Literal (Int 0)) e)
      ) e
  | FunctionCall (MinFunc, args) ->
    Ast.same_pos_as (
      List.fold_left (fun acc arg ->
          if acc = Error then
            Ast.unmark (expand_functions_expr arg)
          else
            let new_arg = expand_functions_expr arg in
            let new_arg_var = LocalVariable.new_var () in
            let acc_var = LocalVariable.new_var () in
            LocalLet (
              acc_var,
              Ast.same_pos_as acc e,
              Ast.same_pos_as (LocalLet (
                  new_arg_var,
                  new_arg,
                  Ast.same_pos_as (
                    Conditional (
                      Ast.same_pos_as (Comparison (
                          Ast.same_pos_as Ast.Lt e,
                          Ast.same_pos_as (LocalVar new_arg_var) e,
                          Ast.same_pos_as (LocalVar acc_var) e
                        )) e,
                      Ast.same_pos_as (LocalVar new_arg_var) e,
                      Ast.same_pos_as (LocalVar acc_var) e
                    )
                  ) e
                ))
                e
            )
        ) Error args
    ) e
  | FunctionCall (MaxFunc, args) ->
    Ast.same_pos_as (
      List.fold_left (fun acc arg ->
          if acc = Error then
            Ast.unmark (expand_functions_expr arg)
          else
            let new_arg = expand_functions_expr arg in
            let new_arg_var = LocalVariable.new_var () in
            let acc_var = LocalVariable.new_var () in
            LocalLet (
              acc_var,
              Ast.same_pos_as acc e,
              Ast.same_pos_as (LocalLet (
                  new_arg_var,
                  new_arg,
                  Ast.same_pos_as (
                    Conditional (
                      Ast.same_pos_as (Comparison (
                          Ast.same_pos_as Ast.Gt e,
                          Ast.same_pos_as (LocalVar new_arg_var) e,
                          Ast.same_pos_as (LocalVar acc_var) e
                        )) e,
                      Ast.same_pos_as (LocalVar new_arg_var) e,
                      Ast.same_pos_as (LocalVar acc_var) e
                    )
                  ) e
                ))
                e
            )
        ) Error args
    ) e
  | FunctionCall (AbsFunc, [arg]) ->
    let arg_var = LocalVariable.new_var () in
    Ast.same_pos_as
      (LocalLet (
          arg_var,
          expand_functions_expr arg,
          Ast.same_pos_as (Conditional (
              (Ast.same_pos_as (Comparison (
                   Ast.same_pos_as Ast.Lt e,
                   Ast.same_pos_as (LocalVar arg_var) e,
                   Ast.same_pos_as (Literal (Int 0)) e)) e,
               Ast.same_pos_as (Unop (Ast.Minus, Ast.same_pos_as (LocalVar arg_var) e)) e,
               Ast.same_pos_as (LocalVar arg_var) e
              ))) e)) e
  | FunctionCall (NullFunc, [arg]) ->
    Ast.same_pos_as
      (Conditional (
          Ast.same_pos_as
            (Comparison (
                Ast.same_pos_as Ast.Eq e,
                expand_functions_expr arg,
                Ast.same_pos_as (Literal (Int 0)) e)
            ) e,
          Ast.same_pos_as (Literal (Int 1)) e,
          Ast.same_pos_as (Literal (Int 0)) e)
      ) e
  | FunctionCall (PresentFunc, [arg]) ->
    (* we do not expand this function as it deals specifically with undefined variables  *)
    Ast.same_pos_as (FunctionCall (PresentFunc, [expand_functions_expr arg])) e
  | FunctionCall (ArrFunc, [arg]) ->
    (* we do not expand this function as it requires modulo or modf *)
    Ast.same_pos_as (FunctionCall (ArrFunc, [expand_functions_expr arg])) e
  | FunctionCall (InfFunc, [arg]) ->
    (* we do not expand this function as it requires modulo or modf *)
    Ast.same_pos_as (FunctionCall (InfFunc, [expand_functions_expr arg])) e
  | _ -> e

let expand_functions (p: program) : program =
  { p with program_vars =
             VariableMap.map (fun def ->
                 match def.var_definition with
                 | InputVar -> def
                 | SimpleVar e -> { def with var_definition = SimpleVar (expand_functions_expr e) }
                 | TableVar (size, defg) -> begin match defg with
                     | IndexGeneric e ->
                       { def with
                         var_definition =
                           TableVar(size, IndexGeneric (expand_functions_expr e))
                       }
                     | IndexTable es ->
                       { def with
                         var_definition =
                           TableVar(size,
                                    IndexTable (IndexMap.map
                                                  (fun e -> expand_functions_expr e)
                                                  es))
                       }
                   end
               ) p.program_vars;
           program_conds = VariableMap.map (fun cond ->
               { cond with cond_expr = expand_functions_expr cond.cond_expr }
             ) p.program_conds
  }
