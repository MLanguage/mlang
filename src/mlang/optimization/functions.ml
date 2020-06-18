(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** Most functions are just syntactic sugar for operations expressible with the rest of the
    language, so we expand these. *)

open Mvg

let rec expand_functions_expr (e : expression Pos.marked) : expression Pos.marked =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      Pos.same_pos_as (Comparison (op, new_e1, new_e2)) e
  | Binop (op, e1, e2) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      Pos.same_pos_as (Binop (op, new_e1, new_e2)) e
  | Unop (op, e1) ->
      let new_e1 = expand_functions_expr e1 in
      Pos.same_pos_as (Unop (op, new_e1)) e
  | Conditional (e1, e2, e3) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      let new_e3 = expand_functions_expr e3 in
      Pos.same_pos_as (Conditional (new_e1, new_e2, new_e3)) e
  | Index (var, e1) ->
      let new_e1 = expand_functions_expr e1 in
      Pos.same_pos_as (Index (var, new_e1)) e
  | Literal _ -> e
  | Var _ -> e
  | LocalVar _ -> e
  | GenericTableIndex -> e
  | Error -> e
  | LocalLet (lvar, e1, e2) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      Pos.same_pos_as (LocalLet (lvar, new_e1, new_e2)) e
  | FunctionCall (SumFunc, args) ->
      Pos.same_pos_as
        (List.fold_left
           (fun acc arg ->
             if acc = Error then Pos.unmark (expand_functions_expr arg)
             else Binop (Pos.same_pos_as Ast.Add e, Pos.same_pos_as acc e, expand_functions_expr arg))
           Error args)
        e
  | FunctionCall (GtzFunc, [ arg ]) ->
      Pos.same_pos_as
        (Conditional
           ( Pos.same_pos_as
               (Comparison
                  ( Pos.same_pos_as Ast.Gt e,
                    expand_functions_expr arg,
                    Pos.same_pos_as (Literal (Float 0.0)) e ))
               e,
             Pos.same_pos_as (Literal (Float 1.0)) e,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FunctionCall (GtezFunc, [ arg ]) ->
      Pos.same_pos_as
        (Conditional
           ( Pos.same_pos_as
               (Comparison
                  ( Pos.same_pos_as Ast.Gte e,
                    expand_functions_expr arg,
                    Pos.same_pos_as (Literal (Float 0.0)) e ))
               e,
             Pos.same_pos_as (Literal (Float 1.0)) e,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FunctionCall (((MinFunc | MaxFunc) as f), [ arg1; arg2 ]) ->
      let earg1 = expand_functions_expr arg1 in
      let earg2 = expand_functions_expr arg2 in
      Pos.same_pos_as (FunctionCall (f, [ earg1; earg2 ])) e
  | FunctionCall (AbsFunc, [ arg ]) ->
      let arg_var = LocalVariable.new_var () in
      Pos.same_pos_as
        (LocalLet
           ( arg_var,
             expand_functions_expr arg,
             Pos.same_pos_as
               (Conditional
                  ( Pos.same_pos_as
                      (Comparison
                         ( Pos.same_pos_as Ast.Lt e,
                           Pos.same_pos_as (LocalVar arg_var) e,
                           Pos.same_pos_as (Literal (Float 0.0)) e ))
                      e,
                    Pos.same_pos_as (Unop (Ast.Minus, Pos.same_pos_as (LocalVar arg_var) e)) e,
                    Pos.same_pos_as (LocalVar arg_var) e ))
               e ))
        e
  | FunctionCall (NullFunc, [ arg ]) ->
      Pos.same_pos_as
        (Conditional
           ( Pos.same_pos_as
               (Comparison
                  ( Pos.same_pos_as Ast.Eq e,
                    expand_functions_expr arg,
                    Pos.same_pos_as (Literal (Float 0.0)) e ))
               e,
             Pos.same_pos_as (Literal (Float 1.0)) e,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FunctionCall (PresentFunc, [ arg ]) ->
      (* we do not expand this function as it deals specifically with undefined variables *)
      Pos.same_pos_as (FunctionCall (PresentFunc, [ expand_functions_expr arg ])) e
  | FunctionCall (ArrFunc, [ arg ]) ->
      (* we do not expand this function as it requires modulo or modf *)
      Pos.same_pos_as (FunctionCall (ArrFunc, [ expand_functions_expr arg ])) e
  | FunctionCall (InfFunc, [ arg ]) ->
      (* we do not expand this function as it requires modulo or modf *)
      Pos.same_pos_as (FunctionCall (InfFunc, [ expand_functions_expr arg ])) e
  | _ -> e

let expand_functions (p : program) : program =
  {
    p with
    program_vars =
      VariableMap.map
        (fun def ->
          match def.var_definition with
          | InputVar -> def
          | SimpleVar e -> { def with var_definition = SimpleVar (expand_functions_expr e) }
          | TableVar (size, defg) -> (
              match defg with
              | IndexGeneric e ->
                  {
                    def with
                    var_definition = TableVar (size, IndexGeneric (expand_functions_expr e));
                  }
              | IndexTable es ->
                  {
                    def with
                    var_definition =
                      TableVar
                        (size, IndexTable (IndexMap.map (fun e -> expand_functions_expr e) es));
                  } ))
        p.program_vars;
    program_conds =
      VariableMap.map
        (fun cond -> { cond with cond_expr = expand_functions_expr cond.cond_expr })
        p.program_conds;
  }
