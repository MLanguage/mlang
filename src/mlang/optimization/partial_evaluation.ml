(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** Partial evaluation mostly relies on the {!module: Verifisc.Interpreter} module ; however it also
    include peehole optimizations that must be checked for compatibility with M's semantics. *)

open Mvg

type partial_expr = PartialLiteral of literal | PartialVar of Variable.t

let partial_to_expr (e : partial_expr) : expression =
  match e with PartialLiteral l -> Literal l | PartialVar v -> Var v

let expr_to_partial (e : expression) : partial_expr =
  match e with Literal l -> PartialLiteral l | Var v -> PartialVar v | _ -> assert false

(* should not happen *)

type var_literal = SimpleVar of partial_expr | TableVar of int * partial_expr array

type ctx = {
  ctx_local_vars : partial_expr LocalVariableMap.t;
  ctx_inside_table_index : int option;
  ctx_inside_var : Variable.t;
}

let empty_ctx (var : Variable.t) (idx : int option) =
  { ctx_local_vars = LocalVariableMap.empty; ctx_inside_table_index = idx; ctx_inside_var = var }

let rec partial_evaluation (ctx : ctx) (interp_ctx : Interpreter.ctx) (p : program)
    (e : expression Pos.marked) : expression Pos.marked =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let new_e1 = partial_evaluation ctx interp_ctx p e1 in
      let new_e2 = partial_evaluation ctx interp_ctx p e2 in
      Pos.same_pos_as
        begin
          match (Pos.unmark new_e1, Pos.unmark new_e2) with
          | Literal Undefined, _ | _, Literal Undefined -> Literal Undefined
          | Conditional (b, (Literal (Float 1.), _), (Literal (Float 0.), _)), Literal (Float 1.)
            when Pos.unmark op = Ast.Eq ->
              Pos.unmark b
          | (FunctionCall (Mvg.PresentFunc, _) as fc), Literal (Float 1.)
            when Pos.unmark op = Ast.Neq ->
              Unop (Ast.Not, Pos.same_pos_as fc new_e1)
          | Literal _, Literal _ ->
              Mvg.Literal
                (Interpreter.evaluate_expr interp_ctx p
                   (Pos.same_pos_as (Comparison (op, new_e1, new_e2)) e))
          | _ -> Comparison (op, new_e1, new_e2)
        end
        e
  | Binop (op, e1, e2) ->
      let new_e1 = partial_evaluation ctx interp_ctx p e1 in
      let new_e2 = partial_evaluation ctx interp_ctx p e2 in
      Pos.same_pos_as
        begin
          match (Pos.unmark op, Pos.unmark new_e1, Pos.unmark new_e2) with
          | Ast.And, Literal Undefined, _
          | Ast.And, _, Literal Undefined
          | Ast.Or, Literal Undefined, _
          | Ast.Or, _, Literal Undefined
          | Ast.Div, _, Literal Undefined ->
              Literal Undefined
          | Ast.Or, Literal (Float f), _ when f != 0. -> Literal true_literal
          | Ast.Or, _, Literal (Float f) when f != 0. -> Literal true_literal
          | Ast.And, Literal (Float 0.), _ | Ast.And, _, Literal (Float 0.) -> Literal false_literal
          | Ast.And, Literal (Float f), e' when f != 0. -> e'
          | Ast.And, e', Literal (Float f) when f != 0. -> e'
          | Ast.Or, Literal (Float 0.), e'
          | Ast.Or, e', Literal (Float 0.)
          | Ast.Add, Literal (Float 0. | Undefined), e'
          | Ast.Add, e', Literal (Float 0. | Undefined)
          | Ast.Mul, Literal (Float 1.), e'
          | Ast.Mul, e', Literal (Float 1.)
          | Ast.Div, e', Literal (Float 1.)
          | Ast.Sub, e', Literal (Float 0. | Undefined) ->
              e'
          | Ast.Sub, Literal (Float 0. | Undefined), e' -> Unop (Minus, Pos.same_pos_as e' e)
          | Ast.Mul, Literal (Float 0. | Undefined), _
          | Ast.Mul, _, Literal (Float 0. | Undefined)
          | Ast.Div, Literal (Float 0. | Undefined), _ ->
              Mvg.Literal (Mvg.Float 0.)
          | _, Literal _, Literal _ ->
              Mvg.Literal
                (Interpreter.evaluate_expr interp_ctx p
                   (Pos.same_pos_as (Binop (op, new_e1, new_e2)) e1))
          | Ast.Add, _, Literal (Float f) when f < 0. ->
              Binop (Pos.same_pos_as Ast.Sub op, e1, Pos.same_pos_as (Literal (Float (-.f))) e2)
          | Ast.Add, _, Unop (Minus, e2') -> Binop (Pos.same_pos_as Ast.Sub op, e1, e2')
          | _ -> Binop (op, new_e1, new_e2)
        end
        e
  | Unop (op, e1) ->
      let new_e1 = partial_evaluation ctx interp_ctx p e1 in
      Pos.same_pos_as
        begin
          match Pos.unmark new_e1 with
          | Literal _ ->
              Mvg.Literal
                (Interpreter.evaluate_expr interp_ctx p (Pos.same_pos_as (Unop (op, new_e1)) e1))
          | _ -> Unop (op, new_e1)
        end
        e
  | Conditional (e1, e2, e3) -> (
      let new_e1 = partial_evaluation ctx interp_ctx p e1 in
      let new_e2 = partial_evaluation ctx interp_ctx p e2 in
      let new_e3 = partial_evaluation ctx interp_ctx p e3 in
      match Pos.unmark new_e1 with
      | Literal (Float 0.) -> new_e3
      | Literal (Float _) -> new_e2
      | Literal Undefined -> Pos.same_pos_as (Literal Undefined) e
      | _ -> Pos.same_pos_as (Conditional (new_e1, new_e2, new_e3)) e )
  | Index (var, e1) -> (
      let new_e1 = partial_evaluation ctx interp_ctx p e1 in
      match Pos.unmark new_e1 with
      | Literal Undefined -> Pos.same_pos_as (Literal Undefined) e
      | Literal l -> (
          let idx =
            match l with
            | Undefined -> assert false (* should not happen *)
            | Float f ->
                if
                  let fraction, _ = modf f in
                  fraction = 0.
                then int_of_float f
                else
                  raise
                    (Interpreter.RuntimeError
                       ( Interpreter.FloatIndex
                           (Format.asprintf "%a" Pos.format_position (Pos.get_position e1)),
                         interp_ctx ))
          in
          match (VariableMap.find (Pos.unmark var) p.program_vars).var_definition with
          | SimpleVar _ | InputVar -> assert false (* should not happen *)
          | TableVar (size, IndexGeneric e') -> (
              if idx >= size || idx < 0 then Pos.same_pos_as (Literal Undefined) e
              else
                match Pos.unmark e' with
                | Literal _ | Var _ -> e'
                | _ -> Pos.same_pos_as (Index (var, new_e1)) e )
          | TableVar (size, IndexTable es') -> (
              if idx >= size || idx < 0 then Pos.same_pos_as (Literal Undefined) e
              else
                match Pos.unmark (IndexMap.find idx es') with
                | Literal _ | Var _ -> IndexMap.find idx es'
                | Index (inner_var, (Literal (Float inner_idx), _))
                (* If the current index depends on the value of a greater index of the same table we
                   return undefined *)
                  when Pos.unmark inner_var = ctx.ctx_inside_var
                       &&
                       match ctx.ctx_inside_table_index with
                       | Some i when i >= int_of_float inner_idx -> true
                       | _ -> false ->
                    Pos.same_pos_as (Literal Undefined) (IndexMap.find idx es')
                | _ -> Pos.same_pos_as (Index (var, new_e1)) e ) )
      | _ -> Pos.same_pos_as (Index (var, new_e1)) e )
  | Literal _ -> e
  | Var var -> (
      match
        try (VariableMap.find var p.program_vars).var_definition with Not_found -> assert false
        (* should not happen *)
      with
      | SimpleVar e' -> ( match Pos.unmark e' with Literal _ | Var _ -> e' | _ -> e )
      | TableVar _ | InputVar -> e )
  | LocalVar lvar -> (
      try Pos.same_pos_as (partial_to_expr (LocalVariableMap.find lvar ctx.ctx_local_vars)) e
      with Not_found -> e )
  | GenericTableIndex -> e
  | Error -> e
  | LocalLet (l1, b, (LocalVar l2, _)) when LocalVariable.compare l1 l2 = 0 -> b
  | LocalLet (lvar, e1, e2) -> (
      let new_e1 = partial_evaluation ctx interp_ctx p e1 in
      match Pos.unmark new_e1 with
      | Literal _ | Var _ ->
          let new_ctx =
            {
              ctx with
              ctx_local_vars =
                LocalVariableMap.add lvar (expr_to_partial (Pos.unmark new_e1)) ctx.ctx_local_vars;
            }
          in
          let new_e2 = partial_evaluation new_ctx interp_ctx p e2 in
          new_e2
      | _ -> (
          let new_e2 = partial_evaluation ctx interp_ctx p e2 in
          match Pos.unmark new_e2 with
          | Literal _ | Var _ -> new_e2
          | _ -> Pos.same_pos_as (LocalLet (lvar, new_e1, new_e2)) e ) )
  | FunctionCall (((ArrFunc | InfFunc | PresentFunc | NullFunc) as f), [ arg ]) -> (
      let new_arg = partial_evaluation ctx interp_ctx p arg in
      match Pos.unmark new_arg with
      | Literal _ ->
          Pos.same_pos_as
            (Mvg.Literal
               (Interpreter.evaluate_expr interp_ctx p
                  (Pos.same_pos_as (FunctionCall (f, [ new_arg ])) e)))
            e
      | _ -> Pos.same_pos_as (FunctionCall (f, [ new_arg ])) e )
  | FunctionCall (func, args) ->
      Pos.same_pos_as
        (FunctionCall (func, List.map (fun arg -> partial_evaluation ctx interp_ctx p arg) args))
        e

let partially_evaluate (p : program) (dep_graph : Dependency.DepGraph.t) : program =
  let exec_order = Execution_order.get_execution_order dep_graph in
  let interp_ctx = Interpreter.empty_ctx p in
  List.fold_left
    (fun p var ->
      try
        let def = VariableMap.find var p.program_vars in
        let new_def =
          match def.var_definition with
          | InputVar -> InputVar
          | SimpleVar e ->
              let e' = partial_evaluation (empty_ctx var None) interp_ctx p e in
              SimpleVar e'
          | TableVar (size, def) -> (
              match def with
              | IndexGeneric e ->
                  TableVar
                    (size, IndexGeneric (partial_evaluation (empty_ctx var None) interp_ctx p e))
              | IndexTable es ->
                  TableVar
                    ( size,
                      IndexTable
                        (IndexMap.mapi
                           (fun idx e ->
                             partial_evaluation (empty_ctx var (Some idx)) interp_ctx p e)
                           es) ) )
        in
        {
          p with
          program_vars = VariableMap.add var { def with var_definition = new_def } p.program_vars;
        }
      with Not_found -> (
        try
          let cond = VariableMap.find var p.program_conds in
          match partial_evaluation (empty_ctx var None) interp_ctx p cond.cond_expr with
          | Literal (Float 0.), _ | Literal Undefined, _ ->
              { p with program_conds = VariableMap.remove var p.program_conds }
          | Literal (Float _), _ ->
              raise
                (Interpreter.RuntimeError
                   (Interpreter.ConditionViolated (cond.cond_errors, cond.cond_expr, []), interp_ctx))
          | new_cond_expr ->
              {
                p with
                program_conds =
                  VariableMap.add var { cond with cond_expr = new_cond_expr } p.program_conds;
              }
        with Not_found -> assert false )
      (* should not happen *))
    p exec_order
