(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** This modules defines M's static semantic. There is only one type: float. The typechecking is
    mostly about differentiating table from non-tables variables *)

open Mvg

type ctx = { ctx_program : program; ctx_is_generic_table : bool }

type typ_info = { table_info_var : bool Mvg.VariableMap.t (* the bool flag is_table *) }

let rec typecheck_top_down (ctx : ctx) (e : expression Pos.marked) : ctx =
  match Pos.unmark e with
  | Comparison (_, e1, e2) ->
      let ctx = typecheck_top_down ctx e1 in
      let ctx = typecheck_top_down ctx e2 in
      ctx
  | Binop (((Ast.And | Ast.Or), _), e1, e2)
  | Binop (((Ast.Add | Ast.Sub | Ast.Mul | Ast.Div), _), e1, e2) ->
      let ctx = typecheck_top_down ctx e1 in
      let ctx = typecheck_top_down ctx e2 in
      ctx
  | Unop (Ast.Not, e) | Unop (Ast.Minus, e) ->
      let ctx = typecheck_top_down ctx e in
      ctx
  | Conditional (e1, e2, e3) ->
      let ctx = typecheck_top_down ctx e1 in
      let ctx = typecheck_top_down ctx e2 in
      let ctx = typecheck_top_down ctx e3 in
      ctx
  | FunctionCall (func, args) ->
      let typechecker = typecheck_func_args func (Pos.get_position e) in
      let ctx = typechecker ctx args in
      ctx
  | Literal (Float _) -> ctx
  | Literal Undefined ->
      (* Literal has all the types *)
      ctx
  | Var _ -> ctx (* All variables are real *)
  | LocalLet (_local_var, e1, e2) ->
      let ctx = typecheck_top_down ctx e1 in
      let ctx = typecheck_top_down ctx e2 in
      ctx
  | Error -> ctx
  | LocalVar _ -> ctx
  | GenericTableIndex ->
      if ctx.ctx_is_generic_table then ctx
      else
        Errors.raise_typ_error Variable "Generic table index appears outside of table %a"
          Pos.format_position (Pos.get_position e)
  | Index ((var, var_pos), e') -> (
      (* Tables are only tables of arrays *)
      let ctx = typecheck_top_down ctx e' in
      let var_data = VariableMap.find var ctx.ctx_program.program_vars in
      match var_data.Mvg.var_definition with
      | SimpleVar _ | InputVar ->
          Errors.raise_typ_error Typing
            "variable %s is accessed %a as a table but it is not defined as one %a"
            (Pos.unmark var.Variable.name) Pos.format_position var_pos Pos.format_position
            (Pos.get_position var.Variable.name)
      | TableVar _ -> ctx )

and typecheck_func_args (f : func) (pos : Pos.position) :
    ctx -> Mvg.expression Pos.marked list -> ctx =
  match f with
  | SumFunc | MinFunc | MaxFunc ->
      fun ctx args ->
        if List.length args = 0 then
          Errors.raise_typ_error Typing "sum function must be called %a with at least one argument"
            Pos.format_position pos
        else
          let ctx = typecheck_top_down ctx (List.hd args) in
          let ctx =
            List.fold_left
              (fun ctx arg ->
                let ctx = typecheck_top_down ctx arg in
                ctx)
              ctx args
          in
          ctx
  | AbsFunc -> (
      fun ctx args ->
        match args with
        | [ arg ] ->
            let ctx = typecheck_top_down ctx arg in
            ctx
        | _ ->
            Errors.raise_typ_error Typing "function abs %a should have only one argument"
              Pos.format_position pos )
  | PresentFunc | NullFunc | GtzFunc | GtezFunc | Supzero -> (
      fun (* These functions return a integer value encoding a boolean; 0 for false and 1 for true *)
            ctx args ->
        match args with
        | [ arg ] ->
            let ctx = typecheck_top_down ctx arg in
            ctx
        | _ ->
            Errors.raise_typ_error Typing "function %a should have only one argument"
              Pos.format_position pos )
  | ArrFunc | InfFunc -> (
      fun ctx args ->
        match args with
        | [ arg ] ->
            let ctx = typecheck_top_down ctx arg in
            ctx
        | _ ->
            Errors.raise_typ_error Typing "function %a should have only one argument"
              Pos.format_position pos )
  | Mvg.Multimax -> (
      fun ctx args ->
        match args with
        | [ bound; table ] ->
            let ctx = typecheck_top_down ctx bound in
            typecheck_top_down ctx table
        | _ ->
            Errors.raise_typ_error Typing "function %a should have two arguments"
              Pos.format_position pos )

let determine_def_complete_cover (table_var : Mvg.Variable.t) (size : int)
    (defs : (int * Pos.position) list) : int list =
  (* Return all undefined indexes *)
  let defs_array = Array.make size false in
  List.iter
    (fun (def, def_pos) ->
      try defs_array.(def) <- true
      with Invalid_argument _ ->
        Errors.raise_typ_error Variable
          "the definitions of index %d %a, from table %s of size %d declared %a is out of bounds"
          def Pos.format_position def_pos
          (Pos.unmark table_var.Variable.name)
          size Pos.format_position
          (Pos.get_position table_var.Variable.name))
    defs;
  let undefined = ref [] in
  Array.iteri (fun index defined -> if not defined then undefined := index :: !undefined) defs_array;
  List.sort compare !undefined

let typecheck_program_conds (ctx : ctx) (conds : condition_data VariableMap.t) : ctx =
  VariableMap.fold (fun _ cond ctx -> typecheck_top_down ctx cond.cond_expr) conds ctx

let rec check_non_recursivity_expr (e : expression Pos.marked) (lvar : Variable.t) : unit =
  match Pos.unmark e with
  | Comparison (_, e1, e2) | Binop (_, e1, e2) | LocalLet (_, e1, e2) ->
      check_non_recursivity_expr e1 lvar;
      check_non_recursivity_expr e2 lvar
  | Unop (_, e1) -> check_non_recursivity_expr e1 lvar
  | Index (_, e1) ->
      (* We don't check for recursivity in indexes because tables can refer to themselves in
         definition. It is only at runtime that we return [Undefined] for index definitions that
         refer to indexes of the same table greater than themselves. *)
      check_non_recursivity_expr e1 lvar
  | Conditional (e1, e2, e3) ->
      check_non_recursivity_expr e1 lvar;
      check_non_recursivity_expr e2 lvar;
      check_non_recursivity_expr e3 lvar
  | FunctionCall (_, args) -> List.iter (fun arg -> check_non_recursivity_expr arg lvar) args
  | Literal _ | LocalVar _ | GenericTableIndex | Error -> ()
  | Var var ->
      if var = lvar then
        Errors.raise_typ_error Variable
          "You cannot refer to the variable %s since you are defining it (%a)"
          (Pos.unmark var.Variable.name) Pos.format_position (Pos.get_position e)
      else ()

let check_non_recursivity_of_variable_defs (var : Variable.t) (def : variable_def) : unit =
  match def with SimpleVar e -> check_non_recursivity_expr e var | TableVar _ | InputVar -> ()

(* The typechecker returns a new program because it defines missing table entries as "undefined" *)
let typecheck (p : program) : typ_info * program =
  let are_tables, ctx, p_vars =
    Mvg.VariableMap.fold
      (fun var def (acc, ctx, p_vars) ->
        check_non_recursivity_of_variable_defs var def.var_definition;
        (* All top-level variables are og type Real *)
        match def.var_definition with
        | SimpleVar e ->
            let new_ctx = typecheck_top_down { ctx with ctx_is_generic_table = false } e in
            (VariableMap.add var false acc, new_ctx, p_vars)
        | TableVar (size, defs) -> (
            match defs with
            | IndexGeneric e ->
                let new_ctx = typecheck_top_down { ctx with ctx_is_generic_table = true } e in
                (VariableMap.add var true acc, new_ctx, p_vars)
            | IndexTable es ->
                let new_ctx =
                  IndexMap.fold
                    (fun _ e ctx -> typecheck_top_down { ctx with ctx_is_generic_table = false } e)
                    es ctx
                in
                let undefined_indexes =
                  determine_def_complete_cover var size
                    (List.map (fun (x, e) -> (x, Pos.get_position e)) (IndexMap.bindings es))
                in
                if List.length undefined_indexes = 0 then
                  (VariableMap.add var true acc, new_ctx, p_vars)
                else
                  let previous_var_def =
                    Ast_to_mvg.get_var_from_name p.program_idmap var.Variable.name
                      var.Variable.execution_number false
                  in
                  let new_es =
                    List.fold_left
                      (fun es undef_index ->
                        Mvg.IndexMap.add undef_index
                          (Pos.same_pos_as
                             (Mvg.Index
                                ( Pos.same_pos_as previous_var_def var.Mvg.Variable.name,
                                  Pos.same_pos_as
                                    (Mvg.Literal (Float (float_of_int undef_index)))
                                    var.Mvg.Variable.name ))
                             var.Mvg.Variable.name)
                          es)
                      es undefined_indexes
                  in
                  ( VariableMap.add var true acc,
                    new_ctx,
                    Mvg.VariableMap.add var
                      { def with Mvg.var_definition = Mvg.TableVar (size, Mvg.IndexTable new_es) }
                      p_vars ) )
        | InputVar ->
            if VariableMap.mem var acc then (acc, ctx, p_vars)
            else (VariableMap.add var false acc, ctx, p_vars))
      p.program_vars
      (Mvg.VariableMap.empty, { ctx_program = p; ctx_is_generic_table = false }, p.program_vars)
  in
  let _ = typecheck_program_conds ctx p.program_conds in
  ({ table_info_var = are_tables }, { p with program_vars = p_vars })
