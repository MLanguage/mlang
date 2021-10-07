(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

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

open Mir

type ctx = { ctx_table_vars : VariableDict.t; ctx_is_generic_table : bool }

let add_table_var var ctx = { ctx with ctx_table_vars = VariableDict.add var ctx.ctx_table_vars }

let is_table_var var ctx = VariableDict.mem var ctx.ctx_table_vars

let rec typecheck_top_down (ctx : ctx) (e : expression Pos.marked) : ctx =
  match Pos.unmark e with
  | Comparison (_, e1, e2) ->
      let ctx = typecheck_top_down ctx e1 in
      let ctx = typecheck_top_down ctx e2 in
      ctx
  | Binop (((Mast.And | Mast.Or), _), e1, e2)
  | Binop (((Mast.Add | Mast.Sub | Mast.Mul | Mast.Div), _), e1, e2) ->
      let ctx = typecheck_top_down ctx e1 in
      let ctx = typecheck_top_down ctx e2 in
      ctx
  | Unop (Mast.Not, e) | Unop (Mast.Minus, e) ->
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
        Errors.raise_spanned_error "Generic table index appears outside of table"
          (Pos.get_position e)
  | Index ((var, var_pos), e') ->
      (* Tables are only tables of arrays *)
      let ctx = typecheck_top_down ctx e' in
      if is_table_var var ctx then ctx
      else
        Errors.raise_multispanned_error
          (Format.asprintf "variable %s is accessed as a table but it is not defined as one"
             (Pos.unmark var.Variable.name))
          [
            (Some "variable access", var_pos);
            (Some "variable definition", Pos.get_position var.Variable.name);
          ]

and typecheck_func_args (f : func) (pos : Pos.t) : ctx -> Mir.expression Pos.marked list -> ctx =
  match f with
  | SumFunc | MinFunc | MaxFunc ->
      fun ctx args ->
        if List.length args = 0 then
          Errors.raise_spanned_error "sum function must be called with at least one argument" pos
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
        | _ -> Errors.raise_spanned_error "function abs should have only one argument" pos)
  | PresentFunc | NullFunc | GtzFunc | GtezFunc | Supzero -> (
      fun (* These functions return a integer value encoding a boolean; 0 for false and 1 for
             true *)
            ctx args ->
        match args with
        | [ arg ] ->
            let ctx = typecheck_top_down ctx arg in
            ctx
        | _ -> Errors.raise_spanned_error "function should have only one argument" pos)
  | ArrFunc | InfFunc -> (
      fun ctx args ->
        match args with
        | [ arg ] ->
            let ctx = typecheck_top_down ctx arg in
            ctx
        | _ -> Errors.raise_spanned_error "function should have only one argument" pos)
  | Mir.Multimax -> (
      fun ctx args ->
        match args with
        | [ bound; table ] ->
            let ctx = typecheck_top_down ctx bound in
            typecheck_top_down ctx table
        | _ -> Errors.raise_spanned_error "function %a should have two arguments" pos)

let determine_def_complete_cover (table_var : Mir.Variable.t) (size : int)
    (defs : (int * Pos.t) list) : int list =
  (* Return all undefined indexes *)
  let defs_array = Array.make size false in
  List.iter
    (fun (def, def_pos) ->
      try defs_array.(def) <- true
      with Invalid_argument _ ->
        Errors.raise_multispanned_error
          (Format.asprintf "the definition of index %d, from table %s of size %d is out of bounds"
             def
             (Pos.unmark table_var.Variable.name)
             size)
          [
            (Some "index definition", def_pos);
            (Some "variable declaration", Pos.get_position table_var.Variable.name);
          ])
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
        Errors.raise_spanned_error
          (Format.asprintf "you cannot refer to the variable %s since you are defining it"
             (Pos.unmark var.Variable.name))
          (Pos.get_position e)
      else ()

let check_non_recursivity_of_variable_defs (var : Variable.t) (def : variable_def) : unit =
  match def with SimpleVar e -> check_non_recursivity_expr e var | TableVar _ | InputVar -> ()

(* The typechecker returns a new program because it defines missing table entries as "undefined" *)
let typecheck (p : Mir_interface.full_program) : Mir_interface.full_program =
  let check_var_def ctx vid def =
    let var = VariableDict.find vid p.program.program_vars in
    check_non_recursivity_of_variable_defs var def.var_definition;
    (* All top-level variables are og type Real *)
    match def.var_definition with
    | SimpleVar e ->
        let new_ctx = typecheck_top_down { ctx with ctx_is_generic_table = false } e in
        (new_ctx, def)
    | TableVar (size, defs) -> (
        match defs with
        | IndexGeneric e ->
            let new_ctx =
              typecheck_top_down { ctx with ctx_is_generic_table = true } e |> add_table_var var
            in
            (new_ctx, def)
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
            if List.length undefined_indexes = 0 then (add_table_var var new_ctx, def)
            else
              let previous_var_def =
                Mast_to_mvg.get_var_from_name p.program.program_idmap var.Variable.name
                  var.Variable.execution_number false
              in
              let new_es =
                List.fold_left
                  (fun es undef_index ->
                    Mir.IndexMap.add undef_index
                      (Pos.same_pos_as
                         (Mir.Index
                            ( Pos.same_pos_as previous_var_def var.Mir.Variable.name,
                              Pos.same_pos_as
                                (Mir.Literal (Float (float_of_int undef_index)))
                                var.Mir.Variable.name ))
                         var.Mir.Variable.name)
                      es)
                  es undefined_indexes
              in
              ( add_table_var var new_ctx,
                { def with Mir.var_definition = Mir.TableVar (size, Mir.IndexTable new_es) } ))
    | InputVar -> if is_table_var var ctx then (ctx, def) else (add_table_var var ctx, def)
  in
  let ctx, program_rules =
    List.fold_left
      (fun (ctx, rules) rule_id ->
        let rule_data = RuleMap.find rule_id p.program.program_rules in
        let ctx, rule_data =
          let ctx, rule_vars =
            List.fold_left
              (fun (ctx, vars) (vid, def) ->
                let ctx, def = check_var_def ctx vid def in
                (ctx, (vid, def) :: vars))
              (ctx, []) (List.rev rule_data.rule_vars)
          in
          (ctx, { rule_data with rule_vars })
        in
        (ctx, RuleMap.add rule_id rule_data rules))
      ({ ctx_is_generic_table = false; ctx_table_vars = VariableDict.empty }, Mir.RuleMap.empty)
      p.main_execution_order
  in
  let _ = typecheck_program_conds ctx p.program.program_conds in
  (* the typechecking modifications do not change the dependency graph *)
  { p with program = { p.program with program_rules } }

(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

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
             else
               Binop (Pos.same_pos_as Mast.Add e, Pos.same_pos_as acc e, expand_functions_expr arg))
           Error args)
        e
  | FunctionCall (GtzFunc, [ arg ]) ->
      Pos.same_pos_as
        (Conditional
           ( Pos.same_pos_as
               (Comparison
                  ( Pos.same_pos_as Mast.Gt e,
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
                  ( Pos.same_pos_as Mast.Gte e,
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
                         ( Pos.same_pos_as Mast.Lt e,
                           Pos.same_pos_as (LocalVar arg_var) e,
                           Pos.same_pos_as (Literal (Float 0.0)) e ))
                      e,
                    Pos.same_pos_as (Unop (Mast.Minus, Pos.same_pos_as (LocalVar arg_var) e)) e,
                    Pos.same_pos_as (LocalVar arg_var) e ))
               e ))
        e
  | FunctionCall (NullFunc, [ arg ]) ->
      Pos.same_pos_as
        (Conditional
           ( Pos.same_pos_as
               (Comparison
                  ( Pos.same_pos_as Mast.Eq e,
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

let expand_functions (p : Mir_interface.full_program) : Mir_interface.full_program =
  {
    (* this expansion does not modify the dependency graph *)
    p with
    program =
      (let program =
         map_vars
           (fun _var def ->
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
                     }))
           p.program
       in
       {
         program with
         program_conds =
           VariableMap.map
             (fun cond -> { cond with cond_expr = expand_functions_expr cond.cond_expr })
             p.program.program_conds;
       });
  }
