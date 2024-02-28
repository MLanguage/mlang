(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

open Mir

let is_table_var var =
  match var.Variable.is_table with Some _ -> true | None -> false

let rec typecheck_top_down ~(in_generic_table : bool)
    (e : expression Pos.marked) : unit =
  match Pos.unmark e with
  | Comparison (_, e1, e2) ->
      typecheck_top_down ~in_generic_table e1;
      typecheck_top_down ~in_generic_table e2
  | Binop (((Mast.And | Mast.Or), _), e1, e2)
  | Binop (((Mast.Add | Mast.Sub | Mast.Mul | Mast.Div), _), e1, e2) ->
      typecheck_top_down ~in_generic_table e1;
      typecheck_top_down ~in_generic_table e2
  | Unop (Mast.Not, e) | Unop (Mast.Minus, e) ->
      typecheck_top_down ~in_generic_table e
  | Conditional (e1, e2, e3) ->
      typecheck_top_down ~in_generic_table e1;
      typecheck_top_down ~in_generic_table e2;
      typecheck_top_down ~in_generic_table e3
  | FunctionCall (func, args) ->
      let typechecker = typecheck_func_args func (Pos.get_position e) in
      typechecker in_generic_table args
  | Literal (Float _) -> ()
  | Literal Undefined ->
      (* Literal has all the types *)
      ()
  | Var _ -> () (* All variables are real *)
  | LocalLet (_local_var, e1, e2) ->
      typecheck_top_down ~in_generic_table e1;
      typecheck_top_down ~in_generic_table e2
  | Error | LocalVar _ | NbCategory _ | Attribut _ | Size _ | NbAnomalies
  | NbDiscordances | NbInformatives | NbBloquantes ->
      ()
  | Index ((var, var_pos), e') ->
      (* Tables are only tables of arrays *)
      typecheck_top_down ~in_generic_table e';
      if not (is_table_var var) then
        Errors.raise_multispanned_error
          (Format.asprintf
             "variable %s is accessed as a table but it is not defined as one"
             (Pos.unmark var.Variable.name))
          [
            (Some "variable access", var_pos);
            (Some "variable definition", Pos.get_position var.Variable.name);
          ]

and typecheck_func_args (f : func) (pos : Pos.t) (in_generic_table : bool)
    (args : Mir.expression Pos.marked list) =
  match f with
  | SumFunc | MinFunc | MaxFunc ->
      if List.length args = 0 then
        Errors.raise_spanned_error
          "sum function must be called with at least one argument" pos
      else begin
        typecheck_top_down ~in_generic_table (List.hd args);
        List.iter (typecheck_top_down ~in_generic_table) args
      end
  | AbsFunc -> (
      match args with
      | [ arg ] -> typecheck_top_down ~in_generic_table arg
      | _ ->
          Errors.raise_spanned_error
            "function abs should have only one argument" pos)
  | PresentFunc | NullFunc | GtzFunc | GtezFunc | Supzero -> (
      (* These functions return a integer value encoding a boolean; 0 for false
         and 1 for true *)
      match args with
      | [ arg ] -> typecheck_top_down ~in_generic_table arg
      | _ ->
          Errors.raise_spanned_error "function should have only one argument"
            pos)
  | ArrFunc | InfFunc -> (
      match args with
      | [ arg ] -> typecheck_top_down ~in_generic_table arg
      | _ ->
          Errors.raise_spanned_error "function should have only one argument"
            pos)
  | Mir.Multimax -> (
      match args with
      | [ bound; table ] ->
          typecheck_top_down ~in_generic_table bound;
          typecheck_top_down ~in_generic_table table
      | _ ->
          Errors.raise_spanned_error "function %a should have two arguments" pos
      )
  | Mir.VerifNumber ->
      if List.length args <> 0 then
        Errors.raise_spanned_error "numero_verif function takes no argument" pos
  | Mir.ComplNumber ->
      if List.length args <> 0 then
        Errors.raise_spanned_error "numero_compl function takes no argument" pos

let rec expand_functions_expr (e : 'var expression_ Pos.marked) :
    'var expression_ Pos.marked =
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
               Binop
                 ( Pos.same_pos_as Mast.Add e,
                   Pos.same_pos_as acc e,
                   expand_functions_expr arg ))
           Error args)
        e
  | FunctionCall (GtzFunc, [ arg ]) ->
      Pos.same_pos_as
        (Comparison
           ( Pos.same_pos_as Mast.Gt e,
             expand_functions_expr arg,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FunctionCall (GtezFunc, [ arg ]) ->
      Pos.same_pos_as
        (Comparison
           ( Pos.same_pos_as Mast.Gte e,
             expand_functions_expr arg,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FunctionCall (((MinFunc | MaxFunc) as f), [ arg1; arg2 ]) ->
      let earg1 = expand_functions_expr arg1 in
      let earg2 = expand_functions_expr arg2 in
      Pos.same_pos_as (FunctionCall (f, [ earg1; earg2 ])) e
  | FunctionCall (AbsFunc, [ arg ]) ->
      Pos.same_pos_as (FunctionCall (AbsFunc, [ expand_functions_expr arg ])) e
  | FunctionCall (NullFunc, [ arg ]) ->
      Pos.same_pos_as
        (Comparison
           ( Pos.same_pos_as Mast.Eq e,
             expand_functions_expr arg,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FunctionCall (PresentFunc, [ arg ]) ->
      (* we do not expand this function as it deals specifically with undefined
         variables *)
      Pos.same_pos_as
        (FunctionCall (PresentFunc, [ expand_functions_expr arg ]))
        e
  | FunctionCall (ArrFunc, [ arg ]) ->
      (* we do not expand this function as it requires modulo or modf *)
      Pos.same_pos_as (FunctionCall (ArrFunc, [ expand_functions_expr arg ])) e
  | FunctionCall (InfFunc, [ arg ]) ->
      (* we do not expand this function as it requires modulo or modf *)
      Pos.same_pos_as (FunctionCall (InfFunc, [ expand_functions_expr arg ])) e
  | _ -> e

let expand_functions (p : Mir.program) : Mir.program =
  let map_var _var def =
    match def.var_definition with
    | InputVar -> def
    | SimpleVar e ->
        { def with var_definition = SimpleVar (expand_functions_expr e) }
    | TableVar (size, defg) -> (
        match defg with
        | IndexGeneric (v, e) ->
            {
              def with
              var_definition =
                TableVar (size, IndexGeneric (v, expand_functions_expr e));
            }
        | IndexTable es ->
            {
              def with
              var_definition =
                TableVar
                  ( size,
                    IndexTable
                      (IndexMap.map (fun e -> expand_functions_expr e) es) );
            })
  in
  let program_targets =
    let rec map_instr m_instr =
      let instr, instr_pos = m_instr in
      match instr with
      | Affectation (v_id, v_data) ->
          (Affectation (v_id, map_var v_id v_data), instr_pos)
      | IfThenElse (i, t, e) ->
          let i' = Pos.unmark (expand_functions_expr (i, Pos.no_pos)) in
          let t' = List.map map_instr t in
          let e' = List.map map_instr e in
          (IfThenElse (i', t', e'), instr_pos)
      | ComputeTarget _ -> m_instr
      | VerifBlock instrs ->
          let instrs' = List.map map_instr instrs in
          (VerifBlock instrs', instr_pos)
      | Print (out, pr_args) ->
          let pr_args' =
            List.map
              (fun m_arg ->
                let arg, arg_pos = m_arg in
                match arg with
                | PrintIndent e ->
                    let e' = expand_functions_expr e in
                    (PrintIndent e', arg_pos)
                | PrintExpr (e, mi, ma) ->
                    let e' = expand_functions_expr e in
                    (PrintExpr (e', mi, ma), arg_pos)
                | PrintString _ | PrintName _ | PrintAlias _ -> m_arg)
              pr_args
          in
          (Print (out, pr_args'), instr_pos)
      | Iterate (v_id, cats, e, instrs) ->
          let e' = expand_functions_expr e in
          let instrs' = List.map map_instr instrs in
          (Iterate (v_id, cats, e', instrs'), instr_pos)
      | Restore (vars, filters, instrs) ->
          let filters' =
            List.map
              (fun (v, cs, e) -> (v, cs, expand_functions_expr e))
              filters
          in
          let instrs' = List.map map_instr instrs in
          (Restore (vars, filters', instrs'), instr_pos)
      | RaiseError _ | CleanErrors | ExportErrors | FinalizeErrors -> m_instr
    in
    TargetMap.map
      (fun t ->
        let target_prog = List.map map_instr t.target_prog in
        { t with target_prog })
      p.program_targets
  in
  { p with program_targets }
