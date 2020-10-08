(* Copyright (C) 2020 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Oir

type partial_expr = PartialLiteral of Mir.literal

let partial_to_expr (e : partial_expr) : Mir.expression =
  match e with PartialLiteral l -> Literal l

let expr_to_partial (e : Mir.expression) : partial_expr option =
  match e with Literal l -> Some (PartialLiteral l) | _ -> None

type var_literal = SimpleVar of partial_expr | TableVar of int * partial_expr array

type partial_ev_ctx = {
  ctx_local_vars : partial_expr Mir.LocalVariableMap.t;
  ctx_vars : var_literal option BlockMap.t Mir.VariableMap.t;
      (** The option at the leaves of the [BlockMap.t] is to account for definitions that are not
          literals but that are to be taken into account for validity of inlining later *)
  ctx_doms : Dominators.dom;
  ctx_paths : Paths.path_checker;
  ctx_entry_block : block_id;
  ctx_inside_block : block_id option;
}

let empty_ctx (g : CFG.t) (entry_block : block_id) =
  {
    ctx_local_vars = Mir.LocalVariableMap.empty;
    ctx_vars = Mir.VariableMap.empty;
    ctx_doms = Dominators.idom_to_dom (Dominators.compute_idom g entry_block);
    ctx_paths = Paths.create g;
    ctx_inside_block = None;
    ctx_entry_block = entry_block;
  }

let reset_ctx (ctx : partial_ev_ctx) (block_id : block_id) =
  { ctx with ctx_local_vars = Mir.LocalVariableMap.empty; ctx_inside_block = Some block_id }

let compare_for_min_dom (dom : Dominators.dom) (id1 : block_id) (id2 : block_id) : int =
  (* the sort puts smaller items first, and we want the first item to be the block that is less
     dominating. So if id1 dominates id2, we want id2 to be smaller hence returning a positive value *)
  if dom id1 id2 then 1 else if dom id2 id1 then -1 else 0

let add_var_def_to_ctx (ctx : partial_ev_ctx) (block_id : block_id) (var : Mir.Variable.t)
    (var_lit : var_literal option) : partial_ev_ctx =
  {
    ctx with
    ctx_vars =
      Mir.VariableMap.add var
        ( match Mir.VariableMap.find_opt var ctx.ctx_vars with
        | None -> BlockMap.singleton block_id var_lit
        | Some defs -> BlockMap.add block_id var_lit defs )
        ctx.ctx_vars;
  }

let get_closest_dominating_def (var : Mir.Variable.t) (ctx : partial_ev_ctx) :
    (block_id * var_literal) option =
  let curr_block = Option.get ctx.ctx_inside_block in
  match Mir.VariableMap.find_opt var ctx.ctx_vars with
  | None -> None
  | Some defs -> (
      let previous_defs = Mir.VariableMap.find var ctx.ctx_vars in
      let dominating_defs =
        BlockMap.bindings
          (BlockMap.filter
             (fun def_block_id def ->
               Option.is_some def
               (* we only consider definitions that were partially evaluated to a literal *)
               && ( def_block_id = Option.get ctx.ctx_inside_block
                  || ctx.ctx_doms def_block_id curr_block ))
             previous_defs)
      in
      let sorted_dominating_defs =
        List.sort (fun (b1, _) (b2, _) -> compare_for_min_dom ctx.ctx_doms b1 b2) dominating_defs
      in
      match sorted_dominating_defs with
      | [] -> None
      | (_, None) :: _ -> assert false (* should not happen *)
      | (def_block, Some def) :: _ ->
          (* Now we have the closest def in a block that dominates the current block. But something
             could go wrong in the case where a branch of an if in between the current block and the
             def block redefines the variable. So we have to check that they are no such defs *)
          if def_block = Option.get ctx.ctx_inside_block then Some (def_block, def)
          else
            let exists_other_def_in_between =
              BlockMap.exists
                (fun intermediate_block _ ->
                  if intermediate_block = def_block || intermediate_block = curr_block then false
                  else
                    Paths.check_path ctx.ctx_paths def_block intermediate_block
                    && Paths.check_path ctx.ctx_paths intermediate_block curr_block)
                defs
            in
            if exists_other_def_in_between then None else Some (def_block, def) )

let interpreter_ctx_from_partial_ev_ctx (ctx : partial_ev_ctx) : Bir_interpreter.ctx =
  {
    Bir_interpreter.empty_ctx with
    Bir_interpreter.ctx_vars =
      Mir.VariableMap.map Option.get
        (Mir.VariableMap.filter
           (fun _ x -> Option.is_some x)
           (Mir.VariableMap.mapi
              (fun var _ ->
                match get_closest_dominating_def var ctx with
                | Some (_, SimpleVar (PartialLiteral l)) -> Some (Bir_interpreter.SimpleVar l)
                | _ -> None)
              ctx.ctx_vars));
  }

let rec partially_evaluate_expr (ctx : partial_ev_ctx) (p : Mir.program)
    (e : Mir.expression Pos.marked) : Mir.expression Pos.marked =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let new_e1 = partially_evaluate_expr ctx p e1 in
      let new_e2 = partially_evaluate_expr ctx p e2 in
      Pos.same_pos_as
        begin
          match (Pos.unmark new_e1, Pos.unmark new_e2) with
          | Literal Undefined, _ | _, Literal Undefined -> Mir.Literal Undefined
          | Literal _, Literal _ ->
              Mir.Literal
                (Bir_interpreter.evaluate_expr Bir_interpreter.empty_ctx p
                   (Pos.same_pos_as (Mir.Comparison (op, new_e1, new_e2)) e))
          | _ -> Comparison (op, new_e1, new_e2)
        end
        e
  | Binop (op, e1, e2) ->
      let new_e1 = partially_evaluate_expr ctx p e1 in
      let new_e2 = partially_evaluate_expr ctx p e2 in
      Pos.same_pos_as
        begin
          match (Pos.unmark op, Pos.unmark new_e1, Pos.unmark new_e2) with
          (* calling the interpreter when verything is a literal *)
          | _, Literal _, Literal _ ->
              Mir.Literal
                (Bir_interpreter.evaluate_expr Bir_interpreter.empty_ctx p
                   (Pos.same_pos_as (Mir.Binop (op, new_e1, new_e2)) e1))
              (* first all the combinations giving undefined *)
          | Mast.And, Literal Undefined, _ -> Mir.Literal Undefined
          | Mast.And, _, Literal Undefined -> Mir.Literal Undefined
          | Mast.Or, Literal Undefined, _ -> Mir.Literal Undefined
          | Mast.Or, _, Literal Undefined -> Mir.Literal Undefined
          | Mast.Mul, _, Literal Undefined -> Mir.Literal Undefined
          | Mast.Mul, Literal Undefined, _ -> Mir.Literal Undefined
          | Mast.Div, Literal Undefined, _ -> Mir.Literal Undefined
          | Mast.Div, _, Literal Undefined -> Mir.Literal Undefined
          (* logical or *)
          | Mast.Or, Literal (Float f), _ when f <> 0. -> Mir.Literal Mir.true_literal
          | Mast.Or, _, Literal (Float f) when f <> 0. -> Literal Mir.true_literal
          | Mast.Or, Literal (Float 0.), e' | Mast.Or, e', Literal (Float 0.) -> e'
          (* logican and *)
          | Mast.And, Literal (Float 0.), _ -> Literal Mir.false_literal
          | Mast.And, _, Literal (Float 0.) -> Literal Mir.false_literal
          | Mast.And, Literal (Float f), e' when f <> 0. -> e'
          | Mast.And, e', Literal (Float f) when f <> 0. -> e'
          (* addition *)
          | Mast.Add, Literal Undefined, e' -> e'
          | Mast.Add, e', Literal Undefined -> e'
          | Mast.Add, _, Literal (Float f) when f < 0. ->
              Binop (Pos.same_pos_as Mast.Sub op, e1, Pos.same_pos_as (Mir.Literal (Float (-.f))) e2)
          | Mast.Add, _, Unop (Minus, e2') -> Binop (Pos.same_pos_as Mast.Sub op, new_e1, e2')
          (* substraction *)
          | Mast.Sub, e1, e2 when e1 = e2 && e1 <> Literal Undefined && e2 <> Literal Undefined ->
              Literal (Float 0.)
          | Mast.Sub, Literal Undefined, e' -> Unop (Minus, Pos.same_pos_as e' e)
          | Mast.Sub, e', Literal Undefined -> e'
          (* multiplication *)
          | Mast.Mul, Literal (Float 1.), e' -> e'
          | Mast.Mul, e', Literal (Float 1.) -> e'
          (* we can't optimize float multiplication by 0 here in the general case, because
             Undefined. But if we know that the other term can't be undefined, then we can go ! This
             is the case for the result of some functions *)
          | ( Mast.Mul,
              Literal (Float 0.),
              Mir.FunctionCall ((Mir.MinFunc | Mir.MaxFunc | Mir.PresentFunc), _) )
          | ( Mast.Mul,
              Mir.FunctionCall ((Mir.MinFunc | Mir.MaxFunc | Mir.PresentFunc), _),
              Literal (Float 0.) ) ->
              Literal (Float 0.)
          (* division *)
          | Mast.Div, e', Literal (Float 1.) -> e'
          | Mast.Div, Literal (Float 0.), _ -> Mir.Literal (Mir.Float 0.)
          (* default case *)
          | _ -> Binop (op, new_e1, new_e2)
        end
        e
  | Unop (op, e1) ->
      let new_e1 = partially_evaluate_expr ctx p e1 in
      Pos.same_pos_as
        begin
          match Pos.unmark new_e1 with
          | Literal _ ->
              Mir.Literal
                (Bir_interpreter.evaluate_expr Bir_interpreter.empty_ctx p
                   (Pos.same_pos_as (Mir.Unop (op, new_e1)) e1))
          | _ -> Unop (op, new_e1)
        end
        e
  | Conditional (e1, e2, e3) -> (
      let new_e1 = partially_evaluate_expr ctx p e1 in
      let new_e2 = partially_evaluate_expr ctx p e2 in
      let new_e3 = partially_evaluate_expr ctx p e3 in
      match Pos.unmark new_e1 with
      | Literal (Float 0.) -> new_e3
      | Literal (Float _) -> new_e2
      | Literal Undefined -> Pos.same_pos_as (Mir.Literal Undefined) e
      | _ -> Pos.same_pos_as (Mir.Conditional (new_e1, new_e2, new_e3)) e )
  | Index (var, e1) -> (
      let new_e1 = partially_evaluate_expr ctx p e1 in
      match Pos.unmark new_e1 with
      | Literal Undefined -> Pos.same_pos_as (Mir.Literal Undefined) e
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
                  let err, ctx =
                    ( Bir_interpreter.FloatIndex
                        (Format.asprintf "%a" Pos.format_position (Pos.get_position e1)),
                      interpreter_ctx_from_partial_ev_ctx ctx )
                  in
                  if !Bir_interpreter.exit_on_rte then
                    Bir_interpreter.raise_runtime_as_structured err ctx p
                  else raise (Bir_interpreter.RuntimeError (err, ctx))
          in
          match get_closest_dominating_def (Pos.unmark var) ctx with
          | Some (_, SimpleVar _) -> assert false (* should not happen *)
          | Some (_, TableVar (size, es')) -> (
              if idx >= size || idx < 0 then Pos.same_pos_as (Mir.Literal Undefined) e
              else match es'.(idx) with PartialLiteral e' -> Pos.same_pos_as (Mir.Literal e') e )
          | None -> Pos.same_pos_as (Mir.Index (var, new_e1)) e )
      | _ -> Pos.same_pos_as (Mir.Index (var, new_e1)) e )
  | Literal _ -> e
  | Var var -> (
      match get_closest_dominating_def var ctx with
      | Some (_, SimpleVar (PartialLiteral e')) -> Pos.same_pos_as (Mir.Literal e') e
      | Some (_, TableVar _) -> e (* this case happens when calling functions like "multimax" *)
      | _ -> e )
  | LocalVar lvar -> (
      try Pos.same_pos_as (partial_to_expr (Mir.LocalVariableMap.find lvar ctx.ctx_local_vars)) e
      with Not_found -> e )
  | GenericTableIndex -> e
  | Error -> e
  (* let l1 = b in l2 *)
  | LocalLet (l1, b, (LocalVar l2, _)) when Mir.LocalVariable.compare l1 l2 = 0 -> b
  | LocalLet (l1, b, (Comparison (c, (LocalVar l2, _), e), pos))
    when Mir.LocalVariable.compare l1 l2 = 0 ->
      (Comparison (c, b, e), pos)
  | LocalLet (lvar, e1, e2) -> (
      let new_e1 = partially_evaluate_expr ctx p e1 in
      match Pos.unmark new_e1 with
      | Literal _ ->
          let new_ctx =
            {
              ctx with
              ctx_local_vars =
                Mir.LocalVariableMap.add lvar
                  (Option.get (expr_to_partial (Pos.unmark new_e1)))
                  ctx.ctx_local_vars;
            }
          in
          let new_e2 = partially_evaluate_expr new_ctx p e2 in
          new_e2
      | _ -> (
          let new_e2 = partially_evaluate_expr ctx p e2 in
          match Pos.unmark new_e2 with
          | Literal _ -> new_e2
          | _ -> Pos.same_pos_as (Mir.LocalLet (lvar, new_e1, new_e2)) e ) )
  | FunctionCall (func, args) ->
      let new_args = List.map (fun arg -> partially_evaluate_expr ctx p arg) args in
      let all_args_literal =
        List.for_all
          (fun arg -> match Pos.unmark arg with Mir.Literal _ -> true | _ -> false)
          new_args
      in
      let new_e = Pos.same_pos_as (Mir.FunctionCall (func, new_args)) e in
      if all_args_literal then
        Pos.same_pos_as
          (Mir.Literal (Bir_interpreter.evaluate_expr Bir_interpreter.empty_ctx p new_e))
          e
      else new_e

let partially_evaluate_stmt (stmt : stmt) (block_id : block_id) (ctx : partial_ev_ctx)
    (new_block : stmt list) (p : program) : stmt list * partial_ev_ctx =
  let ctx = reset_ctx ctx block_id in
  match Pos.unmark stmt with
  | SAssign (var, def) ->
      let new_def, new_ctx =
        match def.var_definition with
        | InputVar -> (Mir.InputVar, ctx)
        | SimpleVar e ->
            let e' = partially_evaluate_expr ctx p.mir_program e in
            let partial_e' = Option.map (fun x -> SimpleVar x) (expr_to_partial (Pos.unmark e')) in
            (SimpleVar e', add_var_def_to_ctx ctx block_id var partial_e')
        | TableVar (size, def) -> (
            match def with
            | IndexGeneric e ->
                let e' = partially_evaluate_expr ctx p.mir_program e in
                let partial_e' =
                  Option.map
                    (fun x -> TableVar (size, Array.init size (fun _ -> x)))
                    (expr_to_partial (Pos.unmark e'))
                in
                (TableVar (size, IndexGeneric e'), add_var_def_to_ctx ctx block_id var partial_e')
            | IndexTable es ->
                let es' =
                  Mir.IndexMap.mapi (fun _ e -> partially_evaluate_expr ctx p.mir_program e) es
                in
                let new_ctx =
                  if
                    Mir.IndexMap.for_all
                      (fun _ e -> Option.is_some (expr_to_partial (Pos.unmark e)))
                      es'
                  then
                    add_var_def_to_ctx ctx block_id var
                      (Some
                         (TableVar
                            ( size,
                              Array.init size (fun i ->
                                  Option.get
                                    (expr_to_partial (Pos.unmark (Mir.IndexMap.find i es')))) )))
                  else add_var_def_to_ctx ctx block_id var None
                in
                (TableVar (size, IndexTable es'), new_ctx) )
      in
      let new_stmt = Pos.same_pos_as (SAssign (var, { def with var_definition = new_def })) stmt in
      (new_stmt :: new_block, new_ctx)
  | SConditional (e, b1, b2, join) -> (
      let new_e = partially_evaluate_expr ctx p.mir_program (e, Pos.no_pos) in
      match expr_to_partial (Pos.unmark new_e) with
      | Some (PartialLiteral (Float 0.0)) -> (Pos.same_pos_as (SGoto b2) stmt :: new_block, ctx)
      | Some (PartialLiteral (Float _)) -> (Pos.same_pos_as (SGoto b1) stmt :: new_block, ctx)
      | Some (PartialLiteral Undefined) -> (Pos.same_pos_as (SGoto join) stmt :: new_block, ctx)
      | _ -> (Pos.same_pos_as (SConditional (Pos.unmark new_e, b1, b2, join)) stmt :: new_block, ctx)
      )
  | SVerif cond -> (
      let new_e = partially_evaluate_expr ctx p.mir_program cond.cond_expr in
      match expr_to_partial (Pos.unmark new_e) with
      | Some (PartialLiteral (Undefined | Float 0.0)) -> (new_block, ctx)
      | Some (PartialLiteral (Float _)) ->
          Cli.error_print "Error during partial evaluation!";
          let err, ctx =
            ( Bir_interpreter.ConditionViolated (cond.cond_errors, cond.cond_expr, []),
              interpreter_ctx_from_partial_ev_ctx ctx )
          in
          if !Bir_interpreter.exit_on_rte then
            Bir_interpreter.raise_runtime_as_structured err ctx p.mir_program
          else raise (Bir_interpreter.RuntimeError (err, ctx))
      | _ -> (Pos.same_pos_as (SVerif { cond with cond_expr = new_e }) stmt :: new_block, ctx) )
  | _ -> (stmt :: new_block, ctx)

let partial_evaluation (p : program) : program =
  let g = get_cfg p in
  let p, _ =
    Topological.fold
      (fun (block_id : block_id) (p, ctx) ->
        let block = BlockMap.find block_id p.blocks in
        let new_block, ctx =
          List.fold_left
            (fun (new_block, ctx) stmt -> partially_evaluate_stmt stmt block_id ctx new_block p)
            ([], ctx) block
        in
        ({ p with blocks = BlockMap.add block_id (List.rev new_block) p.blocks }, ctx))
      g
      (p, empty_ctx g p.entry_block)
  in
  p
