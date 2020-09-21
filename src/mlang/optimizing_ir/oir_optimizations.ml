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

(** {2 Dead code removal}*)

let remove_dead_statements (stmts : block) (used_vars : unit Mir.VariableMap.t)
    (outputs : unit Mir.VariableMap.t) : unit Mir.VariableMap.t * stmt list =
  List.fold_right
    (fun stmt (used_vars, acc) ->
      match Pos.unmark stmt with
      | SAssign (var, var_def) ->
          if Mir.VariableMap.mem var used_vars || Mir.VariableMap.mem var outputs then
            ( begin
                match var_def.Mir.var_definition with
                | Mir.SimpleVar e -> Mir_dependency_graph.get_used_variables e used_vars
                | Mir.TableVar (_, def) -> (
                    match def with
                    | Mir.IndexGeneric e -> Mir_dependency_graph.get_used_variables e used_vars
                    | Mir.IndexTable es ->
                        Mir.IndexMap.fold
                          (fun _ e used_vars -> Mir_dependency_graph.get_used_variables e used_vars)
                          es used_vars )
                | Mir.InputVar -> assert false (* should not happen *)
              end,
              stmt :: acc )
          else (used_vars, acc)
      | SVerif cond ->
          (Mir_dependency_graph.get_used_variables cond.cond_expr used_vars, stmt :: acc)
      | SConditional (cond, _, _, _) ->
          (Mir_dependency_graph.get_used_variables (cond, Pos.no_pos) used_vars, stmt :: acc)
      | SGoto _ -> (used_vars, stmt :: acc))
    stmts (used_vars, [])

let remove_empty_conditionals (p : program) : program =
  let g = get_cfg p in
  let empty_cond_branches : block_id list =
    List.map fst
      (BlockMap.bindings
         (BlockMap.filter
            (fun _ block -> match block with [ (SGoto _, _) ] -> true | _ -> false)
            p.blocks))
  in
  let rev_topological_order = Topological.fold (fun id acc -> id :: acc) g [] in
  let new_blocks =
    List.fold_left
      (fun (new_blocks : block BlockMap.t) (id : block_id) ->
        let old_block = BlockMap.find id new_blocks in
        let new_block =
          try
            match List.hd (List.rev old_block) with
            | SConditional (_, b1, b2, join), pos ->
                if List.mem b1 empty_cond_branches && List.mem b2 empty_cond_branches then
                  List.rev ((SGoto join, pos) :: List.tl (List.rev old_block))
                else old_block
            | _ -> old_block
          with Failure s when s = "hd" -> old_block
        in
        BlockMap.add id new_block new_blocks)
      p.blocks rev_topological_order
  in
  { p with blocks = new_blocks }

let dead_code_removal (p : program) : program =
  let g = get_cfg p in
  let rev_topological_order = Topological.fold (fun id acc -> id :: acc) g [] in
  let is_entry block_id = block_id = p.entry_block in
  let is_reachable = Reachability.analyze is_entry g in
  let p = { p with blocks = BlockMap.filter (fun bid _ -> is_reachable bid) p.blocks } in
  let _, p =
    List.fold_left
      (fun (used_vars, p) block_id ->
        try
          let block = BlockMap.find block_id p.blocks in
          let used_vars, block = remove_dead_statements block used_vars p.outputs in
          let p = { p with blocks = BlockMap.add block_id block p.blocks } in
          (used_vars, p)
        with Not_found -> (used_vars, p))
      (Mir.VariableMap.empty, p) rev_topological_order
  in
  let p = remove_empty_conditionals p in
  p

(** {2 Partial evaluation} *)

type partial_expr = PartialLiteral of Mir.literal | PartialVar of Mir.Variable.t

let partial_to_expr (e : partial_expr) : Mir.expression =
  match e with PartialLiteral l -> Literal l | PartialVar v -> Var v

let expr_to_partial (e : Mir.expression) : partial_expr option =
  match e with Literal l -> Some (PartialLiteral l) | Var v -> Some (PartialVar v) | _ -> None

type var_literal = SimpleVar of partial_expr | TableVar of int * partial_expr array

type partial_ev_ctx = {
  ctx_local_vars : partial_expr Mir.LocalVariableMap.t;
  ctx_vars : var_literal BlockMap.t Mir.VariableMap.t;
  ctx_inside_table_index : int option;
  ctx_inside_var : Mir.Variable.t option;
  ctx_doms : Dominators.dom;
  ctx_inside_block : block_id option;
}

let empty_ctx (g : CFG.t) (entry_block : block_id) =
  {
    ctx_local_vars = Mir.LocalVariableMap.empty;
    ctx_vars = Mir.VariableMap.empty;
    ctx_inside_table_index = None;
    ctx_inside_var = None;
    ctx_doms = Dominators.idom_to_dom (Dominators.compute_idom g entry_block);
    ctx_inside_block = None;
  }

let reset_ctx (ctx : partial_ev_ctx) (block_id : block_id) (var : Mir.Variable.t option)
    (idx : int option) =
  {
    ctx with
    ctx_local_vars = Mir.LocalVariableMap.empty;
    ctx_inside_table_index = idx;
    ctx_inside_var = var;
    ctx_inside_block = Some block_id;
  }

let compare_for_min_dom (dom : Dominators.dom) (id1 : block_id) (id2 : block_id) : int =
  if dom id1 id2 then (* id1 dominates id2 so we want to pick id2 *)
    -1
  else if dom id2 id1 then 1
  else 0

let add_var_def_to_ctx (ctx : partial_ev_ctx) (block_id : block_id) (var : Mir.Variable.t)
    (var_lit : var_literal) : partial_ev_ctx =
  {
    ctx with
    ctx_vars =
      Mir.VariableMap.add var
        ( match Mir.VariableMap.find_opt var ctx.ctx_vars with
        | None -> BlockMap.singleton block_id var_lit
        | Some defs -> BlockMap.add block_id var_lit defs )
        ctx.ctx_vars;
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
          | Conditional (b, (Literal (Float 1.), _), (Literal (Float 0.), _)), Literal (Float 1.)
            when Pos.unmark op = Mast.Eq ->
              Pos.unmark b
          | (FunctionCall (Mir.PresentFunc, _) as fc), Literal (Float 1.)
            when Pos.unmark op = Mast.Neq ->
              Unop (Mast.Not, Pos.same_pos_as fc new_e1)
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
          | Mast.And, Literal Undefined, _
          | Mast.And, _, Literal Undefined
          | Mast.Or, Literal Undefined, _
          | Mast.Or, _, Literal Undefined
          | Mast.Div, _, Literal Undefined ->
              Mir.Literal Undefined
          | Mast.Or, Literal (Float f), _ when f <> 0. -> Mir.Literal Mir.true_literal
          | Mast.Or, _, Literal (Float f) when f <> 0. -> Literal Mir.true_literal
          | Mast.And, Literal (Float 0.), _ | Mast.And, _, Literal (Float 0.) ->
              Literal Mir.false_literal
          | Mast.And, Literal (Float f), e' when f <> 0. -> e'
          | Mast.And, e', Literal (Float f) when f <> 0. -> e'
          | Mast.Or, Literal (Float 0.), e'
          | Mast.Or, e', Literal (Float 0.)
          | Mast.Add, Literal (Float 0. | Undefined), e'
          | Mast.Add, e', Literal (Float 0. | Undefined)
          | Mast.Mul, Literal (Float 1.), e'
          | Mast.Mul, e', Literal (Float 1.)
          | Mast.Div, e', Literal (Float 1.)
          | Mast.Sub, e', Literal (Float 0. | Undefined) ->
              e'
          | Mast.Sub, Literal (Float 0. | Undefined), e' -> Unop (Minus, Pos.same_pos_as e' e)
          | Mast.Mul, Literal (Float 0. | Undefined), _
          | Mast.Mul, _, Literal (Float 0. | Undefined)
          | Mast.Div, Literal (Float 0. | Undefined), _ ->
              Mir.Literal (Mir.Float 0.)
          | _, Literal _, Literal _ ->
              Mir.Literal
                (Bir_interpreter.evaluate_expr Bir_interpreter.empty_ctx p
                   (Pos.same_pos_as (Mir.Binop (op, new_e1, new_e2)) e1))
          | Mast.Add, _, Literal (Float f) when f < 0. ->
              Binop (Pos.same_pos_as Mast.Sub op, e1, Pos.same_pos_as (Mir.Literal (Float (-.f))) e2)
          | Mast.Add, _, Unop (Minus, e2') -> Binop (Pos.same_pos_as Mast.Sub op, e1, e2')
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
                  Bir_interpreter.raise_runtime_as_structured
                    (Bir_interpreter.FloatIndex
                       (Format.asprintf "%a" Pos.format_position (Pos.get_position e1)))
                    Bir_interpreter.empty_ctx p
          in
          try
            let previous_defs = Mir.VariableMap.find (Pos.unmark var) ctx.ctx_vars in
            let dominating_defs =
              BlockMap.bindings
                (BlockMap.filter
                   (fun def_block_id _ ->
                     def_block_id = Option.get ctx.ctx_inside_block
                     || ctx.ctx_doms def_block_id (Option.get ctx.ctx_inside_block))
                   previous_defs)
            in
            let sorted_dominating_defs =
              List.sort
                (fun (b1, _) (b2, _) -> compare_for_min_dom ctx.ctx_doms b1 b2)
                dominating_defs
            in
            match snd (List.hd sorted_dominating_defs) with
            | SimpleVar _ -> assert false (* should not happen *)
            | TableVar (size, es') -> (
                if idx >= size || idx < 0 then Pos.same_pos_as (Mir.Literal Undefined) e
                else
                  match es'.(idx) with
                  | PartialLiteral e' -> Pos.same_pos_as (Mir.Literal e') e
                  | PartialVar v' -> Pos.same_pos_as (Mir.Var v') e )
          with _ -> Pos.same_pos_as (Mir.Index (var, new_e1)) e )
      | _ -> Pos.same_pos_as (Mir.Index (var, new_e1)) e )
  | Literal _ -> e
  | Var var -> (
      try
        let previous_defs = Mir.VariableMap.find var ctx.ctx_vars in
        let dominating_defs =
          BlockMap.bindings
            (BlockMap.filter
               (fun def_block_id _ ->
                 def_block_id = Option.get ctx.ctx_inside_block
                 || ctx.ctx_doms def_block_id (Option.get ctx.ctx_inside_block))
               previous_defs)
        in
        let sorted_dominating_defs =
          List.sort (fun (b1, _) (b2, _) -> compare_for_min_dom ctx.ctx_doms b1 b2) dominating_defs
        in
        match List.hd sorted_dominating_defs with
        | _, SimpleVar (PartialLiteral e') -> Pos.same_pos_as (Mir.Literal e') e
        | _, SimpleVar (PartialVar v') -> Pos.same_pos_as (Mir.Var v') e
        | _ -> assert false
        (* should not happen *)
      with
      | Not_found -> e
      | Failure s when s = "hd" -> e )
  | LocalVar lvar -> (
      try Pos.same_pos_as (partial_to_expr (Mir.LocalVariableMap.find lvar ctx.ctx_local_vars)) e
      with Not_found -> e )
  | GenericTableIndex -> e
  | Error -> e
  | LocalLet (l1, b, (LocalVar l2, _)) when Mir.LocalVariable.compare l1 l2 = 0 -> b
  | LocalLet (lvar, e1, e2) -> (
      let new_e1 = partially_evaluate_expr ctx p e1 in
      match Pos.unmark new_e1 with
      | Literal _ | Var _ ->
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
          | Literal _ | Var _ -> new_e2
          | _ -> Pos.same_pos_as (Mir.LocalLet (lvar, new_e1, new_e2)) e ) )
  | FunctionCall (((ArrFunc | InfFunc | PresentFunc | NullFunc) as f), [ arg ]) -> (
      let new_arg = partially_evaluate_expr ctx p arg in
      match Pos.unmark new_arg with
      | Literal _ ->
          Pos.same_pos_as
            (Mir.Literal
               (Bir_interpreter.evaluate_expr Bir_interpreter.empty_ctx p
                  (Pos.same_pos_as (Mir.FunctionCall (f, [ new_arg ])) e)))
            e
      | _ -> Pos.same_pos_as (Mir.FunctionCall (f, [ new_arg ])) e )
  | FunctionCall (((MinFunc | MaxFunc) as f), [ arg1; arg2 ]) -> (
      let new_arg1 = partially_evaluate_expr ctx p arg1 in
      let new_arg2 = partially_evaluate_expr ctx p arg2 in
      match (Pos.unmark new_arg1, Pos.unmark new_arg2) with
      | Literal _, Literal _ ->
          Pos.same_pos_as
            (Mir.Literal
               (Bir_interpreter.evaluate_expr Bir_interpreter.empty_ctx p
                  (Pos.same_pos_as (Mir.FunctionCall (f, [ new_arg1; new_arg2 ])) e)))
            e
      | _ -> Pos.same_pos_as (Mir.FunctionCall (f, [ new_arg1; new_arg2 ])) e )
  | FunctionCall (func, args) ->
      let new_args = List.map (fun arg -> partially_evaluate_expr ctx p arg) args in
      let all_args_literal =
        List.for_all
          (fun arg ->
            match expr_to_partial (Pos.unmark arg) with
            | Some (PartialLiteral _) -> true
            | _ -> false)
          new_args
      in
      let new_e = Pos.same_pos_as (Mir.FunctionCall (func, new_args)) e in
      if all_args_literal then
        Pos.same_pos_as
          (Mir.Literal (Bir_interpreter.evaluate_expr Bir_interpreter.empty_ctx p new_e))
          e
      else new_e

let partial_evaluation (p : program) : program =
  let g = get_cfg p in
  let p, _ =
    Topological.fold
      (fun (block_id : block_id) (p, ctx) ->
        let block = BlockMap.find block_id p.blocks in
        let new_block, ctx =
          List.fold_left
            (fun (new_block, ctx) stmt ->
              match Pos.unmark stmt with
              | SAssign (var, def) ->
                  let new_def, new_ctx =
                    match def.var_definition with
                    | InputVar -> (Mir.InputVar, ctx)
                    | SimpleVar e -> (
                        let e' =
                          partially_evaluate_expr
                            (reset_ctx ctx block_id (Some var) None)
                            p.mir_program e
                        in
                        ( SimpleVar e',
                          match expr_to_partial (Pos.unmark e') with
                          | None -> ctx
                          | Some e' -> add_var_def_to_ctx ctx block_id var (SimpleVar e') ) )
                    | TableVar (size, def) -> (
                        match def with
                        | IndexGeneric e -> (
                            let e' =
                              partially_evaluate_expr
                                (reset_ctx ctx block_id (Some var) None)
                                p.mir_program e
                            in
                            ( TableVar (size, IndexGeneric e'),
                              match expr_to_partial (Pos.unmark e') with
                              | None -> ctx
                              | Some e' ->
                                  add_var_def_to_ctx ctx block_id var
                                    (TableVar (size, Array.init size (fun _ -> e'))) ) )
                        | IndexTable es ->
                            let es' =
                              Mir.IndexMap.mapi
                                (fun idx e ->
                                  partially_evaluate_expr
                                    (reset_ctx ctx block_id (Some var) (Some idx))
                                    p.mir_program e)
                                es
                            in
                            let new_ctx =
                              if
                                Mir.IndexMap.for_all
                                  (fun _ e -> Option.is_some (expr_to_partial (Pos.unmark e)))
                                  es'
                              then
                                add_var_def_to_ctx ctx block_id var
                                  (TableVar
                                     ( size,
                                       Array.init size (fun i ->
                                           Option.get
                                             (expr_to_partial
                                                (Pos.unmark (Mir.IndexMap.find i es')))) ))
                              else ctx
                            in
                            (TableVar (size, IndexTable es'), new_ctx) )
                  in
                  let new_stmt =
                    Pos.same_pos_as (SAssign (var, { def with var_definition = new_def })) stmt
                  in
                  (new_stmt :: new_block, new_ctx)
              | SConditional (e, b1, b2, join) -> (
                  let new_e =
                    partially_evaluate_expr
                      (reset_ctx ctx block_id None None)
                      p.mir_program (e, Pos.no_pos)
                  in
                  match expr_to_partial (Pos.unmark new_e) with
                  | Some (PartialLiteral (Float 0.0)) ->
                      (Pos.same_pos_as (SGoto b2) stmt :: new_block, ctx)
                  | Some (PartialLiteral (Float _)) ->
                      (Pos.same_pos_as (SGoto b1) stmt :: new_block, ctx)
                  | Some (PartialLiteral Undefined) ->
                      (Pos.same_pos_as (SGoto join) stmt :: new_block, ctx)
                  | _ ->
                      ( Pos.same_pos_as (SConditional (Pos.unmark new_e, b1, b2, join)) stmt
                        :: new_block,
                        ctx ) )
              | SVerif cond -> (
                  let new_e =
                    partially_evaluate_expr
                      (reset_ctx ctx block_id None None)
                      p.mir_program cond.cond_expr
                  in
                  match expr_to_partial (Pos.unmark new_e) with
                  | Some (PartialLiteral (Undefined | Float 0.0)) -> (new_block, ctx)
                  | Some (PartialLiteral (Float _)) ->
                      Bir_interpreter.raise_runtime_as_structured
                        (Bir_interpreter.ConditionViolated (cond.cond_errors, cond.cond_expr, []))
                        Bir_interpreter.empty_ctx p.mir_program
                  | _ ->
                      ( Pos.same_pos_as (SVerif { cond with cond_expr = new_e }) stmt :: new_block,
                        ctx ) )
              | _ -> (stmt :: new_block, ctx))
            ([], ctx) block
        in
        ({ p with blocks = BlockMap.add block_id (List.rev new_block) p.blocks }, ctx))
      g
      (p, empty_ctx g p.entry_block)
  in
  p

let optimize (p : program) : program =
  Cli.debug_print "Intruction count: %d" (count_instr p);
  Cli.debug_print "Dead code removal...";
  let curr_inst_count = ref (count_instr p) in
  let p = dead_code_removal p in
  let p = ref p in
  while count_instr !p < !curr_inst_count do
    curr_inst_count := count_instr !p;
    Cli.debug_print "Intruction count: %d" !curr_inst_count;
    Cli.debug_print "Partial evaluation...";
    p := partial_evaluation !p;
    p := dead_code_removal !p
  done;
  let p = !p in
  Cli.debug_print "Intruction count: %d" (count_instr p);
  p
