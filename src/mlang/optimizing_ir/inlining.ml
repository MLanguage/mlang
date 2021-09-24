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

type ctx = {
  ctx_vars : (Mir.variable_def * int) BlockMap.t Mir.VariableMap.t;
  (* the int is the statement number inside the block *)
  ctx_local_vars : Mir.expression Mir.LocalVariableMap.t;
  ctx_doms : Dominators.dom;
  ctx_paths : Paths.path_checker;
}

let empty_ctx (g : CFG.t) (entry_block : block_id) =
  {
    ctx_vars = Mir.VariableMap.empty;
    ctx_local_vars = Mir.LocalVariableMap.empty;
    ctx_doms = Dominators.idom_to_dom (Dominators.compute_idom g entry_block);
    ctx_paths = Paths.create g;
  }

let add_var_def_to_ctx (var : Mir.Variable.t) (def : Mir.variable_def) (current_block : block_id)
    (current_stmt_pos : int) (ctx : ctx) : ctx =
  {
    ctx with
    ctx_vars =
      Mir.VariableMap.update var
        (fun defs ->
          match defs with
          | None -> Some (BlockMap.singleton current_block (def, current_stmt_pos))
          | Some defs -> Some (BlockMap.add current_block (def, current_stmt_pos) defs))
        ctx.ctx_vars;
  }

let rec no_local_vars (e : Mir.expression Pos.marked) : bool =
  match Pos.unmark e with
  | Mir.LocalVar _ -> false
  | Mir.LocalLet _ -> false
  | Mir.Literal _ | Mir.GenericTableIndex | Mir.Error | Mir.Var _ -> true
  | Mir.Binop (_, e1, e2) | Mir.Comparison (_, e1, e2) -> no_local_vars e1 && no_local_vars e2
  | Mir.Index (_, e1) | Mir.Unop (_, e1) -> no_local_vars e1
  | Mir.FunctionCall (_, args) -> List.for_all (fun arg -> no_local_vars arg) args
  | Mir.Conditional (e1, e2, e3) -> no_local_vars e1 && no_local_vars e2 && no_local_vars e3

let rec has_this_local_var (e : Mir.expression Pos.marked) (l : Mir.LocalVariable.t) : bool =
  match Pos.unmark e with
  | Mir.LocalVar l' -> l = l'
  | Mir.LocalLet (l', e1, e2) -> l = l' || has_this_local_var e1 l || has_this_local_var e2 l
  | Mir.Literal _ | Mir.GenericTableIndex | Mir.Error | Mir.Var _ -> false
  | Mir.Binop (_, e1, e2) | Mir.Comparison (_, e1, e2) ->
      has_this_local_var e1 l || has_this_local_var e2 l
  | Mir.Index (_, e1) | Mir.Unop (_, e1) -> has_this_local_var e1 l
  | Mir.FunctionCall (_, args) -> List.exists (fun arg -> has_this_local_var arg l) args
  | Mir.Conditional (e1, e2, e3) ->
      has_this_local_var e1 l || has_this_local_var e2 l || has_this_local_var e3 l

let rec expr_size (e : Mir.expression Pos.marked) : int =
  match Pos.unmark e with
  | Mir.LocalVar _ | Mir.LocalLet _ | Mir.Literal _ | Mir.GenericTableIndex | Mir.Error | Mir.Var _
    ->
      1
  | Mir.Binop (_, e1, e2) | Mir.Comparison (_, e1, e2) -> expr_size e1 + expr_size e2 + 1
  | Mir.Index (_, e1) | Mir.Unop (_, e1) -> expr_size e1 + 1
  | Mir.FunctionCall (_, args) -> List.fold_left (fun acc arg -> acc + expr_size arg) 1 args
  | Mir.Conditional (e1, e2, e3) -> expr_size e1 + expr_size e2 + expr_size e3

let is_inlining_worthy (e : Mir.expression Pos.marked) : bool =
  (* we forbid inlining expressions with local variables to prevent conflicts of local variable
     names *)

  (* the size limit is arbitrary *)
  no_local_vars e && expr_size e < 5

(* todo: size and no local vars *)

let rec inline_in_expr (e : Mir.expression) (ctx : ctx) (current_block : block_id)
    (current_pos : int) : Mir.expression =
  match e with
  | Mir.Var var_x -> (
      match Mir.VariableMap.find_opt var_x ctx.ctx_vars with
      | Some previous_x_defs -> (
          let candidate =
            BlockMap.filter
              (fun previous_x_def_block_id (previous_x_def, previous_x_def_pos) ->
                (* first we pick dominating definitions *)
                (previous_x_def_block_id = current_block
                || ctx.ctx_doms previous_x_def_block_id current_block)
                &&
                match previous_x_def with
                | Mir.SimpleVar previous_e ->
                    is_inlining_worthy previous_e
                    &&
                    let vars_used_in_previous_x_def =
                      Mir_dependency_graph.get_used_variables previous_e
                    in
                    (* we're trying to replace the use of [var] with [previous_e]. This is valid
                       only if [var] and the variables used in [previous_e] have not been redefined
                       between [previous_def_block_id] and [current_block] *)
                    let exists_def_between_previous_x_def_and_here (var : Mir.Variable.t) : bool =
                      let var_defs =
                        match Mir.VariableMap.find_opt var ctx.ctx_vars with
                        | None -> BlockMap.empty
                        | Some defs -> defs
                      in
                      BlockMap.exists
                        (fun intermediate_block (_, intermediate_block_pos) ->
                          if intermediate_block = previous_x_def_block_id then
                            intermediate_block_pos > previous_x_def_pos
                          else if intermediate_block = current_block then
                            current_pos > intermediate_block_pos
                          else
                            Paths.check_path ctx.ctx_paths previous_x_def_block_id
                              intermediate_block
                            && Paths.check_path ctx.ctx_paths intermediate_block current_block)
                        var_defs
                    in
                    Mir.VariableDict.for_all
                      (fun var -> not (exists_def_between_previous_x_def_and_here var))
                      vars_used_in_previous_x_def
                    && not (exists_def_between_previous_x_def_and_here var_x)
                | _ -> false)
              previous_x_defs
          in
          (* at this point, [candidate] should contain at most one definition *)
          match BlockMap.choose_opt candidate with
          | Some (_, previous_def) -> (
              match fst previous_def with
              | SimpleVar previous_e -> Pos.unmark previous_e
              | _ -> assert false (* should not happen *))
          | None -> e)
      | None -> e)
  | Mir.Comparison (op, e1, e2) ->
      let new_e1 =
        Pos.same_pos_as (inline_in_expr (Pos.unmark e1) ctx current_block current_pos) e1
      in
      let new_e2 =
        Pos.same_pos_as (inline_in_expr (Pos.unmark e2) ctx current_block current_pos) e2
      in
      Mir.Comparison (op, new_e1, new_e2)
  | Mir.Binop (op, e1, e2) ->
      let new_e1 =
        Pos.same_pos_as (inline_in_expr (Pos.unmark e1) ctx current_block current_pos) e1
      in
      let new_e2 =
        Pos.same_pos_as (inline_in_expr (Pos.unmark e2) ctx current_block current_pos) e2
      in
      Mir.Binop (op, new_e1, new_e2)
  | Mir.Unop (op, e1) ->
      let new_e1 =
        Pos.same_pos_as (inline_in_expr (Pos.unmark e1) ctx current_block current_pos) e1
      in
      Mir.Unop (op, new_e1)
  | Mir.Conditional (cond, e_t, e_f) ->
      let new_cond =
        Pos.same_pos_as (inline_in_expr (Pos.unmark cond) ctx current_block current_pos) cond
      in
      let new_e_t =
        Pos.same_pos_as (inline_in_expr (Pos.unmark e_t) ctx current_block current_pos) e_t
      in
      let new_e_f =
        Pos.same_pos_as (inline_in_expr (Pos.unmark e_f) ctx current_block current_pos) e_f
      in
      Mir.Conditional (new_cond, new_e_t, new_e_f)
  | Mir.LocalLet (l, e1, e2) ->
      let new_e1 =
        Pos.same_pos_as (inline_in_expr (Pos.unmark e1) ctx current_block current_pos) e1
      in
      let ctx =
        if is_inlining_worthy new_e1 then
          {
            ctx with
            ctx_local_vars = Mir.LocalVariableMap.add l (Pos.unmark new_e1) ctx.ctx_local_vars;
          }
        else ctx
      in
      let new_e2 =
        Pos.same_pos_as (inline_in_expr (Pos.unmark e2) ctx current_block current_pos) e2
      in
      if has_this_local_var new_e2 l then Mir.LocalLet (l, new_e1, new_e2) else Pos.unmark new_e2
  | Mir.LocalVar l -> (
      match Mir.LocalVariableMap.find_opt l ctx.ctx_local_vars with None -> e | Some e' -> e')
  | Mir.Literal _ | Mir.GenericTableIndex | Mir.Error -> e
  | Mir.Index (v, e2) ->
      let new_e2 =
        Pos.same_pos_as (inline_in_expr (Pos.unmark e2) ctx current_block current_pos) e2
      in
      Mir.Index (v, new_e2)
  | Mir.FunctionCall (f, args) ->
      let new_args =
        List.map
          (fun arg ->
            Pos.same_pos_as (inline_in_expr (Pos.unmark arg) ctx current_block current_pos) arg)
          args
      in
      Mir.FunctionCall (f, new_args)

let rec inline_in_stmt (stmt : stmt) (ctx : ctx) (current_block : block_id) (current_stmt_pos : int)
    : stmt * ctx * int =
  match Pos.unmark stmt with
  | SAssign (var, data) -> (
      match data.var_definition with
      | InputVar -> (stmt, ctx, current_stmt_pos)
      | SimpleVar def ->
          let new_def = inline_in_expr (Pos.unmark def) ctx current_block current_stmt_pos in
          let new_def = Mir.SimpleVar (Pos.same_pos_as new_def def) in
          let new_stmt =
            Pos.same_pos_as (SAssign (var, { data with var_definition = new_def })) stmt
          in
          let new_ctx = add_var_def_to_ctx var new_def current_block current_stmt_pos ctx in
          (new_stmt, new_ctx, current_stmt_pos)
      | TableVar (size, defs) -> (
          match defs with
          | IndexGeneric def ->
              let new_def = inline_in_expr (Pos.unmark def) ctx current_block current_stmt_pos in
              let new_def = Mir.TableVar (size, IndexGeneric (Pos.same_pos_as new_def def)) in
              let new_stmt =
                Pos.same_pos_as (SAssign (var, { data with var_definition = new_def })) stmt
              in
              let new_ctx = add_var_def_to_ctx var new_def current_block current_stmt_pos ctx in
              (new_stmt, new_ctx, current_stmt_pos)
          | IndexTable defs ->
              let new_defs =
                Mir.IndexMap.map
                  (fun def ->
                    Pos.same_pos_as
                      (inline_in_expr (Pos.unmark def) ctx current_block current_stmt_pos)
                      def)
                  defs
              in
              let new_defs = Mir.TableVar (size, IndexTable new_defs) in
              let new_stmt =
                Pos.same_pos_as (SAssign (var, { data with var_definition = new_defs })) stmt
              in
              let new_ctx = add_var_def_to_ctx var new_defs current_block current_stmt_pos ctx in
              (new_stmt, new_ctx, current_stmt_pos)))
  | SVerif cond ->
      let new_e = inline_in_expr (Pos.unmark cond.cond_expr) ctx current_block current_stmt_pos in
      let new_stmt =
        Pos.same_pos_as (SVerif { cond with cond_expr = Pos.same_pos_as new_e cond.cond_expr }) stmt
      in
      (new_stmt, ctx, current_stmt_pos)
  | SConditional (cond, b1, b2, join) ->
      let new_cond = inline_in_expr cond ctx current_block current_stmt_pos in
      let new_stmt = Pos.same_pos_as (SConditional (new_cond, b1, b2, join)) stmt in
      (new_stmt, ctx, current_stmt_pos)
  | SGoto _ -> (stmt, ctx, current_stmt_pos)
  | SRuleCall (rule_id, name, stmts) ->
      let new_stmts, ctx, new_pos =
        List.fold_left
          (fun (stmts, ctx, stmt_pos) stmt ->
            let new_stmt, new_ctx, new_pos = inline_in_stmt stmt ctx current_block stmt_pos in
            (new_stmt :: stmts, new_ctx, new_pos + 1))
          ([], ctx, current_stmt_pos) stmts
      in
      let new_stmt = Pos.same_pos_as (SRuleCall (rule_id, name, List.rev new_stmts)) stmt in
      (new_stmt, ctx, new_pos)

let inlining (p : program) : program =
  let g = get_cfg p in
  let p, _ =
    Topological.fold
      (fun (block_id : block_id) (p, ctx) ->
        let block = BlockMap.find block_id p.blocks in
        let new_block, ctx, _ =
          List.fold_left
            (fun (new_block, ctx, stmt_pos) stmt ->
              let new_stmt, new_ctx, new_pos = inline_in_stmt stmt ctx block_id stmt_pos in
              (new_stmt :: new_block, new_ctx, new_pos + 1))
            ([], ctx, 0) block
        in
        ({ p with blocks = BlockMap.add block_id (List.rev new_block) p.blocks }, ctx))
      g
      (p, empty_ctx g p.entry_block)
  in
  p
