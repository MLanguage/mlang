(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux
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

open Oir
module PosSet = IntSet

type pos_map = PosSet.t BlockMap.t Bir.VariableMap.t

let remove_dead_statements (stmts : block) (id : block_id)
    (path_checker : Paths.path_checker) (doms : Dominators.dom)
    (used_vars : pos_map) (used_defs : pos_map) : pos_map * pos_map * stmt list
    =
  (* used_vars contains, for each variable, the location of the top-most use of
     this variables in every basic block *)
  let update_used_vars (stmt_used_vars : Bir.VariableSet.t) (pos : int)
      (used_vars : pos_map) : pos_map =
    Bir.VariableSet.fold
      (fun stmt_used_var (used_vars : pos_map) ->
        Bir.VariableMap.update stmt_used_var
          (function
            | None -> Some (BlockMap.singleton id (PosSet.singleton pos))
            | Some used -> (
                match BlockMap.find_opt id used with
                | None -> Some (BlockMap.add id (PosSet.singleton pos) used)
                | Some old_pos ->
                    Some (BlockMap.add id (PosSet.add pos old_pos) used)))
          used_vars)
      stmt_used_vars used_vars
  in
  let remove_dead_stmts_of stmts used_vars used_defs pos =
    List.fold_left
      (fun ((used_vars : pos_map), (used_defs : pos_map), acc, pos) stmt ->
        match Pos.unmark stmt with
        | SAssign (var, var_def) ->
            let used_defs_returned =
              update_used_vars (Bir.VariableSet.singleton var) pos used_defs
            in
            if
              (* here we determine whether this definition is useful or not *)
              match Bir.VariableMap.find_opt var used_vars with
              | None -> false
              | Some used_blocks ->
                  (* this definition is useful only if there exists a path from
                     this block to a block where it is being used, and no
                     dominating redefinitions exist between this block and the
                     block where it is being used *)
                  BlockMap.exists
                    (fun (used_block : block_id) (used_pos : PosSet.t) ->
                      (* we only consider the top-most used of the block, since
                         later uses of the same block are dominated by this
                         top-most use *)
                      let used_pos = PosSet.min_elt used_pos in
                      let is_later_use =
                        (id = used_block && pos < used_pos)
                        || Paths.check_path path_checker id used_block
                      in
                      let is_intermediate_def_dominating_later_use
                          (candidate_intermediate_block : block_id)
                          (intermediate_pos : int) =
                        if candidate_intermediate_block = id then
                          intermediate_pos > pos
                          && doms candidate_intermediate_block used_block
                        else if candidate_intermediate_block = used_block then
                          used_pos > intermediate_pos
                          (* && doms candidate_intermediate_block used_block *)
                        else
                          Paths.check_path path_checker id
                            candidate_intermediate_block
                          && Paths.check_path path_checker
                               candidate_intermediate_block used_block
                          && doms candidate_intermediate_block used_block
                      in
                      (* at this point we need to check that there is no
                         superceding dominating definition between [id] and
                         [used_block] *)
                      let defs_blocks =
                        match Bir.VariableMap.find_opt var used_defs with
                        | Some x -> x
                        | None -> BlockMap.empty
                      in
                      let not_superceding_dominating_definition =
                        not
                          (BlockMap.exists
                             (fun (candidate_intermediate_block : block_id)
                                  (intermediate_pos : PosSet.t) ->
                               PosSet.exists
                                 (fun intermediate_pos ->
                                   is_intermediate_def_dominating_later_use
                                     candidate_intermediate_block
                                     intermediate_pos)
                                 intermediate_pos)
                             defs_blocks)
                      in
                      is_later_use && not_superceding_dominating_definition)
                    used_blocks
            then
              let stmt_used_vars =
                match var_def with
                | Mir.SimpleVar e -> Bir.get_used_variables e
                | Mir.TableVar (_, def) -> (
                    match def with
                    | Mir.IndexGeneric (v, e) ->
                        Bir.get_used_variables_ e (Bir.VariableSet.singleton v)
                    | Mir.IndexTable es ->
                        Mir.IndexMap.fold
                          (fun _ e used_vars ->
                            Bir.get_used_variables_ e used_vars)
                          es Bir.VariableSet.empty)
                | Mir.InputVar -> assert false
                (* should not happen *)
              in
              ( update_used_vars stmt_used_vars pos used_vars,
                used_defs_returned,
                stmt :: acc,
                pos - 1 )
            else (used_vars, used_defs_returned, acc, pos - 1)
        (* CR Keryan: why updated [used_defs] here ? This definition is
           removed *)
        | SVerif cond ->
            let stmt_used_vars = Bir.get_used_variables cond.cond_expr in
            ( update_used_vars stmt_used_vars pos used_vars,
              used_defs,
              stmt :: acc,
              pos - 1 )
        | SConditional (cond, _, _, _) ->
            let stmt_used_vars = Bir.get_used_variables (cond, Pos.no_pos) in
            ( update_used_vars stmt_used_vars pos used_vars,
              used_defs,
              stmt :: acc,
              pos - 1 )
        | SPrint (_, args) ->
            let used_vars =
              List.fold_left
                (fun used_vars arg ->
                  match arg with
                  | Mir.PrintString _ | Mir.PrintName _ | Mir.PrintAlias _ ->
                      used_vars
                  | Mir.PrintExpr (e, _, _) ->
                      update_used_vars (Bir.get_used_variables e) pos used_vars)
                used_vars args
            in
            (used_vars, used_defs, stmt :: acc, pos - 1)
        | SGoto _ | SRovCall _ | SFunctionCall _ ->
            (used_vars, used_defs, stmt :: acc, pos - 1))
      (used_vars, used_defs, [], pos)
      (List.rev stmts)
  in
  let used_vars, used_defs, new_stmts, _ =
    remove_dead_stmts_of stmts used_vars used_defs 0
    (* actual [pos] number irrelevant, no problem to have only negative
       values *)
  in
  (used_vars, used_defs, new_stmts)

let dead_code_removal0 (outputs : unit Bir.VariableMap.t) (cfg : cfg) : cfg =
  let g = get_cfg cfg in
  let rev_topological_order = Topological.fold (fun id acc -> id :: acc) g [] in
  let is_entry block_id = block_id = cfg.entry_block in
  let is_reachable = Reachability.analyze is_entry g in
  let cfg =
    {
      cfg with
      blocks = BlockMap.filter (fun bid _ -> is_reachable bid) cfg.blocks;
    }
  in
  let path_checker = Paths.create g in
  let doms =
    Dominators.idom_to_dom (Dominators.compute_idom g cfg.entry_block)
  in
  let _, _, cfg =
    List.fold_left
      (fun (used_vars, defs_vars, p) block_id ->
        try
          let block = BlockMap.find block_id p.blocks in
          let used_vars, defs_vars, block =
            remove_dead_statements block block_id path_checker doms used_vars
              defs_vars
          in
          let p = { p with blocks = BlockMap.add block_id block p.blocks } in
          (used_vars, defs_vars, p)
        with Not_found -> (used_vars, defs_vars, p))
      ( Bir.VariableMap.map
          (fun () -> BlockMap.singleton cfg.exit_block (PosSet.singleton 1))
          outputs,
        Bir.VariableMap.empty,
        cfg )
      rev_topological_order
  in
  cfg

let dead_code_removal (p : program) : program =
  map_program_cfgs (dead_code_removal0 p.outputs) p
