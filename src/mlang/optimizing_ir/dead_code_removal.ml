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

let remove_dead_statements p (stmts : block) (block_id : block_id) (path_checker : Paths.path_checker)
    (doms : Dominators.dom) (used_vars : int BlockMap.t Mir.VariableMap.t) :
    int BlockMap.t Mir.VariableMap.t * stmt list =
  let update_used_vars (stmt_used_vars : unit Mir.VariableMap.t) (pos_in_block : int)
      (used_vars : int BlockMap.t Mir.VariableMap.t) : int BlockMap.t Mir.VariableMap.t =
    Mir.VariableMap.fold
      (fun stmt_used_var _ used_vars ->
        Mir.VariableMap.update stmt_used_var
          (fun old_entry ->
            match old_entry with
            | None -> Some (BlockMap.singleton block_id pos_in_block)
            | Some used -> Some (BlockMap.add block_id pos_in_block used))
          used_vars)
      stmt_used_vars used_vars
  in
  let used_vars, new_stmts, _ =
    List.fold_right
      (fun stmt (used_vars, acc, pos_in_block) ->
        match Pos.unmark stmt with
        | SAssign (var, var_def) ->
            if Pos.unmark var.name = "RMENAGE" then
              Cli.debug_print "block_id %d pos %d: %a" block_id pos_in_block Format_oir.format_stmt stmt;
            if
              (* here we determine whether this definition is useful or not*)
              match Mir.VariableMap.find_opt var used_vars with
              | None -> false
              | Some used_blocks ->
                  (* this definition is useful only if there exists a path from this block to a
                     block where it is being used, and no dominating redefinitions exist between
                     this block and the block where it is being used *)
                  let used =
                    BlockMap.filter
                      (fun used_block used_pos ->
                        let is_later_use =
                          (block_id = used_block && pos_in_block < used_pos)
                          || Paths.check_path path_checker block_id used_block
                        in
                        if Pos.unmark var.name = "RMENAGE" && used_block = 195 && used_pos = 348 then
                          Cli.debug_print "used_block = %d@.content %a@.used_pos = %d, is_later_use = %b" used_block Format_oir.format_stmts (let _ = BlockMap.find used_block p.blocks in [])  used_pos is_later_use;
                        let is_intermediate_def_dominating_later_use
                            (candidate_intermediate_block : block_id) (intermediate_pos : int) =
                          if candidate_intermediate_block = block_id then
                            intermediate_pos > pos_in_block && doms candidate_intermediate_block used_block
                          else if candidate_intermediate_block = used_block then
                            used_pos > intermediate_pos
                            (* && doms candidate_intermediate_block used_block *)
                          else
                            Paths.check_path path_checker block_id candidate_intermediate_block
                            && Paths.check_path path_checker candidate_intermediate_block used_block
                            && doms candidate_intermediate_block used_block
                        in
                        is_later_use
                        && (* at this point we need to check that there is no superceding dominating
                              definition between [block_id] and [used_block] *)
                        not
                          (BlockMap.exists
                             (fun candidate_intermediate_block intermediate_pos ->
                               if Pos.unmark var.name = "RMENAGE" then
                                 Cli.debug_print "candidate_intermediate_block=%d intermediate_pos=%d" candidate_intermediate_block intermediate_pos;
                               is_intermediate_def_dominating_later_use candidate_intermediate_block
                                 intermediate_pos)
                             used_blocks))
                      used_blocks
                  in
                  if Pos.unmark var.name = "RMENAGE" then
                    Cli.debug_print "used in blocks: %a"
                      (Format.pp_print_list (fun fmt (x, y) -> Format.fprintf fmt "(block %d, pos %d)" x y))
                      (BlockMap.bindings used);
                  BlockMap.cardinal used > 0
            then
              let stmt_used_vars =
                match var_def.Mir.var_definition with
                | Mir.SimpleVar e -> Mir_dependency_graph.get_used_variables e
                | Mir.TableVar (_, def) -> (
                    match def with
                    | Mir.IndexGeneric e -> Mir_dependency_graph.get_used_variables e
                    | Mir.IndexTable es ->
                        Mir.IndexMap.fold
                          (fun _ e used_vars ->
                            Mir_dependency_graph.get_used_variables_ e used_vars)
                          es Mir.VariableMap.empty )
                | Mir.InputVar -> assert false
                (* should not happen *)
              in
              if Mir.VariableMap.exists (fun v _ -> Pos.unmark v.name = "RMENAGE") stmt_used_vars then
                Cli.debug_print "RMENAGE used in var %a at pos %d" Format_mir.format_variable var pos_in_block;
              (* if Mir.VariableMap.exists (fun x () -> Pos.unmark x.name = "RMENAGE") stmt_used_vars then
               *   Cli.debug_print "RMENAGE used in %a" Format_oir.format_stmt stmt; *)
              (update_used_vars stmt_used_vars pos_in_block used_vars, stmt :: acc, pos_in_block - 1)
            else (used_vars, acc, pos_in_block - 1)
        | SVerif cond ->
            let stmt_used_vars = Mir_dependency_graph.get_used_variables cond.cond_expr in
            (update_used_vars stmt_used_vars pos_in_block used_vars, stmt :: acc, pos_in_block - 1)
        | SConditional (cond, _, _, _) ->
            let stmt_used_vars = Mir_dependency_graph.get_used_variables (cond, Pos.no_pos) in
            (update_used_vars stmt_used_vars pos_in_block used_vars, stmt :: acc, pos_in_block - 1)
        | SGoto _ -> (used_vars, stmt :: acc, pos_in_block - 1))
      stmts
      (used_vars, [], List.length stmts - 1)
  in
  (used_vars, new_stmts)

let dead_code_removal (p : program) : program =
  let g = get_cfg p in
  let rev_topological_order = Topological.fold (fun id acc -> id :: acc) g [] in
  let is_entry block_id = block_id = p.entry_block in
  let is_reachable = Reachability.analyze is_entry g in
  let p = { p with blocks = BlockMap.filter (fun bid _ -> is_reachable bid) p.blocks } in
  let path_checker = Paths.create g in
  let doms = Dominators.idom_to_dom (Dominators.compute_idom g p.entry_block) in
  let _, p =
    List.fold_left
      (fun (used_vars, p) block_id ->
        try
          let block = BlockMap.find block_id p.blocks in
          let used_vars, block =
            remove_dead_statements p block block_id path_checker doms used_vars
          in
          let p = { p with blocks = BlockMap.add block_id block p.blocks } in
          (used_vars, p)
        with Not_found -> (used_vars, p))
      ( Mir.VariableMap.map
          (fun () ->
            BlockMap.singleton p.exit_block (List.length (BlockMap.find p.exit_block p.blocks)))
          p.outputs,
        p )
      rev_topological_order
  in
  p
