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

let remove_dead_statements (stmts : block) (id : block_id) (path_checker : Paths.path_checker)
    (used_vars : unit BlockMap.t Mir.VariableMap.t) : unit BlockMap.t Mir.VariableMap.t * stmt list
    =
  let update_used_vars (stmt_used_vars : unit Mir.VariableMap.t)
      (used_vars : unit BlockMap.t Mir.VariableMap.t) : unit BlockMap.t Mir.VariableMap.t =
    Mir.VariableMap.fold
      (fun stmt_used_var _ used_vars ->
        Mir.VariableMap.update stmt_used_var
          (fun old_entry ->
            match old_entry with
            | None -> Some (BlockMap.singleton id ())
            | Some used -> Some (BlockMap.add id () used))
          used_vars)
      stmt_used_vars used_vars
  in
  List.fold_right
    (fun stmt (used_vars, acc) ->
      match Pos.unmark stmt with
      | SAssign (var, var_def) ->
          if
            match Mir.VariableMap.find_opt var used_vars with
            | None -> false
            | Some used_blocks ->
                (* this definition is useful only if there exists a path from this block to the
                   block where it is being used *)
                BlockMap.exists
                  (fun used_block _ ->
                    id = used_block || Paths.check_path path_checker id used_block)
                  used_blocks
          then
            let stmt_used_vars =
              match var_def.Mir.var_definition with
              | Mir.SimpleVar e -> Mir_dependency_graph.get_used_variables e
              | Mir.TableVar (_, def) -> (
                  match def with
                  | Mir.IndexGeneric e -> Mir_dependency_graph.get_used_variables e
                  | Mir.IndexTable es ->
                      Mir.IndexMap.fold
                        (fun _ e used_vars -> Mir_dependency_graph.get_used_variables_ e used_vars)
                        es Mir.VariableMap.empty )
              | Mir.InputVar -> assert false
              (* should not happen *)
            in
            (update_used_vars stmt_used_vars used_vars, stmt :: acc)
          else (used_vars, acc)
      | SVerif cond ->
          let stmt_used_vars = Mir_dependency_graph.get_used_variables cond.cond_expr in
          (update_used_vars stmt_used_vars used_vars, stmt :: acc)
      | SConditional (cond, _, _, _) ->
          let stmt_used_vars = Mir_dependency_graph.get_used_variables (cond, Pos.no_pos) in
          (update_used_vars stmt_used_vars used_vars, stmt :: acc)
      | SGoto _ -> (used_vars, stmt :: acc))
    stmts (used_vars, [])

let dead_code_removal (p : program) : program =
  let g = get_cfg p in
  let rev_topological_order = Topological.fold (fun id acc -> id :: acc) g [] in
  let is_entry block_id = block_id = p.entry_block in
  let is_reachable = Reachability.analyze is_entry g in
  let p = { p with blocks = BlockMap.filter (fun bid _ -> is_reachable bid) p.blocks } in
  let path_checker = Paths.create g in
  let _, p =
    List.fold_left
      (fun (used_vars, p) block_id ->
        try
          let block = BlockMap.find block_id p.blocks in
          let used_vars, block = remove_dead_statements block block_id path_checker used_vars in
          let p = { p with blocks = BlockMap.add block_id block p.blocks } in
          (used_vars, p)
        with Not_found -> (used_vars, p))
      (Mir.VariableMap.map (fun () -> BlockMap.singleton p.exit_block ()) p.outputs, p)
      rev_topological_order
  in
  p
