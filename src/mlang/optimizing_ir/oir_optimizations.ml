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

let remove_dead_statements (stmts : block) (used_vars : unit Mir.VariableMap.t) :
    unit Mir.VariableMap.t * stmt list =
  List.fold_right
    (fun stmt (used_vars, acc) ->
      match Pos.unmark stmt with
      | SAssign (var, var_def) ->
          if Mir.VariableMap.mem var used_vars then
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

let dead_code_removal (p : program) (g : CFG.t) : program =
  let rev_topological_order = Topological.fold (fun id acc -> id :: acc) g [] in
  let _, p =
    List.fold_left
      (fun (used_vars, p) block_id ->
        let block = BlockMap.find block_id p.blocks in
        let used_vars, block = remove_dead_statements block used_vars in
        let p = { p with blocks = BlockMap.add block_id block p.blocks } in
        (used_vars, p))
      (Mir.VariableMap.empty, p) rev_topological_order
  in
  p

let optimize (p : program) : program =
  let g = get_cfg p in
  Cli.debug_print "Intruction count: %d" (count_instr p);
  let p = dead_code_removal p g in
  Cli.debug_print "Intruction count: %d" (count_instr p);
  p
