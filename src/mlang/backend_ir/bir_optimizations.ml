(* Copyright (C) 2020 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Bir

let rec remove_dead_statements (stmts : stmt list) (used_vars : unit Mir.VariableMap.t) :
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
      | SConditional (cond, s1, s2) ->
          let used_s1, new_s1 = remove_dead_statements s1 used_vars in
          let used_s2, new_s2 = remove_dead_statements s2 used_vars in
          let combined_used = Mir.VariableMap.union (fun _ _ _ -> Some ()) used_s1 used_s2 in
          let new_used =
            Mir_dependency_graph.get_used_variables (Pos.same_pos_as cond stmt) combined_used
          in
          (new_used, Pos.same_pos_as (SConditional (cond, new_s1, new_s2)) stmt :: acc))
    stmts (used_vars, [])

let dead_code_elimination (p : program) : program =
  { p with statements = snd (remove_dead_statements p.statements p.outputs) }
