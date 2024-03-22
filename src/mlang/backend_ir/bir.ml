(* Copyright (C) 2019-2021-2020 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr> Raphaël Monat <raphael.monat@lip6.fr>

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

type program = {
  targets : Mir.target_data Mir.TargetMap.t;
  main_function : string;
  mir_program : Mir.program;
}

let main_statements (p : program) : Mir.m_instruction list =
  try (Mir.TargetMap.find p.main_function p.targets).target_prog
  with Not_found ->
    Errors.raise_error "Unable to find main function of Bir program"

let format_program fmt (p : program) =
  let format_stmts = Com.format_instructions Format_mir.format_variable in
  Format.fprintf fmt "%a" format_stmts (main_statements p)

let rec remove_empty_conditionals (stmts : Mir.m_instruction list) :
    Mir.m_instruction list =
  List.rev
    (List.fold_left
       (fun acc stmt ->
         match Pos.unmark stmt with
         | Com.IfThenElse (e, b1, b2) ->
             let b1 = remove_empty_conditionals b1 in
             let b2 = remove_empty_conditionals b2 in
             if List.length b1 = 0 && List.length b2 = 0 then acc
               (* empty conditional, we can discard it *)
             else Pos.same_pos_as (Com.IfThenElse (e, b1, b2)) stmt :: acc
         | _ -> stmt :: acc)
       [] stmts)

let get_used_variables_ (e : Mir.expression Pos.marked)
    (acc : Mir.VariableSet.t) : Mir.VariableSet.t =
  Mir.fold_expr_var
    (fun acc var -> Mir.VariableSet.add var acc)
    acc (Pos.unmark e)

let get_used_variables (e : Mir.expression Pos.marked) : Mir.VariableSet.t =
  get_used_variables_ e Mir.VariableSet.empty
