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

type function_name = string

and stmt = stmt_kind Pos.marked

and stmt_kind =
  | SAssign of
      Mir.Variable.t
      * (int * Mir.expression Pos.marked) option
      * Mir.expression Pos.marked
  | SConditional of Mir.expression * stmt list * stmt list
  | SVerifBlock of stmt list
  | SFunctionCall of function_name * Mir.Variable.t list
  | SPrint of Mast.print_std * Mir.Variable.t Mir.print_arg list
  | SIterate of Mir.Variable.t * Com.CatVarSet.t * Mir.expression * stmt list
  | SRestore of
      Mir.VariableSet.t
      * (Mir.Variable.t * Com.CatVarSet.t * Mir.expression) list
      * stmt list
  | SRaiseError of Mir.error * string option
  | SCleanErrors
  | SExportErrors
  | SFinalizeErrors

type target_function = {
  file : string option;
  tmp_vars : (Mir.Variable.t * Pos.t * int option) StrMap.t;
  stmts : stmt list;
}

type program = {
  targets : target_function Mir.TargetMap.t;
  main_function : function_name;
  mir_program : Mir.program;
}

let main_statements (p : program) : stmt list =
  try (Mir.TargetMap.find p.main_function p.targets).stmts
  with Not_found ->
    Errors.raise_error "Unable to find main function of Bir program"

let rec remove_empty_conditionals (stmts : stmt list) : stmt list =
  List.rev
    (List.fold_left
       (fun acc stmt ->
         match Pos.unmark stmt with
         | SConditional (e, b1, b2) ->
             let b1 = remove_empty_conditionals b1 in
             let b2 = remove_empty_conditionals b2 in
             if List.length b1 = 0 && List.length b2 = 0 then acc
               (* empty conditional, we can discard it *)
             else Pos.same_pos_as (SConditional (e, b1, b2)) stmt :: acc
         | _ -> stmt :: acc)
       [] stmts)

let get_used_variables_ (e : Mir.expression Pos.marked)
    (acc : Mir.VariableSet.t) : Mir.VariableSet.t =
  Mir.fold_expr_var
    (fun acc var -> Mir.VariableSet.add var acc)
    acc (Pos.unmark e)

let get_used_variables (e : Mir.expression Pos.marked) : Mir.VariableSet.t =
  get_used_variables_ e Mir.VariableSet.empty
