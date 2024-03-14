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
  | SIterate of Mir.Variable.t * Mir.CatVarSet.t * Mir.expression * stmt list
  | SRestore of
      Mir.VariableSet.t
      * (Mir.Variable.t * Mir.CatVarSet.t * Mir.expression) list
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

val main_statements : program -> stmt list

val remove_empty_conditionals : stmt list -> stmt list

val get_used_variables_ :
  Mir.expression Pos.marked -> Mir.VariableSet.t -> Mir.VariableSet.t

val get_used_variables : Mir.expression Pos.marked -> Mir.VariableSet.t
