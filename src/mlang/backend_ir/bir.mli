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

type rov_id = Mir.rov_id

module ROVMap = Mir.RuleMap

type tgv_id = string

type variable = { on_tgv : tgv_id; offset : int; mir_var : Mir.Variable.t }

module VariableMap : Map.S with type key = variable

module VariableSet : Set.S with type elt = variable

type expression = variable Mir.expression_

type condition_data = variable Mir.condition_data_

type variable_def = variable Mir.variable_def_

type variable_data = variable Mir.variable_data_

type function_name = string

type rule_or_verif_code = Rule of stmt list | Verif of stmt

and rule_or_verif = {
  rov_id : rov_id;
  rov_name : string Pos.marked;
  rov_code : rule_or_verif_code;
}

and stmt = stmt_kind Pos.marked

and stmt_kind =
  | SAssign of variable * variable_data
  | SConditional of expression * stmt list * stmt list
  | SVerif of condition_data
  | SRuleCall of rov_id
  | SFunctionCall of function_name * Mir.Variable.t list

type mpp_function = { mppf_stmts : stmt list; mppf_is_verif : bool }

module FunctionMap : Map.S with type key = function_name

type program = {
  mpp_functions : mpp_function FunctionMap.t;
  rules_and_verifs : rule_or_verif ROVMap.t;
  main_function : function_name;
  idmap : Mir.idmap;
  mir_program : Mir.program;
  outputs : unit VariableMap.t;
}

val default_tgv : tgv_id

val size_of_tgv : unit -> int

val var_from_mir : tgv_id -> Mir.Variable.t -> variable

val var_to_mir : variable -> Mir.Variable.t

val compare_variable : variable -> variable -> int

val map_from_mir_map : tgv_id -> 'a Mir.VariableMap.t -> 'a VariableMap.t

val set_from_mir_dict : tgv_id -> Mir.VariableDict.t -> VariableSet.t

val rule_or_verif_as_statements : rule_or_verif -> stmt list

val main_statements : program -> stmt list

val get_all_statements : program -> stmt list

val squish_statements : program -> int -> string -> program
(** In order to handle backends with limited function / method capacity, such as
    Java's 64kB of bytecode per method, class, etc, this funciton allows a
    [program] to be split into chunks of an arbitrary size using the string
    argument as a suffix to the new function / method name. We piggyback on the
    existing rules semantics, with these chunks being rule definitions and
    inserting rule calls in their place*)

val get_assigned_variables : program -> VariableSet.t

val get_local_variables : program -> unit Mir.LocalVariableMap.t

val get_locals_size : program -> int

val remove_empty_conditionals : stmt list -> stmt list

val get_used_variables_ :
  expression Pos.marked -> VariableSet.t -> VariableSet.t

val get_used_variables : expression Pos.marked -> VariableSet.t
