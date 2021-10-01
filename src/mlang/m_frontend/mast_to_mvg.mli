(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

type loop_param_value = VarName of Mast.variable_name | RangeInt of int

module ParamsMap : Map.S with type key = Char.t

type loop_context = loop_param_value ParamsMap.t

type translating_context = {
  table_definition : bool;
      (** [true] if translating an expression susceptible to contain a generic table index *)
  idmap : Mir.idmap;  (** Current string-to-{!type: Mir.Variable.t} mapping *)
  lc : loop_context option;  (** Current loop translation context *)
  int_const_values : int Mir.VariableMap.t;  (** Mapping from constant variables to their value *)
  exec_number : Mir.execution_number;
      (** Number of the rule of verification condition being translated *)
  current_lvalue : Mast.variable_name;
}

val get_var_from_name :
  Mir.VariableSet.elt list Pos.VarNameToID.t ->
  string Pos.marked ->
  Mir.execution_number ->
  bool ->
  Mir.VariableSet.elt

val list_max_execution_number : Mir.VariableSet.elt list -> Mir.VariableSet.elt

val translate_expression :
  translating_context -> Mast.expression Pos.marked -> Mir.expression Pos.marked

val dummy_exec_number : Pos.t -> Mir.execution_number

val get_conds :
  Mir.Error.t list -> Mir.idmap -> Mast.program -> Mir.condition_data Mir.VariableMap.t

val translate : Mast.program -> Mir.program
