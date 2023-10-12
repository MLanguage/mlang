(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
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

(** {!module: Mast} to {!module: Mir} translation of M programs. *)

(** {1 Translation context}*)

(** {2 General translation context} *)

type translating_context = {
  table_definition : bool;
      (** [true] if translating an expression susceptible to contain a generic
          table index *)
  idmap : Mir.idmap;  (** Current string-to-{!type: Mir.Variable.t} mapping *)
  exec_number : Mir.execution_number;
      (** Number of the rule of verification condition being translated *)
}

(** {1 Translation helpers} *)

val get_var_from_name :
  Mir.idmap ->
  (* name of the variable to query *) string Pos.marked ->
  Mir.execution_number ->
  (* using_var_in_def *) bool ->
  Mir.Variable.t
(** Queries a [type: Mir.variable.t] from an [type:idmap] mapping, the name of a
    variable and the rule number from which the variable is requested. Returns
    the variable with the same name and highest rule number that is below the
    current rule number from where this variable is requested *)

val list_max_execution_number : Mir.Variable.t list -> Mir.Variable.t
(** Helper to compute the max SSA candidate in a list *)

val translate_expression :
  Mir.cat_variable_data Mir.CatVarMap.t ->
  Mir.idmap ->
  translating_context ->
  Mast.expression Pos.marked ->
  Mir.expression Pos.marked
(** Main translation function for expressions *)

val dummy_exec_number : Pos.t -> Mir.execution_number
(** Dummy execution number used for variable declarations *)

val get_conds :
  Mir.verif_domain Mast.DomainIdMap.t ->
  Mir.cat_variable_data Mir.CatVarMap.t ->
  Mir.Error.t list ->
  Mir.idmap ->
  Mast.program ->
  Mir.condition_data Mir.RuleMap.t
(** Returns a map whose keys are dummy variables and whose values are the
    verification conditions. *)

(** {1 Main translation function}*)

val translate : Mast.program -> Mir.program
(** Main translation function from the M AST to the M Variable Graph. This
    function performs 6 linear passes on the input code:

    - [get_constants] gets the value of all constant variables, the values of
      which are needed to compute certain loop bounds;
    - [get_variables_decl] retrieves the declarations of all other variables and
      errors;
    - [get_var_redefinitions] incorporates into [idmap] all definitions inside
      rules along with their execution number;
    - [get_var_data] is the workhorse pass that translates all the expressions
      corresponding to the definitions;
    - [add_dummy_definition_for_variable_declaration] adds [Undefined]
      definitions placeholder for all variables declarations;
    - [get_errors_conds] parses the verification conditions definitions. *)
