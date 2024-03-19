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

(** {1 Translation helpers} *)

val get_var_from_name :
  Mir.Variable.t StrMap.t ->
  (* name of the variable to query *) string Pos.marked ->
  Mir.Variable.t
(** Queries a [type: Mir.variable.t] from an [type:idmap] mapping, the name of a
    variable and the rule number from which the variable is requested. Returns
    the variable with the same name and highest rule number that is below the
    current rule number from where this variable is requested *)

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
