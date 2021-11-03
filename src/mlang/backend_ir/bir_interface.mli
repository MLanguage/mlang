(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr> Raphaël
   Monat <raphael.monat@lip6.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** Input-output management for BIR programs interpretation *)

type bir_function = {
  func_variable_inputs : unit Mir.VariableMap.t;
  func_constant_inputs : Mir.expression Pos.marked Mir.VariableMap.t;
  func_outputs : unit Mir.VariableMap.t;
  func_conds : Mir.condition_data Mir.VariableMap.t;
}
(** Input-output data necessary to interpret a BIR program*)

val translate_external_conditions :
  Mir.idmap -> Mast.expression Pos.marked list -> Mir.condition_data Mir.VariableMap.t
(** [translate_external_conditions idmap conditions] translates a series of boolean expressions
    [conditions] into M verification conditions ready to be added to a BIR program *)

val generate_function_all_vars : Bir.program -> bir_function
(** Function used to generate a [bir_function] that includes all possible inputs and outputs *)

val read_function_from_spec : Bir.program -> string -> bir_function
(** [read_function_from_spec program spec_file] reads and parses [spec_file] and extracts all the
    inputs, outputs and conditions from it. *)

val read_inputs_from_stdin : bir_function -> Mir.literal Mir.VariableMap.t
(** Given an input-output specification, prompts the user on [stdin] for the values of the inputs
    and returns them as a map *)

val adapt_program_to_function : Bir.program -> bir_function -> Bir.program * int
(** [adapt_program_to_function program io] modifies [program] according to the input-output
    specification of [io]*)
