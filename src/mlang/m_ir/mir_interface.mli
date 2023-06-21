(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux
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

(** While [type:Mir.program] contains the AST of all rules, [type:full_program]
    also encapsulates the dependency order of the rules. *)

val reset_all_outputs : Mir.program -> Mir.program
(** This function defines the [var_io] property of [var_data] to [Regular] for
    all variables that don't have [Input] as their [var_io]. *)

type chain_order = {
  dep_graph : Mir_dependency_graph.RG.t;
  execution_order : Mir.rov_id list;
}

type full_program = {
  program : Mir.program;
  domains_orders : chain_order StrSetMap.t;
  chainings_orders : chain_order StrMap.t;
}

val to_full_program : Mir.program -> full_program
(** Creates the dependency graph and stores it *)

val output_var_dependencies :
  full_program -> chain_order -> Mir.variable -> unit
(** Print list of input variables effecting the valuation of the given variable
    in the given chain *)
