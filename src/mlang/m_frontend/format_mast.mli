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

val format_var_type : Mast.var_type -> string

val format_variable : Pp.t -> Com.var_name -> unit

val format_rule_domain : Pp.t -> Mast.rule_domain_decl -> unit

val format_verif_domain : Pp.t -> Mast.verif_domain_decl -> unit

val format_source_file_item : Pp.t -> Mast.source_file_item -> unit

val format_source_file : Pp.t -> Mast.source_file -> unit
