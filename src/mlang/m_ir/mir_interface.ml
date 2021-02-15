(* Copyright (C) 2019 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr> Raphël Monat
   <raphael.monat@lip6.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Mir

let reset_all_outputs (p : program) : program =
  {
    p with
    program_vars =
      VariableMap.mapi
        (fun var var_data ->
          match var_data.var_io with
          | Input ->
              {
                var_data with
                var_io = Input;
                var_definition =
                  ( match var_data.var_definition with
                  | InputVar | SimpleVar _ -> var_data.var_definition
                  | TableVar _ ->
                      Errors.raise_spanned_error
                        (Format.asprintf
                           "Defining a\n\
                           \             variable input for a table variable %s is not supported"
                           (Pos.unmark var.Variable.name))
                        (Pos.get_position var.Variable.name) );
              }
          | _ ->
              {
                var_data with
                var_io = Regular;
                var_definition =
                  ( match var_data.var_definition with
                  | InputVar -> assert false
                  | SimpleVar old -> SimpleVar old
                  | TableVar (size, old) -> TableVar (size, old) );
              })
        p.program_vars;
  }

type full_program = {
  dep_graph : Mir_dependency_graph.G.t;
  execution_order : Mir_dependency_graph.execution_order;
  program : Mir.program;
}

let to_full_program (program : program) : full_program =
  let dep_graph = Mir_dependency_graph.create_dependency_graph program in
  { program; dep_graph; execution_order = Mir_dependency_graph.get_execution_order dep_graph }
