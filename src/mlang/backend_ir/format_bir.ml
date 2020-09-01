(* Copyright (C) 2019-2020 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr> Raphël
   Monat <raphael.monat@lip6.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Bir

let rec format_stmt fmt (stmt : stmt) =
  match Pos.unmark stmt with
  | SAssign (v, vdata) ->
      Format.fprintf fmt "%s = %a" (Pos.unmark v.Mir.Variable.name) Format_mir.format_variable_def
        vdata.var_definition
  | SConditional (cond, t, []) ->
      Format.fprintf fmt "if(%a):@\n@[<h 2>  %a@]@\n" Format_mir.format_expression cond format_stmts
        t
  | SConditional (cond, t, f) ->
      Format.fprintf fmt "if(%a):@\n@[<h 2>  %a@]else:@\n@[<h 2>  %a@]@\n"
        Format_mir.format_expression cond format_stmts t format_stmts f
  | SVerif cond_data ->
      Format.fprintf fmt "assert (%a) or raise %a" Format_mir.format_expression
        (Pos.unmark cond_data.cond_expr)
        (Format_mast.pp_print_list_comma Format_mir.format_error)
        cond_data.cond_errors

and format_stmts fmt (stmts : stmt list) =
  Format.pp_print_list ~pp_sep:(fun _ () -> ()) format_stmt fmt stmts

let format_program fmt (p : program) = Format.fprintf fmt "%a" format_stmts p.statements
