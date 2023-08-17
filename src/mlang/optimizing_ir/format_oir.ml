(* Copyright (C) 2019-2021-2020 Inria, contributors: Denis Merigoux
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

open Oir

let rec format_stmt fmt (stmt : stmt) =
  match Pos.unmark stmt with
  | SAssign (v, vdef) ->
      Format.fprintf fmt "%s = %a@,"
        (Pos.unmark (Bir.var_to_mir v).Mir.Variable.name)
        Format_bir.format_variable_def vdef
  | SConditional (cond, b1, b2, _) ->
      Format.fprintf fmt "if(%a) then goto %d else goto %d@,"
        Format_bir.format_expression cond b1 b2
  | SVerif cond_data ->
      let cond_error_opt_var =
        Option.map Bir.var_to_mir (snd cond_data.cond_error)
      in
      Format.fprintf fmt "assert (%a) or raise %a%a@,"
        Format_bir.format_expression
        (Pos.unmark cond_data.cond_expr)
        Format_mir.format_error (fst cond_data.cond_error)
        (Format.pp_print_option (fun fmt v ->
             Format.fprintf fmt " (%s)" (Pos.unmark v.Mir.Variable.name)))
        cond_error_opt_var
  | SGoto b -> Format.fprintf fmt "goto %d@," b
  | SRovCall rid ->
      Format.fprintf fmt "call(%d)@," (Mir.num_of_rule_or_verif_id rid)
  | SFunctionCall (func, args) ->
      Format.fprintf fmt "call_function: %s with args %a@," func
        (Format.pp_print_list (fun fmt arg ->
             Format.fprintf fmt "%s" (arg.Mir.Variable.name |> Pos.unmark)))
        args

and format_stmts fmt (stmts : stmt list) =
  Format.pp_print_list ~pp_sep:(fun _ () -> ()) format_stmt fmt stmts
