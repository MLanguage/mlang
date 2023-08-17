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

open Bir

let format_expression fmt (e : expression) =
  Format_mir.format_expression fmt (Mir.map_expr_var Bir.var_to_mir e)

let format_variable_def fmt (vdef : variable_def) =
  Format_mir.format_variable_def fmt (Mir.map_var_def_var Bir.var_to_mir vdef)

let rec format_stmt fmt (stmt : stmt) =
  match Pos.unmark stmt with
  | SAssign (v, vdef) ->
      Format.fprintf fmt "%s = %a"
        (Pos.unmark (var_to_mir v).Mir.Variable.name)
        format_variable_def vdef
  | SConditional (cond, t, []) ->
      Format.fprintf fmt "if(%a):@\n@[<h 2>  %a@]@\n" format_expression cond
        format_stmts t
  | SConditional (cond, t, f) ->
      Format.fprintf fmt "if(%a):@\n@[<h 2>  %a@]else:@\n@[<h 2>  %a@]@\n"
        format_expression cond format_stmts t format_stmts f
  | SVerif cond_data ->
      let cond_error_opt_var =
        Option.map var_to_mir (snd cond_data.cond_error)
      in
      Format.fprintf fmt "assert (%a) or raise %a@,%a" format_expression
        (Pos.unmark cond_data.cond_expr)
        Format_mir.format_error (fst cond_data.cond_error)
        (Format.pp_print_option (fun fmt v ->
             Format.fprintf fmt " (%s)" (Pos.unmark v.Mir.Variable.name)))
        cond_error_opt_var
  | SRovCall r ->
      Format.fprintf fmt "call_rule(%d)@\n" (Mir.num_of_rule_or_verif_id r)
  | SFunctionCall (func, args) ->
      Format.fprintf fmt "call_function: %s with args %a@," func
        (Format.pp_print_list (fun fmt arg ->
             Format.fprintf fmt "%s" (arg.Mir.Variable.name |> Pos.unmark)))
        args

and format_stmts fmt (stmts : stmt list) =
  Format.pp_print_list ~pp_sep:(fun _ () -> ()) format_stmt fmt stmts

let format_rule fmt rule =
  match rule.rov_code with
  | Rule stmts ->
      Format.fprintf fmt "rule %d:@\n@[<h 2>  %a@]@\n"
        (Mir.num_of_rule_or_verif_id rule.rov_id)
        format_stmts stmts
  | Verif stmt ->
      Format.fprintf fmt "verif %d:@\n@[<h 2>  %a@]@\n"
        (Mir.num_of_rule_or_verif_id rule.rov_id)
        format_stmts [ stmt ]

let format_rules fmt rules =
  Format.pp_print_list
    ~pp_sep:(fun _ () -> ())
    format_rule fmt
    (Bir.ROVMap.bindings rules |> List.map snd)

let format_program fmt (p : program) =
  Format.fprintf fmt "%a%a" format_rules p.rules_and_verifs format_stmts
    (Bir.main_statements_with_context_and_tgv_init p)
