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
  | SVerifBlock (b, _) -> Format.fprintf fmt "verif_block %d@," b
  | SGoto b -> Format.fprintf fmt "goto %d@," b
  | SRovCall rid ->
      Format.fprintf fmt "call(%d)@," (Mir.num_of_rule_or_verif_id rid)
  | SFunctionCall (func, args) ->
      Format.fprintf fmt "call_function: %s with args %a@," func
        (Format.pp_print_list (fun fmt arg ->
             Format.fprintf fmt "%s" (arg.Mir.Variable.name |> Pos.unmark)))
        args
  | SPrint (std, args) ->
      let print_cmd =
        match std with StdOut -> "afficher" | StdErr -> "afficher_erreur"
      in
      Format.fprintf fmt "%s %a;" print_cmd
        (Format_mast.pp_print_list_space Format_bir.format_print_arg)
        args
  | SIterate (var, vcs, expr, b, _) ->
      Format.fprintf fmt
        "iterate variable %s : categorie %a : avec %a : dans ( %d )@,"
        (Pos.unmark (Bir.var_to_mir var).Mir.Variable.name)
        (Mir.CatVarSet.pp ()) vcs Format_bir.format_expression expr b
  | SRestore (vars, var_params, b, _) ->
      let format_var_param fmt (var, vcs, expr) =
        Format.fprintf fmt ": variable %s : categorie %a : avec %a@\n"
          (Pos.unmark var.Bir.mir_var.name)
          (Mir.CatVarSet.pp ()) vcs Format_bir.format_expression expr
      in
      Format.fprintf fmt "restaure@;: %a@\n%a: apres ( %d )"
        (Bir.VariableSet.pp ~sep:", "
           ~pp_elt:(fun fmt var ->
             Format.fprintf fmt "%s" (Pos.unmark var.Bir.mir_var.name))
           ())
        vars
        (Format_mast.pp_print_list_space format_var_param)
        var_params b
  | SRaiseError (err, var_opt) ->
      Format.fprintf fmt "leve_erreur %s%s@," (Pos.unmark err.Mir.name)
        (match var_opt with Some var -> " " ^ var | None -> "")
  | SCleanErrors -> Format.fprintf fmt "nettoie_erreurs@,"
  | SExportErrors -> Format.fprintf fmt "exporte_erreurs@,"
  | SFinalizeErrors -> Format.fprintf fmt "finalise_erreurs@,"

and format_stmts fmt (stmts : stmt list) =
  Format.pp_print_list ~pp_sep:(fun _ () -> ()) format_stmt fmt stmts
