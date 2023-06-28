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

open Format
open Mpp_ir

let format_scoped_var (fmt : formatter) (sv : scoped_var) : unit =
  fprintf fmt "%s"
    (match sv with
    | Local s -> s
    | Mbased (v, _) -> Pos.unmark v.Mir.Variable.name)

let format_var_filter (fmt : formatter) (f : var_filter) : unit =
  match f with
  | Saisie None -> pp_print_string fmt Mast.input_category
  | Calculee None -> pp_print_string fmt Mast.computed_category
  | Calculee (Some st) | Saisie (Some st) -> fprintf fmt "%s" st

let format_callable (fmt : formatter) (f : mpp_callable) =
  fprintf fmt "%s"
    (match f with
    | Program chain ->
        Format.asprintf "evaluate_program(%a)" Format_mast.format_chain_tag
          chain
    | Verif (chain, filter) ->
        Format.asprintf "verification(%a%a)" Format_mast.format_chain_tag chain
          (pp_print_option format_var_filter)
          filter
    | MppFunction m -> m
    | Present -> "present"
    | Abs -> "abs"
    | Cast -> "cast"
    | DepositDefinedVariables -> "DepositDefinedVariables"
    | TaxbenefitCeiledVariables -> "TaxbenefitCeiledVariables"
    | TaxbenefitDefinedVariables -> "TaxbenefitDefinedVariables")

let format_binop (fmt : formatter) (b : Mpp_ast.binop) : unit =
  fprintf fmt "%s"
    (match b with
    | And -> "and"
    | Or -> "or"
    | Gt -> ">"
    | Gte -> ">="
    | Lt -> "<"
    | Lte -> "<="
    | Eq -> "=="
    | Neq -> "!=")

let format_filter (fmt : formatter) (f : mpp_filter) : unit =
  assert (f = VarIsTaxBenefit);
  fprintf fmt "VarIsTaxBenefit"

let rec format_expression (fmt : formatter) (expr : mpp_expr_kind Pos.marked) :
    unit =
  match Pos.unmark expr with
  | Constant i -> fprintf fmt "%d" i
  | Variable sv -> format_scoped_var fmt sv
  | Unop (Minus, e) -> fprintf fmt "- (%a)" format_expression e
  | Call (f, args) ->
      fprintf fmt "%a(%a)" format_callable f
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt ", ")
           format_scoped_var)
        args
  | Binop (e1, b, e2) ->
      fprintf fmt "(%a %a %a)" format_expression e1 format_binop b
        format_expression e2

let rec format_stmt (fmt : formatter) (stmt : mpp_stmt) : unit =
  match Pos.unmark stmt with
  | Assign (sv, e) ->
      fprintf fmt "%a = %a" format_scoped_var sv format_expression e
  | Conditional (cond, t, []) ->
      fprintf fmt "if(%a):@\n@[<h 2>  %a@]" format_expression cond format_stmts
        t
  | Conditional (cond, t, f) ->
      fprintf fmt "if(%a):@\n@[<h 2>  %a@]else:@\n@[<h 2>  %a@]"
        format_expression cond format_stmts t format_stmts f
  | Delete sv -> fprintf fmt "del %a" format_scoped_var sv
  | Expr e -> format_expression fmt e
  | Partition (f, body) ->
      fprintf fmt "partition with %a:@\n@[<h 2>  %a@]" format_filter f
        format_stmts body

and format_stmts (fmt : formatter) (stmts : mpp_stmt list) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
    format_stmt fmt stmts

let format_compute (fmt : formatter) (compute : mpp_compute) : unit =
  fprintf fmt "%s(%a):@\n@[<h 2>  %a@]@\n" compute.name
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") format_scoped_var)
    compute.args format_stmts compute.body

let format_program (fmt : formatter) (mpp : mpp_compute list) : unit =
  pp_print_list format_compute fmt mpp
