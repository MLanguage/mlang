(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Mvg

let format_execution_number fmt (exec_number : execution_number) =
  if exec_number.rule_number = -1 then
    Format.fprintf fmt "declaration, %a" Pos.format_position exec_number.pos
  else
    Format.fprintf fmt "rule %d, sequence index %d, %a" exec_number.rule_number
      exec_number.seq_number Pos.format_position exec_number.pos

let format_execution_number_short fmt (exec_number : execution_number) =
  if exec_number.rule_number = -1 then Format.fprintf fmt "declaration"
  else Format.fprintf fmt "%d#%d" exec_number.rule_number exec_number.seq_number

let format_typ fmt (t : typ) = Format.pp_print_string fmt (match t with Real -> "real")

let format_io fmt (io : io) =
  Format.pp_print_string fmt
    (match io with Input -> "input" | Output -> "output" | Regular -> "regular")

let format_func fmt (f : func) =
  Format.pp_print_string fmt
    ( match f with
    | SumFunc -> "somme"
    | AbsFunc -> "abs"
    | MinFunc -> "min"
    | MaxFunc -> "max"
    | GtzFunc -> "positif"
    | GtezFunc -> "positif_ou_nul"
    | NullFunc -> "null"
    | ArrFunc -> "arr"
    | InfFunc -> "inf"
    | PresentFunc -> "present"
    | Multimax -> "multimax"
    | Supzero -> "supzero" )

let format_literal fmt (l : literal) =
  Format.pp_print_string fmt (match l with Float f -> string_of_float f | Undefined -> "indéfini")

let rec format_expression fmt (e : expression) =
  match e with
  | Comparison ((op, _), (e1, _), (e2, _)) ->
      Format.fprintf fmt "(%a %a %a)" format_expression e1 Format_ast.format_comp_op op
        format_expression e2
  | Binop ((op, _), (e1, _), (e2, _)) ->
      Format.fprintf fmt "(%a %a %a)" format_expression e1 Format_ast.format_binop op
        format_expression e2
  | Unop (op, (e, _)) -> Format.fprintf fmt "%a %a" Format_ast.format_unop op format_expression e
  | Conditional ((e1, _), (e2, _), (e3, _)) ->
      Format.fprintf fmt "(si %a alors %a sinon %a)" format_expression e1 format_expression e2
        format_expression e3
  | FunctionCall (f, args) ->
      Format.fprintf fmt "%a(%a)" format_func f
        (Format_ast.pp_print_list_comma (Format_ast.pp_unmark format_expression))
        args
  | Literal lit -> format_literal fmt lit
  | Var var ->
      Format.fprintf fmt "%s[%a]" (Pos.unmark var.Variable.name) format_execution_number_short
        var.Variable.execution_number
  | LocalVar lvar -> Format.fprintf fmt "t%d" lvar.LocalVariable.id
  | GenericTableIndex -> Format.fprintf fmt "X"
  | Error -> Format.fprintf fmt "erreur"
  | LocalLet (lvar, (e1, _), (e2, _)) ->
      Format.fprintf fmt "soit t%d = (%a) dans %a" lvar.LocalVariable.id format_expression e1
        format_expression e2
  | Index (var, i) ->
      Format.fprintf fmt "%s[%a]"
        (Pos.unmark (Pos.unmark var).Variable.name)
        format_expression (Pos.unmark i)

let format_variable_def fmt (def : variable_def) =
  match def with
  | SimpleVar e -> Format.fprintf fmt "%a@\n" format_expression (Pos.unmark e)
  | InputVar -> Format.fprintf fmt "[User input]@\n"
  | TableVar (_, IndexGeneric e) -> Format.fprintf fmt "X -> %a@\n" format_expression (Pos.unmark e)
  | TableVar (_, IndexTable defs) ->
      IndexMap.map_printer (Format_ast.pp_unmark format_expression) fmt defs

let format_variable_data fmt (def : variable_data) =
  Format.fprintf fmt "type %a, io %a:\n%a"
    (fun fmt () ->
      match def.var_typ with None -> Format.fprintf fmt "unknown" | Some t -> format_typ fmt t)
    () format_io def.var_io format_variable_def def.var_definition

let format_program_vars fmt (p : variable_data VariableMap.t) =
  VariableMap.map_printer
    (fun fmt var ->
      Format.fprintf fmt "Variable %s%s" (Pos.unmark var.Variable.name)
        (match var.Variable.alias with Some x -> " (alias " ^ x ^ ")" | None -> ""))
    format_variable_data fmt p

let format_error fmt (e : Error.t) =
  Format.fprintf fmt "erreur %s (%s)" (Pos.unmark e.Error.name) (Pos.unmark e.Error.descr)

let format_precondition fmt (precond : condition_data) =
  Format.fprintf fmt "Précondition : %a\nSinon %a" format_expression (Pos.unmark precond.cond_expr)
    (Format_ast.pp_print_list_comma format_error)
    precond.cond_errors

let format_program_conds fmt (conds : condition_data VariableMap.t) =
  Format_ast.pp_print_list_endline
    (fun fmt (_, cond) -> format_precondition fmt cond)
    fmt (VariableMap.bindings conds)

let format_program fmt (p : program) =
  Format.fprintf fmt "%a\n\n%a" format_program_vars p.program_vars format_program_conds
    p.program_conds

let format_variable fmt (v : Variable.t) =
  Format.fprintf fmt "%s: %s" (Pos.unmark v.Variable.name) (Pos.unmark v.Variable.descr)

let format_io fmt (io : io) =
  Format.pp_print_string fmt
    (match io with Input -> "input" | Output -> "output" | Regular -> "regular")

let rec format_stmt fmt (stmt : stmt) =
  match Pos.unmark stmt with
  | SAssign (v, vdata) ->
      Format.fprintf fmt "%s = %a" (Pos.unmark v.Variable.name) format_variable_def
        vdata.var_definition
  | SConditional (cond, t, []) ->
      Format.fprintf fmt "if(%a):@\n@[<h 2>  %a@]@\n" format_expression cond format_stmts t
  | SConditional (cond, t, f) ->
      Format.fprintf fmt "if(%a):@\n@[<h 2>  %a@]else:@\n@[<h 2>  %a@]@\n" format_expression cond
        format_stmts t format_stmts f

and format_stmts fmt stmts = Format.pp_print_list ~pp_sep:(fun _ () -> ()) format_stmt fmt stmts

let format_new_program fmt (p : new_program) =
  Format.fprintf fmt "%a\n\n%a" format_stmts p.statements format_program_conds p.conds
