(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

(** AST pretty printer *)

module Pos = Verifisc.Pos
open Ast

let pp_print_list_comma eldisplay fmt l =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
    eldisplay fmt l

let pp_unmark f = fun fmt e -> f fmt (Pos.unmark e)

let pp_print_list_space eldisplay fmt l =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ") eldisplay fmt l

let pp_print_list_endline eldisplay fmt l =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") eldisplay fmt l


let format_application fmt (app: application)  =
  Format.fprintf fmt "%s" app

let format_chaining fmt (c: chaining) =
  Format.fprintf fmt "%s" c

let format_rule_name fmt (rn : rule_name) =
  (pp_print_list_space (pp_unmark Format.pp_print_string)) fmt rn

let format_variable_name fmt (v: variable_name) =
  Format.fprintf fmt "%s" v

let format_func_name fmt (f:func_name) =
  Format.fprintf fmt "%s" f

let format_variable_generic_name fmt (v: variable_generic_name) =
  Format.fprintf fmt "%s" v.base

let format_variable fmt (v: variable) = match v with
  | Normal v -> format_variable_name fmt v
  | Generic v -> format_variable_generic_name fmt v

let format_verification_name fmt (n: verification_name) =
  (pp_print_list_space (pp_unmark Format.pp_print_string)) fmt n

let format_error_name fmt (e:error_name) =
  Format.fprintf fmt "%s" e

let format_literal fmt (l:literal) = match l with
  | Variable v -> format_variable fmt v
  | Float f -> Format.fprintf fmt "%f" f

let format_table_index fmt (i:table_index) = match i with
  | LiteralIndex i -> Format.fprintf fmt "%d" i
  | SymbolIndex v -> format_variable fmt v

let format_lvalue fmt (lv: lvalue) =
  match lv.index with
  | Some vi ->
    Format.fprintf fmt "%a[%a]" format_variable (Pos.unmark lv.var)
      format_table_index (Pos.unmark vi)
  | None ->
    Format.fprintf fmt "%a" format_variable (Pos.unmark lv.var)

let format_set_value fmt (sv: set_value) = match sv with
  | VarValue v -> format_variable fmt (Pos.unmark v)
  | Interval (i1, i2) -> Format.fprintf fmt "%d..%d" (Pos.unmark i1) (Pos.unmark i2)
  | FloatValue i -> Format.fprintf fmt "%f" (Pos.unmark i)

let format_set_value_loop fmt (sv: set_value_loop) = match sv with
  | VarParam v -> Format.fprintf fmt "%s" (Pos.unmark v)
  | IntervalLoop (i1, i2) -> Format.fprintf fmt "%a..%a" format_literal (Pos.unmark i1) format_literal (Pos.unmark i2)

let format_comp_op fmt (op: comp_op) =
  Format.pp_print_string fmt (match op with
  | Gt -> ">"
  | Gte -> ">="
  | Lt -> "<"
  | Lte -> "<="
  | Eq -> "="
  | Neq -> "!=")

let format_binop fmt (op: binop) =
  Format.pp_print_string fmt (match op with
  | And -> "et"
  | Or -> "ou"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/")

let format_unop fmt (op: unop) =
  Format.pp_print_string fmt (match op with
  | Not -> "non"
  | Minus -> "-")

let format_loop_variable_ranges fmt ((v, vs): loop_variable) =
  Format.fprintf fmt "un %c dans %a"
    (Pos.unmark v)
    (pp_print_list_comma format_set_value_loop) vs

let format_loop_variable_value_set fmt ((v, vs): loop_variable) =
  Format.fprintf fmt "%c=%a"
    (Pos.unmark v)
    (pp_print_list_comma format_set_value_loop) vs

let format_loop_variables fmt (lvs: loop_variables) =
  match lvs with
  | Ranges vvs ->
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " et ") format_loop_variable_ranges fmt vvs
  | ValueSets vvs ->
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";") format_loop_variable_value_set fmt vvs

let option_print pp fmt o = match o with
  | None -> ()
  | Some e -> pp fmt e

let option_bind f o = match o with
  | None -> None
  | Some e -> Some (f e)

let rec format_expression fmt (e: expression) = match e with
  | TestInSet (belong, e, values) ->
    Format.fprintf fmt "(%a %sdans %a)"
      format_expression (Pos.unmark e)
      (if belong then "" else "non ")
      (pp_print_list_comma format_set_value) values
  | Comparison (op, e1, e2) ->
    Format.fprintf fmt "(%a %a %a)"
      format_expression (Pos.unmark e1)
      format_comp_op (Pos.unmark op)
      format_expression (Pos.unmark e2)
  | Binop (op, e1, e2) ->
    Format.fprintf fmt "(%a %a %a)"
      format_expression (Pos.unmark e1)
      format_binop (Pos.unmark op)
      format_expression (Pos.unmark e2)
  | Unop (op, e) ->
    Format.fprintf fmt "%a %a"
      format_unop op
      format_expression (Pos.unmark e)
  | Index (v, i) ->
    Format.fprintf fmt "%a[%a]"
      format_variable (Pos.unmark v)
      format_table_index (Pos.unmark i)
  | Conditional (e1, e2, e3) ->
    Format.fprintf fmt "(si %a alors %a %afinsi)"
      format_expression (Pos.unmark e1)
      format_expression (Pos.unmark e2)
      (option_print format_expression) (option_bind Pos.unmark e3)
  | FunctionCall (f, args) ->
    Format.fprintf fmt "%a(%a)" format_func_name (Pos.unmark f)
      format_func_args args
  | Literal l -> format_literal fmt l
  | Loop (lvs, e) ->
    Format.fprintf fmt "pour %a%a"
      format_loop_variables (Pos.unmark lvs)
      format_expression (Pos.unmark e)

and format_func_args fmt (args:func_args) = match args with
  | ArgList args ->
    pp_print_list_space (pp_unmark format_expression) fmt args
  | LoopList (lvs, e) ->
    Format.fprintf fmt "%a%a"
      format_loop_variables (Pos.unmark lvs)
      format_expression (Pos.unmark e)

let format_formula_decl fmt (f:formula_decl) =
  Format.fprintf fmt "%a = %a"
    format_lvalue (Pos.unmark f.lvalue)
    format_expression (Pos.unmark f.formula)

let format_formula fmt (f:formula) = match f with
  | SingleFormula f ->
    format_formula_decl fmt f
  | MultipleFormulaes (lvs, f) ->
    Format.fprintf fmt "pour %a\n%a"
      format_loop_variables (Pos.unmark lvs)
      format_formula_decl f

let format_rule fmt (r: rule) =
  Format.fprintf fmt
    "regle %a:\napplication %a;\n%a;\n"
    (pp_print_list_space (pp_unmark Format.pp_print_string)) r.rule_name
    (pp_print_list_comma (pp_unmark Format.pp_print_string)) r.rule_applications
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";\n")
       (pp_unmark format_formula)) r.rule_formulaes

let format_computed_typ fmt (t:computed_typ) = match t with
  | Base -> Format.fprintf fmt "base"
  | GivenBack -> Format.fprintf fmt "restituee"

let format_input_variable_subtype fmt (t: input_variable_subtype) =
  Format.pp_print_string fmt (match t with
  | Context -> "contexte"
  | Family -> "famille"
  | Penality -> "penalite"
  | Income -> "revenu")

let format_value_typ fmt (t:value_typ) =
  Format.pp_print_string fmt (match t with
  | Boolean -> "BOOLEEN"
  | DateYear -> "DATE_AAAA"
  | DateDayMonthYear -> "DATE_JJMMAAAA"
  | DateMonth -> "DATE_MM"
  | Integer -> "ENTIER"
  | Real -> "REEL")

let format_input_attribute fmt ((n,v): input_variable_attribute Pos.marked * literal Pos.marked) =
  Format.fprintf fmt "%s = %a"
    (Pos.unmark n)
    format_literal (Pos.unmark v)

let format_input_variable fmt (v:input_variable) =
  Format.fprintf fmt "%a saisie %a %a%s %a : %s%a;"
    format_variable_name (Pos.unmark v.input_name)
    format_input_variable_subtype (Pos.unmark v.input_subtyp)
    (pp_print_list_space format_input_attribute) v.input_attributes
    (if v.input_given_back then " restituee" else "")
    format_variable_name (Pos.unmark v.input_alias)
    (Pos.unmark v.input_description)
    (option_print format_value_typ) (option_bind Pos.unmark v.input_typ)

let format_computed_variable fmt (v: computed_variable) =
  Format.fprintf fmt "%s%a calculee %a : %a%s;"
    (Pos.unmark v.comp_name)
    (option_print Format.pp_print_int) (option_bind Pos.unmark v.comp_table)
    (pp_print_list_space (pp_unmark format_computed_typ)) v.comp_subtyp
    (option_print format_value_typ) (option_bind Pos.unmark v.comp_typ)
    (Pos.unmark v.comp_description)

let format_variable_decl fmt (v: variable_decl) =
  match v with
  | ComputedVar v -> format_computed_variable fmt (Pos.unmark v)
  | ConstVar (name, value) ->
    Format.fprintf fmt "%a : const = %a"
      format_variable_name (Pos.unmark name)
      format_literal (Pos.unmark value)
  | InputVar v -> format_input_variable fmt (Pos.unmark v)

let format_verification_condition fmt (vc: verification_condition) =
  Format.fprintf fmt "si %a\n alors erreur %a;"
    format_expression (Pos.unmark vc.verif_cond_expr)
    (pp_print_list_space (pp_unmark format_error_name)) vc.verif_cond_errors

let format_verification fmt (v: verification) =
  Format.fprintf fmt "verif %a : %a;\n%a"
    format_verification_name v.verif_name
    (pp_print_list_space (pp_unmark format_application)) v.verif_applications
    (pp_print_list_space (pp_unmark format_verification_condition)) v.verif_conditions

let format_error_typ fmt (e:error_typ) =
  Format.pp_print_string fmt (match e with
  | Anomaly -> "anomalie"
  | Discordance -> "discordance"
  | Information -> "information")

let format_error_ fmt (e:error_) =
  Format.fprintf fmt "%a : %a : %a;"
    format_error_name (Pos.unmark e.error_name)
    format_error_typ (Pos.unmark e.error_typ)
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " : ")
       (pp_unmark Format.pp_print_string)) e.error_descr

let format_source_file_item fmt (i:source_file_item) =
  match i with
  | Application app -> Format.fprintf fmt "application %a;" format_application (Pos.unmark app)
  | Chaining (c, apps) ->
    Format.fprintf fmt "enchaineur %a %a;"
      format_chaining c
      (pp_print_list_space (pp_unmark format_application)) apps
  | VariableDecl vd -> format_variable_decl fmt vd
  | Rule r -> format_rule fmt r
  | Verification v -> format_verification fmt v
  | Function  -> ()
  | Error e -> format_error_ fmt e
  | Output o -> Format.fprintf fmt "sortie(%a);" format_variable_name (Pos.unmark o)

let format_source_file fmt (f: source_file) =
  pp_print_list_endline (pp_unmark format_source_file_item) fmt f
