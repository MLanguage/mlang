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

(** AST pretty printer *)

open Mast

let pp_print_list_comma eldisplay fmt l =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
    eldisplay fmt l

let pp_unmark f fmt e = f fmt (Pos.unmark e)

let pp_print_list_space eldisplay fmt l =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
    eldisplay fmt l

let pp_print_list_endline eldisplay fmt l =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    eldisplay fmt l

let format_application fmt (app : application) = Format.fprintf fmt "%s" app

let format_chaining fmt (c : chaining) = Format.fprintf fmt "%s" c

let format_variable_name fmt (v : variable_name) = Format.fprintf fmt "%s" v

let format_func_name fmt (f : func_name) = Format.fprintf fmt "%s" f

let format_variable_generic_name fmt (v : variable_generic_name) =
  Format.fprintf fmt "%s" v.base

let format_variable fmt (v : variable) =
  match v with
  | Normal v -> format_variable_name fmt v
  | Generic v -> format_variable_generic_name fmt v

let format_error_name fmt (e : error_name) = Format.fprintf fmt "%s" e

let format_literal fmt (l : literal) =
  match l with
  | Variable v -> format_variable fmt v
  | Float f -> Format.fprintf fmt "%f" f
  | Undefined -> Format.fprintf fmt "indefini"

let format_table_index fmt (i : table_index) =
  match i with
  | LiteralIndex i -> Format.fprintf fmt "%d" i
  | SymbolIndex v -> format_variable fmt v

let format_lvalue fmt (lv : lvalue) =
  match lv.index with
  | Some vi ->
      Format.fprintf fmt "%a[%a]" format_variable (Pos.unmark lv.var)
        format_table_index (Pos.unmark vi)
  | None -> Format.fprintf fmt "%a" format_variable (Pos.unmark lv.var)

let format_set_value fmt (sv : set_value) =
  match sv with
  | VarValue v -> format_variable fmt (Pos.unmark v)
  | Interval (i1, i2) ->
      Format.fprintf fmt "%d..%d" (Pos.unmark i1) (Pos.unmark i2)
  | FloatValue i -> Format.fprintf fmt "%f" (Pos.unmark i)

let format_set_value_loop fmt (sv : set_value_loop) =
  match sv with
  | Single l -> Format.fprintf fmt "%a" format_literal (Pos.unmark l)
  | Range (i1, i2) ->
      Format.fprintf fmt "%a..%a" format_literal (Pos.unmark i1) format_literal
        (Pos.unmark i2)
  | Interval (i1, i2) ->
      Format.fprintf fmt "%a-%a" format_literal (Pos.unmark i1) format_literal
        (Pos.unmark i2)

let format_comp_op fmt (op : comp_op) =
  Format.pp_print_string fmt
    (match op with
    | Gt -> ">"
    | Gte -> ">="
    | Lt -> "<"
    | Lte -> "<="
    | Eq -> "="
    | Neq -> "!=")

let format_binop fmt (op : binop) =
  Format.pp_print_string fmt
    (match op with
    | And -> "et"
    | Or -> "ou"
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/")

let format_unop fmt (op : unop) =
  Format.pp_print_string fmt (match op with Not -> "non" | Minus -> "-")

let format_loop_variable_ranges fmt ((v, vs) : loop_variable) =
  Format.fprintf fmt "un %c dans %a" (Pos.unmark v)
    (pp_print_list_comma format_set_value_loop)
    vs

let format_loop_variable_value_set fmt ((v, vs) : loop_variable) =
  Format.fprintf fmt "%c=%a" (Pos.unmark v)
    (pp_print_list_comma format_set_value_loop)
    vs

let format_loop_variables fmt (lvs : loop_variables) =
  match lvs with
  | Ranges vvs ->
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " et ")
        format_loop_variable_ranges fmt vvs
  | ValueSets vvs ->
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
        format_loop_variable_value_set fmt vvs

let option_print pp fmt o = match o with None -> () | Some e -> pp fmt e

let option_bind f o = match o with None -> None | Some e -> Some (f e)

let rec format_expression fmt (e : expression) =
  match e with
  | TestInSet (belong, e, values) ->
      Format.fprintf fmt "(%a %sdans %a)" format_expression (Pos.unmark e)
        (if belong then "" else "non ")
        (pp_print_list_comma format_set_value)
        values
  | Comparison (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" format_expression (Pos.unmark e1)
        format_comp_op (Pos.unmark op) format_expression (Pos.unmark e2)
  | Binop (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" format_expression (Pos.unmark e1)
        format_binop (Pos.unmark op) format_expression (Pos.unmark e2)
  | Unop (op, e) ->
      Format.fprintf fmt "%a %a" format_unop op format_expression (Pos.unmark e)
  | Index (v, i) ->
      Format.fprintf fmt "%a[%a]" format_variable (Pos.unmark v)
        format_table_index (Pos.unmark i)
  | Conditional (e1, e2, e3) ->
      Format.fprintf fmt "(si %a alors %a %afinsi)" format_expression
        (Pos.unmark e1) format_expression (Pos.unmark e2)
        (option_print format_expression)
        (option_bind Pos.unmark e3)
  | FunctionCall (f, args) ->
      Format.fprintf fmt "%a(%a)" format_func_name (Pos.unmark f)
        format_func_args args
  | Literal l -> format_literal fmt l
  | Loop (lvs, e) ->
      Format.fprintf fmt "pour %a%a" format_loop_variables (Pos.unmark lvs)
        format_expression (Pos.unmark e)
  | NbCategory l ->
      Format.fprintf fmt "nb_categorie(%a)"
        (pp_print_list_space (pp_unmark Format.pp_print_string))
        (Pos.unmark l)
  | Attribut (v, a) ->
      Format.fprintf fmt "attribut(%a, %s)" format_variable (Pos.unmark v)
        (Pos.unmark a)
  | Size v -> Format.fprintf fmt "taille(%a)" format_variable (Pos.unmark v)
  | NbError -> Format.fprintf fmt "nb_erreur()"

and format_func_args fmt (args : func_args) =
  match args with
  | ArgList args -> pp_print_list_space (pp_unmark format_expression) fmt args
  | LoopList (lvs, e) ->
      Format.fprintf fmt "%a%a" format_loop_variables (Pos.unmark lvs)
        format_expression (Pos.unmark e)

let format_formula_decl fmt (f : formula_decl) =
  Format.fprintf fmt "%a = %a" format_lvalue (Pos.unmark f.lvalue)
    format_expression (Pos.unmark f.formula)

let format_formula fmt (f : formula) =
  match f with
  | SingleFormula f -> format_formula_decl fmt f
  | MultipleFormulaes (lvs, f) ->
      Format.fprintf fmt "pour %a\n%a" format_loop_variables (Pos.unmark lvs)
        format_formula_decl f

let format_print_arg fmt = function
  | PrintString s -> Format.fprintf fmt "\"%s\"" s
  | PrintName v -> Format.fprintf fmt "nom(%a)" format_variable (Pos.unmark v)
  | PrintAlias v ->
      Format.fprintf fmt "alias(%a)" format_variable (Pos.unmark v)
  | PrintExpr (e, min, max) ->
      if min = max_int then
        Format.fprintf fmt "(%a)" (pp_unmark format_expression) e
      else if max = max_int then
        Format.fprintf fmt "(%a):%d" (pp_unmark format_expression) e min
      else
        Format.fprintf fmt "(%a):%d..%d" (pp_unmark format_expression) e min max

let format_var_category_id fmt (vd : var_category_id) =
  match Pos.unmark vd with
  | ("saisie", _) :: l ->
      Format.fprintf fmt "saisie %a"
        (pp_print_list_space (pp_unmark Format.pp_print_string))
        l
  | ("calculee", _) :: l ->
      Format.fprintf fmt "calculee %a"
        (pp_print_list_space (pp_unmark Format.pp_print_string))
        l
  | [ ("*", _) ] -> Format.fprintf fmt "*"
  | _ -> assert false

let rec format_instruction fmt (i : instruction) =
  match i with
  | Formula f -> pp_unmark format_formula fmt f
  | IfThenElse (e, il, []) ->
      Format.fprintf fmt "si %a alors %a finsi"
        (pp_unmark format_expression)
        e format_instruction_list il
  | IfThenElse (e, ilt, ile) ->
      Format.fprintf fmt "si %a alors %a sinon %a finsi"
        (pp_unmark format_expression)
        e format_instruction_list ilt format_instruction_list ile
  | ComputeDomain l ->
      Format.fprintf fmt "calculer domaine %a;"
        (pp_print_list_space (pp_unmark Format.pp_print_string))
        (Pos.unmark l)
  | ComputeChaining ch ->
      Format.fprintf fmt "calculer enchaineur %s;" (Pos.unmark ch)
  | ComputeTarget tn -> Format.fprintf fmt "calculer cible %s;" (Pos.unmark tn)
  | ComputeVerifs (l, expr) ->
      Format.fprintf fmt "verifier %a : avec %a;"
        (pp_print_list_space (pp_unmark Format.pp_print_string))
        (Pos.unmark l)
        (pp_unmark format_expression)
        expr
  | Print (std, args) ->
      let print_cmd =
        match std with StdOut -> "afficher" | StdErr -> "afficher_erreur"
      in
      Format.fprintf fmt "%s %a;" print_cmd
        (pp_print_list_space (pp_unmark format_print_arg))
        args
  | Iterate (var, vcats, expr, instrs) ->
      Format.fprintf fmt
        "iterer : variable %s : categorie %a : avec %a : dans ( %a )"
        (Pos.unmark var)
        (pp_print_list_comma format_var_category_id)
        vcats
        (pp_unmark format_expression)
        expr format_instruction_list instrs
  | Restore (rest_params, instrs) ->
      let pp_rest_param fmt = function
        | VarList l ->
            Format.fprintf fmt ": %a "
              (pp_print_list_comma (pp_unmark Format.pp_print_string))
              l
        | VarCats (var, vcats, expr) ->
            Format.fprintf fmt ": variable %s : categorie %a : avec %a "
              (Pos.unmark var)
              (pp_print_list_comma format_var_category_id)
              vcats
              (pp_unmark format_expression)
              expr
      in
      Format.fprintf fmt "restaurer %a : dans ( %a )"
        (pp_print_list_space (pp_unmark pp_rest_param))
        rest_params format_instruction_list instrs
  | RaiseError (err, var_opt) ->
      Format.fprintf fmt "leve_erreur %s%s;" (Pos.unmark err)
        (match var_opt with Some var -> " " ^ Pos.unmark var | None -> "")
  | CleanErrors -> Format.fprintf fmt "nettoie_erreurs;"

and format_instruction_list fmt (il : instruction Pos.marked list) =
  (Format.pp_print_list
     ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
     (pp_unmark format_instruction))
    fmt il

let format_rule fmt (r : rule) =
  Format.fprintf fmt "regle %d:\napplication %a;\n%a;\n"
    (Pos.unmark r.rule_number)
    (pp_print_list_comma (pp_unmark Format.pp_print_string))
    r.rule_applications
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";\n")
       (pp_unmark format_formula))
    r.rule_formulaes

let format_table_size fmt = function
  | Some (Mast.LiteralSize i, _) -> Format.fprintf fmt "[%d]" i
  | Some (Mast.SymbolSize s, _) -> Format.fprintf fmt "[%s]" s
  | None -> ()

let format_target fmt (t : target) =
  let format_tmp_var fmt (name, size) =
    let name = Pos.unmark name in
    Format.fprintf fmt "%s%a" name format_table_size size
  in
  Format.fprintf fmt
    "cible %s:\napplication %a\n: variables temporaires %a;\n%a;\n"
    (Pos.unmark t.target_name)
    (pp_print_list_comma (pp_unmark Format.pp_print_string))
    t.target_applications
    (pp_print_list_comma format_tmp_var)
    t.target_tmp_vars format_instruction_list t.target_prog

let format_value_typ fmt (t : value_typ) =
  Format.pp_print_string fmt
    (match t with
    | Boolean -> "BOOLEEN"
    | DateYear -> "DATE_AAAA"
    | DateDayMonthYear -> "DATE_JJMMAAAA"
    | DateMonth -> "DATE_MM"
    | Integer -> "ENTIER"
    | Real -> "REEL")

let format_input_attribute fmt ((n, v) : variable_attribute) =
  Format.fprintf fmt "%s = %d" (Pos.unmark n) (Pos.unmark v)

let format_input_variable fmt (v : input_variable) =
  Format.fprintf fmt "%a %s %a %a %a : %s%a;" format_variable_name
    (Pos.unmark v.input_name) Mast.input_category
    (pp_print_list_space Format.pp_print_string)
    (List.map Pos.unmark v.input_category)
    (pp_print_list_space format_input_attribute)
    v.input_attributes format_variable_name (Pos.unmark v.input_alias)
    (Pos.unmark v.input_description)
    (option_print format_value_typ)
    (option_bind Pos.unmark v.input_typ)

let format_computed_variable fmt (v : computed_variable) =
  Format.fprintf fmt "%s%a %s %a : %a%s;" (Pos.unmark v.comp_name)
    format_table_size v.comp_table computed_category
    (pp_print_list_space (pp_unmark Format.pp_print_string))
    v.comp_category
    (option_print format_value_typ)
    (option_bind Pos.unmark v.comp_typ)
    (Pos.unmark v.comp_description)

let format_variable_decl fmt (v : variable_decl) =
  match v with
  | ComputedVar v -> format_computed_variable fmt (Pos.unmark v)
  | ConstVar (name, value) ->
      Format.fprintf fmt "%a : const = %a" format_variable_name
        (Pos.unmark name) format_literal (Pos.unmark value)
  | InputVar v -> format_input_variable fmt (Pos.unmark v)

let format_verification_condition fmt (vc : verification_condition) =
  Format.fprintf fmt "si %a\n alors erreur %a %a;" format_expression
    (Pos.unmark vc.verif_cond_expr)
    (pp_unmark format_error_name)
    (fst vc.verif_cond_error)
    (Format.pp_print_option (pp_unmark format_variable_name))
    (snd vc.verif_cond_error)

let format_verification fmt (v : verification) =
  Format.fprintf fmt "verif %d : %a;\n%a"
    (Pos.unmark v.verif_number)
    (pp_print_list_space (pp_unmark format_application))
    v.verif_applications
    (pp_print_list_space (pp_unmark format_verification_condition))
    v.verif_conditions

let format_error_typ fmt (e : error_typ) =
  Format.pp_print_string fmt
    (match e with
    | Anomaly -> "anomalie"
    | Discordance -> "discordance"
    | Information -> "information")

let format_error_ fmt (e : error_) =
  Format.fprintf fmt "%a : %a : %a;" format_error_name (Pos.unmark e.error_name)
    format_error_typ (Pos.unmark e.error_typ)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt " : ")
       (pp_unmark Format.pp_print_string))
    e.error_descr

let format_var_type (t : var_type) =
  match t with Input -> input_category | Computed -> computed_category

let format_var_category fmt (c : var_category_decl) =
  Format.fprintf fmt "%s %a :@ attributs %a"
    (format_var_type c.var_type)
    (pp_print_list_space (pp_unmark Format.pp_print_string))
    c.var_category
    (pp_print_list_comma (pp_unmark Format.pp_print_string))
    c.var_attributes

let format_specialize_domain fmt (dl : string Pos.marked list Pos.marked list) =
  match dl with
  | [] -> ()
  | _ ->
      Format.fprintf fmt " :@ specialise %a"
        (pp_print_list_comma
           (pp_unmark (pp_print_list_space (pp_unmark Format.pp_print_string))))
        dl

let format_domain_attribute attr fmt b =
  if b then Format.fprintf fmt " :@ %s" attr

let format_domain (pp_data : Format.formatter -> 'a -> unit) fmt
    (d : 'a domain_decl) =
  Format.fprintf fmt "%a%a%a%a"
    (pp_print_list_comma
       (pp_unmark (pp_print_list_space (pp_unmark Format.pp_print_string))))
    d.dom_names format_specialize_domain d.dom_parents
    (format_domain_attribute "par_defaut")
    d.dom_by_default pp_data d.dom_data

let format_rule_domain fmt (rd : rule_domain_decl) =
  let pp_data fmt data =
    Format.fprintf fmt "%a"
      (format_domain_attribute "calculable")
      data.rdom_computable
  in
  format_domain pp_data fmt rd

let format_verif_domain fmt (vd : verif_domain_decl) =
  let pp_data fmt data =
    Format.fprintf fmt "%a"
      (pp_print_list_comma format_var_category_id)
      data.vdom_auth
  in
  format_domain pp_data fmt vd

let format_source_file_item fmt (i : source_file_item) =
  match i with
  | Application app ->
      Format.fprintf fmt "application %a;" format_application (Pos.unmark app)
  | Chaining (c, apps) ->
      Format.fprintf fmt "enchaineur %a %a;" format_chaining (Pos.unmark c)
        (pp_print_list_space (pp_unmark format_application))
        apps
  | VariableDecl vd -> format_variable_decl fmt vd
  | Rule r -> format_rule fmt r
  | Target t -> format_target fmt t
  | Verification v -> format_verification fmt v
  | Function -> ()
  | Error e -> format_error_ fmt e
  | Output o ->
      Format.fprintf fmt "sortie(%a);" format_variable_name (Pos.unmark o)
  | VarCatDecl c ->
      Format.fprintf fmt "variable category %a;" format_var_category
        (Pos.unmark c)
  | RuleDomDecl rd -> Format.fprintf fmt "rule domain %a;" format_rule_domain rd
  | VerifDomDecl vd ->
      Format.fprintf fmt "verif domain %a;" format_verif_domain vd

let format_source_file fmt (f : source_file) =
  pp_print_list_endline (pp_unmark format_source_file_item) fmt f
