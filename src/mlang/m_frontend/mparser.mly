(*
Copyright (C) 2019-2021 Inria, contributor:
    Denis Merigoux <denis.merigoux@inria.fr>
    Raphaël Monat <raphael.monat@lip6.fr>

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

%{
 open Mast
 open Parse_utils

 type comp_subtyp_or_attr =
 | CompSubTyp of string Pos.marked
 | Attr of variable_attribute

 let parse_to_literal (v: parse_val) : literal = match v with
 | ParseVar v -> Variable v
 | ParseInt v -> Float (float_of_int v)

 (** Module generated automaticcaly by Menhir, the parser generator *)
%}

%token<string> SYMBOL STRING

%token PLUS MINUS TIMES DIV
%token GTE LTE GT LT NEQ EQUALS
%token SEMICOLON COLON COMMA
%token AND OR NOTIN NOT UNDEFINED

%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token RANGE

%token BOOLEAN DATE_YEAR DATE_DAY_MONTH_YEAR DATE_MONTH INTEGER REAL
%token ONE IN APPLICATION CHAINING TYPE TABLE
%token COMPUTED CONST ALIAS INPUT FOR
%token RULE VERIFICATION TARGET TEMPORARY SIZE
%token IF THEN ELSEIF ELSE ENDIF PRINT PRINT_ERR NAME INDENT
%token COMPUTE VERIFY WITH VERIF_NUMBER COMPL_NUMBER NB_CATEGORY
%token NB_ANOMALIES NB_DISCORDANCES NB_INFORMATIVES
%token RAISE_ERROR EXPORT_ERRORS CLEAN_ERRORS
%token ITERATE CATEGORY RESTORE AFTER
%token ERROR ANOMALY DISCORDANCE CONDITION
%token INFORMATIVE OUTPUT FONCTION VARIABLE ATTRIBUT
%token BASE GIVEN_BACK COMPUTABLE BY_DEFAULT
%token DOMAIN SPECIALIZE AUTHORIZE VERIFIABLE

%token EOF

%type<Mast.source_file> source_file
%type<Mast.function_spec> function_spec
%type<Mast.literal> literal_input

%nonassoc SEMICOLON
%left OR
%left AND
%nonassoc NOT
(* %nonassoc SYMBOL *)

%start source_file
%start function_spec
%start literal_input

%%

%inline with_pos(X):
| x = X { (x, mk_position $sloc) }

symbol_with_pos:
| s = with_pos(SYMBOL) { s }

symbol_list_with_pos:
| sl = with_pos(symbol_with_pos+) { sl }

source_file:
| vl = with_pos(symbol_colon_etc)* is = source_file_rev EOF {
    List.flatten (vl :: List.rev is)
  }

symbol_colon_etc:
| v = variable_decl { v }
| e = error_ { e }
| fonction { Function }

source_file_rev:
| is = source_file_rev i = source_file_item { i :: is }
| { [] }

source_file_item:
| al = application_etc { al }
| cl = chaining_etc { cl }
| cl = var_category_decl_etc { cl }
| crl = rule_domain_decl_etc { crl }
| cvl = verif_domain_decl_etc { cvl }
| ol = output_etc { ol }
| rl = rule_etc { rl }
| vl = verification_etc { vl }
| tl = target_etc { tl }

var_typ:
| INPUT { Input }
| COMPUTED { Computed }

var_category_decl_etc:
| c = with_pos(var_category_decl) l = with_pos(symbol_colon_etc)* {
    Pos.same_pos_as (VarCatDecl c) c :: l
  }

var_category_decl:
| VARIABLE var_type = var_typ var_category = symbol_with_pos* COLON
  ATTRIBUT var_attributes = separated_nonempty_list(COMMA, symbol_with_pos)
  SEMICOLON {
    { var_type; var_category; var_attributes }
  }

rule_domain_decl_etc:
| cr =with_pos(rule_domain_decl) l = with_pos(symbol_colon_etc)* { cr :: l }

rule_domain_decl:
| DOMAIN RULE rdom_params = separated_nonempty_list(COLON, with_pos(rdom_param))
  SEMICOLON {
    let err msg pos = Errors.raise_spanned_error msg pos in
    let fold (dno, dso, dco, dpdo) = function
    | (Some dn, _, _, _), pos ->
        if dno = None then Some dn, dso, dco, dpdo
        else err "rule domain names are already defined" pos
    | (_, Some ds, _, _), pos ->
        if dso = None then dno, Some ds, dco, dpdo
        else err "rule domain specialization is already specified" pos
    | (_, _, Some dc, _), pos ->
        if dco = None then dno, dso, Some dc, dpdo
        else err "rule domain is already calculated" pos
    | (_, _, _, Some dpd), pos ->
        if dpdo = None then dno, dso, dco, Some dpd
        else err "rule domain is already defined by defaut" pos
    | (_, _, _, _), _ -> assert false
    in
    let init = None, None, None, None in
    let dno, dso, dco, dpdo = List.fold_left fold init rdom_params in
    let dom_names =
      match dno with
      | None -> err "rule domain names must be defined" (mk_position $sloc)
      | Some dn -> dn
    in
    let decl = {
      dom_names;
      dom_parents = (match dso with None -> [] | Some ds -> ds);
      dom_by_default = (match dpdo with None -> false | _ -> true);
      dom_data = {rdom_computable = (match dco with None -> false | _ -> true)};
    } in
    RuleDomDecl decl
  }

rdom_param:
| rdom_names = separated_nonempty_list(COMMA, symbol_list_with_pos)
  { (Some rdom_names, None, None, None) }
| SPECIALIZE rdom_parents = separated_nonempty_list(COMMA, symbol_list_with_pos)
  { (None, Some rdom_parents, None, None) }
| COMPUTABLE
  { (None, None, Some (), None) }
| BY_DEFAULT
  { (None, None, None, Some ()) }

verif_domain_decl_etc:
| cv = with_pos(verif_domain_decl) l = with_pos(symbol_colon_etc)* { cv :: l }

verif_domain_decl:
| DOMAIN VERIFICATION vdom_params = separated_nonempty_list(COLON, with_pos(vdom_param))
  SEMICOLON {
    let err msg pos = Errors.raise_spanned_error msg pos in
    let fold (dno, dso, dvo, dpdo, dco) = function
    | (Some dn, _, _, _, _), pos ->
        if dno = None then Some dn, dso, dvo, dpdo, dco
        else err "verif domain names are already defined" pos
    | (_, Some ds, _, _, _), pos ->
        if dso = None then dno, Some ds, dvo, dpdo, dco
        else err "verif domain specialization is already specified" pos
    | (_, _, Some dv, _, _), pos ->
        if dvo = None then dno, dso, Some dv, dpdo, dco
        else err "verif domain authorization is already specified" pos
    | (_, _, _, Some dpd, _), pos ->
        if dpdo = None then dno, dso, dvo, Some dpd, dco
        else err "verif domain is already defined by defaut" pos
    | (_, _, _, _, Some dcd), pos ->
        if dco = None then dno, dso, dvo, dpdo, Some dcd
        else err "verif domain is already verifiable" pos
    | (_, _, _, _, _), _ -> assert false
    in
    let init = None, None, None, None, None in
    let dno, dso, dvo, dpdo, dco = List.fold_left fold init vdom_params in
    let dom_names =
      match dno with
      | None -> err "rule domain names must be defined" (mk_position $sloc)
      | Some dn -> dn
    in
    let dom_data = {
      vdom_auth = (match dvo with None -> [] | Some dv -> dv);
      vdom_verifiable = (match dco with None -> false | _ -> true); 
    } in
    let decl = {
      dom_names;
      dom_parents = (match dso with None -> [] | Some ds -> ds);
      dom_by_default = (match dpdo with None -> false | _ -> true);
      dom_data;
    } in
    VerifDomDecl decl
  }

var_comp_category:
| BASE { "base" }
| GIVEN_BACK { "restituee" }
| TIMES { "*" }

var_category_id:
| INPUT TIMES { ["saisie", Pos.no_pos; "*", Pos.no_pos] }
| INPUT l = symbol_with_pos+ { ("saisie", Pos.no_pos) :: l }
| COMPUTED l = with_pos(var_comp_category)* { ("calculee", Pos.no_pos) :: l }
| TIMES { ["*", Pos.no_pos] }

vdom_param:
| vdom_names = separated_nonempty_list(COMMA, symbol_list_with_pos)
  { (Some vdom_names, None, None, None, None) }
| SPECIALIZE vdom_parents = separated_nonempty_list(COMMA, symbol_list_with_pos)
  { (None, Some vdom_parents, None, None, None) }
| AUTHORIZE vcats = separated_nonempty_list(COMMA, with_pos(var_category_id))
  { (None, None, Some vcats, None, None) }
| BY_DEFAULT
  { (None, None, None, Some (), None) }
| VERIFIABLE
  { (None, None, None, None, Some ()) }

fonction:
| SYMBOL COLON FONCTION SYMBOL SEMICOLON { () }

application_etc:
| a = with_pos(application) l = with_pos(symbol_colon_etc)* { a :: l }

application:
| APPLICATION s = with_pos(SYMBOL) SEMICOLON { Application s }

application_reference:
| APPLICATION COLON ss = symbol_enumeration { ss }

chaining_etc:
| c = with_pos(chaining) l = with_pos(symbol_colon_etc)* { c :: l }

chaining:
| CHAINING s = symbol_with_pos aps = application_reference SEMICOLON {
    Chaining (s, aps)
  }

chaining_reference:
| CHAINING COLON c = with_pos(SYMBOL) SEMICOLON { c }

variable_decl:
| v = with_pos(comp_variable) { VariableDecl (ComputedVar v) }
| cv = const_variable { let n, v = cv in VariableDecl (ConstVar (n, v)) }
| v = with_pos(input_variable) { VariableDecl (InputVar v) }

const_variable_name:
| name = SYMBOL COLON CONST { parse_variable_name $sloc name }

const_value:
| value = SYMBOL { parse_const_value value }

const_variable:
| name = with_pos(const_variable_name) EQUALS value = with_pos(const_value)
  SEMICOLON { (name, value) }

comp_variable_name:
| name = SYMBOL COLON { parse_variable_name $sloc name }

comp_variable_table:
| TABLE LBRACKET size = SYMBOL RBRACKET { parse_table_size size }

comp_variable_descr:
| descr = STRING { parse_string descr }

comp_attr:
| BASE { "base" }
| GIVEN_BACK { "restituee" }

comp_attr_or_subtyp:
| attr = variable_attribute { let (x, y) = attr in Attr (x, y) }
| attr = with_pos(comp_attr) { CompSubTyp attr }

comp_variable:
| comp_name = with_pos(comp_variable_name) comp_table = with_pos(comp_variable_table)?
  COMPUTED subtyp = comp_attr_or_subtyp* COLON
  comp_description = with_pos(comp_variable_descr) comp_typ = value_type?
  SEMICOLON {
    let comp_attributes =
      subtyp
      |> List.filter (function Attr _ -> true | _ -> false)
      |> List.map (function Attr (x, y) -> (x, y) | _ -> assert false)
    in
    let comp_category =
      subtyp
      |> List.filter (function CompSubTyp _ -> true | _ -> false) 
      |> List.map (function CompSubTyp x -> x | _ -> assert false)
    in
    let comp_is_givenback =
      subtyp
      |> List.exists (function CompSubTyp ("restituee", _) -> true | _ -> false)
    in
    {
      comp_name;
      comp_table;
      comp_attributes;
      comp_category;
      comp_is_givenback;
      comp_description;
      comp_typ;
    }
  }

input_variable_name:
| name = SYMBOL COLON { parse_variable_name $sloc name }

input_descr:
descr = STRING { parse_string descr }

input_attr_or_category:
| attr = variable_attribute { (None, Some attr, false) }
| cat = symbol_with_pos { (Some cat, None, false) }
| GIVEN_BACK { None, None, true }

input_variable_alias:
| ALIAS alias = SYMBOL { parse_variable_name $sloc alias }

variable_attribute_value:
| value = SYMBOL { parse_int $sloc value }

variable_attribute:
| attr = symbol_with_pos EQUALS
  lit = with_pos(variable_attribute_value)
{ (attr, lit) }

value_type_prim:
| BOOLEAN { Boolean }
| DATE_YEAR { DateYear }
| DATE_DAY_MONTH_YEAR { DateDayMonthYear }
| DATE_MONTH { DateMonth }
| INTEGER { Integer }
| REAL { Real }

value_type:
| TYPE typ = with_pos(value_type_prim) { typ }

input_variable:
| name = with_pos(input_variable_name) INPUT
  category_attrs = input_attr_or_category* alias = with_pos(input_variable_alias)
  COLON descr = with_pos(input_descr) typ = value_type?
  SEMICOLON {
    let (category, attrs, givenback) =
      List.fold_left
        (fun (category, attrs, givenback) (c, a, r) -> 
          match c, a, r with
          | Some x, _, _ -> x :: category, attrs, givenback
          | _, Some x, _ -> category, x :: attrs, givenback
          | _, _, true -> category, attrs, true
          | _, _, _ -> category, attrs, givenback)
        ([], [], false)
        category_attrs
    in
    {
      input_name = name;
      input_category = category;
      input_attributes = attrs;
      input_alias = alias;
      input_is_givenback = givenback;
      input_typ = typ;
      input_description = descr;
    }
  }

rule_etc:
| RULE name = symbol_list_with_pos COLON apps = application_reference
  SEMICOLON c = chaining_reference?
  formulaes_etc = formula_list_etc
  {
    let num, rule_tag_names =
      let uname = Pos.unmark name in
      let begPos =
        match uname with
        | h :: _ -> Pos.get_position h
        | [] -> assert false
      in
      let rec aux tags endPos = function
      | [num] ->
           let pos = Pos.make_position_between begPos endPos in
           num, (tags, pos)
      | h :: t -> aux (h :: tags) (Pos.get_position h) t
      | [] -> assert false
      in
      aux [] begPos uname
    in
    let rule_number =
      try Pos.map_under_mark int_of_string num
      with _ ->
        Errors.raise_spanned_error
          "this rule doesn't have an execution number"
          (Pos.get_position num)
    in
    let formulaes, l = formulaes_etc in 
    let rule = {
      rule_number;
      rule_tag_names;
      rule_applications = apps;
      rule_chaining = c;
      rule_formulaes =  formulaes;
    } in
    Pos.same_pos_as (Rule rule) name :: l
  }

target_etc:
| TARGET name = symbol_with_pos COLON
  apps = application_reference SEMICOLON
  tmp_vars = temporary_variables_decl?
  prog_etc = instruction_list_etc
  {
    let target_prog, l = prog_etc in
    let target = {
      target_name = name;
      target_file = None;
      target_applications = apps;
      target_tmp_vars = (match tmp_vars with None -> [] | Some l -> l);
      target_prog;
    } in
    Pos.same_pos_as (Target target) name :: l
  }

temporary_variable_name:
| name = symbol_with_pos size = with_pos(comp_variable_table)? {
    let name_str, name_pos = name in
    (parse_variable_name $sloc name_str, name_pos), size
  }

temporary_variables_decl:
| VARIABLE TEMPORARY COLON
  tmp_vars = separated_nonempty_list(COMMA, temporary_variable_name) SEMICOLON
    { tmp_vars }

instruction_list_etc:
| i = with_pos(instruction) l = with_pos(symbol_colon_etc)* { [i], l }
| i = with_pos(instruction) il_etc = instruction_list_etc {
    let il, l = il_etc in
    i :: il, l
  }

instruction_list_rev:
| i = with_pos(instruction) { [i] }
| il = instruction_list_rev i = with_pos(instruction) { i :: il }

instruction:
| f = with_pos(formula_kind) SEMICOLON { Formula f }
| IF e = with_pos(expression)
  THEN ilt = instruction_list_rev
  ilel = instruction_else_branch {
    let ilite = (Some e, List.rev ilt, Pos.no_pos) :: ilel in
    parse_if_then_etc ilite
  }
| COMPUTE DOMAIN dom = symbol_list_with_pos SEMICOLON { ComputeDomain dom }
| COMPUTE CHAINING chain = symbol_with_pos SEMICOLON { ComputeChaining chain }
| COMPUTE TARGET target = symbol_with_pos SEMICOLON { ComputeTarget target }
| VERIFY DOMAIN dom = symbol_list_with_pos SEMICOLON
    {
      let expr = Mast.Literal (Mast.Float 1.0), Pos.no_pos in
      ComputeVerifs (dom, expr)
    }
| VERIFY DOMAIN dom = symbol_list_with_pos COLON
  WITH expr = with_pos(expression) SEMICOLON {
    ComputeVerifs (dom, expr)
  }
| PRINT args = with_pos(print_argument)* SEMICOLON
    { Print (StdOut, args) }
| PRINT_ERR args = with_pos(print_argument)* SEMICOLON
    { Print (StdErr, args) }
| ITERATE COLON it_params = nonempty_list(with_pos(it_param))
  IN LPAREN instrs = instruction_list_rev RPAREN
    {
      let err msg pos = Errors.raise_spanned_error msg pos in
      let fold (vno, vco, exo) = function
      | (Some vn, _, _), pos ->
          if vno = None then Some vn, vco, exo
          else err "iterator variable is already defined" pos
      | (_, Some vc, _), pos ->
          if vco = None then vno, Some vc, exo
          else err "variable category is already specified" pos
      | (_, _, Some ex), pos ->
          if exo = None then vno, vco, Some ex
          else err "iterator filter is already defined" pos
      | (_, _, _), _ -> assert false
      in
      let init = None, None, None in
      let vno, vco, exo = List.fold_left fold init it_params in
      let var =
        match vno with
        | Some var -> var
        | None -> err "iterator variable must be defined" (mk_position $sloc)
      in
      let vcats =
        match vco with
        | Some vcats -> vcats
        | None -> err "variable category must be defined" (mk_position $sloc)
      in
      let expr =
        match exo with
        | Some expr -> expr
        | None -> Mast.Literal (Mast.Float 1.0), Pos.no_pos
      in
      Iterate (var, vcats, expr, List.rev instrs)
    }
| RESTORE COLON rest_params = with_pos(rest_param)*
  AFTER LPAREN instrs = instruction_list_rev RPAREN {
    Restore (rest_params, List.rev instrs)
  }
| RAISE_ERROR e_name = symbol_with_pos var = with_pos(output_name)? SEMICOLON {
    RaiseError (e_name, var)
  }
| CLEAN_ERRORS SEMICOLON { CleanErrors }
| EXPORT_ERRORS SEMICOLON { ExportErrors }

instruction_else_branch:
| ELSEIF e = with_pos(expression)
  THEN ilt = instruction_list_rev
  ilel = instruction_else_branch {
    (Some e, List.rev ilt, mk_position $sloc) :: ilel
  }
| ELSE il = instruction_list_rev ENDIF {
    [None, List.rev il, mk_position $sloc]
  }
| ENDIF { [] }

print_argument:
| s = STRING { PrintString (parse_string s) }
| f = with_pos(print_function) LPAREN v = symbol_with_pos RPAREN
    {
      match Pos.unmark f with
      | "nom" -> PrintName (parse_variable $sloc (fst v), snd v)
      | "alias" -> PrintAlias (parse_variable $sloc (fst v), snd v)
      | _ -> assert false
    }
| INDENT LPAREN e = with_pos(expression) RPAREN { PrintIndent e }
| LPAREN e = with_pos(expression) RPAREN prec = print_precision?
    {
      match prec with
      | Some (min, max) -> PrintExpr (e, min, max)
      | None -> PrintExpr (e, 0, 20)
    }

print_function:
| NAME { "nom" }
| ALIAS { "alias" }

print_precision:
| COLON min = symbol_with_pos
    {
      let min_str, min_pos = min in
      let min_val =
        try int_of_string min_str with
        | Failure _ -> Errors.raise_spanned_error "should be an integer" min_pos
      in
      (if min_val < 0 then
        Errors.raise_spanned_error "precision must be positive" min_pos);
      (min_val, min_val)
    }
| COLON min = symbol_with_pos RANGE max = symbol_with_pos
    {
      let min_str, min_pos = min in
      let min_val =
        try int_of_string min_str with
        | Failure _ -> Errors.raise_spanned_error "should be an integer" min_pos
      in
      (if min_val < 0 then
        Errors.raise_spanned_error "precision must be positive" min_pos);
      let max_str, max_pos = max in
      let max_val =
        try int_of_string max_str with
        | Failure _ -> Errors.raise_spanned_error "should be an integer" max_pos
      in
      (if max_val < 0 then
        Errors.raise_spanned_error "precision must be positive" max_pos);
      (if max_val < min_val then
        Errors.raise_spanned_error
          "maximum precision must be smaller than minimum precision"
          max_pos);
      (min_val, max_val)
    }

it_param:
| VARIABLE var = symbol_with_pos COLON
    { Some var, None, None }
| CATEGORY vcats = separated_nonempty_list(COMMA, with_pos(var_category_id))
  COLON {
    None, Some vcats, None
  }
| WITH expr = with_pos(expression) COLON
    { None, None, Some expr }

rest_param:
| vars = separated_nonempty_list(COMMA, symbol_with_pos) COLON { VarList vars }
| VARIABLE var = symbol_with_pos COLON
  CATEGORY vcats = separated_nonempty_list(COMMA, with_pos(var_category_id))
  COLON expr_opt = rest_param_with_expr? { 
    let expr =
      match expr_opt with
      | Some expr -> expr
      | None -> Mast.Literal (Mast.Float 1.0), Pos.no_pos
    in
    VarCats (var, vcats, expr)
  }

rest_param_with_expr:
| WITH expr = with_pos(expression) COLON { expr }

formula_list_etc:
| f = with_pos(formula_kind) SEMICOLON l = with_pos(symbol_colon_etc)* { [f], l }
| f = with_pos(formula_kind) SEMICOLON fs = formula_list_etc {
    let fl, l = fs in
    f :: fl, l
  }

formula_kind:
| f = formula { SingleFormula f }
| fs = for_formula { let (lv, ft) = fs in MultipleFormulaes (lv, ft) }

for_formula:
| FOR lv = with_pos(loop_variables) COLON ft = formula { (lv, ft) }

lvalue_name:
| s = SYMBOL { parse_variable $sloc s }

lvalue:
| s = with_pos(lvalue_name) i = with_pos(brackets)? { { var = s; index = i} }

formula:
| lvalue = with_pos(lvalue) EQUALS formula = with_pos(expression) {
    { lvalue; formula }
  }

verification_etc:
| v = with_pos(verification) l = with_pos(symbol_colon_etc)* { v :: l }

verification:
| VERIFICATION name = symbol_list_with_pos
  COLON verif_applications = application_reference SEMICOLON
  verif_conditions = with_pos(verification_condition)+ {
    let num, verif_tag_names =
      let uname = Pos.unmark name in
      let begPos =
        match uname with
        | h :: _ -> Pos.get_position h
        | [] -> assert false
      in
      let rec aux tags endPos = function
      | [num] ->
           let pos = Pos.make_position_between begPos endPos in
           num, (tags, pos)
      | h :: t -> aux (h :: tags) (Pos.get_position h) t
      | [] -> assert false
      in
      aux [] begPos uname
    in
    let verif_number =
      try Pos.map_under_mark int_of_string num
      with _ ->
        Errors.raise_spanned_error
          "this verification doesn't have an execution number"
          (Pos.get_position num)
    in
    let verif = {
      verif_number;
      verif_tag_names;
      verif_applications;
      verif_conditions
    } in
    Verification verif
  }

verification_condition:
| IF e = with_pos(expression) THEN
  ERROR e_name = symbol_with_pos var = with_pos(output_name)? SEMICOLON {
    {
      verif_cond_expr = e;
      verif_cond_error = e_name, var;
    }
  }

error_name:
n = SYMBOL COLON { n }

error_descr:
s = STRING { parse_string s }

error_message:
| COLON  s = with_pos(error_descr) { s }

error_:
| n = with_pos(error_name) t = with_pos(type_error)
  COLON s1 = with_pos(error_descr)
  COLON s2 = with_pos(error_descr)
  COLON s3 = with_pos(error_descr)
  COLON s4 = with_pos(error_descr)
  s5 = error_message? SEMICOLON {
    let err = {
      error_name = n;
      error_typ = t;
      error_descr =
        let s5l = match s5 with None -> [] | Some s -> [s] in
        s1 :: s2 :: s3 :: s4 :: s5l;
    } in
    Error err
  }

type_error:
| ANOMALY { Anomaly }
| DISCORDANCE { Discordance }
| INFORMATIVE { Information }


output_etc:
| o = with_pos(output) l = with_pos(symbol_colon_etc)* { o :: l }

output:
| OUTPUT LPAREN s = with_pos(output_name) RPAREN SEMICOLON { Output s }

output_name:
| s = SYMBOL { parse_variable_name $sloc s }

brackets:
| LBRACKET i = SYMBOL RBRACKET { parse_table_index $sloc i }

loop_variables:
| lrs = loop_variables_ranges { Ranges lrs }
| lvs = loop_variables_values { ValueSets lvs }

loop_variables_values:
| lvs = separated_nonempty_list(SEMICOLON, loop_variables_value) { lvs }

loop_variable_value_name:
| s = SYMBOL { parse_parameter $sloc s }

loop_variables_value:
| s = with_pos(loop_variable_value_name) EQUALS e = enumeration_loop {
    let s, loc = s in ((s, loc), e)
  }

loop_variables_ranges:
| r = loop_variables_range { [r] }
| r = loop_variables_range AND rs = loop_variables_ranges { r::rs }

loop_variables_range:
| ONE s = with_pos(loop_variable_value_name) IN e = enumeration_loop {
   let (s, loc) = s in ((s, loc), e)
 }

enumeration_loop:
| i = enumeration_loop_item { [i] }
| i = enumeration_loop_item COMMA is = enumeration_loop { i::is }

enumeration_loop_item:
| bounds = interval_loop { bounds  }
| s = SYMBOL {
    let pos = mk_position $sloc in
    Single (parse_to_literal (parse_variable_or_int $sloc s), pos)
  }

range_or_minus:
| RANGE { `Range }
| MINUS { `Minus }

interval_loop:
| i1 = SYMBOL rm = range_or_minus i2 = SYMBOL {
    let pos = mk_position $sloc in
    let l1 = parse_to_literal (parse_variable_or_int $sloc i1), pos in
    let l2 = parse_to_literal (parse_variable_or_int $sloc i2), pos in
    match rm with
    | `Range -> Range (l1, l2)
    | `Minus -> Interval (l1, l2)
  }

enumeration:
| i = enumeration_item { [i] }
| i = enumeration_item COMMA is = enumeration { i::is }

enumeration_item:
| bounds = interval { bounds }
| s = SYMBOL {
    let pos = mk_position $sloc in
    match parse_variable_or_int $sloc s with
    | ParseVar v -> VarValue (v, pos)
    | ParseInt i -> FloatValue (float_of_int i, pos)
  }

interval:
| i1 = SYMBOL RANGE i2 = SYMBOL {
    let pos = mk_position $sloc in
    let ir1 = parse_int $sloc i1, pos in
    let ir2 = parse_int $sloc i2, pos in
    Interval (ir1, ir2) : set_value
  }
 (* Some intervals are "03..06" so we must keep the prefix "0" *)

expression:
| e = with_pos(sum_expression) NOTIN LPAREN s = enumeration RPAREN {
    TestInSet (false, e, s)
  }
| e = with_pos(sum_expression) IN LPAREN s = enumeration RPAREN {
    TestInSet (true, e, s)
  }
| e1 = with_pos(sum_expression)
  op = with_pos(comparison_op)
  e2 = with_pos(sum_expression) {
    Comparison (op, e1, e2)
  }
| e = sum_expression { e }
| e1 = with_pos(expression)
  op = with_pos(logical_binop)
  e2 = with_pos(expression) {
    Binop (op, e1, e2)
  }
| FOR le = loop_expression { let l1, l2 = le in Loop (l1, l2) }
| NOT e = with_pos(expression) { Unop (Not, e) }

%inline logical_binop:
| AND { And }
| OR { Or }

sum_expression:
| e = product_expression { e }
| e1 = with_pos(sum_expression)
  op = with_pos(sum_operator)
  e2 = with_pos(product_expression) {
    Binop (op, e1, e2)
  }

%inline sum_operator:
| PLUS { Add }
| MINUS { Sub }

product_expression:
| e = factor { e }
| e1 = with_pos(product_expression)
  op = with_pos(product_operator)
  e2 = with_pos(factor) {
    Binop (op, e1, e2)
  }

%inline product_operator:
| TIMES { Mul }
| DIV { Div }

table_index_name:
s = SYMBOL { parse_variable $sloc s }

factor:
| MINUS e = with_pos(factor) { Unop (Minus, e) }
| e = ternary_operator { e }
| e = function_call { e }
| s = with_pos(table_index_name) i = with_pos(brackets) { Index (s, i) }
| l = factor_literal { Literal l }
| LPAREN e = expression RPAREN { e }

loop_expression:
| lvs = with_pos(loop_variables) COLON e = with_pos(expression) {
    lvs, e
  } %prec SEMICOLON

ternary_operator:
| IF e1 = with_pos(expression)
  THEN e2 = with_pos(expression)
  e3 = else_branch?
  ENDIF {
    Conditional (e1, e2, e3)
  }

else_branch:
| ELSE e = with_pos(expression) { e }

factor_literal:
| UNDEFINED { Mast.Undefined }
| s = SYMBOL { parse_literal $sloc s }(*
  Some symbols start with a digit and make it hard to parse with (float / integer / symbol)
  *)

function_name:
| VERIF_NUMBER { "numero_verif" }
| COMPL_NUMBER { "numero_compl" }
| s = SYMBOL { parse_func_name $sloc s }

function_call:
| NB_CATEGORY LPAREN cats = with_pos(var_category_id) RPAREN { NbCategory cats }
| ATTRIBUT LPAREN var = symbol_with_pos COMMA attr = symbol_with_pos RPAREN {
    Attribut ((parse_variable $sloc (fst var), snd var), attr)
  }
| SIZE LPAREN var = symbol_with_pos RPAREN {
    Size (parse_variable $sloc (fst var), snd var)
  }
| NB_ANOMALIES LPAREN RPAREN { NbAnomalies }
| NB_DISCORDANCES LPAREN RPAREN { NbDiscordances }
| NB_INFORMATIVES LPAREN RPAREN { NbInformatives }
| s = with_pos(function_name) LPAREN RPAREN {
    FunctionCall (s, Mast.ArgList [])
  }
| s = with_pos(function_name) LPAREN args = function_call_args RPAREN {
    FunctionCall (s, args)
  }

function_call_args:
| l = loop_expression { let l1, l2 = l in LoopList (l1, l2) }
| args = function_arguments { ArgList (args) }

function_arguments:
| e = with_pos(sum_expression) { [e] }
| e = with_pos(sum_expression) COMMA es = function_arguments { e :: es }

%inline comparison_op:
| GTE  { Gte }
| LTE  { Lte }
| LT { Lt }
| GT { Gt }
| NEQ  { Neq }
| EQUALS { Eq }

symbol_enumeration:
| ss = separated_nonempty_list(COMMA, symbol_with_pos) { ss }

(* This is for the function spec file *)

spec_input_list:
| NOT { [] }
| inputs = separated_nonempty_list(COMMA, symbol_with_pos)
  { List.map (fun s -> parse_variable_name $sloc (fst s), snd s) inputs }

spec_output_list:
| NOT { [] }
| outputs = separated_nonempty_list(COMMA, symbol_with_pos)
  { List.map (fun s -> parse_variable_name $sloc (fst s), snd s) outputs  }

const_input:
| var = symbol_with_pos EQUALS expr = with_pos(expression) SEMICOLON {
    ((parse_variable_name $sloc (fst var), snd var), expr)
  }

spec_const_list:
| NOT SEMICOLON { [] }
| const = const_input { [const] }
| const = const_input rest = spec_const_list { const::rest }

spec_conds_list:
| NOT SEMICOLON { [] }
| cond = with_pos(expression) SEMICOLON { [cond] }
| cond = with_pos(expression) SEMICOLON others = spec_conds_list {
    cond :: others
  }

function_spec:
| INPUT COLON inputs = spec_input_list SEMICOLON
  CONST COLON consts = spec_const_list
  CONDITION COLON precs = spec_conds_list
  OUTPUT COLON outputs = spec_output_list SEMICOLON {
    {
      spec_inputs = inputs;
      spec_consts = consts;
      spec_outputs = outputs;
      spec_conditions = precs;
    }
  }

| EOF {
  {
    spec_inputs = [];
    spec_consts = [];
    spec_outputs = [];
    spec_conditions = [];
  }
}

literal_input:
| l = factor_literal { l }

