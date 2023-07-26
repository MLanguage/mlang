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
%token<char> PARAMETER

%token PLUS MINUS TIMES DIV
%token GTE LTE GT LT NEQ EQUALS
%token SEMICOLON COLON COMMA
%token AND OR NOTIN NOT

%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token RANGE

%token BOOLEAN DATE_YEAR DATE_DAY_MONTH_YEAR DATE_MONTH INTEGER REAL
%token ONE IN APPLICATION CHAINING TYPE TABLE
%token COMPUTED CONST ALIAS INPUT FOR
%token RULE IF THEN ELSE ENDIF ERROR VERIFICATION ANOMALY DISCORDANCE CONDITION
%token INFORMATIVE OUTPUT FONCTION VARIABLE ATTRIBUT
%token DOMAIN SPECIALIZE AUTHORIZE BASE GIVEN_BACK COMPUTABLE BY_DEFAULT

%token EOF

%type<Mast.source_file> source_file
%type<Mast.function_spec> function_spec
%type<Mast.literal> literal_input

%nonassoc SEMICOLON
%left OR
%left AND
%nonassoc NOT
%nonassoc SYMBOL

%start source_file
%start function_spec
%start literal_input

%%

source_file:
| i = source_file_item is = source_file { i::is }
| EOF { [] }

source_file_item:
| a = application { (Application a, mk_position $sloc) }
| c = chaining { let (s, aps) = c in (Chaining (s, aps), mk_position $sloc) }
| v = variable_decl { (VariableDecl v, mk_position $sloc) }
| r = rule { (Rule r, mk_position $sloc) }
| ver = verification { (Verification ver, mk_position $sloc) }
| e = error_ { (Error e, mk_position $sloc) }
| o = output { (Output o, mk_position $sloc) }
| fonction { (Function, mk_position $sloc) }
| c = var_category_decl { (VarCatDecl c, mk_position $sloc) }
| cr = rule_domain_decl { (RuleDomDecl cr, mk_position $sloc) }
| cv = verif_domain_decl { (VerifDomDecl cv, mk_position $sloc) }

%inline symbol_with_pos:
| s = SYMBOL { (s, mk_position $sloc) }

%inline symbol_list_with_pos:
| sl = nonempty_list(symbol_with_pos) { (sl, mk_position $sloc) }

var_category_decl:
| VARIABLE var_type = var_typ c = symbol_with_pos* COLON ATTRIBUT
  attr = separated_nonempty_list(COMMA, symbol_with_pos) SEMICOLON
  {
    ({
       var_type;
       var_category = c;
       var_attributes = attr;
      },
     mk_position $sloc)
  }

var_typ:
| INPUT { Input }
| COMPUTED { Computed }

rule_domain_decl:
| DOMAIN RULE rdom_params = separated_nonempty_list(COLON, rdom_param_with_pos) SEMICOLON
  {
    let err msg pos = Errors.raise_spanned_error msg pos in
    let fold (dno, dso, dco, dpdo) = function
    | Some dn, _, _, _, pos ->
        if dno = None then Some dn, dso, dco, dpdo
        else err "rule domain names are already defined" pos
    | _, Some ds, _, _, pos ->
        if dso = None then dno, Some ds, dco, dpdo
        else err "rule domain specialization is already specified" pos
    | _, _, Some dc, _, pos ->
        if dco = None then dno, dso, Some dc, dpdo
        else err "rule domain is already calculated" pos
    | _, _, _, Some dpd, pos ->
        if dpdo = None then dno, dso, dco, Some dpd
        else err "rule domain is already defined by defaut" pos
    | _, _, _, _, _ -> assert false
    in
    let init = None, None, None, None in
    let dno, dso, dco, dpdo = List.fold_left fold init rdom_params in
    let dom_names =
      match dno with
      | None -> err "rule domain names must be defined" (mk_position $sloc)
      | Some dn -> dn
    in
    {
      dom_names;
      dom_parents = (match dso with None -> [] | Some ds -> ds);
      dom_by_default = (match dpdo with None -> false | _ -> true);
      dom_data = {rdom_computable = (match dco with None -> false | _ -> true)};
    }
  }

rdom_param_with_pos:
| rdom_names = separated_nonempty_list(COMMA, symbol_list_with_pos)
  { (Some rdom_names, None, None, None, mk_position $sloc) }
| SPECIALIZE rdom_parents = separated_nonempty_list(COMMA, symbol_list_with_pos)
  { (None, Some rdom_parents, None, None, mk_position $sloc) }
| COMPUTABLE
  { (None, None, Some (), None, mk_position $sloc) }
| BY_DEFAULT
  { (None, None, None, Some (), mk_position $sloc) }

verif_domain_decl:
| DOMAIN VERIFICATION vdom_params = separated_nonempty_list(COLON, vdom_param_with_pos) SEMICOLON
  {
    let err msg pos = Errors.raise_spanned_error msg pos in
    let fold (dno, dso, dvo, dpdo) = function
    | Some dn, _, _, _, pos ->
        if dno = None then Some dn, dso, dvo, dpdo
        else err "verif domain names are already defined" pos
    | _, Some ds, _, _, pos ->
        if dso = None then dno, Some ds, dvo, dpdo
        else err "verif domain specialization is already specified" pos
    | _, _, Some dv, _, pos ->
        if dvo = None then dno, dso, Some dv, dpdo
        else err "verif domain authorization is already specified" pos
    | _, _, _, Some dpd, pos ->
        if dpdo = None then dno, dso, dvo, Some dpd
        else err "verif domain is already defined by defaut" pos
    | _, _, _, _, _ -> assert false
    in
    let init = None, None, None, None in
    let dno, dso, dvo, dpdo = List.fold_left fold init vdom_params in
    let dom_names =
      match dno with
      | None -> err "rule domain names must be defined" (mk_position $sloc)
      | Some dn -> dn
    in
    {
      dom_names;
      dom_parents = (match dso with None -> [] | Some ds -> ds);
      dom_by_default = (match dpdo with None -> false | _ -> true);
      dom_data = { vdom_auth = (match dvo with None -> [] | Some dv -> dv) };
    }
  }

%inline var_computed_category:
| BASE { ("base", mk_position $sloc) }
| GIVEN_BACK { ("restituee", mk_position $sloc) }
| TIMES { ("*", mk_position $sloc) }

%inline var_computed_category_list:
| l = var_computed_category* { (l, mk_position $sloc) }

var_category_id:
| INPUT l = symbol_list_with_pos { AuthInput l }
| COMPUTED l = var_computed_category_list { AuthComputed l }
| TIMES { AuthAll }

vdom_param_with_pos:
| vdom_names = separated_nonempty_list(COMMA, symbol_list_with_pos)
  { (Some vdom_names, None, None, None, mk_position $sloc) }
| SPECIALIZE vdom_parents = separated_nonempty_list(COMMA, symbol_list_with_pos)
  { (None, Some vdom_parents, None, None, mk_position $sloc) }
| AUTHORIZE vcats = separated_nonempty_list(COMMA, var_category_id)
  { (None, None, Some vcats, None, mk_position $sloc) }
| BY_DEFAULT
  { (None, None, None, Some (), mk_position $sloc) }

fonction:
| SYMBOL COLON FONCTION SYMBOL SEMICOLON { () }

application_name:
| s = SYMBOL { (s, mk_position $sloc) }

application:
| APPLICATION s = application_name SEMICOLON { s }

application_reference:
| APPLICATION COLON ss = symbol_enumeration { ss }

chaining:
| CHAINING s = SYMBOL aps = application_reference SEMICOLON { (s, aps) }

chaining_reference_name:
| c = SYMBOL { (c, mk_position $sloc) }

chaining_reference:
| CHAINING COLON c = chaining_reference_name SEMICOLON { c }

variable_decl:
| v = computed_variable  { v }
| v = const_variable  { v }
| v = input_variable  { v }

const_variable_name:
| name = SYMBOL COLON CONST { (parse_variable_name $sloc name, mk_position $sloc)}

const_variable_value:
| value = SYMBOL { (parse_literal $sloc value, mk_position $sloc) }

const_variable:
| name = const_variable_name EQUALS value = const_variable_value SEMICOLON
  { ConstVar (name, value) }

computed_variable_name:
| name = SYMBOL COLON { (parse_variable_name $sloc name, mk_position $sloc)}

computed_variable_descr:
| descr = STRING { (parse_string descr, mk_position $sloc) }

computed_attr_or_subtyp:
| attr = variable_attribute { let (x, y) = attr in Attr (x,y) }
| cat = symbol_with_pos { CompSubTyp cat }
| BASE { CompSubTyp ("base", mk_position $sloc) }
| GIVEN_BACK { CompSubTyp ("restituee", mk_position $sloc) }

computed_variable:
| name = computed_variable_name size = computed_variable_table? COMPUTED
  subtyp = computed_attr_or_subtyp*
  COLON descr = computed_variable_descr typ = value_type? SEMICOLON
  { ComputedVar ({
    comp_name = (let (name, nloc) = name in (name, nloc));
    comp_table = size;
    comp_attributes = List.map (fun x -> match x with Attr (x, y) -> (x, y) | _ -> assert false (* should not happen *))
        (List.filter (fun x -> match x with Attr _ -> true | _ -> false) subtyp);
    comp_category = List.map (fun x -> match x with CompSubTyp x -> x | _ -> assert false (* should not happen *))
        (List.filter (fun x -> match x with CompSubTyp _ -> true | _ -> false) subtyp);
    comp_is_givenback = List.exists (fun x -> match x with CompSubTyp ("restituee", _) -> true | _ -> false) subtyp;
    comp_description = descr;
    comp_typ = typ;
  }, mk_position $sloc) }

computed_variable_table:
| TABLE LBRACKET size = SYMBOL RBRACKET { (int_of_string size, mk_position $sloc) }

input_variable_name:
| name = SYMBOL COLON { (parse_variable_name $sloc name, mk_position $sloc) }

input_descr:
descr = STRING { (parse_string descr, mk_position $sloc) }

input_attr_or_category:
| attr = variable_attribute { (None, Some attr, false) }
| cat = symbol_with_pos { (Some cat, None, false) }
| GIVEN_BACK { None, None, true }

input_variable:
| name = input_variable_name INPUT
  category_attrs = input_attr_or_category* alias = input_variable_alias COLON descr = input_descr
  typ = value_type?
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
  InputVar ({
    input_name = name;
    input_category = category;
    input_attributes = attrs;
    input_alias = alias;
    input_is_givenback = givenback;
    input_typ = typ;
    input_description = descr;
  }, mk_position $sloc) }

input_variable_alias:
| ALIAS alias = SYMBOL { (parse_variable_name $sloc alias, mk_position $sloc) }

variable_attribute_name:
| attr = SYMBOL { (attr, mk_position $sloc) }

variable_attribute_value:
 lit = SYMBOL { (parse_literal $sloc lit, mk_position $sloc) }

variable_attribute:
| attr = variable_attribute_name EQUALS
  lit = variable_attribute_value
{ (attr, lit) }

value_type:
| TYPE typ = value_type_prim { typ }

value_type_prim:
| BOOLEAN { (Boolean, mk_position $sloc) }
| DATE_YEAR { (DateYear, mk_position $sloc) }
| DATE_DAY_MONTH_YEAR { (DateDayMonthYear, mk_position $sloc) }
| DATE_MONTH { (DateMonth, mk_position $sloc) }
| INTEGER { (Integer, mk_position $sloc) }
| REAL { (Real, mk_position $sloc) }

rule:
| RULE name = symbol_list_with_pos COLON apps = application_reference
  SEMICOLON c = chaining_reference?
  formulaes = formula_list
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
    {
      rule_number;
      rule_tag_names;
      rule_applications = apps;
      rule_chaining = c;
      rule_formulaes = formulaes;
    }
  }

formula_list:
| f = formula_kind SEMICOLON { [f] }
| f = formula_kind SEMICOLON fs = formula_list { f::fs }

formula_kind:
| f = formula { (SingleFormula f, mk_position $sloc) }
| fs = for_formula
  { let (lv, ft) = fs in (MultipleFormulaes (lv, ft), mk_position $sloc) }

for_formula:
| FOR lv = loop_variables COLON ft = formula { (lv, ft) }


lvalue_name:
| s = SYMBOL { (parse_variable $sloc s, mk_position $sloc) }

lvalue:
| s = lvalue_name i = brackets? { ({ var = s; index = i}, mk_position $sloc) }

formula:
| l = lvalue EQUALS e = expression { {
    lvalue = l;
    formula =  e
  } }

verification_name:
| name = SYMBOL { (name, mk_position $sloc) }

verification:
| VERIFICATION name = symbol_list_with_pos COLON apps = application_reference
  SEMICOLON conds = verification_condition* {
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
    {
      verif_number;
      verif_tag_names;
      verif_applications = apps;
      verif_conditions = conds;
    } }

verification_condition:
| IF e = expression THEN
  ERROR e_name = verification_name var = output_name? SEMICOLON { ({
    verif_cond_expr = e;
    verif_cond_error = e_name, var;
  }, mk_position $sloc) }

error_name:
n = SYMBOL COLON { (n, mk_position $sloc) }

error_descr:
s = STRING { (parse_string s, mk_position $sloc) }

error_:
| n = error_name t = type_error COLON s1 = error_descr COLON
   s2 = error_descr COLON s3 = error_descr COLON s4 = error_descr
   s5 = error_message? SEMICOLON { {
      error_name = n;
      error_typ = t;
      error_descr = s1::s2::s3::s4::(match s5 with
        | None -> []
        | Some s5 -> [s5]);
    } }

error_message:
| COLON  s = error_descr { s }

type_error:
| ANOMALY { (Anomaly, mk_position $sloc) }
| DISCORDANCE { (Discordance, mk_position $sloc) }
| INFORMATIVE { (Information, mk_position $sloc) }

output_name:
s = SYMBOL { (parse_variable_name $sloc s, mk_position $sloc) }

output:
| OUTPUT LPAREN s = output_name RPAREN SEMICOLON { s }

brackets:
| LBRACKET i = SYMBOL RBRACKET { (parse_table_index $sloc i, mk_position $sloc) }

loop_variables:
| lrs = loop_variables_ranges { (Ranges lrs, mk_position $sloc) }
| lvs = loop_variables_values { (ValueSets lvs, mk_position $sloc) }

loop_variables_values:
| lvs = separated_nonempty_list(SEMICOLON, loop_variables_value) { lvs }

loop_variable_value_name:
s = PARAMETER { (s, mk_position $sloc) }

loop_variables_value:
| s = loop_variable_value_name EQUALS e = enumeration_loop {
    let (s,loc) = s in ((s, loc), e)
  }

loop_variables_ranges:
| r = loop_variables_range { [r] }
| r = loop_variables_range AND rs = loop_variables_ranges { r::rs }

loop_variables_range:
| ONE s = loop_variable_value_name IN e = enumeration_loop {
   let (s, loc) = s in ((s, loc), e)
 }

 enumeration_loop:
 | i = enumeration_loop_item { [i] }
 | i = enumeration_loop_item COMMA is = enumeration_loop { i::is }

 enumeration_loop_item:
 | bounds = interval_loop { bounds  }
 | s = SYMBOL { Single (parse_to_literal (parse_variable_or_int $sloc s),
                        mk_position $sloc) }

range_or_minus:
| RANGE { `Range }
| MINUS { `Minus }

interval_loop:
| i1 = SYMBOL rm = range_or_minus i2 = SYMBOL {
    let l1 = parse_to_literal (parse_variable_or_int $sloc i1), mk_position $sloc in
    let l2 = parse_to_literal (parse_variable_or_int $sloc i2), mk_position $sloc in
    match rm with
    | `Range -> Range (l1, l2)
    | `Minus -> Interval (l1, l2)
  }

enumeration:
| i = enumeration_item { [i] }
| i = enumeration_item COMMA is = enumeration { i::is }

enumeration_item:
| bounds = interval { bounds  }
| s = SYMBOL {
    match parse_variable_or_int $sloc s with
    | ParseVar v -> VarValue (v, mk_position $sloc)
    | ParseInt i -> FloatValue (float_of_int i, mk_position $sloc)
}

interval:
| i1 = SYMBOL RANGE i2 = SYMBOL
 { Interval ((parse_int $sloc i1, mk_position $sloc),
   (parse_int $sloc i2, mk_position $sloc)) : set_value }
 (* Some intervals are "03..06" so we must keep the prefix "0" *)

expression:
| e = sum_expression NOTIN LPAREN s = enumeration RPAREN
  { (TestInSet (false, e, s), mk_position $sloc) }
| e = sum_expression IN LPAREN s = enumeration RPAREN
  { (TestInSet (true, e, s), mk_position $sloc) }
| e1 = sum_expression op = comparison_op e2 = sum_expression
  { (Comparison (op, e1, e2), mk_position $sloc) }
| e = sum_expression { e }
| e1 = expression op = logical_binop e2 = expression
  { (Binop (op, e1, e2), mk_position $sloc) }
| FOR le =  loop_expression { let (l1, l2, loc) = le in (Loop(l1,l2), loc) }
| NOT e = expression { (Unop (Not, e), mk_position $sloc) }

%inline logical_binop:
| AND { (And, mk_position $sloc) }
| OR { (Or, mk_position $sloc) }

sum_expression:
| e = product_expression { e }
| e1 = sum_expression op = sum_operator e2 = product_expression { (Binop (op, e1, e2), mk_position $sloc) }

sum_operator:
| PLUS { (Add, mk_position $sloc) }
| MINUS { (Sub, mk_position $sloc) }

product_expression:
| e = factor { e }
| e1 = product_expression op = product_operator e2 = factor
{ (Binop (op, e1, e2), mk_position $sloc) }

product_operator:
| TIMES { (Mul, mk_position $sloc) }
| DIV { (Div, mk_position $sloc) }

table_index_name:
s = SYMBOL { (parse_variable $sloc s, mk_position $sloc) }

factor:
| MINUS e = factor { (Unop (Minus, e), mk_position $sloc) }
| e = ternary_operator { e }
| e = function_call { e }
| s = table_index_name i = brackets { (Index (s, i), mk_position $sloc) }
| l = factor_literal { (Literal l, mk_position $sloc) }
| LPAREN e = expression RPAREN { e }

loop_expression:
| lvs = loop_variables COLON e = expression
  { (lvs, e, mk_position $sloc) } %prec SEMICOLON

ternary_operator:
| IF e1 = expression THEN e2 = expression e3 = else_branch? ENDIF
{ (Conditional (e1, e2, e3), mk_position $sloc) }

else_branch:
| ELSE e = expression { e }

factor_literal:
| s = SYMBOL { parse_literal $sloc s }(*
  Some symbols start with a digit and make it hard to parse with (float / integer / symbol)
  *)

function_name:
s = SYMBOL { (parse_func_name $sloc s, mk_position $sloc) }

function_call:
| s = function_name LPAREN args = function_call_args RPAREN
  { (FunctionCall (s, args), mk_position $sloc) }

function_call_args:
| l = loop_expression { let (l1, l2, _) = l in LoopList (l1, l2) }
| args = function_arguments { ArgList (args) }

function_arguments:
| e = sum_expression { [e] }
| e = sum_expression COMMA es = function_arguments { e::es }


comparison_op:
| GTE  { (Gte, mk_position $sloc) }
| LTE  { (Lte,  mk_position $sloc) }
| LT { (Lt,  mk_position $sloc) }
| GT { (Gt,  mk_position $sloc) }
| NEQ  { (Neq,  mk_position $sloc) }
| EQUALS { (Eq,  mk_position $sloc) }

marked_symbol:
| s = SYMBOL { (s, mk_position $sloc) }

symbol_enumeration:
| ss = separated_nonempty_list(COMMA, marked_symbol) { ss }

(* This is for the function spec file *)

spec_input_list:
| NOT { [] }
| inputs = separated_nonempty_list(COMMA, marked_symbol)
  { List.map (fun s -> parse_variable_name $sloc (fst s), snd s) inputs }

spec_output_list:
| NOT { [] }
| outputs = separated_nonempty_list(COMMA, marked_symbol)
  { List.map (fun s -> parse_variable_name $sloc (fst s), snd s) outputs  }

const_symbol:
| s = SYMBOL { (s, mk_position $sloc) }

const_input:
| var = const_symbol EQUALS expr = expression SEMICOLON { ((parse_variable_name $sloc (fst var), snd var), expr) }

spec_const_list:
| NOT SEMICOLON { [] }
| const = const_input { [const] }
| const = const_input rest = spec_const_list { const::rest }


spec_conds_list:
| NOT SEMICOLON { [] }
| cond = expression SEMICOLON { [cond] }
| cond = expression SEMICOLON others = spec_conds_list { cond::others }

function_spec:
| INPUT COLON inputs = spec_input_list SEMICOLON
  CONST COLON consts = spec_const_list
  CONDITION COLON precs = spec_conds_list
  OUTPUT COLON outputs = spec_output_list SEMICOLON
   { {
      spec_inputs = inputs;
      spec_consts = consts;
      spec_outputs = outputs;
      spec_conditions = precs;
   } }

| EOF { {
    spec_inputs = [];
    spec_consts = [];
    spec_outputs = [];
    spec_conditions = [];
 } }

 literal_input:
 | l = factor_literal { l }
