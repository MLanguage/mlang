%{
 open Ast
 open Lexing
 open Parse_utils
%}

%token<string> SYMBOL STRING

%token PLUS MINUS TIMES DIV
%token GTE LTE GT LT NEQ EQUALS
%token SEMICOLON COLON COMMA
%token AND OR NOTIN NOT

%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token RANGE

%token BOOLEAN DATE_YEAR DATE_DAY_MONTH_YEAR DATE_MONTH INTEGER REAL
%token ONE IN APPLICATION CHAINING TYPE BASE GIVEN_BACK TABLE
%token COMPUTED CONST ALIAS CONTEXT FAMILY PENALITY INCOME INPUT FOR
%token RULE IF THEN ELSE ENDIF ERROR VERIFICATION ANOMALY DISCORDANCE
%token INFORMATIVE OUTPUT

%token EOF

%type <Ast.source_file> source_file

%nonassoc SEMICOLON
%left OR AND
%nonassoc NOT
%nonassoc SYMBOL

%start source_file

%%

source_file:
| i = source_file_item is = source_file { i::is }
| EOF { [] }


source_file_item:
| a = application { (Application a, mk_position $sloc) }
| c = chaining { let (s, aps) = c in (Chaining (s, aps), mk_position $sloc) }
| v = variable_decl { (Variable v, mk_position $sloc) }
| r = rule { (Rule r, mk_position $sloc) }
| ver = verification { (Verification ver, mk_position $sloc) }
| e = error_ { (Error e, mk_position $sloc) }
| o = output { (Output o, mk_position $sloc) }

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
| descr = STRING { (descr, mk_position $sloc) }

computed_variable:
| name = computed_variable_name size = computed_variable_table? COMPUTED
  subtyp = computed_variable_subtype*
  COLON descr = computed_variable_descr typ = value_type? SEMICOLON
  { ComputedVar ({
    comp_name = (let (name, nloc) = name in (name, nloc));
    comp_table = size;
    comp_subtyp = subtyp;
    comp_description = descr;
    comp_typ = typ;
  }, mk_position $sloc) }

computed_variable_table:
| TABLE LBRACKET size = SYMBOL RBRACKET { (int_of_string size, mk_position $sloc) }

computed_variable_subtype:
| BASE { (Base, mk_position $sloc) }
| GIVEN_BACK { (GivenBack, mk_position $sloc) }

input_variable_name:
| name = SYMBOL COLON { (parse_variable_name $sloc name, mk_position $sloc) }

input_descr:
descr = STRING { (descr, mk_position $sloc) }

input_variable:
| name = input_variable_name INPUT
  subtyp = input_variable_subtype
  attrs = input_variable_attribute*
  g = GIVEN_BACK? alias = input_variable_alias COLON descr = input_descr
  typ = value_type?
  SEMICOLON { InputVar ({
    input_name = name;
    input_subtyp = subtyp;
    input_attributes = attrs;
    input_given_back = (match g with Some _ -> true | None -> false);
    input_alias = alias;
    input_typ = typ;
    input_description = descr;
  }, mk_position $sloc) }

input_variable_alias:
| ALIAS alias = SYMBOL { (parse_variable_name $sloc alias, mk_position $sloc) }

input_variable_attribute_name:
| attr = SYMBOL { (attr, mk_position $sloc) }

input_variable_attribute_value:
 lit = SYMBOL { (parse_literal $sloc lit, mk_position $sloc) }

input_variable_attribute:
| attr = input_variable_attribute_name EQUALS
  lit = input_variable_attribute_value
{ (attr, lit) }

input_variable_subtype:
| CONTEXT { (Context, mk_position $sloc) }
| FAMILY { (Family, mk_position $sloc) }
| PENALITY { (Penality, mk_position $sloc) }
| INCOME { (Income, mk_position $sloc) }

value_type:
| TYPE typ = value_type_prim { typ }

value_type_prim:
| BOOLEAN { (Boolean, mk_position $sloc) }
| DATE_YEAR { (DateYear, mk_position $sloc) }
| DATE_DAY_MONTH_YEAR { (DateDayMonthYear, mk_position $sloc) }
| DATE_MONTH { (DateMonth, mk_position $sloc) }
| INTEGER { (Integer, mk_position $sloc) }
| REAL { (Real, mk_position $sloc) }

rule_name_symbol:
name = SYMBOL { (name, mk_position $sloc) }

rule:
| RULE name = rule_name_symbol+ COLON apps = application_reference
  SEMICOLON c = chaining_reference?
  formulaes = formula_list
  { {
      rule_name = name;
      rule_applications = apps;
      rule_chaining = c;
      rule_formulaes = formulaes;
    }
  }

formula_list:
| f = formula_kind SEMICOLON { [f] }
| f = formula_kind SEMICOLON fs = formula_list { f::fs }

formula_kind:
| f = formula_one { (SingleFormula f, mk_position $sloc) }
| fs = for_formula
  { let (lv, ft) = fs in (MultipleFormulaes (lv, ft), mk_position $sloc) }

for_formula:
| FOR lv = loop_variables COLON ft = formula_two { (lv, ft) }

lvalue:
| s = variable_generic i = brackets? { ({ var = Generic s; index = i}, mk_position $sloc) }

formula_two:
| l = lvalue EQUALS e = expression { {
    lvalue = l;
    formula =  e
  } }

formula_one:
| l = lvalue EQUALS e = expression { {
    lvalue = l;
    formula =  e
  } }

variable_generic:
| s = SYMBOL { parse_variable_generic_name $sloc s }

verification_name:
| name = SYMBOL { (name, mk_position $sloc) }

verification:
| VERIFICATION name = verification_name+ COLON apps = application_reference
  SEMICOLON conds = verification_condition+ { {
  verif_name = name;
  verif_applications = apps;
  verif_conditions = conds;
} }

verification_condition:
| IF e = expression THEN ERROR e_names = verification_name+ SEMICOLON { ({
    verif_cond_expr = e;
    verif_cond_errors = e_names;
  }, mk_position $sloc) }

error_name:
n = SYMBOL COLON { (n, mk_position $sloc) }

error_descr:
s = STRING { (s, mk_position $sloc) }

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
s = SYMBOL { (parse_variable_generic_name $sloc s, mk_position $sloc) }

loop_variables_value:
| s = loop_variable_value_name EQUALS e = enumeration {
    let (s,loc) = s in ((Generic s, loc), e)
  }

loop_variables_ranges:
| r = loop_variables_range { [r] }
| r = loop_variables_range AND rs = loop_variables_ranges { r::rs }

loop_variables_range:
| ONE s = loop_variable_value_name IN e = enumeration {
   let (s, loc) = s in ((Generic s, loc), e)
 }

enumeration:
| i = enumeration_item { [i] }
| i = enumeration_item COMMA is = enumeration { i::is }

enumeration_item:
| bounds = interval { bounds  }
| s = SYMBOL { VarValue (parse_variable $sloc s, mk_position $sloc) }

interval:
| i1 = SYMBOL RANGE i2 = SYMBOL
 { Interval ((parse_int $sloc i1, mk_position $sloc),
   (parse_int $sloc i2, mk_position $sloc)) }
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

logical_binop:
| AND { (And, mk_position $sloc) }
| OR { (And, mk_position $sloc) }


sum_expression:
| e = product_expression { e }
| e1 = product_expression op = sum_operator e2 = sum_expression { (Binop (op, e1, e2), mk_position $sloc) }

sum_operator:
| PLUS { (Add, mk_position $sloc) }
| MINUS { (Sub, mk_position $sloc) }

product_expression:
| e = factor { e }
| e1 = factor op = product_operator e2 = product_expression
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
| lvs = loop_variables COLON e = expression { (lvs, e, mk_position $sloc) }

ternary_operator:
| IF e1 = expression THEN e2 = expression e3 = else_branch? ENDIF
{ (Conditional (e1, e2, e3), mk_position $sloc) }

else_branch:
| ELSE e = expression { e }

factor_literal:
| s = SYMBOL { parse_literal $sloc s }(*
  Some symbols start with a digit and make it hard to parse with (float / integer / symbol)
  *)

function_call:
| s = SYMBOL LPAREN args = function_call_args RPAREN
  { (FunctionCall (parse_func_name $sloc s, args), mk_position $sloc) }

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
