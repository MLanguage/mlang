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

const_variable:
| name = SYMBOL COLON CONST EQUALS value = SYMBOL SEMICOLON
  { ConstVar (name, parse_literal $sloc value) }

computed_variable:
| name = SYMBOL COLON size = computed_variable_table? COMPUTED
  subtyp = computed_variable_subtype*
  COLON descr = STRING typ = value_type? SEMICOLON
  { ComputedVar {
    comp_name = Normal (parse_variable_name $sloc name);
    comp_table = size;
    comp_subtyp = subtyp;
    comp_description = descr;
    comp_typ = typ;
  } }

computed_variable_table:
| TABLE LBRACKET size = SYMBOL RBRACKET { int_of_string size }

computed_variable_subtype:
| BASE { Base }
| GIVEN_BACK { GivenBack }

input_variable:
| name = SYMBOL COLON INPUT subtyp = input_variable_subtype
  attrs = input_variable_attribute*
  g = GIVEN_BACK? alias = input_variable_alias COLON descr = STRING
  typ = value_type?
  SEMICOLON { InputVar {
    input_name = parse_variable_name $sloc name;
    input_subtyp = subtyp;
    input_attributes = attrs;
    input_given_back = (match g with Some _ -> true | None -> false);
    input_alias = alias;
    input_typ = typ;
    input_description = descr;
  } }

input_variable_alias:
| ALIAS alias = SYMBOL { parse_variable_name $sloc alias }

input_variable_attribute:
| attr = SYMBOL EQUALS lit = SYMBOL { (attr, parse_literal $sloc lit) }

input_variable_subtype:
| CONTEXT { Context }
| FAMILY { Family }
| PENALITY { Penality }
| INCOME { Income }

value_type:
| TYPE typ = value_type_prim { typ }

value_type_prim:
| BOOLEAN { Boolean }
| DATE_YEAR { DateYear }
| DATE_DAY_MONTH_YEAR { DateDayMonthYear }
| DATE_MONTH { DateMonth }
| INTEGER { Integer }
| REAL { Real }

rule:
| RULE name = SYMBOL+ COLON apps = application_reference
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
| f = formula_one { SingleFormula f }
| fs = for_formula { let (lv, ft) = fs in MultipleFormulaes (lv, ft) }

for_formula:
| FOR lv = loop_variables COLON ft = formula_two { (lv, ft) }

formula_two:
| s = variable_generic i = brackets? EQUALS e = expression { {
    lvalue = { var = Generic s; index = i} ;
    formula =  e
  } }

formula_one:
| s = variable i = brackets? EQUALS e = expression { {
    lvalue = { var = Normal s; index = i} ;
    formula =  e
  } }

variable_generic:
| s = SYMBOL { parse_variable_generic_name $sloc s }


variable:
| s = SYMBOL { parse_variable_name $sloc s }

verification:
| VERIFICATION name = SYMBOL+ COLON apps = application_reference
  SEMICOLON conds = verification_condition+ { {
  verif_name = name;
  verif_applications = apps;
  verif_conditions = conds;
} }

verification_condition:
| IF e = expression THEN ERROR e_names = SYMBOL+ SEMICOLON { {
    verif_cond_expr = e;
    verif_cond_errors = e_names;
  } }


error_:
| n = SYMBOL COLON t = type_error COLON s1 = STRING COLON
   s2 = STRING COLON s3 = STRING COLON s4 = STRING
   s5 = error_message? SEMICOLON { {
      error_name = n;
      error_typ = t;
      error_descr = s1::s2::s3::s4::(match s5 with
        | None -> []
        | Some s5 -> [s5]);
    } }

error_message:
| COLON  s = STRING { s }

type_error:
| ANOMALY { Anomaly }
| DISCORDANCE { Discordance }
| INFORMATIVE { Information }

output:
| OUTPUT LPAREN s = SYMBOL RPAREN SEMICOLON { parse_variable_name $sloc s }

brackets:
| LBRACKET i = SYMBOL RBRACKET { parse_table_index $sloc i }

loop_variables:
| lrs = loop_variables_ranges { Ranges lrs }
| lvs = loop_variables_values { ValueSets lvs }

loop_variables_values:
| lvs = separated_nonempty_list(SEMICOLON, loop_variables_value) { lvs }

loop_variables_value:
| s = SYMBOL EQUALS e = enumeration {
    (Generic (parse_variable_generic_name $sloc s), e)
  }

loop_variables_ranges:
| r = loop_variables_range { [r] }
| r = loop_variables_range AND rs = loop_variables_ranges { r::rs }

loop_variables_range:
| ONE s = SYMBOL IN e = enumeration {
   ((Generic (parse_variable_generic_name $sloc s), e))
 }

enumeration:
| i = enumeration_item { [i] }
| i = enumeration_item COMMA is = enumeration { i::is }

enumeration_item:
| bounds = interval { bounds  }
| s = SYMBOL { VarValue (parse_variable $sloc s) }

interval:
| i1 = SYMBOL RANGE i2 = SYMBOL
 { Interval (parse_int $sloc i1, parse_int $sloc i2) }
 (* Some intervals are "03..06" so we must keep the prefix "0" *)

expression:
| e = sum_expression NOTIN LPAREN s = enumeration RPAREN { TestInSet (false, e, s) }
| e = sum_expression IN LPAREN s = enumeration RPAREN { TestInSet (true, e, s) }
| e1 = sum_expression op = comparison_op e2 = sum_expression { Comparison (op, e1, e2) }
| e = sum_expression { e }
| e1 = expression AND e2 = expression { Binop (And, e1, e2) }
| e1 = expression OR e2 = expression { Binop (Or, e1, e2) }
| NOT e = expression { Unop (Not, e) }

sum_expression:
| e = product_expression { e }
| e1 = product_expression op = sum_operator e2 = sum_expression { Binop (op, e1, e2) }

sum_operator:
| PLUS { Add }
| MINUS { Sub }

product_expression:
| e = factor { e }

| e1 = factor op = product_operator e2 = product_expression { Binop (op, e1, e2) }
product_operator:
| TIMES { Mul }
| DIV { Div }

factor:
| FOR le =  loop_expression { le }
| MINUS e = factor { Unop (Minus, e) }
| e = ternary_operator { e }
| e = function_call { e }
| s = SYMBOL i = brackets { Index (parse_variable $sloc s, i) }
| l = factor_literal { Literal l }
| LPAREN e = expression RPAREN { e }

loop_expression:
| lvs = loop_variables COLON e = expression { Loop (lvs, e) }

ternary_operator:
| IF e1 = expression THEN e2 = expression e3 = else_branch? ENDIF
{ Conditional (e1, e2, e3) }

else_branch:
| ELSE e = expression { e }

factor_literal:
| s = SYMBOL { parse_literal $sloc s }(*
  Some symbols start with a digit and make it hard to parse with (float / integer / symbol)
  *)

function_call:
| s = SYMBOL LPAREN args = function_call_args RPAREN
  { FunctionCall (parse_func_name $sloc s, args) }

function_call_args:
| loop_expression { LoopList () }
| args = function_arguments { ArgList (args) }

function_arguments:
| e = sum_expression { [e] }
| e = sum_expression COMMA es = function_arguments { e::es }


comparison_op:
| GTE  { Gte }
| LTE  { Lte }
| LT { Lt }
| GT { Gt }
| NEQ  { Neq }
| EQUALS { Eq }

marked_symbol:
| s = SYMBOL { (s, mk_position $sloc) }

symbol_enumeration:
| ss = separated_nonempty_list(COMMA, marked_symbol) { ss }
