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
| a = application { Application a }
| c = chaining { Chaining c }
| v = variable_decl { Variable v }
| r = rule { Rule r }
| ver = verification { Verification ver }
| e = error_ { Error e }
| o = output { Output o }

application:
| APPLICATION s = SYMBOL SEMICOLON { () }

application_reference:
| APPLICATION COLON ss = symbol_enumeration { ss }

chaining:
| CHAINING s = SYMBOL application_reference SEMICOLON { () }

chaining_reference:
| CHAINING COLON c = SYMBOL SEMICOLON { c }

variable_decl:
| computed_variable  { () }
| const_variable  { () }
| input_variable  { () }

const_variable:
| SYMBOL COLON CONST EQUALS SYMBOL SEMICOLON { () }

computed_variable:
| SYMBOL COLON computed_variable_table? COMPUTED computed_variable_subtype*
  COLON STRING value_type? SEMICOLON { () }

computed_variable_table:
| TABLE LBRACKET SYMBOL RBRACKET { () }

computed_variable_subtype:
| BASE { () }
| GIVEN_BACK { () }

input_variable:
| SYMBOL COLON INPUT input_variable_subtype input_variable_attribute*
  GIVEN_BACK? input_variable_alias COLON STRING value_type?
  SEMICOLON { () }

input_variable_alias:
| ALIAS SYMBOL { () }

input_variable_attribute:
| SYMBOL EQUALS SYMBOL { () }

input_variable_subtype:
| CONTEXT { () }
| FAMILY { () }
| PENALITY { () }
| INCOME { () }

value_type:
| TYPE value_type_prim { () }

value_type_prim:
| BOOLEAN { () }
| DATE_YEAR { () }
| DATE_DAY_MONTH_YEAR { () }
| DATE_MONTH { () }
| INTEGER { () }
| REAL { () }

rule:
| RULE name = SYMBOL+ COLON apps = application_reference
  SEMICOLON c = chaining_reference?
  formulaes = formula_list
  { {
      name = name;
      applications = apps;
      chaining = c;
      formulaes = formulaes;
    }
  }

formula_list:
| f = formula_kind SEMICOLON { [f] }
| f = formula_kind SEMICOLON fs = formula_list { f::fs }

formula_kind:
| f = formula_one { SingleFormula f }
| fs = for_formula { MultipleFormulaes fs }

for_formula:
| FOR loop_variables COLON formula_two{ () }

formula_two:
| s = variable_generic i = brackets? EQUALS e = expression { () }

formula_one:
| s = variable i = brackets? EQUALS e = expression { {
    variable = s;
    index = i;
    formula =  e
  } }

variable_generic:
| s = SYMBOL { parse_variable_generic_name $sloc s }


variable:
| s = SYMBOL { parse_variable_name $sloc s }

verification:
| VERIFICATION SYMBOL+ COLON application_reference
  SEMICOLON verification_condition+ { () }

verification_condition:
| IF expression THEN ERROR SYMBOL+ SEMICOLON { () }


error_:
| n = SYMBOL COLON type_error COLON STRING COLON
   STRING COLON STRING COLON STRING
   error_message? SEMICOLON { () }

error_message:
| COLON  STRING { () }

type_error:
| ANOMALY { () }
| DISCORDANCE { () }
| INFORMATIVE { () }

output:
| OUTPUT LPAREN s = SYMBOL RPAREN SEMICOLON { () }

brackets:
| LBRACKET i = SYMBOL RBRACKET { parse_table_index $sloc i }

loop_variables:
| loop_variables_ranges { () }
| loop_variables_values { () }

loop_variables_values:
| separated_nonempty_list(SEMICOLON, loop_variables_value) { () }

loop_variables_value:
| SYMBOL EQUALS enumeration { () }

loop_variables_ranges:
| loop_variables_range { () }
| loop_variables_range AND loop_variables_ranges { () }

loop_variables_range:
| ONE SYMBOL IN enumeration { () }

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
| FOR loop_expression { Loop () }
| MINUS e = factor { Unop (Minus, e) }
| e = ternary_operator { e }
| e = function_call { e }
| s = SYMBOL i = brackets { Index (parse_variable $sloc s, i) }
| l = factor_literal { Literal l }
| LPAREN e = expression RPAREN { e }

loop_expression:
| loop_variables COLON expression { () }

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

symbol_enumeration:
| ss = separated_nonempty_list(COMMA, SYMBOL) { ss }
