%{
 open Ast
 open Lexing
 open Cli
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
| enumeration_item { () }
| enumeration_item COMMA enumeration { () }

enumeration_item:
| interval { () }
| SYMBOL { () }

interval:
| SYMBOL RANGE SYMBOL { () } (* Some intervals are "03..06" so we must keep the prefix "0" *)

expression:
| sum_expression NOTIN LPAREN enumeration RPAREN { () }
| sum_expression IN LPAREN enumeration RPAREN { () }
| sum_expression comparison_op sum_expression { () }
| sum_expression { () }
| expression AND expression { () }
| expression OR expression { () }
| NOT expression { () }

sum_expression:
| product_expression { () }
| product_expression sum_operator sum_expression { () }

sum_operator:
| PLUS { () }
| MINUS { () }

product_expression:
| factor { () }
| factor product_operator product_expression { () }

product_operator:
| TIMES { () }
| DIV { () }

factor:
| FOR loop_expression { () }
| MINUS factor { () }
| ternary_operator { () }
| function_call { () }
| SYMBOL brackets { () }
| factor_literal { () }
| LPAREN expression RPAREN { () }

loop_expression:
| loop_variables COLON expression { () }

ternary_operator:
| IF expression THEN expression else_branch? ENDIF { () }

else_branch:
| ELSE expression { () }

factor_literal:
| SYMBOL { () }(*
  Some symbols start with a digit and make it hard to parse with (float / integer / symbol)
  *)

function_call:
| SYMBOL LPAREN function_call_args RPAREN { () }

function_call_args:
| loop_expression { () }
| function_arguments { () }

function_arguments:
| sum_expression { () }
| sum_expression COMMA function_arguments { () }


comparison_op:
| GTE  { () }
| LTE  { () }
| LT { () }
| GT { () }
| NEQ  { () }
| EQUALS { () }

symbol_enumeration:
| ss = separated_nonempty_list(COMMA, SYMBOL) { ss }
