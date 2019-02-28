%{

%}

%token<string> FLOAT
%token<int> INT
%token<string> SYMBOL STRING

%token PLUS MINUS TIMES DIV
%token GTE LTE GT LT NEQ EQUALS
%token SEMICOLON COLON COMMA
%token AND OR NOTIN

%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token RANGE

%token BOOLEAN DATE_YEAR DATE_DAY_MONTH_YEAR DATE_MONTH INTEGER REAL
%token ONE IN APPLICATION CHAINING TYPE BASE GIVEN_BACK TABLE
%token COMPUTED CONST ALIAS CONTEXT FAMILY PENALITY INCOME INPUT FOR
%token RULE IF THEN ELSE ENDIF ERROR VERIFICATION ANOMALY DISCORDANCE
%token INFORMATIVE OUTPUT

%token EOF

%type <unit> source_file

%right SEMICOLON
%right EQUALS
%nonassoc SYMBOL

%start source_file

%%

source_file:
| source_file_item source_file { () }
| EOF { () }


source_file_item:
| application { () }
| chaining { () }
| variable { () }
| rule { () }
| verification { () }
| error_ { () }
| output { () }

application:
| APPLICATION s = SYMBOL SEMICOLON { () }

application_reference:
| APPLICATION COLON symbol_enumeration { () }

chaining:
| CHAINING s = SYMBOL application_reference SEMICOLON { () }

chaining_reference:
| CHAINING COLON SYMBOL SEMICOLON { () }

variable:
| computed_variable  { () }
| const_variable  { () }
| input_variable  { () }

const_variable:
| SYMBOL COLON CONST EQUALS FLOAT SEMICOLON { () }

computed_variable:
| SYMBOL COLON computed_variable_table? COMPUTED computed_variable_subtype*
  COLON STRING value_type? SEMICOLON { () }

computed_variable_table:
| TABLE LBRACKET INT RBRACKET { () }

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
| SYMBOL EQUALS INT { () }

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
| RULE symbol_or_int+ COLON application_reference SEMICOLON chaining_reference?
  formula_list { () }

formula_list:
| formula_kind SEMICOLON { () }
| formula_kind SEMICOLON formula_list { () }

formula_kind:
| formula { () }
| for_formula { () }

for_formula:
| FOR loop_variables COLON formula { () }

formula:
| SYMBOL brackets? EQUALS expression { () }

verification:
| VERIFICATION ; s = SYMBOL ; SYMBOL+ ; COLON ; application_reference ;
  SEMICOLON ; verification_condition ; verification_condition+ { () }

verification_condition:
| IF ; expression ; THEN ; ERROR ; SYMBOL+ ; SEMICOLON { () }


error_:
| n = SYMBOL ; type_error;  COLON ;  STRING ; COLON ;
   STRING ; COLON ;  STRING ; COLON ;  STRING ;
  SEMICOLON ; error_message? ; SEMICOLON { () }

error_message:
| COLON ;  STRING { () }

type_error:
| ANOMALY { () }
| DISCORDANCE { () }
| INFORMATIVE { () }

output:
| OUTPUT LPAREN s = SYMBOL RPAREN SEMICOLON { () }

brackets:
| LBRACKET SYMBOL RBRACKET { () }

loop_variables:
| separated_nonempty_list(AND, loop_variable1) { () }
| separated_nonempty_list(SEMICOLON, loop_variable2) { () }

loop_variable1:
| ONE SYMBOL IN enumeration { () }

loop_variable2:
| SYMBOL EQUALS enumeration { () }

enumeration:
| separated_nonempty_list(COMMA, enumeration_item) { () }

enumeration_item:
| interval { () }
| INT { () }
| SYMBOL { () }

interval:
| SYMBOL RANGE SYMBOL { () } (* Some intervals are "03..06" so we must keep the prefix "0" *)

expression:
| expression_item { () }
| expression_item AND expression { () }
| expression_item OR expression { () }

expression_item:
| comparison { () }

/* expression_item_rest:
| NOTIN LPAREN enumeration RPAREN { () }
| IN LPAREN enumeration RPAREN { () } */

comparison:
| sum_expression { () }
| sum_expression comparison_op sum_expression { () }

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
| group { () }

loop_expression:
| loop_variables COLON expression { () }

ternary_operator:
| IF expression THEN expression else_branch? ENDIF { () }

else_branch:
| ELSE expression { () }

factor_literal:
| FLOAT { () }
| symbol_or_int { () }(*
  Some symbols start with a digit and make it hard to parse with (float / integer / symbol)
  *)

function_call:
| SYMBOL LPAREN function_call_args RPAREN { () }

function_call_args:
| loop_expression { () }
| function_arguments { () }

function_arguments:
| expression { () }
| expression COMMA function_arguments { () }


group:
| LPAREN expression RPAREN { () }

symbol_or_int:
| SYMBOL { () }
| INT { () }

comparison_op:
| GTE  { () }
| LTE  { () }
| LT { () }
| GT { () }
| NEQ  { () }
| EQUALS { () }

symbol_enumeration:
| separated_nonempty_list(COMMA, SYMBOL) { () }
