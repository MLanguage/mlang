
(* The type of tokens. *)

type token = 
  | VERIFICATION
  | TYPE
  | TIMES
  | THEN
  | TABLE
  | SYMBOL of (string)
  | STRING of (string)
  | SEMICOLON
  | RULE
  | RPAREN
  | REAL
  | RBRACKET
  | RANGE
  | PLUS
  | PENALITY
  | OUTPUT
  | OR
  | ONE
  | NOTIN
  | NEQ
  | MINUS
  | LTE
  | LT
  | LPAREN
  | LBRACKET
  | INTEGER
  | INT of (int)
  | INPUT
  | INFORMATIVE
  | INCOME
  | IN
  | IF
  | GTE
  | GT
  | GIVEN_BACK
  | FOR
  | FLOAT of (string)
  | FAMILY
  | ERROR
  | EQUALS
  | EOF
  | ENDIF
  | ELSE
  | DIV
  | DISCORDANCE
  | DATE_YEAR
  | DATE_MONTH
  | DATE_DAY_MONTH_YEAR
  | CONTEXT
  | CONST
  | COMPUTED
  | COMMA
  | COLON
  | CHAINING
  | BOOLEAN
  | BASE
  | APPLICATION
  | ANOMALY
  | AND
  | ALIAS

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val source_file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
