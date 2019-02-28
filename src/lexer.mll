{
  open Parser
  open Lexing

  exception Error of string

  let error msg   =
    Printf.sprintf "Incorrect character (%s)"  msg

}
rule token = parse
| [' ' '\t'] (* also ignore newlines, not only whitespace and tabs *)
  { token lexbuf }
| '#' [^ '\n']* '\n' (* ignore comments *)
  { new_line lexbuf; token lexbuf }
| '\n' | "\r\n"
  { new_line lexbuf; token lexbuf}
| ".."
  { RANGE }
| "un"
  { ONE }
| "dans"
  { IN }
| '='
  { EQUALS }
| "et"
  { AND }
| "ou"
  { OR }
| "application"
  { APPLICATION }
| ';'
  { SEMICOLON }
| ':'
  { COLON }
| ','
  { COMMA }
| "enchaineur"
  { CHAINING }
| "type"
  { TYPE }
| "BOOLEEN"
  { BOOLEAN }
| "DATE_AAAA"
  { DATE_YEAR }
| "DATE_JJMMAAAA"
  { DATE_DAY_MONTH_YEAR}
| "DATE_MM"
  { DATE_MONTH }
| "ENTIER"
  { INTEGER }
| "REEL"
  { REAL }
| "base"
  { BASE }
| "restituee"
  { GIVEN_BACK }
| "tableau"
  { TABLE }
| '['
  { LBRACKET }
| ']'
  { RBRACKET }
| "calculee"
  { COMPUTED }
| "const"
  { CONST }
| "alias"
  { ALIAS }
| "contexte"
  { CONTEXT }
| "famille"
  { FAMILY }
| "penalite"
  { PENALITY }
| "revenu"
  { INCOME }
| "saisie"
  { INPUT }
| '('
  { LPAREN }
| ')'
  { RPAREN }
| "pour"
  { FOR }
| '*'
  { TIMES }
| '/'
  { DIV }
| '+'
  { PLUS }
| '-'
  { MINUS }
| ">="
  { GTE }
| "<="
  { LTE }
| "!="
  { NEQ }
| '>'
  { GT }
| '<'
  { LT }
| "non dans"
  { NOTIN }
| "regle"
  { RULE }
| "si"
  { IF }
| "alors"
  { THEN }
| "sinon"
  { ELSE }
| "finsi"
  { ENDIF }
| "erreur"
  { ERROR }
| "verif"
  { VERIFICATION }
| "anomalie"
  { ANOMALY }
| "discordance"
  { DISCORDANCE }
| "informative"
  { INFORMATIVE }
| "sortie"
  { OUTPUT }
| ['0'-'9']+ '.' ['0'-'9']+ as f
  { FLOAT f }
| ['0'-'9']+ as i
  { INT (int_of_string i) }
| '"' [^'"'] '"' as s
  { STRING s }
| ['a'-'z' 'A'-'Z' '0'-'9' '_']+ as s
  { SYMBOL s }
| eof
  { EOF }
| _
  { raise (Error (error (Lexing.lexeme lexbuf))) }
