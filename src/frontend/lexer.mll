(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

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

{
  open Lexing
  open Parser
  open Errors
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
| "non"
  { NOT }
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
| "condition"
  { CONDITION }
| "anomalie"
  { ANOMALY }
| "discordance"
  { DISCORDANCE }
| "informative"
  { INFORMATIVE }
| "sortie"
  { OUTPUT }
| "fonction"
  { FONCTION }
| '"' [^'"']* '"' as s
  { STRING s }
| ['a'-'z'] as s
    { PARAMETER s }
| (['a'-'z' 'A'-'Z' '0'-'9' '_']+ | ['0' - '9']+ '.' ['0' - '9']+) as s
  { SYMBOL s }
| eof
  { EOF }
| _
  { raise (LexingError (lexer_error lexbuf)) }
