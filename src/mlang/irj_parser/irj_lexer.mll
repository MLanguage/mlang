(*
Copyright Inria, contributors:
  Raphaël Monat <raphael.monat@lip6.fr> (2019)

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
open Irj_parser
}

rule token = parse
| [' ' '\t'] (* also ignore newlines, not only whitespace and tabs *)
  { token lexbuf }
| '*' [^ '\n']* '\n' (* ignore comments *)
  { new_line lexbuf; token lexbuf }
| '\n' | "\r\n"
  { new_line lexbuf; token lexbuf}
| "/"
  { SLASH }
| "#NOM"
  { NOM }
| "#FIP"
  { FIP }
| "#ENTREES-PRIMITIF"
  { ENTREESPRIM }
| "#CONTROLES-PRIMITIF"
  { CONTROLESPRIM }
| "#RESULTATS-PRIMITIF"
  { RESULTATSPRIM }
| "#ENTREES-CORRECTIF"
  { ENTREESCORR }
| "#CONTROLES-CORRECTIF"
  { CONTROLESCORR }
| "#RESULTATS-CORRECTIF"
  { RESULTATSCORR }
| "#ENTREES-RAPPELS"
  { ENTREESRAPP }
| "#CONTROLES-RAPPELS"
  { CONTROLESRAPP }
| "#RESULTATS-RAPPELS"
  { RESULTATSRAPP }
(*| "#DATES"
  { DATES }
| "#AVIS_IR"
  { AVISIR }
| "#AVIS_CSG"
  { AVISCSG }*)
| "##"
  { ENDSHARP }
| '-'? ['0' - '9']+ as i
  { INTEGER i }
| '-'? ['0' - '9']+ '.' ['0' - '9']* as f
  { FLOAT f }
| ['a'-'z' 'A'-'Z' '0'-'9' '_']+ as s
  { SYMBOL s }
| ['a'-'z' 'A'-'Z' ' ' '0'-'9' ';' '-']+ as s
  { NAME s }
| eof
  { EOF }
| _
  {  Errors.raise_spanned_error "Test file lexer error" (Parse_utils.mk_position (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)) }
