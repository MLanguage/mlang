(*
Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

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
  open Mparser

  let mk_lexbuf_pos lexbuf =
    Parse_utils.mk_position (lexeme_start_p lexbuf, lexeme_end_p lexbuf)
}

rule token = parse
| [' ' '\t'] { token lexbuf }
| '\n' | "\r\n" { new_line lexbuf; token lexbuf}
| '#' { one_line_comment lexbuf }
| "#{" { multiline_comment 1 lexbuf }
| "}#" {
    Errors.raise_spanned_error
      "unexpected end of comment"
      (mk_lexbuf_pos lexbuf)
  }
| ';' { SEMICOLON }
| ':' { COLON }
| ',' { COMMA }
| '[' { LBRACKET }
| ']' { RBRACKET }
| '(' { LPAREN }
| ')' { RPAREN }
| ".." { RANGE }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }
| '=' { EQUALS }
| "!=" { NEQ }
| '>' { GT }
| '<' { LT }
| ">=" { GTE }
| "<=" { LTE }
| "BOOLEEN" { BOOLEAN }
| "DATE_AAAA" { DATE_YEAR }
| "DATE_JJMMAAAA" { DATE_DAY_MONTH_YEAR}
| "DATE_MM" { DATE_MONTH }
| "ENTIER" { INTEGER }
| "REEL" { REAL }
| "afficher" { PRINT }
| "afficher_erreur" { PRINT_ERR }
| "alias" { ALIAS }
| "alors" { THEN }
| "anomalie" { ANOMALY }
| "application" { APPLICATION }
| "apres" { AFTER }
| "attribut" { ATTRIBUT }
| "autorise" { AUTHORIZE }
| "avec" { WITH }
| "base" { BASE }
| "calculable" { COMPUTABLE }
| "calculee" { COMPUTED }
| "calculer" { COMPUTE }
| "categorie" { CATEGORY }
| "cible" { TARGET }
| "condition" { CONDITION }
| "const" { CONST }
| "dans" { IN }
| "discordance" { DISCORDANCE }
| "domaine" { DOMAIN }
| "enchaineur" { CHAINING }
| "erreur" { ERROR }
| "et" { AND }
| "fonction" { FONCTION }
| "indefini" { UNDEFINED }
| "indenter" { INDENT }
| "informative" { INFORMATIVE }
| "iterer" { ITERATE }
| "leve_erreur" { RAISE_ERROR }
| "nb_categorie" { NB_CATEGORY }
| "nb_anomalies" { NB_ANOMALIES }
| "nb_discordances" { NB_DISCORDANCES }
| "nb_informatives" { NB_INFORMATIVES }
| "nettoie_erreurs" { CLEAN_ERRORS }
| "exporte_erreurs" { EXPORT_ERRORS }
| "nom" { NAME }
| "non" { NOT }
| "non dans" { NOTIN }
| "numero_compl" { COMPL_NUMBER }
| "numero_verif" { VERIF_NUMBER }
| "ou" { OR }
| "par_defaut" { BY_DEFAULT }
| "pour" { FOR }
| "regle" { RULE }
| "restaurer" { RESTORE }
| "restituee" { GIVEN_BACK }
| "saisie" { INPUT }
| "si" { IF }
| "sinon_si" { ELSEIF }
| "sinon" { ELSE }
| "finsi" { ENDIF }
| "sortie" { OUTPUT }
| "specialise" { SPECIALIZE }
| "tableau" { TABLE }
| "taille" { SIZE }
| "temporaire" { TEMPORARY }
| "type" { TYPE }
| "un" { ONE }
| "variable" { VARIABLE }
| "verif" { VERIFICATION }
| "verifiable" { VERIFIABLE }
| "verifier" { VERIFY }
| '"' [^'"']* '"' as s { STRING s }
| (['a'-'z' 'A'-'Z' '0'-'9' '_']+ | ['0'-'9']+ '.' ['0'-'9']+) as s { SYMBOL s }
| eof { EOF }
| _ { Errors.raise_spanned_error "syntax error" (mk_lexbuf_pos lexbuf) }

and one_line_comment = parse
| '\n' { new_line lexbuf; token lexbuf }
| eof { EOF }
| _ { one_line_comment lexbuf }

and multiline_comment level = parse
| "#{" { multiline_comment (level + 1) lexbuf }
| "}#" {
    match level with
    | 1 -> token lexbuf
    | _ -> multiline_comment (level - 1) lexbuf
  }
| eof {
    let msg =
      match level with
      | 1 -> "comment is not closed at end of file"
      | _ ->
          Format.sprintf
            "comments are not closed  at end of file (%d levels)"
            level
    in
    Errors.raise_error msg
  }
| '\n' { new_line lexbuf; multiline_comment level lexbuf }
| _ { multiline_comment level lexbuf }

