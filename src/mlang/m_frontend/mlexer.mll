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
| "#{" { multiline_comment 1 lexbuf }
| "}#" {
    Errors.raise_spanned_error
      "unexpected end of comment"
      (mk_lexbuf_pos lexbuf)
  }
| '#' { one_line_comment lexbuf }
| [' ' '\t'] { token lexbuf }
| '\n' | "\r\n" { new_line lexbuf; token lexbuf}
| ".." { RANGE }
| "un" { ONE }
| "dans" { IN }
| '=' { EQUALS }
| "et" { AND }
| "ou" { OR }
| "application" { APPLICATION }
| ';' { SEMICOLON }
| ':' { COLON }
| ',' { COMMA }
| "enchaineur" { CHAINING }
| "type" { TYPE }
| "BOOLEEN" { BOOLEAN }
| "DATE_AAAA" { DATE_YEAR }
| "DATE_JJMMAAAA" { DATE_DAY_MONTH_YEAR}
| "DATE_MM" { DATE_MONTH }
| "ENTIER" { INTEGER }
| "REEL" { REAL }
| "tableau" { TABLE }
| '[' { LBRACKET }
| ']' { RBRACKET }
| "calculee" { COMPUTED }
| "base" { BASE }
| "restituee" { GIVEN_BACK }
| "const" { CONST }
| "alias" { ALIAS }
| "saisie" { INPUT }
| "variable" { VARIABLE }
| "temporaire" { TEMPORARY }
| "attribut" { ATTRIBUT }
| '(' { LPAREN }
| ')' { RPAREN }
| "pour" { FOR }
| '*' { TIMES }
| '/' { DIV }
| '+' { PLUS }
| '-' { MINUS }
| ">=" { GTE }
| "<=" { LTE }
| "!=" { NEQ }
| '>' { GT }
| '<' { LT }
| "non dans" { NOTIN }
| "non" { NOT }
| "indefini" { UNDEFINED }
| "domaine" { DOMAIN }
| "specialise" { SPECIALIZE }
| "autorise" { AUTHORIZE }
| "calculable" { COMPUTABLE }
| "verifiable" { VERIFIABLE }
| "par_defaut" { BY_DEFAULT }
| "regle" { RULE }
| "cible" { TARGET }
| "si" { IF }
| "alors" { THEN }
| "sinon" { ELSE }
| "finsi" { ENDIF }
| "calculer" { COMPUTE }
| "verifier" { VERIFY }
| "avec" { WITH }
| "numero_verif" { VERIF_NUMBER }
| "numero_compl" { COMPL_NUMBER }
| "nb_categorie" { NB_CATEGORY }
| "nb_erreur" { NB_ERROR }
| "afficher" { PRINT }
| "afficher_erreur" { PRINT_ERR }
| "iterer" { ITERATE }
| "categorie" { CATEGORY }
| "restaurer" { RESTORE }
| "apres" { AFTER }
| "taille" { SIZE }
| "erreur" { ERROR }
| "leve_erreur" { RAISE_ERROR }
| "nettoie_erreurs" { CLEAN_ERRORS }
| "verif" { VERIFICATION }
| "condition" { CONDITION }
| "anomalie" { ANOMALY }
| "discordance" { DISCORDANCE }
| "informative" { INFORMATIVE }
| "sortie" { OUTPUT }
| "fonction" { FONCTION }
| '"' [^'"']* '"' as s { STRING s }
| ['a'-'z'] as s { PARAMETER s }
| (['a'-'z' 'A'-'Z' '0'-'9' '_']+ | ['0' - '9']+ '.' ['0' - '9']+) as s {
    SYMBOL s
  }
| eof { EOF }
| _ { Errors.raise_spanned_error "M lexer error" (mk_lexbuf_pos lexbuf) }

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
    match level with
    | 1 -> Errors.raise_error "comment is not closed at end of file"
    | _ ->
        let msg =
          Format.sprintf
            "comments are not closed  at end of file (%d levels)"
            level
        in
        Errors.raise_error msg
  }
| '\n' { new_line lexbuf; multiline_comment level lexbuf }
| _ { multiline_comment level lexbuf }

