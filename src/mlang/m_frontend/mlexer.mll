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
| '.' { DOT }
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
| '%' { MOD }
| '=' { EQUALS }
| "!=" { NEQ }
| '>' { GT }
| '<' { LT }
| ">=" { GTE }
| "<=" { LTE }
| '"' [^'"']* '"' as s { STRING s }
| (['a'-'z' 'A'-'Z' '0'-'9' '_']+ | ['0'-'9']+ '.' ['0'-'9']+) as s {
    match s with
    | "BOOLEEN" -> BOOLEAN
    | "DATE_AAAA" -> DATE_YEAR
    | "DATE_JJMMAAAA" -> DATE_DAY_MONTH_YEAR
    | "DATE_MM" -> DATE_MONTH
    | "ENTIER" -> INTEGER
    | "REEL" -> REAL
    | "afficher" -> PRINT
    | "afficher_erreur" -> PRINT_ERR
    | "ajouter" -> ADD
    | "alias" -> ALIAS
    | "alors" -> THEN
    | "anomalie" -> ANOMALY
    | "application" -> APPLICATION
    | "apres" -> AFTER
    | "arguments" -> INPUT_ARGS
    | "arranger_evenements" -> ARRANGE_EVENTS
    | "attribut" -> ATTRIBUT
    | "autorise" -> AUTHORIZE
    | "avec" -> WITH
    | "base" -> BASE
    | "calculable" -> COMPUTABLE
    | "calculee" -> COMPUTED
    | "calculer" -> COMPUTE
    | "categorie" -> CATEGORY
    | "champ_evenement" -> EVENT_FIELD
    | "cible" -> TARGET
    | "const" -> CONST
    | "dans" -> IN
    | "dans_domaine" -> IN_DOMAIN
    | "discordance" -> DISCORDANCE
    | "domaine" -> DOMAIN
    | "enchaineur" -> CHAINING
    | "entre" -> BETWEEN
    | "erreur" -> ERROR
    | "espace" -> SPACE
    | "espace_variables" -> VARIABLE_SPACE
    | "meme_variable" -> SAME_VARIABLE
    | "et" -> AND
    | "evenement" -> EVENT
    | "evenements" -> EVENTS
    | "exporte_erreurs" -> EXPORT_ERRORS
    | "faire" -> DO
    | "filtrer" -> FILTER
    | "finalise_erreurs" -> FINALIZE_ERRORS
    | "finquand" -> ENDWHEN
    | "finsi" -> ENDIF
    | "fonction" -> FONCTION
    | "increment" -> STEP
    | "indefini" -> UNDEFINED
    | "indenter" -> INDENT
    | "informative" -> INFORMATIVE
    | "iterer" -> ITERATE
    | "leve_erreur" -> RAISE_ERROR
    | "nb_bloquantes" -> NB_BLOCKING
    | "nb_categorie" -> NB_CATEGORY
    | "nb_anomalies" -> NB_ANOMALIES
    | "nb_discordances" -> NB_DISCORDANCES
    | "nb_informatives" -> NB_INFORMATIVES
    | "neant" -> NOTHING
    | "nettoie_erreurs" -> CLEAN_ERRORS
    | "nettoie_erreurs_finalisees" -> CLEAN_FINALIZED_ERRORS
    | "nom" -> NAME
    | "non" -> NOT
    | "numero_compl" -> COMPL_NUMBER
    | "numero_verif" -> VERIF_NUMBER
    | "ou" -> OR
    | "par_defaut" -> BY_DEFAULT
    | "pour" -> FOR
    | "puis_quand" -> THEN_WHEN
    | "quand" -> WHEN
    | "reference" -> REFERENCE
    | "regle" -> RULE
    | "restaurer" -> RESTORE
    | "restituee" -> GIVEN_BACK
    | "resultat" -> RESULT
    | "saisie" -> INPUT
    | "si" -> IF
    | "sinon" -> ELSE
    | "sinon_faire" -> ELSE_DO
    | "sinon_si" -> ELSEIF
    | "sortie" -> OUTPUT
    | "specialise" -> SPECIALIZE
    | "tableau" -> TABLE
    | "taille" -> SIZE
    | "trier" -> SORT
    | "type" -> TYPE
    | "un" -> ONE
    | "valeur" -> VALUE
    | "variable" -> VARIABLE
    | "variables_temporaires" -> TEMP_VARS
    | "verif" -> VERIFICATION
    | "verifiable" -> VERIFIABLE
    | "verifier" -> VERIFY
    | _ -> SYMBOL s 
  }
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

