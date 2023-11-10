(* Copyright Inria, contributors: Raphaël Monat <raphael.monat@lip6.fr> (2019)
   David Declerck (2023)

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

{
open Lexing
open Irj_parser

module StrMap = Map.Make (String)

let keywords =
  List.fold_left (fun map (kw, tok) ->
    StrMap.add kw tok map
  ) StrMap.empty [
    "#NOM",                 NOM;
    "#FIP",                 FIP;
    "#ENTREES-PRIMITIF",    ENTREESPRIM;
    "#CONTROLES-PRIMITIF",  CONTROLESPRIM;
    "#RESULTATS-PRIMITIF",  RESULTATSPRIM;
    "#ENTREES-CORRECTIF",   ENTREESCORR;
    "#CONTROLES-CORRECTIF", CONTROLESCORR;
    "#RESULTATS-CORRECTIF", RESULTATSCORR;
    "#ENTREES-RAPPELS",     ENTREESRAPP;
    "#CONTROLES-RAPPELS",   CONTROLESRAPP;
    "#RESULTATS-RAPPELS",   RESULTATSRAPP;
    "#DATES",               DATES;
    "#AVIS_IR",             AVISIR;
    "#AVIS_CSG",            AVISCSG;
  ]

let error lb msg =
  Errors.raise_spanned_error ("Lexing error : " ^ msg)
    (Parse_utils.mk_position (Lexing.lexeme_start_p lb,
                              Lexing.lexeme_end_p lb))

let is_bol lb =
  (* bol = beginning of line *)
  lb.lex_start_p.pos_cnum - lb.lex_start_p.pos_bol = 0

let check_cr lb =
  if String.contains (lexeme lb) '\r' then
    error lb ("Carriage return detected")
  (* No more supposed to be an error in autotests. Keeping it to enforce it later? *)
}

let blank = [' ' '\t']
let any =   [^ '\n']
let nl =    ['\n']

rule token = parse

| '\n' | "\r\n"
  { check_cr lexbuf; new_line lexbuf;
    if is_bol lexbuf then token lexbuf
    else NL }

| '*' any* nl
  { check_cr lexbuf; new_line lexbuf;
    if is_bol lexbuf then token lexbuf
    else error lexbuf "Comment with * must start in the first column" }

| blank any* nl
  { check_cr lexbuf; new_line lexbuf;
    if is_bol lexbuf then error lexbuf "Line can not start with a blank"
    else NL }

| '-'? ['0' - '9']+ as i
  { INTEGER (int_of_string i) }
  
| '-'? ['0' - '9']+ '.' ['0' - '9']* as f
  { FLOAT (float_of_string f) } (* DONT KEEP THAT *)
  (* Probably in order to write a specific function for our number format *)

| ['a'-'z' 'A'-'Z' '0'-'9' '_']+ as s
  { SYMBOL s }

| ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '.' ';' (*' '*)]+ as s
  { NAME s }
  (* Compared to the old lexer, adds _ and . removes space *)

| "/"
  { SLASH }

| "##"
  { ENDSHARP }

| "#" ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']+ as s
  { match StrMap.find_opt s keywords with
    | None -> error lexbuf (Printf.sprintf "Unknown section name: '#%s'" s)
    | Some t -> t }

| eof
  { EOF }

| _ as c
  { error lexbuf (Printf.sprintf
                  "Unexpected character '%c' (%d)" c (Char.code c)) }
