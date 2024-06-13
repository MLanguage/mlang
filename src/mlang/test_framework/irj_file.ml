(* Copyright Inria, contributors: Raphaël Monat <raphael.monat@lip6.fr> (2019)
   Mathieu Durero <mathieu.durero@dgfip.finances.gouv.fr> (2023)

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

open Irj_ast

let parse_file (test_name : string) : irj_file =
  let _text, filebuf = MenhirLib.LexerUtil.read test_name in
  (*Je comprends pas bien l’intérêt, pos_fname c’est déjà le nom du fichier.*)
  (* let filebuf =
       {
         filebuf with
         lex_curr_p = { filebuf.lex_curr_p with pos_fname = test_name };
       }
     in *)
  let f =
    try Irj_parser.irj_file Irj_lexer.token filebuf with
    | TestLexingError e -> raise (TestLexingError e)
    | TestParsingError e -> raise (TestParsingError e)
    | Irj_parser.Error -> raise TestErrorActivating2ndPass
  in
  f
