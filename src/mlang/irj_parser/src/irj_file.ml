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
  let input = open_in test_name in
  let filebuf = Lexing.from_channel input in
  let filebuf =
    {
      filebuf with
      lex_curr_p = { filebuf.lex_curr_p with pos_fname = test_name };
    }
  in
  let f =
    try Irj_parser.irj_file Irj_lexer.token filebuf with
    | Errors.StructuredError e ->
        close_in input;
        raise (Errors.StructuredError e)
    | Irj_parser.Error ->
        close_in input;
        Errors.raise_spanned_error "Test syntax error"
          (Parse_utils.mk_position (filebuf.lex_start_p, filebuf.lex_curr_p))
  in
  close_in input;
  f
