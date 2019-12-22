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

(** Error formatting and helper functions *)

(**{1 Frontend }*)

exception ParsingError of string

exception LexingError of string

let print_lexer_position fmt (pos : Lexing.position) =
  Format.fprintf fmt "%d:%d"
    pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let lexer_error fmt lexbuf  =
  Format.fprintf fmt "Incorrect character (%s) at position %a in file %s\n"
    (Lexing.lexeme lexbuf)
    print_lexer_position lexbuf.Lexing.lex_curr_p
    (lexbuf.Lexing.lex_curr_p.Lexing.pos_fname)

let parser_error (sloc_start, sloc_end) (msg: string) =
  raise (ParsingError (Format.asprintf "Frontend error: %s (file %s, %a to %a)"
                         msg
                         (sloc_start.Lexing.pos_fname)
                         print_lexer_position sloc_start
                         print_lexer_position sloc_end
                      ))

(**{1 Typechecking }*)

type typ_error_kind =
  | Variable | Numeric | Function | LoopParam | Typing | Inlining

type typ_error = typ_error_kind * string

exception TypeError of typ_error

let raise_typ_error kind msg =
  Format.kasprintf (fun str -> raise (TypeError (kind, str))) msg

let format_typ_error fmt ((k, s): typ_error) =
  Format.fprintf fmt "%s error: %s"
    (match k with
     | Variable -> "Variable"
     | Numeric -> "Numeric"
     | Function -> "Function"
     | LoopParam -> "Loop parameter"
     | Typing -> "Typing"
     | Inlining -> "Inlining") s

exception Unimplemented of string

(**{1 Others}*)

exception ArgumentError of string

exception TestError of string
