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

let print_lexer_position (pos : Lexing.position) : string =
  Printf.sprintf "%d:%d"
    pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let lexer_error lexbuf  =
  Printf.sprintf "Incorrect character (%s) at position %s in file %s\n"
    (Lexing.lexeme lexbuf)
    (print_lexer_position lexbuf.Lexing.lex_curr_p)
    (lexbuf.Lexing.lex_curr_p.Lexing.pos_fname)

let parser_error (sloc_start, sloc_end) (msg: string) =
  raise (ParsingError (Printf.sprintf "Frontend error: %s (file %s, %s to %s)"
                         msg
                         (sloc_start.Lexing.pos_fname)
                         (print_lexer_position sloc_start)
                         (print_lexer_position sloc_end)
                      ))

(**{1 Typechecking }*)

type typ_error =
  | Variable of string
  | Numeric of string
  | Function of string
  | LoopParam of string
  | Typing of string
  | Inlining of string

exception TypeError of typ_error

let format_typ_error (e: typ_error) : string = match e with
  | Variable s -> Printf.sprintf "Variable error: %s" s
  | Numeric s -> Printf.sprintf "Numeric error: %s" s
  | Function s -> Printf.sprintf "Function error: %s" s
  | LoopParam s -> Printf.sprintf "Loop parameter error: %s" s
  | Typing s -> Printf.sprintf "Typing error: %s" s
  | Inlining s -> Printf.sprintf "Inlining error: %s" s

exception Unimplemented of string

(**{1 Others}*)

exception ArgumentError of string
