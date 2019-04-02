(*
  Copyright 2018 Denis Merigoux and INRIA

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

(** Error formatting and helper functions *)


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
  raise (ParsingError (Printf.sprintf "Parsing error: %s (file %s, %s to %s)"
                         msg
                         (sloc_start.Lexing.pos_fname)
                         (print_lexer_position sloc_start)
                         (print_lexer_position sloc_end)
                      ))


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

exception Unimplemented of string * Ast.position
