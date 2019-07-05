(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by the DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-C license and that you accept its terms.
*)

(** Error formatting and helper functions *)

(**{1 Frontend }*)

exception FrontendError of string

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
  raise (FrontendError (Printf.sprintf "Frontend error: %s (file %s, %s to %s)"
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

(**{1 Interpretation }*)

type run_error =
  | ErrorValue of string
  | DivByZero of string
  | FloatIndex of string
  | IndexOutOfBounds of string
  | MissingInputValue of string

exception RuntimeError of run_error

let format_runtime_error (e: run_error) : string = match e with
  | ErrorValue s -> Printf.sprintf "Error value at runtime: %s" s
  | DivByZero s -> Printf.sprintf "Division by zero: %s" s
  | FloatIndex s -> Printf.sprintf "Index is not an integer: %s" s
  | IndexOutOfBounds s -> Printf.sprintf "Index out of bounds: %s" s
  | MissingInputValue s -> Printf.sprintf "Missing input value: %s" s
