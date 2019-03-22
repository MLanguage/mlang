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


exception TypeError of typ_error

let format_typ_error (e: typ_error) : string = match e with
  | Variable s -> Printf.sprintf "Variable error: %s" s
