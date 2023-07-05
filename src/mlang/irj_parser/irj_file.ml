(*From test_interpreter.ml*)

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
