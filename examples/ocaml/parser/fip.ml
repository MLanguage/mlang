(*From test_interpreter.ml*)

let parse_file (test_name : string) : Types_module.test_file =
  let input = open_in test_name in
  let filebuf = Lexing.from_channel input in
  let filebuf =
    {
      filebuf with
      lex_curr_p = { filebuf.lex_curr_p with pos_fname = test_name };
    }
  in
  let f =
    try Test_parser.test_file Test_lexer.token filebuf with
    | Types_module.StructuredError e ->
        close_in input;
        raise (Types_module.StructuredError e)
    | Test_parser.Error ->
        close_in input;
        Types_module.raise_spanned_error "Test syntax error"
          (Types_module.mk_position (filebuf.lex_start_p, filebuf.lex_curr_p))
  in
  close_in input;
  f