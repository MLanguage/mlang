let process (ompp_file: string option) (_: Mvg.program) : Cst.program option =
  match ompp_file with
  | None -> None
  | Some mpp_file ->
      Cli.debug_print "Reading m++ file %s" mpp_file;
      let f = open_in mpp_file in
      let buf = Lexing.from_channel f in
      buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = mpp_file };
      try
        let cst = Mpp_parser.file Mpp_lexer.next_token buf in
        close_in f; Some cst
      with
      | Mpp_parser.Error ->
         let b = Lexing.lexeme_start_p buf in
         let e = Lexing.lexeme_end_p buf in
         let l = b.pos_lnum in
         let fc = b.pos_cnum - b.pos_bol + 1 in
         let lc = e.pos_cnum - b.pos_bol + 1 in
         let () = Cli.error_print "File \"%s\", line %d, characters %d-%d:\n@." mpp_file l fc lc in
         None
      | Errors.LexingError e
        | Errors.ParsingError e ->
         let () = Cli.error_print "Parsing Error %s" e in
         None
