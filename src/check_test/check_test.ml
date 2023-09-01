
let () =
  if Array.length (Sys.argv) <> 2 then
    (Printf.eprintf "This program requires a test file as argument\n"; exit 1);
  let f = Sys.argv.(1) in
  let c = open_in f in
  let lb = Lexing.from_channel c in
  let _tf =
    try Mlang.Test_parser.test_file Mlang.Test_lexer.token lb with
    | Mlang.Errors.StructuredError (s, sopl, _fuuo) ->
        Printf.eprintf "%s" (Filename.basename f);
        List.iter (fun (_so, p) ->
            Printf.eprintf ":(%d,%d)-(%d,%d)"
              (Mlang.Pos.get_start_line p)
              (Mlang.Pos.get_start_column p)
              (Mlang.Pos.get_end_line p)
              (Mlang.Pos.get_end_column p)
          ) sopl;
        Printf.eprintf " : %s\n" s;
        close_in c;
        exit 1
    | Mlang.Test_parser.Error ->
        let p = Mlang.Parse_utils.mk_position (lb.lex_start_p, lb.lex_curr_p) in
        Printf.eprintf "%s" (Filename.basename f);
        Printf.eprintf ":(%d,%d)-(%d,%d)"
          (Mlang.Pos.get_start_line p)
          (Mlang.Pos.get_start_column p)
          (Mlang.Pos.get_end_line p)
          (Mlang.Pos.get_end_column p);
        Printf.eprintf " : Parse error\n";
        close_in c;
        exit 1
    | _ ->
        Printf.eprintf "Unknown error\n";
        close_in c;
        exit 1
  in
  close_in c;
  ()

