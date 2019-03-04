open Lexer
open Lexing
open Cli

let parse_cli_args () =
  (** Code block to retrieve and parse command-line arguments. *)
  let speclist = Arg.align [
      ("--verify", Arg.Set verify_flag,
       "Vérifie que les conditions sont valables dans tous les cas");
      ("--debug", Arg.Set debug_flag,
       "Affiche des informations de débuggage")
    ]
  in let usage_msg =
       "M parser"
  in
  let anon_func (file: string) : unit =
    source_files := file::!source_files
  in Arg.parse speclist anon_func usage_msg

let main () =
  parse_cli_args ();
  Cli.debug_print "Reading files...";
  List.iter (fun source_file ->
      let (filebuf, input) = if source_file <> "" then
          let input = open_in source_file in
          (Lexing.from_channel input, Some input)
        else if source_file <> "" then
          (Lexing.from_string source_file, None)
        else
          failwith "Il faut spécifier un fichier !"
      in
      Cli.debug_print (Printf.sprintf "Parsing %s" source_file);
      let filebuf = {filebuf with
                     lex_curr_p = { filebuf.lex_curr_p with
                                    pos_fname = Filename.basename source_file
                                  }
                    }
      in
      try
        let commands = Parser.source_file token filebuf in
        Cli.debug_print (Printf.sprintf "Parsed AST:\n%s" (Ast.show_source_file commands))
      with
      | Lexer.LexingError msg | Cli.ParsingError msg ->
        error_print msg
      | Parser.Error -> begin
          error_print
            (Printf.sprintf "Lexer error at position %s"
               (print_lexer_position filebuf.lex_curr_p));
          begin match input with
            | Some input -> close_in input
            | None -> ()
          end
        end
    ) !source_files

let _ = main ()
