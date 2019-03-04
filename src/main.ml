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
        let _commands = Parser.source_file Lexer.token filebuf in
        ()
      with
      | Lexer.Error msg ->
        Printf.eprintf "%s%!" msg
      | Parser.Error -> begin
          Printf.eprintf "At offset %s: syntax error.\n%!" (print_position filebuf);
          begin match input with
            | Some input -> close_in input
            | None -> ()
          end
        end
    ) !source_files

let _ = main ()
