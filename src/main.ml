open Lexer
open Lexing

let print_position (lexbuf : Lexing.lexbuf) : string =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let source_file = ref ""

let parse_cli_args () =
  (** Code block to retrieve and parse command-line arguments. *)
  let speclist = Arg.align [
      ("-f", Arg.Set_string (source_file),
       "<fichier> Lis un fichier d'instructions (extension .3e)");
      ("--file", Arg.Set_string (source_file),
       "<fichier>");
    ]
  in let usage_msg =
       "M parser"
  in Arg.parse speclist print_endline usage_msg

let main () =
  parse_cli_args ();
  let (filebuf, input) = if !source_file <> "" then
      let input = open_in !source_file in
      (Lexing.from_channel input, Some input)
    else if !source_file <> "" then
      (Lexing.from_string !source_file, None)
    else
      failwith "Il faut spécifier un fichier !"
  in
  let filebuf = {filebuf with
                 lex_curr_p = { filebuf.lex_curr_p with
                                pos_fname = Filename.basename !source_file
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

let _ = main ()
