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
  let program = ref [] in
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
        Parse_utils.current_file := source_file;
        let commands = Parser.source_file token filebuf in
        (* Cli.debug_print
           (Printf.sprintf "Parsed AST:\n%s"
             (Format_ast.format_source_file commands)); *)
        program := commands::!program
      with
      | Errors.LexingError msg | Errors.ParsingError msg ->
        error_print msg
      | Parser.Error -> begin
          error_print
            (Printf.sprintf "Lexer error in file %s at position %s"
               (!Parse_utils.current_file)
               (Errors.print_lexer_position filebuf.lex_curr_p));
          begin match input with
            | Some input -> close_in input
            | None -> ()
          end;
          exit (-1)
        end
    ) !source_files;
  try
    ignore (Ast_to_cfg.translate !program)
  with
  | Errors.TypeError e ->
    error_print (Errors.format_typ_error e)
  | Errors.Unimplemented (msg,pos) ->
    error_print (Printf.sprintf "unimplemented for expression %s (code %s)" (Format_ast.format_position pos) msg)

let _ = main ()
