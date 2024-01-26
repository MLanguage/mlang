(*From test_interpreter.ml*)

open Types_module

let parse_file (test_name : string) : Types_module.irj_file =
  let input = open_in test_name in
  let filebuf = Lexing.from_channel input in
  let filebuf =
    {
      filebuf with
      lex_curr_p = { filebuf.lex_curr_p with pos_fname = test_name };
    }
  in
  let f =
    try Test_parser.irj_file Test_lexer.token filebuf with
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
(*
let () =
let donnees = parse_file "alavoine-2.irj" in
let (entrees, _, _) = donnees.prim in 
print_string donnees.nom;print_newline();
let (code, valeur, _) = List.hd entrees in
print_string "première entrée : "; print_string code; print_string " avec la valeur ";
match valeur with
| I entier -> print_int entier; print_string " entière.";print_newline();flush stdout
| F flottant -> print_float flottant; print_string " flottante.";
print_newline();
flush stdout
*)
