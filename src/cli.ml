let source_files : string list ref = ref []
let verify_flag = ref false
let debug_flag = ref false

let debug_marker () = ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.magenta] "[DEBUG] "
let error_marker () = ANSITerminal.eprintf [ANSITerminal.Bold; ANSITerminal.red] "[ERROR] "


let debug_print (s: string) =
  if !debug_flag then begin
    debug_marker ();
    Printf.printf "%s\n" s;
    flush stdout;
    flush stdout
  end

let error_print (s: string) =
  error_marker ();
  Printf.eprintf "%s\n" s;
  flush stdout;
  flush stdout

exception ParsingError of string

let print_lexer_position (pos : Lexing.position) : string =
  Printf.sprintf "%d:%d"
    pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let parser_error (sloc_start, sloc_end) (msg: string) =
  raise (ParsingError (Printf.sprintf "Parsing error : %s (file %s, %s to %s)"
                         msg
                         (sloc_start.Lexing.pos_fname)
                         (print_lexer_position sloc_start)
                         (print_lexer_position sloc_end)
                      ))

let parse_variable_name sloc (s: string) : Ast.variable_name =
  if not (String.equal (String.uppercase_ascii s) s) then
    parser_error sloc "invalid variable name"
  else
    s

let dup_exists l =
  let rec dup_consecutive = function
    | [] | [_] -> false
    | c1::(c2 as h2)::tl -> Char.equal c1 c2 || dup_consecutive (h2::tl)
  in
  let sort_on_third s1 s2 = Char.compare s1 s2 in
  dup_consecutive (List.sort sort_on_third l)

let parse_variable_generic_name sloc (s: string) : Ast.variable_generic_name =
  let parameters = ref [] in
  for i = String.length s - 1 downto 0 do
    let p = String.get s i in
    if p = '_' || Str.string_match (Str.regexp "[0-9]+") (String.make 1 p) 0 ||
       not (Char.equal (Char.lowercase_ascii p) p)
    then
      ()
    else begin
      parameters := p::!parameters;
    end
  done;
  if dup_exists !parameters then
    parser_error sloc "variable parameters should have distinct names";
  { Ast.parameters = !parameters; Ast.base = s }


let parse_table_index sloc (s: string) : Ast.table_index =
  if String.equal s "X" then
    Ast.GenericIndex
  else
    try Ast.LiteralIndex(int_of_string s) with
    | Failure _ ->
      begin try Ast.SymbolIndex (parse_variable_name sloc s) with
        | ParsingError _ ->
          Printf.printf "s: %s, %b\n" s (String.equal s "X");
          parser_error sloc "table index should be an integer"
      end
