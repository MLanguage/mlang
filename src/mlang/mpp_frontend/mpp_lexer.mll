{
  open Lexing
  open Mpp_parser

  let stack = ref [0]
  let rec unindent n sloc = match !stack with
    | m :: _ when m = n -> []
    | m :: st when m > n -> stack := st; DEDENT :: unindent n sloc
    | _ -> Errors.raise_spanned_error "incorrect indentation" sloc

  let update n sloc =
    match !stack with
    | m :: _ when m < n ->
       stack := n :: !stack;
       [NEWLINE; INDENT]
    | _ -> NEWLINE :: unindent n sloc
}

let space = ' ' | '\t'
let comment = "#" [^'\n']* | '/' '*' ([^')'] | ')' [^'#'])* '*' '/'

let endline = '\n' | '\r' | "\r\n"
let integer = ['0'-'9']+

rule next_tokens = parse
    | (space | comment)+ { next_tokens lexbuf }
    | '\n' { new_line lexbuf; update (indentation lexbuf) (Parse_utils.mk_position (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)) }
    | "<"                       { [LT] }
    | ">"                       { [GT] }
    | "<="                      { [LE] }
    | ">="                      { [GE] }
    | "=="                      { [EQUAL] }
    | "!="                      { [NEQ] }
    | ","                       { [COMMA] }
    | "-"                       { [MINUS] }
    | "and"                     { [AND] }
    | "or"                      { [OR] }
    | "="                       { [EQ] }
    | "<-"                      { [LEFTARROW] }
    | '('                       { [LPAREN] }
    | ')'                       { [RPAREN] }
    | "if"                      { [IF] }
    | "else"                    { [ELSE] }
    | "del"                     { [DELETE] }
    | "partition with"          { [PARTITION] }
    | "saisie"                  { [INPUT] }
    | "calculee"                { [COMPUTED] }
    | "base"                    { [BASE] }
    | "restituee"               { [GIVEN_BACK] }
    | "*"                       { [STAR] }
    | "call_m_rules"            { [CALL_M_RULES] }
    | "call_m_chain"            { [CALL_M_CHAIN] }
    | "call_m_verifs"           { [CALL_M_VERIFS] }
    | "nb_category"             { [NB_CATEGORY] }
    | "exists_attribute_with"   { [EXISTS_ATTRIBUTE_WITH] }
    | "exists_aliases"          { [EXISTS_ALIASES] }
    | ':'                       { [COLON] }
    | integer as i              { [INT (int_of_string i)] }
    | ['a'-'z' 'A'-'Z' '0'-'9' '_']+ as s
                                { [IDENT s] }
    | eof                       { [EOF] }
    | _                         { Errors.raise_spanned_error "M++ lexing error" (Parse_utils.mk_position (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)) }

and indentation = parse
    | (space | comment)* '\n'   { new_line lexbuf; indentation lexbuf }
    | space* as s   { String.length s }


{
  let next_token =
    let tokens = Queue.create () in (* next lexemes to send back *)
    fun lb ->
      if Queue.is_empty tokens then begin
	let l = next_tokens lb in
	List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}
