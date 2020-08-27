{
  open Lexing
  open Mpp_parser
  open Errors

  (* let stack =
   *   let r = Stack.create () in
   *   let () = Stack.push 0 r in
   *   r *)

  let stack = ref [0]
  let rec unindent n = match !stack with
    | m :: _ when m = n -> []
    | m :: st when m > n -> stack := st; DEDENT :: unindent n
    | _ -> raise (LexingError "incorrect indentation")

    (* let rec unindent n =
     * let m = Stack.top stack in
     * if m = n then []
     * else if m > n then
     *   let _ = Stack.pop stack in
     *   DEDENT :: unindent n
     * else raise (LexingError "incorrect indentation") *)

  let update n =
    match !stack with
    | m :: _ when m < n ->
       stack := n :: !stack;
       [NEWLINE; INDENT]
    | _ -> NEWLINE :: unindent n
    (* let m = Stack.top stack in
     * if m < n then
     *   let () = Stack.push n stack in
     *   [NEWLINE; INDENT]
     * else
     *   NEWLINE :: unindent n *)
}

let space = ' ' | '\t'
let comment = "#" [^'\n']*
let endline = '\n' | '\r' | "\r\n"
let integer = ['0'-'9']+

rule next_tokens = parse
    | (space | comment)+ { next_tokens lexbuf }
    | '\n' { new_line lexbuf; update (indentation lexbuf) }
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
    | '('                       { [LPAREN] }
    | ')'                       { [RPAREN] }
    | "if"                      { [IF] }
    | "else"                    { [ELSE] }
    | "del"                     { [DELETE] }
    | "partition with"          { [PARTITION] }
    | ':'                       { [COLON] }
    | integer as i              { [INT (int_of_string i)] }
    | ['a'-'z' 'A'-'Z' '0'-'9' '_']+ as s
                                { [IDENT s] }
    | eof                       { [EOF] }
    | _                         { raise (LexingError (Format.asprintf "%a" lexer_error lexbuf)) }

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
