(* FIXME: add locations *)
%{
  open Mpp_ast

  let mk_position loc = Pos.make_position ((fst loc).Lexing.pos_fname) loc
%}

%token <string> IDENT
%token <int> INT
%token EOF INDENT NEWLINE DEDENT
%token LT GT LE GE
%token EQUAL NEQ  EQ LPAREN RPAREN LEFTARROW
%token AND OR
%token IF ELSE DELETE PARTITION COLON COMMA MINUS

%left OR
%left AND
%left LT GT LE GE EQUAL NEQ
%nonassoc MINUS

%start <Mpp_ast.program> file

%%

file:
| NEWLINE? l = nonempty_list(compute_functions) EOF { l }
;

compute_functions:
| name = IDENT LPAREN arg = IDENT RPAREN COLON body = new_block { {name; args= [arg]; body} }
| name = IDENT LPAREN RPAREN COLON body = new_block { {name; args= []; body} }
;

ident:
| i = IDENT { (i, mk_position $sloc) }

stmt:
| args = separated_list(COMMA, ident) LEFTARROW var = ident LPAREN RPAREN NEWLINE { Expr(Call(var, args), mk_position $sloc), mk_position $sloc }
| var = IDENT EQ e = expr NEWLINE { Assign(var, e), mk_position $sloc }
| DELETE var = IDENT NEWLINE { Delete var, mk_position $sloc }
| var = ident LPAREN args = separated_list(COMMA, ident) RPAREN NEWLINE
                              { Expr(Call(var, args), mk_position $sloc), mk_position $sloc }
| IF b = expr COLON t = new_block ELSE COLON f = new_block { Conditional(b, t, f), mk_position $sloc }
| IF b = expr COLON t = new_block { Conditional(b, t, []), mk_position $sloc }
| PARTITION var = IDENT COLON b = new_block { Partition(var, b), mk_position $sloc }
;

new_block:
| NEWLINE INDENT stmt+ DEDENT { $3 }
;

%inline binop:
| LT { Lt }
| GT { Gt }
| LE { Lte }
| GE { Gte }
| EQUAL { Eq }
| NEQ { Neq }
| AND { And }
| OR { Or }
;

expr:
| i = INT { Constant i, mk_position $sloc }
| var = IDENT { Variable var, mk_position $sloc }
| MINUS e = expr { Unop(Minus, e), mk_position $sloc }
| var = ident LPAREN args = separated_list(COMMA, ident) RPAREN
                              { Call(var, args), mk_position $sloc }
| e1 = expr b = binop e2 = expr { Binop(e1, b, e2), mk_position $sloc }
;
