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
%token CALL_M_RULES CALL_M_CHAIN CALL_M_VERIFS
%token NB_CATEGORY INPUT COMPUTED BASE GIVEN_BACK STAR

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
| COMPUTED { ("calculee", mk_position $sloc) }
| INPUT { ("saisie", mk_position $sloc) }
| BASE { ("base", mk_position $sloc) }
| GIVEN_BACK { ("restituee", mk_position $sloc) }
;

ident_list:
| l = nonempty_list(ident) { (l, mk_position $sloc) }
;

%inline domain_args:
| dom = ident_list { dom, [] }
| dom = ident_list COMMA args = separated_list(COMMA, ident) { dom, args }
;

%inline domain_expr:
| dom = ident_list
  {
    let no_pos e = (e, Pos.no_pos) in
    let true_expr = no_pos (Binop (no_pos (Constant 0), Eq, no_pos (Constant 0))) in
    dom, true_expr
  }
| dom = ident_list COMMA expr = expr { dom, expr }
;

stmt:
| args = separated_list(COMMA, ident) LEFTARROW CALL_M_RULES LPAREN dom = ident_list RPAREN NEWLINE
    { Expr(CallRules(dom, args), mk_position $sloc), mk_position $sloc }
| args = separated_list(COMMA, ident) LEFTARROW CALL_M_CHAIN LPAREN chain = ident RPAREN NEWLINE
    { Expr(CallChain(chain :: args), mk_position $sloc), mk_position $sloc }
| args = separated_list(COMMA, ident) LEFTARROW var = ident LPAREN RPAREN NEWLINE
    { Expr(Call(var, args), mk_position $sloc), mk_position $sloc }
| args = separated_list(COMMA, ident) LEFTARROW var = ident LPAREN chain = ident RPAREN NEWLINE
    { Expr(Call(var, chain :: args), mk_position $sloc), mk_position $sloc }
| var = IDENT EQ e = expr NEWLINE { Assign(var, e), mk_position $sloc }
| DELETE var = IDENT NEWLINE { Delete var, mk_position $sloc }
| CALL_M_RULES LPAREN dom_args = domain_args RPAREN NEWLINE
    { Expr(CallRules(fst dom_args, snd dom_args), mk_position $sloc), mk_position $sloc }
| CALL_M_CHAIN LPAREN args = separated_list(COMMA, ident) RPAREN NEWLINE
    { Expr(CallChain(args), mk_position $sloc), mk_position $sloc }
| CALL_M_VERIFS LPAREN dom_expr = domain_expr RPAREN NEWLINE
    { Expr(CallVerifs(fst dom_expr, snd dom_expr), mk_position $sloc), mk_position $sloc }
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

var_category:
| STAR { (["*"], mk_position $sloc) }
| INPUT STAR { (["*"], mk_position $sloc) }
| INPUT l = nonempty_list(ident) { ("saisie" :: (List.map fst l), mk_position $sloc) }
| COMPUTED STAR { (["calculee"; "*"], mk_position $sloc) }
| COMPUTED BASE STAR { (["calulee"; "base"; "*"], mk_position $sloc) }
| COMPUTED GIVEN_BACK STAR { (["calulee"; "restituee"; "*"], mk_position $sloc) }
| COMPUTED BASE GIVEN_BACK | COMPUTED GIVEN_BACK BASE
    { (["calulee"; "base"; "restituee"], mk_position $sloc) }
;

expr:
| i = INT { Constant i, mk_position $sloc }
| NB_CATEGORY LPAREN cat = var_category RPAREN
    { NbVarCategory(cat), mk_position $sloc }
| var = IDENT { Variable var, mk_position $sloc }
| MINUS e = expr { Unop(Minus, e), mk_position $sloc }
| var = ident LPAREN args = separated_list(COMMA, ident) RPAREN
    { Call(var, args), mk_position $sloc }
| e1 = expr b = binop e2 = expr { Binop(e1, b, e2), mk_position $sloc }
;
