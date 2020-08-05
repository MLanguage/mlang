(* FIXME: add locations *)
%{
    open Errors
    open Cst
%}

%token <string> IDENT
%token <int> INT
%token EOF INDENT NEWLINE DEDENT
%token LT GT LE GE
%token EQUAL NEQ AND OR EQ LPAREN RPAREN
%token IF ELSE DELETE PARTITION COLON COMMA MINUS

%left OR
%left AND
%nonassoc LT GT LE GE EQUAL NEQ
%nonassoc MINUS

%start <Cst.program> file

%%

file:
| NEWLINE? l = nonempty_list(compute_functions) EOF { l }
;

compute_functions:
| name = IDENT LPAREN arg = IDENT RPAREN COLON body = new_block { {name; args= [arg]; body} }
| name = IDENT LPAREN RPAREN COLON body = new_block { {name; args= []; body} }
;

stmt:
| var = IDENT EQ e = expr NEWLINE { Assign(var, e) }
| DELETE var = IDENT NEWLINE { Delete var }
| var = IDENT LPAREN args = separated_list(COMMA, IDENT) RPAREN NEWLINE
                              { Expr(Call(var, List.map (fun x -> Variable x) args)) }
| IF b = expr COLON t = new_block ELSE COLON f = new_block { Conditional(b, t, f) }
| IF b = expr COLON t = new_block { Conditional(b, t, []) }
| PARTITION var = IDENT COLON b = new_block {
                                      if var = "var_is_taxbenefit" then
                                        Partition(VarIsTaxBenefit, b)
                                      else
                                        raise (ParsingError (Format.asprintf "unknown filter %s" var))}
;

new_block:
| NEWLINE INDENT stmt+ DEDENT { $3 }
;

binop:
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
| i = INT { Constant i }
| var = IDENT { Variable var }
| MINUS e = expr { Unop(Minus, e) }
| var = IDENT LPAREN args = separated_list(COMMA, IDENT) RPAREN
                              { Call(var, List.map (fun x -> Variable x) args) }
| e1 = expr b = binop e2 = expr { Binop(e1, b, e2) }
;
