open Format
open Mpp_ast

let format_scoped_var fmt sv =
  fprintf fmt "%s" (match sv with Local s -> s | Mbased (v, _) -> Pos.unmark v.Mvg.Variable.name)

let format_callable fmt f =
  fprintf fmt "%s"
    ( match f with
    | Program -> "evaluate_program"
    | MppFunction m -> m
    | Present -> "present"
    | Abs -> "abs"
    | Cast -> "cast"
    | DepositDefinedVariables -> "DepositDefinedVariables"
    | TaxbenefitCeiledVariables -> "TaxbenefitCeiledVariables"
    | TaxbenefitDefinedVariables -> "TaxbenefitDefinedVariables" )

let format_binop fmt (b : Cst.binop) =
  fprintf fmt "%s"
    ( match b with
    | And -> "and"
    | Or -> "or"
    | Gt -> ">"
    | Gte -> ">="
    | Lt -> "<"
    | Lte -> "<="
    | Eq -> "=="
    | Neq -> "!=" )

let format_filter fmt f =
  assert (f = VarIsTaxBenefit);
  fprintf fmt "VarIsTaxBenefit"

let rec format_expression fmt expr =
  match Pos.unmark expr with
  | Constant i -> fprintf fmt "%d" i
  | Variable sv -> format_scoped_var fmt sv
  | Unop (Minus, e) -> fprintf fmt "- (%a)" format_expression e
  | Call (f, args) ->
      fprintf fmt "%a(%a)" format_callable f
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") format_scoped_var)
        args
  | Binop (e1, b, e2) ->
      fprintf fmt "(%a %a %a)" format_expression e1 format_binop b format_expression e2

let rec format_stmt fmt (stmt : mpp_stmt) =
  match Pos.unmark stmt with
  | Assign (sv, e) -> fprintf fmt "%a = %a" format_scoped_var sv format_expression e
  | Conditional (cond, t, []) ->
      fprintf fmt "if(%a):@\n@[<h 2>  %a@]" format_expression cond format_stmts t
  | Conditional (cond, t, f) ->
      fprintf fmt "if(%a):@\n@[<h 2>  %a@]else:@\n@[<h 2>  %a@]" format_expression cond format_stmts
        t format_stmts f
  | Delete sv -> fprintf fmt "del %a" format_scoped_var sv
  | Expr e -> format_expression fmt e
  | Partition (f, body) ->
      fprintf fmt "partition with %a:@\n@[<h 2>  %a@]" format_filter f format_stmts body

and format_stmts fmt stmts =
  Format.pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") format_stmt fmt stmts

let format_compute fmt compute =
  fprintf fmt "%s(%a):@\n@[<h 2>  %a@]@\n" compute.name
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") format_scoped_var)
    compute.args format_stmts compute.body

let format_program fmt mpp = pp_print_list format_compute fmt mpp
