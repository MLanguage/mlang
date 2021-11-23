type var = string

type compute_name = string

type callable = string

type filter = string

type unop = Minus

type binop = And | Or | Gt | Gte | Lt | Lte | Eq | Neq

type expr = expr_kind Pos.marked

and expr_kind =
  | Constant of int
  | Variable of var
  | Unop of unop * expr
  | Call of callable Pos.marked * var Pos.marked list
  | Binop of expr * binop * expr

type stmt = stmt_kind Pos.marked

and stmt_kind =
  | Assign of var * expr
  | Conditional of expr * stmt list * stmt list
  | Delete of var
  | Expr of expr
  (* | MultiAssign of var list * expr *)
  (* no multiassign: all assignments performed to input scope. Multiassign-ed
     variables would have output scope *)
  | Partition of filter * stmt list

type compute = { name : compute_name; args : var list; body : stmt list }

type program = compute list
