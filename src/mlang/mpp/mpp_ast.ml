type scope =
  | Input
  (* is this suppposed to be read/written in the future inputs? *)
  | Output

(* is this supposed to be read from a previous computation *)

type scoped_var =
  | Local of string (* lowercase variable used only to define something locally *)
  | Mbased of Mvg.Variable.t * scope

(* variables defined in the M codebase *)

type mpp_compute_name = string

type mpp_callable =
  | Program (* M codebase *)
  | MppFunction of mpp_compute_name
  | Present
  | Abs
  | Cast (* cast undefined to 0, identity function otherwise *)
  | DepositDefinedVariables
  | TaxbenefitCeiledVariables
  | TaxbenefitDefinedVariables

type mpp_filter = VarIsTaxBenefit

type unop = Minus

type binop = Cst.binop

type mpp_expr = mpp_expr_kind Pos.marked

and mpp_expr_kind =
  | Constant of int
  | Variable of scoped_var
  | Unop of unop * mpp_expr
  | Call of mpp_callable * scoped_var list
  | Binop of mpp_expr * binop * mpp_expr

type mpp_stmt = mpp_stmt_kind Pos.marked

and mpp_stmt_kind =
  | Assign of scoped_var * mpp_expr
  | Delete of scoped_var
  | Expr of mpp_expr
  | Partition of mpp_filter * mpp_stmt list

type mpp_compute = { name : mpp_compute_name; args : scoped_var list; body : mpp_stmt list }

type mpp_program = mpp_compute list
