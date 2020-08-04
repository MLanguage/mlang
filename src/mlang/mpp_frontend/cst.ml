type var = string
  (* | Local of string (\* lowercase variable used only to define something locally *\)
   * | Mbased of string (\* variables defined in the M codebase *\) *)

type compute_name = string

type callable = string
  (* | Program (\* M codebase *\)
   * | MppFunction of compute_name
   * | Present
   * | Abs
   * | Cast (\* cast undefined to 0, identity function otherwise *\)
   * | DepositDefinedVariables
   * | ExistsTaxbenefitCeiledVariables
   * | ExistsTaxbenefitDefinedVariables *)

type filter = VarIsTaxBenefit

type unop = Minus

type binop = And | Or | Gt | Gte | Lt | Lte | Eq | Neq

type expr =
  | Constant of int
  | Variable of var
  | Unop of unop * expr
  | Call of callable * expr list
  | Binop of expr * binop * expr

type stmt =
  | Assign of var * expr
  | Conditional of expr * stmt list * stmt list
  | Delete of var
  | Expr of expr
(* | MultiAssign of var list * expr *)
(* no multiassign: all assignments performed to input scope.
   Multiassign-ed variables would have output scope *)
  | Partition of filter * stmt list

type compute = {
    name: compute_name;
    args: var list;
    body: stmt list;
}

type program = compute list
