(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr> Raphaël Monat <raphael.monat@lip6.fr>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

type scope =
  | Input
  (* is this suppposed to be read/written in the future inputs? *)
  | Output

(* is this supposed to be read from a previous computation *)

type scoped_var =
  | Local of string
  (* lowercase variable used only to define something locally *)
  | Mbased of Mir.Variable.t * scope

(* variables defined in the M codebase *)

type mpp_compute_name = string

type mpp_filter = VarIsTaxBenefit

type unop = Minus

type binop = Mpp_ast.binop

type mpp_expr = mpp_expr_kind Pos.marked

and mpp_expr_kind =
  | Constant of int
  | Variable of scoped_var
  | Unop of unop * mpp_expr
  | Call of mpp_callable * scoped_var list
  | Binop of mpp_expr * binop * mpp_expr

and mpp_callable =
  | Rules of Mast.DomainId.t (* M codebase *)
  | Chain of Mast.chaining (* M codebase *)
  | Verifs of Mast.DomainId.t * mpp_expr
  | NbVarCat of Mir.CatVarSet.t
  (* M codebase *)
  | MppFunction of mpp_compute_name
  | Present
  | Abs
  | Cast (* cast undefined to 0, identity function otherwise *)
  | DepositDefinedVariables
  | TaxbenefitCeiledVariables
  | TaxbenefitDefinedVariables

type mpp_stmt = mpp_stmt_kind Pos.marked

and mpp_stmt_kind =
  | Assign of scoped_var * mpp_expr
  | Conditional of mpp_expr * mpp_stmt list * mpp_stmt list
  | Delete of scoped_var
  | Expr of mpp_expr
  | Partition of mpp_filter * mpp_stmt list

type mpp_compute = {
  name : mpp_compute_name;
  args : scoped_var list;
  body : mpp_stmt list;
}

type mpp_program = mpp_compute list
