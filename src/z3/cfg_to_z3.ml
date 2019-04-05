(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by thge DGFiP.

This software is governed by the CeCILL-B license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-B
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-B license and that you accept its terms.
*)

let bv_repr_ints_base = 20

let declare_var (var: Cfg.Variable.t) (typ: Z3_repr.repr) (ctx: Z3.context) : Z3.Expr.expr =
  match typ with
  | Z3_repr.Boolean ->
    Z3.Boolean.mk_const_s ctx (Ast.unmark var.Cfg.Variable.name)
  | Z3_repr.Integer o ->
    Z3.BitVector.mk_const_s ctx (Ast.unmark var.Cfg.Variable.name) (bv_repr_ints_base * o)
  | Z3_repr.Real o ->
    Z3.BitVector.mk_const_s ctx (Ast.unmark var.Cfg.Variable.name) (bv_repr_ints_base * o)

let declare_local_var (var: Cfg.LocalVariable.t) (typ: Z3_repr.repr) (ctx: Z3.context) : Z3.Expr.expr =
  match typ with
  | Z3_repr.Boolean ->
    Z3.Boolean.mk_const_s ctx
      ("t" ^ (string_of_int var.Cfg.LocalVariable.id))
  | Z3_repr.Integer o ->
    Z3.BitVector.mk_const_s ctx
      ("t" ^ (string_of_int var.Cfg.LocalVariable.id)) (bv_repr_ints_base * o)
  | Z3_repr.Real o ->
    Z3.BitVector.mk_const_s ctx
      ("t" ^ (string_of_int var.Cfg.LocalVariable.id)) (bv_repr_ints_base * o)

let int_const i ctx : Z3.Expr.expr =
  Z3.BitVector.mk_numeral ctx (string_of_int i) bv_repr_ints_base

let bool_const b ctx : Z3.Expr.expr =
  if b then
    Z3.Boolean.mk_true ctx
  else
    Z3.Boolean.mk_false ctx

let error_const ctx : Z3.Expr.expr =
  Z3.Expr.mk_numeral_string ctx "error" (Z3.Sort.mk_uninterpreted_s ctx "error")


let rec translate_expression
    (repr_data: Z3_repr.repr_data)
    (e: Cfg.expression Ast.marked)
    (ctx: Z3.context)
    (s: Z3.Solver.solver)
  : Z3.Expr.expr =
  match Ast.unmark e with
  | Cfg.Comparison (op, e1, e2) ->
    let z3_e1 = translate_expression repr_data e1 ctx s in
    let z3_e2 = translate_expression repr_data e2 ctx s in
    begin match Ast.unmark op with
      | Ast.Gt -> Z3.BitVector.mk_sgt ctx z3_e1 z3_e2
      | Ast.Gte -> Z3.BitVector.mk_sge ctx z3_e1 z3_e2
      | Ast.Lt -> Z3.BitVector.mk_slt ctx z3_e1 z3_e2
      | Ast.Lte -> Z3.BitVector.mk_sle ctx z3_e1 z3_e2
      | Ast.Eq -> Z3.Boolean.mk_eq ctx z3_e1 z3_e2
      | Ast.Neq -> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx z3_e1 z3_e2)
    end
  | Cfg.Binop (op, e1, e2) ->
    let z3_e1 = translate_expression repr_data e1 ctx s in
    let z3_e2 = translate_expression repr_data e2 ctx s in
    begin match Ast.unmark op with
      | Ast.And -> Z3.Boolean.mk_and ctx [z3_e1; z3_e2]
      | Ast.Or -> Z3.Boolean.mk_or ctx [z3_e1; z3_e2]
      | Ast.Mul -> Z3.BitVector.mk_mul ctx z3_e1 z3_e2
      | Ast.Div -> Z3.BitVector.mk_sdiv ctx z3_e1 z3_e2
      | Ast.Sub -> Z3.BitVector.mk_sub ctx z3_e1 z3_e2
      | Ast.Add -> Z3.BitVector.mk_add ctx z3_e1 z3_e2
    end
  | Cfg.Unop (op, e1) ->
    let z3_e1 = translate_expression repr_data e1 ctx s in
    begin match op with
      | Ast.Not -> Z3.Boolean.mk_not ctx z3_e1
      | Ast.Minus -> Z3.BitVector.mk_sub ctx (int_const 0 ctx) z3_e1
    end
  | Cfg.Index (var, index) ->
    assert false (* TODO: implement *)
  | Cfg.LocalLet (lvar1, (Cfg.Conditional (e1, e2, e3), _), (Cfg.LocalVar lvar2, _))
    when lvar1 = lvar2 ->
    let z3_e1 = translate_expression repr_data e1 ctx s in
    let z3_e2 = translate_expression repr_data e2 ctx s in
    let z3_e3 = translate_expression repr_data e3 ctx s in
    let (z3_lvar, _) = Cfg.LocalVariableMap.find lvar1 repr_data.Z3_repr.repr_data_local_var in
    Z3.Solver.add s [
      Z3.Boolean.mk_implies ctx z3_e1 (Z3.Boolean.mk_eq ctx z3_lvar z3_e2);
      Z3.Boolean.mk_implies ctx
        (Z3.Boolean.mk_not ctx z3_e1)
        (Z3.Boolean.mk_eq ctx z3_lvar z3_e3)
    ];
    z3_lvar
  | Cfg.Conditional _ -> assert false (* should not happen *)
  | Cfg.FunctionCall (Cfg.ArrFunc , [arg]) ->
    assert false (* TODO: implement *)
  | Cfg.FunctionCall (Cfg.InfFunc , [arg]) ->
    assert false (* TODO: implement *)
  | Cfg.FunctionCall _ -> assert false (* should not happen *)
  | Cfg.Literal (Cfg.Int i) ->
    int_const i ctx
  | Cfg.Literal (Cfg.Float f) ->
    int_const (int_of_float (f *. 100.0)) ctx
  | Cfg.Literal (Cfg.Bool b) ->
    bool_const b ctx
  | Cfg.Var var ->
    let (z3_var , _) = Cfg.VariableMap.find var repr_data.Z3_repr.repr_data_var in
    z3_var
  | Cfg.LocalVar lvar ->
    let (z3_lvar , _) =
      Cfg.LocalVariableMap.find lvar repr_data.Z3_repr.repr_data_local_var
    in
    z3_lvar
  | Cfg.GenericTableIndex -> assert false (* TODO: implement *)
  | Cfg.Error -> error_const ctx
  | Cfg.LocalLet (lvar, e1, e2) ->
    let z3_e1 = translate_expression repr_data e1 ctx s in
    let z3_e2 = translate_expression repr_data e2 ctx s in
    let (z3_lvar, _) = Cfg.LocalVariableMap.find lvar repr_data.Z3_repr.repr_data_local_var in
    Z3.Solver.add s [Z3.Boolean.mk_eq ctx z3_lvar z3_e1];
    z3_e2


let translate_program
    (p: Cfg.program)
    (typing: Z3_repr.repr_info)
    (ctx: Z3.context)
    (s: Z3.Solver.solver)
  : Z3_repr.repr_data =
  (* first we declare to Z3 all the variables *)
  let z3_vars = Cfg.VariableMap.mapi (fun var typ ->
      try
        (declare_var var typ ctx, typ)
      with
      | Not_found -> assert false (* should not happen *)
    ) typing.Z3_repr.repr_info_var in
  let z3_local_vars =  Cfg.LocalVariableMap.mapi (fun lvar typ ->
      try
        (declare_local_var lvar typ ctx, typ)
      with
      | Not_found -> assert false (* should not happen *)
    ) typing.Z3_repr.repr_info_local_var in
  let repr_data =
    { Z3_repr.repr_data_var = z3_vars; Z3_repr.repr_data_local_var = z3_local_vars }
  in
  Cfg.VariableMap.iter (fun var def ->
      match def.Cfg.var_definition with
      | Cfg.InputVar -> () (* TODO: retrieve the constraints from the input values attributes? *)
      | Cfg.SimpleVar e ->
        let z3_e = translate_expression repr_data e ctx s in
        let (z3_var, _) = Cfg.VariableMap.find var repr_data.Z3_repr.repr_data_var in
        Z3.Solver.add s [Z3.Boolean.mk_eq ctx z3_var z3_e]
      | Cfg.TableVar (size, def) -> begin match def with
          | Cfg.IndexGeneric e ->
            () (* TODO: implement *)
          | Cfg.IndexTable es ->
            () (* TODO: implement *)
        end
    ) p;
  repr_data
