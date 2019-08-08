(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by the DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
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
knowledge of the CeCILL-C license and that you accept its terms.
*)

let declare_var_not_table (var: Mvg.Variable.t) (typ: Z3_encoding.repr) (ctx: Z3.context) : Z3.Expr.expr =
  match typ.Z3_encoding.repr_kind with
  | Z3_encoding.Boolean ->
    Z3.Boolean.mk_const_s ctx (Ast.unmark var.Mvg.Variable.name)
  | Z3_encoding.Integer o ->
    Z3.BitVector.mk_const_s ctx (Ast.unmark var.Mvg.Variable.name) o
  | Z3_encoding.Real o ->
    Z3.BitVector.mk_const_s ctx (Ast.unmark var.Mvg.Variable.name) o

let declare_local_var (var: Mvg.LocalVariable.t) (typ: Z3_encoding.repr) (ctx: Z3.context) : Z3_encoding.var_repr =
  match typ.Z3_encoding.repr_kind with
  | Z3_encoding.Boolean ->
    Z3_encoding.Regular (Z3.Boolean.mk_const_s ctx
                           ("t" ^ (string_of_int var.Mvg.LocalVariable.id)))
  | Z3_encoding.Integer o ->
    Z3_encoding.Regular (Z3.BitVector.mk_const_s ctx
                           ("t" ^ (string_of_int var.Mvg.LocalVariable.id))
                           o)
  | Z3_encoding.Real o ->
    Z3_encoding.Regular (Z3.BitVector.mk_const_s ctx
                           ("t" ^ (string_of_int var.Mvg.LocalVariable.id))
                           o)

let int_const i ctx : Z3.Expr.expr =
  Z3.BitVector.mk_numeral ctx (string_of_int i) !Z3_encoding.bitvec_size

let bool_const b ctx : Z3.Expr.expr =
  if b then Z3.Boolean.mk_true ctx
  else Z3.Boolean.mk_false ctx

let error_const ctx : Z3.Expr.expr =
  Z3.Expr.mk_numeral_string ctx "error" (Z3.Sort.mk_uninterpreted_s ctx "error")

let mult_factor = 100

let rec translate_expression
    (repr_data: Z3_encoding.repr_data)
    (e: Mvg.expression Ast.marked)
    (ctx: Z3.context)
    (s: Z3.Solver.solver)
    (is_e_boolean: bool)
  : Z3.Expr.expr =
  let zero = int_const 0 ctx in
  let imult_factor = int_const mult_factor ctx in
  let hundred = imult_factor in
  match Ast.unmark e with
  | Mvg.Comparison (op, e1, e2) ->
    let z3_e1 = translate_expression repr_data e1 ctx s true in
    let z3_e2 = translate_expression repr_data e2 ctx s true in
    begin match Ast.unmark op with
      | Ast.Gt -> Z3.BitVector.mk_sgt ctx z3_e1 z3_e2
      | Ast.Gte -> Z3.BitVector.mk_sge ctx z3_e1 z3_e2
      | Ast.Lt -> Z3.BitVector.mk_slt ctx z3_e1 z3_e2
      | Ast.Lte -> Z3.BitVector.mk_sle ctx z3_e1 z3_e2
      | Ast.Eq -> Z3.Boolean.mk_eq ctx z3_e1 z3_e2
      | Ast.Neq -> Z3.Boolean.mk_distinct ctx [z3_e1; z3_e2]
    end
  | Mvg.Binop (op, e1, e2) ->
    let z3_e1 = translate_expression repr_data e1 ctx s false in
    let z3_e2 = translate_expression repr_data e2 ctx s false in
    begin match Ast.unmark op with
      | Ast.And -> Z3.Boolean.mk_and ctx [z3_e1; z3_e2]
      | Ast.Or -> Z3.Boolean.mk_or ctx [z3_e1; z3_e2]
      | Ast.Mul -> Z3.BitVector.mk_sdiv ctx (Z3.BitVector.mk_mul ctx z3_e1 z3_e2) imult_factor
      | Ast.Div -> Z3.BitVector.mk_sdiv ctx (Z3.BitVector.mk_mul ctx z3_e1 imult_factor) z3_e2
      | Ast.Sub -> Z3.BitVector.mk_sub ctx z3_e1 z3_e2
      | Ast.Add -> Z3.BitVector.mk_add ctx z3_e1 z3_e2
    end
  | Mvg.Unop (op, e1) ->
    let z3_e1 = translate_expression repr_data e1 ctx s (op = Not) in
    begin match op with
      | Ast.Not -> Z3.Boolean.mk_not ctx z3_e1
      | Ast.Minus -> Z3.BitVector.mk_sub ctx zero z3_e1
    end
  | Mvg.Index _ ->
    raise (Errors.Unimplemented "z3: Mvg.Index")
  | Mvg.Conditional (cond, tt, ff) ->
    let z3_cond = translate_expression repr_data cond ctx s true in
    let z3_tt = translate_expression repr_data tt ctx s false in
    let z3_ff = translate_expression repr_data ff ctx s false in
    (* this actually happens after desugaring due to rewriting basic functions into conditionals. *)
    Z3.Boolean.mk_ite ctx z3_cond z3_tt z3_ff
  | Mvg.FunctionCall (Mvg.ArrFunc , [arg]) ->
    (* we just need to add 50, divide by 100 (this loses precision) and then multiply by 100 *)
    let earg = translate_expression repr_data arg ctx s false in
    let eargadded = Z3.BitVector.mk_add ctx earg (int_const (mult_factor / 2) ctx) in
    let eargdivided = Z3.BitVector.mk_sdiv ctx eargadded hundred in
    Z3.BitVector.mk_mul ctx eargdivided hundred
  | Mvg.FunctionCall (Mvg.InfFunc , [arg]) ->
    let earg = translate_expression repr_data arg ctx s false in
    let eargdivided = Z3.BitVector.mk_sdiv ctx earg hundred in
    Z3.BitVector.mk_mul ctx eargdivided hundred
  | Mvg.FunctionCall (Mvg.PresentFunc, [arg]) ->
    let earg = translate_expression repr_data arg ctx s false in
    let cond = Z3.Boolean.mk_distinct ctx [earg; zero] in
    if is_e_boolean then  cond
    else
      Z3.Boolean.mk_ite ctx cond imult_factor zero
  | Mvg.FunctionCall (Mvg.NullFunc, [arg]) ->
    let earg = translate_expression repr_data arg ctx s false in
    Z3.Boolean.mk_eq ctx earg zero
  | Mvg.FunctionCall _ -> assert false (* should not happen *)
  | Mvg.Literal (Mvg.Int i) ->
    int_const (mult_factor * i) ctx
  | Mvg.Literal (Mvg.Float f) ->
    int_const (int_of_float (f *. (float_of_int mult_factor))) ctx
  | Mvg.Literal (Mvg.Bool b) ->
    if is_e_boolean then
      bool_const b ctx
    else if b then
      int_const (mult_factor * 1) ctx
    else zero
  | Mvg.Literal Mvg.Undefined ->
    Cli.warning_print "Unsafe cast from undef to 0";
    zero
  | Mvg.Var var ->
    let (z3_var , _) = Mvg.VariableMap.find var repr_data.Z3_encoding.repr_data_var in
    begin match z3_var with
      | Z3_encoding.Regular z3_var ->
        z3_var
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalVar lvar ->
    let (z3_lvar , _) =
      Mvg.LocalVariableMap.find lvar repr_data.Z3_encoding.repr_data_local_var
    in
    begin match z3_lvar with
      | Z3_encoding.Regular z3_lvar ->
        z3_lvar
      | _ -> assert false (* should not happen *)
    end
  | Mvg.GenericTableIndex ->
    raise (Errors.Unimplemented "z3: Mvg.GenericTableIndex")
  | Mvg.Error -> error_const ctx
  | Mvg.LocalLet (lvar, e1, e2) ->
    let z3_e1 = translate_expression repr_data e1 ctx s false in
    let z3_e2 = translate_expression repr_data e2 ctx s false in
    let (z3_lvar, _) = Mvg.LocalVariableMap.find lvar repr_data.Z3_encoding.repr_data_local_var in
    begin match z3_lvar with
      | Z3_encoding.Regular z3_lvar ->
        Z3.Solver.add s [Z3.Boolean.mk_eq ctx z3_lvar z3_e1];
        z3_e2
      | _ -> assert false (* should not happen *)
    end

let translate_program
    (p: Mvg.program)
    (typing: Z3_encoding.repr_info)
    (ctx: Z3.context)
    (s: Z3.Solver.solver)
  : Z3_encoding.repr_data =
  (* first we declare to Z3 all the local variables *)
  let z3_local_vars =  Mvg.LocalVariableMap.mapi (fun lvar typ ->
      try
        declare_local_var lvar typ ctx, typ
      with
      | Not_found -> assert false (* should not happen *)
    ) typing.Z3_encoding.repr_info_local_var in
  let repr_data =
    { Z3_encoding.repr_data_var = Mvg.VariableMap.empty;
      Z3_encoding.repr_data_local_var = z3_local_vars }
  in
  let repr_data =
    Execution_order.fold_on_vars
      (fun var repr_data ->
         if Mvg.VariableMap.mem var p.program_vars then
           let def = Mvg.VariableMap.find var p.program_vars in
           let typ = Mvg.VariableMap.find var typing.Z3_encoding.repr_info_var in
           match def.Mvg.var_definition with
           | Mvg.InputVar ->
             let in_var = declare_var_not_table var typ ctx in
             { repr_data with
               Z3_encoding.repr_data_var =
                 Mvg.VariableMap.add
                   var
                   (Z3_encoding.Regular in_var, typ)
                   repr_data.Z3_encoding.repr_data_var
             }
           | Mvg.SimpleVar e ->
             let z3_e = translate_expression repr_data e ctx s false in
             let z3_var = declare_var_not_table var typ ctx in
             Z3.Solver.add s [Z3.Boolean.mk_eq ctx z3_var z3_e];
             { repr_data with
               Z3_encoding.repr_data_var =
                 Mvg.VariableMap.add
                   var
                   (Z3_encoding.Regular z3_var, typ)
                   repr_data.Z3_encoding.repr_data_var
             }
           | Mvg.TableVar _ ->
             raise (Errors.Unimplemented "z3: Mvg.TableVar")
         else
           try
             let cond = Mvg.VariableMap.find var p.program_conds in
             (* FIXME: specify which error is raised in that case? *)
             let typ = {Z3_encoding.repr_kind = Boolean; is_table = false} in
             let z3_var = declare_var_not_table var typ ctx in
             begin match Ast.unmark cond.cond_expr with
               | Mvg.Unop (Ast.Not, cond_expr) ->
                 let z3_e = translate_expression repr_data cond_expr ctx s true in
                 Z3.Solver.add s [z3_e]
               | _ ->
                 let z3_e = translate_expression repr_data cond.cond_expr ctx s true in
                 let neg_z3e = Z3.Boolean.mk_not ctx z3_e in
                 Z3.Solver.add s [neg_z3e] end;
             { repr_data with
               Z3_encoding.repr_data_var =
                 Mvg.VariableMap.add var
                   (Z3_encoding.Regular z3_var, typ)
                   repr_data.Z3_encoding.repr_data_var }
           with Not_found -> assert false
      ) p repr_data in
  Cli.debug_print "Translation finished!";
  repr_data
