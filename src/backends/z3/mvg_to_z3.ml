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

let bv_repr_ints_base = 20

let declare_var_not_table (var: Mvg.Variable.t) (typ: Z3_encoding.repr) (ctx: Z3.context) : Z3.Expr.expr =
  match typ.Z3_encoding.repr_kind with
  | Z3_encoding.Boolean ->
    Z3.Boolean.mk_const_s ctx (Ast.unmark var.Mvg.Variable.name)
  | Z3_encoding.Integer o ->
    Z3.BitVector.mk_const_s ctx (Ast.unmark var.Mvg.Variable.name) (bv_repr_ints_base * o)
  | Z3_encoding.Real o ->
    Z3.BitVector.mk_const_s ctx (Ast.unmark var.Mvg.Variable.name) (bv_repr_ints_base * o)

let declare_local_var (var: Mvg.LocalVariable.t) (typ: Z3_encoding.repr) (ctx: Z3.context) : Z3_encoding.var_repr =
  match typ.Z3_encoding.repr_kind with
  | Z3_encoding.Boolean ->
    Z3_encoding.Regular (Z3.Boolean.mk_const_s ctx
                           ("t" ^ (string_of_int var.Mvg.LocalVariable.id)))
  | Z3_encoding.Integer o ->
    Z3_encoding.Regular (Z3.BitVector.mk_const_s ctx
                           ("t" ^ (string_of_int var.Mvg.LocalVariable.id))
                           (bv_repr_ints_base * o))
  | Z3_encoding.Real o ->
    Z3_encoding.Regular (Z3.BitVector.mk_const_s ctx
                           ("t" ^ (string_of_int var.Mvg.LocalVariable.id))
                           (bv_repr_ints_base * o))

let int_const i ctx : Z3.Expr.expr =
  Z3.BitVector.mk_numeral ctx (string_of_int i) (bv_repr_ints_base * !Z3_encoding.bitvec_size)

let dummy_param ctx typ =
  match typ.Z3_encoding.repr_kind with
  | Z3_encoding.Boolean ->
    Z3.Boolean.mk_true ctx
  | Z3_encoding.Integer o ->
    Z3.BitVector.mk_numeral ctx (string_of_int 0) (bv_repr_ints_base * o)
  | Z3_encoding.Real o ->
    Z3.BitVector.mk_numeral ctx (string_of_int 0) (bv_repr_ints_base * o)

let bool_const b ctx : Z3.Expr.expr =
  if b then
    Z3.Boolean.mk_true ctx
  else
    Z3.Boolean.mk_false ctx

let error_const ctx : Z3.Expr.expr =
  Z3.Expr.mk_numeral_string ctx "error" (Z3.Sort.mk_uninterpreted_s ctx "error")

(* let enforce_bitvec ctx e =
 *   match Z3.Sort.get_sort_kind (Z3.Expr.get_sort e) with Z3enums.BOOL_SORT -> ... *)

let harmonize_sizes (ctx: Z3.context) (e1: Z3.Expr.expr) (e2: Z3.Expr.expr) : Z3.Expr.expr * Z3.Expr.expr =
  e1, e2
  (* let s1 = Z3.BitVector.get_size (Z3.Expr.get_sort e1) in
   * let s2 = Z3.BitVector.get_size (Z3.Expr.get_sort e2) in
   * if s1 = s2 then
   *   (e1, e2)
   * else if s1 < s2 then
   *   (Z3.BitVector.mk_concat ctx
   *      (Z3.BitVector.mk_numeral ctx (string_of_int 0) (s2 - s1))
   *      e1,
   *    e2
   *   )
   * else
   *   (e1,
   *    Z3.BitVector.mk_concat ctx
   *      (Z3.BitVector.mk_numeral ctx (string_of_int 0) (s2 - s1))
   *      e2
   *   ) *)


let rec translate_expression
    (repr_data: Z3_encoding.repr_data)
    (e: Mvg.expression Ast.marked)
    (ctx: Z3.context)
    (s: Z3.Solver.solver)
  : Z3.Expr.expr -> Z3.Expr.expr =
  fun orig_arg ->
  match Ast.unmark e with
  | Mvg.Comparison (op, e1, e2) ->
    let z3_e1 = translate_expression repr_data e1 ctx s in
    let z3_e2 = translate_expression repr_data e2 ctx s in
    let (z3_e1, z3_e2) = harmonize_sizes ctx (z3_e1 orig_arg) (z3_e2 orig_arg) in
    begin match Ast.unmark op with
      | Ast.Gt -> Z3.BitVector.mk_sgt ctx z3_e1 z3_e2
      | Ast.Gte -> Z3.BitVector.mk_sge ctx z3_e1 z3_e2
      | Ast.Lt -> Z3.BitVector.mk_slt ctx z3_e1 z3_e2
      | Ast.Lte -> Z3.BitVector.mk_sle ctx z3_e1 z3_e2
      | Ast.Eq -> Z3.Boolean.mk_eq ctx z3_e1 z3_e2
      | Ast.Neq ->
        Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx z3_e1 z3_e2)
    end
  | Mvg.Binop (op, e1, e2) ->
    let z3_e1 = translate_expression repr_data e1 ctx s in
    let z3_e2 = translate_expression repr_data e2 ctx s in
    let (z3_e1, z3_e2) = harmonize_sizes ctx (z3_e1 orig_arg) (z3_e2 orig_arg) in
    (* Printf.printf "binop: z3_e1: %s (%d), z3_e2: %s (%d)\n"
     *   (Z3.Expr.to_string z3_e1)
     *   (Z3.BitVector.get_size (Z3.Expr.get_sort (z3_e1)))
     *   (Z3.Expr.to_string (z3_e2))
     *   (Z3.BitVector.get_size (Z3.Expr.get_sort (z3_e2))); *)
    begin match Ast.unmark op with
      | Ast.And -> Z3.Boolean.mk_and ctx [z3_e1; z3_e2]
      | Ast.Or -> Z3.Boolean.mk_or ctx [z3_e1; z3_e2]
      | Ast.Mul -> Z3.BitVector.mk_mul ctx (z3_e1) (z3_e2)
      | Ast.Div -> Z3.BitVector.mk_sdiv ctx (z3_e1) (z3_e2)
      | Ast.Sub -> Z3.BitVector.mk_sub ctx (z3_e1) (z3_e2)
      | Ast.Add -> Z3.BitVector.mk_add ctx (z3_e1) (z3_e2)
    end
  | Mvg.Unop (op, e1) ->
    let z3_e1 = translate_expression repr_data e1 ctx s in
    begin match op with
      | Ast.Not -> Z3.Boolean.mk_not ctx (z3_e1 orig_arg)
      | Ast.Minus -> Z3.BitVector.mk_sub ctx (int_const 0 ctx) (z3_e1 orig_arg)
    end
  | Mvg.Index ((var, _), index) ->
    let (z3_var , _) = Mvg.VariableMap.find var repr_data.Z3_encoding.repr_data_var in
    let z3_index = translate_expression repr_data index ctx s in
    begin match z3_var with
      | Z3_encoding.Table z3_var ->
        z3_var (z3_index orig_arg)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalLet (lvar1, (Mvg.Conditional (e1, e2, e3), _), (Mvg.LocalVar lvar2, _))
    when lvar1 = lvar2 ->
    let z3_e1 = translate_expression repr_data e1 ctx s in
    let z3_e2 = translate_expression repr_data e2 ctx s in
    let z3_e3 = translate_expression repr_data e3 ctx s in
    let (z3_lvar, _) = Mvg.LocalVariableMap.find lvar1 repr_data.Z3_encoding.repr_data_local_var in
    begin match z3_lvar with
      | Z3_encoding.Regular z3_lvar ->
        Z3.Solver.add s [
          Z3.Boolean.mk_implies ctx
            (z3_e1 orig_arg)
            (Z3.Boolean.mk_eq ctx z3_lvar (z3_e2 orig_arg));
          Z3.Boolean.mk_implies ctx
            (Z3.Boolean.mk_not ctx (z3_e1 orig_arg))
            (Z3.Boolean.mk_eq ctx z3_lvar (z3_e3 orig_arg))
        ];
        z3_lvar
      | _ -> assert false (* should not happen *)
    end
  | Mvg.Conditional (cond, tt, ff) ->
    let z3_cond = orig_arg |> translate_expression repr_data cond ctx s in
    let z3_tt = orig_arg |> translate_expression repr_data tt ctx s in
    let z3_ff = orig_arg |> translate_expression repr_data ff ctx s in
    (* this actually happens after desugaring due to rewriting basic functions into conditionals. I think. FIXME: We could add a check to verify it only happens here? *)
    Z3.Boolean.mk_ite ctx z3_cond z3_tt z3_ff
    (* Cli.debug_print "conditional:\n";
     * Cli.debug_print @@ Ast.format_position (Ast.get_position e);
     * (\* (Format_mvg.format_expression @@ fst e); *\)
     * Cli.debug_print @@ Mvg.show_expression (Ast.unmark e);
     * assert false (\* should not happen *\) *)
  | Mvg.FunctionCall (Mvg.ArrFunc , [arg]) ->
    (* Z3.FloatingPoint.RoundingMode.mk_round_nearest_ties_to_even *)
    let earg = orig_arg |> translate_expression repr_data arg ctx s in
    let eargadded = Z3.BitVector.mk_add ctx earg (int_const 50 ctx) in
    let hundred = int_const 100 ctx in
    let eargdivided = Z3.BitVector.mk_sdiv ctx eargadded hundred in
    Z3.BitVector.mk_mul ctx eargdivided hundred
    (* we just need to add 50, divide by 100 (this loses precision) and then multiply by 100 *)
  | Mvg.FunctionCall (Mvg.InfFunc , [arg]) ->
    let earg = orig_arg |> translate_expression repr_data arg ctx s in
    let hundred = int_const 100 ctx in
    let eargdivided = Z3.BitVector.mk_sdiv ctx earg hundred in
    Z3.BitVector.mk_mul ctx eargdivided hundred
  | Mvg.FunctionCall _ -> assert false (* should not happen *)
  | Mvg.Literal (Mvg.Int i) ->
    int_const (100 * i) ctx
  | Mvg.Literal (Mvg.Float f) ->
    (* Z3.FloatingPoint.mk_to_ieee_bv ctx (Z3.FloatingPoint.mk_numeral_f ctx f (Z3.FloatingPoint.mk_sort_64 ctx)) *)
    (* FIXME *)
    int_const (int_of_float (f *. 100.0)) ctx
  | Mvg.Literal (Mvg.Bool b) ->
    bool_const b ctx
  | Mvg.Literal Mvg.Undefined -> assert false (* TODO: implement *)
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
  | Mvg.GenericTableIndex -> orig_arg
  | Mvg.Error -> error_const ctx
  | Mvg.LocalLet (lvar, e1, e2) ->
    let z3_e1 = translate_expression repr_data e1 ctx s in
    let z3_e2 = translate_expression repr_data e2 ctx s in
    let (z3_lvar, _) = Mvg.LocalVariableMap.find lvar repr_data.Z3_encoding.repr_data_local_var in
    begin match z3_lvar with
      | Z3_encoding.Regular z3_lvar ->
        Z3.Solver.add s [Z3.Boolean.mk_eq ctx z3_lvar (z3_e1 orig_arg)];
        (z3_e2 orig_arg)
      | _ -> assert false (* should not happen *)
    end

(* let cast ctx size expr =
 *   let se = Z3.BitVector.get_size (Z3.Expr.get_sort expr) in
 *   if se = size then expr
 *   else if se < size then
 *     Z3.BitVector.mk_concat ctx
 *       (Z3.BitVector.mk_numeral ctx (string_of_int 0) (size - se)) expr
 *   else assert false *)

let translate_program
    (p: Mvg.program)
    (dep_graph: Dependency.DepGraph.t)
    (typing: Z3_encoding.repr_info)
    (ctx: Z3.context)
    (s: Z3.Solver.solver)
  : Z3_encoding.repr_data =
  (* first we declare to Z3 all the local variables *)
  let z3_local_vars =  Mvg.LocalVariableMap.mapi (fun lvar typ ->
      try
        (declare_local_var lvar typ ctx, typ)
      with
      | Not_found -> assert false (* should not happen *)
    ) typing.Z3_encoding.repr_info_local_var in
  let repr_data =
    { Z3_encoding.repr_data_var = Mvg.VariableMap.empty;
      Z3_encoding.repr_data_local_var = z3_local_vars }
  in
  let exec_order = Execution_order.get_execution_order p in
  let repr_data =
    List.fold_left (fun repr_data scc ->
        Mvg.VariableMap.fold
          (fun var () repr_data ->
             if Mvg.VariableMap.mem var p.program_vars then
               let def = Mvg.VariableMap.find var p.program_vars in
               let typ = Mvg.VariableMap.find var typing.Z3_encoding.repr_info_var in
               (* Cli.debug_print (Format.sprintf "Processing %s" (\*(Mvg.Variable.show var));*\) (Ast.unmark var.Mvg.Variable.name)); *)
               (* Cli.debug_print (Format.sprintf "|repr_data| = %d; |repr_data_local| = %d\n" (Mvg.VariableMap.cardinal repr_data.Z3_encoding.repr_data_var) (Mvg.LocalVariableMap.cardinal repr_data.Z3_encoding.repr_data_local_var)); *)
               match def.Mvg.var_definition with
               | Mvg.InputVar ->
                 Cli.debug_print (Format.sprintf "input %s, repr=%s\n" (Mvg.Variable.show var) (Z3_encoding.show_repr typ));
                 { repr_data with
                   Z3_encoding.repr_data_var =
                     Mvg.VariableMap.add
                       var
                       (Z3_encoding.Regular (declare_var_not_table var typ ctx), typ)
                       repr_data.Z3_encoding.repr_data_var
                 }
               | Mvg.SimpleVar e ->
                 (* Cli.debug_print (Format.sprintf "var: %s, type: %s\nexpr: %s\n" (Mvg.Variable.show var) (Z3_encoding.show_repr @@ Mvg.VariableMap.find var typing.repr_info_var) (Format_mvg.format_expression @@ fst e)); *)
                 let z3_e = translate_expression repr_data e ctx s in
                 let z3_var = declare_var_not_table var typ ctx in
                 let cast_expr = z3_e (dummy_param ctx typ)
                 (* cast ctx (Z3.BitVector.get_size (Z3.Expr.get_sort z3_var)) (z3_e (dummy_param ctx typ)) *) in
                 (* Printf.printf "\nz3_var: %s\nz3_e: %s (%d)\ncast_expr = %s\n"
                  *   (Z3.Expr.to_string z3_var)
                  *   (Z3.Expr.to_string (z3_e (dummy_param ctx typ)))
                  *   (Z3.BitVector.get_size (Z3.Expr.get_sort (z3_e (dummy_param ctx typ))))
                  *   (Z3.Expr.to_string cast_expr); *)
                 Z3.Solver.add s [Z3.Boolean.mk_eq ctx z3_var cast_expr];
                 { repr_data with
                   Z3_encoding.repr_data_var =
                     Mvg.VariableMap.add
                       var
                       (Z3_encoding.Regular z3_var, typ)
                       repr_data.Z3_encoding.repr_data_var
                 }
               | Mvg.TableVar (_, def) -> begin match def with
                   | Mvg.IndexGeneric e ->
                     let z3_e = translate_expression repr_data e ctx s in
                     { repr_data with
                       Z3_encoding.repr_data_var =
                         Mvg.VariableMap.add
                           var
                           (Z3_encoding.Table z3_e, typ)
                           repr_data.Z3_encoding.repr_data_var
                     }
                   | Mvg.IndexTable _ ->
                     Cli.warning_print "TODO: implement";
                     repr_data
                 end
             else
               let () = Cli.warning_print (Printf.sprintf "FIXME: verif condition on %s but in verif conds = %b\n" (Mvg.Variable.show var) (Mvg.VariableMap.mem var p.program_conds)) in
               repr_data
          )
          scc repr_data
      ) repr_data exec_order in
  Cli.debug_print "Translation finished!\n";
  repr_data
