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

open Cfg
open Dependency

let rec is_expr_completely_const (e: expression Ast.marked) : bool = match Ast.unmark e with
  | Conditional (e1, e2, e3) ->
    (is_expr_completely_const e1) && (is_expr_completely_const e2) && (is_expr_completely_const e3)
  | Comparison (_, e1, e2) | Binop (_, e1, e2) | LocalLet (_, e1, e2) ->
    (is_expr_completely_const e1) && (is_expr_completely_const e2)
  | Unop(_, e1) -> is_expr_completely_const e1
  | FunctionCall (_, args) ->
    List.fold_left (fun acc arg -> acc && (is_expr_completely_const arg)) true args
  | Index _ | Var _ | GenericTableIndex -> false
  | Error | Literal _ | LocalVar _ -> true

let is_var_completely_const (var : Variable.t) (p:program) : bool =
  match (VariableMap.find var p).var_definition with
  | InputVar -> false
  | SimpleVar e -> is_expr_completely_const e
  | TableVar (size, def) -> begin match def with
      | IndexGeneric e ->
        is_expr_completely_const e
      | IndexTable es ->
        false
    end

let get_const_variables_evaluation_order (g: DepGraph.t) (p: program) : Cfg.Variable.t list =
  let is_completely_const = fun var ->
    is_var_completely_const var p
  in
  let is_const = Constability.analyze is_completely_const g in
  let subgraph = DepGraph.fold_vertex (fun var subgraph ->
      if is_const var then subgraph else
        DepGraph.remove_vertex subgraph var
    ) g g in
  TopologicalOrder.fold (fun var acc -> var::acc) subgraph []

type ctx = expression Ast.marked LocalVariableMap.t

let rec partial_evaluation (ctx: ctx) (p: program) (e: expression Ast.marked) : expression Ast.marked =
  match Ast.unmark e with
  | Comparison (op, e1, e2) ->
    let new_e1 = partial_evaluation ctx p e1 in
    let new_e2 = partial_evaluation ctx p e2 in
    Ast.same_pos_as begin match (Ast.unmark op, Ast.unmark new_e1, Ast.unmark new_e2) with
      | (Ast.Gt, Literal (Int i1), Literal (Int i2)) -> Literal (Bool (i1 > i2))
      | (Ast.Gte, Literal (Int i1), Literal (Int i2)) -> Literal (Bool (i1 >= i2))
      | (Ast.Lt, Literal (Int i1), Literal (Int i2)) -> Literal (Bool (i1 < i2))
      | (Ast.Lte, Literal (Int i1), Literal (Int i2)) -> Literal (Bool (i1 <= i2))
      | (Ast.Eq, Literal (Int i1), Literal (Int i2)) -> Literal (Bool (i1 = i2))
      | (Ast.Neq, Literal (Int i1), Literal (Int i2)) -> Literal (Bool (i1 <> i2))
      | (Ast.Gt, Literal (Float f1), Literal (Float f2)) -> Literal (Bool (f1 > f2))
      | (Ast.Gte, Literal (Float f1), Literal (Float f2)) -> Literal (Bool (f1 >= f2))
      | (Ast.Lt, Literal (Float f1), Literal (Float f2)) -> Literal (Bool (f1 < f2))
      | (Ast.Lte, Literal (Float f1), Literal (Float f2)) -> Literal (Bool (f1 <= f2))
      | (Ast.Eq, Literal (Float f1), Literal (Float f2)) -> Literal (Bool (f1 = f2))
      | (Ast.Neq, Literal (Float f1), Literal (Float f2)) -> Literal (Bool (f1 <> f2))
      | _ -> Comparison (op, new_e1, new_e2)
    end e
  | Binop (op, e1, e2) ->
    let new_e1 = partial_evaluation ctx p e1 in
    let new_e2 = partial_evaluation ctx p e2 in
    Ast.same_pos_as begin match (Ast.unmark op, Ast.unmark new_e1, Ast.unmark new_e2) with
      | (Ast.Add, Literal (Int i1), Literal (Int i2)) -> Literal (Int (i1 + i2))
      | (Ast.Sub, Literal (Int i1), Literal (Int i2)) -> Literal (Int (i1 - i2))
      | (Ast.Mul, Literal (Int i1), Literal (Int i2)) -> Literal (Int (i1 * i2))
      | (Ast.Div, Literal (Int i1), Literal (Int i2)) -> Literal (Int (i1 / i2))
      | (Ast.Add, Literal (Float f1), Literal (Float f2)) -> Literal (Float (f1 +. f2))
      | (Ast.Sub, Literal (Float f1), Literal (Float f2)) -> Literal (Float (f1 -. f2))
      | (Ast.Mul, Literal (Float f1), Literal (Float f2)) -> Literal (Float (f1 *. f2))
      | (Ast.Div, Literal (Float f1), Literal (Float f2)) -> Literal (Float (f1 /. f2))
      | (Ast.Or, Literal (Bool b1), Literal (Bool b2)) -> Literal (Bool (b1 || b2))
      | (Ast.And, Literal (Bool b1), Literal (Bool b2)) -> Literal (Bool (b1 && b2))
      | _ -> Binop (op, new_e1, new_e2)
    end e
  | Unop (op, e1) ->
    let new_e1 = partial_evaluation ctx p e1 in
    Ast.same_pos_as begin match (op, Ast.unmark new_e1) with
      | (Ast.Not, Literal (Bool b1)) -> Literal (Bool (not b1))
      | (Ast.Minus, Literal (Int i1)) -> Literal (Int (- i1))
      | (Ast.Minus, Literal (Float f1)) -> Literal (Float (-. f1))
      | _ -> Unop (op, new_e1)
    end e
  | Conditional (e1, e2, e3) ->
    let new_e1 = partial_evaluation ctx p e1 in
    let new_e2 = partial_evaluation ctx p e2 in
    let new_e3 = partial_evaluation ctx p e3 in
    begin match Ast.unmark new_e1 with
      | Literal (Bool true) -> new_e2
      | Literal (Bool false) -> new_e3
      | _ -> Ast.same_pos_as (Conditional (new_e1, new_e2, new_e3)) e
    end
  | Index (var, e1) ->
    let new_e1 = partial_evaluation ctx p e1 in
    Ast.same_pos_as (Index(var, new_e1)) e
  | Literal _ -> e
  | Var var -> begin match (VariableMap.find var p).var_definition with
      | SimpleVar e | TableVar (_, IndexGeneric e) -> begin match Ast.unmark e with
          | Literal lit -> Ast.same_pos_as (Literal lit) e
          | _ -> e
        end
      | _ -> e
    end
  | LocalVar lvar -> begin try LocalVariableMap.find lvar ctx with
      | Not_found -> e
    end
  | GenericTableIndex -> e
  | Error -> e
  | LocalLet (lvar, e1, e2) ->
    let new_e1 = partial_evaluation ctx p e1 in
    begin match Ast.unmark new_e1 with
      | Literal _ ->
        let new_ctx = LocalVariableMap.add lvar new_e1 ctx in
        let new_e2 = partial_evaluation new_ctx p e2 in
        new_e2
      | _ ->
        let new_e2 = partial_evaluation ctx p e2 in
        Ast.same_pos_as (LocalLet(lvar, new_e1, new_e2)) e
    end
  | FunctionCall (func, args) ->
    Ast.same_pos_as
      (FunctionCall
         (func,
          List.map
            (fun arg -> partial_evaluation ctx p arg)
            args))
      e

let partially_evaluate (p: program) : program =
  VariableMap.map (fun def ->
      let new_def = match def.var_definition with
        | InputVar -> InputVar
        | SimpleVar e -> SimpleVar (partial_evaluation LocalVariableMap.empty p e)
        | TableVar (size, def) -> begin match def with
            | IndexGeneric e ->
              TableVar(size, IndexGeneric (partial_evaluation LocalVariableMap.empty p e))
            | IndexTable es ->
              TableVar(size, IndexTable (IndexMap.map (fun e -> partial_evaluation LocalVariableMap.empty p e) es))
          end
      in
      { def with var_definition = new_def }
    ) p


let propagate_constants (g: DepGraph.t) (p: program) : program =
  let const_vars = get_const_variables_evaluation_order g p in
  List.fold_left (fun p const_var ->
      let const_var_data = VariableMap.find const_var p in
      let new_const_var_def = match const_var_data.var_definition with
        | InputVar -> assert false (* should not happen *)
        | SimpleVar e -> SimpleVar (partial_evaluation LocalVariableMap.empty p e)
        | TableVar (size, def) -> begin match def with
            | IndexGeneric e ->
              TableVar(size, IndexGeneric (partial_evaluation LocalVariableMap.empty p e))
            | IndexTable es ->
              assert false (* should not happen *)
          end
      in
      VariableMap.add
        const_var
        {const_var_data with var_definition = new_const_var_def }
        p
    ) p const_vars
