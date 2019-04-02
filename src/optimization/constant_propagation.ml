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

let rec partial_evaluation (p: program) (e: expression Ast.marked) : expression Ast.marked =
  match Ast.unmark e with
  | Comparison (op, e1, e2) ->
    let new_e1 = partial_evaluation p e1 in
    let new_e2 = partial_evaluation p e2 in
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
    let new_e1 = partial_evaluation p e1 in
    let new_e2 = partial_evaluation p e2 in
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
    let new_e1 = partial_evaluation p e1 in
    Ast.same_pos_as begin match (op, Ast.unmark new_e1) with
      | (Ast.Not, Literal (Bool b1)) -> Literal (Bool (not b1))
      | (Ast.Minus, Literal (Int i1)) -> Literal (Int (- i1))
      | (Ast.Minus, Literal (Float f1)) -> Literal (Float (-. f1))
      | _ -> Unop (op, new_e1)
    end e
  | Conditional (e1, e2, e3) ->
    let new_e1 = partial_evaluation p e1 in
    let new_e2 = partial_evaluation p e2 in
    let new_e3 = partial_evaluation p e3 in
    begin match Ast.unmark e1 with
      | Literal (Bool true) -> new_e2
      | Literal (Bool false) -> new_e3
      | _ -> Ast.same_pos_as (Conditional (new_e1, new_e2, new_e3)) e
    end
  | _ -> e

let propagate_constants (g: DepGraph.t) (p: program) : program =
  let const_vars = get_const_variables_evaluation_order g p in
  List.fold_left (fun p const_var ->
      let const_var_data = VariableMap.find const_var p in
      let new_const_var_def = match const_var_data.var_definition with
        | InputVar -> assert false (* should not happen *)
        | SimpleVar e -> SimpleVar (partial_evaluation p e)
        | TableVar (size, def) -> begin match def with
            | IndexGeneric e ->
              TableVar(size, IndexGeneric (partial_evaluation p e))
            | IndexTable es ->
              assert false (* should not happen *)
          end
      in
      VariableMap.add
        const_var
        {const_var_data with var_definition = new_const_var_def }
        p
    ) p const_vars
