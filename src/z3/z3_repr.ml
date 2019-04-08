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

type repr_kind =
  | Integer of int
  | Real of int
  | Boolean

type repr = { repr_kind: repr_kind; is_table: bool }

type repr_info = {
  repr_info_var : repr Cfg.VariableMap.t;
  repr_info_local_var : repr Cfg.LocalVariableMap.t
}

type var_repr =
  | Regular of Z3.Expr.expr
  | Table of (Z3.Expr.expr -> Z3.Expr.expr)


type repr_data = {
  repr_data_var : (var_repr * repr) Cfg.VariableMap.t;
  repr_data_local_var : (var_repr * repr) Cfg.LocalVariableMap.t
}


let rec find_bitvec_order_expr
    (e: Cfg.expression Ast.marked)
    (new_typing: repr Cfg.VariableMap.t)
    (lvar_typing: repr Cfg.LocalVariableMap.t)
    (old_typing: Typechecker.typ_info)
  : (int * repr Cfg.LocalVariableMap.t) = match Ast.unmark e with
  | Cfg.Comparison _ -> (1, lvar_typing)
  | Cfg.Binop (((Ast.And, _) | (Ast.Or, _)), _, _) ->
    (1, lvar_typing)
  | Cfg.Binop (((Ast.Add, _) | (Ast.Sub, _)), e1, e2)
  | Cfg.Conditional (_, e1, e2)  ->
    let (o1, lvar_typing) =
      find_bitvec_order_expr e1 new_typing lvar_typing old_typing
    in
    let (o2, lvar_typing) =
      find_bitvec_order_expr e2 new_typing lvar_typing old_typing
    in
    (max o1 o2, lvar_typing)
  | Cfg.Binop (((Ast.Mul, _) | (Ast.Div, _)), e1, e2) ->
    let (o1, lvar_typing) =
      find_bitvec_order_expr e1 new_typing lvar_typing old_typing
    in
    let (o2, lvar_typing) =
      find_bitvec_order_expr e2 new_typing lvar_typing old_typing
    in
    (o1 + o2, lvar_typing)
  | Cfg.Unop (_, e1) ->
    find_bitvec_order_expr e1 new_typing lvar_typing old_typing
  | Cfg.Index ((var, _), _) | Cfg.Var var ->
    begin try match (Cfg.VariableMap.find var new_typing).repr_kind with
      | Boolean -> (1, lvar_typing)
      | Integer o | Real o -> (o, lvar_typing)
      with
      | Not_found -> assert false (* should not happen *)
    end
  | Cfg.FunctionCall (Cfg.ArrFunc, [arg])
  | Cfg.FunctionCall (Cfg.InfFunc, [arg]) ->
    find_bitvec_order_expr arg new_typing lvar_typing old_typing
  | Cfg.Literal _ -> (1, lvar_typing)
  | Cfg.LocalVar lvar ->
    begin try match  (Cfg.LocalVariableMap.find lvar lvar_typing).repr_kind with
      | Boolean -> (1, lvar_typing)
      | Integer o | Real o -> (o, lvar_typing)
      with
      | Not_found -> assert false (* should not happen *)
    end
  | Cfg.GenericTableIndex -> (1, lvar_typing) (* should be changed... *)
  | Cfg.Error -> (1, lvar_typing)
  | Cfg.LocalLet (lvar, e1, e2) ->
    let (o1, lvar_typing) =
      find_bitvec_order_expr e1 new_typing lvar_typing old_typing
    in
    let lvar_repr =
      { repr_kind =
          begin match Cfg.LocalVariableMap.find lvar old_typing.Typechecker.typ_info_local_var with
            | Cfg.Boolean -> Boolean
            | Cfg.Integer -> Integer o1
            | Cfg.Real -> Real o1
          end;
        is_table = false
      }
    in
    let lvar_typing = Cfg.LocalVariableMap.add lvar lvar_repr lvar_typing in
    find_bitvec_order_expr e2 new_typing lvar_typing old_typing
  | _ -> assert false (* should not happen *)

let find_bitvec_order
    (program:Cfg.program)
    (var:Cfg.Variable.t)
    (new_typing: repr_info)
    (old_typing: Typechecker.typ_info)
  : (int * repr_info) =
  let var_def = Cfg.VariableMap.find var program in
  match var_def.Cfg.var_definition with
  | Cfg.InputVar -> (1, new_typing)
  | Cfg.SimpleVar e ->
    let (o, lvar_new_typing) =
      find_bitvec_order_expr
        e
        new_typing.repr_info_var
        new_typing.repr_info_local_var
        old_typing
    in
    (o, {new_typing with repr_info_local_var = lvar_new_typing})
  | Cfg.TableVar (size, def) -> begin match def with
      | Cfg.IndexGeneric e ->
        let (o, lvar_new_typing) =
          find_bitvec_order_expr
            e
            new_typing.repr_info_var
            new_typing.repr_info_local_var
            old_typing
        in
        (o, {new_typing with repr_info_local_var = lvar_new_typing})
      | Cfg.IndexTable es ->
        let (orders, new_typing) =
          Cfg.IndexMap.fold (fun i e (acc, new_typing) ->
              let (o, lvar_new_typing) =
                find_bitvec_order_expr
                  e
                  new_typing.repr_info_var
                  new_typing.repr_info_local_var
                  old_typing
              in
              (
                Cfg.IndexMap.add i o acc,
                {new_typing with repr_info_local_var = lvar_new_typing}
              )
            ) es (Cfg.IndexMap.empty, new_typing)
        in
        (List.fold_left (fun acc (_, order) ->
             max order acc
           ) 1 (Cfg.IndexMap.bindings orders), new_typing)
    end

let find_bitvec_repr
    (p: Cfg.program)
    (dep_graph : Dependency.DepGraph.t)
    (old_typing: Typechecker.typ_info)
  : repr_info =
  let vars =
    Dependency.TopologicalOrder.fold (fun var acc -> var::acc) dep_graph []
  in
  (*
    The variables are ordered starting from the input and by usage so when we find
    a variable B in the definition of A then B should already have been processed.
  *)
  List.fold_left (fun (new_typing : repr_info) var ->
      match Cfg.VariableMap.find var old_typing.Typechecker.typ_info_var with
      | (Cfg.Boolean, is_table) ->
        {new_typing with
         repr_info_var =
           Cfg.VariableMap.add var { repr_kind = Boolean; is_table }
             new_typing.repr_info_var }
      | (Cfg.Integer, is_table) ->
        let (bitvec_order, new_typing) = find_bitvec_order p var new_typing old_typing in
        {new_typing with
         repr_info_var =
           Cfg.VariableMap.add var { repr_kind = Integer bitvec_order ; is_table }
             new_typing.repr_info_var }
      | (Cfg.Real, is_table) ->
        let (bitvec_order, new_typing) = find_bitvec_order p var new_typing old_typing in
        {new_typing with
         repr_info_var =
           Cfg.VariableMap.add var { repr_kind = Real bitvec_order; is_table}
             new_typing.repr_info_var }
    ) {
    repr_info_var = Cfg.VariableMap.empty ;
    repr_info_local_var = Cfg.LocalVariableMap.empty;
  } vars
