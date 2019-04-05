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

type repr =
  | Integer of int
  | Real of int
  | Boolean

let rec find_bitvec_order_expr
    (e: Cfg.expression Ast.marked)
    (new_typing: repr Cfg.VariableMap.t)
    (lvar_typing: int Cfg.LocalVariableMap.t)
  : int = match Ast.unmark e with
  | Cfg.Comparison _ -> 1
  | Cfg.Binop (((Ast.Add, _) | (Ast.Sub, _)), e1, e2)
  | Cfg.Conditional (_, e1, e2)  ->
    let o1 = find_bitvec_order_expr e1 new_typing lvar_typing in
    let o2 = find_bitvec_order_expr e2 new_typing lvar_typing in
    max o1 o2
  | Cfg.Binop (((Ast.Mul, _) | (Ast.Div, _)), e1, e2) ->
    let o1 = find_bitvec_order_expr e1 new_typing lvar_typing in
    let o2 = find_bitvec_order_expr e2 new_typing lvar_typing in
    o1 + o2
  | Cfg.Unop (_, e1) ->
    find_bitvec_order_expr e1 new_typing lvar_typing
  | Cfg.Index ((var, _), _) | Cfg.Var var ->
    begin try match Cfg.VariableMap.find var new_typing with
      | Boolean -> 1
      | Integer o | Real o -> o
      with
      | Not_found -> assert false (* should not happen *)
    end
  | Cfg.FunctionCall (Cfg.ArrFunc, [arg])
  | Cfg.FunctionCall (Cfg.InfFunc, [arg]) ->
    find_bitvec_order_expr arg new_typing lvar_typing
  | Cfg.Literal _ -> 1
  | Cfg.LocalVar lvar ->
    Cfg.LocalVariableMap.find lvar lvar_typing
  | Cfg.GenericTableIndex -> 1 (* should be changed... *)
  | Cfg.Error -> 1
  | Cfg.LocalLet (lvar, e1, e2) ->
    let o1 = find_bitvec_order_expr e1 new_typing lvar_typing in
    let lvar_typing = Cfg.LocalVariableMap.add lvar o1 lvar_typing in
    find_bitvec_order_expr e2 new_typing lvar_typing
  | _ -> assert false (* should not happen *)

let find_bitvec_order
    (program:Cfg.program)
    (var:Cfg.Variable.t)
    (new_typing: repr Cfg.VariableMap.t)
  : int =
  let var_def = Cfg.VariableMap.find var program in
  match var_def.Cfg.var_definition with
  | Cfg.InputVar -> 1
  | Cfg.SimpleVar e ->
    find_bitvec_order_expr e new_typing Cfg.LocalVariableMap.empty
  | Cfg.TableVar (size, def) -> begin match def with
      | Cfg.IndexGeneric e ->
        find_bitvec_order_expr e new_typing Cfg.LocalVariableMap.empty
      | Cfg.IndexTable es ->
        let orders =
          Cfg.IndexMap.map (fun e ->
              find_bitvec_order_expr e new_typing Cfg.LocalVariableMap.empty
            ) es
        in
        List.fold_left (fun acc (_, order) ->
            max order acc
          ) 1 (Cfg.IndexMap.bindings orders)
    end

let find_bitvec_repr
    (p: Cfg.program)
    (dep_graph : Dependency.DepGraph.t)
    (old_typing: Cfg.typ Cfg.VariableMap.t)
  : repr Cfg.VariableMap.t =
  let vars =
    Dependency.TopologicalOrder.fold (fun var acc -> var::acc) dep_graph []
  in
  (*
    The variables are ordered starting from the input and by usage so when we find
    a variable B in the definition of A then B should already have been processed.
  *)
  List.fold_left (fun new_typing var ->
      match Cfg.VariableMap.find var old_typing with
      | Cfg.Boolean -> Cfg.VariableMap.add var Boolean new_typing
      | Cfg.Integer ->
        let bitvec_order = find_bitvec_order p var new_typing in
        Cfg.VariableMap.add var (Integer bitvec_order) new_typing
      | Cfg.Real ->
        let bitvec_order = find_bitvec_order p var new_typing in
        Cfg.VariableMap.add var (Real bitvec_order) new_typing
    ) Cfg.VariableMap.empty vars
