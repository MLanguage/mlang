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

type repr_kind =
  | Integer of int
  | Real of int
  | Boolean
[@@deriving show]

type repr = { repr_kind: repr_kind; is_table: bool }
[@@deriving show]

type repr_info = {
  repr_info_var : repr Mvg.VariableMap.t;
  repr_info_local_var : repr Mvg.LocalVariableMap.t
}

type var_repr =
  | Regular of Z3.Expr.expr
  | Table of (Z3.Expr.expr -> Z3.Expr.expr)


type repr_data = {
  repr_data_var : (var_repr * repr) Mvg.VariableMap.t;
  repr_data_local_var : (var_repr * repr) Mvg.LocalVariableMap.t
}


let rec find_bitvec_order_expr
    (e: Mvg.expression Ast.marked)
    (new_typing: repr Mvg.VariableMap.t)
    (lvar_typing: repr Mvg.LocalVariableMap.t)
    (old_typing: Typechecker.typ_info)
  : (int * repr Mvg.LocalVariableMap.t) = match Ast.unmark e with
  | Mvg.Comparison _ -> (1, lvar_typing)
  | Mvg.Binop (((Ast.And, _) | (Ast.Or, _)), _, _) ->
    (1, lvar_typing)
  | Mvg.Binop (((Ast.Add, _) | (Ast.Sub, _)), e1, e2)
  | Mvg.Conditional (_, e1, e2)  ->
    let (o1, lvar_typing) =
      find_bitvec_order_expr e1 new_typing lvar_typing old_typing
    in
    let (o2, lvar_typing) =
      find_bitvec_order_expr e2 new_typing lvar_typing old_typing
    in
    (max o1 o2, lvar_typing)
  | Mvg.Binop (((Ast.Mul, _) | (Ast.Div, _)), e1, e2) ->
    let (o1, lvar_typing) =
      find_bitvec_order_expr e1 new_typing lvar_typing old_typing
    in
    let (o2, lvar_typing) =
      find_bitvec_order_expr e2 new_typing lvar_typing old_typing
    in
    (o1 + o2, lvar_typing)
  | Mvg.Unop (_, e1) ->
    find_bitvec_order_expr e1 new_typing lvar_typing old_typing
  | Mvg.Index ((var, _), _) | Mvg.Var var ->
    begin try match (Mvg.VariableMap.find var new_typing).repr_kind with
      | Boolean -> (1, lvar_typing)
      | Integer o | Real o -> (o, lvar_typing)
      with
      | Not_found -> assert false (* should not happen *)
    end
  | Mvg.FunctionCall (Mvg.ArrFunc, [arg])
  | Mvg.FunctionCall (Mvg.InfFunc, [arg]) ->
    find_bitvec_order_expr arg new_typing lvar_typing old_typing
  | Mvg.Literal _ -> (1, lvar_typing)
  | Mvg.LocalVar lvar ->
    begin try match  (Mvg.LocalVariableMap.find lvar lvar_typing).repr_kind with
      | Boolean -> (1, lvar_typing)
      | Integer o | Real o -> (o, lvar_typing)
      with
      | Not_found -> assert false (* should not happen *)
    end
  | Mvg.GenericTableIndex -> (1, lvar_typing) (* should be changed... *)
  | Mvg.Error -> (1, lvar_typing)
  | Mvg.LocalLet (lvar, e1, e2) ->
    let (o1, lvar_typing) =
      find_bitvec_order_expr e1 new_typing lvar_typing old_typing
    in
    let lvar_repr =
      { repr_kind =
          begin match Mvg.LocalVariableMap.find lvar old_typing.Typechecker.typ_info_local_var with
            | Mvg.Boolean -> Boolean
            | Mvg.Integer -> Integer o1
            | Mvg.Real -> Real o1
          end;
        is_table = false
      }
    in
    let lvar_typing = Mvg.LocalVariableMap.add lvar lvar_repr lvar_typing in
    find_bitvec_order_expr e2 new_typing lvar_typing old_typing
  | _ -> assert false (* should not happen *)

let find_bitvec_order
    (program:Mvg.program)
    (var:Mvg.Variable.t)
    (new_typing: repr_info)
    (old_typing: Typechecker.typ_info)
  : (int * repr_info) =
  let var_def = Mvg.VariableMap.find var program.program_vars in
  match var_def.Mvg.var_definition with
  | Mvg.InputVar -> (1, new_typing)
  | Mvg.SimpleVar e ->
    let (o, lvar_new_typing) =
      find_bitvec_order_expr
        e
        new_typing.repr_info_var
        new_typing.repr_info_local_var
        old_typing
    in
    (o, {new_typing with repr_info_local_var = lvar_new_typing})
  | Mvg.TableVar (_, def) -> begin match def with
      | Mvg.IndexGeneric e ->
        let (o, lvar_new_typing) =
          find_bitvec_order_expr
            e
            new_typing.repr_info_var
            new_typing.repr_info_local_var
            old_typing
        in
        (o, {new_typing with repr_info_local_var = lvar_new_typing})
      | Mvg.IndexTable es ->
        let (orders, new_typing) =
          Mvg.IndexMap.fold (fun i e (acc, new_typing) ->
              let (o, lvar_new_typing) =
                find_bitvec_order_expr
                  e
                  new_typing.repr_info_var
                  new_typing.repr_info_local_var
                  old_typing
              in
              (
                Mvg.IndexMap.add i o acc,
                {new_typing with repr_info_local_var = lvar_new_typing}
              )
            ) es (Mvg.IndexMap.empty, new_typing)
        in
        (List.fold_left (fun acc (_, order) ->
             max order acc
           ) 1 (Mvg.IndexMap.bindings orders), new_typing)
    end

let bitvec_size = ref 2

let find_bitvec_repr
    (p: Mvg.program)
    (dep_graph : Dependency.DepGraph.t)
    (old_typing: Typechecker.typ_info)
  : repr_info =
  let exec_order = Execution_order.get_execution_order p in
  let size = !bitvec_size in
  (*
    The variables are ordered starting from the input and by usage so when we find
    a variable B in the definition of A then B should already have been processed.
  *)
  let new_typing = { repr_info_var = Mvg.VariableMap.empty;
                     repr_info_local_var =
                       Mvg.LocalVariableMap.map (fun ty ->
                           match ty with
                           | Mvg.Boolean ->
                             {repr_kind = Boolean; is_table = false}
                           | Mvg.Integer ->
                             {repr_kind = Integer size; is_table = false}
                           | Mvg.Real ->
                             {repr_kind = Real size; is_table = false}
                         )
                       old_typing.typ_info_local_var } in
  List.fold_left (fun new_typing scc ->
      Mvg.VariableMap.fold
        (fun var () new_typing ->
           if Mvg.VariableMap.mem var old_typing.Typechecker.typ_info_var then
             match Mvg.VariableMap.find var old_typing.Typechecker.typ_info_var with
             | (Mvg.Boolean, is_table) ->
               {new_typing with
                repr_info_var =
                  Mvg.VariableMap.add var { repr_kind = Boolean; is_table }
                    new_typing.repr_info_var }
             | (Mvg.Integer, is_table) ->
               let (bitvec_order, new_typing) =
                 size, new_typing
                 (* find_bitvec_order p var new_typing old_typing *) in
               {new_typing with
                repr_info_var =
                  Mvg.VariableMap.add var { repr_kind = Integer bitvec_order ; is_table }
                    new_typing.repr_info_var }
             | (Mvg.Real, is_table) ->
               let (bitvec_order, new_typing) =
                 size, new_typing
                 (* find_bitvec_order p var new_typing old_typing *) in
               {new_typing with
                repr_info_var =
                  Mvg.VariableMap.add var { repr_kind = Real bitvec_order; is_table}
                    new_typing.repr_info_var }
           else
             let () = Cli.warning_print (Printf.sprintf "var %s not used when computing sizes\n" (Mvg.Variable.show var)) in
             (* fixme: add negation of condition to z3? *)
             new_typing
        ) scc new_typing
    ) new_typing exec_order
