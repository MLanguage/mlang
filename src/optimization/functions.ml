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

let rec expand_functions_expr (e: expression Ast.marked) : expression Ast.marked =
  match Ast.unmark e with
  | Comparison (op, e1, e2) ->
    let new_e1 = expand_functions_expr e1 in
    let new_e2 = expand_functions_expr e2 in
    Ast.same_pos_as (Comparison (op, new_e1, new_e2)) e
  | Binop (op, e1, e2) ->
    let new_e1 = expand_functions_expr e1 in
    let new_e2 = expand_functions_expr e2 in
    Ast.same_pos_as (Binop (op, new_e1, new_e2)) e
  | Unop (op, e1) ->
    let new_e1 = expand_functions_expr e1 in
    Ast.same_pos_as (Unop (op, new_e1)) e
  | Conditional (e1, e2, e3) ->
    let new_e1 = expand_functions_expr e1 in
    let new_e2 = expand_functions_expr e2 in
    let new_e3 = expand_functions_expr e3 in
    Ast.same_pos_as (Conditional (new_e1, new_e2, new_e3)) e
  | Index (var, e1) ->
    let new_e1 = expand_functions_expr e1 in
    Ast.same_pos_as (Index(var, new_e1)) e
  | Literal _ -> e
  | Var _ -> e
  | LocalVar _ -> e
  | GenericTableIndex -> e
  | Error -> e
  | LocalLet (lvar, e1, e2) ->
    let new_e1 = expand_functions_expr e1 in
    let new_e2 = expand_functions_expr e2 in
    Ast.same_pos_as (LocalLet(lvar, new_e1, new_e2)) e
  | FunctionCall (SumFunc, args) ->
    Ast.same_pos_as (
      List.fold_left (fun acc arg ->
          if acc = Error then
            Ast.unmark (expand_functions_expr arg)
          else
            Binop (Ast.same_pos_as Ast.Add e, Ast.same_pos_as acc e, expand_functions_expr arg)
        ) Error args
    ) e
  | FunctionCall (GtzFunc, [arg]) ->
    Ast.same_pos_as
      (Comparison (
          Ast.same_pos_as Ast.Gt e,
          expand_functions_expr arg,
          Ast.same_pos_as (Literal (Int 0)) e)
      ) e
  | FunctionCall (GtezFunc, [arg]) ->
    Ast.same_pos_as
      (Comparison (
          Ast.same_pos_as Ast.Gte e,
          expand_functions_expr arg,
          Ast.same_pos_as (Literal (Int 0)) e)
      ) e
  | FunctionCall (NullFunc, [arg]) ->
    Ast.same_pos_as
      (Comparison (
          Ast.same_pos_as Ast.Eq e,
          expand_functions_expr arg,
          Ast.same_pos_as (Literal (Int 0)) e)
      ) e
  | FunctionCall (PresentFunc, [arg]) ->
    Ast.same_pos_as
      (Comparison (
          Ast.same_pos_as Ast.Neq e,
          expand_functions_expr arg,
          Ast.same_pos_as (Literal (Int 0)) e)
      ) e
  | FunctionCall (MinFunc, args) ->
    Ast.same_pos_as (
      List.fold_left (fun acc arg ->
          if acc = Error then
            Ast.unmark (expand_functions_expr arg)
          else
            let new_arg = expand_functions_expr arg in
            let new_arg_var = LocalVariable.new_var () in
            let acc_var = LocalVariable.new_var () in
            LocalLet (
              acc_var,
              Ast.same_pos_as acc e,
              Ast.same_pos_as (LocalLet (
                  new_arg_var,
                  new_arg,
                  Ast.same_pos_as (
                    Conditional (
                      Ast.same_pos_as (Comparison (
                          Ast.same_pos_as Ast.Lt e,
                          Ast.same_pos_as (LocalVar new_arg_var) e,
                          Ast.same_pos_as (LocalVar acc_var) e
                        )) e,
                      Ast.same_pos_as (LocalVar new_arg_var) e,
                      Ast.same_pos_as (LocalVar acc_var) e
                    )
                  ) e
                ))
                e
            )
        ) Error args
    ) e
  | FunctionCall (MaxFunc, args) ->
    Ast.same_pos_as (
      List.fold_left (fun acc arg ->
          if acc = Error then
            Ast.unmark (expand_functions_expr arg)
          else
            let new_arg = expand_functions_expr arg in
            let new_arg_var = LocalVariable.new_var () in
            let acc_var = LocalVariable.new_var () in
            LocalLet (
              acc_var,
              Ast.same_pos_as acc e,
              Ast.same_pos_as (LocalLet (
                  new_arg_var,
                  new_arg,
                  Ast.same_pos_as (
                    Conditional (
                      Ast.same_pos_as (Comparison (
                          Ast.same_pos_as Ast.Gt e,
                          Ast.same_pos_as (LocalVar new_arg_var) e,
                          Ast.same_pos_as (LocalVar acc_var) e
                        )) e,
                      Ast.same_pos_as (LocalVar new_arg_var) e,
                      Ast.same_pos_as (LocalVar acc_var) e
                    )
                  ) e
                ))
                e
            )
        ) Error args
    ) e
  | _ -> e

let expand_functions (p: program) : program =
  VariableMap.map (fun def ->
      match def.var_definition with
      | InputVar -> def
      | SimpleVar e -> { def with var_definition = SimpleVar (expand_functions_expr e) }
      | TableVar (size, defg) -> begin match defg with
          | IndexGeneric e ->
            { def with
              var_definition =
                TableVar(size, IndexGeneric (expand_functions_expr e))
            }
          | IndexTable es ->
            { def with
              var_definition =
                TableVar(size,
                         IndexTable (IndexMap.map
                                       (fun e -> expand_functions_expr e)
                                       es))
            }
        end
    ) p
