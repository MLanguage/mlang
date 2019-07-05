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

open Mvg

let truncatef x = snd (modf x)
let roundf x = snd (modf (x +. copysign 0.5 x))

type var_literal =
  | SimpleVar of literal
  | TableVar of int * literal array

let format_var_literal_with_var (var: Variable.t) (vl: var_literal) : string = match vl with
  | SimpleVar value ->
    Printf.sprintf "%s (%s): %s"
      (Ast.unmark var.Variable.name)
      (Ast.unmark var.Variable.descr)
      (Format_mvg.format_literal value)
  | TableVar (size, values) ->
    Printf.sprintf "%s (%s): Table (%d values)\n%s"
      (Ast.unmark var.Variable.name)
      (Ast.unmark var.Variable.descr)
      size
      (String.concat "\n"
         (List.mapi
            (fun idx value ->
               Printf.sprintf "| %d -> %s"
                 idx
                 (Format_mvg.format_literal value)
            ) (Array.to_list values)))

type ctx = {
  ctx_local_vars: literal Ast.marked LocalVariableMap.t;
  ctx_vars: var_literal VariableMap.t;
  ctx_generic_index: int option;
}

let empty_ctx : ctx = {
  ctx_local_vars = LocalVariableMap.empty;
  ctx_vars = VariableMap.empty;
  ctx_generic_index = None
}

let int_of_bool (b: bool) = if b then 1 else 0
let float_of_bool (b: bool) = if b then 1. else 0.

let is_zero (l: literal) : bool = match l with
  | Bool false | Int 0 | Float 0. -> true
  | _ -> false

let rec evaluate_expr (ctx: ctx) (p: program) (e: expression Ast.marked) : literal =
  match Ast.unmark e with
  | Comparison (op, e1, e2) ->
    let new_e1 = evaluate_expr ctx p e1 in
    let new_e2 = evaluate_expr ctx p e2 in
    begin match (Ast.unmark op, new_e1, new_e2) with
      | (Ast.Gt, Bool i1, Bool i2) -> Bool(i1 > i2)
      | (Ast.Gt, Bool i1, Int i2) -> Bool(int_of_bool i1 > i2)
      | (Ast.Gt, Bool i1, Float i2) -> Bool(float_of_bool i1 > i2)
      | (Ast.Gt, Int i1, Bool i2) -> Bool(i1 > int_of_bool i2)
      | (Ast.Gt, Int i1, Int i2) -> Bool(i1 > i2)
      | (Ast.Gt, Int i1, Float i2) -> Bool(float_of_int i1 > i2)
      | (Ast.Gt, Float i1, Bool i2) -> Bool(i1 > float_of_bool i2)
      | (Ast.Gt, Float i1, Int i2) -> Bool(i1 > float_of_int i2)
      | (Ast.Gt, Float i1, Float i2) -> Bool(i1 > i2)

      | (Ast.Gte, Bool i1, Bool i2) -> Bool(i1 >= i2)
      | (Ast.Gte, Bool i1, Int i2) -> Bool(int_of_bool i1 >=  i2)
      | (Ast.Gte, Bool i1, Float i2) -> Bool(float_of_bool i1 >= i2)
      | (Ast.Gte, Int i1, Bool i2) -> Bool(i1 >= int_of_bool i2)
      | (Ast.Gte, Int i1, Int i2) -> Bool(i1 >= i2)
      | (Ast.Gte, Int i1, Float i2) -> Bool(float_of_int i1 >= i2)
      | (Ast.Gte, Float i1, Bool i2) -> Bool(i1 >= float_of_bool i2)
      | (Ast.Gte, Float i1, Int i2) -> Bool(i1 >= float_of_int i2)
      | (Ast.Gte, Float i1, Float i2) -> Bool(i1 >= i2)

      | (Ast.Lt, Bool i1, Bool i2) -> Bool(i1 < i2)
      | (Ast.Lt, Bool i1, Int i2) -> Bool(int_of_bool i1 < i2)
      | (Ast.Lt, Bool i1, Float i2) -> Bool(float_of_bool i1 < i2)
      | (Ast.Lt, Int i1, Bool i2) -> Bool(i1 < int_of_bool i2)
      | (Ast.Lt, Int i1, Int i2) -> Bool(i1 < i2)
      | (Ast.Lt, Int i1, Float i2) -> Bool(float_of_int i1 < i2)
      | (Ast.Lt, Float i1, Bool i2) -> Bool(i1 < float_of_bool i2)
      | (Ast.Lt, Float i1, Int i2) -> Bool(i1 < float_of_int i2)
      | (Ast.Lt, Float i1, Float i2) -> Bool(i1 < i2)

      | (Ast.Lte, Bool i1, Bool i2) -> Bool(i1 <= i2)
      | (Ast.Lte, Bool i1, Int i2) -> Bool(int_of_bool i1 <= i2)
      | (Ast.Lte, Bool i1, Float i2) -> Bool(float_of_bool i1 <= i2)
      | (Ast.Lte, Int i1, Bool i2) -> Bool(i1 <= int_of_bool i2)
      | (Ast.Lte, Int i1, Int i2) -> Bool(i1 <= i2)
      | (Ast.Lte, Int i1, Float i2) -> Bool(float_of_int i1 <= i2)
      | (Ast.Lte, Float i1, Bool i2) -> Bool(i1 <= float_of_bool i2)
      | (Ast.Lte, Float i1, Int i2) -> Bool(i1 <= float_of_int i2)
      | (Ast.Lte, Float i1, Float i2) -> Bool(i1 <= i2)

      | (Ast.Eq, Bool i1, Bool i2) -> Bool(i1 = i2)
      | (Ast.Eq, Bool i1, Int i2) -> Bool(int_of_bool i1 = i2)
      | (Ast.Eq, Bool i1, Float i2) -> Bool(float_of_bool i1 = i2)
      | (Ast.Eq, Int i1, Bool i2) -> Bool(i1 = int_of_bool i2)
      | (Ast.Eq, Int i1, Int i2) -> Bool(i1 = i2)
      | (Ast.Eq, Int i1, Float i2) -> Bool(float_of_int i1 = i2)
      | (Ast.Eq, Float i1, Bool i2) -> Bool(i1 = float_of_bool i2)
      | (Ast.Eq, Float i1, Int i2) -> Bool(i1 = float_of_int i2)
      | (Ast.Eq, Float i1, Float i2) -> Bool(i1 = i2)

      | (Ast.Neq, Bool i1, Bool i2) -> Bool(i1 <> i2)
      | (Ast.Neq, Bool i1, Int i2) -> Bool(int_of_bool i1 <> i2)
      | (Ast.Neq, Bool i1, Float i2) -> Bool(float_of_bool i1 <> i2)
      | (Ast.Neq, Int i1, Bool i2) -> Bool(i1 <> int_of_bool i2)
      | (Ast.Neq, Int i1, Int i2) -> Bool(i1 <> i2)
      | (Ast.Neq, Int i1, Float i2) -> Bool(float_of_int i1 <> i2)
      | (Ast.Neq, Float i1, Bool i2) -> Bool(i1 <> float_of_bool i2)
      | (Ast.Neq, Float i1, Int i2) -> Bool(i1 <> float_of_int i2)
      | (Ast.Neq, Float i1, Float i2) -> Bool(i1 <> i2)
    end
  | Binop (op, e1, e2) ->
    let new_e1 = evaluate_expr ctx p e1 in
    let new_e2 = evaluate_expr ctx p e2 in
    begin match (Ast.unmark op, new_e1, new_e2) with
      | (Ast.Add, Bool i1, Bool i2)   -> Int   (int_of_bool i1   +  int_of_bool i2)
      | (Ast.Add, Bool i1, Int i2)    -> Int   (int_of_bool i1   +  i2)
      | (Ast.Add, Bool i1, Float i2)  -> Float (float_of_bool i1 +. i2)
      | (Ast.Add, Int i1, Bool i2)    -> Int   (i1               +  int_of_bool i2)
      | (Ast.Add, Int i1, Int i2)     -> Int   (i1               +  i2)
      | (Ast.Add, Int i1, Float i2)   -> Float (float_of_int i1  +. i2)
      | (Ast.Add, Float i1, Bool i2)  -> Float (i1               +. float_of_bool i2)
      | (Ast.Add, Float i1, Int i2)   -> Float (i1               +. float_of_int i2)
      | (Ast.Add, Float i1, Float i2) -> Float (i1               +. i2)

      | (Ast.Sub, Bool i1, Bool i2)   -> Int   (int_of_bool i1   -  int_of_bool i2)
      | (Ast.Sub, Bool i1, Int i2)    -> Int   (int_of_bool i1   -  i2)
      | (Ast.Sub, Bool i1, Float i2)  -> Float (float_of_bool i1 -. i2)
      | (Ast.Sub, Int i1, Bool i2)    -> Int   (i1               -  int_of_bool i2)
      | (Ast.Sub, Int i1, Int i2)     -> Int   (i1               -  i2)
      | (Ast.Sub, Int i1, Float i2)   -> Float (float_of_int i1  -. i2)
      | (Ast.Sub, Float i1, Bool i2)  -> Float (i1               -. float_of_bool i2)
      | (Ast.Sub, Float i1, Int i2)   -> Float (i1               -. float_of_int i2)
      | (Ast.Sub, Float i1, Float i2) -> Float (i1               -. i2)

      | (Ast.Mul, Bool i1, Bool i2)   -> Int   (int_of_bool i1   *  int_of_bool i2)
      | (Ast.Mul, Bool i1, Int i2)    -> Int   (int_of_bool i1   *  i2)
      | (Ast.Mul, Bool i1, Float i2)  -> Float (float_of_bool i1 *. i2)
      | (Ast.Mul, Int i1, Bool i2)    -> Int   (i1               *  int_of_bool i2)
      | (Ast.Mul, Int i1, Int i2)     -> Int   (i1               *  i2)
      | (Ast.Mul, Int i1, Float i2)   -> Float (float_of_int i1  *. i2)
      | (Ast.Mul, Float i1, Bool i2)  -> Float (i1               *. float_of_bool i2)
      | (Ast.Mul, Float i1, Int i2)   -> Float (i1               *. float_of_int i2)
      | (Ast.Mul, Float i1, Float i2) -> Float (i1               *. i2)

      | (Ast.Div, Bool false, _) -> Bool false
      | (Ast.Div, Int 0, _) -> Int 0
      | (Ast.Div, Float 0., _) -> Float 0.
      | (Ast.Div, _, l2) when is_zero l2 ->
        raise
          (Errors.RuntimeError (
              Errors.DivByZero (Format_ast.format_position (Ast.get_position e))
            ))

      | (Ast.Div, Bool i1, Bool i2)   -> Int   (int_of_bool i1   /  int_of_bool i2)
      | (Ast.Div, Bool i1, Int i2)    -> Int   (int_of_bool i1   /  i2)
      | (Ast.Div, Bool i1, Float i2)  -> Float (float_of_bool i1 /. i2)
      | (Ast.Div, Int i1, Bool i2)    -> Int   (i1               /  int_of_bool i2)
      | (Ast.Div, Int i1, Int i2)     -> Int   (i1               /  i2)
      | (Ast.Div, Int i1, Float i2)   -> Float (float_of_int i1  /. i2)
      | (Ast.Div, Float i1, Bool i2)  -> Float (i1               /. float_of_bool i2)
      | (Ast.Div, Float i1, Int i2)   -> Float (i1               /. float_of_int i2)
      | (Ast.Div, Float i1, Float i2) -> Float (i1               /. i2)

      | (Ast.And, Bool i1, Bool i2)   -> Bool  (i1 && i2)
      | (Ast.Or, Bool i1, Bool i2)    -> Bool  (i1 || i2)

      | _ -> assert false (* should not happen by virtue of typechecking *)
    end
  | Unop (op, e1) ->
    let new_e1 = evaluate_expr ctx p e1 in
    begin match (op, new_e1) with
      | (Ast.Not, Bool b1) -> (Bool (not b1))
      | (Ast.Minus, Int i1) -> (Int (- i1))
      | (Ast.Minus, Float f1) -> (Float (-. f1))
      | (Ast.Minus, Bool false) -> Bool false
      | (Ast.Minus, Bool true) -> Int (-1)

      | _ -> assert false (* should not happen by virtue of typechecking *)
    end
  | Conditional (e1, e2, e3) ->
    let new_e1 = evaluate_expr ctx p e1 in
    begin match new_e1 with
      | Bool true -> evaluate_expr ctx p e2
      | Bool false -> evaluate_expr ctx p e3
      | _ -> assert false (* should not happen by virtue of typechecking *)
    end
  | Literal l -> l
  | Index (var, e1) ->
    let new_e1 = evaluate_expr ctx p e1 in
    begin try match VariableMap.find (Ast.unmark var) ctx.ctx_vars with
      | SimpleVar _ -> assert false (* should not happen *)
      | TableVar (size, values) ->
        let idx = match new_e1 with
          | Bool b -> int_of_bool b
          | Int i -> i
          | Float _ ->
            raise (Errors.RuntimeError (
                Errors.FloatIndex (
                  Printf.sprintf "%s" (Format_ast.format_position (Ast.get_position e1))
                )
              ))
        in
        if idx >= size || idx < 0 then
          raise (Errors.RuntimeError (
              Errors.IndexOutOfBounds (
                Printf.sprintf "%s, index value of %d for table %s of size %d"
                  (Format_ast.format_position (Ast.get_position e1))
                  idx
                  (Ast.unmark (Ast.unmark var).Mvg.Variable.name)
                  size
              )
            ))
        else
          Array.get values idx
      with
      | Not_found -> assert false (* should not happen *)
    end
  | LocalVar lvar -> begin try Ast.unmark (LocalVariableMap.find lvar ctx.ctx_local_vars) with
      | Not_found -> assert false (* should not happen*)
    end
  | Var var -> begin try match VariableMap.find var ctx.ctx_vars with
      | SimpleVar l -> l (* should not happen *)
      | TableVar _ -> assert false
      with
      | Not_found -> assert false (* should not happen *)
    end
  | GenericTableIndex -> begin match ctx.ctx_generic_index with
      | None -> assert false (* should not happen *)
      | Some i -> Int i
    end
  | Error -> raise (Errors.RuntimeError (
      Errors.ErrorValue (Format_ast.format_position (Ast.get_position e))
    ))
  | LocalLet (lvar, e1, e2) ->
    let new_e1 = evaluate_expr ctx p e1 in
    let new_e2 =
      evaluate_expr
        { ctx with
          ctx_local_vars =
            LocalVariableMap.add lvar (Ast.same_pos_as new_e1 e1) ctx.ctx_local_vars
        }
        p e2
    in
    new_e2
  | FunctionCall (ArrFunc, [arg]) ->
    let new_arg = evaluate_expr ctx p arg in
    begin match new_arg with
      | Int x -> Int x
      | Float x ->
        Int (int_of_float (roundf x))
      | Bool x -> Int (int_of_bool x)
    end
  | FunctionCall (InfFunc, [arg]) ->
    let new_arg = evaluate_expr ctx p arg in
    begin match new_arg with
      | Int x -> Int x
      | Float x ->
        Int (int_of_float (truncatef x))
      | Bool x -> Int (int_of_bool x)
    end
  | FunctionCall (_, _) ->
    raise (Errors.RuntimeError (Errors.ErrorValue (Printf.sprintf "the function %s has not been expanded" (Format_ast.format_position (Ast.get_position e)))))

let evaluate_program (p: program) (dep_graph: Dependency.DepGraph.t) (input_values: literal VariableMap.t) : var_literal VariableMap.t =
  let ctx = Dependency.TopologicalOrder.fold (fun var ctx ->
      try
        match (VariableMap.find var p.program_vars).var_definition with
        | Mvg.SimpleVar e ->
          let l_e = evaluate_expr ctx p e in
          { ctx with ctx_vars = VariableMap.add var (SimpleVar l_e) ctx.ctx_vars }
        | Mvg.TableVar (size, es) ->
        (*
          Right now we suppose that the different indexes of table arrays don't depend on each other
          for computing. Otherwise, it would trigger a runtime Not_found error at interpretation.
          TODO: add a check for that at typechecking.
        *)
          { ctx with
            ctx_vars =
              VariableMap.add
                var
                (TableVar (
                    size,
                    Array.init size
                      (fun idx -> match es with
                         | IndexGeneric e ->
                           evaluate_expr { ctx with ctx_generic_index = Some idx } p e
                         | IndexTable es ->
                           evaluate_expr ctx p (IndexMap.find idx es)
                      )
                  )
                )
                ctx.ctx_vars
          }
        | Mvg.InputVar -> begin try
              { ctx with ctx_vars = VariableMap.add var (SimpleVar (VariableMap.find var input_values)) ctx.ctx_vars }
            with
            | Not_found -> raise (
                Errors.RuntimeError (
                  Errors.MissingInputValue (
                    Printf.sprintf "%s (%s)"
                      (Ast.unmark var.Mvg.Variable.name)
                      (Ast.unmark var.Mvg.Variable.descr)
                  )
                )
              )
          end
      with
      | Not_found ->
        let cond = VariableMap.find var p.program_conds in
        let l_cond = evaluate_expr ctx p cond.cond_expr in
        match l_cond with
        | Bool false -> ctx (* error condition is not trigerred, we continue *)
        | Bool true -> (* the condition is triggered, we throw errors *)
          raise (Errors.RuntimeError (
              Errors.ConditionViolated (
                Printf.sprintf "%s. Errors thrown:\n%s\nViolated condition:\n%s\nValues of the relevant variables at this point:\n%s"
                  (Format_ast.format_position (Ast.get_position cond.cond_expr))
                  (String.concat "\n" (List.map (fun err ->
                       Printf.sprintf "Error %s [%s]" (Ast.unmark err.Error.name) (Ast.unmark err.Error.descr)
                     ) cond.cond_errors))
                  (Format_mvg.format_expression (Ast.unmark cond.cond_expr))
                  (String.concat "\n" (List.map (fun (var) ->
                       let l = VariableMap.find var ctx.ctx_vars in
                       format_var_literal_with_var var l
                     ) (
                       Dependency.DepGraph.pred dep_graph var
                     )))
              )
            ))
        | _ -> assert false (* should not happen *)
    ) dep_graph empty_ctx
  in ctx.ctx_vars
