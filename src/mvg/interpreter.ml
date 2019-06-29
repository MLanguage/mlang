(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by thge DGFiP.

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

type ctx = literal Ast.marked LocalVariableMap.t

let int_of_bool (b: bool) = if b then 1 else 0
let float_of_bool (b: bool) = if b then 1. else 0.

let is_zero (l: literal) : bool = match l with
  | Bool false | Int 0 | Float 0. -> true
  | _ -> false

let rec evaluate (ctx: ctx) (p: program) (e: expression Ast.marked) : literal =
  match Ast.unmark e with
  | Comparison (op, e1, e2) ->
    let new_e1 = evaluate ctx p e1 in
    let new_e2 = evaluate ctx p e2 in
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
    let new_e1 = evaluate ctx p e1 in
    let new_e2 = evaluate ctx p e2 in
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

      | (Ast.Div, Bool false, l2) -> Bool false
      | (Ast.Div, Int 0, l2) -> Int 0
      | (Ast.Div, Float 0., l2) -> Float 0.
      | (Ast.Div, l1, l2) when is_zero l2 ->
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
    let new_e1 = evaluate ctx p e1 in
    begin match (op, new_e1) with
      | (Ast.Not, Bool b1) -> (Bool (not b1))
      | (Ast.Minus, Int i1) -> (Int (- i1))
      | (Ast.Minus, Float f1) -> (Float (-. f1))
      | (Ast.Minus, Bool false) -> Bool false
      | (Ast.Minus, Bool true) -> Int (-1)

      | _ -> assert false (* should not happen by virtue of typechecking *)
    end
  | Conditional (e1, e2, e3) ->
    let new_e1 = evaluate ctx p e1 in
    begin match new_e1 with
      | Bool true -> evaluate ctx p e2
      | Bool false -> evaluate ctx p e3
      | _ -> assert false (* should not happen by virtue of typechecking *)
    end
  | Literal l -> l
  | Index (var, e1) ->
    let new_e1 = evaluate ctx p e1 in
    raise (Errors.Unimplemented ("", Ast.get_position e))
  | LocalVar lvar -> begin try Ast.unmark (LocalVariableMap.find lvar ctx) with
      | Not_found -> assert false (* should not happen*)
    end
  | Var _ -> raise (Errors.Unimplemented ("", Ast.get_position e))
  | GenericTableIndex -> raise (Errors.Unimplemented ("", Ast.get_position e))
  | Error -> raise (Errors.Unimplemented ("", Ast.get_position e))
  | LocalLet (lvar, e1, e2) ->
    let new_e1 = evaluate ctx p e1 in
    let new_e2 = evaluate (LocalVariableMap.add lvar (Ast.same_pos_as new_e1 e1) ctx) p e2 in
    new_e2
  | FunctionCall (ArrFunc, [arg]) ->
    let new_arg = evaluate ctx p arg in
    begin match new_arg with
      | Int x -> Int x
      | Float x ->
        Int (int_of_float (roundf x))
      | Bool x -> Int (int_of_bool x)
    end
  | FunctionCall (InfFunc, [arg]) ->
    let new_arg = evaluate ctx p arg in
    begin match new_arg with
      | Int x -> Int x
      | Float x ->
        Int (int_of_float (truncatef x))
      | Bool x -> Int (int_of_bool x)
    end
  | FunctionCall (func, args) ->
    raise (Errors.RuntimeError (Errors.ErrorValue (Printf.sprintf "the function %s has not been expanded" (Format_ast.format_position (Ast.get_position e)))))
