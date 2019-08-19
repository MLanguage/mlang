(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

module Pos = Verifisc.Pos
open Mvg

let repl_debug = ref false

let truncatef x = snd (modf x)
let roundf x = snd (modf (x +. copysign 0.5 x))

type var_literal =
  | SimpleVar of literal
  | TableVar of int * literal array

let format_var_literal_with_var (var: Variable.t) (vl: var_literal) : string = match vl with
  | SimpleVar value ->
    Printf.sprintf "%s (%s): %s"
      (Pos.unmark var.Variable.name)
      (Pos.unmark var.Variable.descr)
      (Format_mvg.format_literal value)
  | TableVar (size, values) ->
    Printf.sprintf "%s (%s): Table (%d values)\n%s"
      (Pos.unmark var.Variable.name)
      (Pos.unmark var.Variable.descr)
      size
      (String.concat "\n"
         (List.mapi
            (fun idx value ->
               Printf.sprintf "| %d -> %s"
                 idx
                 (Format_mvg.format_literal value)
            ) (Array.to_list values)))

type ctx = {
  ctx_local_vars: literal Pos.marked LocalVariableMap.t;
  ctx_vars: var_literal VariableMap.t;
  ctx_generic_index: int option;
  ctx_current_scc_values: var_literal VariableMap.t;
}

let empty_ctx  : ctx = {
  ctx_local_vars = LocalVariableMap.empty;
  ctx_vars = VariableMap.empty;
  ctx_generic_index = None;
  ctx_current_scc_values = VariableMap.empty;
}

let int_of_bool (b: bool) = if b then 1 else 0
let float_of_bool (b: bool) = if b then 1. else 0.

let is_zero (l: literal) : bool = match l with
  | Bool false | Int 0 | Float 0. -> true
  | _ -> false

let repl_debugguer
    (ctx: ctx )
    (p: Mvg.program) : unit
  =
  Cli.warning_print ("Starting interactive debugger. Please query the interpreter state for the values of variables." ^
                     " Exit with \"quit\".");
  let exit = ref false in
  while not !exit do
    Printf.printf "> ";
    let query = read_line () in
    if query = "quit" then exit := true else
    if query = "explain" then begin
      Printf.printf ">> ";
      let query = read_line () in
      try
        let vars = Pos.VarNameToID.find query p.Mvg.program_idmap in
        Printf.printf "%s\n"
          (String.concat "\n"
             (List.map (fun var ->
                  Printf.sprintf "[%s] -> %s"
                    (Format_mvg.format_execution_number_short var.Variable.execution_number)
                    (try
                       Format_mvg.format_variable_def (VariableMap.find var p.program_vars).Mvg.var_definition
                     with
                     | Not_found -> "unused definition")
                ) vars))
      with
      | Not_found -> Printf.printf "Inexisting variable\n"
    end else try
        let vars = Pos.VarNameToID.find query p.Mvg.program_idmap in
        Printf.printf "%s\n"
          (String.concat "\n"
             (List.map (fun var ->
                  try begin
                    let var_l =  Mvg.VariableMap.find var ctx.ctx_vars  in
                    Printf.sprintf "[%s] -> %s "
                      (Format_mvg.format_execution_number_short var.Variable.execution_number)
                      (format_var_literal_with_var var var_l)
                  end with
                  | Not_found ->
                    Printf.sprintf "[%s] -> not computed"
                      (Format_mvg.format_execution_number_short var.Variable.execution_number)
                ) vars))

      with
      | Not_found -> Printf.printf "Inexisting variable\n"
  done

type run_error =
  | ErrorValue of string
  | UndefinedValue of string
  | FloatIndex of string
  | IndexOutOfBounds of string
  | MissingInputValue of string
  | ConditionViolated of string

exception RuntimeError of run_error * ctx

let format_runtime_error (e: run_error) : string = match e with
  | UndefinedValue s -> Printf.sprintf "Undefined value at runtime: %s" s
  | ErrorValue s -> Printf.sprintf "Error value at runtime: %s" s
  | FloatIndex s -> Printf.sprintf "Index is not an integer: %s" s
  | IndexOutOfBounds s -> Printf.sprintf "Index out of bounds: %s" s
  | MissingInputValue s -> Printf.sprintf "Missing input value: %s" s
  | ConditionViolated s -> Printf.sprintf "Verification condition failed: %s" s

let evaluate_array_index
    (ctx: ctx)
    (index: literal)
    (size: int)
    (values: literal array)
    (pos: Pos.position)
  : literal =
  let idx = match index with
    | Bool b -> int_of_bool b
    | Int i -> i
    | Undefined  -> assert false (* should not happen *)
    | Float f ->
      if let (fraction, _) = modf f in fraction = 0. then
        int_of_float f
      else
        raise (RuntimeError (
            FloatIndex (
              Printf.sprintf "%s" (Pos.format_position pos)
            ), ctx
          ))
  in
  if idx >= size || idx < 0 then
    Undefined
  else
    Array.get values idx

let eval_debug = ref false

let rec evaluate_expr (ctx: ctx) (p: program) (e: expression Pos.marked) : literal =
  if !eval_debug then Cli.debug_print (Printf.sprintf "evaluate_expr %s" (Format_mvg.format_expression @@ Pos.unmark e));
  try begin match Pos.unmark e with
    | Comparison (op, e1, e2) ->
      let new_e1 = evaluate_expr ctx p e1 in
      let new_e2 = evaluate_expr ctx p e2 in
      begin match (Pos.unmark op, new_e1, new_e2) with
        | (Ast.Gt, Bool i1, Bool i2) -> Bool(i1 > i2)
        | (Ast.Gt, Bool i1, Int i2) -> Bool(int_of_bool i1 > i2)
        | (Ast.Gt, Bool i1, Float i2) -> Bool(float_of_bool i1 > i2)
        | (Ast.Gt, Int i1, Bool i2) -> Bool(i1 > int_of_bool i2)
        | (Ast.Gt, Int i1, Int i2) -> Bool(i1 > i2)
        | (Ast.Gt, Int i1, Float i2) -> Bool(float_of_int i1 > i2)
        | (Ast.Gt, Float i1, Bool i2) -> Bool(i1 > float_of_bool i2)
        | (Ast.Gt, Float i1, Int i2) -> Bool(i1 > float_of_int i2)
        | (Ast.Gt, Float i1, Float i2) -> Bool(i1 > i2)
        | (Ast.Gt, _, Undefined)
        | (Ast.Gt, Undefined, _) -> Undefined


        | (Ast.Gte, Bool i1, Bool i2) -> Bool(i1 >= i2)
        | (Ast.Gte, Bool i1, Int i2) -> Bool(int_of_bool i1 >= i2)
        | (Ast.Gte, Bool i1, Float i2) -> Bool(float_of_bool i1 >= i2)
        | (Ast.Gte, Int i1, Bool i2) -> Bool(i1 >= int_of_bool i2)
        | (Ast.Gte, Int i1, Int i2) -> Bool(i1 >= i2)
        | (Ast.Gte, Int i1, Float i2) -> Bool(float_of_int i1 >= i2)
        | (Ast.Gte, Float i1, Bool i2) -> Bool(i1 >= float_of_bool i2)
        | (Ast.Gte, Float i1, Int i2) -> Bool(i1 >= float_of_int i2)
        | (Ast.Gte, Float i1, Float i2) -> Bool(i1 >= i2)
        | (Ast.Gte, _, Undefined)
        | (Ast.Gte, Undefined, _) -> Undefined

        | (Ast.Lt, Bool i1, Bool i2) -> Bool(i1 < i2)
        | (Ast.Lt, Bool i1, Int i2) -> Bool(int_of_bool i1 < i2)
        | (Ast.Lt, Bool i1, Float i2) -> Bool(float_of_bool i1 < i2)
        | (Ast.Lt, Int i1, Bool i2) -> Bool(i1 < int_of_bool i2)
        | (Ast.Lt, Int i1, Int i2) -> Bool(i1 < i2)
        | (Ast.Lt, Int i1, Float i2) -> Bool(float_of_int i1 < i2)
        | (Ast.Lt, Float i1, Bool i2) -> Bool(i1 < float_of_bool i2)
        | (Ast.Lt, Float i1, Int i2) -> Bool(i1 < float_of_int i2)
        | (Ast.Lt, Float i1, Float i2) -> Bool(i1 < i2)
        | (Ast.Lt, _, Undefined)
        | (Ast.Lt, Undefined, _) -> Undefined

        | (Ast.Lte, Bool i1, Bool i2) -> Bool(i1 <= i2)
        | (Ast.Lte, Bool i1, Int i2) -> Bool(int_of_bool i1 <= i2)
        | (Ast.Lte, Bool i1, Float i2) -> Bool(float_of_bool i1 <= i2)
        | (Ast.Lte, Int i1, Bool i2) -> Bool(i1 <= int_of_bool i2)
        | (Ast.Lte, Int i1, Int i2) -> Bool(i1 <= i2)
        | (Ast.Lte, Int i1, Float i2) -> Bool(float_of_int i1 <= i2)
        | (Ast.Lte, Float i1, Bool i2) -> Bool(i1 <= float_of_bool i2)
        | (Ast.Lte, Float i1, Int i2) -> Bool(i1 <= float_of_int i2)
        | (Ast.Lte, Float i1, Float i2) -> Bool(i1 <= i2)
        | (Ast.Lte, _, Undefined)
        | (Ast.Lte, Undefined, _) -> Undefined

        | (Ast.Eq, Bool i1, Bool i2) -> Bool(i1 = i2)
        | (Ast.Eq, Bool i1, Int i2) -> Bool(int_of_bool i1 = i2)
        | (Ast.Eq, Bool i1, Float i2) -> Bool(float_of_bool i1 = i2)
        | (Ast.Eq, Int i1, Bool i2) -> Bool(i1 = int_of_bool i2)
        | (Ast.Eq, Int i1, Int i2) -> Bool(i1 = i2)
        | (Ast.Eq, Int i1, Float i2) -> Bool(float_of_int i1 = i2)
        | (Ast.Eq, Float i1, Bool i2) -> Bool(i1 = float_of_bool i2)
        | (Ast.Eq, Float i1, Int i2) -> Bool(i1 = float_of_int i2)
        | (Ast.Eq, Float i1, Float i2) -> Bool(i1 = i2)
        | (Ast.Eq, _, Undefined)
        | (Ast.Eq, Undefined, _) -> Undefined

        | (Ast.Neq, Bool i1, Bool i2) -> Bool(i1 <> i2)
        | (Ast.Neq, Bool i1, Int i2) -> Bool(int_of_bool i1 <> i2)
        | (Ast.Neq, Bool i1, Float i2) -> Bool(float_of_bool i1 <> i2)
        | (Ast.Neq, Int i1, Bool i2) -> Bool(i1 <> int_of_bool i2)
        | (Ast.Neq, Int i1, Int i2) -> Bool(i1 <> i2)
        | (Ast.Neq, Int i1, Float i2) -> Bool(float_of_int i1 <> i2)
        | (Ast.Neq, Float i1, Bool i2) -> Bool(i1 <> float_of_bool i2)
        | (Ast.Neq, Float i1, Int i2) -> Bool(i1 <> float_of_int i2)
        | (Ast.Neq, Float i1, Float i2) -> Bool(i1 <> i2)
        | (Ast.Neq, _, Undefined)
        | (Ast.Neq, Undefined, _) -> Undefined
      end
    | Binop (op, e1, e2) ->
      let new_e1 = evaluate_expr ctx p e1 in
      let new_e2 = evaluate_expr ctx p e2 in
      begin match (Pos.unmark op, new_e1, new_e2) with
        | (Ast.Add, Bool i1, Bool i2)     -> Int   (int_of_bool i1   +  int_of_bool i2)
        | (Ast.Add, Bool i1, Int i2)      -> Int   (int_of_bool i1   +  i2)
        | (Ast.Add, Bool i1, Float i2)    -> Float (float_of_bool i1 +. i2)
        | (Ast.Add, Bool i1, Undefined)   -> Int   (int_of_bool i1   +  0)
        | (Ast.Add, Int i1, Bool i2)      -> Int   (i1               +  int_of_bool i2)
        | (Ast.Add, Int i1, Int i2)       -> Int   (i1               +  i2)
        | (Ast.Add, Int i1, Float i2)     -> Float (float_of_int i1  +. i2)
        | (Ast.Add, Int i1, Undefined)    -> Int   (i1               +  0)
        | (Ast.Add, Float i1, Bool i2)    -> Float (i1               +. float_of_bool i2)
        | (Ast.Add, Float i1, Int i2)     -> Float (i1               +. float_of_int i2)
        | (Ast.Add, Float i1, Float i2)   -> Float (i1               +. i2)
        | (Ast.Add, Float i1, Undefined)  -> Float (i1               +. 0.)
        | (Ast.Add, Undefined, Bool i2)   -> Int   (0                +  int_of_bool i2)
        | (Ast.Add, Undefined, Int i2)    -> Int   (0                +  i2)
        | (Ast.Add, Undefined, Float i2)  -> Float (0.               +. i2)
        | (Ast.Add, Undefined, Undefined) -> Undefined

        | (Ast.Sub, Bool i1, Bool i2)     -> Int   (int_of_bool i1   -  int_of_bool i2)
        | (Ast.Sub, Bool i1, Int i2)      -> Int   (int_of_bool i1   -  i2)
        | (Ast.Sub, Bool i1, Float i2)    -> Float (float_of_bool i1 -. i2)
        | (Ast.Sub, Bool i1, Undefined)   -> Int   (int_of_bool i1   -  0)
        | (Ast.Sub, Int i1, Bool i2)      -> Int   (i1               -  int_of_bool i2)
        | (Ast.Sub, Int i1, Int i2)       -> Int   (i1               -  i2)
        | (Ast.Sub, Int i1, Float i2)     -> Float (float_of_int i1  -. i2)
        | (Ast.Sub, Int i1, Undefined)    -> Int   (i1               -  0)
        | (Ast.Sub, Float i1, Bool i2)    -> Float (i1               -. float_of_bool i2)
        | (Ast.Sub, Float i1, Int i2)     -> Float (i1               -. float_of_int i2)
        | (Ast.Sub, Float i1, Float i2)   -> Float (i1               -. i2)
        | (Ast.Sub, Float i1, Undefined)  -> Float (i1               -. 0.)
        | (Ast.Sub, Undefined, Bool i2)   -> Int   (0                -  int_of_bool i2)
        | (Ast.Sub, Undefined, Int i2)    -> Int   (0                -  i2)
        | (Ast.Sub, Undefined, Float i2)  -> Float (0.               -. i2)
        | (Ast.Sub, Undefined, Undefined) -> Undefined

        | (Ast.Mul, Bool i1, Bool i2)     -> Int   (int_of_bool i1   *  int_of_bool i2)
        | (Ast.Mul, Bool i1, Int i2)      -> Int   (int_of_bool i1   *  i2)
        | (Ast.Mul, Bool i1, Float i2)    -> Float (float_of_bool i1 *. i2)
        | (Ast.Mul, Bool i1, Undefined)   -> Int   (int_of_bool i1   *  0)
        | (Ast.Mul, Int i1, Bool i2)      -> Int   (i1               *  int_of_bool i2)
        | (Ast.Mul, Int i1, Int i2)       -> Int   (i1               *  i2)
        | (Ast.Mul, Int i1, Float i2)     -> Float (float_of_int i1  *. i2)
        | (Ast.Mul, Int i1, Undefined)    -> Int   (i1               *  0)
        | (Ast.Mul, Float i1, Bool i2)    -> Float (i1               *. float_of_bool i2)
        | (Ast.Mul, Float i1, Int i2)     -> Float (i1               *. float_of_int i2)
        | (Ast.Mul, Float i1, Float i2)   -> Float (i1               *. i2)
        | (Ast.Mul, Float i1, Undefined)  -> Float (i1               *. 0.)
        | (Ast.Mul, Undefined, Bool i2)   -> Int   (0                *  int_of_bool i2)
        | (Ast.Mul, Undefined, Int i2)    -> Int   (0                *  i2)
        | (Ast.Mul, Undefined, Float i2)  -> Float (0.               *. i2)
        | (Ast.Mul, Undefined, Undefined) -> Undefined

        | (Ast.Div, Bool false, _) -> Bool false
        | (Ast.Div, Int 0, _) -> Int 0
        | (Ast.Div, Float 0., _) -> Float 0.
        | (Ast.Div, _, Undefined) -> Undefined (* yes... *)
        | (Ast.Div, Undefined, _) -> Int 0

        | (Ast.Div, _, l2) when is_zero l2  ->
          Cli.warning_print (Printf.sprintf "Division by 0: %s"
                               (Pos.format_position (Pos.get_position e))
                            );
          Undefined
        | (Ast.Div, Bool i1, Bool i2)     -> Int   (int_of_bool i1   /  int_of_bool i2)
        | (Ast.Div, Bool i1, Int i2)      -> Float (float_of_bool i1 /. float_of_int i2)
        | (Ast.Div, Bool i1, Float i2)    -> Float (float_of_bool i1 /. i2)
        | (Ast.Div, Int i1, Bool i2)      -> Int   (i1               /  int_of_bool i2)
        | (Ast.Div, Int i1, Int i2)       -> Float (float_of_int i1  /. float_of_int i2)
        | (Ast.Div, Int i1, Float i2)     -> Float (float_of_int i1  /. i2)
        | (Ast.Div, Float i1, Bool i2)    -> Float (i1               /. float_of_bool i2)
        | (Ast.Div, Float i1, Int i2)     -> Float (i1               /. float_of_int i2)
        | (Ast.Div, Float i1, Float i2)   -> Float (i1               /. i2)

        | (Ast.And, Undefined, _)
        | (Ast.And, _, Undefined)
        | (Ast.Or, Undefined, _)
        | (Ast.Or, _, Undefined) -> Undefined
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

        | (Ast.Not, Undefined) -> Undefined
        | (Ast.Minus, Undefined) -> Int 0

        | _ -> assert false (* should not happen by virtue of typechecking *)
      end
    | Conditional (e1, e2, e3) ->
      let new_e1 = evaluate_expr ctx p e1 in
      begin match new_e1 with
        | Bool true -> evaluate_expr ctx p e2
        | Bool false -> evaluate_expr ctx p e3
        | Undefined -> Undefined
        | _ -> assert false (* should not happen by virtue of typechecking *)
      end
    | Literal l -> l
    | Index (var, e1) ->
      let new_e1 = evaluate_expr ctx p e1 in
      if new_e1 = Undefined then Undefined else
        begin
          (* First we look if the value used is inside the current SCC. If yes then we return the value for this pass *)
          try match VariableMap.find (Pos.unmark var) ctx.ctx_current_scc_values with
            | SimpleVar _ -> assert false (* should not happen *)
            | TableVar (size, values) ->
              evaluate_array_index ctx new_e1 size values (Pos.get_position e1)
          with
          (* Else it is a value that has been computed before in the SCC graph *)
          | Not_found -> begin match VariableMap.find (Pos.unmark var) ctx.ctx_vars with
              | SimpleVar _ -> assert false (* should not happen *)
              | TableVar (size, values) ->
                evaluate_array_index ctx new_e1 size values (Pos.get_position e1)
            end
        end
    | LocalVar lvar -> begin try Pos.unmark (LocalVariableMap.find lvar ctx.ctx_local_vars) with
        | Not_found -> assert false (* should not happen*)
      end
    | Var var -> begin
        (* First we look if the value used is inside the current SCC. If yes then we return the value for this pass *)
        try match VariableMap.find var ctx.ctx_current_scc_values with
          | SimpleVar l -> l (* should not happen *)
          | TableVar _ -> assert false
        with
        (* Else it is a value that has been computed before in the SCC graph *)
        | Not_found ->
          try
            begin match VariableMap.find var ctx.ctx_vars with
              | SimpleVar l -> l (* should not happen *)
              | TableVar _ -> assert false
            end
          with Not_found ->
            assert false
      end
    | GenericTableIndex -> begin match ctx.ctx_generic_index with
        | None -> assert false (* should not happen *)
        | Some i -> Int i
      end
    | Error -> raise (RuntimeError (
        ErrorValue (Pos.format_position (Pos.get_position e)), ctx
      ))
    | LocalLet (lvar, e1, e2) ->
      let new_e1 = evaluate_expr ctx p e1 in
      let new_e2 =
        evaluate_expr
          { ctx with
            ctx_local_vars =
              LocalVariableMap.add lvar (Pos.same_pos_as new_e1 e1) ctx.ctx_local_vars
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
        | Undefined -> Int 0
      end

    | FunctionCall (InfFunc, [arg]) ->
      let new_arg = evaluate_expr ctx p arg in
      begin match new_arg with
        | Int x -> Int x
        | Float x ->
          Int (int_of_float (truncatef x))
        | Bool x -> Int (int_of_bool x)
        | Undefined -> Int 0
      end

    | FunctionCall (PresentFunc, [arg]) ->
      begin match evaluate_expr ctx p arg with
        | Undefined -> Bool false
        | _ -> Bool true
      end

    | FunctionCall (Multimax, [arg1; arg2]) ->
      let up = match evaluate_expr ctx p arg1 with
        | Int x -> x
        | Float f when float_of_int (int_of_float f) = f -> int_of_float f
        | e ->
          raise (RuntimeError (ErrorValue
                                 (Printf.sprintf
                                    "evaluation of %s should be an integer, not %s"
                                    (Format_mvg.format_expression @@ Pos.unmark arg1)
                                    (Format_mvg.format_literal e)
                                 ), ctx))
      in
      let var_arg2 = match Pos.unmark arg2 with
        | Var v -> v
        | _ -> assert false (* todo: rte *) in
      let cast_to_int e = match e with
        | Int x -> x
        | Float f when float_of_int (int_of_float f) = f -> int_of_float f
        | Undefined ->
          Cli.warning_print "cast from undefined to 0 in multimax computation";
          0
        | _ -> assert false in
      let pos = Pos.get_position arg2 in
      let access_index i = cast_to_int @@ evaluate_expr ctx p (Index ((var_arg2, pos), (Literal (Int i), pos)), pos) in
      let maxi = ref (access_index 0) in
      for i = 0 to up do
        maxi := max !maxi (access_index i)
      done;
      Int !maxi
    | FunctionCall (func, _) ->
      raise
        (RuntimeError
           (ErrorValue
              (Printf.sprintf
                 "the function %s %s has not been expanded"
                 (Format_mvg.format_func func)
                 (Pos.format_position (Pos.get_position e))),
            ctx
           ))
  end with
  | RuntimeError (e,ctx) -> begin
      Cli.error_print (format_runtime_error e);
      flush_all ();
      flush_all ();
      if !repl_debug then repl_debugguer ctx p ;
      exit 1
    end

let rec repeati (init: 'a) (f: 'a -> 'a) (n: int) : 'a =
  if n = 0 then init else repeati (f init) f (n-1)

let evaluate_program
    (p: program)
    (input_values: literal VariableMap.t)
    (number_of_passes : int)
  : ctx =
  try
    let dep_graph = Dependency.create_dependency_graph p in
    let execution_order = Execution_order.get_execution_order p in
    let ctx = List.fold_left (fun (ctx : ctx) (scc: unit VariableMap.t) ->
        (** We have to update the current scc value *)
        let ctx =
          { ctx with
            ctx_current_scc_values =
              VariableMap.mapi (fun var _ ->
                  try begin match (VariableMap.find var p.program_vars).var_definition with
                    | Mvg.SimpleVar _ -> SimpleVar Undefined
                    | Mvg.TableVar (size, _) -> TableVar (size, Array.make size Undefined)
                    | InputVar -> begin match VariableMap.find_opt var input_values with
                        | Some e -> SimpleVar e
                        | None ->
                          Cli.error_print @@ Pos.unmark @@ var.name;
                          assert false (* should not happen *)
                      end
                  end with
                  | Not_found ->
                    try
                      let _ = VariableMap.find var p.program_conds in
                      SimpleVar Undefined
                    with
                    | Not_found ->
                      Printf.printf "Variable not found: %s %s\nSame name: %s\n"
                        (Pos.unmark var.Mvg.Variable.name)
                        (Format_mvg.format_execution_number var.Mvg.Variable.execution_number)
                        (String.concat "," (List.map (fun var ->
                             Printf.sprintf "%s[%s]" (Pos.unmark var.Mvg.Variable.name) (Format_mvg.format_execution_number_short var.Mvg.Variable.execution_number)
                           ) (Pos.VarNameToID.find (Pos.unmark var.Mvg.Variable.name) p.Mvg.program_idmap)));
                      assert false
                ) scc
          }
        in
        (* Because variables can be defined circularly interpretation is repeated an arbitrary number of times *)
        repeati ctx (fun (ctx : ctx) ->
            let ctx = VariableMap.fold
                (fun var _ (ctx : ctx) ->
                   try
                     match (VariableMap.find var p.program_vars).var_definition with
                     | Mvg.SimpleVar e ->
                       let l_e = evaluate_expr ctx p e in
                       { ctx with ctx_vars = VariableMap.add var (SimpleVar l_e) ctx.ctx_vars };
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
                                        let e = (IndexMap.find idx es) in
                                        evaluate_expr ctx p e
                                   )
                               )
                             )
                             ctx.ctx_vars
                       }
                     | Mvg.InputVar -> begin try
                           let l =
                             evaluate_expr ctx p
                               (
                                 Pos.same_pos_as (Literal  (VariableMap.find var input_values))
                                   var.Variable.name
                               ) in
                           { ctx with ctx_vars = VariableMap.add var (SimpleVar l) ctx.ctx_vars }
                         with
                         | Not_found ->
                           raise (
                             RuntimeError (
                               MissingInputValue (
                                 Printf.sprintf "%s (%s)"
                                   (Pos.unmark var.Mvg.Variable.name)
                                   (Pos.unmark var.Mvg.Variable.descr)
                               ), ctx
                             )
                           )
                       end
                   with
                   | Not_found ->
                     let cond = VariableMap.find var p.program_conds in
                     Cli.debug_print  (Printf.sprintf "checking cond  %s" (Format_mvg.format_precondition cond));
                     let l_cond = evaluate_expr ctx p cond.cond_expr in
                     match l_cond with
                     | Bool false | Undefined -> ctx (* error condition is not trigerred, we continue *)
                     | Bool true -> (* the condition is triggered, we throw errors *)
                       raise (RuntimeError (
                           ConditionViolated (
                             Printf.sprintf "%s. Errors thrown:\n%s\nViolated condition:\n%s\nValues of the relevant variables at this point:\n%s"
                               (Pos.format_position (Pos.get_position cond.cond_expr))
                               (String.concat "\n" (List.map (fun err ->
                                    Printf.sprintf "Error %s [%s]" (Pos.unmark err.Error.name) (Pos.unmark err.Error.descr)
                                  ) cond.cond_errors))
                               (Format_mvg.format_expression (Pos.unmark cond.cond_expr))
                               (String.concat "\n" (List.map (fun (var) ->
                                    let l = VariableMap.find var ctx.ctx_vars in
                                    format_var_literal_with_var var l
                                  ) (
                                    Dependency.DepGraph.pred dep_graph var
                                  )))
                           ), ctx
                         ))
                     | _ -> assert false (* should not happen *)
                ) scc ctx
            in
            (** After a pass we have to update the current SCC values to what has just been computed *)
            { ctx with
              ctx_current_scc_values =
                VariableMap.mapi
                  (fun var _ -> try VariableMap.find var ctx.ctx_vars with
                     | Not_found -> SimpleVar Undefined (* this is for verification conditions variables *)
                  ) ctx.ctx_current_scc_values
            }
          )
          (** For SCC of one variable no need to repeat multiple passes *)
          (if VariableMap.cardinal scc = 1 then 1 else number_of_passes)
      ) empty_ctx execution_order
    in ctx
  with
  | RuntimeError (e,ctx) -> begin
      Cli.error_print (format_runtime_error e);
      flush_all ();
      flush_all ();
      if !repl_debug then repl_debugguer ctx p ;
      exit 1
    end
