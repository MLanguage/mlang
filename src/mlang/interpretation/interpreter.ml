(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Mvg

let repl_debug = ref false

let exit_on_rte = ref true

let truncatef x = snd (modf x)

(* Careful : rounding in M is done by excess and not by default (sic). Which is why we add a tiny
   bit after 0.5 *)
let roundf x = snd (modf (x +. copysign 0.5000001 x))

type var_literal = SimpleVar of literal | TableVar of int * literal array

let format_var_literal_with_var fmt ((var, vl) : Variable.t * var_literal) =
  match vl with
  | SimpleVar value ->
      Format.fprintf fmt "%s (%s): %a" (Pos.unmark var.Variable.name)
        (Pos.unmark var.Variable.descr) Format_mvg.format_literal value
  | TableVar (size, values) ->
      Format.fprintf fmt "%s (%s): Table (%d values)@\n" (Pos.unmark var.Variable.name)
        (Pos.unmark var.Variable.descr) size;
      List.iteri
        (fun idx value -> Format.fprintf fmt "| %d -> %a\n" idx Format_mvg.format_literal value)
        (Array.to_list values)

type ctx = {
  ctx_local_vars : literal Pos.marked LocalVariableMap.t;
  ctx_typing : Typechecker.typ_info;
  ctx_vars : var_literal VariableMap.t;
  ctx_generic_index : int option;
  ctx_current_scc_values : var_literal VariableMap.t;
}

let empty_ctx (p : program) (typing : Typechecker.typ_info) : ctx =
  {
    ctx_local_vars = LocalVariableMap.empty;
    ctx_typing = typing;
    ctx_vars =
      VariableMap.map
        (fun def ->
          match def.var_definition with
          | Mvg.SimpleVar _ | InputVar -> SimpleVar Undefined
          | Mvg.TableVar (size, _) -> TableVar (size, Array.make size Undefined))
        p.program_vars;
    ctx_generic_index = None;
    ctx_current_scc_values = VariableMap.empty;
  }

let int_of_bool (b : bool) = if b then 1 else 0

let float_of_bool (b : bool) = if b then 1. else 0.

let is_zero (l : literal) : bool = match l with Bool false | Float 0. -> true | _ -> false

let repl_debugguer (ctx : ctx) (p : Mvg.program) : unit =
  Cli.warning_print
    "Starting interactive debugger. Please query the interpreter state for the values of \
     variables. Exit with \"quit\".@\n";
  let exit = ref false in
  while not !exit do
    Format.printf "> @?";
    let query = read_line () in
    if query = "quit" then exit := true
    else if query = "explain" then begin
      Format.printf ">> @?";
      let query = read_line () in
      try
        let vars = Pos.VarNameToID.find query p.Mvg.program_idmap in
        let vars =
          List.sort
            (fun var1 var2 ->
              compare_execution_number var1.Variable.execution_number var2.Variable.execution_number)
            vars
        in
        List.iter
          (fun var ->
            Format.printf "[%a %a] -> %a@\n" Format_mvg.format_execution_number_short
              var.Variable.execution_number Pos.format_position var.Variable.execution_number.pos
              (fun fmt () ->
                try
                  Format_mvg.format_variable_def fmt
                    (VariableMap.find var p.program_vars).Mvg.var_definition
                with Not_found -> Format.fprintf fmt "unused definition")
              ())
          vars
      with Not_found -> Format.printf "Inexisting variable@\n"
    end
    else
      try
        let vars = Pos.VarNameToID.find query p.Mvg.program_idmap in
        let vars =
          List.sort
            (fun var1 var2 ->
              compare_execution_number var1.Variable.execution_number var2.Variable.execution_number)
            vars
        in
        List.iter
          (fun var ->
            try
              let var_l = Mvg.VariableMap.find var ctx.ctx_vars in
              Format.printf "[%a %a] -> %a@\n" Format_mvg.format_execution_number_short
                var.Variable.execution_number Pos.format_position var.Variable.execution_number.pos
                format_var_literal_with_var (var, var_l)
            with Not_found ->
              Format.printf "[%a %a] -> not computed@\n" Format_mvg.format_execution_number_short
                var.Variable.execution_number Pos.format_position var.Variable.execution_number.pos)
          vars
      with Not_found -> Format.printf "Inexisting variable@\n"
  done

type run_error =
  | ErrorValue of string
  | UndefinedValue of string
  | FloatIndex of string
  | IndexOutOfBounds of string
  | MissingInputValue of string
  | ConditionViolated of Error.t list * expression Pos.marked * (Variable.t * var_literal) list

exception RuntimeError of run_error * ctx

let format_runtime_error fmt (e : run_error) =
  match e with
  | UndefinedValue s -> Format.fprintf fmt "Undefined value at runtime: %s" s
  | ErrorValue s -> Format.fprintf fmt "Error value at runtime: %s" s
  | FloatIndex s -> Format.fprintf fmt "Index is not an integer: %s" s
  | IndexOutOfBounds s -> Format.fprintf fmt "Index out of bounds: %s" s
  | MissingInputValue s -> Format.fprintf fmt "Missing input value: %s" s
  | ConditionViolated (errors, condition, bindings) ->
      Format.fprintf fmt
        "Verification condition failed: %a. Errors thrown:\n\
         %a\n\
         Violated condition:\n\
         %a\n\
         Values of the relevant variables at this point:\n\
         %a@\n"
        Pos.format_position (Pos.get_position condition)
        (Format_ast.pp_print_list_endline (fun fmt err ->
             Format.fprintf fmt "Error %s [%s]" (Pos.unmark err.Error.name)
               (Pos.unmark err.Error.descr)))
        errors Format_mvg.format_expression (Pos.unmark condition)
        (Format_ast.pp_print_list_endline format_var_literal_with_var)
        bindings

let evaluate_array_index (ctx : ctx) (index : literal) (size : int) (values : literal array)
    (pos : Pos.position) : literal =
  let idx =
    match index with
    | Bool b -> int_of_bool b
    | Undefined -> assert false (* should not happen *)
    | Float f ->
        if
          let fraction, _ = modf f in
          fraction = 0.
        then int_of_float f
        else raise (RuntimeError (FloatIndex (Format.asprintf "%a" Pos.format_position pos), ctx))
  in
  if idx >= size then Undefined else if idx < 0 then Float 0. else values.(idx)

let eval_debug = ref false

let rec evaluate_expr (ctx : ctx) (p : program) (e : expression Pos.marked) (t : typ) : literal =
  if !eval_debug then Cli.debug_print "evaluate_expr %a" Format_mvg.format_expression (Pos.unmark e);
  try
    match Pos.unmark e with
    | Comparison (op, e1, e2) ->
        let new_e1 = evaluate_expr ctx p e1 Real in
        let new_e2 = evaluate_expr ctx p e2 Real in
        let l =
          match (Pos.unmark op, new_e1, new_e2) with
          | Ast.Gt, Float i1, Bool i2 -> Bool (i1 > float_of_bool i2)
          | Ast.Gt, Float i1, Float i2 -> Bool (i1 > i2)
          | Ast.Gt, _, Undefined | Ast.Gt, Undefined, _ -> Undefined
          | Ast.Gte, Float i1, Bool i2 -> Bool (i1 >= float_of_bool i2)
          | Ast.Gte, Float i1, Float i2 -> Bool (i1 >= i2)
          | Ast.Gte, _, Undefined | Ast.Gte, Undefined, _ -> Undefined
          | Ast.Lt, Float i1, Bool i2 -> Bool (i1 < float_of_bool i2)
          | Ast.Lt, Float i1, Float i2 -> Bool (i1 < i2)
          | Ast.Lt, _, Undefined | Ast.Lt, Undefined, _ -> Undefined
          | Ast.Lte, Float i1, Bool i2 -> Bool (i1 <= float_of_bool i2)
          | Ast.Lte, Float i1, Float i2 -> Bool (i1 <= i2)
          | Ast.Lte, _, Undefined | Ast.Lte, Undefined, _ -> Undefined
          | Ast.Eq, Float i1, Bool i2 -> Bool (i1 = float_of_bool i2)
          | Ast.Eq, Float i1, Float i2 -> Bool (i1 = i2)
          | Ast.Eq, _, Undefined | Ast.Eq, Undefined, _ -> Undefined
          | Ast.Neq, Float i1, Bool i2 -> Bool (i1 <> float_of_bool i2)
          | Ast.Neq, Float i1, Float i2 -> Bool (i1 <> i2)
          | Ast.Neq, _, Undefined | Ast.Neq, Undefined, _ -> Undefined
          | _ -> assert false
          (* should not happen *)
        in
        evaluate_expr ctx p (Pos.same_pos_as (Literal l) e) t
    | Binop (op, e1, e2) ->
        let t_under = match Pos.unmark op with Ast.And | Ast.Or -> Boolean | _ -> Real in
        let new_e1 = evaluate_expr ctx p e1 t_under in
        let new_e2 = evaluate_expr ctx p e2 t_under in
        let l =
          match (Pos.unmark op, new_e1, new_e2) with
          | Ast.Add, Float i1, Float i2 -> Float (i1 +. i2)
          | Ast.Add, Float i1, Undefined -> Float (i1 +. 0.)
          | Ast.Add, Undefined, Float i2 -> Float (0. +. i2)
          | Ast.Add, Undefined, Undefined -> Undefined
          | Ast.Sub, Float i1, Float i2 -> Float (i1 -. i2)
          | Ast.Sub, Float i1, Undefined -> Float (i1 -. 0.)
          | Ast.Sub, Undefined, Float i2 -> Float (0. -. i2)
          | Ast.Sub, Undefined, Undefined -> Undefined
          | Ast.Mul, _, Undefined | Ast.Mul, Undefined, _ -> Undefined
          | Ast.Mul, Float i1, Float i2 -> Float (i1 *. i2)
          | Ast.Div, Undefined, _ | Ast.Div, _, Undefined -> Undefined (* yes... *)
          | Ast.Div, _, l2 when is_zero l2 -> Undefined
          | Ast.Div, Float i1, Float i2 -> Float (i1 /. i2)
          | Ast.And, Undefined, _
          | Ast.And, _, Undefined
          | Ast.Or, Undefined, _
          | Ast.Or, _, Undefined ->
              Undefined
          | Ast.And, Bool i1, Bool i2 -> Bool (i1 && i2)
          | Ast.Or, Bool i1, Bool i2 -> Bool (i1 || i2)
          | _ -> assert false
          (* should not happen *)
        in
        evaluate_expr ctx p (Pos.same_pos_as (Literal l) e) t
    | Unop (op, e1) ->
        let new_e1 = evaluate_expr ctx p e1 t in
        let l =
          match (op, new_e1) with
          | Ast.Not, Bool b1 -> Bool (not b1)
          | Ast.Minus, Float f1 -> Float (-.f1)
          | Ast.Not, Undefined -> Undefined
          | Ast.Minus, Undefined -> Float 0.
          | _ -> assert false
          (* should not happen *)
        in
        evaluate_expr ctx p (Pos.same_pos_as (Literal l) e) t
    | Conditional (e1, e2, e3) -> (
        let new_e1 = evaluate_expr ctx p e1 Boolean in
        match new_e1 with
        | Bool true -> evaluate_expr ctx p e2 t
        | Bool false -> evaluate_expr ctx p e3 t
        | Undefined -> Undefined
        | _ -> assert false
        (* should not happen *) )
    | Literal l -> (
        match (l, t) with
        | Float _, Real | Bool _, Boolean -> l
        | Bool b, Real ->
            (* the subtyping relation fits here! *)
            if b then Float 1. else Float 0.
        | Undefined, _ -> Undefined
        | _ ->
            Cli.error_print "Interpreting literal %a %a with type %a\n" Format_mvg.format_literal l
              Pos.format_position (Pos.get_position e) Format_mvg.format_typ t;
            assert false (* should not happen *) )
    | Index (var, e1) -> (
        let new_e1 = evaluate_expr ctx p e1 Real in
        if new_e1 = Undefined then Undefined
        else
          try
            (* First we look if the value used is inside the current SCC. If yes then we return the
               value for this pass *)
            match VariableMap.find (Pos.unmark var) ctx.ctx_current_scc_values with
            | SimpleVar _ -> assert false (* should not happen *)
            | TableVar (size, values) ->
                evaluate_array_index ctx new_e1 size values (Pos.get_position e1)
          with
          (* Else it is a value that has been computed before in the SCC graph *)
          | Not_found -> (
            match VariableMap.find (Pos.unmark var) ctx.ctx_vars with
            | SimpleVar _ -> assert false (* should not happen *)
            | TableVar (size, values) ->
                evaluate_array_index ctx new_e1 size values (Pos.get_position e1) ) )
    | LocalVar lvar -> (
        try Pos.unmark (LocalVariableMap.find lvar ctx.ctx_local_vars)
        with Not_found -> assert false (* should not happen*) )
    | Var var -> (
        try
          (* First we look if the value used is inside the current SCC. If yes then we return the
             value for this pass *)
          match VariableMap.find var ctx.ctx_current_scc_values with
          | SimpleVar l -> l
          | TableVar _ -> assert false
          (* should not happen *)
        with (* Else it is a value that has been computed before in the SCC graph *)
        | Not_found -> (
          try
            match VariableMap.find var ctx.ctx_vars with
            | SimpleVar l -> l
            | TableVar _ -> assert false
            (* should not happen *)
          with Not_found ->
            Cli.error_print "Var not found (should not happen): %s %a\n"
              (Pos.unmark var.Variable.name) Pos.format_position (Pos.get_position e);
            assert false ) )
    | GenericTableIndex -> (
        match ctx.ctx_generic_index with
        | None -> assert false (* should not happen *)
        | Some i -> Float (float_of_int i) )
    | Error ->
        raise
          (RuntimeError
             (ErrorValue (Format.asprintf "%a" Pos.format_position (Pos.get_position e)), ctx))
    | LocalLet (lvar, e1, e2) ->
        let new_e1 =
          evaluate_expr ctx p e1 (LocalVariableMap.find lvar ctx.ctx_typing.typ_info_local_var)
        in
        let new_e2 =
          evaluate_expr
            {
              ctx with
              ctx_local_vars =
                LocalVariableMap.add lvar (Pos.same_pos_as new_e1 e1) ctx.ctx_local_vars;
            }
            p e2 t
        in
        new_e2
    | FunctionCall (ArrFunc, [ arg ]) ->
        let new_arg = evaluate_expr ctx p arg Real in
        let l =
          match new_arg with
          | Float x -> Float (roundf x)
          | Bool x -> Float (float_of_bool x)
          | Undefined -> Float 0.
        in
        evaluate_expr ctx p (Pos.same_pos_as (Literal l) e) Real
    | FunctionCall (InfFunc, [ arg ]) ->
        let new_arg = evaluate_expr ctx p arg Real in
        let l =
          match new_arg with
          | Float x -> Float (truncatef x)
          | Bool x -> Float (float_of_bool x)
          | Undefined -> Float 0.
        in
        evaluate_expr ctx p (Pos.same_pos_as (Literal l) e) Real
    | FunctionCall (PresentFunc, [ arg ]) ->
        let l =
          match evaluate_expr ctx p arg Real with Undefined -> Bool false | _ -> Bool true
        in
        evaluate_expr ctx p (Pos.same_pos_as (Literal l) e) Real
    | FunctionCall (Multimax, [ arg1; arg2 ]) ->
        let up =
          match evaluate_expr ctx p arg1 Real with
          | Float f when float_of_int (int_of_float f) = f -> int_of_float f
          | e ->
              raise
                (RuntimeError
                   ( ErrorValue
                       (Format.asprintf "evaluation of %a should be an integer, not %a"
                          Format_mvg.format_expression (Pos.unmark arg1) Format_mvg.format_literal
                          e),
                     ctx ))
        in
        let var_arg2 =
          match Pos.unmark arg2 with Var v -> v | _ -> assert false
          (* todo: rte *)
        in
        let cast_to_int e =
          match e with
          | Float f when float_of_int (int_of_float f) = f -> Some (int_of_float f)
          | Undefined -> Some 0
          | _ -> assert false
        in
        let pos = Pos.get_position arg2 in
        let access_index i =
          cast_to_int
          @@ evaluate_expr ctx p
               (Index ((var_arg2, pos), (Literal (Float (float_of_int i)), pos)), pos)
               Real
        in
        let maxi = ref (access_index 0) in
        for i = 0 to up do
          maxi := max !maxi (access_index i)
        done;
        let l = match !maxi with None -> Undefined | Some f -> Float (float_of_int f) in
        (* let l = match !maxi with None -> Undefined | Some v -> Float (float_of_int v) in *)
        evaluate_expr ctx p (Pos.same_pos_as (Literal l) e) Real
    | FunctionCall (func, _) ->
        raise
          (RuntimeError
             ( ErrorValue
                 (Format.asprintf "the function %a %a has not been expanded" Format_mvg.format_func
                    func Pos.format_position (Pos.get_position e)),
               ctx ))
  with RuntimeError (e, ctx) ->
    if !exit_on_rte then begin
      Cli.error_print "%a" format_runtime_error e;
      flush_all ();
      flush_all ();
      if !repl_debug then repl_debugguer ctx p;
      exit 1
    end
    else raise (RuntimeError (e, ctx))

let rec repeati (init : 'a) (f : 'a -> 'a) (n : int) : 'a =
  if n = 0 then init else repeati (f init) f (n - 1)

let evaluate_program (p : program) (typing : Typechecker.typ_info)
    (input_values : literal VariableMap.t) (number_of_passes : int) : ctx * program =
  try
    let dep_graph = Dependency.create_dependency_graph p in
    let execution_order = Execution_order.get_execution_order p in
    let ctx, _ =
      (* We evaluate the multiple passes of the program *)
      List.fold_left
        (fun (ctx, p) pass ->
          Cli.debug_print "Starting computation pass with:";
          if VariableMap.cardinal pass.exec_pass_set_variables > 0 then
            VariableMap.iter
              (fun var l ->
                Cli.debug_print "%s = %a" (Pos.unmark var.Variable.name) Format_mvg.format_literal
                  (Pos.unmark l))
              pass.exec_pass_set_variables
          else Cli.debug_print "No additional variables set";
          (* We update p with the values set in the pass *)
          let p : program =
            {
              p with
              program_vars =
                VariableMap.fold
                  (fun var init p_vars ->
                    begin
                      match VariableMap.find_opt var typing.table_info_var with
                      | None ->
                          Errors.raise_typ_error Variable
                            "variable %s set in execution pass to value %a does not exist"
                            (Pos.unmark var.Variable.name) Format_mvg.format_literal
                            (Pos.unmark init)
                      | Some true ->
                          Errors.raise_typ_error Variable
                            "variable %s set in execution pass to value %a is a table, which is \
                             not supported"
                            (Pos.unmark var.Variable.name) Format_mvg.format_literal
                            (Pos.unmark init)
                      | Some false -> ()
                    end;
                    VariableMap.add var
                      {
                        (VariableMap.find var p_vars) with
                        var_definition =
                          SimpleVar (Pos.same_pos_as (Literal (Pos.unmark init)) init);
                      }
                      p_vars)
                  pass.exec_pass_set_variables p.program_vars;
            }
          in
          (* For one pass we traverse each SCC in execution order *)
          List.fold_left
            (fun ((ctx, p) : ctx * program) (scc : unit VariableMap.t) ->
              (* We have to update the current scc value *)
              let ctx =
                {
                  ctx with
                  ctx_current_scc_values =
                    VariableMap.mapi
                      (fun var _ ->
                        try VariableMap.find var ctx.ctx_vars
                        with Not_found -> (
                          try
                            match (VariableMap.find var p.program_vars).var_definition with
                            | Mvg.SimpleVar _ -> SimpleVar Undefined
                            | Mvg.TableVar (size, _) -> TableVar (size, Array.make size Undefined)
                            | InputVar -> (
                                match VariableMap.find_opt var input_values with
                                | Some e -> SimpleVar e
                                | None -> assert false (* should not happen *) )
                          with Not_found -> (
                            try
                              let _ = VariableMap.find var p.program_conds in
                              SimpleVar Undefined
                            with Not_found -> assert false (* should not happen *) ) ))
                      scc;
                }
              in
              (* Because variables can be defined circularly interpretation is repeated an arbitrary
                 number of times *)
              let ctx =
                repeati ctx
                  (fun (ctx : ctx) ->
                    let ctx =
                      VariableMap.fold
                        (fun var _ (ctx : ctx) ->
                          try
                            match (VariableMap.find var p.program_vars).var_definition with
                            | Mvg.SimpleVar e ->
                                let l_e = evaluate_expr ctx p e Real in
                                {
                                  ctx with
                                  ctx_vars = VariableMap.add var (SimpleVar l_e) ctx.ctx_vars;
                                }
                            | Mvg.TableVar (size, es) ->
                                (* Right now we suppose that the different indexes of table arrays
                                   don't depend on each other for computing. Otherwise, it would
                                   trigger a runtime Not_found error at interpretation. TODO: add a
                                   check for that at typechecking. *)
                                {
                                  ctx with
                                  ctx_vars =
                                    VariableMap.add var
                                      (TableVar
                                         ( size,
                                           Array.init size (fun idx ->
                                               match es with
                                               | IndexGeneric e ->
                                                   evaluate_expr
                                                     { ctx with ctx_generic_index = Some idx }
                                                     p e Real
                                               | IndexTable es ->
                                                   let e = IndexMap.find idx es in
                                                   evaluate_expr ctx p e Real) ))
                                      ctx.ctx_vars;
                                }
                            | Mvg.InputVar -> (
                                try
                                  let l =
                                    evaluate_expr ctx p
                                      (Pos.same_pos_as
                                         (Literal (VariableMap.find var input_values))
                                         var.Variable.name)
                                      Real
                                  in
                                  {
                                    ctx with
                                    ctx_vars = VariableMap.add var (SimpleVar l) ctx.ctx_vars;
                                  }
                                with Not_found ->
                                  raise
                                    (RuntimeError
                                       ( MissingInputValue
                                           (Format.asprintf "%s (%s)"
                                              (Pos.unmark var.Mvg.Variable.name)
                                              (Pos.unmark var.Mvg.Variable.descr)),
                                         ctx )) )
                          with Not_found -> (
                            let cond = VariableMap.find var p.program_conds in
                            let l_cond = evaluate_expr ctx p cond.cond_expr Boolean in
                            match l_cond with
                            | Bool false | Undefined ->
                                ctx (* error condition is not trigerred, we continue *)
                            | Bool true ->
                                (* the condition is triggered, we throw errors *)
                                raise
                                  (RuntimeError
                                     ( ConditionViolated
                                         ( cond.cond_errors,
                                           cond.cond_expr,
                                           List.rev
                                           @@ List.fold_left
                                                (fun acc var ->
                                                  (var, VariableMap.find var ctx.ctx_vars) :: acc)
                                                []
                                                (Dependency.DepGraph.pred dep_graph var) ),
                                       ctx ))
                            | _ -> assert false )
                          (* should not happen *))
                        scc ctx
                    in
                    (* After a pass we have to update the current SCC values to what has just been
                       computed *)
                    {
                      ctx with
                      ctx_current_scc_values =
                        VariableMap.mapi
                          (fun var _ ->
                            try VariableMap.find var ctx.ctx_vars
                            with Not_found -> SimpleVar Undefined
                            (* this is for verification conditions variables *))
                          ctx.ctx_current_scc_values;
                    })
                  (* For SCC of one variable no need to repeat multiple passes *)
                  (if VariableMap.cardinal scc = 1 then 1 else number_of_passes)
              in
              (ctx, p))
            (ctx, p) execution_order)
        (empty_ctx p typing, p)
        p.program_exec_passes
    in
    (ctx, p)
  with RuntimeError (e, ctx) ->
    if !exit_on_rte then begin
      Cli.error_print "%a@?" format_runtime_error e;
      flush_all ();
      flush_all ();
      if !repl_debug then repl_debugguer ctx p;
      exit 1
    end
    else raise (RuntimeError (e, ctx))
