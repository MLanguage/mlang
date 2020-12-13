(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Mir

type var_literal = SimpleVar of literal | TableVar of int * literal array

type code_location_segment = InsideBlock of int | ConditionalBranch of bool

let format_code_location_segment (fmt : Format.formatter) (s : code_location_segment) =
  match s with
  | InsideBlock i -> Format.fprintf fmt "#%d" i
  | ConditionalBranch b -> Format.fprintf fmt "?%b" b

type code_location = code_location_segment list

let format_code_location (fmt : Format.formatter) (l : code_location) =
  Format.pp_print_list
    ~pp_sep:(fun fmt _ -> Format.fprintf fmt "->")
    format_code_location_segment fmt l

let assign_hook : (Mir.Variable.t -> (unit -> var_literal) -> code_location -> unit) ref =
  ref (fun _var _lit _code_loc -> ())

let exit_on_rte = ref true

let repl_debug = ref false

module Make (N : Bir_number.NumberInterface) = struct
  (* Careful : this behavior mimics the one imposed by the original Mlang compiler... *)
  let truncatef (x : N.t) : N.t = N.floor N.(x +. N.of_float 0.000001)

  (* Careful : rounding in M is done with this arbitrary behavior. We can't use copysign here
     because [x < zero] is critical to have the correct behavior on -0 *)
  let roundf (x : N.t) =
    N.of_int (N.to_int N.(x +. N.of_float (if N.(x < zero ()) then -0.50005 else 0.50005)))

  type value = Number of N.t | Undefined

  let false_value () = Number (N.zero ())

  let true_value () = Number (N.one ())

  let format_value (fmt : Format.formatter) (x : value) =
    match x with
    | Undefined -> Format_mir.format_literal fmt Mir.Undefined
    | Number x -> N.format_t fmt x

  type var_value = SimpleVar of value | TableVar of int * value array

  let format_var_value (fmt : Format.formatter) (var_lit : var_value) : unit =
    match var_lit with
    | SimpleVar e -> Format.fprintf fmt "%a" format_value e
    | TableVar (_, es) ->
        Format.fprintf fmt "[%a]"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt e -> Format.fprintf fmt "%a" format_value e))
          (Array.to_list es)

  let format_var_value_with_var (fmt : Format.formatter) ((var, vl) : Variable.t * var_value) =
    match vl with
    | SimpleVar value ->
        Format.fprintf fmt "%s (%s): %a" (Pos.unmark var.Variable.name)
          (Pos.unmark var.Variable.descr) format_value value
    | TableVar (size, values) ->
        Format.fprintf fmt "%s (%s): Table (%d values)@\n" (Pos.unmark var.Variable.name)
          (Pos.unmark var.Variable.descr) size;
        List.iteri
          (fun idx value -> Format.fprintf fmt "| %d -> %a\n" idx format_value value)
          (Array.to_list values)

  type ctx = {
    ctx_local_vars : value Pos.marked LocalVariableMap.t;
    ctx_vars : var_value VariableMap.t;
    ctx_generic_index : int option;
  }

  let empty_ctx : ctx =
    {
      ctx_local_vars = LocalVariableMap.empty;
      ctx_vars = VariableMap.empty;
      ctx_generic_index = None;
    }

  let literal_to_value (l : Mir.literal) : value =
    match l with Mir.Undefined -> Undefined | Mir.Float f -> Number (N.of_float f)

  let var_literal_to_var_value (def : var_literal) : var_value =
    match def with
    | SimpleVar v -> SimpleVar (literal_to_value v)
    | TableVar (size, defs) -> TableVar (size, Array.map (fun v -> literal_to_value v) defs)

  let value_to_literal (l : value) : Mir.literal =
    match l with Undefined -> Mir.Undefined | Number f -> Mir.Float (N.to_float f)

  let var_value_to_var_literal (def : var_value) : var_literal =
    let l : var_literal =
      match def with
      | SimpleVar v -> SimpleVar (value_to_literal v)
      | TableVar (size, defs) -> TableVar (size, Array.map (fun v -> value_to_literal v) defs)
    in
    l

  let update_ctx_with_inputs (ctx : ctx) (inputs : literal VariableMap.t) : ctx =
    {
      ctx with
      ctx_vars =
        VariableMap.fold
          (fun var value ctx_vars -> VariableMap.add var (SimpleVar value) ctx_vars)
          (VariableMap.mapi
             (fun v l ->
               match l with
               | Mir.Undefined -> Undefined
               | Mir.Float f -> Number (N.of_float_input v f))
             inputs)
          ctx.ctx_vars;
    }

  type run_error =
    | ErrorValue of string * Pos.t
    | FloatIndex of string * Pos.t
    | IndexOutOfBounds of string * Pos.t
    | IncorrectOutputVariable of string * Pos.t
    | UnknownInputVariable of string * Pos.t
    | ConditionViolated of Error.t list * expression Pos.marked * (Variable.t * var_value) list
    | NanOrInf of string * expression Pos.marked
    | StructuredError of (string * (string option * Pos.t) list * (unit -> unit) option)

  exception RuntimeError of run_error * ctx

  (* During evaluation, variables that have an I/O property set to InputVariable have a value that
     is read directly from the input map. However, one can pass inside the input map a value for a
     variable whose I/O type was not properly set to InputVariable. This function is precisely for
     these cases, it set the I/O flag properly for execution. Not that such a change to the program
     does not require to recompute the dependency graph and the execution order. *)
  let replace_undefined_with_input_variables (p : program) (input_values : literal VariableMap.t) :
      program =
    VariableMap.fold
      (fun var _ p ->
        try
          let old_var_data = VariableMap.find var p.program_vars in
          {
            p with
            program_vars =
              VariableMap.add var
                { old_var_data with var_definition = InputVar; var_io = Input }
                p.program_vars;
          }
        with Not_found ->
          raise
            (RuntimeError
               ( UnknownInputVariable
                   ( Format.asprintf "%s (%s)"
                       (Pos.unmark var.Mir.Variable.name)
                       (Pos.unmark var.Mir.Variable.descr),
                     Pos.get_position var.Mir.Variable.name ),
                 empty_ctx )))
      input_values p

  let print_output (f : Bir_interface.bir_function) (results : ctx) : unit =
    Mir.VariableMap.iter
      (fun var value ->
        if Mir.VariableMap.mem var f.func_outputs then
          Cli.result_print "%a" format_var_value_with_var (var, value))
      results.ctx_vars

  let repl_debugguer (ctx : ctx) (p : Mir.program) : unit =
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
          let vars = Pos.VarNameToID.find query p.Mir.program_idmap in
          let vars =
            List.sort
              (fun var1 var2 ->
                compare_execution_number var1.Variable.execution_number
                  var2.Variable.execution_number)
              vars
          in
          List.iter
            (fun var ->
              Format.printf "[%a %a] -> %a@\n" Format_mir.format_execution_number_short
                var.Variable.execution_number Pos.format_position var.Variable.execution_number.pos
                (fun fmt () ->
                  try
                    Format_mir.format_variable_def fmt
                      (VariableMap.find var p.program_vars).Mir.var_definition
                  with Not_found -> Format.fprintf fmt "unused definition")
                ())
            vars
        with Not_found -> Format.printf "Inexisting variable@\n"
      end
      else
        try
          let vars = Pos.VarNameToID.find query p.Mir.program_idmap in
          let vars =
            List.sort
              (fun var1 var2 ->
                compare_execution_number var1.Variable.execution_number
                  var2.Variable.execution_number)
              vars
          in
          List.iter
            (fun var ->
              try
                let var_l = Mir.VariableMap.find var ctx.ctx_vars in
                Format.printf "[%a %a] -> %a@\n" Format_mir.format_execution_number_short
                  var.Variable.execution_number Pos.format_position
                  var.Variable.execution_number.pos format_var_value_with_var (var, var_l)
              with Not_found ->
                Format.printf "[%a %a] -> not computed@\n" Format_mir.format_execution_number_short
                  var.Variable.execution_number Pos.format_position
                  var.Variable.execution_number.pos)
            vars
        with Not_found -> Format.printf "Inexisting variable@\n"
    done

  let raise_runtime_as_structured (e : run_error) (ctx : ctx) (p : program) =
    match e with
    | ErrorValue (s, pos) ->
        Errors.raise_spanned_error (Format.asprintf "Error value at runtime: %s" s) pos
    | FloatIndex (s, pos) ->
        Errors.raise_spanned_error (Format.asprintf "Index is not an integer: %s" s) pos
    | IndexOutOfBounds (s, pos) ->
        Errors.raise_spanned_error (Format.asprintf "Index out of bounds: %s" s) pos
    | NanOrInf (v, e) ->
        Errors.raise_spanned_error_with_continuation
          (Format.asprintf "Expression evaluated to %s: %a" v Format_mir.format_expression
             (Pos.unmark e))
          (Pos.get_position e)
          (fun _ -> repl_debugguer ctx p)
    | UnknownInputVariable (s, pos) ->
        Errors.raise_spanned_error (Format.asprintf "Unknown input variable: %s" s) pos
    | IncorrectOutputVariable (s, pos) ->
        Errors.raise_spanned_error (Format.asprintf "Incorrect output variable: %s" s) pos
    | ConditionViolated (errors, condition, bindings) ->
        Errors.raise_spanned_error_with_continuation
          (Format.asprintf
             "Verification condition failed! Errors thrown:\n\
             \  * %a\n\
              Violated condition:\n\
             \  * %a\n\
              Values of the relevant variables at this point:\n\
              %a"
             (Format_mast.pp_print_list_endline (fun fmt err ->
                  Format.fprintf fmt "Error %s [%s]" (Pos.unmark err.Error.name)
                    (Pos.unmark err.Error.descr)))
             errors Format_mir.format_expression (Pos.unmark condition)
             (Format_mast.pp_print_list_endline (fun fmt v ->
                  Format.fprintf fmt "  * %a" format_var_value_with_var v))
             bindings)
          (Pos.get_position condition)
          (fun _ -> repl_debugguer ctx p)
    | StructuredError (msg, pos, kont) -> raise (Errors.StructuredError (msg, pos, kont))

  let int_of_bool (b : bool) = if b then 1 else 0

  let is_zero (l : value) : bool = match l with Number z -> N.is_zero z | _ -> false

  let real_of_bool (b : bool) = if b then N.one () else N.zero ()

  let bool_of_real (f : N.t) : bool = not N.(f =. zero ())

  let evaluate_array_index (index : value) (size : int) (values : value array) : value =
    let idx =
      match index with Undefined -> assert false (* should not happen *) | Number f -> roundf f
    in
    if N.(idx >=. N.of_int (Int64.of_int size)) then Undefined
    else if N.(idx <. N.zero ()) then Number (N.zero ())
    else values.(Int64.to_int (N.to_int idx))

  let eval_debug = ref false

  let rec evaluate_expr (ctx : ctx) (p : Mir.program) (e : expression Pos.marked) : value =
    let out =
      try
        match Pos.unmark e with
        | Comparison (op, e1, e2) -> (
            let new_e1 = evaluate_expr ctx p e1 in
            let new_e2 = evaluate_expr ctx p e2 in
            match (Pos.unmark op, new_e1, new_e2) with
            | Mast.Gt, Number i1, Number i2 -> Number N.(real_of_bool (i1 >. i2))
            | Mast.Gt, _, Undefined | Mast.Gt, Undefined, _ -> Undefined
            | Mast.Gte, Number i1, Number i2 -> Number N.(real_of_bool (i1 >=. i2))
            | Mast.Gte, _, Undefined | Mast.Gte, Undefined, _ -> Undefined
            | Mast.Lt, Number i1, Number i2 -> Number N.(real_of_bool (i1 <. i2))
            | Mast.Lt, _, Undefined | Mast.Lt, Undefined, _ -> Undefined
            | Mast.Lte, Number i1, Number i2 -> Number N.(real_of_bool (i1 <=. i2))
            | Mast.Lte, _, Undefined | Mast.Lte, Undefined, _ -> Undefined
            | Mast.Eq, Number i1, Number i2 -> Number N.(real_of_bool (i1 =. i2))
            | Mast.Eq, _, Undefined | Mast.Eq, Undefined, _ -> Undefined
            | Mast.Neq, Number i1, Number i2 -> Number N.(real_of_bool (not (i1 =. i2)))
            | Mast.Neq, _, Undefined | Mast.Neq, Undefined, _ -> Undefined )
        | Binop (op, e1, e2) -> (
            let new_e1 = evaluate_expr ctx p e1 in
            let new_e2 = evaluate_expr ctx p e2 in
            match (Pos.unmark op, new_e1, new_e2) with
            | Mast.Add, Number i1, Number i2 -> Number N.(i1 +. i2)
            | Mast.Add, Number i1, Undefined -> Number N.(i1 +. zero ())
            | Mast.Add, Undefined, Number i2 -> Number N.(zero () +. i2)
            | Mast.Add, Undefined, Undefined -> Undefined
            | Mast.Sub, Number i1, Number i2 -> Number N.(i1 -. i2)
            | Mast.Sub, Number i1, Undefined -> Number N.(i1 -. zero ())
            | Mast.Sub, Undefined, Number i2 -> Number N.(zero () -. i2)
            | Mast.Sub, Undefined, Undefined -> Undefined
            | Mast.Mul, _, Undefined | Mast.Mul, Undefined, _ -> Undefined
            | Mast.Mul, Number i1, Number i2 -> Number N.(i1 *. i2)
            | Mast.Div, Undefined, _ | Mast.Div, _, Undefined -> Undefined (* yes... *)
            | Mast.Div, _, l2 when is_zero l2 -> Number (N.zero ())
            | Mast.Div, Number i1, Number i2 -> Number N.(i1 /. i2)
            | Mast.And, Undefined, _
            | Mast.And, _, Undefined
            | Mast.Or, Undefined, _
            | Mast.Or, _, Undefined ->
                Undefined
            | Mast.And, Number i1, Number i2 ->
                Number (real_of_bool (bool_of_real i1 && bool_of_real i2))
            | Mast.Or, Number i1, Number i2 ->
                Number (real_of_bool (bool_of_real i1 || bool_of_real i2)) )
        | Unop (op, e1) -> (
            let new_e1 = evaluate_expr ctx p e1 in
            match (op, new_e1) with
            | Mast.Not, Number b1 -> Number (real_of_bool (not (bool_of_real b1)))
            | Mast.Minus, Number f1 -> Number N.(zero () -. f1)
            | Mast.Not, Undefined -> Undefined
            | Mast.Minus, Undefined -> Number (N.zero ()) )
        | Conditional (e1, e2, e3) -> (
            let new_e1 = evaluate_expr ctx p e1 in
            match new_e1 with
            | Number z when N.(z =. zero ()) -> evaluate_expr ctx p e3
            | Number _ -> evaluate_expr ctx p e2 (* the float is not zero *)
            | Undefined -> Undefined )
        | Literal Undefined -> Undefined
        | Literal (Float f) -> Number (N.of_float f)
        | Index (var, e1) -> (
            let new_e1 = evaluate_expr ctx p e1 in
            if new_e1 = Undefined then Undefined
            else
              match VariableMap.find (Pos.unmark var) ctx.ctx_vars with
              | SimpleVar _ -> assert false (* should not happen *)
              | TableVar (size, values) -> evaluate_array_index new_e1 size values )
        | LocalVar lvar -> (
            try Pos.unmark (LocalVariableMap.find lvar ctx.ctx_local_vars)
            with Not_found -> assert false (* should not happen*) )
        | Var var ->
            let r =
              try
                match VariableMap.find var ctx.ctx_vars with
                | SimpleVar l -> l
                | TableVar _ -> assert false
                (* should not happen *)
              with Not_found ->
                Errors.raise_spanned_error
                  ("Var not found (should not happen): " ^ Pos.unmark var.Variable.name)
                  (Pos.get_position e)
            in
            r
        | GenericTableIndex -> (
            match ctx.ctx_generic_index with
            | None -> assert false (* should not happen *)
            | Some i -> Number (N.of_int (Int64.of_int i)) )
        | Error ->
            raise
              (RuntimeError
                 ( ErrorValue
                     ( Format.asprintf "%a" Pos.format_position (Pos.get_position e),
                       Pos.get_position e ),
                   ctx ))
        | LocalLet (lvar, e1, e2) ->
            let new_e1 = evaluate_expr ctx p e1 in
            let new_e2 =
              evaluate_expr
                {
                  ctx with
                  ctx_local_vars =
                    LocalVariableMap.add lvar (Pos.same_pos_as new_e1 e1) ctx.ctx_local_vars;
                }
                p e2
            in
            new_e2
        | FunctionCall (ArrFunc, [ arg ]) -> (
            let new_arg = evaluate_expr ctx p arg in
            match new_arg with Number x -> Number (roundf x) | Undefined -> Undefined
            (*nope:Float 0.*) )
        | FunctionCall (InfFunc, [ arg ]) -> (
            let new_arg = evaluate_expr ctx p arg in
            match new_arg with Number x -> Number (truncatef x) | Undefined -> Undefined
            (*Float 0.*) )
        | FunctionCall (PresentFunc, [ arg ]) -> (
            match evaluate_expr ctx p arg with Undefined -> false_value () | _ -> true_value () )
        | FunctionCall (MinFunc, [ arg1; arg2 ]) -> (
            match (evaluate_expr ctx p arg1, evaluate_expr ctx p arg2) with
            | Undefined, Number f | Number f, Undefined -> Number (N.min (N.zero ()) f)
            | Undefined, Undefined -> Number (N.zero ())
            | Number fl, Number fr -> Number (N.min fl fr) )
        | FunctionCall (MaxFunc, [ arg1; arg2 ]) -> (
            match (evaluate_expr ctx p arg1, evaluate_expr ctx p arg2) with
            | Undefined, Undefined -> Number (N.zero ())
            | Undefined, Number f | Number f, Undefined -> Number (N.max (N.zero ()) f)
            | Number fl, Number fr -> Number (N.max fl fr) )
        | FunctionCall (Multimax, [ arg1; arg2 ]) -> (
            let up =
              match evaluate_expr ctx p arg1 with
              | Number f -> N.to_int (roundf f)
              | e ->
                  raise
                    (RuntimeError
                       ( ErrorValue
                           ( Format.asprintf "evaluation of %a should be an integer, not %a"
                               Format_mir.format_expression (Pos.unmark arg1) format_value e,
                             Pos.get_position arg1 ),
                         ctx ))
            in
            let var_arg2 =
              match Pos.unmark arg2 with Var v -> v | _ -> assert false
              (* todo: rte *)
            in
            let cast_to_int (v : value) : Int64.t option =
              match v with Number f -> Some (N.to_int (roundf f)) | Undefined -> Some Int64.zero
            in
            let pos = Pos.get_position arg2 in
            let access_index (i : int) : Int64.t option =
              cast_to_int
              @@ evaluate_expr ctx p
                   (Index ((var_arg2, pos), (Literal (Float (float_of_int i)), pos)), pos)
            in
            let maxi = ref (access_index 0) in
            for i = 0 to Int64.to_int up do
              maxi := max !maxi (access_index i)
            done;
            match !maxi with None -> Undefined | Some f -> Number (N.of_int f) )
        | FunctionCall (func, _) ->
            raise
              (RuntimeError
                 ( ErrorValue
                     ( Format.asprintf "the function %a  has not been expanded"
                         Format_mir.format_func func,
                       Pos.get_position e ),
                   ctx ))
      with
      | RuntimeError (e, ctx) ->
          if !exit_on_rte then raise_runtime_as_structured e ctx p
          else raise (RuntimeError (e, ctx))
      | Errors.StructuredError (msg, pos, kont) ->
          if !exit_on_rte then
            raise
              (Errors.StructuredError
                 (msg, pos @ [ (Some "Expression raising the error:", Pos.get_position e) ], kont))
          else raise (RuntimeError (StructuredError (msg, pos, kont), ctx))
    in
    if match out with Undefined -> false | Number out -> N.is_nan_or_inf out then
      let e =
        NanOrInf
          ( ( match out with
            | Undefined -> assert false
            | Number out -> Format.asprintf "%a" N.format_t out ),
            e )
      in
      if !exit_on_rte then raise_runtime_as_structured e ctx p else raise (RuntimeError (e, ctx))
    else out

  let report_violatedcondition (cond : condition_data) (ctx : ctx) : 'a =
    raise
      (RuntimeError
         ( ConditionViolated
             ( cond.cond_errors,
               cond.cond_expr,
               List.rev
               @@ List.fold_left
                    (fun acc var -> (var, VariableMap.find var ctx.ctx_vars) :: acc)
                    []
                    (List.map
                       (fun (x, _) -> x)
                       (Mir.VariableMap.bindings
                          (Mir_dependency_graph.get_used_variables cond.cond_expr))) ),
           ctx ))

  let evaluate_variable (p : Bir.program) (ctx : ctx) (vdef : variable_def) : var_value =
    match vdef with
    | Mir.SimpleVar e -> SimpleVar (evaluate_expr ctx p.mir_program e)
    | Mir.TableVar (size, es) ->
        TableVar
          ( size,
            Array.init size (fun idx ->
                match es with
                | IndexGeneric e ->
                    evaluate_expr { ctx with ctx_generic_index = Some idx } p.mir_program e
                | IndexTable es ->
                    let e = IndexMap.find idx es in
                    evaluate_expr ctx p.mir_program e) )
    | Mir.InputVar -> assert false

  let rec evaluate_stmt (p : Bir.program) (ctx : ctx) (stmt : Bir.stmt) (loc : code_location) =
    match Pos.unmark stmt with
    | Bir.SAssign (var, vdata) ->
        let res = evaluate_variable p ctx vdata.var_definition in
        !assign_hook var (fun _ -> var_value_to_var_literal res) loc;
        { ctx with ctx_vars = VariableMap.add var res ctx.ctx_vars }
    | Bir.SConditional (b, t, f) -> (
        match evaluate_variable p ctx (SimpleVar (b, Pos.no_pos)) with
        | SimpleVar (Number z) when N.(z =. zero ()) ->
            evaluate_stmts p ctx f (ConditionalBranch false :: loc) 0
        | SimpleVar (Number _) -> evaluate_stmts p ctx t (ConditionalBranch true :: loc) 0
        | SimpleVar Undefined -> ctx
        | _ -> assert false )
    | Bir.SVerif data -> (
        match evaluate_expr ctx p.mir_program data.cond_expr with
        | Number f when not (N.is_zero f) -> report_violatedcondition data ctx
        | _ -> ctx )

  and evaluate_stmts (p : Bir.program) (ctx : ctx) (stmts : Bir.stmt list) (loc : code_location)
      (start_value : int) : ctx =
    let ctx, _ =
      List.fold_left
        (fun (ctx, i) stmt -> (evaluate_stmt p ctx stmt (InsideBlock i :: loc), i + 1))
        (ctx, start_value) stmts
    in
    ctx

  let evaluate_program (p : Bir.program) (ctx : ctx) (code_loc_start_value : int) : ctx =
    try
      let ctx = evaluate_stmts p ctx p.statements [] code_loc_start_value in
      ctx
    with RuntimeError (e, ctx) ->
      if !exit_on_rte then raise_runtime_as_structured e ctx p.mir_program
      else raise (RuntimeError (e, ctx))
end

module RegularFloatInterpreter = Make (Bir_number.RegularFloatNumber)
module MPFRInterpreter = Make (Bir_number.MPFRNumber)

module BigIntPrecision = struct
  let scaling_factor_bits = ref 64
end

module BigIntInterpreter = Make (Bir_number.BigIntFixedPointNumber (BigIntPrecision))
module IntervalInterpreter = Make (Bir_number.IntervalNumber)
module RationalInterpreter = Make (Bir_number.RationalNumber)

type value_sort =
  | RegularFloat
  | MPFR of int  (** bitsize of the floats *)
  | BigInt of int  (** precision of the fixed point *)
  | Interval
  | Rational

let evaluate_program (bir_func : Bir_interface.bir_function) (p : Bir.program)
    (inputs : literal VariableMap.t) (code_loc_start_value : int) (sort : value_sort) : unit -> unit
    =
  match sort with
  | RegularFloat ->
      let ctx =
        RegularFloatInterpreter.update_ctx_with_inputs RegularFloatInterpreter.empty_ctx inputs
      in
      let ctx = RegularFloatInterpreter.evaluate_program p ctx code_loc_start_value in
      fun () -> RegularFloatInterpreter.print_output bir_func ctx
  | MPFR prec ->
      Mpfr.set_default_prec prec;
      let ctx = MPFRInterpreter.update_ctx_with_inputs MPFRInterpreter.empty_ctx inputs in
      let ctx = MPFRInterpreter.evaluate_program p ctx code_loc_start_value in
      fun () -> MPFRInterpreter.print_output bir_func ctx
  | BigInt prec ->
      BigIntPrecision.scaling_factor_bits := prec;
      let ctx = BigIntInterpreter.update_ctx_with_inputs BigIntInterpreter.empty_ctx inputs in
      let ctx = BigIntInterpreter.evaluate_program p ctx code_loc_start_value in
      fun () -> BigIntInterpreter.print_output bir_func ctx
  | Interval ->
      Mpfr.set_default_prec 64;
      let ctx = IntervalInterpreter.update_ctx_with_inputs IntervalInterpreter.empty_ctx inputs in
      let ctx = IntervalInterpreter.evaluate_program p ctx code_loc_start_value in
      fun () -> IntervalInterpreter.print_output bir_func ctx
  | Rational ->
      let ctx = RationalInterpreter.update_ctx_with_inputs RationalInterpreter.empty_ctx inputs in
      let ctx = RationalInterpreter.evaluate_program p ctx code_loc_start_value in
      fun () -> RationalInterpreter.print_output bir_func ctx

let evaluate_expr (p : Mir.program) (e : expression Pos.marked) (sort : value_sort) : literal =
  let f p e =
    match sort with
    | RegularFloat ->
        RegularFloatInterpreter.value_to_literal
          (RegularFloatInterpreter.evaluate_expr RegularFloatInterpreter.empty_ctx p e)
    | MPFR prec ->
        Mpfr.set_default_prec prec;
        MPFRInterpreter.value_to_literal
          (MPFRInterpreter.evaluate_expr MPFRInterpreter.empty_ctx p e)
    | BigInt prec ->
        BigIntPrecision.scaling_factor_bits := prec;
        BigIntInterpreter.value_to_literal
          (BigIntInterpreter.evaluate_expr BigIntInterpreter.empty_ctx p e)
    | Interval ->
        Mpfr.set_default_prec 64;
        IntervalInterpreter.value_to_literal
          (IntervalInterpreter.evaluate_expr IntervalInterpreter.empty_ctx p e)
    | Rational ->
        RationalInterpreter.value_to_literal
          (RationalInterpreter.evaluate_expr RationalInterpreter.empty_ctx p e)
  in
  f p e
