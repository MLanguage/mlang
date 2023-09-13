(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

type var_literal =
  | SimpleVar of Mir.literal
  | TableVar of int * Mir.literal array

type code_location_segment =
  | InsideBlock of int
  | ConditionalBranch of bool
  | InsideRule of Bir.rov_id
  | InsideFunction of Bir.function_name

let format_code_location_segment (fmt : Format.formatter)
    (s : code_location_segment) =
  match s with
  | InsideBlock i -> Format.fprintf fmt "#%d" i
  | ConditionalBranch b -> Format.fprintf fmt "?%b" b
  | InsideRule r -> Format.fprintf fmt "R_%d" (Mir.num_of_rule_or_verif_id r)
  | InsideFunction f -> Format.fprintf fmt "%s" f

type code_location = code_location_segment list

let format_code_location (fmt : Format.formatter) (l : code_location) =
  Format.pp_print_list
    ~pp_sep:(fun fmt _ -> Format.fprintf fmt "->")
    format_code_location_segment fmt l

let assign_hook :
    (Bir.variable -> (unit -> var_literal) -> code_location -> unit) ref =
  ref (fun _var _lit _code_loc -> ())

let exit_on_rte = ref true

let repl_debug = ref false

module type S = sig
  type custom_float

  type value = Number of custom_float | Undefined

  val format_value : Format.formatter -> value -> unit

  type var_value = SimpleVar of value | TableVar of int * value array

  val format_var_value : Format.formatter -> var_value -> unit

  val format_var_value_with_var :
    Format.formatter -> Bir.variable * var_value -> unit

  type ctx = {
    ctx_local_vars : value Pos.marked Mir.LocalVariableMap.t;
    ctx_vars : var_value Bir.VariableMap.t;
  }

  val empty_ctx : ctx

  val literal_to_value : Mir.literal -> value

  val var_literal_to_var_value : var_literal -> var_value

  val value_to_literal : value -> Mir.literal

  val var_value_to_var_literal : var_value -> var_literal

  val update_ctx_with_inputs : ctx -> Mir.literal Bir.VariableMap.t -> ctx

  type run_error =
    | ErrorValue of string * Pos.t
    | FloatIndex of string * Pos.t
    | IndexOutOfBounds of string * Pos.t
    | IncorrectOutputVariable of string * Pos.t
    | UnknownInputVariable of string * Pos.t
    | ConditionViolated of
        Mir.Error.t
        * Bir.expression Pos.marked
        * (Bir.variable * var_value) list
    | NanOrInf of string * Bir.expression Pos.marked
    | StructuredError of
        (string * (string option * Pos.t) list * (unit -> unit) option)

  exception RuntimeError of run_error * ctx

  val replace_undefined_with_input_variables :
    Mir.program -> Mir.VariableDict.t -> Mir.program

  val print_output : Bir_interface.bir_function -> ctx -> unit

  val raise_runtime_as_structured : run_error -> ctx -> Mir.program -> 'a

  val evaluate_expr : ctx -> Mir.program -> Bir.expression Pos.marked -> value

  val evaluate_program : Bir.program -> ctx -> int -> ctx
end

module Make (N : Bir_number.NumberInterface) (RF : Bir_roundops.RoundOpsFunctor) =
struct
  (* Careful : this behavior mimics the one imposed by the original Mlang
     compiler... *)

  module R = RF (N)

  type custom_float = N.t

  let truncatef (x : N.t) : N.t = R.truncatef x

  let roundf (x : N.t) = R.roundf x

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

  let format_var_value_with_var (fmt : Format.formatter)
      ((var, vl) : Bir.variable * var_value) =
    let var = Bir.var_to_mir var in
    match vl with
    | SimpleVar value ->
        Format.fprintf fmt "%s (%s): %a"
          (Pos.unmark var.Mir.Variable.name)
          (Pos.unmark var.Mir.Variable.descr)
          format_value value
    | TableVar (size, values) ->
        Format.fprintf fmt "%s (%s): Table (%d values)@\n"
          (Pos.unmark var.Mir.Variable.name)
          (Pos.unmark var.Mir.Variable.descr)
          size;
        List.iteri
          (fun idx value ->
            Format.fprintf fmt "| %d -> %a\n" idx format_value value)
          (Array.to_list values)

  type ctx = {
    ctx_local_vars : value Pos.marked Mir.LocalVariableMap.t;
    ctx_vars : var_value Bir.VariableMap.t;
  }

  let empty_ctx : ctx =
    {
      ctx_local_vars = Mir.LocalVariableMap.empty;
      ctx_vars = Bir.VariableMap.empty;
    }

  let literal_to_value (l : Mir.literal) : value =
    match l with
    | Mir.Undefined -> Undefined
    | Mir.Float f -> Number (N.of_float f)

  let var_literal_to_var_value (def : var_literal) : var_value =
    match def with
    | SimpleVar v -> SimpleVar (literal_to_value v)
    | TableVar (size, defs) ->
        TableVar (size, Array.map (fun v -> literal_to_value v) defs)

  let value_to_literal (l : value) : Mir.literal =
    match l with
    | Undefined -> Mir.Undefined
    | Number f -> Mir.Float (N.to_float f)

  let var_value_to_var_literal (def : var_value) : var_literal =
    let l : var_literal =
      match def with
      | SimpleVar v -> SimpleVar (value_to_literal v)
      | TableVar (size, defs) ->
          TableVar (size, Array.map (fun v -> value_to_literal v) defs)
    in
    l

  let update_ctx_with_inputs (ctx : ctx)
      (inputs : Mir.literal Bir.VariableMap.t) : ctx =
    {
      ctx with
      ctx_vars =
        Bir.VariableMap.fold
          (fun var value ctx_vars ->
            Bir.VariableMap.add var (SimpleVar value) ctx_vars)
          (Bir.VariableMap.mapi
             (fun v l ->
               match l with
               | Mir.Undefined -> Undefined
               | Mir.Float f -> Number (N.of_float_input (Bir.var_to_mir v) f))
             inputs)
          ctx.ctx_vars;
    }

  type run_error =
    | ErrorValue of string * Pos.t
    | FloatIndex of string * Pos.t
    | IndexOutOfBounds of string * Pos.t
    | IncorrectOutputVariable of string * Pos.t
    | UnknownInputVariable of string * Pos.t
    | ConditionViolated of
        Mir.Error.t
        * Bir.expression Pos.marked
        * (Bir.variable * var_value) list
    | NanOrInf of string * Bir.expression Pos.marked
    | StructuredError of
        (string * (string option * Pos.t) list * (unit -> unit) option)

  exception RuntimeError of run_error * ctx

  (* During evaluation, variables that have an I/O property set to InputVariable
     have a value that is read directly from the input map. However, one can
     pass inside the input map a value for a variable whose I/O type was not
     properly set to InputVariable. This function is precisely for these cases,
     it set the I/O flag properly for execution. Not that such a change to the
     program does not require to recompute the dependency graph and the
     execution order. *)
  let replace_undefined_with_input_variables (p : Mir.program)
      (input_values : Mir.VariableDict.t) : Mir.program =
    Mir.map_vars
      (fun var def ->
        match Mir.VariableDict.find var.id input_values with
        | _input -> { def with var_definition = InputVar; var_io = Input }
        | exception Not_found -> def)
      p

  let print_output (f : Bir_interface.bir_function) (results : ctx) : unit =
    Bir.VariableMap.iter
      (fun var value ->
        if Bir.VariableMap.mem var f.func_outputs then
          Cli.result_print "%a" format_var_value_with_var (var, value))
      results.ctx_vars

  let repl_debugguer (ctx : ctx) (p : Mir.program) : unit =
    Cli.warning_print
      "Starting interactive debugger. Please query the interpreter state for \
       the values of variables. Exit with \"quit\".@\n";
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
              (fun (var1 : Mir.Variable.t) var2 ->
                Mir.(
                  compare_execution_number var1.Variable.execution_number
                    var2.Variable.execution_number))
              vars
          in
          List.iter
            Mir.(
              fun var ->
                Format.printf "[%a %a] -> %a@\n"
                  Format_mir.format_execution_number_short
                  var.Variable.execution_number Pos.format_position
                  var.Variable.execution_number.pos
                  (fun fmt () ->
                    try
                      let rule, def = Mir.find_var_definition p var in
                      Format.fprintf fmt "rule %d, %a"
                        (Mir.num_of_rule_or_verif_id
                           (Pos.unmark rule.rule_number))
                        Format_mir.format_variable_def def.var_definition
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
                Mir.(
                  compare_execution_number var1.Variable.execution_number
                    var2.Variable.execution_number))
              vars
          in
          List.iter
            Mir.(
              fun var ->
                let bvar = Bir.(var_from_mir default_tgv) var in
                try
                  let var_l = Bir.VariableMap.find bvar ctx.ctx_vars in
                  Format.printf "[%a %a] -> %a@\n"
                    Format_mir.format_execution_number_short
                    var.Variable.execution_number Pos.format_position
                    var.Variable.execution_number.pos format_var_value_with_var
                    (bvar, var_l)
                with Not_found ->
                  Format.printf "[%a %a] -> not computed@\n"
                    Format_mir.format_execution_number_short
                    var.Variable.execution_number Pos.format_position
                    var.Variable.execution_number.pos)
            vars
        with Not_found -> Format.printf "Inexisting variable@\n"
    done

  let raise_runtime_as_structured (e : run_error) (ctx : ctx) (p : Mir.program)
      =
    match e with
    | ErrorValue (s, pos) ->
        Errors.raise_spanned_error
          (Format.asprintf "Error value at runtime: %s" s)
          pos
    | FloatIndex (s, pos) ->
        Errors.raise_spanned_error
          (Format.asprintf "Index is not an integer: %s" s)
          pos
    | IndexOutOfBounds (s, pos) ->
        Errors.raise_spanned_error
          (Format.asprintf "Index out of bounds: %s" s)
          pos
    | NanOrInf (v, e) ->
        Errors.raise_spanned_error_with_continuation
          (Format.asprintf "Expression evaluated to %s: %a" v
             Format_bir.format_expression (Pos.unmark e))
          (Pos.get_position e)
          (fun _ -> repl_debugguer ctx p)
    | UnknownInputVariable (s, pos) ->
        Errors.raise_spanned_error
          (Format.asprintf "Unknown input variable: %s" s)
          pos
    | IncorrectOutputVariable (s, pos) ->
        Errors.raise_spanned_error
          (Format.asprintf "Incorrect output variable: %s" s)
          pos
    | ConditionViolated (error, condition, bindings) ->
        Errors.raise_spanned_error_with_continuation
          (Format.asprintf
             "Verification condition failed! Errors thrown:\n\
             \  * %a\n\
              Violated condition:\n\
             \  * %a\n\
              Values of the relevant variables at this point:\n\
              %a"
             (fun fmt err ->
               Format.fprintf fmt "Error %s [%s]"
                 (Pos.unmark err.Mir.Error.name)
                 (Pos.unmark @@ Mir.Error.err_descr_string err))
             error Format_bir.format_expression (Pos.unmark condition)
             (Format_mast.pp_print_list_endline (fun fmt v ->
                  Format.fprintf fmt "  * %a" format_var_value_with_var v))
             bindings)
          (Pos.get_position condition)
          (fun _ -> repl_debugguer ctx p)
    | StructuredError (msg, pos, kont) ->
        raise (Errors.StructuredError (msg, pos, kont))

  let is_zero (l : value) : bool =
    match l with Number z -> N.is_zero z | _ -> false

  let real_of_bool (b : bool) = if b then N.one () else N.zero ()

  let bool_of_real (f : N.t) : bool = not N.(f =. zero ())

  let evaluate_array_index (index : value) (size : int) (values : value array) :
      value =
    let idx =
      match index with
      | Undefined -> assert false (* should not happen *)
      | Number f -> roundf f
    in
    if N.(idx >=. N.of_int (Int64.of_int size)) then Undefined
    else if N.(idx <. N.zero ()) then Number (N.zero ())
    else values.(Int64.to_int (N.to_int idx))

  let rec evaluate_expr (ctx : ctx) (p : Mir.program)
      (e : Bir.expression Pos.marked) : value =
    let out =
      try
        match Pos.unmark e with
        | Comparison (op, e1, e2) -> (
            let new_e1 = evaluate_expr ctx p e1 in
            let new_e2 = evaluate_expr ctx p e2 in
            match (Pos.unmark op, new_e1, new_e2) with
            | Mast.Gt, Number i1, Number i2 ->
                Number N.(real_of_bool (i1 >. i2))
            | Mast.Gt, _, Undefined | Mast.Gt, Undefined, _ -> Undefined
            | Mast.Gte, Number i1, Number i2 ->
                Number N.(real_of_bool (i1 >=. i2))
            | Mast.Gte, _, Undefined | Mast.Gte, Undefined, _ -> Undefined
            | Mast.Lt, Number i1, Number i2 ->
                Number N.(real_of_bool (i1 <. i2))
            | Mast.Lt, _, Undefined | Mast.Lt, Undefined, _ -> Undefined
            | Mast.Lte, Number i1, Number i2 ->
                Number N.(real_of_bool (i1 <=. i2))
            | Mast.Lte, _, Undefined | Mast.Lte, Undefined, _ -> Undefined
            | Mast.Eq, Number i1, Number i2 ->
                Number N.(real_of_bool (i1 =. i2))
            | Mast.Eq, _, Undefined | Mast.Eq, Undefined, _ -> Undefined
            | Mast.Neq, Number i1, Number i2 ->
                Number N.(real_of_bool (not (i1 =. i2)))
            | Mast.Neq, _, Undefined | Mast.Neq, Undefined, _ -> Undefined)
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
            | Mast.Div, Undefined, _ | Mast.Div, _, Undefined ->
                Undefined (* yes... *)
            | Mast.Div, _, l2 when is_zero l2 -> Number (N.zero ())
            | Mast.Div, Number i1, Number i2 -> Number N.(i1 /. i2)
            | Mast.And, Undefined, _ | Mast.And, _, Undefined -> Undefined
            | Mast.Or, Undefined, Undefined -> Undefined
            | Mast.Or, Undefined, Number i | Mast.Or, Number i, Undefined ->
                Number i
            | Mast.And, Number i1, Number i2 ->
                Number (real_of_bool (bool_of_real i1 && bool_of_real i2))
            | Mast.Or, Number i1, Number i2 ->
                Number (real_of_bool (bool_of_real i1 || bool_of_real i2)))
        | Unop (op, e1) -> (
            let new_e1 = evaluate_expr ctx p e1 in
            match (op, new_e1) with
            | Mast.Not, Number b1 ->
                Number (real_of_bool (not (bool_of_real b1)))
            | Mast.Minus, Number f1 -> Number N.(zero () -. f1)
            | Mast.Not, Undefined -> Undefined
            | Mast.Minus, Undefined -> Undefined)
        | Conditional (e1, e2, e3) -> (
            let new_e1 = evaluate_expr ctx p e1 in
            match new_e1 with
            | Number z when N.(z =. zero ()) -> evaluate_expr ctx p e3
            | Number _ -> evaluate_expr ctx p e2 (* the float is not zero *)
            | Undefined -> Undefined)
        | Literal Undefined -> Undefined
        | Literal (Float f) -> Number (N.of_float f)
        | Index (var, e1) -> (
            let new_e1 = evaluate_expr ctx p e1 in
            if new_e1 = Undefined then Undefined
            else
              match Bir.VariableMap.find (Pos.unmark var) ctx.ctx_vars with
              | SimpleVar _ -> assert false (* should not happen *)
              | TableVar (size, values) ->
                  evaluate_array_index new_e1 size values)
        | LocalVar lvar -> (
            try Pos.unmark (Mir.LocalVariableMap.find lvar ctx.ctx_local_vars)
            with Not_found -> assert false (* should not happen*))
        | Var var ->
            let r =
              try
                match Bir.VariableMap.find var ctx.ctx_vars with
                | SimpleVar l -> l
                | TableVar _ -> assert false
                (* should not happen *)
              with Not_found ->
                Errors.raise_spanned_error
                  ("Var not found (should not happen): "
                  ^ Pos.unmark (Bir.var_to_mir var).Mir.Variable.name)
                  (Pos.get_position e)
            in
            r
        | Error ->
            raise
              (RuntimeError
                 ( ErrorValue
                     ( Format.asprintf "%a" Pos.format_position
                         (Pos.get_position e),
                       Pos.get_position e ),
                   ctx ))
        | LocalLet (lvar, e1, e2) ->
            let new_e1 = evaluate_expr ctx p e1 in
            let new_e2 =
              evaluate_expr
                {
                  ctx with
                  ctx_local_vars =
                    Mir.LocalVariableMap.add lvar
                      (Pos.same_pos_as new_e1 e1)
                      ctx.ctx_local_vars;
                }
                p e2
            in
            new_e2
        | FunctionCall (ArrFunc, [ arg ]) -> (
            let new_arg = evaluate_expr ctx p arg in
            match new_arg with
            | Number x -> Number (roundf x)
            | Undefined -> Undefined
            (*nope:Float 0.*))
        | FunctionCall (InfFunc, [ arg ]) -> (
            let new_arg = evaluate_expr ctx p arg in
            match new_arg with
            | Number x -> Number (truncatef x)
            | Undefined -> Undefined
            (*Float 0.*))
        | FunctionCall (PresentFunc, [ arg ]) -> (
            match evaluate_expr ctx p arg with
            | Undefined -> false_value ()
            | _ -> true_value ())
        | FunctionCall (NullFunc, [ arg ]) -> (
            match evaluate_expr ctx p arg with
            | Undefined -> Undefined
            | Number f -> if N.is_zero f then true_value () else false_value ())
        | FunctionCall (AbsFunc, [ arg ]) -> (
            match evaluate_expr ctx p arg with
            | Undefined -> Undefined
            | Number f -> Number (N.abs f))
        | FunctionCall (MinFunc, [ arg1; arg2 ]) -> (
            match (evaluate_expr ctx p arg1, evaluate_expr ctx p arg2) with
            | Undefined, Undefined -> Undefined
            | Undefined, Number f | Number f, Undefined ->
                Number (N.min (N.zero ()) f)
            | Number fl, Number fr -> Number (N.min fl fr))
        | FunctionCall (MaxFunc, [ arg1; arg2 ]) -> (
            match (evaluate_expr ctx p arg1, evaluate_expr ctx p arg2) with
            | Undefined, Undefined -> Undefined
            | Undefined, Number f | Number f, Undefined ->
                Number (N.max (N.zero ()) f)
            | Number fl, Number fr -> Number (N.max fl fr))
        | FunctionCall (Multimax, [ arg1; arg2 ]) -> (
            let up =
              match evaluate_expr ctx p arg1 with
              | Number f -> N.to_int (roundf f)
              | e ->
                  raise
                    (RuntimeError
                       ( ErrorValue
                           ( Format.asprintf
                               "evaluation of %a should be an integer, not %a"
                               Format_bir.format_expression (Pos.unmark arg1)
                               format_value e,
                             Pos.get_position arg1 ),
                         ctx ))
            in
            let var_arg2 =
              match Pos.unmark arg2 with Var v -> v | _ -> assert false
              (* todo: rte *)
            in
            let cast_to_int (v : value) : Int64.t option =
              match v with
              | Number f -> Some (N.to_int (roundf f))
              | Undefined -> None
            in
            let pos = Pos.get_position arg2 in
            let access_index (i : int) : Int64.t option =
              cast_to_int
              @@ evaluate_expr ctx p
                   ( Index
                       ((var_arg2, pos), (Literal (Float (float_of_int i)), pos)),
                     pos )
            in
            let maxi = ref (access_index 0) in
            for i = 0 to Int64.to_int up do
              match access_index i with
              | None -> ()
              | Some n ->
                  maxi :=
                    Option.fold ~none:(Some n)
                      ~some:(fun m -> Some (max n m))
                      !maxi
            done;
            match !maxi with None -> Undefined | Some f -> Number (N.of_int f))
        | FunctionCall (func, _) ->
            raise
              (RuntimeError
                 ( ErrorValue
                     ( Format.asprintf "the function %a  has not been expanded"
                         Format_mir.format_func func,
                       Pos.get_position e ),
                   ctx ))
        | NbCategory _ -> assert false
      with
      | RuntimeError (e, ctx) ->
          if !exit_on_rte then raise_runtime_as_structured e ctx p
          else raise (RuntimeError (e, ctx))
      | Errors.StructuredError (msg, pos, kont) ->
          if !exit_on_rte then
            raise
              (Errors.StructuredError
                 ( msg,
                   pos
                   @ [
                       (Some "Expression raising the error:", Pos.get_position e);
                     ],
                   kont ))
          else raise (RuntimeError (StructuredError (msg, pos, kont), ctx))
    in
    if match out with Undefined -> false | Number out -> N.is_nan_or_inf out
    then
      let e =
        NanOrInf
          ( (match out with
            | Undefined -> assert false
            | Number out -> Format.asprintf "%a" N.format_t out),
            e )
      in
      if !exit_on_rte then raise_runtime_as_structured e ctx p
      else raise (RuntimeError (e, ctx))
    else out

  let report_violatedcondition (cond : Bir.condition_data) (ctx : ctx) : 'a =
    let err = fst cond.cond_error in
    match err.Mir.Error.typ with
    | Mast.Anomaly ->
        raise
          (RuntimeError
             ( ConditionViolated
                 ( fst cond.cond_error,
                   cond.cond_expr,
                   List.rev
                   @@ List.fold_left
                        (fun acc var ->
                          (var, Bir.VariableMap.find var ctx.ctx_vars) :: acc)
                        []
                        (List.map
                           (fun (_, x) -> Bir.(var_from_mir default_tgv) x)
                           (Mir.VariableDict.bindings
                              (Mir_dependency_graph.get_used_variables
                                 (Pos.map_under_mark
                                    (Mir.map_expr_var Bir.var_to_mir)
                                    cond.cond_expr)))) ),
               ctx ))
    | Mast.Discordance ->
        Cli.warning_print "Anomaly: %s"
          (Pos.unmark (Mir.Error.err_descr_string err));
        ctx
    | Mast.Information ->
        Cli.debug_print "Information: %s"
          (Pos.unmark (Mir.Error.err_descr_string err));
        ctx

  let evaluate_variable (p : Bir.program) (ctx : ctx) (curr_value : var_value)
      (vdef : Bir.variable Mir.variable_def_) : var_value =
    match vdef with
    | Mir.SimpleVar e -> SimpleVar (evaluate_expr ctx p.mir_program e)
    | Mir.TableVar (size, es) ->
        TableVar
          ( size,
            match es with
            | IndexGeneric (v, e) ->
                let i =
                  match Bir.VariableMap.find v ctx.ctx_vars with
                  | SimpleVar n -> n
                  | TableVar _ -> assert false
                  (* should not happen *)
                  | exception Not_found ->
                      raise
                        (RuntimeError
                           ( ErrorValue
                               ( "no value found for dynamic index",
                                 Pos.get_position e ),
                             ctx ))
                in
                let tval =
                  match curr_value with
                  | SimpleVar _ -> assert false (* should not happen *)
                  | TableVar (s, vals) ->
                      assert (s = size);
                      vals
                in
                (match i with
                | Undefined -> ()
                | Number f ->
                    let i = int_of_float (N.to_float f) in
                    if i < 0 || i >= size then
                      raise
                        (RuntimeError
                           ( IndexOutOfBounds
                               ("dynamic index out of bound", Pos.get_position e),
                             ctx )));
                Array.init size (fun idx ->
                    match i with
                    | Number f when int_of_float (N.to_float f) = idx ->
                        evaluate_expr ctx p.mir_program e
                    | Number _ | Undefined -> tval.(idx))
            | IndexTable es ->
                Array.init size (fun idx ->
                    let e = Mir.IndexMap.find idx es in
                    evaluate_expr ctx p.mir_program e) )
    | Mir.InputVar -> assert false

  let rec evaluate_stmt (p : Bir.program) (ctx : ctx) (stmt : Bir.stmt)
      (loc : code_location) =
    match Pos.unmark stmt with
    | Bir.SAssign (var, vdef) ->
        let value =
          try Bir.VariableMap.find var ctx.ctx_vars
          with Not_found -> (
            match (Bir.var_to_mir var).is_table with
            | Some size -> TableVar (size, Array.make size Undefined)
            | None -> SimpleVar Undefined)
        in
        let res = evaluate_variable p ctx value vdef in
        !assign_hook var (fun _ -> var_value_to_var_literal res) loc;
        { ctx with ctx_vars = Bir.VariableMap.add var res ctx.ctx_vars }
    | Bir.SConditional (b, t, f) -> (
        match
          evaluate_variable p ctx (SimpleVar Undefined)
            (SimpleVar (b, Pos.no_pos))
        with
        | SimpleVar (Number z) when N.(z =. zero ()) ->
            evaluate_stmts p ctx f (ConditionalBranch false :: loc) 0
        | SimpleVar (Number _) ->
            evaluate_stmts p ctx t (ConditionalBranch true :: loc) 0
        | SimpleVar Undefined -> ctx
        | _ -> assert false)
    | Bir.SVerif data -> (
        match evaluate_expr ctx p.mir_program data.cond_expr with
        | Number f when not (N.is_zero f) -> report_violatedcondition data ctx
        | _ -> ctx)
    | Bir.SRovCall r ->
        let rule = Bir.ROVMap.find r p.rules_and_verifs in
        evaluate_stmts p ctx
          (Bir.rule_or_verif_as_statements rule)
          (InsideRule r :: loc) 0
    | Bir.SFunctionCall (f, _args) -> (
        match Mir.TargetMap.find_opt f p.targets with
        | Some tf -> evaluate_stmts p ctx tf.stmts loc 0
        | None ->
            evaluate_stmts p ctx
              (Bir.FunctionMap.find f p.mpp_functions).mppf_stmts loc 0)
  (* Mpp_function arguments seem to be used only to determine which variables
     are actually output. Does this actually make sense ? *)

  and evaluate_stmts (p : Bir.program) (ctx : ctx) (stmts : Bir.stmt list)
      (loc : code_location) (start_value : int) : ctx =
    let ctx, _ =
      List.fold_left
        (fun (ctx, i) stmt ->
          (evaluate_stmt p ctx stmt (InsideBlock i :: loc), i + 1))
        (ctx, start_value) stmts
    in
    ctx

  let evaluate_program (p : Bir.program) (ctx : ctx)
      (code_loc_start_value : int) : ctx =
    try
      let ctx =
        evaluate_stmts p ctx
          (Bir.main_statements_with_context_and_tgv_init p)
          [] code_loc_start_value
        (* For the interpreter to operate properly, all input variables must be
           declared at some point, even if they aren't used as input (either
           contextual constants or entered at interpreter prompt). The M program
           doesn't include default assignation for non-entered input variables,
           so unused inputs are not declared in the main statements.

           The use of main_statement_with_context_and_tgv_init ensures every
           variable from the TGV dictionnary is assigned to "undefined" by
           default, before context statements overload the contextual constants
           according to the spec file and interpreter prompt assignements
           overload entered variables. *)
      in
      ctx
    with RuntimeError (e, ctx) ->
      if !exit_on_rte then raise_runtime_as_structured e ctx p.mir_program
      else raise (RuntimeError (e, ctx))
end

module BigIntPrecision = struct
  let scaling_factor_bits = ref 64
end

module MainframeLongSize = struct
  let max_long = ref Int64.max_int
end

module FloatDefInterp =
  Make (Bir_number.RegularFloatNumber) (Bir_roundops.DefaultRoundOps)
module FloatMultInterp =
  Make (Bir_number.RegularFloatNumber) (Bir_roundops.MultiRoundOps)
module FloatMfInterp =
  Make
    (Bir_number.RegularFloatNumber)
    (Bir_roundops.MainframeRoundOps (MainframeLongSize))
module MPFRDefInterp =
  Make (Bir_number.MPFRNumber) (Bir_roundops.DefaultRoundOps)
module MPFRMultInterp =
  Make (Bir_number.MPFRNumber) (Bir_roundops.MultiRoundOps)
module MPFRMfInterp =
  Make
    (Bir_number.MPFRNumber)
    (Bir_roundops.MainframeRoundOps (MainframeLongSize))
module BigIntDefInterp =
  Make
    (Bir_number.BigIntFixedPointNumber
       (BigIntPrecision))
       (Bir_roundops.DefaultRoundOps)
module BigIntMultInterp =
  Make
    (Bir_number.BigIntFixedPointNumber
       (BigIntPrecision))
       (Bir_roundops.MultiRoundOps)
module BigIntMfInterp =
  Make
    (Bir_number.BigIntFixedPointNumber
       (BigIntPrecision))
       (Bir_roundops.MainframeRoundOps (MainframeLongSize))
module IntvDefInterp =
  Make (Bir_number.IntervalNumber) (Bir_roundops.DefaultRoundOps)
module IntvMultInterp =
  Make (Bir_number.IntervalNumber) (Bir_roundops.MultiRoundOps)
module IntvMfInterp =
  Make
    (Bir_number.IntervalNumber)
    (Bir_roundops.MainframeRoundOps (MainframeLongSize))
module RatDefInterp =
  Make (Bir_number.RationalNumber) (Bir_roundops.DefaultRoundOps)
module RatMultInterp =
  Make (Bir_number.RationalNumber) (Bir_roundops.MultiRoundOps)
module RatMfInterp =
  Make
    (Bir_number.RationalNumber)
    (Bir_roundops.MainframeRoundOps (MainframeLongSize))

let get_interp (sort : Cli.value_sort) (roundops : Cli.round_ops) : (module S) =
  match (sort, roundops) with
  | RegularFloat, RODefault -> (module FloatDefInterp)
  | RegularFloat, ROMulti -> (module FloatMultInterp)
  | RegularFloat, ROMainframe _ -> (module FloatMfInterp)
  | MPFR _, RODefault -> (module MPFRDefInterp)
  | MPFR _, ROMulti -> (module MPFRMultInterp)
  | MPFR _, ROMainframe _ -> (module MPFRMfInterp)
  | BigInt _, RODefault -> (module BigIntDefInterp)
  | BigInt _, ROMulti -> (module BigIntMultInterp)
  | BigInt _, ROMainframe _ -> (module BigIntMfInterp)
  | Interval, RODefault -> (module IntvDefInterp)
  | Interval, ROMulti -> (module IntvMultInterp)
  | Interval, ROMainframe _ -> (module IntvMfInterp)
  | Rational, RODefault -> (module RatDefInterp)
  | Rational, ROMulti -> (module RatMultInterp)
  | Rational, ROMainframe _ -> (module RatMfInterp)

let prepare_interp (sort : Cli.value_sort) (roundops : Cli.round_ops) : unit =
  begin
    match sort with
    | MPFR prec -> Mpfr.set_default_prec prec
    | BigInt prec -> BigIntPrecision.scaling_factor_bits := prec
    | Interval -> Mpfr.set_default_prec 64
    | _ -> ()
  end;
  match roundops with
  | ROMainframe long_size ->
      let max_long =
        if long_size = 32 then Int64.of_int32 Int32.max_int
        else if long_size = 64 then Int64.max_int
        else assert false
        (* checked when parsing command line *)
      in
      MainframeLongSize.max_long := max_long
  | _ -> ()

let evaluate_program (bir_func : Bir_interface.bir_function) (p : Bir.program)
    (inputs : Mir.literal Bir.VariableMap.t) (code_loc_start_value : int)
    (sort : Cli.value_sort) (roundops : Cli.round_ops) : unit -> unit =
  prepare_interp sort roundops;
  let module Interp = (val get_interp sort roundops : S) in
  let ctx = Interp.update_ctx_with_inputs Interp.empty_ctx inputs in
  let ctx = Interp.evaluate_program p ctx code_loc_start_value in
  fun () -> Interp.print_output bir_func ctx

let evaluate_expr (p : Mir.program) (e : Bir.expression Pos.marked)
    (sort : Cli.value_sort) (roundops : Cli.round_ops) : Mir.literal =
  let module Interp = (val get_interp sort roundops : S) in
  Interp.value_to_literal (Interp.evaluate_expr Interp.empty_ctx p e)
