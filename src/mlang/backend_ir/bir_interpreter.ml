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
  | InsideIterate of Bir.variable

let format_code_location_segment (fmt : Format.formatter)
    (s : code_location_segment) =
  match s with
  | InsideBlock i -> Format.fprintf fmt "#%d" i
  | ConditionalBranch b -> Format.fprintf fmt "?%b" b
  | InsideRule r -> Format.fprintf fmt "R_%d" (Mir.num_of_rule_or_verif_id r)
  | InsideFunction f -> Format.fprintf fmt "%s" f
  | InsideIterate v ->
      Format.fprintf fmt "IT_%s" (Pos.unmark v.Bir.mir_var.name)

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

  val format_value_prec : int -> int -> Format.formatter -> value -> unit

  type var_value = SimpleVar of value | TableVar of int * value array

  val format_var_value : Format.formatter -> var_value -> unit

  val format_var_value_prec :
    int -> int -> Format.formatter -> var_value -> unit

  val format_var_value_with_var :
    Format.formatter -> Bir.variable * var_value -> unit

  type print_ctx = { indent : int; is_newline : bool }

  type ctx = {
    ctx_local_vars : value Pos.marked Mir.LocalVariableMap.t;
    ctx_vars : var_value Bir.VariableMap.t;
    ctx_it : Mir.variable IntMap.t;
    ctx_pr_out : print_ctx;
    ctx_pr_err : print_ctx;
    ctx_anos : (Mir.error * string option) list;
    ctx_old_anos : StrSet.t;
    ctx_nb_anos : int;
    ctx_nb_discos : int;
    ctx_nb_infos : int;
    ctx_nb_bloquantes : int;
    ctx_finalized_anos : (Mir.error * string option) list;
    ctx_exported_anos : (Mir.error * string option) list;
  }

  val empty_ctx : ctx

  val literal_to_value : Mir.literal -> value

  val var_literal_to_var_value : var_literal -> var_value

  val value_to_literal : value -> Mir.literal

  val var_value_to_var_literal : var_value -> var_literal

  val update_ctx_with_inputs : ctx -> Mir.literal Bir.VariableMap.t -> ctx

  val complete_ctx : ctx -> Mir.VariableDict.t -> ctx

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
    | RaisedError of Mir.Error.t * string option * Pos.t

  exception RuntimeError of run_error * ctx

  val print_output : Bir_interface.bir_function -> ctx -> unit

  val raise_runtime_as_structured : run_error -> ctx -> Mir.program -> 'a

  val compare_numbers : Mast.comp_op -> custom_float -> custom_float -> bool

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

  let format_value_prec (mi : int) (ma : int) (fmt : Format.formatter)
      (x : value) =
    match x with
    | Undefined -> Format_mir.format_literal fmt Mir.Undefined
    | Number x -> N.format_prec_t mi ma fmt x

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

  let format_var_value_prec (mi : int) (ma : int) (fmt : Format.formatter)
      (var_lit : var_value) : unit =
    match var_lit with
    | SimpleVar e -> Format.fprintf fmt "%a" (format_value_prec mi ma) e
    | TableVar (_, es) ->
        Format.fprintf fmt "[%a]"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt e -> Format.fprintf fmt "%a" (format_value_prec mi ma) e))
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

  type print_ctx = { indent : int; is_newline : bool }

  type ctx = {
    ctx_local_vars : value Pos.marked Mir.LocalVariableMap.t;
    ctx_vars : var_value Bir.VariableMap.t;
    ctx_it : Mir.variable IntMap.t;
    ctx_pr_out : print_ctx;
    ctx_pr_err : print_ctx;
    ctx_anos : (Mir.error * string option) list;
    ctx_old_anos : StrSet.t;
    ctx_nb_anos : int;
    ctx_nb_discos : int;
    ctx_nb_infos : int;
    ctx_nb_bloquantes : int;
    ctx_finalized_anos : (Mir.error * string option) list;
    ctx_exported_anos : (Mir.error * string option) list;
  }

  let empty_ctx : ctx =
    {
      ctx_local_vars = Mir.LocalVariableMap.empty;
      ctx_vars = Bir.VariableMap.empty;
      ctx_it = IntMap.empty;
      ctx_pr_out = { indent = 0; is_newline = true };
      ctx_pr_err = { indent = 0; is_newline = true };
      ctx_anos = [];
      ctx_old_anos = StrSet.empty;
      ctx_nb_anos = 0;
      ctx_nb_discos = 0;
      ctx_nb_infos = 0;
      ctx_nb_bloquantes = 0;
      ctx_finalized_anos = [];
      ctx_exported_anos = [];
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

  let complete_ctx (ctx : ctx) (vars : Mir.VariableDict.t) : ctx =
    {
      ctx with
      ctx_vars =
        Mir.VariableDict.fold
          (fun mvar ctx_vars ->
            let var = Bir.(var_from_mir default_tgv) mvar in
            match Bir.VariableMap.find_opt var ctx.ctx_vars with
            | Some _ -> ctx_vars
            | None ->
                let value =
                  match (Bir.var_to_mir var).is_table with
                  | Some size -> TableVar (size, Array.make size Undefined)
                  | None -> SimpleVar Undefined
                in
                Bir.VariableMap.add var value ctx_vars)
          vars ctx.ctx_vars;
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
    | RaisedError of Mir.Error.t * string option * Pos.t

  exception RuntimeError of run_error * ctx

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
    | RaisedError (err, var_opt, pos) ->
        Errors.raise_spanned_error_with_continuation
          (Format.sprintf "Error %s thrown%s: %s"
             (Pos.unmark err.Mir.Error.name)
             (match var_opt with
             | Some var -> " with variable " ^ var
             | None -> "")
             (Pos.unmark @@ Mir.Error.err_descr_string err))
          pos
          (fun _ -> repl_debugguer ctx p)

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

  let compare_numbers op i1 i2 =
    let epsilon = N.of_float !Cli.comparison_error_margin in
    match op with
    | Mast.Gt -> N.(i1 >. i2 +. epsilon)
    | Mast.Gte -> N.(i1 >. i2 -. epsilon)
    | Mast.Lt -> N.(i1 +. epsilon <. i2)
    | Mast.Lte -> N.(i1 -. epsilon <. i2)
    | Mast.Eq -> N.(N.abs (i1 -. i2) <. epsilon)
    | Mast.Neq -> N.(N.abs (i1 -. i2) >=. epsilon)

  let rec evaluate_expr (ctx : ctx) (p : Mir.program)
      (e : Bir.expression Pos.marked) : value =
    let out =
      try
        match Pos.unmark e with
        | Comparison (op, e1, e2) -> (
            let new_e1 = evaluate_expr ctx p e1 in
            let new_e2 = evaluate_expr ctx p e2 in
            match (Pos.unmark op, new_e1, new_e2) with
            | Mast.Gt, _, Undefined | Mast.Gt, Undefined, _ -> Undefined
            | Mast.Gte, _, Undefined | Mast.Gte, Undefined, _ -> Undefined
            | Mast.Lt, _, Undefined | Mast.Lt, Undefined, _ -> Undefined
            | Mast.Lte, _, Undefined | Mast.Lte, Undefined, _ -> Undefined
            | Mast.Eq, _, Undefined | Mast.Eq, Undefined, _ -> Undefined
            | Mast.Neq, _, Undefined | Mast.Neq, Undefined, _ -> Undefined
            | op, Number i1, Number i2 ->
                Number (real_of_bool (compare_numbers op i1 i2)))
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
            let var = Pos.unmark var in
            let var =
              match IntMap.find_opt var.Bir.mir_var.id ctx.ctx_it with
              | Some mvar -> Bir.(var_from_mir default_tgv) mvar
              | None -> var
            in
            let new_e1 = evaluate_expr ctx p e1 in
            if new_e1 = Undefined then Undefined
            else
              match Bir.VariableMap.find var ctx.ctx_vars with
              | SimpleVar e ->
                  let idx =
                    match new_e1 with
                    | Undefined -> assert false (* should not happen *)
                    | Number f -> roundf f
                  in
                  if N.(idx >=. N.of_int (Int64.of_int 1)) then Undefined
                  else if N.(idx <. N.zero ()) then Number (N.zero ())
                  else e
              | TableVar (size, values) ->
                  evaluate_array_index new_e1 size values)
        | LocalVar lvar -> (
            try Pos.unmark (Mir.LocalVariableMap.find lvar ctx.ctx_local_vars)
            with Not_found -> assert false (* should not happen*))
        | Var var ->
            let var =
              match IntMap.find_opt var.Bir.mir_var.id ctx.ctx_it with
              | Some mvar -> Bir.(var_from_mir default_tgv) mvar
              | None -> var
            in
            let r =
              try
                match Bir.VariableMap.find var ctx.ctx_vars with
                | SimpleVar l -> l
                | TableVar (size, tab) ->
                    if size > 0 then tab.(0) else Undefined
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
        | Attribut (_v, var, a) -> (
            match IntMap.find_opt var.mir_var.id ctx.ctx_it with
            | Some mvar -> (
                match
                  List.find_opt
                    (fun (attr, _) -> Pos.unmark a = Pos.unmark attr)
                    mvar.attributes
                with
                | Some (_, l) -> Number (N.of_float (float (Pos.unmark l)))
                | None -> Undefined)
            | None -> assert false)
        | Size var -> (
            match IntMap.find_opt var.mir_var.id ctx.ctx_it with
            | Some mvar -> (
                match mvar.is_table with
                | Some i -> Number (N.of_float (float_of_int i))
                | None -> Number (N.of_float 1.0))
            | None -> assert false)
        | NbAnomalies -> Number (N.of_float (float ctx.ctx_nb_anos))
        | NbDiscordances -> Number (N.of_float (float ctx.ctx_nb_discos))
        | NbInformatives -> Number (N.of_float (float ctx.ctx_nb_infos))
        | NbBloquantes -> Number (N.of_float (float ctx.ctx_nb_bloquantes))
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

  let _report_error (err : Mir.error) (var_opt : string option) (_pos : Pos.t)
      (ctx : ctx) : 'a =
    match err.Mir.Error.typ with
    | Mast.Anomaly ->
        (* raise (RuntimeError (RaisedError (err, var_opt, pos), ctx)) *)
        Cli.warning_print "Anomaly%s: %s"
          (match var_opt with
          | Some var -> Format.sprintf " (%s)" var
          | None -> "")
          (Pos.unmark (Mir.Error.err_descr_string err));
        ctx
    | Mast.Discordance ->
        Cli.warning_print "Discordance%s: %s"
          (match var_opt with
          | Some var -> Format.sprintf " (%s)" var
          | None -> "")
          (Pos.unmark (Mir.Error.err_descr_string err));
        ctx
    | Mast.Information ->
        Cli.debug_print "Information%s: %s"
          (match var_opt with
          | Some var -> Format.sprintf " (%s)" var
          | None -> "")
          (Pos.unmark (Mir.Error.err_descr_string err));
        ctx

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

  let evaluate_simple_variable (p : Bir.program) (ctx : ctx)
      (expr : Bir.expression) : var_value =
    SimpleVar (evaluate_expr ctx p.mir_program (expr, Pos.no_pos))

  let evaluate_variable (p : Bir.program) (ctx : ctx) (var : Bir.variable)
      (curr_value : var_value) (vdef : Bir.variable Mir.variable_def_) :
      var_value =
    match vdef with
    | Mir.SimpleVar e -> (
        match var.Bir.mir_var.Mir.is_table with
        | Some sz ->
            let value = evaluate_expr ctx p.mir_program e in
            let tab =
              match curr_value with
              | SimpleVar _ -> Array.make sz value
              | TableVar (size, tab) ->
                  assert (size = sz);
                  for i = 0 to size - 1 do
                    tab.(i) <- value
                  done;
                  tab
            in
            TableVar (sz, tab)
        | None -> SimpleVar (evaluate_expr ctx p.mir_program e))
    | Mir.TableVar (size, es) -> (
        match es with
        | IndexGeneric (v, e) -> (
            let i =
              match Bir.VariableMap.find_opt v ctx.ctx_vars with
              | Some (SimpleVar n) -> n
              | Some (TableVar (s, t)) -> if s > 0 then t.(0) else Undefined
              | None -> assert false
            in
            match var.Bir.mir_var.Mir.is_table with
            | Some sz ->
                assert (size = sz);
                let tab =
                  match curr_value with
                  | SimpleVar e -> Array.make sz e
                  | TableVar (s, vals) ->
                      assert (s = size);
                      vals
                in
                (match i with
                | Undefined -> ()
                | Number f ->
                    let i' = int_of_float (N.to_float f) in
                    if 0 <= i' && i' < size then
                      tab.(i') <- evaluate_expr ctx p.mir_program e);
                TableVar (size, tab)
            | None -> (
                match i with
                | Undefined -> curr_value
                | Number f ->
                    let i' = int_of_float (N.to_float f) in
                    if i' = 0 then SimpleVar (evaluate_expr ctx p.mir_program e)
                    else curr_value))
        | IndexTable it -> (
            match var.Bir.mir_var.Mir.is_table with
            | Some sz ->
                assert (size = sz);
                let tab =
                  match curr_value with
                  | SimpleVar e -> Array.make sz e
                  | TableVar (s, vals) ->
                      assert (s = size);
                      vals
                in
                Mir.IndexMap.iter
                  (fun i e ->
                    if 0 <= i && i < sz then
                      tab.(i) <- evaluate_expr ctx p.mir_program e)
                  it;
                TableVar (size, tab)
            | None -> (
                match Mir.IndexMap.find_opt 0 it with
                | Some e -> SimpleVar (evaluate_expr ctx p.mir_program e)
                | None -> curr_value)))
    | Mir.InputVar -> assert false

  exception BlockingError of ctx

  let rec evaluate_stmt (canBlock : bool) (p : Bir.program) (ctx : ctx)
      (stmt : Bir.stmt) (loc : code_location) =
    match Pos.unmark stmt with
    | Bir.SAssign (var, vdef) ->
        let var =
          match IntMap.find_opt var.Bir.mir_var.id ctx.ctx_it with
          | Some mvar -> Bir.(var_from_mir default_tgv) mvar
          | None -> var
        in
        let value =
          try Bir.VariableMap.find var ctx.ctx_vars
          with Not_found -> (
            match (Bir.var_to_mir var).is_table with
            | Some size -> TableVar (size, Array.make size Undefined)
            | None -> SimpleVar Undefined)
        in
        let res = evaluate_variable p ctx var value vdef in
        !assign_hook var (fun _ -> var_value_to_var_literal res) loc;
        { ctx with ctx_vars = Bir.VariableMap.add var res ctx.ctx_vars }
    | Bir.SConditional (b, t, f) -> (
        match evaluate_simple_variable p ctx b with
        | SimpleVar (Number z) when N.(z =. zero ()) ->
            evaluate_stmts canBlock p ctx f (ConditionalBranch false :: loc) 0
        | SimpleVar (Number _) ->
            evaluate_stmts canBlock p ctx t (ConditionalBranch true :: loc) 0
        | SimpleVar Undefined -> ctx
        | _ -> assert false)
    | Bir.SVerif data -> (
        match evaluate_expr ctx p.mir_program data.cond_expr with
        | Number f when not (N.is_zero f) -> report_violatedcondition data ctx
        | _ -> ctx)
    | Bir.SVerifBlock stmts ->
        evaluate_stmts true p ctx stmts (InsideBlock 0 :: loc) 0
    | Bir.SRovCall r ->
        let rule = Bir.ROVMap.find r p.rules_and_verifs in
        evaluate_stmts canBlock p ctx
          (Bir.rule_or_verif_as_statements rule)
          (InsideRule r :: loc) 0
    | Bir.SFunctionCall (f, _args) -> (
        match Mir.TargetMap.find_opt f p.targets with
        | Some tf -> evaluate_target canBlock p ctx loc tf
        | None ->
            evaluate_stmts canBlock p ctx
              (Bir.FunctionMap.find f p.mpp_functions).mppf_stmts loc 0)
    (* Mpp_function arguments seem to be used only to determine which variables
       are actually output. Does this actually make sense ? *)
    | Bir.SPrint (std, args) -> (
        let std_fmt, ctx_pr =
          match std with
          | Mast.StdOut -> (Format.std_formatter, ctx.ctx_pr_out)
          | Mast.StdErr -> (Format.err_formatter, ctx.ctx_pr_err)
        in
        let pr_indent ctx_pr =
          if ctx_pr.is_newline then (
            for _i = 1 to ctx_pr.indent do
              Format.fprintf std_fmt " "
            done;
            { ctx_pr with is_newline = false })
          else ctx_pr
        in
        let pr_raw ctx_pr s =
          let len = String.length s in
          let rec aux ctx_pr = function
            | n when n >= len -> ctx_pr
            | n -> (
                match s.[n] with
                | '\n' ->
                    Format.fprintf std_fmt "\n";
                    aux { ctx_pr with is_newline = true } (n + 1)
                | c ->
                    let ctx_pr = pr_indent ctx_pr in
                    Format.fprintf std_fmt "%c" c;
                    aux ctx_pr (n + 1))
          in
          aux ctx_pr 0
        in
        let ctx_pr =
          List.fold_left
            (fun ctx_pr arg ->
              match arg with
              | Mir.PrintString s -> pr_raw ctx_pr s
              | Mir.PrintName (_, var) -> (
                  match IntMap.find_opt var.Mir.id ctx.ctx_it with
                  | Some mvar -> pr_raw ctx_pr (Pos.unmark mvar.Mir.name)
                  | None -> assert false)
              | Mir.PrintAlias (_, var) -> (
                  match IntMap.find_opt var.Mir.id ctx.ctx_it with
                  | Some mvar ->
                      pr_raw ctx_pr
                        (match mvar.Mir.alias with Some a -> a | None -> "")
                  | None -> assert false)
              | Mir.PrintIndent e ->
                  let var_value =
                    evaluate_simple_variable p ctx (Pos.unmark e)
                  in
                  let diff =
                    match var_value with
                    | SimpleVar e -> (
                        match e with
                        | Undefined -> 0
                        | Number x -> Int64.to_int (N.to_int (roundf x)))
                    | TableVar (_, es) -> (
                        if Array.length es = 0 then 0
                        else
                          match es.(0) with
                          | Undefined -> 0
                          | Number x -> Int64.to_int (N.to_int (roundf x)))
                  in
                  { ctx_pr with indent = max 0 (ctx_pr.indent + diff) }
              | Mir.PrintExpr (e, mi, ma) ->
                  let var_value =
                    evaluate_simple_variable p ctx (Pos.unmark e)
                  in
                  let ctx_pr = pr_indent ctx_pr in
                  format_var_value_prec mi ma std_fmt var_value;
                  ctx_pr)
            ctx_pr args
        in
        match std with
        | Mast.StdOut -> { ctx with ctx_pr_out = ctx_pr }
        | Mast.StdErr -> { ctx with ctx_pr_err = ctx_pr })
    | Bir.SIterate (var, vcs, expr, stmts) ->
        let eval vc ctx =
          Mir.VariableDict.fold
            (fun v ctx ->
              if v.Mir.cats = Some vc then
                let ctx =
                  {
                    ctx with
                    ctx_it = IntMap.add var.Bir.mir_var.id v ctx.ctx_it;
                  }
                in
                match evaluate_simple_variable p ctx expr with
                | SimpleVar (Number z) when N.(z =. one ()) ->
                    evaluate_stmts canBlock p ctx stmts
                      (ConditionalBranch true :: loc)
                      0
                | SimpleVar _ -> ctx
                | _ -> assert false
              else ctx)
            p.Bir.mir_program.program_vars ctx
        in
        Mir.CatVarSet.fold eval vcs ctx
    | Bir.SRestore (vars, var_params, stmts) ->
        let backup =
          Bir.VariableSet.fold
            (fun v backup ->
              let v =
                match IntMap.find_opt v.mir_var.id ctx.ctx_it with
                | None -> v
                | Some v -> Bir.(var_from_mir default_tgv) v
              in
              let value = Bir.VariableMap.find v ctx.ctx_vars in
              (v, value) :: backup)
            vars []
        in
        let backup =
          List.fold_left
            (fun backup (var, vcs, expr) ->
              Mir.CatVarSet.fold
                (fun vc backup ->
                  Mir.VariableDict.fold
                    (fun v backup ->
                      if v.Mir.cats = Some vc then
                        let ctx =
                          {
                            ctx with
                            ctx_it = IntMap.add var.Bir.mir_var.id v ctx.ctx_it;
                          }
                        in
                        match evaluate_simple_variable p ctx expr with
                        | SimpleVar (Number z) when N.(z =. one ()) ->
                            let v = Bir.(var_from_mir default_tgv) v in
                            let value = Bir.VariableMap.find v ctx.ctx_vars in
                            (v, value) :: backup
                        | SimpleVar _ -> backup
                        | _ -> assert false
                      else backup)
                    p.Bir.mir_program.program_vars backup)
                vcs backup)
            backup var_params
        in
        let ctx =
          evaluate_stmts canBlock p ctx stmts (InsideBlock 0 :: loc) 0
        in
        let ctx_vars =
          List.fold_left
            (fun ctx_vars (v, value) ->
              Bir.VariableMap.update v (fun _ -> Some value) ctx_vars)
            ctx.ctx_vars backup
        in
        { ctx with ctx_vars }
    | Bir.SRaiseError (err, var_opt) ->
        let ctx_nb_anos =
          if err.typ = Mast.Anomaly then ctx.ctx_nb_anos + 1
          else ctx.ctx_nb_anos
        in
        let ctx_nb_discos =
          if err.typ = Mast.Discordance then ctx.ctx_nb_discos + 1
          else ctx.ctx_nb_discos
        in
        let ctx_nb_infos =
          if err.typ = Mast.Information then ctx.ctx_nb_infos + 1
          else ctx.ctx_nb_infos
        in
        let ctx_nb_bloquantes, is_blocking =
          let is_b =
            err.typ = Mast.Anomaly && Pos.unmark err.descr.isisf = "N"
          in
          ((ctx.ctx_nb_bloquantes + if is_b then 1 else 0), is_b)
        in
        let ctx =
          {
            ctx with
            ctx_anos = ctx.ctx_anos @ [ (err, var_opt) ];
            ctx_nb_anos;
            ctx_nb_discos;
            ctx_nb_infos;
            ctx_nb_bloquantes;
          }
        in
        (* Format.eprintf "leve erreur %s\n" (Pos.unmark err.Mir.name);*)
        if is_blocking && ctx.ctx_nb_bloquantes >= 4 && canBlock then
          raise (BlockingError ctx)
        else ctx
    | Bir.SCleanErrors ->
        (*Format.eprintf "nettoie erreurs\n";*)
        {
          ctx with
          ctx_anos = [];
          ctx_nb_anos = 0;
          ctx_nb_discos = 0;
          ctx_nb_infos = 0;
          ctx_nb_bloquantes = 0;
        }
    | Bir.SFinalizeErrors ->
        let not_in_old_anos (err, _) =
          let name = Pos.unmark err.Mir.name in
          not (StrSet.mem name ctx.ctx_old_anos)
        in
        let ctx_finalized_anos =
          let rec merge_anos old_anos new_anos =
            match (old_anos, new_anos) with
            | [], anos | anos, [] -> anos
            | _ :: old_tl, a :: new_tl -> a :: merge_anos old_tl new_tl
          in
          let new_anos = List.filter not_in_old_anos ctx.ctx_anos in
          (* List.iter (fun (err, _) -> Format.eprintf "finalise: %s\n"
             (Pos.unmark err.Mir.name)) new_anos;*)
          merge_anos ctx.ctx_finalized_anos new_anos
        in
        let add_ano res (err, _) = StrSet.add (Pos.unmark err.Mir.name) res in
        let ctx_old_anos =
          List.fold_left add_ano ctx.ctx_old_anos ctx.ctx_anos
        in
        { ctx with ctx_finalized_anos; ctx_old_anos }
    | Bir.SExportErrors ->
        let ctx_exported_anos =
          ctx.ctx_exported_anos @ ctx.ctx_finalized_anos
        in
        (* List.iter (fun (err, _) -> Format.eprintf "sortie: %s\n" (Pos.unmark
           err.Mir.name)) ctx.ctx_finalized_anos;*)
        { ctx with ctx_exported_anos; ctx_finalized_anos = [] }

  and evaluate_stmts canBlock (p : Bir.program) (ctx : ctx)
      (stmts : Bir.stmt list) (loc : code_location) (start_value : int) : ctx =
    let ctx, _ =
      try
        List.fold_left
          (fun (ctx, i) stmt ->
            (evaluate_stmt canBlock p ctx stmt (InsideBlock i :: loc), i + 1))
          (ctx, start_value) stmts
      with BlockingError ctx as b_err ->
        if canBlock then raise b_err else (ctx, 0)
    in
    ctx

  and evaluate_target canBlock (p : Bir.program) (ctx : ctx)
      (loc : code_location) (tf : Bir.target_function) =
    let ctx =
      let ctx_vars =
        StrMap.fold
          (fun _ (var, _, size) ctx_vars ->
            match size with
            | None -> Bir.VariableMap.add var (SimpleVar Undefined) ctx_vars
            | Some sz ->
                let values = Array.init sz (fun _ -> Undefined) in
                Bir.VariableMap.add var (TableVar (sz, values)) ctx_vars)
          tf.tmp_vars ctx.ctx_vars
      in
      { ctx with ctx_vars }
    in
    evaluate_stmts canBlock p ctx tf.stmts loc 0

  let evaluate_program (p : Bir.program) (ctx : ctx)
      (code_loc_start_value : int) : ctx =
    try
      let ctx =
        evaluate_stmts false p ctx
          (Bir.main_statements_with_context_and_tgv_init p
          @ [ (Bir.SExportErrors, Pos.no_pos) ])
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
    (sort : Cli.value_sort) (roundops : Cli.round_ops) :
    (unit -> unit) * (Mir.error * string option) list =
  prepare_interp sort roundops;
  let module Interp = (val get_interp sort roundops : S) in
  let ctx = Interp.update_ctx_with_inputs Interp.empty_ctx inputs in
  let ctx = Interp.complete_ctx ctx p.Bir.mir_program.Mir.program_vars in
  let ctx = Interp.evaluate_program p ctx code_loc_start_value in
  let sorted_anos =
    let cmp (e0, so0) (e1, so1) =
      compare (Pos.unmark e0.Mir.name, so0) (Pos.unmark e1.Mir.name, so1)
    in
    List.sort_uniq cmp ctx.ctx_exported_anos
  in
  ((fun () -> Interp.print_output bir_func ctx), sorted_anos)

let evaluate_expr (p : Mir.program) (e : Bir.expression Pos.marked)
    (sort : Cli.value_sort) (roundops : Cli.round_ops) : Mir.literal =
  let module Interp = (val get_interp sort roundops : S) in
  Interp.value_to_literal (Interp.evaluate_expr Interp.empty_ctx p e)
