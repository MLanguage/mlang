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
  | InsideFunction of Bir.function_name
  | InsideIterate of Mir.Variable.t

let format_code_location_segment (fmt : Format.formatter)
    (s : code_location_segment) =
  match s with
  | InsideBlock i -> Format.fprintf fmt "#%d" i
  | ConditionalBranch b -> Format.fprintf fmt "?%b" b
  | InsideFunction f -> Format.fprintf fmt "%s" f
  | InsideIterate v ->
      Format.fprintf fmt "IT_%s" (Pos.unmark v.Mir.Variable.name)

type code_location = code_location_segment list

let format_code_location (fmt : Format.formatter) (l : code_location) =
  Format.pp_print_list
    ~pp_sep:(fun fmt _ -> Format.fprintf fmt "->")
    format_code_location_segment fmt l

let assign_hook :
    (Mir.Variable.t -> (unit -> var_literal) -> code_location -> unit) ref =
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
    Format.formatter -> Mir.Variable.t * var_value -> unit

  type print_ctx = { indent : int; is_newline : bool }

  type ctx = {
    ctx_local_vars : value Pos.marked Mir.LocalVariableMap.t;
    ctx_vars : var_value Mir.VariableMap.t;
    ctx_it : Mir.Variable.t StrMap.t;
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

  val update_ctx_with_inputs : ctx -> Mir.literal Mir.VariableMap.t -> ctx

  val complete_ctx : ctx -> Mir.Variable.t StrMap.t -> ctx

  type run_error =
    | NanOrInf of string * Mir.expression Pos.marked
    | StructuredError of
        (string * (string option * Pos.t) list * (unit -> unit) option)

  exception RuntimeError of run_error * ctx

  val raise_runtime_as_structured : run_error -> 'a

  val compare_numbers : Mast.comp_op -> custom_float -> custom_float -> bool

  val evaluate_expr : ctx -> Mir.program -> Mir.expression Pos.marked -> value

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
      ((var, vl) : Mir.Variable.t * var_value) =
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
    ctx_vars : var_value Mir.VariableMap.t;
    ctx_it : Mir.Variable.t StrMap.t;
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
      ctx_vars = Mir.VariableMap.empty;
      ctx_it = StrMap.empty;
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
      (inputs : Mir.literal Mir.VariableMap.t) : ctx =
    {
      ctx with
      ctx_vars =
        Mir.VariableMap.fold
          (fun var value ctx_vars ->
            Mir.VariableMap.add var (SimpleVar value) ctx_vars)
          (Mir.VariableMap.mapi
             (fun v l ->
               match l with
               | Mir.Undefined -> Undefined
               | Mir.Float f -> Number (N.of_float_input v f))
             inputs)
          ctx.ctx_vars;
    }

  let complete_ctx (ctx : ctx) (vars : Mir.Variable.t StrMap.t) : ctx =
    {
      ctx with
      ctx_vars =
        StrMap.fold
          (fun _ var ctx_vars ->
            match Mir.VariableMap.find_opt var ctx.ctx_vars with
            | Some _ -> ctx_vars
            | None ->
                let value =
                  match var.is_table with
                  | Some size -> TableVar (size, Array.make size Undefined)
                  | None -> SimpleVar Undefined
                in
                Mir.VariableMap.add var value ctx_vars)
          vars ctx.ctx_vars;
    }

  type run_error =
    | NanOrInf of string * Mir.expression Pos.marked
    | StructuredError of
        (string * (string option * Pos.t) list * (unit -> unit) option)

  exception RuntimeError of run_error * ctx

  let raise_runtime_as_structured (e : run_error) =
    match e with
    | NanOrInf (v, e) ->
        Errors.raise_spanned_error
          (Format.asprintf "Expression evaluated to %s: %a" v
             Format_mir.format_expression (Pos.unmark e))
          (Pos.get_position e)
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
      (e : Mir.expression Pos.marked) : value =
    let var_value var =
      let var =
        match StrMap.find_opt var.Mir.Variable.id ctx.ctx_it with
        | Some mvar -> mvar
        | None -> var
      in
      try
        match Mir.VariableMap.find var ctx.ctx_vars with
        | SimpleVar l -> l
        | TableVar (size, tab) -> if size > 0 then tab.(0) else Undefined
      with Not_found ->
        Errors.raise_spanned_error
          ("Var not found (should not happen): " ^ Pos.unmark var.name)
          (Pos.get_position e)
    in
    let comparison op new_e1 new_e2 =
      match (op, new_e1, new_e2) with
      | Mast.Gt, _, Undefined | Mast.Gt, Undefined, _ -> Undefined
      | Mast.Gte, _, Undefined | Mast.Gte, Undefined, _ -> Undefined
      | Mast.Lt, _, Undefined | Mast.Lt, Undefined, _ -> Undefined
      | Mast.Lte, _, Undefined | Mast.Lte, Undefined, _ -> Undefined
      | Mast.Eq, _, Undefined | Mast.Eq, Undefined, _ -> Undefined
      | Mast.Neq, _, Undefined | Mast.Neq, Undefined, _ -> Undefined
      | op, Number i1, Number i2 ->
          Number (real_of_bool (compare_numbers op i1 i2))
    in
    let unop op new_e1 =
      match (op, new_e1) with
      | Mast.Not, Number b1 -> Number (real_of_bool (not (bool_of_real b1)))
      | Mast.Minus, Number f1 -> Number N.(zero () -. f1)
      | Mast.Not, Undefined -> Undefined
      | Mast.Minus, Undefined -> Undefined
    in
    let binop op new_e1 new_e2 =
      match (op, new_e1, new_e2) with
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
      | Mast.Or, Undefined, Number i | Mast.Or, Number i, Undefined -> Number i
      | Mast.And, Number i1, Number i2 ->
          Number (real_of_bool (bool_of_real i1 && bool_of_real i2))
      | Mast.Or, Number i1, Number i2 ->
          Number (real_of_bool (bool_of_real i1 || bool_of_real i2))
    in
    let out =
      try
        match Pos.unmark e with
        | Mir.TestInSet (positive, e0, values) ->
            let new_e0 = evaluate_expr ctx p e0 in
            let or_chain =
              List.fold_left
                (fun or_chain set_value ->
                  let equal_test =
                    match set_value with
                    | Mir.VarValue set_var ->
                        let new_set_var = var_value (Pos.unmark set_var) in
                        comparison Mast.Eq new_e0 new_set_var
                    | Mir.FloatValue i ->
                        let val_i = Number (N.of_float (Pos.unmark i)) in
                        comparison Mast.Eq new_e0 val_i
                    | Mir.Interval (bn, en) ->
                        let val_bn =
                          Number (N.of_float (float_of_int (Pos.unmark bn)))
                        in
                        let val_en =
                          Number (N.of_float (float_of_int (Pos.unmark en)))
                        in
                        binop Mast.And
                          (comparison Mast.Gte new_e0 val_bn)
                          (comparison Mast.Lte new_e0 val_en)
                  in
                  binop Mast.Or or_chain equal_test)
                Undefined values
            in
            if positive then or_chain else unop Mast.Not or_chain
        | Comparison (op, e1, e2) ->
            let new_e1 = evaluate_expr ctx p e1 in
            let new_e2 = evaluate_expr ctx p e2 in
            comparison (Pos.unmark op) new_e1 new_e2
        | Binop (op, e1, e2) ->
            let new_e1 = evaluate_expr ctx p e1 in
            let new_e2 = evaluate_expr ctx p e2 in
            binop (Pos.unmark op) new_e1 new_e2
        | Unop (op, e1) ->
            let new_e1 = evaluate_expr ctx p e1 in
            unop op new_e1
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
              match StrMap.find_opt var.Mir.Variable.id ctx.ctx_it with
              | Some mvar -> mvar
              | None -> var
            in
            let new_e1 = evaluate_expr ctx p e1 in
            if new_e1 = Undefined then Undefined
            else
              match Mir.VariableMap.find var ctx.ctx_vars with
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
        | Var var -> var_value (Pos.unmark var)
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
        | FunctionCall (Supzero, [ arg ]) -> (
            match evaluate_expr ctx p arg with
            | Undefined -> Undefined
            | Number f as n ->
                if compare_numbers Mast.Lte f (N.zero ()) then Undefined else n)
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
            match evaluate_expr ctx p arg1 with
            | Undefined -> Undefined
            | Number f -> (
                let up = N.to_int (roundf f) in
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
                           (var_arg2, (Literal (Float (float_of_int i)), pos)),
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
                match !maxi with
                | None -> Undefined
                | Some f -> Number (N.of_int f)))
        | FunctionCall (_func, _) -> assert false
        | Attribut (_v, var, a) -> (
            match StrMap.find_opt var.id ctx.ctx_it with
            | Some mvar -> (
                match StrMap.find_opt (Pos.unmark a) mvar.attributes with
                | Some l -> Number (N.of_float (float (Pos.unmark l)))
                | None -> Undefined)
            | None -> assert false)
        | Size var -> (
            match StrMap.find_opt (Pos.unmark var).id ctx.ctx_it with
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
          if !exit_on_rte then raise_runtime_as_structured e
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
      if !exit_on_rte then raise_runtime_as_structured e
      else raise (RuntimeError (e, ctx))
    else out

  let evaluate_simple_variable (p : Bir.program) (ctx : ctx)
      (expr : Mir.expression) : var_value =
    SimpleVar (evaluate_expr ctx p.mir_program (expr, Pos.no_pos))

  let evaluate_variable (p : Bir.program) (ctx : ctx) (var : Mir.Variable.t)
      (curr_value : var_value)
      (vidx_opt : (int * Mir.expression Pos.marked) option)
      (vexpr : Mir.expression Pos.marked) : var_value =
    match vidx_opt with
    | None -> (
        match var.is_table with
        | Some sz ->
            let value = evaluate_expr ctx p.mir_program vexpr in
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
        | None -> SimpleVar (evaluate_expr ctx p.mir_program vexpr))
    | Some (size, ei) -> (
        let i = evaluate_expr ctx p.mir_program ei in
        match var.is_table with
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
                  tab.(i') <- evaluate_expr ctx p.mir_program vexpr);
            TableVar (size, tab)
        | None -> (
            match i with
            | Undefined -> curr_value
            | Number f ->
                let i' = int_of_float (N.to_float f) in
                if i' = 0 then SimpleVar (evaluate_expr ctx p.mir_program vexpr)
                else curr_value))

  exception BlockingError of ctx

  let rec evaluate_stmt (canBlock : bool) (p : Bir.program) (ctx : ctx)
      (stmt : Bir.stmt) (loc : code_location) =
    match Pos.unmark stmt with
    | Bir.SAssign (var, vidx_opt, vexpr) ->
        let var =
          match StrMap.find_opt var.id ctx.ctx_it with
          | Some mvar -> mvar
          | None -> var
        in
        let value =
          try Mir.VariableMap.find var ctx.ctx_vars
          with Not_found -> (
            match var.is_table with
            | Some size -> TableVar (size, Array.make size Undefined)
            | None -> SimpleVar Undefined)
        in
        let res = evaluate_variable p ctx var value vidx_opt vexpr in
        !assign_hook var (fun _ -> var_value_to_var_literal res) loc;
        { ctx with ctx_vars = Mir.VariableMap.add var res ctx.ctx_vars }
    | Bir.SConditional (b, t, f) -> (
        match evaluate_simple_variable p ctx b with
        | SimpleVar (Number z) when N.(z =. zero ()) ->
            evaluate_stmts canBlock p ctx f (ConditionalBranch false :: loc) 0
        | SimpleVar (Number _) ->
            evaluate_stmts canBlock p ctx t (ConditionalBranch true :: loc) 0
        | SimpleVar Undefined -> ctx
        | _ -> assert false)
    | Bir.SVerifBlock stmts ->
        evaluate_stmts true p ctx stmts (InsideBlock 0 :: loc) 0
    | Bir.SFunctionCall (f, _args) ->
        let tf = Mir.TargetMap.find f p.targets in
        evaluate_target canBlock p ctx loc tf
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
                  match StrMap.find_opt var.id ctx.ctx_it with
                  | Some mvar -> pr_raw ctx_pr (Pos.unmark mvar.name)
                  | None -> assert false)
              | Mir.PrintAlias (_, var) -> (
                  match StrMap.find_opt var.id ctx.ctx_it with
                  | Some mvar ->
                      pr_raw ctx_pr
                        (match mvar.alias with
                        | Some a -> Pos.unmark a
                        | None -> "")
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
          StrMap.fold
            (fun _ v ctx ->
              if v.Mir.Variable.cats = Some vc then
                let ctx =
                  { ctx with ctx_it = StrMap.add var.id v ctx.ctx_it }
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
          Mir.VariableSet.fold
            (fun v backup ->
              let v =
                match StrMap.find_opt v.id ctx.ctx_it with
                | None -> v
                | Some v -> v
              in
              let value = Mir.VariableMap.find v ctx.ctx_vars in
              (v, value) :: backup)
            vars []
        in
        let backup =
          List.fold_left
            (fun backup ((var : Mir.Variable.t), vcs, expr) ->
              Mir.CatVarSet.fold
                (fun vc backup ->
                  StrMap.fold
                    (fun _ v backup ->
                      if v.Mir.Variable.cats = Some vc then
                        let ctx =
                          { ctx with ctx_it = StrMap.add var.id v ctx.ctx_it }
                        in
                        match evaluate_simple_variable p ctx expr with
                        | SimpleVar (Number z) when N.(z =. one ()) ->
                            let value = Mir.VariableMap.find v ctx.ctx_vars in
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
              Mir.VariableMap.update v (fun _ -> Some value) ctx_vars)
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
          let is_b = err.typ = Mast.Anomaly && Pos.unmark err.isisf = "N" in
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
            | None -> Mir.VariableMap.add var (SimpleVar Undefined) ctx_vars
            | Some sz ->
                let values = Array.init sz (fun _ -> Undefined) in
                Mir.VariableMap.add var (TableVar (sz, values)) ctx_vars)
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
          (Bir.main_statements p @ [ (Bir.SExportErrors, Pos.no_pos) ])
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
      if !exit_on_rte then raise_runtime_as_structured e
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

let evaluate_program (p : Bir.program) (inputs : Mir.literal Mir.VariableMap.t)
    (sort : Cli.value_sort) (roundops : Cli.round_ops) :
    float option StrMap.t * StrSet.t =
  prepare_interp sort roundops;
  let module Interp = (val get_interp sort roundops : S) in
  let ctx = Interp.update_ctx_with_inputs Interp.empty_ctx inputs in
  let ctx = Interp.complete_ctx ctx p.Bir.mir_program.Mir.program_vars in
  let ctx = Interp.evaluate_program p ctx 0 in
  let varMap =
    let fold (var : Mir.Variable.t) value res =
      let name = Pos.unmark var.name in
      let fVal =
        match value with
        | Interp.SimpleVar litt -> (
            match Interp.value_to_literal litt with
            | Mir.Float f -> Some f
            | Mir.Undefined -> None)
        | _ -> None
      in
      StrMap.add name fVal res
    in
    Mir.VariableMap.fold fold ctx.ctx_vars StrMap.empty
  in
  let anoSet =
    let fold res (e, _) = StrSet.add (Pos.unmark e.Mir.name) res in
    List.fold_left fold StrSet.empty ctx.ctx_exported_anos
  in
  (varMap, anoSet)

let evaluate_expr (p : Mir.program) (e : Mir.expression Pos.marked)
    (sort : Cli.value_sort) (roundops : Cli.round_ops) : Mir.literal =
  let module Interp = (val get_interp sort roundops : S) in
  Interp.value_to_literal (Interp.evaluate_expr Interp.empty_ctx p e)
