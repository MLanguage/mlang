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
  | SimpleVar of Com.literal
  | TableVar of int * Com.literal array

type code_location_segment =
  | InsideBlock of int
  | ConditionalBranch of bool
  | InsideFunction of string
  | InsideIterate of Mir.Var.t

let format_code_location_segment (fmt : Format.formatter)
    (s : code_location_segment) =
  match s with
  | InsideBlock i -> Format.fprintf fmt "#%d" i
  | ConditionalBranch b -> Format.fprintf fmt "?%b" b
  | InsideFunction f -> Format.fprintf fmt "%s" f
  | InsideIterate v -> Format.fprintf fmt "IT_%s" (Pos.unmark v.Mir.Var.name)

type code_location = code_location_segment list

let format_code_location (fmt : Format.formatter) (l : code_location) =
  Format.pp_print_list
    ~pp_sep:(fun fmt _ -> Format.fprintf fmt "->")
    format_code_location_segment fmt l

let assign_hook :
    (Mir.Var.t -> (unit -> var_literal) -> code_location -> unit) ref =
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
    Format.formatter -> Mir.Var.t * var_value -> unit

  type print_ctx = { indent : int; is_newline : bool }

  type ctx = {
    ctx_tgv : var_value Array.t;
    mutable ctx_tmps : var_value Array.t list;
    ctx_it : Mir.Var.t StrMap.t;
    ctx_pr_out : print_ctx;
    ctx_pr_err : print_ctx;
    ctx_anos : (Com.Error.t * string option) list;
    ctx_old_anos : StrSet.t;
    ctx_nb_anos : int;
    ctx_nb_discos : int;
    ctx_nb_infos : int;
    ctx_nb_bloquantes : int;
    ctx_finalized_anos : (Com.Error.t * string option) list;
    ctx_exported_anos : (Com.Error.t * string option) list;
  }

  val empty_ctx : Mir.program -> ctx

  val literal_to_value : Com.literal -> value

  val var_literal_to_var_value : var_literal -> var_value

  val value_to_literal : value -> Com.literal

  val var_value_to_var_literal : var_value -> var_literal

  val update_ctx_with_inputs : ctx -> Com.literal Mir.VariableMap.t -> ctx

  type run_error =
    | NanOrInf of string * Mir.expression Pos.marked
    | StructuredError of
        (string * (string option * Pos.t) list * (unit -> unit) option)

  exception RuntimeError of run_error * ctx

  val raise_runtime_as_structured : run_error -> 'a

  val compare_numbers : Com.comp_op -> custom_float -> custom_float -> bool

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
    | Undefined -> Com.format_literal fmt Com.Undefined
    | Number x -> N.format_t fmt x

  let format_value_prec (mi : int) (ma : int) (fmt : Format.formatter)
      (x : value) =
    match x with
    | Undefined -> Com.format_literal fmt Com.Undefined
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
      ((var, vl) : Mir.Var.t * var_value) =
    match vl with
    | SimpleVar value ->
        Format.fprintf fmt "%s (%s): %a"
          (Pos.unmark var.Mir.Var.name)
          (Mir.Var.descr_str var) format_value value
    | TableVar (size, values) ->
        Format.fprintf fmt "%s (%s): Table (%d values)@\n"
          (Pos.unmark var.Mir.Var.name)
          (Mir.Var.descr_str var) size;
        List.iteri
          (fun idx value ->
            Format.fprintf fmt "| %d -> %a\n" idx format_value value)
          (Array.to_list values)

  type print_ctx = { indent : int; is_newline : bool }

  type ctx = {
    ctx_tgv : var_value Array.t;
    mutable ctx_tmps : var_value Array.t list;
    ctx_it : Mir.Var.t StrMap.t;
    ctx_pr_out : print_ctx;
    ctx_pr_err : print_ctx;
    ctx_anos : (Com.Error.t * string option) list;
    ctx_old_anos : StrSet.t;
    ctx_nb_anos : int;
    ctx_nb_discos : int;
    ctx_nb_infos : int;
    ctx_nb_bloquantes : int;
    ctx_finalized_anos : (Com.Error.t * string option) list;
    ctx_exported_anos : (Com.Error.t * string option) list;
  }

  let empty_ctx (p : Mir.program) : ctx =
    let ctx_tgv = Array.make p.program_stats.sz_vars (SimpleVar Undefined) in
    StrMap.iter
      (fun _ (var : Mir.Var.t) ->
        match var.is_table with
        | Some sz ->
            ctx_tgv.(Mir.Var.loc_int var) <-
              TableVar (sz, Array.make sz Undefined)
        | None -> ())
      p.program_vars;
    {
      ctx_tgv;
      ctx_tmps = [];
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

  let literal_to_value (l : Com.literal) : value =
    match l with
    | Com.Undefined -> Undefined
    | Com.Float f -> Number (N.of_float f)

  let var_literal_to_var_value (def : var_literal) : var_value =
    match def with
    | SimpleVar v -> SimpleVar (literal_to_value v)
    | TableVar (size, defs) ->
        TableVar (size, Array.map (fun v -> literal_to_value v) defs)

  let value_to_literal (l : value) : Com.literal =
    match l with
    | Undefined -> Com.Undefined
    | Number f -> Com.Float (N.to_float f)

  let var_value_to_var_literal (def : var_value) : var_literal =
    let l : var_literal =
      match def with
      | SimpleVar v -> SimpleVar (value_to_literal v)
      | TableVar (size, defs) ->
          TableVar (size, Array.map (fun v -> value_to_literal v) defs)
    in
    l

  let update_ctx_with_inputs (ctx : ctx)
      (inputs : Com.literal Mir.VariableMap.t) : ctx =
    let value_inputs =
      Mir.VariableMap.mapi
        (fun v l ->
          match l with
          | Com.Undefined -> Undefined
          | Com.Float f -> Number (N.of_float_input v f))
        inputs
    in
    Mir.VariableMap.iter
      (fun (var : Mir.Var.t) value ->
        ctx.ctx_tgv.(Mir.Var.loc_int var) <- SimpleVar value)
      value_inputs;
    ctx

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

  let compare_numbers op i1 i2 =
    let epsilon = N.of_float !Cli.comparison_error_margin in
    let open Com in
    match op with
    | Gt -> N.(i1 >. i2 +. epsilon)
    | Gte -> N.(i1 >. i2 -. epsilon)
    | Lt -> N.(i1 +. epsilon <. i2)
    | Lte -> N.(i1 -. epsilon <. i2)
    | Eq -> N.(N.abs (i1 -. i2) <. epsilon)
    | Neq -> N.(N.abs (i1 -. i2) >=. epsilon)

  let get_var_value ctx (var : Mir.Var.t) =
    let var =
      match StrMap.find_opt var.Mir.Var.id ctx.ctx_it with
      | Some mvar -> mvar
      | None -> var
    in
    let tab =
      if Mir.Var.is_temp var then List.hd ctx.ctx_tmps else ctx.ctx_tgv
    in
    match tab.(Mir.Var.loc_int var) with
    | SimpleVar e -> e
    | TableVar (size, values) -> if size > 0 then values.(0) else Undefined

  let get_var_tab ctx var idx =
    match idx with
    | Undefined -> Undefined
    | Number f -> (
        let var = Pos.unmark var in
        let var =
          match StrMap.find_opt var.Mir.Var.id ctx.ctx_it with
          | Some mvar -> mvar
          | None -> var
        in
        let tab =
          if Mir.Var.is_temp var then List.hd ctx.ctx_tmps else ctx.ctx_tgv
        in
        let idx_f = roundf f in
        match tab.(Mir.Var.loc_int var) with
        | SimpleVar e ->
            if N.(idx_f >=. N.of_int (Int64.of_int 1)) then Undefined
            else if N.(idx_f <. N.zero ()) then Number (N.zero ())
            else e
        | TableVar (size, values) ->
            if N.(idx_f >=. N.of_int (Int64.of_int size)) then Undefined
            else if N.(idx_f <. N.zero ()) then Number (N.zero ())
            else
              let i = Int64.to_int (N.to_int idx_f) in
              let vt =
                match tab.(Mir.Var.loc_int var + i) with
                | SimpleVar vt -> vt
                | TableVar (_, t) -> if i = 0 then values.(0) else assert false
              in
              let ve = values.(i) in
              (match (vt, ve) with
              | Undefined, Undefined -> ()
              | Number z0, Number z1 when z0 = z1 -> ()
              | _ -> assert false);
              ve)

  let rec evaluate_expr (ctx : ctx) (p : Mir.program)
      (e : Mir.expression Pos.marked) : value =
    let comparison op new_e1 new_e2 =
      match (op, new_e1, new_e2) with
      | Com.Gt, _, Undefined | Com.Gt, Undefined, _ -> Undefined
      | Com.Gte, _, Undefined | Com.Gte, Undefined, _ -> Undefined
      | Com.Lt, _, Undefined | Com.Lt, Undefined, _ -> Undefined
      | Com.Lte, _, Undefined | Com.Lte, Undefined, _ -> Undefined
      | Com.Eq, _, Undefined | Com.Eq, Undefined, _ -> Undefined
      | Com.Neq, _, Undefined | Com.Neq, Undefined, _ -> Undefined
      | op, Number i1, Number i2 ->
          Number (real_of_bool (compare_numbers op i1 i2))
    in
    let unop op new_e1 =
      let open Com in
      match (op, new_e1) with
      | Not, Number b1 -> Number (real_of_bool (not (bool_of_real b1)))
      | Minus, Number f1 -> Number N.(zero () -. f1)
      | Not, Undefined -> Undefined
      | Minus, Undefined -> Undefined
    in
    let binop op new_e1 new_e2 =
      let open Com in
      match (op, new_e1, new_e2) with
      | Add, Number i1, Number i2 -> Number N.(i1 +. i2)
      | Add, Number i1, Undefined -> Number N.(i1 +. zero ())
      | Add, Undefined, Number i2 -> Number N.(zero () +. i2)
      | Add, Undefined, Undefined -> Undefined
      | Sub, Number i1, Number i2 -> Number N.(i1 -. i2)
      | Sub, Number i1, Undefined -> Number N.(i1 -. zero ())
      | Sub, Undefined, Number i2 -> Number N.(zero () -. i2)
      | Sub, Undefined, Undefined -> Undefined
      | Mul, _, Undefined | Mul, Undefined, _ -> Undefined
      | Mul, Number i1, Number i2 -> Number N.(i1 *. i2)
      | Div, Undefined, _ | Div, _, Undefined -> Undefined (* yes... *)
      | Div, _, l2 when is_zero l2 -> Number (N.zero ())
      | Div, Number i1, Number i2 -> Number N.(i1 /. i2)
      | And, Undefined, _ | And, _, Undefined -> Undefined
      | Or, Undefined, Undefined -> Undefined
      | Or, Undefined, Number i | Or, Number i, Undefined -> Number i
      | And, Number i1, Number i2 ->
          Number (real_of_bool (bool_of_real i1 && bool_of_real i2))
      | Or, Number i1, Number i2 ->
          Number (real_of_bool (bool_of_real i1 || bool_of_real i2))
    in
    let out =
      try
        match Pos.unmark e with
        | Com.TestInSet (positive, e0, values) ->
            let new_e0 = evaluate_expr ctx p e0 in
            let or_chain =
              List.fold_left
                (fun or_chain set_value ->
                  let equal_test =
                    match set_value with
                    | Com.VarValue set_var ->
                        let new_set_var =
                          get_var_value ctx (Pos.unmark set_var)
                        in
                        comparison Com.Eq new_e0 new_set_var
                    | Com.FloatValue i ->
                        let val_i = Number (N.of_float (Pos.unmark i)) in
                        comparison Com.Eq new_e0 val_i
                    | Com.Interval (bn, en) ->
                        let val_bn =
                          Number (N.of_float (float_of_int (Pos.unmark bn)))
                        in
                        let val_en =
                          Number (N.of_float (float_of_int (Pos.unmark en)))
                        in
                        binop Com.And
                          (comparison Com.Gte new_e0 val_bn)
                          (comparison Com.Lte new_e0 val_en)
                  in
                  binop Com.Or or_chain equal_test)
                Undefined values
            in
            if positive then or_chain else unop Com.Not or_chain
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
        | Conditional (e1, e2, e3_opt) -> (
            let new_e1 = evaluate_expr ctx p e1 in
            match new_e1 with
            | Number z when N.(z =. zero ()) -> (
                match e3_opt with
                | None -> Undefined
                | Some e3 -> evaluate_expr ctx p e3)
            | Number _ -> evaluate_expr ctx p e2 (* the float is not zero *)
            | Undefined -> Undefined)
        | Literal Undefined -> Undefined
        | Literal (Float f) -> Number (N.of_float f)
        | Index (var, e1) ->
            let idx = evaluate_expr ctx p e1 in
            get_var_tab ctx var idx
        | Var var -> get_var_value ctx var
        | FuncCall ((ArrFunc, _), [ arg ]) -> (
            let new_arg = evaluate_expr ctx p arg in
            match new_arg with
            | Number x -> Number (roundf x)
            | Undefined -> Undefined
            (*nope:Float 0.*))
        | FuncCall ((InfFunc, _), [ arg ]) -> (
            let new_arg = evaluate_expr ctx p arg in
            match new_arg with
            | Number x -> Number (truncatef x)
            | Undefined -> Undefined
            (*Float 0.*))
        | FuncCall ((PresentFunc, _), [ arg ]) -> (
            match evaluate_expr ctx p arg with
            | Undefined -> false_value ()
            | _ -> true_value ())
        | FuncCall ((Supzero, _), [ arg ]) -> (
            match evaluate_expr ctx p arg with
            | Undefined -> Undefined
            | Number f as n ->
                if compare_numbers Com.Lte f (N.zero ()) then Undefined else n)
        | FuncCall ((AbsFunc, _), [ arg ]) -> (
            match evaluate_expr ctx p arg with
            | Undefined -> Undefined
            | Number f -> Number (N.abs f))
        | FuncCall ((MinFunc, _), [ arg1; arg2 ]) -> (
            match (evaluate_expr ctx p arg1, evaluate_expr ctx p arg2) with
            | Undefined, Undefined -> Undefined
            | Undefined, Number f | Number f, Undefined ->
                Number (N.min (N.zero ()) f)
            | Number fl, Number fr -> Number (N.min fl fr))
        | FuncCall ((MaxFunc, _), [ arg1; arg2 ]) -> (
            match (evaluate_expr ctx p arg1, evaluate_expr ctx p arg2) with
            | Undefined, Undefined -> Undefined
            | Undefined, Number f | Number f, Undefined ->
                Number (N.max (N.zero ()) f)
            | Number fl, Number fr -> Number (N.max fl fr))
        | FuncCall ((Multimax, _), [ arg1; arg2 ]) -> (
            match evaluate_expr ctx p arg1 with
            | Undefined -> Undefined
            | Number f -> (
                let up = N.to_int (roundf f) in
                let var_arg2 =
                  match Pos.unmark arg2 with
                  | Var v -> (v, Pos.get_position e)
                  | _ -> assert false
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
        | FuncCall (_func, _) -> assert false
        | Attribut (var, a) -> (
            match StrMap.find_opt (Pos.unmark var).id ctx.ctx_it with
            | Some mvar -> (
                match StrMap.find_opt (Pos.unmark a) (Mir.Var.attrs mvar) with
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
        | FuncCallLoop _ | Loop _ -> assert false
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

  let set_var_value (ctx : ctx) (var : Mir.Var.t) (i_opt : (int * value) option)
      (value : value) : unit =
    let tab =
      if Mir.Var.is_temp var then List.hd ctx.ctx_tmps else ctx.ctx_tgv
    in
    match i_opt with
    | None -> (
        (*  let value = evaluate_expr ctx p.mir_program vexpr in*)
        match var.is_table with
        | None ->
            let res = SimpleVar value in
            tab.(Mir.Var.loc_int var) <- res
        | Some _ -> (
            match tab.(Mir.Var.loc_int var) with
            | SimpleVar _ -> assert false
            | TableVar (size, t) ->
                for i = 0 to size - 1 do
                  if i > 0 then tab.(Mir.Var.loc_int var + i) <- SimpleVar value;
                  t.(i) <- value
                done))
    | Some (_, Undefined) -> ()
    | Some (_, Number f) -> (
        let i' = int_of_float (N.to_float f) in
        match var.is_table with
        | None ->
            if i' = 0 then
              (*  let value = evaluate_expr ctx p.mir_program vexpr in*)
              tab.(Mir.Var.loc_int var) <- SimpleVar value
        | Some sz ->
            let t =
              match tab.(Mir.Var.loc_int var) with
              | SimpleVar _ -> assert false
              | TableVar (_, t) -> t
            in
            if 0 <= i' && i' < sz then (
              (*  let value = evaluate_expr ctx p.mir_program vexpr in*)
              if i' > 0 then tab.(Mir.Var.loc_int var + i') <- SimpleVar value;
              t.(i') <- value))

  exception BlockingError of ctx

  let rec evaluate_stmt (canBlock : bool) (p : Bir.program) (ctx : ctx)
      (stmt : Mir.m_instruction) (loc : code_location) =
    match Pos.unmark stmt with
    | Com.Affectation (var, vidx_opt, vexpr) ->
        let var =
          match StrMap.find_opt var.id ctx.ctx_it with
          | Some mvar -> mvar
          | None -> var
        in
        let idx_opt =
          match vidx_opt with
          | None -> None
          | Some (sz, ei) -> Some (sz, evaluate_expr ctx p.mir_program ei)
        in
        let value = evaluate_expr ctx p.mir_program vexpr in
        set_var_value ctx var idx_opt value;
        (*        !assign_hook var (fun _ -> var_value_to_var_literal res) loc;*)
        ctx
    | Com.IfThenElse (b, t, f) -> (
        match evaluate_simple_variable p ctx (Pos.unmark b) with
        | SimpleVar (Number z) when N.(z =. zero ()) ->
            evaluate_stmts canBlock p ctx f (ConditionalBranch false :: loc) 0
        | SimpleVar (Number _) ->
            evaluate_stmts canBlock p ctx t (ConditionalBranch true :: loc) 0
        | SimpleVar Undefined -> ctx
        | _ -> assert false)
    | Com.VerifBlock stmts ->
        evaluate_stmts true p ctx stmts (InsideBlock 0 :: loc) 0
    | Com.ComputeTarget (f, _args) ->
        let tf = Mir.TargetMap.find f p.targets in
        evaluate_target canBlock p ctx loc f tf
    | Com.Print (std, args) -> (
        let std_fmt, ctx_pr =
          match std with
          | Com.StdOut -> (Format.std_formatter, ctx.ctx_pr_out)
          | Com.StdErr -> (Format.err_formatter, ctx.ctx_pr_err)
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
            (fun ctx_pr (arg : Mir.Var.t Com.print_arg Pos.marked) ->
              match Pos.unmark arg with
              | PrintString s -> pr_raw ctx_pr s
              | PrintName (var, _) -> (
                  match StrMap.find_opt var.Mir.Var.id ctx.ctx_it with
                  | Some mvar -> pr_raw ctx_pr (Pos.unmark mvar.name)
                  | None -> assert false)
              | PrintAlias (var, _) -> (
                  match StrMap.find_opt var.Mir.Var.id ctx.ctx_it with
                  | Some mvar -> pr_raw ctx_pr (Mir.Var.alias_str mvar)
                  | None -> assert false)
              | PrintIndent e ->
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
              | PrintExpr (e, mi, ma) ->
                  let var_value =
                    evaluate_simple_variable p ctx (Pos.unmark e)
                  in
                  let ctx_pr = pr_indent ctx_pr in
                  format_var_value_prec mi ma std_fmt var_value;
                  ctx_pr)
            ctx_pr args
        in
        match std with
        | Com.StdOut -> { ctx with ctx_pr_out = ctx_pr }
        | Com.StdErr -> { ctx with ctx_pr_err = ctx_pr })
    | Com.Iterate (var, vcs, expr, stmts) ->
        let eval vc ctx =
          StrMap.fold
            (fun _ v ctx ->
              if Mir.Var.cat v = Some vc then
                let ctx =
                  { ctx with ctx_it = StrMap.add var.id v ctx.ctx_it }
                in
                match evaluate_simple_variable p ctx (Pos.unmark expr) with
                | SimpleVar (Number z) when N.(z =. one ()) ->
                    evaluate_stmts canBlock p ctx stmts
                      (ConditionalBranch true :: loc)
                      0
                | SimpleVar _ -> ctx
                | _ -> assert false
              else ctx)
            p.Bir.mir_program.program_vars ctx
        in
        Com.CatVarSet.fold eval vcs ctx
    | Com.Restore (vars, var_params, stmts) ->
        let backup =
          List.fold_left
            (fun backup (v : Mir.Var.t) ->
              let v =
                match StrMap.find_opt v.Mir.Var.id ctx.ctx_it with
                | None -> v
                | Some v -> v
              in
              let tab =
                if Mir.Var.is_temp v then List.hd ctx.ctx_tmps else ctx.ctx_tgv
              in
              let sz = match v.is_table with None -> 1 | Some sz -> sz in
              let rec aux backup i =
                if i = sz then backup
                else
                  let value =
                    match tab.(Mir.Var.loc_int v) with
                    | SimpleVar value -> value
                    | TableVar (_, t) -> t.(i)
                  in
                  aux ((v, i, value) :: backup) (i + 1)
              in
              aux backup 0)
            [] vars
        in
        let backup =
          List.fold_left
            (fun backup ((var : Mir.Var.t), vcs, expr) ->
              Com.CatVarSet.fold
                (fun vc backup ->
                  StrMap.fold
                    (fun _ v backup ->
                      if Mir.Var.cat v = Some vc then
                        let ctx =
                          { ctx with ctx_it = StrMap.add var.id v ctx.ctx_it }
                        in
                        match
                          evaluate_simple_variable p ctx (Pos.unmark expr)
                        with
                        | SimpleVar (Number z) when N.(z =. one ()) ->
                            let tab =
                              if Mir.Var.is_temp v then List.hd ctx.ctx_tmps
                              else ctx.ctx_tgv
                            in
                            let sz =
                              match v.is_table with None -> 1 | Some sz -> sz
                            in
                            let rec aux backup i =
                              if i = sz then backup
                              else
                                let value =
                                  match tab.(Mir.Var.loc_int v) with
                                  | SimpleVar value -> value
                                  | TableVar (_, t) -> t.(i)
                                in
                                aux ((v, i, value) :: backup) (i + 1)
                            in
                            aux backup 0
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
        List.iter
          (fun ((v : Mir.Var.t), i, value) ->
            let tab =
              if Mir.Var.is_temp v then List.hd ctx.ctx_tmps else ctx.ctx_tgv
            in
            match tab.(Mir.Var.loc_int v) with
            | SimpleVar _ -> tab.(Mir.Var.loc_int v) <- SimpleVar value
            | TableVar (_, t) ->
                if i > 0 then tab.(Mir.Var.loc_int v + i) <- SimpleVar value;
                t.(i) <- value)
          backup;
        ctx
    | Com.RaiseError (err, var_opt) ->
        let ctx_nb_anos =
          if err.typ = Com.Error.Anomaly then ctx.ctx_nb_anos + 1
          else ctx.ctx_nb_anos
        in
        let ctx_nb_discos =
          if err.typ = Com.Error.Discordance then ctx.ctx_nb_discos + 1
          else ctx.ctx_nb_discos
        in
        let ctx_nb_infos =
          if err.typ = Com.Error.Information then ctx.ctx_nb_infos + 1
          else ctx.ctx_nb_infos
        in
        let ctx_nb_bloquantes, is_blocking =
          let is_b =
            err.typ = Com.Error.Anomaly && Pos.unmark err.isisf = "N"
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
    | Com.CleanErrors ->
        (*Format.eprintf "nettoie erreurs\n";*)
        {
          ctx with
          ctx_anos = [];
          ctx_nb_anos = 0;
          ctx_nb_discos = 0;
          ctx_nb_infos = 0;
          ctx_nb_bloquantes = 0;
        }
    | Com.FinalizeErrors ->
        let not_in_old_anos (err, _) =
          let name = Pos.unmark err.Com.Error.name in
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
        let add_ano res (err, _) =
          StrSet.add (Pos.unmark err.Com.Error.name) res
        in
        let ctx_old_anos =
          List.fold_left add_ano ctx.ctx_old_anos ctx.ctx_anos
        in
        { ctx with ctx_finalized_anos; ctx_old_anos }
    | Com.ExportErrors ->
        let ctx_exported_anos =
          ctx.ctx_exported_anos @ ctx.ctx_finalized_anos
        in
        (* List.iter (fun (err, _) -> Format.eprintf "sortie: %s\n" (Pos.unmark
           err.Mir.name)) ctx.ctx_finalized_anos;*)
        { ctx with ctx_exported_anos; ctx_finalized_anos = [] }

  and evaluate_stmts canBlock (p : Bir.program) (ctx : ctx)
      (stmts : Mir.m_instruction list) (loc : code_location) (start_value : int)
      : ctx =
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
      (loc : code_location) (_tn : string) (tf : Mir.target_data) =
    let env = Array.make tf.target_sz_vars (SimpleVar Undefined) in
    ignore
      (StrMap.fold
         (fun _ (_, _, size) n ->
           match size with
           | None -> n + 1
           | Some sz ->
               let values = Array.init sz (fun _ -> Undefined) in
               env.(n) <- TableVar (sz, values);
               n + 1)
         tf.target_tmp_vars 0);
    ctx.ctx_tmps <- env :: ctx.ctx_tmps;
    let ctx = evaluate_stmts canBlock p ctx tf.target_prog loc 0 in
    ctx.ctx_tmps <- List.tl ctx.ctx_tmps;
    ctx

  let evaluate_program (p : Bir.program) (ctx : ctx)
      (code_loc_start_value : int) : ctx =
    try
      let main_target =
        match Mir.TargetMap.find_opt p.main_function p.targets with
        | Some t -> t
        | None ->
            Errors.raise_error "Unable to find main function of Bir program"
      in
      let loc = [ InsideBlock code_loc_start_value ] in
      let ctx = evaluate_target false p ctx loc p.main_function main_target in
      evaluate_stmt false p ctx (Com.ExportErrors, Pos.no_pos) loc
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

let evaluate_program (p : Bir.program) (inputs : Com.literal Mir.VariableMap.t)
    (sort : Cli.value_sort) (roundops : Cli.round_ops) :
    float option StrMap.t * StrSet.t =
  prepare_interp sort roundops;
  let module Interp = (val get_interp sort roundops : S) in
  let ctx =
    Interp.update_ctx_with_inputs (Interp.empty_ctx p.mir_program) inputs
  in
  let ctx = Interp.evaluate_program p ctx 0 in
  let varMap =
    let fold name (var : Mir.Var.t) res =
      let fVal =
        match ctx.ctx_tgv.(Mir.Var.loc_int var) with
        | Interp.SimpleVar litt -> (
            match Interp.value_to_literal litt with
            | Com.Float f -> Some f
            | Com.Undefined -> None)
        | _ -> None
      in
      StrMap.add name fVal res
    in
    StrMap.fold fold p.mir_program.program_vars StrMap.empty
  in
  let anoSet =
    let fold res (e, _) = StrSet.add (Pos.unmark e.Com.Error.name) res in
    List.fold_left fold StrSet.empty ctx.ctx_exported_anos
  in
  (varMap, anoSet)

let evaluate_expr (p : Mir.program) (e : Mir.expression Pos.marked)
    (sort : Cli.value_sort) (roundops : Cli.round_ops) : Com.literal =
  let module Interp = (val get_interp sort roundops : S) in
  Interp.value_to_literal (Interp.evaluate_expr (Interp.empty_ctx p) p e)
