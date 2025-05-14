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

let exit_on_rte = ref true

let repl_debug = ref false

module type S = sig
  type custom_float

  type value = Number of custom_float | Undefined

  val format_value : Format.formatter -> value -> unit

  val format_value_prec : int -> int -> Format.formatter -> value -> unit

  type print_ctx = { mutable indent : int; mutable is_newline : bool }

  type ctx = {
    ctx_tgv : value Array.t;
    ctx_tmps : value Array.t;
    mutable ctx_tmps_org : int;
    ctx_ref : (Com.Var.t * int) Array.t;
    mutable ctx_ref_org : int;
    mutable ctx_args : value Array.t list;
    mutable ctx_res : value list;
    ctx_pr_out : print_ctx;
    ctx_pr_err : print_ctx;
    mutable ctx_anos : (Com.Error.t * string option) list;
    mutable ctx_old_anos : StrSet.t;
    mutable ctx_nb_anos : int;
    mutable ctx_nb_discos : int;
    mutable ctx_nb_infos : int;
    mutable ctx_nb_bloquantes : int;
    mutable ctx_finalized_anos : (Com.Error.t * string option) list;
    mutable ctx_exported_anos : (Com.Error.t * string option) list;
  }

  val empty_ctx : Mir.program -> ctx

  val literal_to_value : Com.literal -> value

  val value_to_literal : value -> Com.literal

  val update_ctx_with_inputs : ctx -> Com.literal Com.Var.Map.t -> unit

  type run_error =
    | NanOrInf of string * Mir.expression Pos.marked
    | StructuredError of
        (string * (string option * Pos.t) list * (unit -> unit) option)

  exception RuntimeError of run_error * ctx

  val raise_runtime_as_structured : run_error -> 'a

  val compare_numbers : Com.comp_op -> custom_float -> custom_float -> bool

  val evaluate_expr : ctx -> Mir.program -> Mir.expression Pos.marked -> value

  val evaluate_program : Mir.program -> ctx -> unit
end

module Make (N : Mir_number.NumberInterface) (RF : Mir_roundops.RoundOpsFunctor) =
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

  type print_ctx = { mutable indent : int; mutable is_newline : bool }

  type ctx = {
    ctx_tgv : value Array.t;
    ctx_tmps : value Array.t;
    mutable ctx_tmps_org : int;
    ctx_ref : (Com.Var.t * int) Array.t;
    mutable ctx_ref_org : int;
    mutable ctx_args : value Array.t list;
    mutable ctx_res : value list;
    ctx_pr_out : print_ctx;
    ctx_pr_err : print_ctx;
    mutable ctx_anos : (Com.Error.t * string option) list;
    mutable ctx_old_anos : StrSet.t;
    mutable ctx_nb_anos : int;
    mutable ctx_nb_discos : int;
    mutable ctx_nb_infos : int;
    mutable ctx_nb_bloquantes : int;
    mutable ctx_finalized_anos : (Com.Error.t * string option) list;
    mutable ctx_exported_anos : (Com.Error.t * string option) list;
  }

  let empty_ctx (p : Mir.program) : ctx =
    let dummy_ref =
      (Com.Var.new_ref ~name:("", Pos.no_pos) ~loc_int:(-1), -1)
    in
    {
      ctx_tgv = Array.make p.program_stats.sz_vars Undefined;
      ctx_tmps = Array.make p.program_stats.sz_all_tmps Undefined;
      ctx_tmps_org = 0;
      ctx_ref = Array.make p.program_stats.nb_all_refs dummy_ref;
      ctx_ref_org = 0;
      ctx_args = [];
      ctx_res = [];
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

  let value_to_literal (l : value) : Com.literal =
    match l with
    | Undefined -> Com.Undefined
    | Number f -> Com.Float (N.to_float f)

  let update_ctx_with_inputs (ctx : ctx) (inputs : Com.literal Com.Var.Map.t) :
      unit =
    let value_inputs =
      Com.Var.Map.map
        (fun l ->
          match l with
          | Com.Undefined -> Undefined
          | Com.Float f -> Number (N.of_float_input f))
        inputs
    in
    Com.Var.Map.iter
      (fun (var : Com.Var.t) value ->
        ctx.ctx_tgv.(Com.Var.loc_int var) <- value)
      value_inputs

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

  let get_var ctx (var : Com.Var.t) =
    match var.loc with
    | LocRef (_, i) -> ctx.ctx_ref.(ctx.ctx_ref_org + i)
    | LocTgv (_, { loc_int; _ }) -> (var, loc_int)
    | LocTmp (_, i) -> (var, ctx.ctx_tmps_org + i)
    | LocArg (_, i) -> (var, i)
    | LocRes _ -> (var, -1)

  let get_var_value ctx (var : Com.Var.t) (i : int) =
    let var, vi = get_var ctx var in
    match var.scope with
    | Com.Var.Tgv _ -> ctx.ctx_tgv.(vi + i)
    | Com.Var.Temp _ -> ctx.ctx_tmps.(vi + i)
    | Com.Var.Ref -> assert false
    | Com.Var.Arg -> (List.hd ctx.ctx_args).(vi)
    | Com.Var.Res -> List.hd ctx.ctx_res

  let get_var_tab ctx var idx =
    match idx with
    | Undefined -> Undefined
    | Number f ->
        let var, _vi = get_var ctx (Pos.unmark var) in
        let idx_f = roundf f in
        let sz = Com.Var.size var in
        if N.(idx_f >=. N.of_int (Int64.of_int sz)) then Undefined
        else if N.(idx_f <. N.zero ()) then Number (N.zero ())
        else
          let i = Int64.to_int (N.to_int idx_f) in
          get_var_value ctx var i

  exception BlockingError

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
                          get_var_value ctx (Pos.unmark set_var) 0
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
        | Var var -> get_var_value ctx var 0
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
        | FuncCall ((Func fn, _), args) ->
            let fd = Com.TargetMap.find fn p.program_functions in
            let atab = Array.of_list (List.map (evaluate_expr ctx p) args) in
            ctx.ctx_args <- atab :: ctx.ctx_args;
            ctx.ctx_res <- Undefined :: ctx.ctx_res;
            evaluate_target false p ctx fn fd;
            ctx.ctx_args <- List.tl ctx.ctx_args;
            let res = List.hd ctx.ctx_res in
            ctx.ctx_res <- List.tl ctx.ctx_res;
            res
        | FuncCall (_, _) -> assert false
        | Attribut (var, a) -> (
            let var, _ = get_var ctx (Pos.unmark var) in
            match StrMap.find_opt (Pos.unmark a) (Com.Var.attrs var) with
            | Some l -> Number (N.of_float (float (Pos.unmark l)))
            | None -> Undefined)
        | Size var -> (
            let var, _ = get_var ctx (Pos.unmark var) in
            match Com.Var.is_table var with
            | Some i -> Number (N.of_float (float_of_int i))
            | None -> Number (N.of_float 1.0))
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

  and set_var_value (p : Mir.program) (ctx : ctx) ((var, vi) : Com.Var.t * int)
      (vexpr : Mir.expression Pos.marked) : unit =
    let value = evaluate_expr ctx p vexpr in
    match Com.Var.is_table var with
    | None -> (
        match var.scope with
        | Com.Var.Tgv _ -> ctx.ctx_tgv.(vi) <- value
        | Com.Var.Temp _ -> ctx.ctx_tmps.(vi) <- value
        | Com.Var.Ref -> assert false
        | Com.Var.Arg -> (List.hd ctx.ctx_args).(vi) <- value
        | Com.Var.Res -> ctx.ctx_res <- value :: List.tl ctx.ctx_res)
    | Some sz -> (
        match var.scope with
        | Com.Var.Tgv _ ->
            for i = 0 to sz - 1 do
              ctx.ctx_tgv.(vi + i) <- value
            done
        | Com.Var.Temp _ ->
            for i = 0 to sz - 1 do
              ctx.ctx_tmps.(vi + i) <- value
            done
        | Com.Var.Ref -> assert false
        | Com.Var.Arg -> (List.hd ctx.ctx_args).(vi) <- value
        | Com.Var.Res -> ctx.ctx_res <- value :: List.tl ctx.ctx_res)

  and set_var_value_tab (p : Mir.program) (ctx : ctx)
      ((var, vi) : Com.Var.t * int) (ei : Mir.expression Pos.marked)
      (vexpr : Mir.expression Pos.marked) : unit =
    match evaluate_expr ctx p ei with
    | Undefined -> ()
    | Number f -> (
        let i = int_of_float (N.to_float f) in
        let sz = Com.Var.size var in
        if 0 <= i && i < sz then
          let value = evaluate_expr ctx p vexpr in
          match var.scope with
          | Com.Var.Tgv _ -> ctx.ctx_tgv.(vi + i) <- value
          | Com.Var.Temp _ -> ctx.ctx_tmps.(vi + i) <- value
          | Com.Var.Ref -> assert false
          | Com.Var.Arg -> (List.hd ctx.ctx_args).(vi) <- value
          | Com.Var.Res -> ctx.ctx_res <- value :: List.tl ctx.ctx_res)

  and evaluate_stmt (canBlock : bool) (p : Mir.program) (ctx : ctx)
      (stmt : Mir.m_instruction) : unit =
    match Pos.unmark stmt with
    | Com.Affectation (Com.SingleFormula (m_var, vidx_opt, vexpr), _) -> (
        let vari = get_var ctx (Pos.unmark m_var) in
        match vidx_opt with
        | None -> set_var_value p ctx vari vexpr
        | Some ei -> set_var_value_tab p ctx vari ei vexpr)
    | Com.Affectation _ -> assert false
    | Com.IfThenElse (b, t, f) -> (
        match evaluate_expr ctx p b with
        | Number z when N.(z =. zero ()) -> evaluate_stmts canBlock p ctx f
        | Number _ -> evaluate_stmts canBlock p ctx t
        | Undefined -> ())
    | Com.WhenDoElse (wdl, ed) ->
        let rec aux = function
          | (expr, dl, _) :: l -> (
              match evaluate_expr ctx p expr with
              | Number z when N.(z =. zero ()) ->
                  evaluate_stmts canBlock p ctx (Pos.unmark ed)
              | Number _ ->
                  evaluate_stmts canBlock p ctx dl;
                  aux l
              | Undefined -> aux l)
          | [] -> ()
        in
        aux wdl
    | Com.VerifBlock stmts -> evaluate_stmts true p ctx stmts
    | Com.ComputeTarget ((tn, _), args) ->
        let tf = Com.TargetMap.find tn p.program_targets in
        let rec set_args n = function
          | [] -> ()
          | m_a :: al' ->
              let a = m_a |> Pos.unmark |> get_var ctx in
              ctx.ctx_ref.(ctx.ctx_ref_org + n) <- a;
              set_args (n + 1) al'
        in
        set_args 0 args;
        evaluate_target canBlock p ctx tn tf
    | Com.Print (std, args) -> begin
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
            ctx_pr.is_newline <- false)
        in
        let pr_raw ctx_pr s =
          let len = String.length s in
          let rec aux = function
            | n when n >= len -> ()
            | n -> (
                match s.[n] with
                | '\n' ->
                    Format.fprintf std_fmt "\n";
                    ctx_pr.is_newline <- true;
                    aux (n + 1)
                | c ->
                    pr_indent ctx_pr;
                    Format.fprintf std_fmt "%c" c;
                    aux (n + 1))
          in
          aux 0
        in
        List.iter
          (fun (arg : Com.Var.t Com.print_arg Pos.marked) ->
            match Pos.unmark arg with
            | PrintString s -> pr_raw ctx_pr s
            | PrintName (var, _) ->
                let var, _ = get_var ctx var in
                pr_raw ctx_pr (Pos.unmark var.name)
            | PrintAlias (var, _) ->
                let var, _ = get_var ctx var in
                pr_raw ctx_pr (Com.Var.alias_str var)
            | PrintIndent e ->
                let diff =
                  match evaluate_expr ctx p e with
                  | Undefined -> 0
                  | Number x -> Int64.to_int (N.to_int (roundf x))
                in
                ctx_pr.indent <- max 0 (ctx_pr.indent + diff)
            | PrintExpr (e, mi, ma) ->
                let value = evaluate_expr ctx p e in
                pr_indent ctx_pr;
                format_value_prec mi ma std_fmt value)
          args;
        match std with
        | Com.StdOut -> ()
        | Com.StdErr -> Format.pp_print_flush Format.err_formatter ()
      end
    | Com.Iterate ((m_var : Com.Var.t Pos.marked), vars, var_params, stmts) ->
        let var = Pos.unmark m_var in
        let var_i =
          match var.loc with LocRef (_, i) -> i | _ -> assert false
        in
        List.iter
          (fun (v, _) ->
            ctx.ctx_ref.(ctx.ctx_ref_org + var_i) <- get_var ctx v;
            evaluate_stmts canBlock p ctx stmts)
          vars;
        List.iter
          (fun (vcs, expr) ->
            let eval vc _ =
              StrMap.iter
                (fun _ v ->
                  if Com.CatVar.compare (Com.Var.cat v) vc = 0 then (
                    ctx.ctx_ref.(ctx.ctx_ref_org + var_i) <- get_var ctx v;
                    match evaluate_expr ctx p expr with
                    | Number z when N.(z =. one ()) ->
                        evaluate_stmts canBlock p ctx stmts
                    | _ -> ()))
                p.program_vars
            in
            Com.CatVar.Map.iter eval vcs)
          var_params
    | Com.Restore (vars, var_params, stmts) ->
        let backup =
          List.fold_left
            (fun backup (m_v : Com.Var.t Pos.marked) ->
              let v, vi = m_v |> Pos.unmark |> get_var ctx in
              let rec aux backup i =
                if i = Com.Var.size v then backup
                else
                  let value = get_var_value ctx v i in
                  aux ((v, vi + i, value) :: backup) (i + 1)
              in
              aux backup 0)
            [] vars
        in
        let backup =
          List.fold_left
            (fun backup ((m_var : Com.Var.t Pos.marked), vcs, expr) ->
              let var = Pos.unmark m_var in
              let var_i =
                match var.loc with LocRef (_, i) -> i | _ -> assert false
              in
              Com.CatVar.Map.fold
                (fun vc _ backup ->
                  StrMap.fold
                    (fun _ v backup ->
                      if Com.CatVar.compare (Com.Var.cat v) vc = 0 then (
                        let var, vi = get_var ctx v in
                        ctx.ctx_ref.(ctx.ctx_ref_org + var_i) <- (var, vi);
                        match evaluate_expr ctx p expr with
                        | Number z when N.(z =. one ()) ->
                            let rec aux backup i =
                              if i = Com.Var.size var then backup
                              else
                                let value = get_var_value ctx var i in
                                aux ((v, vi + i, value) :: backup) (i + 1)
                            in
                            aux backup 0
                        | _ -> backup)
                      else backup)
                    p.program_vars backup)
                vcs backup)
            backup var_params
        in
        evaluate_stmts canBlock p ctx stmts;
        List.iter
          (fun ((v : Com.Var.t), i, value) ->
            match v.scope with
            | Com.Var.Tgv _ -> ctx.ctx_tgv.(i) <- value
            | Com.Var.Temp _ -> ctx.ctx_tmps.(i) <- value
            | Com.Var.Ref -> assert false
            | Com.Var.Arg -> (List.hd ctx.ctx_args).(i) <- value
            | Com.Var.Res -> ctx.ctx_res <- value :: List.tl ctx.ctx_res)
          backup
    | Com.RaiseError (m_err, var_opt) ->
        let err = Pos.unmark m_err in
        (match err.typ with
        | Com.Error.Anomaly -> ctx.ctx_nb_anos <- ctx.ctx_nb_anos + 1
        | Com.Error.Discordance -> ctx.ctx_nb_discos <- ctx.ctx_nb_discos + 1
        | Com.Error.Information -> ctx.ctx_nb_infos <- ctx.ctx_nb_infos + 1);
        let is_blocking =
          err.typ = Com.Error.Anomaly && Pos.unmark err.is_isf = "N"
        in
        ctx.ctx_nb_bloquantes <-
          (ctx.ctx_nb_bloquantes + if is_blocking then 1 else 0);
        let v_opt = Option.map Pos.unmark var_opt in
        ctx.ctx_anos <- ctx.ctx_anos @ [ (err, v_opt) ];
        if is_blocking && ctx.ctx_nb_bloquantes >= 4 && canBlock then
          raise BlockingError
    | Com.CleanErrors ->
        ctx.ctx_anos <- [];
        ctx.ctx_nb_anos <- 0;
        ctx.ctx_nb_discos <- 0;
        ctx.ctx_nb_infos <- 0;
        ctx.ctx_nb_bloquantes <- 0
    | Com.FinalizeErrors ->
        let not_in_old_anos (err, _) =
          let name = Pos.unmark err.Com.Error.name in
          not (StrSet.mem name ctx.ctx_old_anos)
        in
        ctx.ctx_finalized_anos <-
          (let rec merge_anos old_anos new_anos =
             match (old_anos, new_anos) with
             | [], anos | anos, [] -> anos
             | _ :: old_tl, a :: new_tl -> a :: merge_anos old_tl new_tl
           in
           let new_anos = List.filter not_in_old_anos ctx.ctx_anos in
           merge_anos ctx.ctx_finalized_anos new_anos);
        let add_ano res (err, _) =
          StrSet.add (Pos.unmark err.Com.Error.name) res
        in
        ctx.ctx_old_anos <- List.fold_left add_ano ctx.ctx_old_anos ctx.ctx_anos
    | Com.ExportErrors ->
        ctx.ctx_exported_anos <- ctx.ctx_exported_anos @ ctx.ctx_finalized_anos;
        ctx.ctx_finalized_anos <- []
    | Com.ComputeDomain _ | Com.ComputeChaining _ | Com.ComputeVerifs _ ->
        assert false

  and evaluate_stmts canBlock (p : Mir.program) (ctx : ctx)
      (stmts : Mir.m_instruction list) : unit =
    try List.iter (evaluate_stmt canBlock p ctx) stmts
    with BlockingError as b_err -> if canBlock then raise b_err

  and evaluate_target canBlock (p : Mir.program) (ctx : ctx) (_tn : string)
      (tf : Mir.target_data) : unit =
    for i = 0 to tf.target_sz_tmps - 1 do
      ctx.ctx_tmps.(ctx.ctx_tmps_org + i) <- Undefined
    done;
    ctx.ctx_tmps_org <- ctx.ctx_tmps_org + tf.target_sz_tmps;
    ctx.ctx_ref_org <- ctx.ctx_ref_org + tf.target_nb_refs;
    evaluate_stmts canBlock p ctx tf.target_prog;
    ctx.ctx_ref_org <- ctx.ctx_ref_org - tf.target_nb_refs;
    ctx.ctx_tmps_org <- ctx.ctx_tmps_org - tf.target_sz_tmps

  let evaluate_program (p : Mir.program) (ctx : ctx) : unit =
    try
      let main_target =
        match
          Com.TargetMap.find_opt p.program_main_target p.program_targets
        with
        | Some t -> t
        | None ->
            Errors.raise_error "Unable to find main function of Bir program"
      in
      evaluate_target false p ctx p.program_main_target main_target;
      evaluate_stmt false p ctx (Com.ExportErrors, Pos.no_pos)
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
  Make (Mir_number.RegularFloatNumber) (Mir_roundops.DefaultRoundOps)
module FloatMultInterp =
  Make (Mir_number.RegularFloatNumber) (Mir_roundops.MultiRoundOps)
module FloatMfInterp =
  Make
    (Mir_number.RegularFloatNumber)
    (Mir_roundops.MainframeRoundOps (MainframeLongSize))
module MPFRDefInterp =
  Make (Mir_number.MPFRNumber) (Mir_roundops.DefaultRoundOps)
module MPFRMultInterp =
  Make (Mir_number.MPFRNumber) (Mir_roundops.MultiRoundOps)
module MPFRMfInterp =
  Make
    (Mir_number.MPFRNumber)
    (Mir_roundops.MainframeRoundOps (MainframeLongSize))
module BigIntDefInterp =
  Make
    (Mir_number.BigIntFixedPointNumber
       (BigIntPrecision))
       (Mir_roundops.DefaultRoundOps)
module BigIntMultInterp =
  Make
    (Mir_number.BigIntFixedPointNumber
       (BigIntPrecision))
       (Mir_roundops.MultiRoundOps)
module BigIntMfInterp =
  Make
    (Mir_number.BigIntFixedPointNumber
       (BigIntPrecision))
       (Mir_roundops.MainframeRoundOps (MainframeLongSize))
module IntvDefInterp =
  Make (Mir_number.IntervalNumber) (Mir_roundops.DefaultRoundOps)
module IntvMultInterp =
  Make (Mir_number.IntervalNumber) (Mir_roundops.MultiRoundOps)
module IntvMfInterp =
  Make
    (Mir_number.IntervalNumber)
    (Mir_roundops.MainframeRoundOps (MainframeLongSize))
module RatDefInterp =
  Make (Mir_number.RationalNumber) (Mir_roundops.DefaultRoundOps)
module RatMultInterp =
  Make (Mir_number.RationalNumber) (Mir_roundops.MultiRoundOps)
module RatMfInterp =
  Make
    (Mir_number.RationalNumber)
    (Mir_roundops.MainframeRoundOps (MainframeLongSize))

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

let evaluate_program (p : Mir.program) (inputs : Com.literal Com.Var.Map.t)
    (sort : Cli.value_sort) (roundops : Cli.round_ops) :
    Com.literal StrMap.t * StrSet.t =
  prepare_interp sort roundops;
  let module Interp = (val get_interp sort roundops : S) in
  let ctx = Interp.empty_ctx p in
  Interp.update_ctx_with_inputs ctx inputs;
  Interp.evaluate_program p ctx;
  let varMap =
    let fold name (var : Com.Var.t) res =
      if Com.Var.is_given_back var then
        let fVal =
          let litt = ctx.ctx_tgv.(Com.Var.loc_int var) in
          Interp.value_to_literal litt
        in
        StrMap.add name fVal res
      else res
    in
    StrMap.fold fold p.program_vars StrMap.empty
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
