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

open M_ir
open Types

exception Stop_instruction of Com.stop_kind

let exit_on_rte = ref true

let repl_debug = ref false

module type S = sig
  module N : Number.S

  module Tracer : Tracers.S

  type value = N.t Types.value

  type ctx = (N.t, Tracer.ctx) Context.t

  exception RuntimeError of Types.run_error * ctx

  val empty_ctx :
    ?dbg_info:Dbg_info.t ->
    ?inputs:Com.literal Com.Var.Map.t ->
    ?events:(Com.literal, Com.Var.t) Com.event_value StrMap.t list ->
    Mir.program ->
    ctx

  val format_value : Format.formatter -> value -> unit

  val format_value_prec : int -> int -> Format.formatter -> value -> unit

  val get_dbg_info : ctx -> Dbg_info.t option

  val raise_runtime_as_structured : Types.run_error -> 'a

  val evaluate_expr : ctx -> Mir.expression Pos.marked -> value

  val evaluate_program : ctx -> unit
end

module type PartialInterp = functor (_ : Tracers.S) -> S

module Make (N : Number.S) (Tracer : Tracers.S) :
  S with module N = N and module Tracer = Tracer = struct
  (* Careful : this behavior mimics the one imposed by the original Mlang
     compiler... *)

  module N = N
  module Tracer = Tracer
  module Fun = Functions.Make (N)

  type custom_float = N.t

  type tracer_ctx = Tracer.ctx

  type value = custom_float Types.value

  type ctx = (custom_float, tracer_ctx) Context.t

  module C = Context.Make (N) (Tracer)

  exception RuntimeError of Types.run_error * ctx

  let format_value (fmt : Format.formatter) (x : value) =
    match x with
    | Undefined -> Com.format_literal fmt Com.Undefined
    | Number x -> N.format_t fmt x

  let format_value_prec (mi : int) (ma : int) (fmt : Format.formatter)
      (x : value) =
    match x with
    | Undefined -> Com.format_literal fmt Com.Undefined
    | Number x -> N.format_prec_t mi ma fmt x

  let empty_ctx ?dbg_info ?inputs ?events (p : Mir.program) : ctx =
    C.empty_ctx ?dbg_info ?inputs ?events p

  let raise_runtime_as_structured (e : run_error) =
    match e with
    | NanOrInf (v, e) ->
        Errors.raise_spanned_error
          (Format.asprintf "Expression evaluated to %s: %a" v
             Format_mir.format_expression (Pos.unmark e))
          (Pos.get e)
    | StructuredError (msg, kont) -> raise @@ Errors.StructuredError (msg, kont)

  let is_zero (l : value) : bool =
    match l with Number z -> N.is_zero z | _ -> false

  let real_of_bool (b : bool) = if b then N.one () else N.zero ()

  let bool_of_real (f : N.t) : bool = not N.(f =. zero ())

  let compare_numbers op i1 i2 =
    let epsilon = N.of_float !Config.comparison_error_margin in
    let open Com in
    match op with
    | Gt -> N.(i1 >. i2 +. epsilon)
    | Gte -> N.(i1 >. i2 -. epsilon)
    | Lt -> N.(i1 +. epsilon <. i2)
    | Lte -> N.(i1 -. epsilon <. i2)
    | Eq -> N.(N.abs (i1 -. i2) <. epsilon)
    | Neq -> N.(N.abs (i1 -. i2) >=. epsilon)

  let mode_corr (ctx : ctx) =
    match StrMap.find_opt "MODE_CORR" ctx.ctx_prog.program_vars with
    | Some var -> (
        let vsd = ctx.ctx_prog.program_var_space_def in
        let _, var, vorg = C.get_var ctx None var in
        match C.get_var_value_org ctx vsd var vorg with
        | Undefined -> false
        | Number n -> compare_numbers Eq n (N.one ()))
    | None -> false

  exception BlockingError

  let comparison op new_e1 new_e2 =
    match (op, new_e1, new_e2) with
    | Com.(Gt | Gte | Lt | Lte | Eq | Neq), _, Undefined
    | Com.(Gt | Gte | Lt | Lte | Eq | Neq), Undefined, _ ->
        Undefined
    | op, Number i1, Number i2 ->
        Number (real_of_bool @@ compare_numbers op i1 i2)

  let unop op new_e1 =
    match (op, new_e1) with
    | Com.Not, Number b1 -> Number (real_of_bool (not (bool_of_real b1)))
    | Com.Minus, Number f1 -> Number N.(zero () -. f1)
    | Com.(Not | Minus), Undefined -> Undefined

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
    | Mod, Undefined, _ | Mod, _, Undefined -> Undefined (* yes... *)
    | Mod, _, l2 when is_zero l2 -> Number (N.zero ())
    | Mod, Number i1, Number i2 -> Number N.(i1 %. i2)
    | And, Undefined, _ | And, _, Undefined -> Undefined
    | Or, Undefined, Undefined -> Undefined
    | Or, Undefined, Number i | Or, Number i, Undefined -> Number i
    | And, Number i1, Number i2 ->
        Number (real_of_bool (bool_of_real i1 && bool_of_real i2))
    | Or, Number i1, Number i2 ->
        Number (real_of_bool (bool_of_real i1 || bool_of_real i2))

  (** Fails if the value is a nan or infinite. *)
  let fail_if_nan_or_inf ctx e = function
    | Number n when N.is_nan_or_inf n ->
        let e = NanOrInf (Format.asprintf "%a" N.format_t n, e) in
        if !exit_on_rte then raise_runtime_as_structured e
        else raise (RuntimeError (e, ctx))
    | _ -> ()

  let rec evaluate_switch_expr (ctx : ctx) s_e =
    match s_e with
    | Com.SEValue e -> (
        match evaluate_expr ctx e with
        | Undefined -> `Undefined
        | Number n -> `Value n)
    | SESameVariable v -> `Var v

  (* print aux *)

  and pr_string pctx s =
    Printer.string pctx s

  and pr_access ~ctx (pctx : Printer.t) info acc =
    match C.get_access_var ~eval:evaluate_expr ctx acc with
    | Some (vsd, var, _) ->
        let _, v, _ = C.get_var ctx None var in
        Printer.info pctx info vsd v;
        Printer.flush pctx
    | None -> ()

  and pr_indent ~ctx (pctx : Printer.t) e =
    match evaluate_expr ctx e with
    | Undefined -> ()
    | Number x ->
        let diff = Int64.to_int @@ N.to_int @@ N.roundf x in
        Printer.set_indent pctx diff

  and pr_expr ~ctx (pctx : Printer.t) (mi : int) ma e =
    e |> evaluate_expr ctx
    |> Pp.spr "%a" (format_value_prec mi ma)
    |> Printer.raw pctx;
    Printer.flush pctx

  (* end of print aux *)

  and same_variable ctx m_acc m_acc' : bool =
    let v0_opt = get_access_var ctx (Pos.unmark m_acc) in
    let v1_opt = get_access_var ctx (Pos.unmark m_acc') in
    match (v0_opt, v1_opt) with
    | Some (_, v0, _), Some (_, v1, _) ->
        Com.Var.name_str v0 = Com.Var.name_str v1
    | _, _ -> false

  (* Useful aliases *)

  and get_access_value ctx = C.get_access_value ~eval:evaluate_expr ctx

  and get_access_var ctx = C.get_access_var ~eval:evaluate_expr ctx

  and eval_m_index (ctx : ctx) m_i =
    match evaluate_expr ctx m_i with
    | Number z -> Int64.to_string @@ N.to_int z
    | Undefined -> "indefini"

  and set_access ctx acc vexpr =
    let value = evaluate_expr ctx vexpr in
    C.set_access ~eval:evaluate_expr ctx acc value;
    match C.get_access_var ~eval:evaluate_expr ctx acc with
    | None -> ()
    | Some (_, v, _) ->
        let value = N.to_literal value in
        Tracer.register_access ctx.tracer_ctx vexpr acc v
          ctx.ctx_prog.program_dict value (eval_m_index ctx)

  (* interpret *)

  and evaluate_fun_call ctx (f : Com.func Pos.marked)
      (args : Com.Var.t Com.m_expression list) =
    match (Pos.unmark f, args) with
    | ArrFunc, [ arg ] -> Fun.arr @@ evaluate_expr ctx arg
    | InfFunc, [ arg ] -> Fun.inf @@ evaluate_expr ctx arg
    | PresentFunc, [ arg ] -> Fun.present @@ evaluate_expr ctx arg
    | Supzero, [ arg ] -> Fun.supzero @@ evaluate_expr ctx arg
    | AbsFunc, [ arg ] -> Fun.abs @@ evaluate_expr ctx arg
    | MinFunc, [ a1; a2 ] ->
        Fun.min (evaluate_expr ctx a1) (evaluate_expr ctx a2)
    | MaxFunc, [ a1; a2 ] ->
        Fun.max (evaluate_expr ctx a1) (evaluate_expr ctx a2)
    | Multimax, [ a1; acc ] ->
        let a2 =
          match Pos.unmark acc with
          | Com.Var v -> begin
              match get_access_var ctx v with
              | None -> []
              | Some (vsd, var, vorg) ->
                  if Com.Var.is_table var then
                    List.map
                      (fun v -> C.get_var_value_org ctx vsd v vorg)
                      (C.get_vars_tab ctx var)
                  else [ C.get_var_value_org ctx vsd var vorg ]
            end
          | _ -> []
        in
        Fun.multimax (evaluate_expr ctx a1) a2
    | NbEvents, [] -> Fun.nb_events ctx
    | Func fn, args ->
        let fd = StrMap.find fn ctx.ctx_prog.program_functions in
        evaluate_function ctx fd args
    | ( ( ArrFunc | InfFunc | PresentFunc | Supzero | AbsFunc | MinFunc
        | MaxFunc | Multimax | NbEvents ),
        _ ) ->
        Errors.raise_error "arity error"
    | (SumFunc | GtzFunc | GtezFunc | NullFunc | VerifNumber | ComplNumber), _
      ->
        Errors.raise_error "not implemented"

  and evaluate_test_in_set ctx positive e0 values =
    let value0 = evaluate_expr ctx e0 in
    let or_chain =
      List.fold_left
        (fun or_chain set_value ->
          let equal_test =
            match set_value with
            | Com.VarValue (Pos.Mark (access, _)) ->
                let value = get_access_value ctx access in
                comparison Com.Eq value0 value
            | Com.FloatValue i ->
                let value_i = Number (N.of_float @@ Pos.unmark i) in
                comparison Com.Eq value0 value_i
            | Com.IntervalValue (bn, en) ->
                let value_bn =
                  Number (N.of_float @@ float_of_int @@ Pos.unmark bn)
                in
                let value_en =
                  Number (N.of_float @@ float_of_int @@ Pos.unmark en)
                in
                binop Com.And
                  (comparison Com.Gte value0 value_bn)
                  (comparison Com.Lte value0 value_en)
          in
          binop Com.Or or_chain equal_test)
        Undefined values
    in
    if positive then or_chain else unop Com.Not or_chain

  and evaluate_comparison ctx op e1 e2 =
    let value1 = evaluate_expr ctx e1 in
    let value2 = evaluate_expr ctx e2 in
    comparison (Pos.unmark op) value1 value2

  and evaluate_binop ctx op e1 e2 =
    let value1 = evaluate_expr ctx e1 in
    let value2 = evaluate_expr ctx e2 in
    binop (Pos.unmark op) value1 value2

  and evaluate_unop ctx op e = unop op @@ evaluate_expr ctx e

  and evaluate_conditional ctx cond th el =
    match evaluate_expr ctx cond with
    | Number z when N.(z =. zero ()) -> (
        match el with None -> Undefined | Some el -> evaluate_expr ctx el)
    | Number _ -> evaluate_expr ctx th
    | Undefined -> Undefined

  and evaluate_literal _ = function
    | Com.{ lit = Undefined; _ } -> Undefined
    | { lit = Float f; _ } -> Number (N.of_float f)

  and evaluate_attribut ctx m_acc a =
    match get_access_var ctx (Pos.unmark m_acc) with
    | Some (_, v, _) -> (
        match StrMap.find_opt (Pos.unmark a) (Com.Var.attrs v) with
        | Some l -> Number (N.of_float (float (Pos.unmark l)))
        | None -> Undefined)
    | None -> Undefined

  and evaluate_size ctx m_acc =
    match get_access_var ctx (Pos.unmark m_acc) with
    | Some (_, v, _) -> Number (N.of_float @@ float @@ Com.Var.size v)
    | None -> Undefined

  and evaluate_type ctx m_acc m_typ =
    match get_access_var ctx (Pos.unmark m_acc) with
    | Some (_, v, _) ->
        if Com.Var.is_tgv v && Com.Var.typ v = Some (Pos.unmark m_typ) then
          Number (N.one ())
        else Number (N.zero ())
    | None -> Undefined

  and evaluate_same_variable ctx m_acc0 m_acc1 =
    if same_variable ctx m_acc0 m_acc1 then Number (N.one ())
    else Number (N.zero ())

  and evaluate_in_domain ctx m_acc cvm =
    match get_access_var ctx (Pos.unmark m_acc) with
    | Some (_, v, _) ->
        if Com.Var.is_tgv v && Com.CatVar.Map.mem (Com.Var.cat v) cvm then
          Number (N.one ())
        else Number (N.zero ())
    | None -> Number (N.zero ())

  and evaluate_expr (ctx : ctx) (e : Mir.expression Pos.marked) : value =
    (* Format.eprintf {|"%a"@.|} (Com.format_expression Com.Var.pp) (Pos.unmark exp); *)
    let out =
      try
        match Pos.unmark e with
        | Com.TestInSet (positive, e0, values) ->
            evaluate_test_in_set ctx positive e0 values
        | Comparison (op, e1, e2) -> evaluate_comparison ctx op e1 e2
        | Binop (op, e1, e2) -> evaluate_binop ctx op e1 e2
        | Unop (op, e1) -> evaluate_unop ctx op e1
        | Conditional (e1, e2, e3_opt) -> evaluate_conditional ctx e1 e2 e3_opt
        | Literal l -> evaluate_literal ctx l
        | Var access -> get_access_value ctx access
        | FuncCall (f, args) -> evaluate_fun_call ctx f args
        | Attribut (m_acc, a) -> evaluate_attribut ctx m_acc a
        | Size m_acc -> evaluate_size ctx m_acc
        | Type (m_acc, m_typ) -> evaluate_type ctx m_acc m_typ
        | SameVariable (m_acc0, m_acc1) ->
            evaluate_same_variable ctx m_acc0 m_acc1
        | InDomain (m_acc, cvm) -> evaluate_in_domain ctx m_acc cvm
        | NbAnomalies ->
            Number (N.of_float @@ float_of_int @@ Anomaly.nb_anomalies ctx)
        | NbDiscordances ->
            Number (N.of_float @@ float_of_int @@ Anomaly.nb_discordances ctx)
        | NbInformatives ->
            Number (N.of_float @@ float_of_int @@ Anomaly.nb_informatives ctx)
        | NbBloquantes ->
            Number (N.of_float @@ float_of_int @@ Anomaly.nb_bloquantes ctx)
        | NbCategory _ | FuncCallLoop _ | Loop _ -> assert false
      with
      | RuntimeError (e, ctx) ->
          if !exit_on_rte then raise_runtime_as_structured e
          else raise (RuntimeError (e, ctx))
      | Errors.StructuredError (msg, kont) as exn ->
          if !exit_on_rte then raise exn
          else raise (RuntimeError (StructuredError (msg, kont), ctx))
    in
    fail_if_nan_or_inf ctx e out;
    out

  (* stmt evaluation *)

  and evaluate_affectation ctx a =
    match Pos.unmark a with
    | Com.SingleFormula (VarDecl (m_acc, vexpr)) ->
        set_access ctx (Pos.unmark m_acc) vexpr
    | SingleFormula (EventFieldRef (idx, _, j, var)) -> (
        match evaluate_expr ctx idx with
        | Number z when N.(z >=. zero ()) -> (
            let i = Int64.to_int @@ N.to_int z in
            let events = List.hd ctx.ctx_events in
            if 0 <= i && i < Array.length events then
              match events.(i).(j) with
              | Com.RefVar _ ->
                  let _, v, _ = C.get_var ctx None var in
                  if Com.Var.is_tgv v && not (Com.Var.is_table v) then
                    events.(i).(j) <- Com.RefVar v
              | Com.Numeric _ -> ())
        | _ -> ())
    | Com.MultipleFormulaes _ -> assert false

  and evaluate_ite canBlock ctx c t e =
    match evaluate_expr ctx c with
    | Number z when N.(z =. zero ()) -> evaluate_stmts canBlock ctx e
    | Number _ -> evaluate_stmts canBlock ctx t
    | Undefined -> ()

  and evaluate_switch canBlock ctx c l =
    let exception INTERNAL_STOP_SWITCH in
    let then_ () = raise INTERNAL_STOP_SWITCH in
    let v = evaluate_switch_expr ctx c in
    let default = ref None in
    try
      List.iter
        (fun (cases, stmts) ->
          List.iter
            (fun case ->
              match (case, v) with
              | Com.CDefault, _ ->
                  (* Trigged only if all other cases fail *)
                  default := Some stmts
              | CValue Undefined, `Undefined ->
                  evaluate_stmts ~then_ canBlock ctx stmts
              | CValue _, `Undefined | CValue Undefined, _ -> ()
              | CValue (Float f), `Value v ->
                  if N.of_float f = v then
                    evaluate_stmts ~then_ canBlock ctx stmts
              | CValue _, `Var _ -> failwith "Cannot match value with variable"
              | CVar m_acc, `Var v ->
                  if same_variable ctx m_acc v then
                    evaluate_stmts ~then_ canBlock ctx stmts
              | CVar _, (`Value _ | `Undefined) ->
                  failwith "Cannot match variable with value")
            cases)
        l
    with INTERNAL_STOP_SWITCH -> ()

  and evaluate_when_do_else canBlock ctx wdl ed =
    let rec aux = function
      | (expr, dl, _) :: l -> (
          match evaluate_expr ctx expr with
          | Number z when N.(z =. zero ()) ->
              evaluate_stmts canBlock ctx (Pos.unmark ed)
          | Number _ ->
              evaluate_stmts canBlock ctx dl;
              aux l
          | Undefined -> aux l)
      | [] -> ()
    in
    aux wdl

  and evaluate_print (ctx : C.ctx) std args =
    let pctx =
      match std with Com.StdOut -> ctx.ctx_pr_out | StdErr -> ctx.ctx_pr_err
    in
    List.iter
      (fun (arg : Com.Var.t Com.print_arg Pos.marked) ->
        match Pos.unmark arg with
        | PrintString s -> pr_string pctx s
        | PrintAccess (info, m_a) -> pr_access ~ctx pctx info (Pos.unmark m_a)
        | PrintIndent e -> pr_indent ~ctx pctx e
        | PrintExpr (e, mi, ma) -> pr_expr ~ctx pctx mi ma e)
      args;
    Printer.flush pctx

  and evaluate_iterate canBlock ctx var al var_params stmts =
    try
      List.iter
        (fun m_a ->
          match get_access_var ctx @@ Pos.unmark m_a with
          | Some (vsd, v, vorg) ->
              C.set_var_ref ctx var vsd v vorg;
              evaluate_stmts canBlock ctx stmts
          | None -> ())
        al;
      List.iter
        (fun (vcs, expr, m_sp_opt) ->
          let eval vc _ =
            StrMap.iter
              (fun _ v ->
                if
                  Com.CatVar.compare (Com.Var.cat v) vc = 0
                  && not (Com.Var.is_table v)
                then (
                  let vsd, v, org = C.get_var ctx m_sp_opt v in
                  C.set_var_ref ctx var vsd v org;
                  match evaluate_expr ctx expr with
                  | Number z when N.(z =. one ()) ->
                      evaluate_stmts canBlock ctx stmts
                  | _ -> ()))
              ctx.ctx_prog.program_vars
          in
          Com.CatVar.Map.iter eval vcs)
        var_params
    with
    | Stop_instruction (SKId None) -> ()
    | Stop_instruction (SKId (Some scope)) as exn ->
        if scope = Pos.unmark var.name then () else raise exn

  and evaluate_iterate_values canBlock ctx var var_intervals stmts =
    try
      List.iter
        (fun (e0, e1, step) ->
          let val0 = evaluate_expr ctx e0 in
          let val1 = evaluate_expr ctx e1 in
          let valStep = evaluate_expr ctx step in
          match (val0, val1, valStep) with
          | Number z0, Number z1, Number zStep when not N.(is_zero zStep) ->
              let cmp = N.(if zStep > zero () then ( <=. ) else ( >=. )) in
              let rec loop i =
                if cmp i z1 then (
                  let vsd, var, vorg = C.get_var ctx None var in
                  C.set_var_value_org ctx vsd var vorg (Number i);
                  evaluate_stmts canBlock ctx stmts;
                  loop N.(i +. zStep))
              in
              loop z0
          | _, _, _ -> ())
        var_intervals
    with
    | Stop_instruction (SKId None) -> ()
    | Stop_instruction (SKId (Some scope)) as exn ->
        if scope = Pos.unmark var.name then () else raise exn

  and evaluate_restore canBlock ctx al var_params evts evtfs stmts =
    let backup backup_vars vsd var vorg =
      if Com.Var.is_table var then
        let sz = Com.Var.size var in
        let rec loop backup_vars i =
          if i >= sz then backup_vars
          else
            let v_i = C.get_var_tab ctx var i in
            let value = C.get_var_value_org ctx vsd v_i vorg in
            loop ((vsd, v_i, vorg, value) :: backup_vars) (i + 1)
        in
        loop backup_vars 0
      else
        let value = C.get_var_value_org ctx vsd var vorg in
        (vsd, var, vorg, value) :: backup_vars
    in
    let backup_vars =
      List.fold_left
        (fun backup_vars m_acc ->
          match get_access_var ctx (Pos.unmark m_acc) with
          | Some (vsd, var, vorg) -> backup backup_vars vsd var vorg
          | None -> backup_vars)
        [] al
    in
    let backup_vars =
      List.fold_left
        (fun backup_vars ((var : Com.Var.t), vcs, expr, m_sp_opt) ->
          Com.CatVar.Map.fold
            (fun vc _ backup_vars ->
              StrMap.fold
                (fun _ v backup_vars ->
                  if Com.CatVar.compare (Com.Var.cat v) vc = 0 then (
                    let vsd, v', vorg = C.get_var ctx m_sp_opt v in
                    C.set_var_ref ctx var vsd v' vorg;
                    match evaluate_expr ctx expr with
                    | Number z when N.(z =. one ()) ->
                        backup backup_vars vsd v' vorg
                    | _ -> backup_vars)
                  else backup_vars)
                ctx.ctx_prog.program_vars backup_vars)
            vcs backup_vars)
        backup_vars var_params
    in
    let backup_evts =
      List.fold_left
        (fun backup_evts expr ->
          match evaluate_expr ctx expr with
          | Number z ->
              let i = Int64.to_int @@ N.to_int z in
              let events0 = List.hd ctx.ctx_events in
              if 0 <= i && i < Array.length events0 then (
                let evt = events0.(i) in
                events0.(i) <- Array.copy evt;
                (i, evt) :: backup_evts)
              else backup_evts
          | _ -> backup_evts)
        [] evts
    in
    let backup_evts =
      List.fold_left
        (fun backup_evts ((var : Com.Var.t), expr) ->
          let events0 = List.hd ctx.ctx_events in
          let rec aux backup_evts i =
            if i < Array.length events0 then (
              let vi = N.of_int @@ Int64.of_int i in
              C.set_var_value ctx None var (Number vi);
              match evaluate_expr ctx expr with
              | Number z when N.(z =. one ()) ->
                  let evt = events0.(i) in
                  events0.(i) <- Array.copy evt;
                  aux ((i, evt) :: backup_evts) (i + 1)
              | _ -> aux backup_evts (i + 1))
            else backup_evts
          in
          aux backup_evts 0)
        backup_evts evtfs
    in
    let then_ () =
      List.iter
        (fun (vsd, v, vorg, value) -> C.set_var_value_org ctx vsd v vorg value)
        backup_vars;
      let events0 = List.hd ctx.ctx_events in
      List.iter (fun (i, evt) -> events0.(i) <- evt) backup_evts
    in
    evaluate_stmts ~then_ canBlock ctx stmts

  and evaluate_arrange_events canBlock ctx sort filter add stmts =
    let event_list, nbAdd =
      match add with
      | Some expr -> (
          match evaluate_expr ctx expr with
          | Number z when N.(z >. zero ()) ->
              let nb = Int64.to_int @@ N.to_int z in
              if nb > 0 then
                let nbProgFields =
                  IntMap.cardinal ctx.ctx_prog.program_event_field_idxs
                in
                let defEvt =
                  let init id =
                    let fname =
                      IntMap.find id ctx.ctx_prog.program_event_field_idxs
                    in
                    let ef =
                      StrMap.find fname ctx.ctx_prog.program_event_fields
                    in
                    match ef.is_var with
                    | true ->
                        let defVar =
                          snd @@ StrMap.min_binding ctx.ctx_prog.program_vars
                        in
                        Com.RefVar defVar
                    | false -> Com.Numeric Undefined
                  in
                  Array.init nbProgFields init
                in
                let init = function 0 -> defEvt | _ -> Array.copy defEvt in
                (List.init nb init, nb)
              else ([], 0)
          | _ -> ([], 0))
      | None -> ([], 0)
    in
    let events =
      match filter with
      | Some (var, expr) ->
          let events0 = List.hd ctx.ctx_events in
          let rec aux res i =
            if i >= Array.length events0 then Array.of_list (List.rev res)
            else
              let vi = Number (N.of_int @@ Int64.of_int i) in
              C.set_var_value ctx None var vi;
              let res' =
                match evaluate_expr ctx expr with
                | Number z when N.(z =. one ()) -> events0.(i) :: res
                | _ -> res
              in
              aux res' (i + 1)
          in
          aux event_list 0
      | None ->
          let events0 = List.hd ctx.ctx_events in
          let rec aux res i =
            if i >= Array.length events0 then Array.of_list (List.rev res)
            else aux (events0.(i) :: res) (i + 1)
          in
          aux event_list 0
    in
    ctx.ctx_events <- events :: ctx.ctx_events;
    (match sort with
    | Some (var0, var1, expr) ->
        let sort_fun i _ j _ =
          let vi = Number (N.of_int @@ Int64.of_int i) in
          C.set_var_value ctx None var0 vi;
          let vj = Number (N.of_int @@ Int64.of_int j) in
          C.set_var_value ctx None var1 vj;
          match evaluate_expr ctx expr with
          | Number z when N.(z =. zero ()) -> false
          | Number _ -> true
          | Undefined -> false
        in
        Sorting.mergeSort sort_fun nbAdd (Array.length events) events
    | None -> ());
    let then_ () = ctx.ctx_events <- List.tl ctx.ctx_events in
    evaluate_stmts ~then_ canBlock ctx stmts

  and evaluate_raise_error canBlock ctx m_err var_opt =
    let is_blocking =
      Anomaly.raise ctx (Pos.unmark m_err) (Option.map Pos.unmark var_opt)
    in
    Tracer.register_ano ctx.tracer_ctx m_err;
    if is_blocking && ctx.ctx_nb_bloquantes >= 4 && canBlock then
      raise BlockingError

  and evaluate_stmt (canBlock : bool) (ctx : ctx) (stmt : Mir.m_instruction) :
      unit =
    match Pos.unmark stmt with
    | Com.Affectation a -> evaluate_affectation ctx a
    | Com.IfThenElse (b, t, f) -> evaluate_ite canBlock ctx b t f
    | Com.Switch (c, l) -> evaluate_switch canBlock ctx c l
    | Com.WhenDoElse (wdl, ed) -> evaluate_when_do_else canBlock ctx wdl ed
    | Com.VerifBlock stmts -> evaluate_stmts true ctx stmts
    | Com.ComputeTarget (Pos.Mark (tn, _), args, m_sp_opt) ->
        let tf = StrMap.find tn ctx.ctx_prog.program_targets in
        let vsd = C.get_var_space ctx m_sp_opt in
        evaluate_target canBlock ctx tf args vsd
    | Com.Print (std, args) -> evaluate_print ctx std args
    | Com.Iterate ((var : Com.Var.t), al, var_params, stmts) ->
        evaluate_iterate canBlock ctx var al var_params stmts
    | Com.Iterate_values ((var : Com.Var.t), var_intervals, stmts) ->
        evaluate_iterate_values canBlock ctx var var_intervals stmts
    | Com.Stop scope -> raise (Stop_instruction scope)
    | Com.Restore (al, var_params, evts, evtfs, stmts) ->
        evaluate_restore canBlock ctx al var_params evts evtfs stmts
    | Com.ArrangeEvents (sort, filter, add, stmts) ->
        evaluate_arrange_events canBlock ctx sort filter add stmts
    | Com.RaiseError (m_err, var_opt) ->
        evaluate_raise_error canBlock ctx m_err var_opt
    | Com.CleanErrors -> Anomaly.clean ctx
    | Com.CleanFinalizedErrors -> Anomaly.clean_finalized ctx
    | Com.FinalizeErrors -> Anomaly.finalize ~mode_corr:(mode_corr ctx) ctx
    | Com.ExportErrors -> Anomaly.export ~mode_corr:(mode_corr ctx) ctx
    | Com.ComputeDomain _ | Com.ComputeChaining _ | Com.ComputeVerifs _ ->
        assert false

  and evaluate_stmts ?(then_ = ignore) canBlock (ctx : ctx)
      (stmts : Mir.m_instruction list) : unit =
    let () =
      try List.iter (evaluate_stmt canBlock ctx) stmts with
      | BlockingError as b_err -> if canBlock then raise b_err
      | Stop_instruction _ as exn ->
          then_ ();
          raise exn
    in
    then_ ()

  and evaluate_function (ctx : ctx) (target : Mir.target)
      (args : Mir.m_expression list) : value =
    let rec set_args n vl el =
      match (vl, el) with
      | [], [] -> ()
      | v :: vl', e :: el' ->
          let i = ctx.ctx_tmps_org + n + 1 in
          let e_val = evaluate_expr ctx e in
          ctx.ctx_tmps.(i).var <- v;
          ctx.ctx_tmps.(i).value <- e_val;
          set_args (n + 1) vl' el'
      | _ -> assert false
    in
    set_args 0 target.target_args args;
    ctx.ctx_tmps.(ctx.ctx_tmps_org).var <- Option.get target.target_result;
    ctx.ctx_tmps.(ctx.ctx_tmps_org).value <- Undefined;
    evaluate_target_aux ~is_fun:true false ctx target;
    ctx.ctx_tmps.(ctx.ctx_tmps_org).value

  and evaluate_target (canBlock : bool) (ctx : ctx) (target : Mir.target)
      (args : Mir.m_access list) (vsd : Com.variable_space) : unit =
    (* We check if the current target is in the rule map.
       If it is, we assume we're in a rule, and register it
       to annotate the value we'll set later in the dbg_info. *)
    let target_name = Pos.unmark target.target_name in
    let rule_id =
      ctx.ctx_prog.program_rules |> IntMap.to_seq
      |> Seq.find (fun (_, str) -> str = target_name)
      |> Option.map fst
    in
    Tracer.update_execution_ctx ctx.tracer_ctx rule_id target_name;
    let rec set_args n vl al =
      match (vl, al) with
      | v :: vl', m_a :: al' -> (
          ctx.ctx_ref.(ctx.ctx_ref_org + n).var <- v;
          match get_access_var ctx (Pos.unmark m_a) with
          | Some (var_space, ref_var, org) ->
              ctx.ctx_ref.(ctx.ctx_ref_org + n).var_space <- var_space;
              ctx.ctx_ref.(ctx.ctx_ref_org + n).ref_var <- ref_var;
              ctx.ctx_ref.(ctx.ctx_ref_org + n).org <- org;
              set_args (n + 1) vl' al'
          | None -> ())
      | [], [] ->
          let vs_id_sav = ctx.ctx_var_space in
          ctx.ctx_var_space <- vsd.vs_id;
          evaluate_target_aux ~is_fun:false canBlock ctx target;
          ctx.ctx_var_space <- vs_id_sav
      | _ -> assert false
    in
    set_args 0 target.target_args args

  and evaluate_target_aux ~(is_fun : bool) (canBlock : bool) (ctx : ctx)
      (target : Mir.target) : unit =
    let sav_target = ctx.ctx_target in
    ctx.ctx_target <- target;
    ctx.ctx_tmps_org <- ctx.ctx_tmps_org + target.target_sz_tmps;
    StrMap.iter
      (fun _ v ->
        let i = ctx.ctx_tmps_org + Com.Var.loc_idx v in
        ctx.ctx_tmps.(i).var <- v;
        ctx.ctx_tmps.(i).value <- Undefined)
      target.target_tmp_vars;
    ctx.ctx_ref_org <- ctx.ctx_ref_org + target.target_nb_refs;
    let then_ () =
      ctx.ctx_ref_org <- ctx.ctx_ref_org - target.target_nb_refs;
      ctx.ctx_tmps_org <- ctx.ctx_tmps_org - target.target_sz_tmps
    in
    let () =
      try evaluate_stmts ~then_ canBlock ctx target.target_prog with
      | Stop_instruction SKTarget when not is_fun -> ()
      | Stop_instruction SKFun when is_fun -> ()
    in
    ctx.ctx_target <- sav_target

  let evaluate_program (ctx : ctx) : unit =
    try
      let main_target =
        match
          StrMap.find_opt ctx.ctx_prog.program_main_target
            ctx.ctx_prog.program_targets
        with
        | Some t -> t
        | None ->
            Errors.raise_error "Unable to find main function of Bir program"
      in
      let vsd = ctx.ctx_prog.program_var_space_def in
      ctx.ctx_target <- main_target;
      evaluate_target false ctx main_target [] vsd;
      evaluate_stmt false ctx (Pos.without Com.ExportErrors)
    with
    | RuntimeError (e, ctx) ->
        if !exit_on_rte then raise_runtime_as_structured e
        else raise (RuntimeError (e, ctx))
    | Stop_instruction SKApplication ->
        (* The only stop never caught by anything else *) ()
    | Stop_instruction SKTarget -> (* May not be caught by anything else *) ()

  let get_dbg_info (ctx : ctx) = Tracer.get_dbg_info ctx.tracer_ctx
end

module type RunnerKind = sig
  module FloatDefInterp : S

  module FloatMultInterp : S

  module FloatMfInterp : S

  module MPFRDefInterp : S

  module MPFRMultInterp : S

  module MPFRMfInterp : S

  module BigIntDefInterp : S

  module BigIntMultInterp : S

  module BigIntMfInterp : S

  module IntvDefInterp : S

  module IntvMultInterp : S

  module IntvMfInterp : S

  module RatDefInterp : S

  module RatMultInterp : S

  module RatMfInterp : S
end

module Runner = struct
  module NoTracing = struct
    module FloatDefInterp = Make (Number.FloatDef) (Tracers.NonTracer)
    module FloatMultInterp = Make (Number.FloatMult) (Tracers.NonTracer)
    module FloatMfInterp = Make (Number.FloatMf) (Tracers.NonTracer)
    module MPFRDefInterp = Make (Number.MPFRDef) (Tracers.NonTracer)
    module MPFRMultInterp = Make (Number.MPFRMult) (Tracers.NonTracer)
    module MPFRMfInterp = Make (Number.MPFRMf) (Tracers.NonTracer)
    module BigIntDefInterp = Make (Number.BigIntDef) (Tracers.NonTracer)
    module BigIntMultInterp = Make (Number.BigIntMult) (Tracers.NonTracer)
    module BigIntMfInterp = Make (Number.BigIntMf) (Tracers.NonTracer)
    module IntvDefInterp = Make (Number.IntvDef) (Tracers.NonTracer)
    module IntvMultInterp = Make (Number.IntvMult) (Tracers.NonTracer)
    module IntvMfInterp = Make (Number.IntvMf) (Tracers.NonTracer)
    module RatDefInterp = Make (Number.RatDef) (Tracers.NonTracer)
    module RatMultInterp = Make (Number.RatMult) (Tracers.NonTracer)
    module RatMfInterp = Make (Number.RatMf) (Tracers.NonTracer)
  end

  module WithTracing = struct
    module FloatDefInterp = Make (Number.FloatDef) (Tracers.Tracer)
    module FloatMultInterp = Make (Number.FloatMult) (Tracers.Tracer)
    module FloatMfInterp = Make (Number.FloatMf) (Tracers.Tracer)
    module MPFRDefInterp = Make (Number.MPFRDef) (Tracers.Tracer)
    module MPFRMultInterp = Make (Number.MPFRMult) (Tracers.Tracer)
    module MPFRMfInterp = Make (Number.MPFRMf) (Tracers.Tracer)
    module BigIntDefInterp = Make (Number.BigIntDef) (Tracers.Tracer)
    module BigIntMultInterp = Make (Number.BigIntMult) (Tracers.Tracer)
    module BigIntMfInterp = Make (Number.BigIntMf) (Tracers.Tracer)
    module IntvDefInterp = Make (Number.IntvDef) (Tracers.Tracer)
    module IntvMultInterp = Make (Number.IntvMult) (Tracers.Tracer)
    module IntvMfInterp = Make (Number.IntvMf) (Tracers.Tracer)
    module RatDefInterp = Make (Number.RatDef) (Tracers.Tracer)
    module RatMultInterp = Make (Number.RatMult) (Tracers.Tracer)
    module RatMfInterp = Make (Number.RatMf) (Tracers.Tracer)
  end
end

let get_interp (sort : Config.value_sort) (roundops : Config.round_ops)
    ~(trace : bool) : (module S) =
  let (module R : RunnerKind) =
    if trace then (module Runner.WithTracing) else (module Runner.NoTracing)
  in
  match (sort, roundops) with
  | RegularFloat, RODefault -> (module R.FloatDefInterp)
  | RegularFloat, ROMulti -> (module R.FloatMultInterp)
  | RegularFloat, ROMainframe _ -> (module R.FloatMfInterp)
  | MPFR _, RODefault -> (module R.MPFRDefInterp)
  | MPFR _, ROMulti -> (module R.MPFRMultInterp)
  | MPFR _, ROMainframe _ -> (module R.MPFRMfInterp)
  | BigInt _, RODefault -> (module R.BigIntDefInterp)
  | BigInt _, ROMulti -> (module R.BigIntMultInterp)
  | BigInt _, ROMainframe _ -> (module R.BigIntMfInterp)
  | Interval, RODefault -> (module R.IntvDefInterp)
  | Interval, ROMulti -> (module R.IntvMultInterp)
  | Interval, ROMainframe _ -> (module R.IntvMfInterp)
  | Rational, RODefault -> (module R.RatDefInterp)
  | Rational, ROMulti -> (module R.RatMultInterp)
  | Rational, ROMainframe _ -> (module R.RatMfInterp)

let evaluate_program ?(dbg_info : Dbg_info.t option) (p : Mir.program)
    (inputs : Com.literal Com.Var.Map.t)
    (events : (Com.literal, Com.Var.t) Com.event_value StrMap.t list)
    (sort : Config.value_sort) (roundops : Config.round_ops) :
    Com.literal Com.Var.Map.t * Com.Error.Set.t * Dbg_info.t option =
  Number.setup_precision sort roundops;
  let trace = !Config.trace in
  let module Interp = (val get_interp sort roundops ~trace : S) in
  let ctx = Interp.empty_ctx ?dbg_info ~inputs ~events p in
  Interp.evaluate_program ctx;
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  let varMap =
    let default_space =
      ctx.ctx_var_spaces.(ctx.ctx_prog.program_var_space_def.vs_id)
    in
    let fold _ (var : Com.Var.t) res =
      if Com.Var.is_given_back var || true then
        let litt =
          match Com.Var.cat_var_loc var with
          | LocInput -> default_space.input.(Com.Var.loc_idx var)
          | LocComputed -> default_space.computed.(Com.Var.loc_idx var)
          | LocBase -> default_space.base.(Com.Var.loc_idx var)
        in
        let fVal = Interp.N.to_literal litt in
        Com.Var.Map.add var fVal res
      else res
    in
    StrMap.fold fold ctx.ctx_prog.program_vars Com.Var.Map.empty
  in
  let anoSet =
    let fold res (e, _) = Com.Error.Set.add e res in
    List.fold_left fold Com.Error.Set.empty ctx.ctx_exported_anos
  in
  let dbg_info = Interp.get_dbg_info ctx in
  (varMap, anoSet, dbg_info)

let evaluate_expr ?(dbg_info : Dbg_info.t option) (p : Mir.program)
    (e : Mir.expression Pos.marked) (sort : Config.value_sort)
    (roundops : Config.round_ops) : Com.literal =
  let trace = !Config.trace in
  let module Interp = (val get_interp sort roundops ~trace : S) in
  try
    Interp.N.to_literal (Interp.evaluate_expr (Interp.empty_ctx ?dbg_info p) e)
  with Stop_instruction _ -> Undefined
