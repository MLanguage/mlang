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

module Make (N : Mir_number.NumberInterface) (RF : Mir_roundops.RoundOpsFunctor) :
  S with type custom_float = N.t = struct
  (* Careful : this behavior mimics the one imposed by the original Mlang
     compiler... *)

  module R = RF (N)
  module Funs = Functions.Make (N) (R)

  type custom_float = N.t

  type nonrec value = custom_float value

  type nonrec ctx = custom_float ctx

  type nonrec pctx = custom_float pctx

  exception InternalRuntimeError of run_error * ctx

  let roundf (x : N.t) = R.roundf x

  let _format_value (fmt : Format.formatter) (x : value) =
    match x with
    | Undefined -> Com.format_literal fmt Com.Undefined
    | Number x -> N.format_t fmt x

  let format_value_prec (mi : int) (ma : int) (fmt : Format.formatter)
      (x : value) =
    match x with
    | Undefined -> Com.format_literal fmt Com.Undefined
    | Number x -> N.format_prec_t mi ma fmt x

  let literal_to_value (l : Com.literal) : value =
    match l with
    | Com.Undefined -> Undefined
    | Com.Float f -> Number (N.of_float f)

  let value_to_literal (l : value) : Com.literal =
    match l with
    | Undefined -> Com.Undefined
    | Number f -> Com.Float (N.to_float f)

  let literal_event_to_value_event = function
    | Com.Numeric Com.Undefined -> Com.Numeric Undefined
    | Com.Numeric (Com.Float f) -> Com.Numeric (Number (N.of_float f))
    | Com.RefVar v -> Com.RefVar v

  let raise_runtime_as_structured (e : run_error) =
    match e with
    | NanOrInf (v, e) ->
        Errors.raise_spanned_error
          (Format.asprintf "Expression evaluated to %s: %a" v
             Format_mir.format_expression (Pos.unmark e))
          (Pos.get e)
    | StructuredError (msg, pos, kont) ->
        raise (Errors.StructuredError (msg, pos, kont))

  let is_zero (l : value) : bool =
    match l with Number z -> N.is_zero z | _ -> false

  let real_of_bool (b : bool) = if b then N.one () else N.zero ()

  let bool_of_real (f : N.t) : bool = not N.(f =. zero ())

  let get_var_space (ctx : ctx) (m_sp_opt : Com.var_space) =
    let i_sp =
      match m_sp_opt with None -> ctx.ctx_var_space | Some (_, i_sp) -> i_sp
    in
    IntMap.find i_sp ctx.ctx_prog.program_var_spaces_idx

  let get_var (ctx : ctx) (m_sp_opt : Com.var_space) (var : Com.Var.t) :
      Com.variable_space * Com.Var.t * int =
    match var.scope with
    | Com.Var.Tgv _ -> (get_var_space ctx m_sp_opt, var, 0)
    | Com.Var.Temp _ -> (get_var_space ctx None, var, ctx.ctx_tmps_org)
    | Com.Var.Ref ->
        let rv = ctx.ctx_ref.(ctx.ctx_ref_org + Com.Var.loc_idx var) in
        let vsd =
          match m_sp_opt with
          | None -> rv.var_space
          | _ -> get_var_space ctx m_sp_opt
        in
        (vsd, rv.ref_var, rv.org)

  let get_var_tab (ctx : ctx) (var : Com.Var.t) (i : int) : Com.Var.t =
    match Com.Var.get_table var with
    | Some _ -> ctx.ctx_tab_map.(Com.Var.loc_tab_idx var + 1 + i)
    | None -> assert false

  let get_var_value_org (ctx : ctx) (vsd : Com.variable_space) (var : Com.Var.t)
      (vorg : int) : value =
    let vi = Com.Var.loc_idx var in
    match var.scope with
    | Com.Var.Tgv _ ->
        let var_space = ctx.ctx_var_spaces.(vsd.vs_id) in
        let var_tab =
          match Com.Var.cat_var_loc var with
          | LocInput -> var_space.input
          | LocComputed -> var_space.computed
          | LocBase -> var_space.base
        in
        if Array.length var_tab > 0 then var_tab.(vi) else Undefined
    | Com.Var.Temp _ -> ctx.ctx_tmps.(vorg + vi).value
    | Com.Var.Ref -> assert false

  let get_var_value (ctx : ctx) (m_sp_opt : Com.var_space) (v : Com.Var.t) :
      value =
    let vsd, var, vorg = get_var ctx m_sp_opt v in
    let var = if Com.Var.is_table var then get_var_tab ctx var 0 else var in
    get_var_value_org ctx vsd var vorg

  let get_var_value_tab (ctx : ctx) (m_sp_opt : Com.var_space) (v : Com.Var.t)
      (i : int) : value =
    let vsd, var, vorg = get_var ctx m_sp_opt v in
    if i < 0 then Number (N.zero ())
    else if Com.Var.size var <= i then Undefined
    else if Com.Var.is_table var then
      let var_i = get_var_tab ctx var i in
      get_var_value_org ctx vsd var_i vorg
    else get_var_value_org ctx vsd var vorg

  let set_var_ref (ctx : ctx) (var : Com.Var.t) (var_space : Com.variable_space)
      (ref_var : Com.Var.t) (org : int) : unit =
    match var.loc with
    | LocRef (_, i) ->
        ctx.ctx_ref.(ctx.ctx_ref_org + i).var <- var;
        ctx.ctx_ref.(ctx.ctx_ref_org + i).var_space <- var_space;
        ctx.ctx_ref.(ctx.ctx_ref_org + i).ref_var <- ref_var;
        ctx.ctx_ref.(ctx.ctx_ref_org + i).org <- org
    | _ -> assert false

  let mode_corr (ctx : ctx) =
    match StrMap.find_opt "MODE_CORR" ctx.ctx_prog.program_vars with
    | Some var -> (
        let vsd = ctx.ctx_prog.program_var_space_def in
        let _, var, vorg = get_var ctx None var in
        match get_var_value_org ctx vsd var vorg with
        | Undefined -> false
        | Number n -> N.compare Eq n (N.one ()))
    | None -> false

  let comparison op new_e1 new_e2 =
    match (op, new_e1, new_e2) with
    | Com.(Gt | Gte | Lt | Lte | Eq | Neq), _, Undefined
    | Com.(Gt | Gte | Lt | Lte | Eq | Neq), Undefined, _ ->
        Undefined
    | op, Number i1, Number i2 -> Number (real_of_bool @@ N.compare op i1 i2)

  let unop op new_e1 =
    match (op, new_e1) with
    | Com.Not, Number b1 -> Number (real_of_bool (not (bool_of_real b1)))
    | Minus, Number f1 -> Number N.(zero () -. f1)
    | (Not | Minus), Undefined -> Undefined

  let binop (op : Com.binop) new_e1 new_e2 =
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
    | Div, Undefined, _ | Div, _, Undefined -> Undefined
    | Div, _, l2 when is_zero l2 -> Number (N.zero ()) (* yes... *)
    | Div, Number i1, Number i2 -> Number N.(i1 /. i2)
    | Mod, Undefined, _ | Mod, _, Undefined -> Undefined
    | Mod, _, l2 when is_zero l2 -> Number (N.zero ()) (* yes... *)
    | Mod, Number i1, Number i2 -> Number N.(i1 %. i2)
    | And, Undefined, _ | And, _, Undefined -> Undefined
    | Or, Undefined, Undefined -> Undefined
    | Or, Undefined, Number i | Or, Number i, Undefined -> Number i
    | And, Number i1, Number i2 ->
        Number (real_of_bool (bool_of_real i1 && bool_of_real i2))
    | Or, Number i1, Number i2 ->
        Number (real_of_bool (bool_of_real i1 || bool_of_real i2))

  exception BlockingError

  let rec get_access_value ctx access =
    match access with
    | Com.VarAccess (m_sp_opt, v) -> get_var_value ctx m_sp_opt v
    | Com.TabAccess (m_sp_opt, v, m_idx) -> (
        match evaluate_expr ctx m_idx with
        | Number z ->
            let i = Int64.to_int @@ N.to_int z in
            get_var_value_tab ctx m_sp_opt v i
        | Undefined -> Undefined)
    | Com.FieldAccess (m_sp_opt, e, _, j) -> (
        match evaluate_expr ctx e with
        | Number z ->
            let i = Int64.to_int @@ N.to_int z in
            let events = List.hd ctx.ctx_events in
            if 0 <= i && i < Array.length events then
              match events.(i).(j) with
              | Com.Numeric n -> n
              | Com.RefVar v -> get_var_value ctx m_sp_opt v
            else Undefined
        | Undefined -> Undefined)

  and get_access_var ctx access =
    match access with
    | Com.VarAccess (m_sp_opt, v) ->
        let vsd, v, vorg = get_var ctx m_sp_opt v in
        Some (vsd, v, vorg)
    | Com.TabAccess (m_sp_opt, m_v, m_i) -> (
        match evaluate_expr ctx m_i with
        | Number z ->
            let vsd, v, vorg = get_var ctx m_sp_opt m_v in
            let i = Int64.to_int @@ N.to_int z in
            if 0 <= i && i < Com.Var.size v then
              if Com.Var.is_table v then
                let v_i = get_var_tab ctx v i in
                Some (vsd, v_i, vorg)
              else Some (vsd, v, vorg)
            else None
        | Undefined -> None)
    | Com.FieldAccess (m_sp_opt, m_e, _, j) -> (
        match evaluate_expr ctx m_e with
        | Number z ->
            let i = Int64.to_int @@ N.to_int z in
            let events = List.hd ctx.ctx_events in
            if 0 <= i && i < Array.length events then
              match events.(i).(j) with
              | Com.RefVar v ->
                  let vsd, var, vorg = get_var ctx m_sp_opt v in
                  Some (vsd, var, vorg)
              | Com.Numeric _ -> None
            else None
        | _ -> None)

  and set_var_value_org (ctx : ctx) (vsd : Com.variable_space) (var : Com.Var.t)
      (vorg : int) (value : value) : unit =
    let vi = Com.Var.loc_idx var in
    match var.scope with
    | Com.Var.Tgv _ ->
        let var_space = ctx.ctx_var_spaces.(vsd.vs_id) in
        let var_tab =
          match Com.Var.cat_var_loc var with
          | LocInput -> var_space.input
          | LocComputed -> var_space.computed
          | LocBase -> var_space.base
        in
        if Array.length var_tab > 0 then var_tab.(vi) <- value
    | Com.Var.Temp _ -> ctx.ctx_tmps.(vorg + vi).value <- value
    | Com.Var.Ref -> assert false

  and set_var_value (ctx : ctx) (m_sp_opt : Com.var_space) (var : Com.Var.t)
      (value : value) : unit =
    let vsd, v, vorg = get_var ctx m_sp_opt var in
    if Com.Var.is_table v then
      for i = 0 to Com.Var.size v - 1 do
        let v_i = get_var_tab ctx v i in
        set_var_value_org ctx vsd v_i vorg value
      done
    else set_var_value_org ctx vsd v vorg value

  and set_var_value_tab (ctx : ctx) (m_sp_opt : Com.var_space) (v : Com.Var.t)
      (i : int) (value : value) : unit =
    let vsd, var, vorg = get_var ctx m_sp_opt v in
    if 0 <= i && i < Com.Var.size var then
      if Com.Var.is_table var then
        let var_i = get_var_tab ctx var i in
        set_var_value_org ctx vsd var_i vorg value
      else set_var_value_org ctx vsd var vorg value

  and set_access ctx access value =
    match access with
    | Com.VarAccess (m_sp_opt, v) -> set_var_value ctx m_sp_opt v value
    | Com.TabAccess (m_sp_opt, v, m_idx) -> (
        match evaluate_expr ctx m_idx with
        | Number z ->
            let i = Int64.to_int @@ N.to_int z in
            set_var_value_tab ctx m_sp_opt v i value
        | Undefined -> ())
    | Com.FieldAccess (m_sp_opt, e, _, j) -> (
        match evaluate_expr ctx e with
        | Number z -> (
            let i = Int64.to_int @@ N.to_int z in
            let events = List.hd ctx.ctx_events in
            if 0 <= i && i < Array.length events then
              match events.(i).(j) with
              | Com.Numeric _ -> events.(i).(j) <- Com.Numeric value
              | Com.RefVar v -> set_var_value ctx m_sp_opt v value)
        | Undefined -> ())

  (* print aux *)

  and pr_ctx std ctx =
    match std with
    | Com.StdOut ->
        { std; ctx; std_fmt = Format.std_formatter; ctx_pr = ctx.ctx_pr_out }
    | Com.StdErr ->
        { std; ctx; std_fmt = Format.err_formatter; ctx_pr = ctx.ctx_pr_err }

  and pr_flush (pctx : pctx) =
    match pctx.std with
    | Com.StdOut -> ()
    | Com.StdErr -> Format.pp_print_flush pctx.std_fmt ()

  and pr_out_indent (pctx : pctx) =
    if pctx.ctx_pr.is_newline then (
      for _i = 1 to pctx.ctx_pr.indent do
        Format.fprintf pctx.std_fmt " "
      done;
      pctx.ctx_pr.is_newline <- false)

  and pr_raw (pctx : pctx) s =
    let len = String.length s in
    let rec aux = function
      | n when n >= len -> ()
      | n -> (
          match s.[n] with
          | '\n' ->
              Format.fprintf pctx.std_fmt "\n";
              pr_flush pctx;
              pctx.ctx_pr.is_newline <- true;
              aux (n + 1)
          | c ->
              pr_out_indent pctx;
              Format.fprintf pctx.std_fmt "%c" c;
              aux (n + 1))
    in
    aux 0

  and pr_set_indent (pctx : pctx) diff =
    pctx.ctx_pr.indent <- max 0 (pctx.ctx_pr.indent + diff)

  and pr_value (pctx : pctx) mi ma value =
    pr_raw pctx (Pp.spr "%a" (format_value_prec mi ma) value)

  and pr_info (pctx : pctx) info (vsd : Com.variable_space) var =
    if not vsd.vs_by_default then (
      pr_raw pctx (Pos.unmark vsd.vs_name);
      pr_raw pctx ".");
    let _, v, _ = get_var pctx.ctx None var in
    match info with
    | Com.Name -> pr_raw pctx (Com.Var.name_str v)
    | Com.Alias -> pr_raw pctx (Com.Var.alias_str v)

  and pr_string (pctx : pctx) s =
    pr_raw pctx s;
    pr_flush pctx

  and pr_access (pctx : pctx) info acc =
    match get_access_var pctx.ctx acc with
    | Some (vsd, var, _) ->
        pr_info pctx info vsd var;
        pr_flush pctx
    | None -> ()

  and pr_indent (pctx : pctx) e =
    match evaluate_expr pctx.ctx e with
    | Undefined -> ()
    | Number x ->
        let diff = Int64.to_int @@ N.to_int @@ roundf x in
        pr_set_indent pctx diff

  and pr_expr (pctx : pctx) mi ma e =
    pr_value pctx mi ma (evaluate_expr pctx.ctx e);
    pr_flush pctx

  (* interpret *)

  and evaluate_expr (ctx : ctx) (e : Mir.expression Pos.marked) : value =
    let out =
      try
        match Pos.unmark e with
        | Com.TestInSet (positive, e0, values) ->
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
        | Comparison (op, e1, e2) ->
            let value1 = evaluate_expr ctx e1 in
            let value2 = evaluate_expr ctx e2 in
            comparison (Pos.unmark op) value1 value2
        | Binop (op, e1, e2) ->
            let value1 = evaluate_expr ctx e1 in
            let value2 = evaluate_expr ctx e2 in
            binop (Pos.unmark op) value1 value2
        | Unop (op, e1) -> unop op @@ evaluate_expr ctx e1
        | Conditional (e1, e2, e3_opt) -> (
            match evaluate_expr ctx e1 with
            | Number z when N.(z =. zero ()) -> (
                match e3_opt with
                | None -> Undefined
                | Some e3 -> evaluate_expr ctx e3)
            | Number _ -> evaluate_expr ctx e2
            | Undefined -> Undefined)
        | Literal Undefined -> Undefined
        | Literal (Float f) -> Number (N.of_float f)
        | Var access -> get_access_value ctx access
        | FuncCall (Pos.Mark (ArrFunc, _), [ arg ]) ->
            Funs.arr (evaluate_expr ctx arg)
        | FuncCall (Pos.Mark (InfFunc, _), [ arg ]) ->
            Funs.inf (evaluate_expr ctx arg)
        | FuncCall (Pos.Mark (PresentFunc, _), [ arg ]) ->
            Funs.present (evaluate_expr ctx arg)
        | FuncCall (Pos.Mark (Supzero, _), [ arg ]) ->
            Funs.supzero (evaluate_expr ctx arg)
        | FuncCall (Pos.Mark (AbsFunc, _), [ arg ]) ->
            Funs.abs (evaluate_expr ctx arg)
        | FuncCall (Pos.Mark (MinFunc, _), [ arg1; arg2 ]) ->
            Funs.min (evaluate_expr ctx arg1) (evaluate_expr ctx arg2)
        | FuncCall (Pos.Mark (MaxFunc, _), [ arg1; arg2 ]) ->
            Funs.max (evaluate_expr ctx arg1) (evaluate_expr ctx arg2)
        | FuncCall (Pos.Mark (Multimax, _), [ arg1; arg2 ]) -> (
            match evaluate_expr ctx arg1 with
            | Undefined -> Undefined
            | Number f -> (
                let nb = Int64.to_int @@ N.to_int @@ roundf f in
                let var_opt =
                  match Pos.unmark arg2 with
                  | Var access -> get_access_var ctx access
                  | _ -> None
                in
                match var_opt with
                | None -> Undefined
                | Some (vsd, var, vorg) ->
                    if Com.Var.is_table var then
                      let rec loop res i =
                        if i >= Com.Var.size var || i >= nb then res
                        else
                          let var_i = get_var_tab ctx var i in
                          let val_i = get_var_value_org ctx vsd var_i vorg in
                          let res =
                            match (res, val_i) with
                            | Undefined, _ -> val_i
                            | Number _, Undefined -> res
                            | Number nr, Number ni ->
                                if N.(nr <. ni) then val_i else res
                          in
                          loop res (i + 1)
                      in
                      loop Undefined 0
                    else if nb >= 1 then get_var_value_org ctx vsd var vorg
                    else Undefined))
        | FuncCall (Pos.Mark (NbEvents, _), _) -> Funs.nb_events ctx
        | FuncCall (Pos.Mark (Func fn, _), args) ->
            let fd = StrMap.find fn ctx.ctx_prog.program_functions in
            evaluate_function ctx fd args
        | FuncCall (_, _) -> assert false
        | Attribut (m_acc, a) -> (
            match get_access_var ctx (Pos.unmark m_acc) with
            | Some (_, v, _) -> (
                match StrMap.find_opt (Pos.unmark a) (Com.Var.attrs v) with
                | Some l -> Number (N.of_float (float (Pos.unmark l)))
                | None -> Undefined)
            | None -> Undefined)
        | Size m_acc -> (
            match get_access_var ctx (Pos.unmark m_acc) with
            | Some (_, v, _) -> Number (N.of_float @@ float @@ Com.Var.size v)
            | None -> Undefined)
        | Type (m_acc, m_typ) -> (
            match get_access_var ctx (Pos.unmark m_acc) with
            | Some (_, v, _) ->
                if Com.Var.is_tgv v && Com.Var.typ v = Some (Pos.unmark m_typ)
                then Number (N.one ())
                else Number (N.zero ())
            | None -> Undefined)
        | SameVariable (m_acc0, m_acc1) -> (
            let v0_opt = get_access_var ctx (Pos.unmark m_acc0) in
            let v1_opt = get_access_var ctx (Pos.unmark m_acc1) in
            match (v0_opt, v1_opt) with
            | Some (_, v0, _), Some (_, v1, _) ->
                if Com.Var.name_str v0 = Com.Var.name_str v1 then
                  Number (N.one ())
                else Number (N.zero ())
            | _, _ -> Number (N.zero ()))
        | InDomain (m_acc, cvm) -> (
            match get_access_var ctx (Pos.unmark m_acc) with
            | Some (_, v, _) ->
                if Com.Var.is_tgv v && Com.CatVar.Map.mem (Com.Var.cat v) cvm
                then Number (N.one ())
                else Number (N.zero ())
            | None -> Number (N.zero ()))
        | NbAnomalies -> Number (N.of_float (float ctx.ctx_nb_anos))
        | NbDiscordances -> Number (N.of_float (float ctx.ctx_nb_discos))
        | NbInformatives -> Number (N.of_float (float ctx.ctx_nb_infos))
        | NbBloquantes -> Number (N.of_float (float ctx.ctx_nb_bloquantes))
        | NbCategory _ | FuncCallLoop _ | Loop _ -> assert false
      with
      | InternalRuntimeError (e, ctx) ->
          if !exit_on_rte then raise_runtime_as_structured e
          else raise (InternalRuntimeError (e, ctx))
      | Errors.StructuredError (msg, pos, kont) ->
          if !exit_on_rte then
            raise
              (Errors.StructuredError
                 ( msg,
                   pos @ [ (Some "Expression raising the error:", Pos.get e) ],
                   kont ))
          else
            raise (InternalRuntimeError (StructuredError (msg, pos, kont), ctx))
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
      else raise (InternalRuntimeError (e, ctx))
    else out

  and evaluate_stmt (canBlock : bool) (ctx : ctx) (stmt : Mir.m_instruction) :
      unit =
    match Pos.unmark stmt with
    | Com.Affectation (Pos.Mark (SingleFormula (VarDecl (m_acc, vexpr)), _)) ->
        set_access ctx (Pos.unmark m_acc) @@ evaluate_expr ctx vexpr
    | Com.Affectation
        (Pos.Mark (SingleFormula (EventFieldRef (idx, _, j, var)), _)) -> (
        match evaluate_expr ctx idx with
        | Number z when N.(z >=. zero ()) -> (
            let i = Int64.to_int @@ N.to_int z in
            let events = List.hd ctx.ctx_events in
            if 0 <= i && i < Array.length events then
              match events.(i).(j) with
              | Com.RefVar _ ->
                  let _, v, _ = get_var ctx None var in
                  if Com.Var.is_tgv v && not (Com.Var.is_table v) then
                    events.(i).(j) <- Com.RefVar v
              | Com.Numeric _ -> ())
        | _ -> ())
    | Com.Affectation (Pos.Mark (Com.MultipleFormulaes _, _)) -> assert false
    | Com.IfThenElse (b, t, f) -> (
        match evaluate_expr ctx b with
        | Number z when N.(z =. zero ()) -> evaluate_stmts canBlock ctx f
        | Number _ -> evaluate_stmts canBlock ctx t
        | Undefined -> ())
    | Com.Switch (c, l) -> (
        let v = evaluate_expr ctx c in
        let exception INTERNAL_STOP_SWITCH in
        let then_ () = raise INTERNAL_STOP_SWITCH in
        try
          List.iter
            (fun (cases, stmts) ->
              List.iter
                (fun case ->
                  match (case, v) with
                  | Com.Default, _ | Value Undefined, Undefined ->
                      evaluate_stmts ~then_ canBlock ctx stmts
                  | Value (Float f), Number n when N.compare Eq n (N.of_float f)
                    ->
                      evaluate_stmts ~then_ canBlock ctx stmts
                  | _ -> ())
                cases)
            l
        with INTERNAL_STOP_SWITCH -> ())
    | Com.WhenDoElse (wdl, ed) ->
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
    | Com.VerifBlock stmts -> evaluate_stmts true ctx stmts
    | Com.ComputeTarget (Pos.Mark (tn, _), args, m_sp_opt) ->
        let tf = StrMap.find tn ctx.ctx_prog.program_targets in
        let vsd = get_var_space ctx m_sp_opt in
        evaluate_target canBlock ctx tf args vsd
    | Com.Print (std, args) ->
        let pctx = pr_ctx std ctx in
        List.iter
          (fun (arg : Com.Var.t Com.print_arg Pos.marked) ->
            match Pos.unmark arg with
            | PrintString s -> pr_string pctx s
            | PrintAccess (info, m_a) -> pr_access pctx info (Pos.unmark m_a)
            | PrintIndent e -> pr_indent pctx e
            | PrintExpr (e, mi, ma) -> pr_expr pctx mi ma e)
          args;
        pr_flush pctx
    | Com.Iterate ((var : Com.Var.t), al, var_params, stmts) -> (
        try
          List.iter
            (fun m_a ->
              match get_access_var ctx @@ Pos.unmark m_a with
              | Some (vsd, v, vorg) ->
                  set_var_ref ctx var vsd v vorg;
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
                      let vsd, v, org = get_var ctx m_sp_opt v in
                      set_var_ref ctx var vsd v org;
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
            if scope = Pos.unmark var.name then () else raise exn)
    | Com.Iterate_values ((var : Com.Var.t), var_intervals, stmts) -> (
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
                      let vsd, var, vorg = get_var ctx None var in
                      set_var_value_org ctx vsd var vorg (Number i);
                      evaluate_stmts canBlock ctx stmts;
                      loop N.(i +. zStep))
                  in
                  loop z0
              | _, _, _ -> ())
            var_intervals
        with
        | Stop_instruction (SKId None) -> ()
        | Stop_instruction (SKId (Some scope)) as exn ->
            if scope = Pos.unmark var.name then () else raise exn)
    | Com.Stop scope -> raise (Stop_instruction scope)
    | Com.Restore (al, var_params, evts, evtfs, stmts) ->
        let backup backup_vars vsd var vorg =
          if Com.Var.is_table var then
            let sz = Com.Var.size var in
            let rec loop backup_vars i =
              if i >= sz then backup_vars
              else
                let v_i = get_var_tab ctx var i in
                let value = get_var_value_org ctx vsd v_i vorg in
                loop ((vsd, v_i, vorg, value) :: backup_vars) (i + 1)
            in
            loop backup_vars 0
          else
            let value = get_var_value_org ctx vsd var vorg in
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
                        let vsd, v', vorg = get_var ctx m_sp_opt v in
                        set_var_ref ctx var vsd v' vorg;
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
                  set_var_value ctx None var (Number vi);
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
            (fun (vsd, v, vorg, value) ->
              set_var_value_org ctx vsd v vorg value)
            backup_vars;
          let events0 = List.hd ctx.ctx_events in
          List.iter (fun (i, evt) -> events0.(i) <- evt) backup_evts
        in
        evaluate_stmts ~then_ canBlock ctx stmts
    | Com.ArrangeEvents (sort, filter, add, stmts) ->
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
                              snd
                              @@ StrMap.min_binding ctx.ctx_prog.program_vars
                            in
                            Com.RefVar defVar
                        | false -> Com.Numeric Undefined
                      in
                      Array.init nbProgFields init
                    in
                    let init = function
                      | 0 -> defEvt
                      | _ -> Array.copy defEvt
                    in
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
                  set_var_value ctx None var vi;
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
              set_var_value ctx None var0 vi;
              let vj = Number (N.of_int @@ Int64.of_int j) in
              set_var_value ctx None var1 vj;
              match evaluate_expr ctx expr with
              | Number z when N.(z =. zero ()) -> false
              | Number _ -> true
              | Undefined -> false
            in
            Sorting.mergeSort sort_fun nbAdd (Array.length events) events
        | None -> ());
        let then_ () = ctx.ctx_events <- List.tl ctx.ctx_events in
        evaluate_stmts ~then_ canBlock ctx stmts
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
    | Com.CleanFinalizedErrors -> ctx.ctx_finalized_anos <- []
    | Com.FinalizeErrors ->
        let mem (ano : Com.Error.t) anos =
          List.fold_left
            (fun res ((a : Com.Error.t), _) ->
              res || Pos.unmark a.name = Pos.unmark ano.name)
            false anos
        in
        if mode_corr ctx then
          let rec merge_anos () =
            match ctx.ctx_anos with
            | [] -> ()
            | ((ano : Com.Error.t), arg) :: discos ->
                let cont =
                  if not (mem ano ctx.ctx_finalized_anos) then (
                    ctx.ctx_finalized_anos <-
                      ctx.ctx_finalized_anos @ [ (ano, arg) ];
                    ano.typ <> Com.Error.Anomaly)
                  else true
                in
                ctx.ctx_anos <- discos;
                if cont then merge_anos ()
          in
          merge_anos ()
        else (
          ctx.ctx_finalized_anos <- [];
          let rec merge_anos () =
            match ctx.ctx_anos with
            | [] -> ctx.ctx_finalized_anos <- List.rev ctx.ctx_finalized_anos
            | ((ano : Com.Error.t), arg) :: discos ->
                if not (StrSet.mem (Pos.unmark ano.name) ctx.ctx_archived_anos)
                then (
                  ctx.ctx_archived_anos <-
                    StrSet.add (Pos.unmark ano.name) ctx.ctx_archived_anos;
                  ctx.ctx_finalized_anos <- (ano, arg) :: ctx.ctx_finalized_anos);
                ctx.ctx_anos <- discos;
                merge_anos ()
          in
          merge_anos ())
    | Com.ExportErrors ->
        if mode_corr ctx then
          let rec merge_anos () =
            match ctx.ctx_finalized_anos with
            | [] -> ()
            | ((ano : Com.Error.t), arg) :: fins ->
                if not (StrSet.mem (Pos.unmark ano.name) ctx.ctx_archived_anos)
                then (
                  ctx.ctx_archived_anos <-
                    StrSet.add (Pos.unmark ano.name) ctx.ctx_archived_anos;
                  ctx.ctx_exported_anos <-
                    ctx.ctx_exported_anos @ [ (ano, arg) ]);
                ctx.ctx_finalized_anos <- fins;
                merge_anos ()
          in
          merge_anos ()
        else (
          ctx.ctx_exported_anos <-
            ctx.ctx_exported_anos @ ctx.ctx_finalized_anos;
          ctx.ctx_finalized_anos <- [])
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
    | InternalRuntimeError (e, ctx) ->
        if !exit_on_rte then raise_runtime_as_structured e
        else raise (InternalRuntimeError (e, ctx))
    | Stop_instruction SKApplication ->
        (* The only stop never caught by anything else *) ()
    | Stop_instruction SKTarget -> (* May not be caught by anything else *) ()
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

let get_interp (sort : Config.value_sort) (roundops : Config.round_ops) :
    (module S) =
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

let prepare_interp (sort : Config.value_sort) (roundops : Config.round_ops) :
    unit =
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

let evaluate_program ~(p : Mir.program) ~(inputs : Com.literal Com.Var.Map.t)
    ~(events : (Com.literal, Com.Var.t) Com.event_value StrMap.t list)
    ~(sort : Config.value_sort) ~(round_ops : Config.round_ops) :
    Com.literal Com.Var.Map.t * Com.Error.Set.t =
  prepare_interp sort round_ops;
  let module Interp = (val get_interp sort round_ops : S) in
  let ctx =
    let inputs = Com.Var.Map.map Interp.literal_to_value inputs in
    let events =
      List.map (StrMap.map Interp.literal_event_to_value_event) events
    in
    Context.empty_ctx ~inputs ~events p
  in
  let () =
    try Interp.evaluate_program ctx
    with Interp.InternalRuntimeError (r, _) -> raise (RuntimeError r)
  in
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
        let fVal = Interp.value_to_literal litt in
        Com.Var.Map.add var fVal res
      else res
    in
    StrMap.fold fold ctx.ctx_prog.program_vars Com.Var.Map.empty
  in
  let anoSet =
    let fold res (e, _) = Com.Error.Set.add e res in
    List.fold_left fold Com.Error.Set.empty ctx.ctx_exported_anos
  in
  (varMap, anoSet)

let evaluate_expr ~(p : Mir.program) ~(e : Mir.expression Pos.marked)
    ~(sort : Config.value_sort) ~(round_ops : Config.round_ops) : Com.literal =
  let module Interp = (val get_interp sort round_ops : S) in
  try
    Interp.value_to_literal (Interp.evaluate_expr (Context.empty_ctx p) e)
  with
  | Stop_instruction _ -> Undefined
  | Interp.InternalRuntimeError (r, _) -> raise (RuntimeError r)
