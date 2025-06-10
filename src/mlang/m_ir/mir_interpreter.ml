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

  type ctx_var_space = {
    input : value Array.t;
    computed : value Array.t;
    base : value Array.t;
  }

  type ctx = {
    ctx_prog : Mir.program;
    mutable ctx_target : Mir.target;
    mutable ctx_var_space : int;
    ctx_var_spaces : ctx_var_space Array.t;
    ctx_tmps : value Array.t;
    ctx_tmps_var : Com.Var.t Array.t;
    mutable ctx_tmps_org : int;
    ctx_ref : (Com.Var.t * (Com.Var.t * int)) Array.t;
    mutable ctx_ref_org : int;
    ctx_tab_map : Com.Var.t Array.t;
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
    mutable ctx_events : (value, Com.Var.t) Com.event_value Array.t Array.t list;
  }

  val empty_ctx : Mir.program -> ctx

  val literal_to_value : Com.literal -> value

  val value_to_literal : value -> Com.literal

  val update_ctx_with_inputs : ctx -> Com.literal Com.Var.Map.t -> unit

  val update_ctx_with_events :
    ctx -> (Com.literal, Com.Var.t) Com.event_value StrMap.t list -> unit

  type run_error =
    | NanOrInf of string * Mir.expression Pos.marked
    | StructuredError of
        (string * (string option * Pos.t) list * (unit -> unit) option)

  exception RuntimeError of run_error * ctx

  val raise_runtime_as_structured : run_error -> 'a

  val compare_numbers : Com.comp_op -> custom_float -> custom_float -> bool

  val evaluate_expr : ctx -> Mir.expression Pos.marked -> value

  val evaluate_program : ctx -> unit
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

  type ctx_var_space = {
    input : value Array.t;
    computed : value Array.t;
    base : value Array.t;
  }

  type ctx = {
    ctx_prog : Mir.program;
    mutable ctx_target : Mir.target;
    mutable ctx_var_space : int;
    ctx_var_spaces : ctx_var_space Array.t;
    ctx_tmps : value Array.t;
    ctx_tmps_var : Com.Var.t Array.t;
    mutable ctx_tmps_org : int;
    ctx_ref : (Com.Var.t * (Com.Var.t * int)) Array.t;
    mutable ctx_ref_org : int;
    ctx_tab_map : Com.Var.t Array.t;
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
    mutable ctx_events : (value, Com.Var.t) Com.event_value Array.t Array.t list;
  }

  let empty_ctx (p : Mir.program) : ctx =
    let dummy_var = Com.Var.new_ref ~name:(Pos.without "") in
    let dummy_ref = (dummy_var, (dummy_var, -1)) in
    let ctx_tab_map =
      let init i = IntMap.find i p.program_stats.table_map in
      Array.init (IntMap.cardinal p.program_stats.table_map) init
    in
    let ctx_var_spaces =
      let init i =
        let vsd = IntMap.find i p.program_var_spaces_idx in
        let input =
          if Com.CatVar.LocMap.mem Com.CatVar.LocInput vsd.vs_cats then
            Array.make p.program_stats.sz_input Undefined
          else Array.make 0 Undefined
        in
        let computed =
          if Com.CatVar.LocMap.mem Com.CatVar.LocComputed vsd.vs_cats then
            Array.make p.program_stats.sz_computed Undefined
          else Array.make 0 Undefined
        in
        let base =
          if Com.CatVar.LocMap.mem Com.CatVar.LocBase vsd.vs_cats then
            Array.make p.program_stats.sz_base Undefined
          else Array.make 0 Undefined
        in
        { input; computed; base }
      in
      Array.init (IntMap.cardinal p.program_var_spaces_idx) init
    in
    {
      ctx_prog = p;
      ctx_target = snd (StrMap.min_binding p.program_targets);
      ctx_var_space = p.program_var_space_def.vs_id;
      ctx_var_spaces;
      ctx_tmps = Array.make p.program_stats.sz_all_tmps Undefined;
      ctx_tmps_var = Array.make p.program_stats.sz_all_tmps dummy_var;
      ctx_tmps_org = 0;
      ctx_ref = Array.make p.program_stats.nb_all_refs dummy_ref;
      ctx_ref_org = 0;
      ctx_tab_map;
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
      ctx_events = [];
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
      Com.Var.Map.mapi
        (fun v l ->
          match l with
          | Com.Undefined -> Undefined
          | Com.Float f -> Number (N.of_float_input v f))
        inputs
    in
    let default_space =
      ctx.ctx_var_spaces.(ctx.ctx_prog.program_var_space_def.vs_id)
    in
    Com.Var.Map.iter
      (fun (var : Com.Var.t) value ->
        match Com.Var.cat_var_loc var with
        | LocInput -> default_space.input.(Com.Var.loc_idx var) <- value
        | LocComputed -> default_space.computed.(Com.Var.loc_idx var) <- value
        | LocBase -> default_space.base.(Com.Var.loc_idx var) <- value)
      value_inputs

  let update_ctx_with_events (ctx : ctx)
      (events : (Com.literal, Com.Var.t) Com.event_value StrMap.t list) : unit =
    let nbEvt = List.length events in
    let ctx_event_tab = Array.make nbEvt [||] in
    let fold idx (evt : (Com.literal, Com.Var.t) Com.event_value StrMap.t) =
      let nbProgFields = StrMap.cardinal ctx.ctx_prog.program_event_fields in
      let map = Array.make nbProgFields (Com.Numeric Undefined) in
      for id = 0 to nbProgFields - 1 do
        let fname = IntMap.find id ctx.ctx_prog.program_event_field_idxs in
        let ef = StrMap.find fname ctx.ctx_prog.program_event_fields in
        if ef.is_var then
          map.(id) <-
            Com.RefVar (snd (StrMap.min_binding ctx.ctx_prog.program_vars))
      done;
      let iter' fname ev =
        match StrMap.find_opt fname ctx.ctx_prog.program_event_fields with
        | Some ef -> (
            match (ev, ef.is_var) with
            | Com.Numeric Com.Undefined, false ->
                map.(ef.index) <- Com.Numeric Undefined
            | Com.Numeric (Com.Float f), false ->
                map.(ef.index) <- Com.Numeric (Number (N.of_float f))
            | Com.RefVar v, true -> map.(ef.index) <- Com.RefVar v
            | _ -> Errors.raise_error "wrong event field type")
        | None -> Errors.raise_error "unknown event field"
      in
      StrMap.iter iter' evt;
      ctx_event_tab.(idx) <- map;
      idx + 1
    in
    ignore (List.fold_left fold 0 events);
    (* let max_field_length =
         StrMap.fold
           (fun s _ r -> max r (String.length s))
           ctx.ctx_prog.program_event_fields 0
       in
       let pp_field fmt s =
         let l = String.length s in
         Format.fprintf fmt "%s%s" s (String.make (max_field_length - l + 1) ' ')
       in
       let pp_ev fmt = function
         | Com.Numeric Undefined -> Pp.string fmt "indefini"
         | Com.Numeric (Number v) -> N.format_t fmt v
         | Com.RefVar v -> Pp.string fmt (Com.Var.name_str v)
       in
       for i = 0 to Array.length ctx_event_tab - 1 do
         Format.eprintf "%d@." i;
         let map = ctx_event_tab.(i) in
         for j = 0 to Array.length map - 1 do
           let s = IntMap.find j ctx.ctx_prog.program_event_field_idxs in
           Format.eprintf "  %a%a@." pp_field s pp_ev map.(j)
         done
       done;*)
    ctx.ctx_events <- [ ctx_event_tab ]

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
          (Pos.get e)
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

  let get_var_space (ctx : ctx) =
    let i_sp = ctx.ctx_var_space in
    IntMap.find i_sp ctx.ctx_prog.program_var_spaces_idx

  let get_var_space_from (ctx : ctx) = function
    | None -> get_var_space ctx
    | Some (_, i_sp) -> IntMap.find i_sp ctx.ctx_prog.program_var_spaces_idx

  let get_var_space_opt (ctx : ctx) = function
    | None -> None
    | Some (_, i_sp) -> IntMap.find_opt i_sp ctx.ctx_prog.program_var_spaces_idx

  let get_var (ctx : ctx) (var : Com.Var.t) : Com.Var.t * int =
    match var.scope with
    | Com.Var.Tgv _ -> (var, 0)
    | Com.Var.Temp _ -> (var, ctx.ctx_tmps_org)
    | Com.Var.Ref -> snd ctx.ctx_ref.(ctx.ctx_ref_org + Com.Var.loc_idx var)

  let get_var_tab (ctx : ctx) (var : Com.Var.t) (idx : int) : Com.Var.t * int =
    let v, vorg = get_var ctx var in
    match Com.Var.get_table v with
    | Some _ ->
        let v' = ctx.ctx_tab_map.(Com.Var.loc_tab_idx v + 1 + idx) in
        (v', vorg)
    | None -> assert false

  let get_var_value_org (ctx : ctx) (vsd : Com.variable_space) (var : Com.Var.t)
      (org : int) : value =
    let vi = org + Com.Var.loc_idx var in
    match var.scope with
    | Com.Var.Tgv _ -> (
        let var_space = ctx.ctx_var_spaces.(vsd.vs_id) in
        match Com.Var.cat_var_loc var with
        | LocInput -> var_space.input.(vi)
        | LocComputed -> var_space.computed.(vi)
        | LocBase -> var_space.base.(vi))
    | Com.Var.Temp _ -> ctx.ctx_tmps.(vi)
    | Com.Var.Ref -> assert false

  let get_var_value (ctx : ctx) (vsd : Com.variable_space) (var : Com.Var.t) :
      value =
    let var, vorg = get_var ctx var in
    let var =
      if Com.Var.is_table var then ctx.ctx_tab_map.(Com.Var.loc_tab_idx var + 2)
      else var
    in
    get_var_value_org ctx vsd var vorg

  let set_var_ref (ctx : ctx) (var : Com.Var.t) (ref_val : Com.Var.t * int) :
      unit =
    match var.loc with
    | LocRef (_, i) -> ctx.ctx_ref.(ctx.ctx_ref_org + i) <- (var, ref_val)
    | _ -> assert false

  exception BlockingError

  let rec get_access_value ctx access =
    match access with
    | Com.VarAccess (m_sp_opt, v) ->
        let vsd = get_var_space_from ctx m_sp_opt in
        get_var_value ctx vsd v
    | Com.TabAccess (m_sp_opt, m_v, m_idx) -> (
        match evaluate_expr ctx m_idx with
        | Number z ->
            let v, vorg = get_var ctx m_v in
            let i = Int64.to_int @@ N.to_int z in
            let sz = Com.Var.size v in
            if i < 0 then Number (N.zero ())
            else if sz <= i then Undefined
            else
              let v' =
                if Com.Var.is_table v then
                  ctx.ctx_tab_map.(Com.Var.loc_tab_idx v + 1 + i)
                else v
              in
              let vsd = get_var_space_from ctx m_sp_opt in
              get_var_value_org ctx vsd v' vorg
        | Undefined -> Undefined)
    | Com.FieldAccess (m_sp_opt, e, _, j) -> (
        match evaluate_expr ctx e with
        | Number z ->
            let i = Int64.to_int @@ N.to_int z in
            let events = List.hd ctx.ctx_events in
            if 0 <= i && i < Array.length events then
              match events.(i).(j) with
              | Com.Numeric n -> n
              | Com.RefVar v ->
                  let vsd = get_var_space_from ctx m_sp_opt in
                  get_var_value ctx vsd v
            else Undefined
        | _ -> Undefined)

  and get_access_var ctx access =
    match access with
    | Com.VarAccess (m_sp_opt, v) -> Some (get_var_space_opt ctx m_sp_opt, v)
    | Com.TabAccess (m_sp_opt, m_v, m_i) -> (
        match evaluate_expr ctx m_i with
        | Number z ->
            let vsd_opt = get_var_space_opt ctx m_sp_opt in
            let v = fst @@ get_var ctx m_v in
            let i = Int64.to_int @@ N.to_int z in
            if 0 <= i && i < Com.Var.size v then
              if Com.Var.is_table v then
                Some (vsd_opt, ctx.ctx_tab_map.(Com.Var.loc_tab_idx v + 1 + i))
              else Some (vsd_opt, v)
            else None
        | Undefined -> None)
    | Com.FieldAccess (m_sp_opt, m_e, _, j) -> (
        match evaluate_expr ctx m_e with
        | Number z ->
            let vsd_opt = get_var_space_opt ctx m_sp_opt in
            let i = Int64.to_int @@ N.to_int z in
            let events = List.hd ctx.ctx_events in
            if 0 <= i && i < Array.length events then
              match events.(i).(j) with
              | Com.RefVar v -> Some (vsd_opt, v)
              | Com.Numeric _ -> None
            else None
        | _ -> None)

  and set_var_value_org (ctx : ctx) (vsd : Com.variable_space) (var : Com.Var.t)
      (org : int) (value : value) : unit =
    let vi = org + Com.Var.loc_idx var in
    match var.scope with
    | Com.Var.Tgv _ -> (
        let var_space = ctx.ctx_var_spaces.(vsd.vs_id) in
        match Com.Var.cat_var_loc var with
        | LocInput -> var_space.input.(vi) <- value
        | LocComputed -> var_space.computed.(vi) <- value
        | LocBase -> var_space.base.(vi) <- value)
    | Com.Var.Temp _ -> ctx.ctx_tmps.(vi) <- value
    | Com.Var.Ref -> assert false

  and set_var_value (ctx : ctx) (vsd : Com.variable_space) (var : Com.Var.t)
      (value : value) : unit =
    let v, vorg = get_var ctx var in
    if Com.Var.is_table v then
      for i = 0 to Com.Var.size v - 1 do
        let v' = ctx.ctx_tab_map.(Com.Var.loc_tab_idx v + 1 + i) in
        set_var_value_org ctx vsd v' vorg value
      done
    else set_var_value_org ctx vsd v vorg value

  and set_access ctx access vexpr =
    match get_access_var ctx access with
    | Some (vsd_opt, v) ->
        let vsd =
          match vsd_opt with None -> get_var_space ctx | Some vsd -> vsd
        in
        set_var_value ctx vsd v @@ evaluate_expr ctx vexpr
    | None -> ()

  and evaluate_expr (ctx : ctx) (e : Mir.expression Pos.marked) : value =
    let comparison op new_e1 new_e2 =
      match (op, new_e1, new_e2) with
      | Com.(Gt | Gte | Lt | Lte | Eq | Neq), _, Undefined
      | Com.(Gt | Gte | Lt | Lte | Eq | Neq), Undefined, _ ->
          Undefined
      | op, Number i1, Number i2 ->
          Number (real_of_bool @@ compare_numbers op i1 i2)
    in
    let unop op new_e1 =
      match (op, new_e1) with
      | Com.Not, Number b1 -> Number (real_of_bool (not (bool_of_real b1)))
      | Com.Minus, Number f1 -> Number N.(zero () -. f1)
      | Com.(Not | Minus), Undefined -> Undefined
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
    in
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
        | FuncCall (Pos.Mark (ArrFunc, _), [ arg ]) -> (
            match evaluate_expr ctx arg with
            | Number x -> Number (roundf x)
            | Undefined -> Undefined (*nope:Float 0.*))
        | FuncCall (Pos.Mark (InfFunc, _), [ arg ]) -> (
            match evaluate_expr ctx arg with
            | Number x -> Number (truncatef x)
            | Undefined -> Undefined (*Float 0.*))
        | FuncCall (Pos.Mark (PresentFunc, _), [ arg ]) -> (
            match evaluate_expr ctx arg with
            | Undefined -> false_value ()
            | _ -> true_value ())
        | FuncCall (Pos.Mark (Supzero, _), [ arg ]) -> (
            match evaluate_expr ctx arg with
            | Undefined -> Undefined
            | Number f as n ->
                if compare_numbers Com.Lte f (N.zero ()) then Undefined else n)
        | FuncCall (Pos.Mark (AbsFunc, _), [ arg ]) -> (
            match evaluate_expr ctx arg with
            | Undefined -> Undefined
            | Number f -> Number (N.abs f))
        | FuncCall (Pos.Mark (MinFunc, _), [ arg1; arg2 ]) -> (
            match (evaluate_expr ctx arg1, evaluate_expr ctx arg2) with
            | Undefined, Undefined -> Undefined
            | Undefined, Number f | Number f, Undefined ->
                Number (N.min (N.zero ()) f)
            | Number fl, Number fr -> Number (N.min fl fr))
        | FuncCall (Pos.Mark (MaxFunc, _), [ arg1; arg2 ]) -> (
            match (evaluate_expr ctx arg1, evaluate_expr ctx arg2) with
            | Undefined, Undefined -> Undefined
            | Undefined, Number f | Number f, Undefined ->
                Number (N.max (N.zero ()) f)
            | Number fl, Number fr -> Number (N.max fl fr))
        | FuncCall (Pos.Mark (Multimax, _), [ arg1; arg2 ]) -> (
            match evaluate_expr ctx arg1 with
            | Undefined -> Undefined
            | Number f -> (
                let up = Int64.sub (N.to_int @@ roundf f) 1L in
                let var_arg2_opt =
                  match Pos.unmark arg2 with
                  | Var access -> get_access_var ctx access
                  | _ -> None
                in
                match var_arg2_opt with
                | None -> Undefined
                | Some (vsd_opt, var_arg2) -> (
                    let cast_to_int (v : value) : Int64.t option =
                      match v with
                      | Number f -> Some (N.to_int @@ roundf f)
                      | Undefined -> None
                    in
                    let access_index (i : int) : Int64.t option =
                      let ei =
                        Pos.same (Com.Literal (Float (float_of_int i))) arg2
                      in
                      let instr =
                        let vsd =
                          match vsd_opt with
                          | None -> get_var_space ctx
                          | Some vsd -> vsd
                        in
                        let m_sp_opt =
                          let vn = Com.Normal (Pos.unmark vsd.vs_name) in
                          Some (Pos.without vn, vsd.vs_id)
                        in
                        let a = Com.TabAccess (m_sp_opt, var_arg2, ei) in
                        Pos.same (Com.Var a) arg2
                      in
                      cast_to_int @@ evaluate_expr ctx instr
                    in
                    let maxi = ref None in
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
                    | Some f -> Number (N.of_int f))))
        | FuncCall (Pos.Mark (NbEvents, _), _) ->
            let card = Array.length (List.hd ctx.ctx_events) in
            Number (N.of_int @@ Int64.of_int @@ card)
        | FuncCall (Pos.Mark (Func fn, _), args) ->
            let fd = StrMap.find fn ctx.ctx_prog.program_functions in
            evaluate_function ctx fd args
        | FuncCall (_, _) -> assert false
        | Attribut (m_acc, a) -> (
            match get_access_var ctx (Pos.unmark m_acc) with
            | Some (_, v) -> (
                let v' = fst @@ get_var ctx v in
                match StrMap.find_opt (Pos.unmark a) (Com.Var.attrs v') with
                | Some l -> Number (N.of_float (float (Pos.unmark l)))
                | None -> Undefined)
            | None -> Undefined)
        | Size m_acc -> (
            match get_access_var ctx (Pos.unmark m_acc) with
            | Some (_, v) ->
                let v' = fst @@ get_var ctx v in
                Number (N.of_float @@ float @@ Com.Var.size v')
            | None -> Undefined)
        | IsVariable (m_acc, m_name) -> (
            match get_access_var ctx (Pos.unmark m_acc) with
            | Some (_, v) -> (
                let v' = fst @@ get_var ctx v in
                let name = Pos.unmark m_name in
                if Com.Var.name_str v' = name then Number (N.one ())
                else
                  match Com.Var.alias v' with
                  | Some m_a when Pos.unmark m_a = name -> Number (N.one ())
                  | _ -> Number (N.zero ()))
            | None -> Number (N.zero ()))
        | NbAnomalies -> Number (N.of_float (float ctx.ctx_nb_anos))
        | NbDiscordances -> Number (N.of_float (float ctx.ctx_nb_discos))
        | NbInformatives -> Number (N.of_float (float ctx.ctx_nb_infos))
        | NbBloquantes -> Number (N.of_float (float ctx.ctx_nb_bloquantes))
        | NbCategory _ | FuncCallLoop _ | Loop _ -> assert false
      with
      | RuntimeError (e, ctx) ->
          if !exit_on_rte then raise_runtime_as_structured e
          else raise (RuntimeError (e, ctx))
      | Errors.StructuredError (msg, pos, kont) ->
          if !exit_on_rte then
            raise
              (Errors.StructuredError
                 ( msg,
                   pos @ [ (Some "Expression raising the error:", Pos.get e) ],
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

  and evaluate_stmt (canBlock : bool) (ctx : ctx) (stmt : Mir.m_instruction) :
      unit =
    match Pos.unmark stmt with
    | Com.Affectation (Pos.Mark (SingleFormula (VarDecl (m_acc, vexpr)), _)) ->
        set_access ctx (Pos.unmark m_acc) vexpr
    | Com.Affectation
        (Pos.Mark (SingleFormula (EventFieldRef (idx, _, j, var)), _)) -> (
        match evaluate_expr ctx idx with
        | Number z when N.(z >=. zero ()) -> (
            let i = Int64.to_int @@ N.to_int z in
            let events = List.hd ctx.ctx_events in
            if 0 <= i && i < Array.length events then
              match events.(i).(j) with
              | Com.RefVar _ -> events.(i).(j) <- Com.RefVar var
              | Com.Numeric _ -> ())
        | _ -> ())
    | Com.Affectation (Pos.Mark (Com.MultipleFormulaes _, _)) -> assert false
    | Com.IfThenElse (b, t, f) -> (
        match evaluate_expr ctx b with
        | Number z when N.(z =. zero ()) -> evaluate_stmts canBlock ctx f
        | Number _ -> evaluate_stmts canBlock ctx t
        | Undefined -> ())
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
    | Com.ComputeTarget (Pos.Mark (tn, _), args) ->
        let tf = StrMap.find tn ctx.ctx_prog.program_targets in
        evaluate_target canBlock ctx tf args
    | Com.Print (std, args) ->
        let std_fmt, ctx_pr =
          match std with
          | Com.StdOut -> (Format.std_formatter, ctx.ctx_pr_out)
          | Com.StdErr -> (Format.err_formatter, ctx.ctx_pr_err)
        in
        let pr_flush () =
          match std with
          | Com.StdOut -> ()
          | Com.StdErr -> Format.pp_print_flush Format.err_formatter ()
        in
        let pr_indent () =
          if ctx_pr.is_newline then (
            for _i = 1 to ctx_pr.indent do
              Format.fprintf std_fmt " "
            done;
            ctx_pr.is_newline <- false)
        in
        let pr_raw s =
          let len = String.length s in
          let rec aux = function
            | n when n >= len -> ()
            | n -> (
                match s.[n] with
                | '\n' ->
                    Format.fprintf std_fmt "\n";
                    pr_flush ();
                    ctx_pr.is_newline <- true;
                    aux (n + 1)
                | c ->
                    pr_indent ();
                    Format.fprintf std_fmt "%c" c;
                    aux (n + 1))
          in
          aux 0
        in
        let pr_set_indent diff =
          ctx_pr.indent <- max 0 (ctx_pr.indent + diff)
        in
        let pr_value mi ma value =
          pr_raw (Pp.spr "%a" (format_value_prec mi ma) value)
        in
        let pr_info info (vsd_opt : Com.variable_space option) var =
          (match vsd_opt with
          | None ->
              let vsd = get_var_space ctx in
              if not vsd.vs_by_default then (
                pr_raw (Pos.unmark vsd.vs_name);
                pr_raw ".")
          | Some vsd ->
              pr_raw (Pos.unmark vsd.vs_name);
              pr_raw ".");
          let v = fst @@ get_var ctx var in
          match info with
          | Com.Name -> pr_raw (Com.Var.name_str v)
          | Com.Alias -> pr_raw (Com.Var.alias_str v)
        in
        List.iter
          (fun (arg : Com.Var.t Com.print_arg Pos.marked) ->
            match Pos.unmark arg with
            | PrintString s ->
                pr_raw s;
                pr_flush ()
            | PrintAccess (info, m_a) -> (
                match get_access_var ctx @@ Pos.unmark m_a with
                | Some (vsd_opt, var) ->
                    pr_info info vsd_opt var;
                    pr_flush ()
                | None -> ())
            | PrintIndent e -> (
                match evaluate_expr ctx e with
                | Undefined -> ()
                | Number x ->
                    let diff = Int64.to_int @@ N.to_int @@ roundf x in
                    pr_set_indent diff)
            | PrintExpr (e, mi, ma) ->
                pr_value mi ma (evaluate_expr ctx e);
                pr_flush ())
          args;
        pr_flush ()
    | Com.Iterate ((var : Com.Var.t), vars, var_params, stmts) ->
        List.iter
          (fun v ->
            set_var_ref ctx var (get_var ctx v);
            evaluate_stmts canBlock ctx stmts)
          vars;
        List.iter
          (fun (vcs, expr) ->
            let eval vc _ =
              StrMap.iter
                (fun _ v ->
                  if
                    Com.CatVar.compare (Com.Var.cat v) vc = 0
                    && not (Com.Var.is_table v)
                  then (
                    set_var_ref ctx var (get_var ctx v);
                    match evaluate_expr ctx expr with
                    | Number z when N.(z =. one ()) ->
                        evaluate_stmts canBlock ctx stmts
                    | _ -> ()))
                ctx.ctx_prog.program_vars
            in
            Com.CatVar.Map.iter eval vcs)
          var_params
    | Com.Iterate_values ((var : Com.Var.t), var_intervals, stmts) ->
        List.iter
          (fun (e0, e1, step) ->
            match evaluate_expr ctx e0 with
            | Number z0 -> (
                match evaluate_expr ctx e1 with
                | Number z1 -> (
                    match evaluate_expr ctx step with
                    | Number zStep when not N.(is_zero zStep) ->
                        let cmp =
                          if N.(zStep > zero ()) then N.( <=. ) else N.( >=. )
                        in
                        let rec loop i =
                          if cmp i z1 then (
                            let vsd = ctx.ctx_prog.program_var_space_def in
                            set_var_value ctx vsd var (Number i);
                            evaluate_stmts canBlock ctx stmts;
                            loop N.(i +. zStep))
                        in
                        loop z0
                    | _ -> ())
                | Undefined -> ())
            | Undefined -> ())
          var_intervals
    | Com.Restore (vars, var_params, evts, evtfs, stmts) ->
        let vsd_def = ctx.ctx_prog.program_var_space_def in
        let backup backup_vars var =
          let var, vorg = get_var ctx var in
          if Com.Var.is_table var then
            let sz = Com.Var.size var in
            let rec loop backup_vars i =
              if i = sz then backup_vars
              else
                let v = fst @@ get_var_tab ctx var i in
                let value = get_var_value_org ctx vsd_def v vorg in
                loop ((v, vorg, value) :: backup_vars) (i + 1)
            in
            loop backup_vars 0
          else
            let value = get_var_value ctx vsd_def var in
            (var, vorg, value) :: backup_vars
        in
        let backup_vars = List.fold_left backup [] vars in
        let backup_vars =
          List.fold_left
            (fun backup_vars ((var : Com.Var.t), vcs, expr) ->
              Com.CatVar.Map.fold
                (fun vc _ backup_vars ->
                  StrMap.fold
                    (fun _ v backup_vars ->
                      if Com.CatVar.compare (Com.Var.cat v) vc = 0 then (
                        set_var_ref ctx var (get_var ctx v);
                        match evaluate_expr ctx expr with
                        | Number z when N.(z =. one ()) -> backup backup_vars v
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
                  set_var_value ctx vsd_def var (Number vi);
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
        evaluate_stmts canBlock ctx stmts;
        List.iter
          (fun (v, vorg, value) -> set_var_value_org ctx vsd_def v vorg value)
          backup_vars;
        let events0 = List.hd ctx.ctx_events in
        List.iter (fun (i, evt) -> events0.(i) <- evt) backup_evts
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
                  let vsd_def = ctx.ctx_prog.program_var_space_def in
                  let vi = Number (N.of_int @@ Int64.of_int i) in
                  set_var_value ctx vsd_def var vi;
                  let res' =
                    match evaluate_expr ctx expr with
                    | Number z when N.(z =. one ()) -> events0.(i) :: res
                    | _ -> res
                  in
                  aux res' (i + 1)
              in
              aux event_list 0
          | None when event_list = [] -> Array.copy (List.hd ctx.ctx_events)
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
              let vsd_def = ctx.ctx_prog.program_var_space_def in
              let vi = Number (N.of_int @@ Int64.of_int i) in
              set_var_value ctx vsd_def var0 vi;
              let vj = Number (N.of_int @@ Int64.of_int j) in
              set_var_value ctx vsd_def var1 vj;
              match evaluate_expr ctx expr with
              | Number z when N.(z =. zero ()) -> false
              | Number _ -> true
              | Undefined -> false
            in
            Sorting.mergeSort sort_fun nbAdd (Array.length events) events
        | None -> ());
        evaluate_stmts canBlock ctx stmts;
        ctx.ctx_events <- List.tl ctx.ctx_events
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

  and evaluate_stmts canBlock (ctx : ctx) (stmts : Mir.m_instruction list) :
      unit =
    try List.iter (evaluate_stmt canBlock ctx) stmts
    with BlockingError as b_err -> if canBlock then raise b_err

  and evaluate_function (ctx : ctx) (target : Mir.target)
      (args : Mir.m_expression list) : value =
    let rec set_args n vl el =
      match (vl, el) with
      | [], [] -> ()
      | v :: vl', e :: el' ->
          let i = ctx.ctx_tmps_org + n + 1 in
          ctx.ctx_tmps.(i) <- evaluate_expr ctx e;
          ctx.ctx_tmps_var.(i) <- v;
          set_args (n + 1) vl' el'
      | _ -> assert false
    in
    set_args 0 target.target_args args;
    ctx.ctx_tmps.(ctx.ctx_tmps_org) <- Undefined;
    ctx.ctx_tmps_var.(ctx.ctx_tmps_org) <- Option.get target.target_result;
    evaluate_target_aux false ctx target;
    ctx.ctx_tmps.(ctx.ctx_tmps_org)

  and evaluate_target (canBlock : bool) (ctx : ctx) (target : Mir.target)
      (args : Com.Var.t list) : unit =
    let rec set_args n = function
      | [] -> ()
      | v :: vl ->
          ctx.ctx_ref.(ctx.ctx_ref_org + n) <- (v, get_var ctx v);
          set_args (n + 1) vl
    in
    set_args 0 args;
    evaluate_target_aux canBlock ctx target

  and evaluate_target_aux (canBlock : bool) (ctx : ctx) (target : Mir.target) :
      unit =
    let sav_target = ctx.ctx_target in
    ctx.ctx_target <- target;
    ctx.ctx_tmps_org <- ctx.ctx_tmps_org + target.target_sz_tmps;
    StrMap.iter
      (fun _ v ->
        let i = ctx.ctx_tmps_org + Com.Var.loc_idx v in
        ctx.ctx_tmps.(i) <- Undefined;
        ctx.ctx_tmps_var.(i) <- v)
      target.target_tmp_vars;
    ctx.ctx_ref_org <- ctx.ctx_ref_org + target.target_nb_refs;
    evaluate_stmts canBlock ctx target.target_prog;
    ctx.ctx_ref_org <- ctx.ctx_ref_org - target.target_nb_refs;
    ctx.ctx_tmps_org <- ctx.ctx_tmps_org - target.target_sz_tmps;
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
      ctx.ctx_target <- main_target;
      evaluate_target false ctx main_target [];
      evaluate_stmt false ctx (Pos.without Com.ExportErrors)
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
    (events : (Com.literal, Com.Var.t) Com.event_value StrMap.t list)
    (sort : Cli.value_sort) (roundops : Cli.round_ops) :
    Com.literal Com.Var.Map.t * Com.Error.Set.t =
  prepare_interp sort roundops;
  let module Interp = (val get_interp sort roundops : S) in
  let ctx = Interp.empty_ctx p in
  Interp.update_ctx_with_inputs ctx inputs;
  Interp.update_ctx_with_events ctx events;
  Interp.evaluate_program ctx;
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  let varMap =
    let default_space =
      ctx.ctx_var_spaces.(ctx.ctx_prog.program_var_space_def.vs_id)
    in
    let fold _ (var : Com.Var.t) res =
      if Com.Var.is_given_back var then
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

let evaluate_expr (p : Mir.program) (e : Mir.expression Pos.marked)
    (sort : Cli.value_sort) (roundops : Cli.round_ops) : Com.literal =
  let module Interp = (val get_interp sort roundops : S) in
  Interp.value_to_literal (Interp.evaluate_expr (Interp.empty_ctx p) e)
