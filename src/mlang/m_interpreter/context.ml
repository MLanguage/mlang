open M_ir
open Types

type 'a ctx_tmp_var = { mutable var : Com.Var.t; mutable value : 'a value }

type ctx_ref_var = {
  mutable var : Com.Var.t;
  mutable var_space : Com.variable_space;
  mutable ref_var : Com.Var.t;
  mutable org : int;
}

type 'a ctx_var_space = {
  input : 'a value Array.t;
  computed : 'a value Array.t;
  base : 'a value Array.t;
}

type ctx_print = { mutable indent : int; mutable is_newline : bool }

type ('a, 'tc) t = {
  ctx_prog : Mir.program;
  mutable ctx_target : Mir.target;
  mutable ctx_var_space : int;
  ctx_var_spaces : 'a ctx_var_space Array.t;
  ctx_tmps : 'a ctx_tmp_var Array.t;
  mutable ctx_tmps_org : int;
  ctx_ref : ctx_ref_var Array.t;
  mutable ctx_ref_org : int;
  ctx_tab_map : Com.Var.t Array.t;
  ctx_pr_out : ctx_print;
  ctx_pr_err : ctx_print;
  mutable ctx_anos : (Com.Error.t * string option) list;
  mutable ctx_nb_anos : int;
  mutable ctx_nb_discos : int;
  mutable ctx_nb_infos : int;
  mutable ctx_nb_bloquantes : int;
  mutable ctx_archived_anos : StrSet.t;
  mutable ctx_finalized_anos : (Com.Error.t * string option) list;
  mutable ctx_exported_anos : (Com.Error.t * string option) list;
  mutable ctx_events :
    ('a value, Com.Var.t) Com.event_value Array.t Array.t list;
  tracer_ctx : 'tc;
}

let empty_ctx ~(tracer_ctx : 'tc) (p : Mir.program) : ('a, 'tc) t =
  let dummy_var = Com.Var.new_ref ~name:(Pos.without "") in
  let init_tmp_var _i = { var = dummy_var; value = Undefined } in
  let init_ref _i =
    {
      var = dummy_var;
      var_space = p.program_var_space_def;
      ref_var = dummy_var;
      org = -1;
    }
  in
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
    ctx_tmps = Array.init p.program_stats.sz_all_tmps init_tmp_var;
    ctx_tmps_org = 0;
    ctx_ref = Array.init p.program_stats.nb_all_refs init_ref;
    ctx_ref_org = 0;
    ctx_tab_map;
    ctx_pr_out = { indent = 0; is_newline = true };
    ctx_pr_err = { indent = 0; is_newline = true };
    ctx_anos = [];
    ctx_nb_anos = 0;
    ctx_nb_discos = 0;
    ctx_nb_infos = 0;
    ctx_nb_bloquantes = 0;
    ctx_archived_anos = StrSet.empty;
    ctx_finalized_anos = [];
    ctx_exported_anos = [];
    ctx_events = [];
    tracer_ctx;
  }

let get_var_space (ctx : _ t) (m_sp_opt : Com.var_space) =
  let i_sp =
    match m_sp_opt with None -> ctx.ctx_var_space | Some (_, i_sp) -> i_sp
  in
  IntMap.find i_sp ctx.ctx_prog.program_var_spaces_idx

let get_var (ctx : _ t) (m_sp_opt : Com.var_space) (var : Com.Var.t) :
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

let unsafe_get_var_tab (ctx : _ t) (var : Com.Var.t) (i : int) =
  ctx.ctx_tab_map.(Com.Var.loc_tab_idx var + 1 + i)

let get_var_tab (ctx : _ t) (var : Com.Var.t) (i : int) : Com.Var.t =
  assert (Com.Var.is_table var);
  unsafe_get_var_tab ctx var i

let get_vars_tab (ctx : _ t) (var : Com.Var.t) : Com.Var.t list =
  assert (Com.Var.is_table var);
  let rec loop sz l =
    if sz <= 0 then l else loop (sz - 1) (unsafe_get_var_tab ctx var sz :: l)
  in
  loop (Com.Var.size var) []

let get_var_value_org (ctx : ('a, _) t) (vsd : Com.variable_space)
    (var : Com.Var.t) (vorg : int) : 'a value =
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

let get_var_value (ctx : ('a, _) t) (m_sp_opt : Com.var_space) (v : Com.Var.t) :
    'a value =
  let vsd, var, vorg = get_var ctx m_sp_opt v in
  let var = if Com.Var.is_table var then get_var_tab ctx var 0 else var in
  get_var_value_org ctx vsd var vorg

let get_var_value_tab (ctx : ('a, _) t) (m_sp_opt : Com.var_space)
    (v : Com.Var.t) (i : int) : 'a value =
  if i < 0 then invalid_arg "Context.get_var_value_tab";
  (* Devrait retourner 0 *)
  let vsd, var, vorg = get_var ctx m_sp_opt v in
  if Com.Var.size var <= i then Undefined
  else if Com.Var.is_table var then
    let var_i = get_var_tab ctx var i in
    get_var_value_org ctx vsd var_i vorg
  else get_var_value_org ctx vsd var vorg

let set_var_ref (ctx : _ t) (var : Com.Var.t) (var_space : Com.variable_space)
    (ref_var : Com.Var.t) (org : int) : unit =
  match var.loc with
  | LocRef (_, i) ->
      ctx.ctx_ref.(ctx.ctx_ref_org + i).var <- var;
      ctx.ctx_ref.(ctx.ctx_ref_org + i).var_space <- var_space;
      ctx.ctx_ref.(ctx.ctx_ref_org + i).ref_var <- ref_var;
      ctx.ctx_ref.(ctx.ctx_ref_org + i).org <- org
  | _ -> assert false

let with_inputs (ctx : ('a, 'b) t) (inputs : 'a Types.value Com.Var.Map.t) :
    unit =
  let default_space =
    ctx.ctx_var_spaces.(ctx.ctx_prog.program_var_space_def.vs_id)
  in
  Com.Var.Map.iter
    (fun (var : Com.Var.t) value ->
      match Com.Var.cat_var_loc var with
      | LocInput -> default_space.input.(Com.Var.loc_idx var) <- value
      | LocComputed -> default_space.computed.(Com.Var.loc_idx var) <- value
      | LocBase -> default_space.base.(Com.Var.loc_idx var) <- value)
    inputs

let with_events (ctx : ('a, 'b) t)
    (events : ('a Types.value, Com.Var.t) Com.event_value StrMap.t list) : unit
    =
  let nbEvt = List.length events in
  let ctx_event_tab = Array.make nbEvt [||] in
  let fold idx (evt : ('a Types.value, Com.Var.t) Com.event_value StrMap.t) =
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
          | Com.Numeric _, false | Com.RefVar _, true -> map.(ef.index) <- ev
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
