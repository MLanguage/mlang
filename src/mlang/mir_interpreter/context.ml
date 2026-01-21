open Types

module type S = sig
  type custom_float

  val get_var_space :
    custom_float Types.ctx -> M_ir.Com.var_space -> M_ir.Com.variable_space

  val get_var :
    custom_float Types.ctx ->
    M_ir.Com.var_space ->
    M_ir.Com.Var.t ->
    M_ir.Com.variable_space * M_ir.Com.Var.t * int

  val get_var_tab :
    custom_float Types.ctx -> M_ir.Com.Var.t -> int -> M_ir.Com.Var.t

  val get_var_value_org :
    custom_float Types.ctx ->
    M_ir.Com.variable_space ->
    M_ir.Com.Var.t ->
    int ->
    custom_float Types.value

  val get_var_value :
    custom_float Types.ctx ->
    M_ir.Com.var_space ->
    M_ir.Com.Var.t ->
    custom_float Types.value

  val get_var_value_tab :
    custom_float Types.ctx ->
    M_ir.Com.var_space ->
    M_ir.Com.Var.t ->
    int ->
    custom_float Types.value

  val set_var_ref :
    custom_float Types.ctx ->
    M_ir.Com.Var.t ->
    M_ir.Com.variable_space ->
    M_ir.Com.Var.t ->
    int ->
    unit

  val get_access_value :
    eval:
      (custom_float Types.ctx ->
      M_ir.Com.Var.t M_ir.Com.m_expression ->
      custom_float Types.value) ->
    custom_float Types.ctx ->
    M_ir.Com.Var.t M_ir.Com.access ->
    custom_float Types.value

  val get_access_var :
    eval:
      (custom_float Types.ctx ->
      M_ir.Com.Var.t M_ir.Com.m_expression ->
      custom_float Types.value) ->
    custom_float Types.ctx ->
    M_ir.Com.Var.t M_ir.Com.access ->
    (M_ir.Com.variable_space * M_ir.Com.Var.t * int) option

  val set_var_value_org :
    custom_float Types.ctx ->
    M_ir.Com.variable_space ->
    M_ir.Com.Var.t ->
    int ->
    custom_float Types.value ->
    unit

  val set_var_value :
    custom_float Types.ctx ->
    M_ir.Com.var_space ->
    M_ir.Com.Var.t ->
    custom_float Types.value ->
    unit

  val set_var_value_tab :
    custom_float Types.ctx ->
    M_ir.Com.var_space ->
    M_ir.Com.Var.t ->
    int ->
    custom_float Types.value ->
    unit

  val set_access :
    eval:
      (custom_float Types.ctx ->
      M_ir.Com.Var.t M_ir.Com.m_expression ->
      custom_float Types.value) ->
    custom_float Types.ctx ->
    M_ir.Com.Var.t M_ir.Com.access ->
    custom_float Types.value ->
    unit
end

let update_ctx_with_inputs (ctx : 'a ctx)
    (value_inputs : 'a value Com.Var.Map.t) : unit =
  (* let value_inputs = *)
  (*   Com.Var.Map.mapi *)
  (*     (fun v l -> *)
  (*       match l with *)
  (*       | Com.Undefined -> Undefined *)
  (*       | Com.Float f -> Number (N.of_float_input v f)) *)
  (*     inputs *)
  (* in *)
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

let update_ctx_with_events (ctx : 'a ctx)
    (events : ('a value, Com.Var.t) Com.event_value StrMap.t list) : unit =
  let nbEvt = List.length events in
  let ctx_event_tab = Array.make nbEvt [||] in
  let fold idx (evt : ('a value, Com.Var.t) Com.event_value StrMap.t) =
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

let empty_ctx ?inputs ?events (p : M_ir.Mir.program) : 'a ctx =
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
  let res =
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
    }
  in
  Option.iter (update_ctx_with_inputs res) inputs;
  Option.iter (update_ctx_with_events res) events;
  res

module Make (N : Number.S) = struct
  let get_var_space (ctx : N.t ctx) (m_sp_opt : Com.var_space) =
    let i_sp =
      match m_sp_opt with None -> ctx.ctx_var_space | Some (_, i_sp) -> i_sp
    in
    IntMap.find i_sp ctx.ctx_prog.program_var_spaces_idx

  let get_var (ctx : N.t ctx) (m_sp_opt : Com.var_space) (var : Com.Var.t) :
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

  let get_var_tab (ctx : N.t ctx) (var : Com.Var.t) (i : int) : Com.Var.t =
    match Com.Var.get_table var with
    | Some _ -> ctx.ctx_tab_map.(Com.Var.loc_tab_idx var + 1 + i)
    | None -> assert false

  let get_var_value_org (ctx : N.t ctx) (vsd : Com.variable_space)
      (var : Com.Var.t) (vorg : int) : N.t value =
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

  let get_var_value (ctx : N.t ctx) (m_sp_opt : Com.var_space) (v : Com.Var.t) :
      N.t value =
    let vsd, var, vorg = get_var ctx m_sp_opt v in
    let var = if Com.Var.is_table var then get_var_tab ctx var 0 else var in
    get_var_value_org ctx vsd var vorg

  let get_var_value_tab (ctx : N.t ctx) (m_sp_opt : Com.var_space)
      (v : Com.Var.t) (i : int) : N.t value =
    let vsd, var, vorg = get_var ctx m_sp_opt v in
    if i < 0 then Number (N.zero ())
    else if Com.Var.size var <= i then Undefined
    else if Com.Var.is_table var then
      let var_i = get_var_tab ctx var i in
      get_var_value_org ctx vsd var_i vorg
    else get_var_value_org ctx vsd var vorg

  let set_var_ref (ctx : N.t ctx) (var : Com.Var.t)
      (var_space : Com.variable_space) (ref_var : Com.Var.t) (org : int) : unit
      =
    match var.loc with
    | LocRef (_, i) ->
        ctx.ctx_ref.(ctx.ctx_ref_org + i).var <- var;
        ctx.ctx_ref.(ctx.ctx_ref_org + i).var_space <- var_space;
        ctx.ctx_ref.(ctx.ctx_ref_org + i).ref_var <- ref_var;
        ctx.ctx_ref.(ctx.ctx_ref_org + i).org <- org
    | _ -> assert false

  let rec get_access_value ~eval ctx access =
    match access with
    | Com.VarAccess (m_sp_opt, v) -> get_var_value ctx m_sp_opt v
    | Com.TabAccess ((m_sp_opt, v), m_idx) -> (
        match eval ctx m_idx with
        | Number z ->
            let i = Int64.to_int @@ N.to_int z in
            get_var_value_tab ctx m_sp_opt v i
        | Undefined -> Undefined)
    | Com.FieldAccess (m_sp_opt, e, _, j) -> (
        match eval ctx e with
        | Number z ->
            let i = Int64.to_int @@ N.to_int z in
            let events = List.hd ctx.ctx_events in
            if 0 <= i && i < Array.length events then
              match events.(i).(j) with
              | Com.Numeric n -> n
              | Com.RefVar v -> get_var_value ctx m_sp_opt v
            else Undefined
        | Undefined -> Undefined)

  and get_access_var ~eval ctx access =
    match access with
    | Com.VarAccess (m_sp_opt, v) ->
        let vsd, v, vorg = get_var ctx m_sp_opt v in
        Some (vsd, v, vorg)
    | Com.TabAccess ((m_sp_opt, m_v), m_i) -> (
        match eval ctx m_i with
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
        match eval ctx m_e with
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

  and set_var_value_org (ctx : N.t ctx) (vsd : Com.variable_space)
      (var : Com.Var.t) (vorg : int) (value : N.t value) : unit =
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

  and set_var_value (ctx : N.t ctx) (m_sp_opt : Com.var_space) (var : Com.Var.t)
      (value : N.t value) : unit =
    let vsd, v, vorg = get_var ctx m_sp_opt var in
    if Com.Var.is_table v then
      for i = 0 to Com.Var.size v - 1 do
        let v_i = get_var_tab ctx v i in
        set_var_value_org ctx vsd v_i vorg value
      done
    else set_var_value_org ctx vsd v vorg value

  and set_var_value_tab (ctx : N.t ctx) (m_sp_opt : Com.var_space)
      (v : Com.Var.t) (i : int) (value : N.t value) : unit =
    let vsd, var, vorg = get_var ctx m_sp_opt v in
    if 0 <= i && i < Com.Var.size var then
      if Com.Var.is_table var then
        let var_i = get_var_tab ctx var i in
        set_var_value_org ctx vsd var_i vorg value
      else set_var_value_org ctx vsd var vorg value

  and set_access ~eval ctx access value =
    match access with
    | Com.VarAccess (m_sp_opt, v) -> set_var_value ctx m_sp_opt v value
    | Com.TabAccess ((m_sp_opt, v), m_idx) -> (
        match eval ctx m_idx with
        | Number z ->
            let i = Int64.to_int @@ N.to_int z in
            set_var_value_tab ctx m_sp_opt v i value
        | Undefined -> ())
    | Com.FieldAccess (m_sp_opt, e, _, j) -> (
        match eval ctx e with
        | Number z -> (
            let i = Int64.to_int @@ N.to_int z in
            let events = List.hd ctx.ctx_events in
            if 0 <= i && i < Array.length events then
              match events.(i).(j) with
              | Com.Numeric _ -> events.(i).(j) <- Com.Numeric value
              | Com.RefVar v -> set_var_value ctx m_sp_opt v value)
        | Undefined -> ())
end
