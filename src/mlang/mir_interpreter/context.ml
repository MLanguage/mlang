open Types

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
