open M_ir
open Context

let raise (ctx : _ Context.t) (err : M_ir.Com.Error.t) (v_opt : string option) =
  (match err.typ with
  | Com.Error.Anomaly -> ctx.ctx_nb_anos <- ctx.ctx_nb_anos + 1
  | Com.Error.Discordance -> ctx.ctx_nb_discos <- ctx.ctx_nb_discos + 1
  | Com.Error.Information -> ctx.ctx_nb_infos <- ctx.ctx_nb_infos + 1);
  let is_blocking =
    err.typ = Com.Error.Anomaly && Pos.unmark err.is_isf = "N"
  in
  ctx.ctx_nb_bloquantes <- (ctx.ctx_nb_bloquantes + if is_blocking then 1 else 0);
  ctx.ctx_anos <- ctx.ctx_anos @ [ (err, v_opt) ];
  is_blocking

let clean (ctx : _ Context.t) =
  ctx.ctx_anos <- [];
  ctx.ctx_nb_anos <- 0;
  ctx.ctx_nb_discos <- 0;
  ctx.ctx_nb_infos <- 0;
  ctx.ctx_nb_bloquantes <- 0

let clean_finalized (ctx : _ Context.t) = ctx.ctx_finalized_anos <- []

let finalize ~mode_corr ctx =
  let mem (ano : Com.Error.t) anos =
    List.fold_left
      (fun res ((a : Com.Error.t), _) ->
        res || Pos.unmark a.name = Pos.unmark ano.name)
      false anos
  in
  if mode_corr then
    let rec merge_anos () =
      match ctx.ctx_anos with
      | [] -> ()
      | ((ano : Com.Error.t), arg) :: discos ->
          let cont =
            if not (mem ano ctx.ctx_finalized_anos) then (
              ctx.ctx_finalized_anos <- ctx.ctx_finalized_anos @ [ (ano, arg) ];
              ano.typ <> Com.Error.Anomaly)
            else true
          in
          ctx.ctx_anos <- discos;
          if cont then merge_anos ()
    in
    merge_anos ()
  else
    let not_in_old_anos (err, _) =
      let name = Pos.unmark err.Com.Error.name in
      not (StrSet.mem name ctx.ctx_archived_anos)
    in
    ctx.ctx_finalized_anos <-
      (let rec merge_anos old_anos new_anos =
         match (old_anos, new_anos) with
         | [], anos | anos, [] -> anos
         | _ :: old_tl, a :: new_tl -> a :: merge_anos old_tl new_tl
       in
       let new_anos = List.filter not_in_old_anos ctx.ctx_anos in
       merge_anos ctx.ctx_finalized_anos new_anos);
    let add_ano res (err, _) = StrSet.add (Pos.unmark err.Com.Error.name) res in
    ctx.ctx_archived_anos <-
      List.fold_left add_ano ctx.ctx_archived_anos ctx.ctx_anos

let export ~mode_corr ctx =
  if mode_corr then
    let rec merge_anos () =
      match ctx.ctx_finalized_anos with
      | [] -> ()
      | ((ano : Com.Error.t), arg) :: fins ->
          if not (StrSet.mem (Pos.unmark ano.name) ctx.ctx_archived_anos) then (
            ctx.ctx_archived_anos <-
              StrSet.add (Pos.unmark ano.name) ctx.ctx_archived_anos;
            ctx.ctx_exported_anos <- ctx.ctx_exported_anos @ [ (ano, arg) ]);
          ctx.ctx_finalized_anos <- fins;
          merge_anos ()
    in
    merge_anos ()
  else (
    ctx.ctx_exported_anos <- ctx.ctx_exported_anos @ ctx.ctx_finalized_anos;
    ctx.ctx_finalized_anos <- [])
