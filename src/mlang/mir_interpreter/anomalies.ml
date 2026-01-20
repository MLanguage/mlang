open Types

let raise ctx err v_opt =
  (match err.Com.Error.typ with
  | Com.Error.Anomaly -> ctx.ctx_nb_anos <- ctx.ctx_nb_anos + 1
  | Com.Error.Discordance -> ctx.ctx_nb_discos <- ctx.ctx_nb_discos + 1
  | Com.Error.Information -> ctx.ctx_nb_infos <- ctx.ctx_nb_infos + 1);
  let is_blocking =
    err.typ = Com.Error.Anomaly && Pos.unmark err.is_isf = "N"
  in
  ctx.ctx_nb_bloquantes <- (ctx.ctx_nb_bloquantes + if is_blocking then 1 else 0);
  ctx.ctx_anos <- ctx.ctx_anos @ [ (err, v_opt) ];
  is_blocking

let clean (ctx : 'a ctx) =
  ctx.ctx_anos <- [];
  ctx.ctx_nb_anos <- 0;
  ctx.ctx_nb_discos <- 0;
  ctx.ctx_nb_infos <- 0;
  ctx.ctx_nb_bloquantes <- 0

let clean_finalized (ctx : 'a ctx) = ctx.ctx_finalized_anos <- []

let finalize ~mode_corr (ctx : 'a ctx) =
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
  else (
    clean_finalized ctx;
    let rec merge_anos () =
      match ctx.ctx_anos with
      | [] -> ctx.ctx_finalized_anos <- List.rev ctx.ctx_finalized_anos
      | ((ano : Com.Error.t), arg) :: discos ->
          if not (StrSet.mem (Pos.unmark ano.name) ctx.ctx_archived_anos) then (
            ctx.ctx_archived_anos <-
              StrSet.add (Pos.unmark ano.name) ctx.ctx_archived_anos;
            ctx.ctx_finalized_anos <- (ano, arg) :: ctx.ctx_finalized_anos);
          ctx.ctx_anos <- discos;
          merge_anos ()
    in
    merge_anos ())

let export ~mode_corr (ctx : 'a ctx) =
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
    clean_finalized ctx)
