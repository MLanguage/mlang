(** This module describes the interface of a _tracer_ module. A tracer keeps
    assignations in memory, and allow to 'trace' the execution of some M code.
*)

open M_ir

module type S = sig
  type ctx
  (** The execution context *)

  val empty_ctx : Dbg_info.t option -> ctx
  (** [empty_ctx infos] inits a tracing context. infos might contain the input
      variables. *)

  val register_temp : ctx -> Com.literal -> Com.Var.t -> unit
  (** [register_temp ctx lit var] registers a temporary variable [var] of value
      [lit] *)

  val register_access :
    ctx ->
    Com.Var.t Com.expression Pos.marked ->
    Com.Var.t Com.access ->
    Com.Var.t ->
    Com.Var.t IntMap.t ->
    Com.literal ->
    (Com.Var.t Com.m_expression -> string) ->
    unit
  (** [register_access ctx expr access var program_dict value eval_m_index]
      registers an access in the tracer, and adds it to [ctx]. An access
      corresponds to an assignation. *)

  val update_execution_ctx : ctx -> int option -> string -> unit
  (** [update_execution ctx rule_id target_name] updates the tracing context
      [ctx] with the corresponding context (target, rule). *)

  val get_dbg_info : ctx -> Dbg_info.t option
  (** [get_dbg_info ctx] returns the tracing info stored in the context. *)

  val register_ano : ctx -> Com.Error.t Pos.marked -> unit
  (** [register_ano ctx err] registers an anomaly to the trace. *)
end

module Tracer : S = struct
  (** This tracer uses Dbg_info.t to store the execution trace. As such, it
      requires to split information based on dynamic and static properties.
      (value is dynamic, code location is static) *)
  type ctx_exec_ctx = CtxUndefined | CtxTarget of string | CtxRule of int

  type ctx = { mutable dbg_info : Dbg_info.t; mutable exec_ctx : ctx_exec_ctx }

  let get_dbg_info ctx = Some ctx.dbg_info

  let empty_ctx dbg_info =
    let dbg_info =
      Option.value dbg_info ~default:(Dbg_info.make_empty ~aliases:StrMap.empty)
    in
    { dbg_info; exec_ctx = CtxUndefined }

  let get_rule ctx =
    match ctx.exec_ctx with
    | CtxRule i -> Dbg_info.Origin.Rule i
    | CtxTarget s -> Dbg_info.Origin.Target s
    | CtxUndefined ->
        Errors.raise_error
          "Expression is located neither in a rule nor in a target."

  let register_temp ctx lit var =
    let open Dbg_info in
    let tick = Tick.tick () in
    let rule = get_rule ctx in
    let value = lit in
    let info = Info.make_from_var tick var rule value None false in
    let dbg_info = Dbg_info.register ctx.dbg_info info in
    ctx.dbg_info <- dbg_info

  let trace_deps deps dbg_info eval_m_index =
    let open Dbg_info in
    let trace_dep (ticks, dbg_info) dep =
      match dep with
      | Com.V var ->
          let name = Com.Var.name_str var in
          (* For now, we add uninstantiated depedencies as undefined *)
          begin match TickMap.find_opt name dbg_info.ledger with
          | None ->
              let tick = Tick.tick () in
              let rule = Origin.Declared in
              let value = Com.Undefined in
              let info = Info.make_from_var tick var rule value None false in
              let dbg_info = Dbg_info.register dbg_info info in
              (tick :: ticks, dbg_info)
          | Some tick -> (tick :: ticks, dbg_info)
          end
      | Const const ->
          let name = const.Com.id in
          begin match TickMap.find_opt name dbg_info.ledger with
          | Some tick -> (tick :: ticks, dbg_info)
          | None ->
              let tick = Tick.tick () in
              let info =
                Info.make tick name const.pos Const const.Com.value None false
              in
              let const = Const.make_from_pos name const.Com.value const.pos in
              let consts = Tick.Map.add tick const dbg_info.consts in
              let ledger = StrMap.add name tick dbg_info.ledger in
              let dbg_info = Dbg_info.register dbg_info info in
              let dbg_info = { dbg_info with consts; ledger } in
              (tick :: ticks, dbg_info)
          end
      | Tab (var, m_i) ->
          let name = Com.Var.name_str var in
          let idx_str = eval_m_index m_i in
          let name = Format.asprintf "%s[%s]" name idx_str in
          begin match TickMap.find_opt name dbg_info.ledger with
          | None ->
              let tick = Tick.tick () in
              let rule = Origin.Declared in
              let lit_value = Com.Undefined in
              let info =
                Info.make_from_var tick var rule lit_value None false
              in
              let dbg_info = Dbg_info.register dbg_info info in
              (tick :: ticks, dbg_info)
          | Some tick -> (tick :: ticks, dbg_info)
          end
      | LiteralDep _lit -> (ticks, dbg_info)
    in
    List.fold_left trace_dep ([], dbg_info) deps

  let register_access ctx vexpr access v program_dict value eval_m_index =
    let open Dbg_info in
    let deps = Com.get_used_variables @@ Pos.unmark vexpr in
    let ticks, dbg_info = trace_deps deps ctx.dbg_info eval_m_index in
    (* Create the tick for this  variable after the deps so that they are
       in the right order on marple side. *)
    let tick = Tick.tick () in
    let access_name name =
      match access with
      | Com.VarAccess _ -> (
          match Com.Var.get_table_cell v with
          | None -> name
          | Some (cell_id, i) ->
              let var = IntMap.find cell_id program_dict in
              let tab = Com.Var.name_str var in
              Format.asprintf "%s[%d]" tab i)
      | Com.TabAccess ((_, v), m_i) ->
          let name = Com.Var.name_str v in
          let idx_str = eval_m_index m_i in
          Format.asprintf "%s[%s]" name idx_str
      | Com.FieldAccess (_, _, _, _) -> name
    in
    let name = access_name @@ Com.Var.name_str v in
    let is_input =
      match Com.Var.cat_var_loc v with
      | Com.CatVar.LocInput -> true
      | (exception Failure _) | _ -> false
    in
    let pos = Pos.get vexpr in
    let rule_id = get_rule ctx in
    let descr =
      match Com.Var.descr_str v with exception _ -> None | descr -> Some descr
    in
    let info = Info.make tick name pos rule_id value descr is_input in
    let dbg_info = Dbg_info.register dbg_info info in
    let vert = Dbg_info.Graph.V.create tick in
    let graph = dbg_info.graph in
    List.iter
      (fun deptick ->
        let dep_vert = Dbg_info.Graph.V.create deptick in
        Dbg_info.Graph.add_edge graph vert dep_vert)
      ticks;
    ctx.dbg_info <- { dbg_info with graph }

  let update_execution_ctx ctx rule_id target_name =
    match rule_id with
    | None -> ctx.exec_ctx <- CtxTarget target_name
    | Some rule_id -> ctx.exec_ctx <- CtxRule rule_id

  let register_ano ctx (m_err : Com.Error.t Pos.marked) =
    let (Pos.Mark (err, pos)) = m_err in
    let (Pos.Mark (name, declared_pos)) = err.name in
    let origin = Dbg_info.Origin.make_from_pos declared_pos Declared in
    let raised_origin = Dbg_info.Origin.make_from_pos pos Anomaly in
    let anomaly : Dbg_info.anomaly = { name; origin; raised_origin } in
    let dbg_info = ctx.dbg_info in
    let anomalies = anomaly :: dbg_info.anomalies in
    ctx.dbg_info <- { dbg_info with anomalies }
end

(** The Empty tracer, that does nothing. *)
module NonTracer : S = struct
  type ctx = unit

  let empty_ctx _ = ()

  let register_temp _ _ _ = ()

  let register_access _ _ _ _ _ _ _ = ()

  let register_ano _ _ = ()

  let update_execution_ctx _ _ _ = ()

  let get_dbg_info _ = None
end
