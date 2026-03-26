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
  ctx_pr_out : Printer.t;
  ctx_pr_err : Printer.t;
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

module Make (N : Number.S) (Tracer : Tracers.S) : sig
  type ctx = (N.t, Tracer.ctx) t

  val empty_ctx :
    ?dbg_info:Dbg_info.t ->
    ?inputs:Com.literal Com.Var.Map.t ->
    ?events:(Com.literal, Com.Var.t) Com.event_value StrMap.t list ->
    Mir.program ->
    ctx

  val get_var_space : ctx -> Com.var_space -> Com.variable_space

  val get_var :
    (N.t, Tracer.ctx) t ->
    Com.var_space ->
    Com.Var.t ->
    Com.variable_space * Com.Var.t * int

  val get_var_tab : ('a, 'b) t -> Com.Var.t -> int -> Com.Var.t

  val get_vars_tab : ('a, 'b) t -> Com.Var.t -> Com.Var.t list

  val get_var_value_org :
    ctx -> Com.variable_space -> Com.Var.t -> int -> N.t value

  val get_var_value :
    (N.t, Tracer.ctx) t -> Com.var_space -> Com.Var.t -> N.t value

  val set_var_ref :
    ctx -> Com.Var.t -> Com.variable_space -> Com.Var.t -> int -> unit

  val set_var_value_org :
    ctx -> Com.variable_space -> Com.Var.t -> int -> N.t value -> unit

  val set_var_value : ctx -> Com.var_space -> Com.Var.t -> N.t value -> unit

  val get_access_value :
    eval:(ctx -> Com.Var.t Com.m_expression -> N.t value) ->
    ctx ->
    Com.Var.t Com.access ->
    N.t value

  val get_access_var :
    eval:(ctx -> Com.Var.t Com.m_expression -> N.t value) ->
    ctx ->
    Com.Var.t Com.access ->
    (Com.variable_space * Com.Var.t * int) option

  val set_access :
    eval:(ctx -> Com.Var.t Com.m_expression -> N.t value) ->
    ctx ->
    Com.Var.t Com.access ->
    Com.Var.t Com.m_expression ->
    unit

  val get_dbg_info : ctx -> Dbg_info.t option
end
