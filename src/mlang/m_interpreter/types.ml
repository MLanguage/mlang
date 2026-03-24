open M_ir

type 'a value = Number of 'a | Undefined

type 'a ctx_tmp_var = { mutable var : Com.Var.t; mutable value : 'a value }

type ctx_ref_var = {
  mutable var : Com.Var.t;
  mutable var_space : Com.variable_space;
  mutable ref_var : Com.Var.t;
  mutable org : int;
}

type print_ctx = { mutable indent : int; mutable is_newline : bool }

type 'a ctx_var_space = {
  input : 'a value Array.t;
  computed : 'a value Array.t;
  base : 'a value Array.t;
}

type ('a, 'tc) ctx = {
  ctx_prog : Mir.program;
  mutable ctx_target : Mir.target;
  mutable ctx_var_space : int;
  ctx_var_spaces : 'a ctx_var_space Array.t;
  ctx_tmps : 'a ctx_tmp_var Array.t;
  mutable ctx_tmps_org : int;
  ctx_ref : ctx_ref_var Array.t;
  mutable ctx_ref_org : int;
  ctx_tab_map : Com.Var.t Array.t;
  ctx_pr_out : print_ctx;
  ctx_pr_err : print_ctx;
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

type run_error =
  | NanOrInf of string * Mir.expression Pos.marked
  | StructuredError of
      (string * (string option * Pos.t) list * (unit -> unit) option)
