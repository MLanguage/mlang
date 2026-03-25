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

val empty_ctx : tracer_ctx:'tc -> Mir.program -> ('a, 'tc) t

val get_var_space : ('a, 'b) t -> Com.var_space -> Com.variable_space

val get_var :
  ('a, 'b) t ->
  Com.var_space ->
  Com.Var.t ->
  Com.variable_space * Com.Var.t * int

val get_var_tab : ('a, 'b) t -> Com.Var.t -> int -> Com.Var.t

val get_var_value_org :
  ('a, 'b) t -> Com.variable_space -> Com.Var.t -> int -> 'a value

val get_var_value : ('a, 'b) t -> Com.var_space -> Com.Var.t -> 'a value

val get_var_value_tab :
  ('a, 'b) t -> Com.var_space -> Com.Var.t -> int -> 'a value

val set_var_ref :
  ('a, 'b) t -> Com.Var.t -> Com.variable_space -> Com.Var.t -> int -> unit
