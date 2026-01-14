(* A faire : header *)

module Com = M_ir.Com

(** The type values used in the interpreter. *)
type 'f value = Number of 'f | Undefined

type 'f ctx_tmp_var = { mutable var : Com.Var.t; mutable value : 'f value }
(** An association between a variable and its value. *)

type ctx_ref_var = {
  mutable var : Com.Var.t;
  mutable var_space : Com.variable_space;
  mutable ref_var : Com.Var.t;
  mutable org : int;
}

type print_ctx = { mutable indent : int; mutable is_newline : bool }

type 'f ctx_var_space = {
  input : 'f Array.t;
  computed : 'f Array.t;
  base : 'f Array.t;
}

type 'f ctx = {
  ctx_prog : M_ir.Mir.program;
  mutable ctx_target : M_ir.Mir.target;
  mutable ctx_var_space : int;
  ctx_var_spaces : 'f value ctx_var_space Array.t;
  ctx_tmps : 'f ctx_tmp_var Array.t;
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
    ('f value, Com.Var.t) Com.event_value Array.t Array.t list;
}

type 'f pctx = {
  std : Com.print_std;
  ctx : 'f ctx;
  std_fmt : Format.formatter;
  ctx_pr : print_ctx;
}

type run_error =
  | NanOrInf of string * M_ir.Mir.expression Pos.marked
  | StructuredError of
      (string * (string option * Pos.t) list * (unit -> unit) option)

module type S = sig
  type custom_float

  type nonrec ctx = custom_float ctx

  exception RuntimeError of run_error * ctx

  val evaluate_expr :
    ctx -> M_ir.Mir.expression Pos.marked -> custom_float value
  (** Evaluates an expression. *)

  val evaluate_program :
    inputs:Com.literal Com.Var.Map.t ->
    events:(Com.literal, Com.Var.t) Com.event_value StrMap.t list ->
    ctx ->
    unit
  (** Evaluates a whole program. Proper initialisation of inputs and events
      is required before calling this function (through [update_ctx_with_inputs]
      and [update_ctx_with_events]. *)

  val literal_to_value : Com.literal -> custom_float value

  val value_to_literal : custom_float value -> Com.literal
end
