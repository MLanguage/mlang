open M_ir
open Types

type 'a ctx_tmp_var = { mutable var : Com.Var.t; mutable value : 'a value }
(** Temporary variables are represented by their representant ([var]) and their
    [value]. *)

type ctx_ref_var = {
  mutable var : Com.Var.t;  (** The variable name *)
  mutable var_space : Com.variable_space;  (** Its original var space *)
  mutable ref_var : Com.Var.t;  (** The referenced variable *)
  mutable org : int;
      (** Its position in the array it is in. For TGV variables, it is 0. For
          temporary variables, it is the position in the context temporary
          variable array. For references, it uses the [org] from its original
          counterpart. *)
}
(** A reference to another variable.*)

type 'a ctx_var_space = {
  input : 'a value Array.t;
  computed : 'a value Array.t;
  base : 'a value Array.t;
}
(** A full set of variables representing a variable space. *)

type ('a, 'tc) t = {
  ctx_prog : Mir.program;  (** The interpreted program. *)
  mutable ctx_target : Mir.target;  (** The current target. *)
  mutable ctx_var_space : int;
      (** The var space in which the current target is working on. *)
  ctx_var_spaces : 'a ctx_var_space Array.t;  (** The var spaces array. *)
  ctx_tmps : 'a ctx_tmp_var Array.t;
      (** The temporary variables. Its size is the maximum number of temporary
          variables needed for the interpretation, and the array cells
          themselves are not mutated. This way, we don't need to allocate new
          variables, we just need to overwrite them. *)
  mutable ctx_tmps_org : int;
      (** The size of the [ctx_tmps] sub-array refering to the currently used
          temprary variables. Cells after [ctx_tmps_org] in [ctx_tmps] do not
          represent variables, only unused-but-already-allocated cells. *)
  ctx_ref : ctx_ref_var Array.t;
      (** Similar to [ctx_tmps], but for references. *)
  mutable ctx_ref_org : int;
      (** Similar to [ctx_tmps_org], but for references. *)
  ctx_tab_map : Com.Var.t Array.t;
      (** All concatenated tables of the program. The first index of each table
          can be accessed with [1 + Com.Var.loc_tab_idx]. Check
          [Context.get_var_tab]. *)
  ctx_pr_out : Printer.t;  (** A printer for the standard output stream. *)
  ctx_pr_err : Printer.t;  (** A printer for the error output stream. *)
  mutable ctx_anos : (Com.Error.t * string option) list;
      (** Errors raised during the interpretation. *)
  mutable ctx_nb_anos : int;
      (** Number of anomalies in the previous error list.*)
  mutable ctx_nb_discos : int;
      (** Number of discordances in the previous error list. *)
  mutable ctx_nb_infos : int;
      (** Number of informations in the previous error list. *)
  mutable ctx_nb_bloquantes : int;  (** Number of blocking anomalies. *)
  mutable ctx_archived_anos : StrSet.t;  (** Archived anomalies *)
  mutable ctx_finalized_anos : (Com.Error.t * string option) list;
      (** Finalized errors. *)
  mutable ctx_exported_anos : (Com.Error.t * string option) list;
      (** Exported errors. *)
  mutable ctx_events :
    ('a value, Com.Var.t) Com.event_value Array.t Array.t list;
      (** Current list of events. *)
  tracer_ctx : 'tc;  (** The context of the interpreter tracer. *)
}
(** The interpretation context. *)

module Make (N : Number.S) (Tracer : Tracers.S) : sig
  type ctx = (N.t, Tracer.ctx) t

  val empty_ctx :
    ?dbg_info:Dbg_info.t ->
    ?inputs:Com.literal Com.Var.Map.t ->
    ?events:(Com.literal, Com.Var.t) Com.event_value StrMap.t list ->
    Mir.program ->
    ctx
  (** Builds an empty context from a Mir program. A given set of inputs and
      events can be given, otherwise the program will start with both
      uninitialized. If [dbg_info] is not provided, it will use a default tracer
      context. *)

  val get_var_space : ctx -> Com.var_space -> Com.variable_space
  (** From a variable space identifier, returns the whoe variable space data. *)

  val get_var :
    ctx -> Com.var_space -> Com.Var.t -> Com.variable_space * Com.Var.t * int
  (** From a variable and a variable space, fetches the actual variable in the
      context, data about the variable space and its offset if it is temporary.
      The output variable is the same than the input except if it is a
      reference, in which case the returned variable is the referenced one. *)

  val get_var_tab : ctx -> Com.Var.t -> int -> Com.Var.t
  (** Variable must be a table. Returns the variable associated to the ith cell
      of the table. *)

  val get_vars_tab : ctx -> Com.Var.t -> Com.Var.t list
  (** Variable must be a table. Returns the list of variables for each cell of
      the table. *)

  val get_var_value_org :
    ctx -> Com.variable_space -> Com.Var.t -> int -> N.t value

  val get_var_value : ctx -> Com.var_space -> Com.Var.t -> N.t value
  (** Returns the value of a variable. *)

  val set_var_ref :
    ctx -> Com.Var.t -> Com.variable_space -> Com.Var.t -> int -> unit
  (** [set_var_ref ctx var vs ref_var org]

      Sets [var] as a reference to variable [ref_var]. *)

  val set_var_value_org :
    ctx -> Com.variable_space -> Com.Var.t -> int -> N.t value -> unit

  val set_var_value : ctx -> Com.var_space -> Com.Var.t -> N.t value -> unit
  (** Assigns a value to a given variable. *)

  val get_access_value :
    eval:(ctx -> Com.Var.t Com.m_expression -> N.t value) ->
    ctx ->
    Com.Var.t Com.access ->
    N.t value
  (** Accesses are similar to variables, except they depend on expressions.
      Table accesses are represented as [T[<expr>]], and field accesses as
      [champ_evenement(...)] that both require to evaluate an expression.

      This function evaluates the possible expressions in an access and returns
      the corresponding value. *)

  val get_access_var :
    eval:(ctx -> Com.Var.t Com.m_expression -> N.t value) ->
    ctx ->
    Com.Var.t Com.access ->
    (Com.variable_space * Com.Var.t * int) option
  (** Same as [get_access_value], but returns a variable. Its value can then be
      accessed with get_var_value_org. *)

  val set_access :
    eval:(ctx -> Com.Var.t Com.m_expression -> N.t value) ->
    ctx ->
    Com.Var.t Com.access ->
    N.t value ->
    unit
  (** Assigns a value to an access. *)
end
