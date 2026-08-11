(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2026 DGFiP - INRIA                                      *)
(*                                                                            *)
(* Ce programme est distribué sous la licence CeCILL-C: vous pouvez le        *)
(* redistribuer et/ou le modifier sous les contraintes de celle-ci.           *)
(*                                                                            *)
(* L'accessibilité au code source et les droits de copie, de modification et  *)
(* de redistribution qui découlent de ce contrat ont pour contrepartie de     *)
(* n'offrir aux utilisateurs qu'une garantie limitée et de ne faire peser sur *)
(* l'auteur du logiciel, le titulaire des droits patrimoniaux et les          *)
(* concédants successifs qu'une responsabilité restreinte.                    *)
(*                                                                            *)
(******************************************************************************)

open M_ir
(** This module describes the interface of a _tracer_ module. A tracer keeps
    assignations in memory, and allow to 'trace' the execution of some M code.
*)

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

module Tracer : S
(** This tracer uses Dbg_info.t to store the execution trace. As such, it
    requires to split information based on dynamic and static properties. (value
    is dynamic, code location is static) *)

module NonTracer : S
(** The Empty tracer, that does nothing. *)
