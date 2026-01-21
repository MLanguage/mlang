val raise : 'a Types.ctx -> M_ir.Com.Error.t -> string option -> bool
(** Adds the anomaly to the context and returns [true] if the said anomaly is
    blocking, [false] otherwise. *)

val clean : 'a Types.ctx -> unit
(** Cleans the context from its unfinalized and unarchived anomalies. *)

val clean_finalized : 'a Types.ctx -> unit
(** Cleans the context from its finalized anomalies. *)

val finalize : mode_corr:bool -> 'a Types.ctx -> unit
(** Moves the raised anomalies to the finalized anomalies (and the archived
    anomalies if [mode_corr] is [true]). *)

val export : mode_corr:bool -> 'a Types.ctx -> unit
(** Moves the finalized anomalies to the exported anomalies (and the archived
    anomalies if [mode_corr] is [true]). *)
