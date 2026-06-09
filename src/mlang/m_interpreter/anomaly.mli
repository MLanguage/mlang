(** Implementation of anomaly management in the interpretation context. *)

val raise : _ Context.t -> M_ir.Com.Error.t -> string option -> bool
(** Adds the anomaly to the context and returns [true] if the said anomaly is
    blocking, [false] otherwise. *)

val clean : _ Context.t -> unit
(** Cleans the context from its unfinalized and unarchived anomalies. *)

val clean_finalized : _ Context.t -> unit
(** Cleans the context from its finalized anomalies. *)

val finalize : mode_corr:bool -> _ Context.t -> unit
(** Moves the raised anomalies to the finalized anomalies (and the archived
    anomalies if [mode_corr] is [true]). *)

val export : mode_corr:bool -> _ Context.t -> unit
(** Moves the finalized anomalies to the exported anomalies (and the archived
    anomalies if [mode_corr] is [true]). *)

val nb_anomalies : _ Context.t -> int
(** Returns the amount of [raise]d anomalies. *)

val nb_discordances : _ Context.t -> int
(** Returns the amount of [raise]d discordances. *)

val nb_informatives : _ Context.t -> int
(** Returns the amount of [raise]d informations. *)

val nb_bloquantes : _ Context.t -> int
(** Returns the amount of [raise]d blocking anomalies. *)
