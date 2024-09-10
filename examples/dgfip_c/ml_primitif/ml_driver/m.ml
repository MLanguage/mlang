
open Common

external annee_calc : unit -> int = "ml_annee_calc"

external exec_ench_raw : string -> TGV.t -> unit = "ml_exec_ench"

external exec_verif_raw : string -> TGV.t -> string list = "ml_exec_verif"

external dump_raw_tgv_in : string -> TGV.t -> string list -> unit = "ml_dump_raw_tgv" (* filename, tgv, err *)

external export_errs : TGV.t -> unit = "ml_export_errs"

let exec_verif ench tgv = exec_verif_raw ench tgv

let enchainement_primitif tgv = exec_verif "enchainement_primitif" tgv

