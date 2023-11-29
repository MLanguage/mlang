
open Common

external annee_calc : unit -> int = "ml_annee_calc"

external exec_ench_raw : string -> TGV.t -> unit = "ml_exec_ench"

external exec_verif_raw : string -> TGV.t -> string list = "ml_exec_verif"

external dump_raw_tgv_in : string -> TGV.t -> string list -> unit = "ml_dump_raw_tgv" (* filename, tgv, err *)

let exec_ench ench tgv = exec_ench_raw ench tgv
let exec_verif ench tgv = exec_verif_raw ench tgv

let calcul_primitif tgv = exec_ench "calcul_primitif" tgv
let calcul_primitif_isf tgv = exec_ench "calcul_primitif_isf" tgv
let calcul_primitif_taux tgv = exec_ench "calcul_primitif_taux" tgv
let calcul_correctif tgv = exec_ench "calcul_correctif" tgv

let sauve_base_primitive_penalisee tgv = exec_ench "sauve_base_primitive_penalisee" tgv
let sauve_base_initial tgv = exec_ench "sauve_base_initial" tgv
let sauve_base_1728 tgv = exec_ench "sauve_base_1728" tgv
let sauve_base_anterieure_cor tgv = exec_ench "sauve_base_anterieure_cor" tgv
let sauve_base_premier tgv = exec_ench "sauve_base_premier" tgv

let sauve_base_tl_init tgv = exec_ench "sauve_base_tl_init" tgv
let sauve_base_tl tgv = exec_ench "sauve_base_tl" tgv
let sauve_base_tl_rect tgv = exec_ench "sauve_base_tl_rect" tgv
let sauve_base_tlnunv tgv = exec_ench "sauve_base_tlnunv" tgv

let sauve_base_inr_r9901 tgv = exec_ench "sauve_base_inr_r9901" tgv
let sauve_base_inr_cimr99 tgv = exec_ench "sauve_base_inr_cimr99" tgv
let sauve_base_HR tgv = exec_ench "sauve_base_HR" tgv
let sauve_base_inr_cimr07 tgv = exec_ench "sauve_base_inr_cimr07" tgv
let sauve_base_inr_tlcimr07 tgv = exec_ench "sauve_base_inr_tlcimr07" tgv
let sauve_base_inr_cimr24 tgv = exec_ench "sauve_base_inr_cimr24" tgv
let sauve_base_inr_tlcimr24 tgv = exec_ench "sauve_base_inr_tlcimr24" tgv
let sauve_base_inr_ref tgv = exec_ench "sauve_base_inr_ref" tgv
let sauve_base_inr_ntl tgv = exec_ench "sauve_base_inr_ntl" tgv
let sauve_base_abat98 tgv = exec_ench "sauve_base_abat98" tgv
let sauve_base_inr_intertl tgv = exec_ench "sauve_base_inr_intertl" tgv
let sauve_base_inr_ntl22 tgv = exec_ench "sauve_base_inr_ntl22" tgv
let sauve_base_inr tgv = exec_ench "sauve_base_inr" tgv
let sauve_base_inr_ntl24 tgv = exec_ench "sauve_base_inr_ntl24" tgv
let sauve_base_inr_tl tgv = exec_ench "sauve_base_inr_tl" tgv
let sauve_base_abat99 tgv = exec_ench "sauve_base_abat99" tgv
let sauve_base_inr_tl22 tgv = exec_ench "sauve_base_inr_tl22" tgv
let sauve_base_inr_tl24 tgv = exec_ench "sauve_base_inr_tl24" tgv
let sauve_base_inr_inter22 tgv = exec_ench "sauve_base_inr_inter22" tgv

let sauve_base_majo tgv = exec_ench "sauve_base_majo" tgv
let sauve_base_anterieure tgv = exec_ench "sauve_base_anterieure" tgv
let sauve_base_stratemajo tgv = exec_ench "sauve_base_stratemajo" tgv

let verif_calcul_primitive tgv = exec_verif "verif_calcul_primitive" tgv
let verif_calcul_primitive_isf tgv = exec_verif "verif_calcul_primitive_isf" tgv
let verif_calcul_corrective tgv = exec_verif "verif_calcul_corrective" tgv

let verif_saisie_cohe_primitive tgv = exec_verif "verif_saisie_cohe_primitive" tgv
let verif_saisie_cohe_primitive_isf tgv = exec_verif "verif_saisie_cohe_primitive_isf" tgv
let verif_saisie_cohe_corrective tgv = exec_verif "verif_saisie_cohe_corrective" tgv
let verif_cohe_horizontale tgv = exec_verif "verif_cohe_horizontale" tgv

let traite_double_liquidation_2 tgv = exec_verif "traite_double_liquidation_2" tgv

