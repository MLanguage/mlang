
open Common

external annee_calc : unit -> int = "ml_annee_calc"

external exec_ench_raw : string -> (string * int * float) list -> (string * int * float) list = "ml_exec_ench"

external exec_verif_raw : string -> (string * int * float) list -> (string * int * float) list = "ml_exec_verif"

let tgv_to_list tgv =
  IndexedVarMap.fold (fun code id m l ->
      let id = match id with None -> -1 | Some id -> id in
      (code, id, m) :: l
    ) tgv []

let list_to_tgv tgv_list =
  List.fold_left (fun tgv (code, id, montant) ->
      let id = if id < 0 then None else Some id in
      IndexedVarMap.add code id montant tgv) IndexedVarMap.empty tgv_list

let exec_ench ench tgv =
  list_to_tgv (exec_ench_raw ench (tgv_to_list tgv))

let exec_verif verif tgv =
  list_to_tgv (exec_verif_raw verif (tgv_to_list tgv))

let calcul_primitif tgv = exec_ench "calcul_primitif" tgv
let calcul_irisf tgv = exec_ench "calcul_irisf" tgv
let calcul_primitif_isf tgv = exec_ench "calcul_primitif_isf" tgv
let calcul_primitif_taux tgv = exec_ench "calcul_primitif_taux" tgv
let calcul_correctif tgv = exec_ench "calcul_correctif" tgv

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



type traitement =
  | Primitif
  | Correctif

let rec traite_double_liquidation_2 tgv traitement =
  (* modcat is always 1, so this function does nothing *)
  (* let tgv = modulation_taxation tgv in *)
  traite_double_liquidation_pvro tgv traitement

and traite_double_liquidation_pvro tgv traitement =
  let tgv =
    if TGV.defined tgv "3WG" then
      let tgv = TGV.set_bool tgv "FLAG_PVRO" true in
      let tgv = traite_double_liquidation_exit_taxe tgv traitement in
      TGV.internal_copy ~ignore_undefined:true tgv [ "IAD11", "IPVRO" ]
    else
      tgv
  in
  let tgv = TGV.set_bool tgv "FLAG_PVRO" false in
  traite_double_liquidation_exit_taxe tgv traitement

and traite_double_liquidation_exit_taxe tgv traitement =
  let traite_3W_RW tgv flag_EXIT code_3W code_RW
      code_3WNEG code_NAPTIR3W code_CHR3W code_ID113W =
    if TGV.defined tgv code_3W || TGV.defined tgv code_RW then
      begin
        let tgv = TGV.set_bool tgv code_3WNEG false in (* do we have to reset it right now ? *)
        let tgv = TGV.set_int tgv "FLAG_EXIT" flag_EXIT in
        let tgv = traite_double_liquidation3 tgv traitement false in
        let tgv = TGV.copy_abs tgv "NAPTIR" code_NAPTIR3W code_3WNEG in
        let tgv = TGV.internal_copy ~ignore_undefined:true tgv
            [ "IHAUTREVT", code_CHR3W; "ID11", code_ID113W ] in
        TGV.set_int tgv "FLAG_EXIT" 0
      end
    else
      tgv
  in
  let tgv = traite_3W_RW tgv 1 "3WB" "RWB"
      "FLAG_3WBNEG" "NAPTIR3WB" "CHR3WB" "ID113WB" in
  let tgv = traite_3W_RW tgv 2 "3WA" "RWA"
      "FLAG_3WANEG" "NAPTIR3WA" "CHR3WA" "ID113WA" in
  let tgv = TGV.set_bool tgv "FLAG_BAREM" true in
  let tgv = traite_double_liquidation3 tgv traitement true in
  let tgv = TGV.internal_copy ~ignore_undefined:true tgv
      [ "RASTXFOYER", "BARTXFOYER";
        "RASTXDEC1", "BARTXDEC1";
        "RASTXDEC2", "BARTXDEC2";
        "INDTAZ", "BARINDTAZ";
        "IRTOTAL", "BARIRTOTAL" ] in
  let tgv = TGV.copy_abs tgv "IITAZIR" "BARIITAZIR" "FLAG_BARIITANEG" in
  let tgv = TGV.set_bool tgv "FLAG_BAREM" false in
  traite_double_liquidation3 tgv traitement true

and traite_double_liquidation3 tgv traitement p_is_calcul_acomptes =
  (* aucune de ces variables ne sont plus utilisées par le code M actuel *)
  let tgv = TGV.set_bool_list tgv
      [ "FLAG_ACO", false; "NEGACO", false;
        "AVFISCOPBIS", false; "DIFTEOREEL", false ] in
  let tgv =
    if traitement = Primitif then
      let tgv = TGV.set_bool tgv "PREM8_11" false in
      article_1731_bis tgv traitement
    else tgv
  in
  let calcul_acomptes = is_calcul_acomptes tgv in
  let calcul_avfisc = is_calcul_avfisc tgv in
  let tgv, montant_8ZG =
    if calcul_avfisc then
      let m = TGV.get_def tgv "8ZG" 0.0 in
      TGV.reset tgv "8ZG", m
    else
      tgv, 0.0
  in
  let tgv =
    if calcul_acomptes && p_is_calcul_acomptes then
      begin
        let vars_ac = VarDict.filter (fun code var ->
            match var.Var.domaine with
            | Revenu | RevenuCorr -> var.Var.acompte = false
            | _ -> false)
        in
        let vars_ac = fst (List.split (StrMap.bindings vars_ac)) in
        let sauve_ac = TGV.get_map_opt tgv vars_ac in (* AC_GetCodesAcompte *)
        let tgv = TGV.reset_list tgv vars_ac in (* AC_SupprimeCodesAcomptes *)
        let tgv =
          if calcul_avfisc then calcule_acomptes_avfisc tgv traitement 0.0
          else calcule_acomptes tgv traitement
        in
        TGV.set_map tgv sauve_ac (* AC_ReplaceCodesAcomptes *)
      end
    else
      tgv
  in
  let tgv =
    if calcul_avfisc then
      let tgv = TGV.set_bool_list tgv
          [ "AVFISCOPBIS", false; "DIFTEOREEL", false; "INDTEO", true ] in
      let tgv = calcule_avfiscal tgv traitement in
      let tgv = TGV.set_bool_list tgv
          [ "INDTEO", false; "NEGREEL", true; "NAPREEL", false ] in
      tgv
    else
      tgv
  in
  let tgv =
    if calcul_avfisc && montant_8ZG <> 0.0 then TGV.set tgv "8ZG" montant_8ZG
    else tgv
  in
  let tgv = TGV.set_list tgv [ "ACO_MTAP", 0.0; "NEGACO", 0.0 ] in
  let tgv = calcul_primitif_isf tgv in
  let tgv = calcul_prim_corr tgv traitement in
  let tgv = calcul_primitif_taux tgv in
  if traitement = Primitif then verif_calcul_primitive tgv
  else tgv

and calcul_prim_corr tgv traitement =
  if traitement = Primitif then calcul_primitif tgv
  else calcul_correctif tgv

and article_1731_bis tgv traitement =
  if traitement = Primitif then
    match TGV.get_int_opt tgv "CMAJ" with
    | Some (8 | 11) ->
        TGV.set_bool_list tgv [ "ART1731BIS", true; "PREM8_11", true ]
    | _ ->
        TGV.set_bool tgv "ART1731BIS" false
  else
    tgv

and is_calcul_acomptes tgv =
  let vars_ac = VarDict.filter (fun code var ->
      match var.Var.domaine with
      | Revenu | RevenuCorr -> var.Var.acompte = false
      | _ -> false)
  in
  let vars_ac = fst (List.split (StrMap.bindings vars_ac)) in
  List.exists (fun code -> TGV.defined tgv code) vars_ac

and is_calcul_avfisc tgv =
  let vars_av = VarDict.filter (fun code var ->
      match var.Var.domaine with
      | Revenu | RevenuCorr -> var.Var.avfisc = 1
      | _ -> false)
  in
  let vars_av = fst (List.split (StrMap.bindings vars_av)) in
  List.exists (fun code -> TGV.defined tgv code) vars_av
  || is_code_supp_avfisc tgv

and is_code_supp_avfisc tgv =
  let vars_av = VarDict.filter (fun code var ->
      match var.Var.domaine with
      | Revenu ->
          var.Var.avfisc = 2
      | RevenuCorr ->
          code => [ "7QK"; "7QD"; "7QB"; "7QC"; "4BA"; "4BY"; "4BB"; "4BC";
                    "7CL"; "7CM"; "7CN"; "7QE"; "7QF"; "7QG"; "7QH"; "7QI";
                    "7QJ"; "7LG"; "7MA"; "7QM"; "2DC"; "7KM"; "7KG"; "7QP";
                    "7QS"; "7QN"; "7QO"; "7QL"; "7LS" ]
      | _ ->
          false)
  in
  let vars_av = fst (List.split (StrMap.bindings vars_av)) in
  List.exists (fun code -> TGV.defined tgv code) vars_av

and calcule_acomptes_avfisc tgv traitement nap_sans_pena_reel =
  let tgv = TGV.set_int tgv "FLAG_ACO" 1 in
  let tgv = calcule_avfiscal tgv traitement in
  let tgv = TGV.set_bool tgv "INDTEO" false in
  let tgv = TGV.set_bool tgv "NEGREEL" (nap_sans_pena_reel <= 0.0) in (* TODO: set_abs ? *)
  let tgv = TGV.set tgv "NAPREEL" (Float.abs nap_sans_pena_reel) in
  let tgv = TGV.set_bool tgv "CALCUL_ACO" true in
  let tgv = calcul_prim_corr tgv traitement in
  (* save is useless, the saved vars are of type base, hence not reset *)
  (* however it allows to set them to 0 if not set *)
  let sauve = TGV.get_map_def tgv [ "ART1731BIS"; "PREM8_11" ] 0.0 in
  let tgv = TGV.reset_calculee tgv in
  if traitement = Primitif then TGV.set_map tgv sauve else tgv

and calcule_acomptes tgv traitement =
  let tgv = TGV.set_int tgv "FLAG_ACO" 1 in
  let tgv = TGV.set_bool tgv "CALCUL_ACO" true in (* var plus utilisée dans M *)
  let tgv = calcul_prim_corr tgv traitement in
  let tgv = TGV.set_bool tgv "CALCUL_ACO" false in
  let tgv = TGV.set_int tgv "FLAG_ACO" 2 in
  let sauve = TGV.get_map_def tgv [ "ART1731BIS"; "PREM8_11" ] 0.0 in
  let tgv = TGV.reset_calculee tgv in
  if traitement = Primitif then TGV.set_map (TGV.reset_base tgv) sauve else tgv

and calcule_avfiscal tgv traitement =
  let vars_av = VarDict.filter (fun code var ->
      match var.Var.domaine with
      | Revenu | RevenuCorr -> var.Var.avfisc = 1
      | _ -> false)
  in
  let vars_av = fst (List.split (StrMap.bindings vars_av)) in
  let sauve_av = TGV.get_map_opt tgv vars_av in
  let tgv = TGV.reset_list tgv vars_av in
  if is_code_supp_avfisc tgv || List.length vars_av <> 0 (* subsumes the previous ? *) then
    let tgv = TGV.set_bool_list tgv [ "INDTEO", true; "CALCUL_NAPS", true ] in
    let tgv = calcul_prim_corr tgv traitement in
    let tgv = TGV.set_bool tgv "CALCUL_NAPS" false in
    let sauve1 = TGV.get_map_def tgv [ "ART1731BIS"; "PREM8_11" ] 0.0 in
    let sauve2 = TGV.get_map_def tgv [ "IAD11"; "INE"; "IRE" ] 0.0 in
    let tgv = TGV.reset_calculee tgv in
    let tgv =
      if traitement = Primitif then TGV.set_map (TGV.reset_base tgv) sauve1
      else tgv
    in
    let tgv = TGV.set_map tgv sauve_av in
    TGV.set_list tgv
      [ "IAD11TEO", StrMap.find "IAD11" sauve2;
        "IRETEO", StrMap.find "IRE" sauve2;
        "INETEO", StrMap.find "INE" sauve2 ]
  else
    tgv
