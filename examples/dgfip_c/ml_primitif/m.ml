
open Common

external annee_calc : unit -> int = "ml_annee_calc"

external exec_ench_raw : string -> TGV.t -> unit = "ml_exec_ench"

external exec_verif_raw : string -> TGV.t -> unit = "ml_exec_verif"

let exec_ench ench tgv = exec_ench_raw ench tgv
let exec_verif ench tgv = exec_verif_raw ench tgv

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
  if TGV.defined tgv "3WG" then
    begin
      TGV.set_bool tgv "FLAG_PVRO" true;
      traite_double_liquidation_exit_taxe tgv traitement;
      TGV.internal_copy ~ignore_undefined:true tgv [ "IAD11", "IPVRO" ]
    end;
  TGV.set_bool tgv "FLAG_PVRO" false;
  traite_double_liquidation_exit_taxe tgv traitement

and traite_double_liquidation_exit_taxe tgv traitement =
  let traite_3W_RW tgv flag_EXIT code_3W code_RW
      code_3WNEG code_NAPTIR3W code_CHR3W code_ID113W =
    if TGV.defined tgv code_3W || TGV.defined tgv code_RW then
      begin
        TGV.set_bool tgv code_3WNEG false; (* do we have to reset it right now ? *)
        TGV.set_int tgv "FLAG_EXIT" flag_EXIT;
        traite_double_liquidation3 tgv traitement false;
        TGV.copy_abs tgv "NAPTIR" code_NAPTIR3W code_3WNEG;
        TGV.internal_copy ~ignore_undefined:true tgv
          [ "IHAUTREVT", code_CHR3W; "ID11", code_ID113W ];
        TGV.set_int tgv "FLAG_EXIT" 0
      end
  in
  traite_3W_RW tgv 1 "3WB" "RWB"
    "FLAG_3WBNEG" "NAPTIR3WB" "CHR3WB" "ID113WB";
  traite_3W_RW tgv 2 "3WA" "RWA"
    "FLAG_3WANEG" "NAPTIR3WA" "CHR3WA" "ID113WA";
  TGV.set_bool tgv "FLAG_BAREM" true;
  traite_double_liquidation3 tgv traitement true;
  TGV.internal_copy ~ignore_undefined:true tgv
    [ "RASTXFOYER", "BARTXFOYER";
      "RASTXDEC1", "BARTXDEC1";
      "RASTXDEC2", "BARTXDEC2";
      "INDTAZ", "BARINDTAZ";
      "IRTOTAL", "BARIRTOTAL" ];
  TGV.copy_abs tgv "IITAZIR" "BARIITAZIR" "FLAG_BARIITANEG";
  TGV.set_bool tgv "FLAG_BAREM" false;
  traite_double_liquidation3 tgv traitement true

and traite_double_liquidation3 tgv traitement p_is_calcul_acomptes =
  (* aucune de ces variables ne sont plus utilisées par le code M actuel *)
  TGV.set_bool_list tgv
    [ "FLAG_ACO", false; "NEGACO", false;
      "AVFISCOPBIS", false; "DIFTEOREEL", false ];
  if traitement = Primitif then
    begin
      TGV.set_bool tgv "PREM8_11" false;
      article_1731_bis tgv traitement
    end;
  let calcul_acomptes = is_calcul_acomptes tgv in
  let calcul_avfisc = is_calcul_avfisc tgv in
  let montant_8ZG =
    if calcul_avfisc then
      let m = TGV.get_def tgv "8ZG" 0.0 in
      TGV.reset tgv "8ZG";
      m
    else 0.0
  in
  if calcul_acomptes && p_is_calcul_acomptes then
    begin
      let vars_ac = VarDict.filter (fun code var ->
          match var.Var.domaine with
          | Revenu | RevenuCorr -> var.Var.acompte = false
          | _ -> false)
      in
      let vars_ac = fst (List.split (StrMap.bindings vars_ac)) in
      let sauve_ac = TGV.get_map_opt tgv vars_ac in (* AC_GetCodesAcompte *)
      TGV.reset_list tgv vars_ac; (* AC_SupprimeCodesAcomptes *)
      if calcul_avfisc then calcule_acomptes_avfisc tgv traitement 0.0
      else calcule_acomptes tgv traitement;
      TGV.set_map tgv sauve_ac (* AC_ReplaceCodesAcomptes *)
    end;
  if calcul_avfisc then
    begin
      TGV.set_bool_list tgv
        [ "AVFISCOPBIS", false; "DIFTEOREEL", false; "INDTEO", true ];
      calcule_avfiscal tgv traitement;
      TGV.set_bool_list tgv
        [ "INDTEO", false; "NEGREEL", true; "NAPREEL", false ]
    end;
  if calcul_avfisc && montant_8ZG <> 0.0 then TGV.set tgv "8ZG" montant_8ZG;
  TGV.set_list tgv [ "ACO_MTAP", 0.0; "NEGACO", 0.0 ];
  calcul_primitif_isf tgv;
  calcul_prim_corr tgv traitement;
  calcul_primitif_taux tgv;
  if traitement = Primitif then verif_calcul_primitive tgv

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
  TGV.set_int tgv "FLAG_ACO" 1;
  calcule_avfiscal tgv traitement;
  TGV.set_bool tgv "INDTEO" false;
  TGV.set_bool tgv "NEGREEL" (nap_sans_pena_reel <= 0.0); (* TODO: set_abs ? *)
  TGV.set tgv "NAPREEL" (Float.abs nap_sans_pena_reel);
  TGV.set_bool tgv "CALCUL_ACO" true;
  calcul_prim_corr tgv traitement;
  (* save is useless, the saved vars are of type base, hence not reset *)
  (* however it allows to set them to 0 if not set *)
  let sauve = TGV.get_map_def tgv [ "ART1731BIS"; "PREM8_11" ] 0.0 in
  TGV.reset_calculee tgv;
  if traitement = Primitif then TGV.set_map tgv sauve

and calcule_acomptes tgv traitement =
  TGV.set_int tgv "FLAG_ACO" 1;
  TGV.set_bool tgv "CALCUL_ACO" true; (* var plus utilisée dans M *)
  calcul_prim_corr tgv traitement;
  TGV.set_bool tgv "CALCUL_ACO" false;
  TGV.set_int tgv "FLAG_ACO" 2;
  let sauve = TGV.get_map_def tgv [ "ART1731BIS"; "PREM8_11" ] 0.0 in
  TGV.reset_calculee tgv;
  TGV.reset_base tgv;
  if traitement = Primitif then TGV.set_map tgv sauve

and calcule_avfiscal tgv traitement =
  let vars_av = VarDict.filter (fun code var ->
      match var.Var.domaine with
      | Revenu | RevenuCorr -> var.Var.avfisc = 1
      | _ -> false)
  in
  let vars_av = fst (List.split (StrMap.bindings vars_av)) in
  let sauve_av = TGV.get_map_opt tgv vars_av in
  TGV.reset_list tgv vars_av;
  if is_code_supp_avfisc tgv || List.length vars_av <> 0 (* subsumes the previous ? *) then
    TGV.set_bool_list tgv [ "INDTEO", true; "CALCUL_NAPS", true ];
    calcul_prim_corr tgv traitement;
    TGV.set_bool tgv "CALCUL_NAPS" false;
    let sauve1 = TGV.get_map_def tgv [ "ART1731BIS"; "PREM8_11" ] 0.0 in
    let sauve2 = TGV.get_map_def tgv [ "IAD11"; "INE"; "IRE" ] 0.0 in
    TGV.reset_calculee tgv;
    TGV.reset_base tgv;
    if traitement = Primitif then TGV.set_map tgv sauve1;
    TGV.set_map tgv sauve_av;
    TGV.set_list tgv
      [ "IAD11TEO", StrMap.find "IAD11" sauve2;
        "IRETEO", StrMap.find "IRE" sauve2;
        "INETEO", StrMap.find "INE" sauve2 ]
