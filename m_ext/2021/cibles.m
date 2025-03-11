# compir

cible regle_1:
application: iliad;
BIDON = 1;
APPLI_OCEANS = 0;
APPLI_BATCH = 0;
APPLI_ILIAD = 1;

cible calcul_primitif:
application: iliad;
calculer domaine primitive;

cible calcul_primitif_isf:
application: iliad;
calculer domaine isf;

cible calcul_primitif_taux:
application: iliad;
calculer domaine taux;

cible calcul_correctif:
application: iliad;
calculer domaine corrective;

cible sauve_base_1728:
application: iliad;
calculer domaine base_1728 corrective;

cible sauve_base_premier:
application: iliad;
calculer domaine base_premier corrective;

cible sauve_base_stratemajo:
application: iliad;
calculer domaine base_stratemajo corrective;

cible sauve_base_anterieure:
application: iliad;
calculer domaine base_anterieure corrective;

cible sauve_base_anterieure_cor:
application: iliad;
calculer domaine base_anterieure_cor corrective;

cible sauve_base_inr_tl:
application: iliad;
calculer domaine base_inr_tl corrective;

cible sauve_base_inr_tl22:
application: iliad;
calculer domaine base_inr_tl22 corrective;

cible sauve_base_inr_tl24:
application: iliad;
calculer domaine base_inr_tl24 corrective;

cible sauve_base_inr_ntl:
application: iliad;
calculer domaine base_inr_ntl corrective;

cible sauve_base_inr_ntl22:
application: iliad;
calculer domaine base_inr_ntl22 corrective;

cible sauve_base_inr_ntl24:
application: iliad;
calculer domaine base_inr_ntl24 corrective;

cible sauve_base_inr_ref:
application: iliad;
calculer domaine base_inr_ref corrective;

cible sauve_base_inr_r9901:
application: iliad;
calculer domaine base_inr_r9901 corrective;

cible sauve_base_inr_intertl:
application: iliad;
calculer domaine base_inr_intertl corrective;

cible sauve_base_inr_inter22:
application: iliad;
calculer domaine base_inr_inter22 corrective;

cible sauve_base_inr_cimr99:
application: iliad;
calculer domaine base_inr_cimr99 corrective;

cible sauve_base_inr_cimr07:
application: iliad;
calculer domaine base_inr_cimr07 corrective;

cible sauve_base_inr_cimr24:
application: iliad;
calculer domaine base_inr_cimr24 corrective;

cible sauve_base_inr_tlcimr07:
application: iliad;
calculer domaine base_inr_tlcimr07 corrective;

cible sauve_base_inr_tlcimr24:
application: iliad;
calculer domaine base_inr_tlcimr24 corrective;

cible sauve_base_tlnunv:
application: iliad;
calculer domaine base_TLNUNV corrective;

cible sauve_base_tl:
application: iliad;
calculer domaine base_tl corrective;

cible sauve_base_tl_init:
application: iliad;
calculer domaine base_tl_init corrective;

cible sauve_base_tl_rect:
application: iliad;
calculer domaine base_tl_rect corrective;

cible sauve_base_initial:
application: iliad;
calculer domaine base_INITIAL corrective;

cible sauve_base_abat98:
application: iliad;
calculer domaine base_ABAT98 corrective;

cible sauve_base_abat99:
application: iliad;
calculer domaine base_ABAT99 corrective;

cible sauve_base_majo:
application: iliad;
calculer domaine base_MAJO corrective;

cible sauve_base_inr:
application: iliad;
calculer domaine base_INR corrective;

cible sauve_base_HR:
application: iliad;
calculer domaine base_HR corrective;

cible sauve_base_primitive_penalisee:
application: iliad;
calculer domaine base_primitive_penalisee corrective;

cible ENCH_TL:
application: iliad;
calculer enchaineur ENCH_TL;

cible verif_calcul_primitive_isf:
application: iliad;
verifier domaine isf : avec nb_categorie(calculee *) > 0;

cible verif_calcul_primitive:
application: iliad;
calculer cible verif_calcul_primitive_isf;
si nb_anomalies() = 0 alors
  verifier domaine primitive : avec nb_categorie(calculee *) > 0;
finsi

cible verif_calcul_corrective:
application: iliad;
calculer cible calcul_primitif_isf;
calculer cible verif_calcul_primitive_isf;
si nb_anomalies() = 0 alors
  verifier domaine corrective : avec nb_categorie(calculee *) > 0;
finsi

cible verif_saisie_cohe_primitive_isf_raw:
application: iliad;
verifier domaine isf
: avec nb_categorie(saisie *) > 0 et nb_categorie(calculee *) = 0;

cible verif_saisie_cohe_primitive:
application: iliad;
calculer cible verif_saisie_cohe_primitive_isf_raw;
si nb_anomalies() = 0 alors
  calculer cible calcul_primitif_isf;
  calculer cible verif_calcul_primitive_isf;
  si nb_anomalies() = 0 alors
    verifier domaine primitive
    : avec nb_categorie(saisie *) > 0 et nb_categorie(calculee *) = 0;
  finsi
finsi

cible verif_saisie_cohe_corrective:
application: iliad;
calculer cible verif_saisie_cohe_primitive_isf_raw;
si nb_anomalies() = 0 alors
  verifier domaine corrective
  : avec nb_categorie(saisie *) > 0 et nb_categorie(calculee *) = 0;
finsi

cible verif_cohe_horizontale:
application: iliad;
verifier domaine horizontale corrective;

cible verif_contexte_cohe_primitive:
application: iliad;
verifier domaine primitive
: avec nb_categorie(saisie contexte) > 0 et nb_categorie(calculee *) = 0;

cible verif_contexte_cohe_corrective:
application: iliad;
verifier domaine corrective
: avec nb_categorie(saisie contexte) > 0 et nb_categorie(calculee *) = 0;

cible verif_famille_cohe_primitive:
application: iliad;
verifier domaine primitive
: avec nb_categorie(saisie famille) > 0 et nb_categorie(calculee *) = 0;

cible verif_famille_cohe_corrective:
application: iliad;
verifier domaine corrective
: avec nb_categorie(saisie famille) > 0 et nb_categorie(calculee *) = 0;

cible verif_revenu_cohe_primitive:
application: iliad;
verifier domaine primitive
: avec nb_categorie(saisie revenu) > 0 et nb_categorie(calculee *) = 0;

cible verif_revenu_cohe_corrective:
application: iliad;
verifier domaine corrective
: avec nb_categorie(saisie revenu) > 0 et nb_categorie(calculee *) = 0;

# primitif ml

cible calcul_prim_corr:
application: iliad;
si V_IND_TRAIT = 4 alors # PRIMITIF
  calculer cible calcul_primitif;
sinon
  calculer cible calcul_correctif;
finsi

cible effacer_base_etc:
application : iliad;
iterer
: variable ITBASE
: categorie calculee base
: dans (
  ITBASE = indefini;
)

cible effacer_calculee_etc:
application : iliad;
iterer
: variable ITCAL
: categorie calculee
: dans (
  ITCAL = indefini;
)

cible calcule_acomptes:
application: iliad;
variables_temporaires: SAUV_ART1731BIS, SAUV_PREM8_11;
FLAG_ACO = 1;
V_CALCUL_ACO = 1;
calculer cible calcul_prim_corr;
V_CALCUL_ACO = 0;
FLAG_ACO = 2;
SAUV_ART1731BIS = ART1731BIS + 0;
SAUV_PREM8_11 = PREM8_11 + 0;
calculer cible effacer_calculee_etc;
si V_IND_TRAIT = 4 alors # PRIMITIF
  calculer cible effacer_base_etc;
  ART1731BIS = SAUV_ART1731BIS;
  PREM8_11 = SAUV_PREM8_11;
finsi

cible effacer_avfisc_1:
application: iliad;
iterer
: variable REV_AV
: categorie saisie revenu, saisie revenu corrective
: avec attribut(REV_AV, avfisc) = 1 et present(REV_AV)
: dans (
  REV_AV = indefini;
)

cible calcule_avfiscal:
application: iliad;
variables_temporaires: EXISTE_AVFISC, SAUV_IAD11, SAUV_INE, SAUV_IRE, SAUV_ART1731BIS, SAUV_PREM8_11;
EXISTE_AVFISC = 0;
iterer
: variable REV_AV
: categorie saisie revenu, saisie revenu corrective
  : avec attribut(REV_AV, avfisc) = 1 et present(REV_AV)  
: dans (
  EXISTE_AVFISC = 1;
)
si EXISTE_AVFISC = 1 alors
  restaurer
  : variable REV_AV
  : categorie saisie revenu, saisie revenu corrective
  : avec attribut(REV_AV, avfisc) = 1 et present(REV_AV)
  : apres (
    calculer cible effacer_avfisc_1;
    V_INDTEO = 1;
    V_CALCUL_NAPS = 1;
    calculer cible calcul_prim_corr;
    V_CALCUL_NAPS = 0;
    SAUV_ART1731BIS = ART1731BIS + 0;
    SAUV_PREM8_11 = PREM8_11 + 0;
    SAUV_IAD11 = IAD11;
    SAUV_INE = INE;
    SAUV_IRE = IRE;
    calculer cible effacer_calculee_etc;
    si V_IND_TRAIT = 4 alors # PRIMITIF
      calculer cible effacer_base_etc;
      ART1731BIS = SAUV_ART1731BIS;
      PREM8_11 = SAUV_PREM8_11;
    finsi
  )
  V_IAD11TEO = SAUV_IAD11;
  V_IRETEO = SAUV_IRE;
  V_INETEO = SAUV_INE;
sinon
  calculer cible effacer_avfisc_1;
finsi

cible article_1731_bis:
application : iliad;
si V_IND_TRAIT = 4 alors # PRIMITIF
  si CMAJ dans (8, 11) alors
    ART1731BIS = 1;
    PREM8_11 = 1;
  sinon
    ART1731BIS = 0;
  finsi
finsi

cible calcule_acomptes_avfisc:
application: iliad;
variables_temporaires: NAP_SANS_PENA_REEL, SAUV_ART1731BIS, SAUV_PREM8_11;
NAP_SANS_PENA_REEL = 0; # toujours 0 ?
FLAG_ACO = 1;
calculer cible calcule_avfiscal;
V_INDTEO = 0;
V_NEGREEL = si (NAP_SANS_PENA_REEL <= 0.0) alors (1) sinon (0) finsi;
V_NAPREEL = abs(NAP_SANS_PENA_REEL);
V_CALCUL_ACO = 1;
calculer cible calcul_prim_corr;
SAUV_ART1731BIS = ART1731BIS + 0;
SAUV_PREM8_11 = PREM8_11 + 0;
calculer cible effacer_calculee_etc;
si V_IND_TRAIT = 4 alors # PRIMITIF
  ART1731BIS = SAUV_ART1731BIS;
  PREM8_11 = SAUV_PREM8_11;
finsi

cible est_calcul_acomptes:
application: iliad;
VARTMP1 = 0;
iterer
: variable REV_AC
: categorie saisie revenu, saisie revenu corrective
: avec attribut(REV_AC, acompte) = 0 et present(REV_AC)
: dans (
  VARTMP1 = 1;
)

cible est_code_supp_avfisc:
application: iliad;
VARTMP1 = 0;
si
     present(COD7QD)
  ou present(COD7QB)
  ou present(COD7QC)
  ou present(RFORDI)
  ou present(RFROBOR)
  ou present(RFDORD)
  ou present(RFDHIS)
  # ou present(REPSNO3_A)
  ou present(COD7QF)
  ou present(COD7QH)
  # ou present(CELRREDLG_A)
  # ou present(PINELQM_A)
  ou present(RCMABD)
  ou present(COD7KM)
  # ou present(PINELQP_A)
  # ou present(COD7QS_A)
  # ou present(PINELQN_A)
  # ou present(PINELQO_A)
alors
  VARTMP1 = 1;
sinon
  iterer
  : variable REV_AV
  : categorie saisie revenu, saisie revenu corrective
  : avec attribut(REV_AV, avfisc) = 2 et present(REV_AV)
  : dans (
    VARTMP1 = 1;
  )
finsi

cible est_calcul_avfisc:
application: iliad;
calculer cible est_code_supp_avfisc;
si VARTMP1 = 0 alors
  iterer
  : variable REV_AV
  : categorie saisie revenu, saisie revenu corrective
  : avec attribut(REV_AV, avfisc) = 1 et present(REV_AV)
  : dans (
    VARTMP1 = 1;
  )
finsi

cible traite_double_liquidation3:
application: iliad;
variables_temporaires: P_EST_CALCUL_ACOMPTES, CALCUL_ACOMPTES, CALCUL_AVFISC, SAUV_IRANT;
P_EST_CALCUL_ACOMPTES = VARTMP1;
FLAG_ACO = 0;
V_NEGACO = 0;
V_AVFISCOPBIS = 0;
V_DIFTEOREEL = 0;
si V_IND_TRAIT = 4 alors # primitif
  PREM8_11 = 0;
  calculer cible article_1731_bis;
finsi
calculer cible est_calcul_acomptes;
CALCUL_ACOMPTES = VARTMP1;
calculer cible est_calcul_avfisc;
CALCUL_AVFISC = VARTMP1;
si CALCUL_AVFISC = 1 alors
  SAUV_IRANT = IRANT + 0 ;
  IRANT = indefini;
sinon
  SAUV_IRANT = 0;
finsi
si CALCUL_ACOMPTES  = 1 et P_EST_CALCUL_ACOMPTES alors
  restaurer
  : variable REV_AC
  : categorie saisie revenu, saisie revenu corrective
  : avec attribut(REV_AC, acompte) = 0
  : apres (
    iterer
    : variable REV_AC
    : categorie saisie revenu, saisie revenu corrective
    : avec attribut(REV_AC, acompte) = 0
    : dans (
      REV_AC = indefini;
    )
    si CALCUL_AVFISC = 1 alors
      calculer cible calcule_acomptes_avfisc;
    sinon
      calculer cible calcule_acomptes;
    finsi
  )
finsi
si CALCUL_AVFISC = 1 alors
  V_AVFISCOPBIS = 0;
  V_DIFTEOREEL = 0;
  V_INDTEO = 1;
  calculer cible calcule_avfiscal;
  V_INDTEO = 0;
  V_NEGREEL = 1;
  V_NAPREEL = 0;
finsi
si CALCUL_AVFISC = 1 et SAUV_IRANT != 0 alors
  IRANT = SAUV_IRANT;
finsi
V_ACO_MTAP = 0;
V_NEGACO = 0;
calculer cible calcul_primitif_isf;
calculer cible calcul_prim_corr;
calculer cible calcul_primitif_taux;
si V_IND_TRAIT = 4 alors # primitif
  calculer cible verif_calcul_primitive;
finsi

cible traite_double_liquidation_exit_taxe:
application: iliad;
si present(PVIMPOS) ou present(CODRWB) alors
  FLAG_3WBNEG = 0;
  FLAG_EXIT = 1;
  VARTMP1 = 0;
  calculer cible traite_double_liquidation3;
  si present(NAPTIR) alors
    FLAG_3WBNEG = (NAPTIR < 0);
    V_NAPTIR3WB = abs(NAPTIR);
    NAPTIR = V_NAPTIR3WB;
  finsi
  si present(IHAUTREVT) alors
    V_CHR3WB = IHAUTREVT;
  finsi
  si present(ID11) alors
    V_ID113WB = ID11;
  finsi
  FLAG_EXIT = 0;
finsi
si present(PVSURSI) ou present(CODRWA) alors
  FLAG_3WANEG = 0;
  FLAG_EXIT = 2;
  VARTMP1 = 0;
  calculer cible traite_double_liquidation3;
  si present(NAPTIR) alors
    FLAG_3WANEG = (NAPTIR < 0);
    V_NAPTIR3WA = abs(NAPTIR);
    NAPTIR = V_NAPTIR3WA;
  finsi
  si present(IHAUTREVT) alors
    V_CHR3WA = IHAUTREVT;
  finsi
  si present(ID11) alors
    V_ID113WA = ID11;
  finsi
  FLAG_EXIT = 0;
finsi
FLAG_BAREM = 1;
VARTMP1 = 1;
calculer cible traite_double_liquidation3;
si present(RASTXFOYER) alors
  V_BARTXFOYER = RASTXFOYER;
finsi
si present(RASTXDEC1) alors
  V_BARTXDEC1 = RASTXDEC1;
finsi
si present(RASTXDEC2) alors
  V_BARTXDEC2 = RASTXDEC2;
finsi
si present(INDTAZ) alors
  V_BARINDTAZ = INDTAZ;
finsi
si present(IITAZIR) alors
  FLAG_BARIITANEG = (IITAZIR < 0);
  V_BARIITAZIR = abs(IITAZIR);
  IITAZIR = V_BARIITAZIR;
finsi
si present(IRTOTAL) alors
  V_BARIRTOTAL = IRTOTAL;
finsi
FLAG_BAREM = 0;
VARTMP1 = 1;
calculer cible traite_double_liquidation3;

cible traite_double_liquidation_pvro:
application: iliad;
si present(COD3WG) alors
  FLAG_PVRO = 1;
  calculer cible traite_double_liquidation_exit_taxe;
  si present(IAD11) alors
    V_IPVRO = IAD11;
  finsi
finsi
FLAG_PVRO = 0;
calculer cible traite_double_liquidation_exit_taxe;

cible teste_tableaux:
application: iliad;
VARTMP1 = 0;
afficher "0) VARTMP1 = " (VARTMP1) : 0..2 "\n";
VARTMPTAB1[0] = 0;
VARTMPTAB1[1] = 1;
VARTMPTAB1[2] = indefini;
iterer
: variable V
: categorie calculee *
: dans (
  V = 1;
)
afficher "1) VARTMP1 = " (VARTMP1) : 0..2 "\n";
afficher "2) VARTMPTAB1[0] = " (VARTMPTAB1[0]) : 0..2 "\n";
afficher "2) VARTMPTAB1[1] = " (VARTMPTAB1[1]) : 0..2 "\n";
afficher "2) VARTMPTAB1[2] = " (VARTMPTAB1[2]) : 0..2 "\n";

cible enchainement_primitif:
application: iliad;
# calculer cible teste_tableaux;
calculer cible traite_double_liquidation_pvro;

# primitif iterpréteur

cible enchainement_primitif_interpreteur:
application: iliad;
V_IND_TRAIT = 4;
calculer cible enchainement_primitif;


# obsolète

cible dgfip_calculation:
application: iliad;
APPLI_OCEANS = 0;
V_IND_TRAIT = 4;  # 4 = PRIMITIF, 5 = CORRECTIF
FLAG_PVRO = 0;
FLAG_EXIT = 0;
FLAG_BAREM = 0;
FLAG_ACO = 0;
V_NEGACO = 0;
V_AVFISCOPBIS = 0;
V_DIFTEOREEL = 0;
PREM8_11 = 0;
ART1731BIS = 0;
V_ACO_MTAP = 0;
V_NEGACO = 0;
calculer cible calcul_primitif_isf;
calculer cible calcul_primitif;
calculer cible calcul_primitif_taux;

