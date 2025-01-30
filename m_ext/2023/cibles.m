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
nettoie_erreurs;
verifier domaine isf : avec nb_categorie(calculee *) > 0;

cible verif_calcul_primitive:
application: iliad;
calculer cible verif_calcul_primitive_isf;
si nb_bloquantes() = 0 alors
  verifier domaine primitive
  : avec
      nb_categorie(calculee *) > 0
      ou numero_verif() = 1021;
finsi

cible verif_calcul_corrective:
application: iliad;
nettoie_erreurs;
calculer cible calcul_primitif_isf;
calculer cible verif_calcul_primitive_isf;
si nb_bloquantes() = 0 alors
  verifier domaine corrective
  : avec
      nb_categorie(calculee *) > 0
      ou numero_verif() = 1021;
finsi

cible verif_saisie_cohe_primitive_isf_raw:
application: iliad;
nettoie_erreurs;
verifier domaine isf
: avec nb_categorie(saisie *) > 0 et nb_categorie(calculee *) = 0;

cible verif_saisie_cohe_primitive:
application: iliad;
nettoie_erreurs;
calculer cible verif_saisie_cohe_primitive_isf_raw;
si nb_bloquantes() = 0 alors
  calculer cible calcul_primitif_isf;
  calculer cible verif_calcul_primitive_isf;
  si nb_bloquantes() = 0 alors
    verifier domaine primitive
    : avec
        nb_categorie(saisie *) > 0 et nb_categorie(calculee *) = 0
        et numero_verif() != 1021;
  finsi
finsi

cible verif_saisie_cohe_corrective:
application: iliad;
nettoie_erreurs;
calculer cible verif_saisie_cohe_primitive_isf_raw;
si nb_bloquantes() = 0 alors
  verifier domaine corrective
  : avec
      nb_categorie(saisie *) > 0 et nb_categorie(calculee *) = 0
      et numero_verif() != 1021;
finsi

cible verif_cohe_horizontale:
application: iliad;
nettoie_erreurs;
verifier domaine horizontale corrective;

cible verif_contexte_cohe_primitive:
application: iliad;
nettoie_erreurs;
verifier domaine primitive
: avec nb_categorie(saisie contexte) = nb_categorie(*);

cible verif_contexte_cohe_corrective:
application: iliad;
nettoie_erreurs;
verifier domaine corrective
: avec nb_categorie(saisie contexte) = nb_categorie(*);

cible verif_famille_cohe_primitive:
application: iliad;
nettoie_erreurs;
verifier domaine primitive
: avec
    nb_categorie(saisie famille) > 0
    et nb_categorie(*) = nb_categorie(saisie famille) + nb_categorie(saisie contexte)
    et numero_verif() != 1021;

cible verif_famille_cohe_corrective:
application: iliad;
nettoie_erreurs;
verifier domaine corrective
: avec
    nb_categorie(saisie famille) > 0
    et nb_categorie(*) = nb_categorie(saisie famille) + nb_categorie(saisie contexte)
    et numero_verif() != 1021;

cible verif_revenu_cohe_primitive:
application: iliad;
nettoie_erreurs;
verifier domaine primitive
: avec nb_categorie(saisie revenu) > 0 et nb_categorie(calculee *) = 0;

cible verif_revenu_cohe_corrective:
application: iliad;
nettoie_erreurs;
verifier domaine corrective
: avec nb_categorie(saisie revenu) > 0 et nb_categorie(calculee *) = 0;

# primitif ml

cible trace_in:
application: iliad;
variables_temporaires: TOTO;
TOTO = 0;
#afficher_erreur indenter(2);

cible trace_out:
application: iliad;
variables_temporaires: TOTO;
TOTO = 0;
#afficher_erreur indenter(-2);

cible calcul_prim_corr:
application: iliad;
#afficher_erreur "calcul_prim_corr[\n";
calculer cible trace_in;
si V_IND_TRAIT = 4 alors # PRIMITIF
  calculer cible calcul_primitif;
sinon
  calculer cible calcul_correctif;
finsi
calculer cible trace_out;
#afficher_erreur "]calcul_prim_corr\n";

cible effacer_base_etc:
application : iliad;
#afficher_erreur "effacer_base_etc[\n";
calculer cible trace_in;
iterer
: variable ITBASE
: categorie calculee base
: dans (
  ITBASE = indefini;
)
calculer cible trace_out;
#afficher_erreur "]effacer_base_etc\n";

cible effacer_calculee_etc:
application : iliad;
#afficher_erreur "effacer_calculee_etc[\n";
calculer cible trace_in;
iterer
: variable ITCAL
: categorie calculee
: dans (
  ITCAL = indefini;
)
calculer cible trace_out;
#afficher_erreur "]effacer_calculee_etc\n";

cible calcule_acomptes:
application: iliad;
variables_temporaires: SAUV_ART1731BIS, SAUV_PREM8_11;
#afficher_erreur "calcule_acomptes[\n";
calculer cible trace_in;
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
calculer cible trace_out;
#afficher_erreur "]calcule_acomptes\n";

cible effacer_avfisc_1:
application: iliad;
#afficher_erreur "effacer_avfisc_1[\n";
calculer cible trace_in;
iterer
: variable REV_AV
: categorie saisie revenu, saisie revenu corrective
: avec attribut(REV_AV, avfisc) = 1 et present(REV_AV)
: dans (
  REV_AV = indefini;
)
calculer cible trace_out;
#afficher_erreur "]effacer_avfisc_1\n";

cible est_code_supp_avfisc:
application: iliad;
arguments: EXISTE_CODE_SUPP;
#afficher_erreur "est_code_supp_avfisc[\n";
calculer cible trace_in;
EXISTE_CODE_SUPP = 0;
#si
#     present(COD7QD)  ou present(COD7QB)  ou present(COD7QC)
#  ou present(RFORDI)  ou present(RFROBOR) ou present(RFDORD)
#  ou present(RFDHIS)  ou present(REPSNO3_A)
#  ou present(COD7QF)  ou present(COD7QH)  ou present(CELRREDLG_A)
#  ou present(PINELQM_A) ou present(RCMABD)  ou present(COD7KM)
#  ou present(PINELQP_A) ou present(COD7QS_A)  ou present(PINELQN_A)
#  ou present(PINELQO_A)
#alors
#  EXISTE_CODE_SUPP = 1;
#sinon
  iterer
  : variable REV_AV
  : categorie saisie revenu, saisie revenu corrective
  : avec attribut(REV_AV, avfisc) = 2 et present(REV_AV)
  : dans (
    EXISTE_CODE_SUPP = 1;
  )
#finsi
calculer cible trace_out;
#afficher_erreur "]est_code_supp_avfisc\n";

cible calcule_avfiscal:
application: iliad;
variables_temporaires:
  EXISTE_AVFISC, EXISTE_CODE_SUPP,
  SAUV_IAD11, SAUV_INE, SAUV_IRE, SAUV_ART1731BIS, SAUV_PREM8_11;
#afficher_erreur "calcule_avfiscal[\n";
calculer cible trace_in;
EXISTE_AVFISC = 0;
iterer
: variable REV_AV
: categorie saisie revenu, saisie revenu corrective
: avec attribut(REV_AV, avfisc) dans (1, 2) et present(REV_AV)  
: dans (
  EXISTE_AVFISC = 1;
)
calculer cible est_code_supp_avfisc : avec EXISTE_CODE_SUPP;
si EXISTE_CODE_SUPP = 0 alors
  EXISTE_AVFISC = 1;
finsi
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
    SAUV_IAD11 = IAD11;
    SAUV_INE = INE;
    SAUV_IRE = IRE;
    SAUV_ART1731BIS = ART1731BIS + 0;
    SAUV_PREM8_11 = PREM8_11 + 0;
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
calculer cible trace_out;
#afficher_erreur "]calcule_avfiscal\n";

cible article_1731_bis:
application : iliad;
#afficher_erreur "article_1731_bis[\n";
calculer cible trace_in;
si V_IND_TRAIT = 4 alors # PRIMITIF
  si CMAJ dans (8, 11) alors
    ART1731BIS = 1;
    PREM8_11 = 1;
  sinon
    ART1731BIS = 0;
  finsi
finsi
calculer cible trace_out;
#afficher_erreur "]article_1731_bis\n";

cible calcule_acomptes_avfisc:
application: iliad;
variables_temporaires: NAP_SANS_PENA_REEL, SAUV_ART1731BIS, SAUV_PREM8_11;
#afficher_erreur "calcule_acomptes_avfisc[\n";
calculer cible trace_in;
NAP_SANS_PENA_REEL = 0; # toujours 0 ?
FLAG_ACO = 1;
calculer cible calcule_avfiscal;
V_INDTEO = 0;
V_NEGREEL = si (NAP_SANS_PENA_REEL > 0.0) alors (0) sinon (1) finsi;
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
calculer cible trace_out;
#afficher_erreur "]calcule_acomptes_avfisc\n";

cible est_calcul_acomptes:
application: iliad;
arguments: EXISTE_ACOMPTES;
#afficher_erreur "est_calcul_acomptes[\n";
calculer cible trace_in;
EXISTE_ACOMPTES = 0;
iterer
: variable REV_AC
: categorie saisie revenu, saisie revenu corrective
: avec attribut(REV_AC, acompte) = 0 et present(REV_AC)
: dans (
  EXISTE_ACOMPTES = 1;
)
calculer cible trace_out;
#afficher_erreur "]est_calcul_acomptes\n";

cible est_calcul_avfisc:
application: iliad;
arguments: EXISTE_AVFISC;
#afficher_erreur "est_calcul_avfisc[\n";
calculer cible trace_in;
EXISTE_AVFISC = 0;
iterer
: variable REV_AV
: categorie saisie revenu, saisie revenu corrective
: avec attribut(REV_AV, avfisc) = 1 et present(REV_AV)
: dans (
  EXISTE_AVFISC = 1;
)
si EXISTE_AVFISC = 0 alors
  calculer cible est_code_supp_avfisc : avec EXISTE_AVFISC; 
finsi
calculer cible trace_out;
#afficher_erreur "]est_calcul_avfisc\n";

cible traite_double_liquidation3:
application: iliad;
arguments: P_EST_CALCUL_ACOMPTES;
variables_temporaires: CALCUL_ACOMPTES, CALCUL_AVFISC, SAUV_IRANT;
#afficher_erreur "traite_double_liquidation3[\n";
calculer cible trace_in;
FLAG_ACO = 0;
V_NEGACO = 0;
V_AVFISCOPBIS = 0;
V_DIFTEOREEL = 0;
si V_IND_TRAIT = 4 alors # primitif
  PREM8_11 = 0;
  calculer cible article_1731_bis;
finsi
calculer cible est_calcul_acomptes : avec CALCUL_ACOMPTES;
calculer cible est_calcul_avfisc : avec CALCUL_AVFISC;
si CALCUL_AVFISC = 1 alors
  SAUV_IRANT = IRANT + 0 ;
  IRANT = indefini;
sinon
  SAUV_IRANT = 0;
finsi
si CALCUL_ACOMPTES = 1 et P_EST_CALCUL_ACOMPTES != 0 alors
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
#afficher_erreur "calcul_primitif_taux[\n";
calculer cible trace_in;
calculer cible calcul_primitif_taux;
calculer cible trace_out;
#afficher_erreur "]calcul_primitif_taux\n";
si V_IND_TRAIT = 4 alors # primitif
  calculer cible verif_calcul_primitive;
finsi
calculer cible trace_out;
#afficher_erreur "]traite_double_liquidation3\n";

cible abs_flag:
application: iliad;
arguments: VAR, ABS, FLAG;
si present(VAR) alors
  FLAG = (VAR < 0);
  ABS = abs(VAR);
  VAR = ABS;
finsi

cible traite_double_liquidation_exit_taxe:
application: iliad;
variables_temporaires: CALCULER_ACOMPTES;
#afficher_erreur "traite_double_liquidation_exit_taxe[\n";
calculer cible trace_in;
si present(PVIMPOS) ou present(CODRWB) alors
  FLAG_3WBNEG = 0;
  FLAG_EXIT = 1;
  CALCULER_ACOMPTES = 0;
  calculer cible traite_double_liquidation3 : avec CALCULER_ACOMPTES;
  calculer cible abs_flag : avec NAPTIR, V_NAPTIR3WB, FLAG_3WBNEG;
  si present(IHAUTREVT) alors
    V_CHR3WB = IHAUTREVT;
  finsi
  si present(IAD11) alors
    V_ID113WB = IAD11;
  finsi
  FLAG_EXIT = 0;
finsi
si present(PVSURSI) ou present(CODRWA) alors
  FLAG_3WANEG = 0;
  FLAG_EXIT = 2;
  CALCULER_ACOMPTES = 0;
  calculer cible traite_double_liquidation3 : avec CALCULER_ACOMPTES;
  calculer cible abs_flag : avec NAPTIR, V_NAPTIR3WA, FLAG_3WANEG;  
  si present(IHAUTREVT) alors
    V_CHR3WA = IHAUTREVT;
  finsi
  si present(IAD11) alors
    V_ID113WA = IAD11;
  finsi
  FLAG_EXIT = 0;
finsi
FLAG_BAREM = 1;
CALCULER_ACOMPTES = 1;
calculer cible traite_double_liquidation3 : avec CALCULER_ACOMPTES;
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
  si INDTAZ >= 0 alors
    V_BARINDTAZ = INDTAZ;
## Segfault !!! ##
#  sinon
#    leve_erreur A000;
  finsi
finsi
calculer cible abs_flag : avec IITAZIR, V_BARIITAZIR, FLAG_BARIITANEG;  
si present(IRTOTAL) alors
  V_BARIRTOTAL = IRTOTAL;
finsi
FLAG_BAREM = 0;
CALCULER_ACOMPTES = 1;
calculer cible traite_double_liquidation3 : avec CALCULER_ACOMPTES;
calculer cible trace_out;
#afficher_erreur "]traite_double_liquidation_exit_taxe\n";

cible traite_double_liquidation_pvro:
application: iliad;
#afficher_erreur "traite_double_liquidation_pvro[\n";
calculer cible trace_in;
si present(COD3WG) alors
  FLAG_PVRO = 1;
  calculer cible traite_double_liquidation_exit_taxe;
  si present(IAD11) alors
    V_IPVRO = IAD11;
  finsi
finsi
FLAG_PVRO = 0;
calculer cible traite_double_liquidation_exit_taxe;
calculer cible trace_out;
#afficher_erreur "]traite_double_liquidation_pvro\n";

cible ir_verif_saisie_isf:
application: iliad;
calculer cible regle_1;
calculer cible verif_saisie_cohe_primitive_isf_raw;

cible ir_verif_contexte:
application: iliad;
calculer cible regle_1;
calculer cible verif_contexte_cohe_primitive;

cible ir_verif_famille:
application: iliad;
calculer cible regle_1;
calculer cible verif_famille_cohe_primitive;

cible ir_verif_revenu:
application: iliad;
#afficher_erreur "ir_verif_revenu[\n";
calculer cible trace_in;
si
  present(COD9AA) ou present(COD9AB) ou present(COD9AC) ou present(COD9AD)
  ou present(COD9AE) ou present(COD9BA) ou present(COD9BB) ou present(COD9CA)
  ou present(COD9GF) ou present(COD9GH) ou present(COD9GL) ou present(COD9GM)
  ou present(COD9GN) ou present(COD9GY) ou present(COD9NC) ou present(COD9NG)
  ou present(COD9PR) ou present(COD9PX) ou present(COD9RS) ou present(CMAJ_ISF)
  ou present(MOISAN_ISF)
alors
  si V_REGCO + 0 = 0 alors
    V_REGCO = 1;
  finsi
  si V_0DA + 0 = 0 alors
    V_0DA = 1980;
  finsi
finsi
calculer cible regle_1;
calculer cible verif_revenu_cohe_primitive;
calculer cible trace_out;
#afficher_erreur "]ir_verif_revenu\n";

cible ir_calcul_primitif_isf:
application: iliad;
#afficher_erreur "ir_calcul_primitif_isf[\n";
calculer cible trace_in;
calculer cible calcul_primitif_isf;
nettoie_erreurs;
calculer cible verif_calcul_primitive_isf;
calculer cible trace_out;
#afficher_erreur "]ir_calcul_primitif_isf\n";

cible modulation_taxation:
application: iliad;
#afficher_erreur "modulation_taxation[\n";
calculer cible trace_in;
si V_MODUL = 1 alors
  iterer
  : variable IT_MOD
  : categorie saisie revenu, saisie revenu corrective, saisie famille
  : avec present(IT_MOD) et attribut(IT_MOD, modcat) < 1
  : dans (
    IT_MOD = indefini;
    leve_erreur DD40 IT_MOD;
  )
  iterer
  : variable IT_MOD
  : categorie saisie contexte
  : avec present(IT_MOD) et attribut(IT_MOD, modcat) < 1
  : dans (
    IT_MOD = indefini;
  )
finsi
si (non present(V_MODUL)) ou V_MODUL != 1 alors
  iterer
  : variable IT_MOD
  : categorie saisie revenu, saisie revenu corrective, saisie famille
  : avec present(IT_MOD) et attribut(IT_MOD, modcat) > 1
  : dans (
    IT_MOD = indefini;
  )
  iterer
  : variable IT_MOD
  : categorie saisie contexte
  : avec present(IT_MOD) et attribut(IT_MOD, modcat) > 1
  : dans (
    IT_MOD = indefini;
    leve_erreur DD40 IT_MOD;
  )
finsi
calculer cible trace_out;
#afficher_erreur "]modulation_taxation\n";

cible traite_double_liquidation_2:
application: iliad;
calculer cible modulation_taxation;
calculer cible traite_double_liquidation_pvro;

cible enchaine_calcul:
application: iliad;
# variables_temporaires: CALCULER_ACOMPTES;
si V_IND_TRAIT = 4 alors # primitif
  calculer cible effacer_base_etc;
  calculer cible traite_double_liquidation_2;
  calculer cible sauve_base_initial;
  calculer cible sauve_base_1728;
  calculer cible sauve_base_anterieure;
  calculer cible sauve_base_anterieure_cor;
  calculer cible sauve_base_inr_inter22;
sinon
  V_ACO_MTAP = 0;
  V_NEGACO = 0;
#  CALCULER_ACOMPTES = si (present(FLAGDERNIE)) alors (1) sinon (0) finsi;
#  calculer cible traite_double_liquidation3 : avec CALCULER_ACOMPTES;
  calculer cible traite_double_liquidation_pvro;
finsi

cible exporte_si_non_bloquantes:
application: iliad;
si nb_discordances() + nb_informatives() > 0 alors
  exporte_erreurs;
finsi

fonction truc:
application: iliad;
arguments: A0, A1;
resultat: RES;
variables_temporaires: TOTO;
#V_IND_TRAIT = 4;
afficher_erreur "truc\n" indenter(2);
TOTO = 1;
iterer
: variable I
: A0 .. A1 increment 1
: dans (
  si I = A0 alors
    RES = 1;
  sinon
    RES = 2 * RES + TOTO;
  finsi
  afficher_erreur (I) ": " (RES) "\n";
)
afficher_erreur indenter(-2);

cible test_boucle:
application: iliad;
arguments: I0, I1;
variables_temporaires: TOTO;
TOTO = 0;
iterer
: variable I
: I0 .. I1 increment 0.7
: 2 .. 1 increment -1
: dans (
  iterer
  : variable J
  : -3 .. -1 increment 1
  : 1 .. 0 increment -1
  : dans (
    afficher_erreur nom(I) " = " (I) ", " nom(J) " = " (J) "\n";
  )
)
TOTO = truc(TOTO, truc(4, truc(7, 9)));
afficher_erreur "truc: " (TOTO) "\n";

cible afficher_evenement:
application: iliad;
arguments: I;
afficher_erreur (I) ": ";
si (present(champ_evenement(I, numero))) alors afficher_erreur (champ_evenement(I, numero)); finsi
afficher_erreur "/";
si (present(champ_evenement(I, rappel))) alors afficher_erreur (champ_evenement(I, rappel)); finsi
afficher_erreur "/" alias(I, code) "," nom(I, code) "/";
si (present(champ_evenement(I, montant))) alors afficher_erreur (champ_evenement(I, montant)); finsi
afficher_erreur "/";
si (present(champ_evenement(I, sens))) alors
  si (champ_evenement(I, sens) = 0) alors
    afficher_erreur "R";
  sinon_si (champ_evenement(I, sens) = 1) alors
      afficher_erreur "C";
  sinon_si (champ_evenement(I, sens) = 2) alors
    afficher_erreur "M";
  sinon_si (champ_evenement(I, sens) = 3) alors
    afficher_erreur "P";
  finsi
finsi
afficher_erreur "/";
si (present(champ_evenement(I, penalite))) alors afficher_erreur (champ_evenement(I, penalite)); finsi
afficher_erreur "/";
si (present(champ_evenement(I, base_tl))) alors afficher_erreur (champ_evenement(I, base_tl)); finsi
afficher_erreur "/";
si (present(champ_evenement(I, date))) alors afficher_erreur (champ_evenement(I, date)); finsi
afficher_erreur "/";
si (present(champ_evenement(I, 2042_rect))) alors afficher_erreur (champ_evenement(I, 2042_rect)); finsi

cible afficher_evenements:
application: iliad;
iterer
: variable I
: 0 .. (nb_evenements() - 1) increment 1
: dans (
  calculer cible afficher_evenement : avec I;
  afficher_erreur "\n";
)

cible test:
application: iliad;
variables_temporaires: A0, A1, EVT;
A0 = 1.6;
A1 = 3.6;
calculer cible test_boucle : avec A0, A1;
afficher_erreur "\n";
afficher_erreur "nb_evenements() = " (nb_evenements()) "\n";
afficher_erreur "\n";
calculer cible afficher_evenements;
afficher_erreur "\n";
si nb_evenements() > 0 alors
  afficher_erreur "0: " nom(0, code) " = " (champ_evenement(0, code)) "\n";
  champ_evenement(0, code) = 456;
  afficher_erreur "1: " nom(0, code) " = " (champ_evenement(0, code)) "\n";
  afficher_erreur "0: montant " (champ_evenement(0, montant)) "\n";
  champ_evenement(0, montant) = 123.456;
  afficher_erreur "1: montant " (champ_evenement(0, montant)) "\n";
sinon
  afficher_erreur "!!! AUCUN EVENEMENT !!!\n";
finsi
afficher_erreur "\n";
arranger_evenements
: trier I, J : avec
  champ_evenement(I, rappel) <= champ_evenement(J, rappel)
  ou (
    champ_evenement(I, rappel) = champ_evenement(J, rappel)
    et champ_evenement(I, montant) <= champ_evenement(J, montant)
  )
: filtrer I : avec 32 <= champ_evenement(I, rappel) et champ_evenement(I, rappel) <= 55
: ajouter 3
: dans (
  calculer cible afficher_evenements;
)
#{
afficher_erreur "\n";
arranger_evenements
: trier I, J : avec champ_evenement(I, rappel) <= champ_evenement(J, rappel)
: dans (
  EVT = 25;
  afficher_erreur "0: ";
  calculer cible afficher_evenement : avec EVT;
  afficher_erreur "\n";
  iterer : variable I : 0 .. nb_evenements() increment 1 : dans (
    si inf(champ_evenement(I, rappel) % 2) = 0 alors
      afficher_erreur "0: ";
      calculer cible afficher_evenement : avec I;
      afficher_erreur "\n";
    finsi
  )
  afficher_erreur "\n";
  restaurer
  : evenements EVT
  : evenement I : avec inf(champ_evenement(I, rappel) % 2) = 0
  : apres (
    champ_evenement(EVT, montant) = 111111.111111;
    afficher_erreur "1: ";
    calculer cible afficher_evenement : avec EVT;
    afficher_erreur "\n";
    iterer : variable I : 0 .. nb_evenements() increment 1 : dans (
      si inf(champ_evenement(I, rappel) % 2) = 0 alors
        champ_evenement(I, montant) = 111111.111111;
        afficher_erreur "2: ";
        calculer cible afficher_evenement : avec I;
        afficher_erreur "\n";
      finsi
    )
  )
  afficher_erreur "\n";
  afficher_erreur "2: ";
  calculer cible afficher_evenement : avec EVT;
  afficher_erreur "\n";
  iterer : variable I : 0 .. nb_evenements() increment 1 : dans (
    si inf(champ_evenement(I, rappel) % 2) = 0 alors
      afficher_erreur "2: ";
      calculer cible afficher_evenement : avec I;
      afficher_erreur "\n";
    finsi
  )
)
}#

cible enchainement_primitif:
application: iliad;
variables_temporaires: EXPORTE_ERREUR;
#afficher_erreur "traite_double_liquidation2[\n";
calculer cible trace_in;
calculer cible ir_verif_saisie_isf;
finalise_erreurs;
EXPORTE_ERREUR = 1;
quand nb_anomalies() = 0 faire
  EXPORTE_ERREUR = 0;
puis_quand nb_discordances() + nb_informatives() = 0 faire
  calculer cible ir_verif_contexte;
  finalise_erreurs;
  EXPORTE_ERREUR = 0;
puis_quand nb_anomalies() = 0 faire
  calculer cible exporte_si_non_bloquantes;
  calculer cible ir_verif_famille;
  finalise_erreurs;
puis_quand nb_anomalies() = 0 faire
  EXPORTE_ERREUR = 1;
puis_quand nb_discordances() + nb_informatives() = 0 faire
  calculer cible ir_verif_revenu;
  finalise_erreurs;
puis_quand nb_anomalies() = 0 faire
  calculer cible exporte_si_non_bloquantes;
  calculer cible ir_calcul_primitif_isf;
#  finalise_erreurs;
  calculer cible enchaine_calcul;
  finalise_erreurs;
  calculer cible exporte_si_non_bloquantes;
sinon_faire
  si EXPORTE_ERREUR = 1 alors
    exporte_erreurs;
  finsi
finquand
calculer cible trace_out;
#afficher_erreur "]traite_double_liquidation2\n";
calculer cible test;

# primitif iterpréteur

cible enchainement_primitif_interpreteur:
application: iliad;
V_IND_TRAIT = 4; # primitif
calculer cible enchainement_primitif;

