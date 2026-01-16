# compir

espace_variables CORR : categorie saisie, calculee, base;
espace_variables D2042 : categorie saisie;
espace_variables D2042_ABAT : categorie saisie;
espace_variables D2042_RECT : categorie saisie;
espace_variables D2042_CTXT : categorie saisie;
espace_variables TL_D2042 : categorie saisie;
espace_variables TL_D2042_INIT : categorie saisie;
espace_variables TL_D2042_RECT : categorie saisie;
espace_variables TL_D2042_ABAT : categorie saisie;
espace_variables TL_D2042_ABAT_INIT : categorie saisie;
espace_variables TL_D2042_ABAT_RECT : categorie saisie;
espace_variables INR_D2042 : categorie saisie;
espace_variables INR_D2042_PROV_ANT : categorie saisie;
espace_variables INR_D2042_REFR99R_ANT : categorie saisie;
espace_variables INR_D2042_R9901_ANT : categorie saisie;
espace_variables MAJO_D2042_STRATE : categorie saisie;
espace_variables MAJO_D2042_REF_ABAT : categorie saisie;
espace_variables MAJO_D2042_P : categorie saisie;
espace_variables MAJO_D2042_ABAT_P : categorie saisie;
espace_variables MAJO_D2042_ABAT_STRATE : categorie saisie;

ANNEE_REVENU : calculee primrest = 0 : "" ;
NB_STRATES : calculee primrest = 0 : "" ;
MODE_CORR :  saisie environnement toto = 0 alias EST_CORR : "" ;

# Données globales
PRESENT_8VV : calculee primrest = 0 : "" ;
PRESENT_8VW : calculee primrest = 0 : "" ;
PRESENT_9YT : calculee primrest = 0 : "" ;
MONTANT_9YT : calculee primrest = 0 : "" ;
PENALITE_9YT : calculee primrest = 0 : "" ;
SENS_9YT : calculee primrest = 0 : "" ;
NUM_EVT_9YT : calculee primrest = 0 : "" ;
IND_20_9YT : calculee primrest = 0 : "" ;
BASE_TL_9YT : calculee primrest = 0 : "" ;
PRESENT_9YU : calculee primrest = 0 : "" ;
MONTANT_9YU : calculee primrest = 0 : "" ;
PENALITE_9YU : calculee primrest = 0 : "" ;
SENS_9YU : calculee primrest = 0 : "" ;
NUM_EVT_9YU : calculee primrest = 0 : "" ;
DATE_9YU : calculee primrest = 0 : "" ;
PRESENT_9XT : calculee primrest = 0 : "" ;
MONTANT_9XT : calculee primrest = 0 : "" ;
PENALITE_9XT : calculee primrest = 0 : "" ;
SENS_9XT : calculee primrest = 0 : "" ;
NUM_EVT_9XT : calculee primrest = 0 : "" ;
IND_20_9XT : calculee primrest = 0 : "" ;
BASE_TL_9XT : calculee primrest = 0 : "" ;
PRESENT_9XU : calculee primrest = 0 : "" ;
MONTANT_9XU : calculee primrest = 0 : "" ;
PENALITE_9XU : calculee primrest = 0 : "" ;
SENS_9XU : calculee primrest = 0 : "" ;
NUM_EVT_9XU : calculee primrest = 0 : "" ;
DATE_9XU : calculee primrest = 0 : "" ;
PRESENT_REGCO : calculee primrest = 0 : "" ;
PENALITE_REGCO : calculee primrest = 0 : "" ;
NUM_EVT_REGCO : calculee primrest = 0 : "" ;
NUM_RAP_REGCO : calculee primrest = 0 : "" ;
IND_20_REGCO : calculee primrest = 0 : "" ;
PREMIER_EVT : calculee primrest = 0 : "" ;
CODE_9ZA : calculee primrest = 0 : "" ;
SAUVE_INR_R99 : calculee primrest = 0 : "" ;
ANNEE_DECES_CONJOINT : calculee primrest = 0 : "" ;
NB_RAPPELS_RES : calculee primrest = 0 : "" ;

# sommes inr 2
INR_SOMMEBAND_2 : calculee primrest = 0 : "" ;
INR_SOMMEBA_2 : calculee primrest = 0 : "" ;
INR_SOMMEBICND_2 : calculee primrest = 0 : "" ;
INR_SOMMEBIC_2 : calculee primrest = 0 : "" ;
INR_SOMMEBNCND_2 : calculee primrest = 0 : "" ;
INR_SOMMEBNC_2 : calculee primrest = 0 : "" ;
INR_SOMMEGLOBAL_2 : calculee primrest = 0 : "" ;
INR_SOMMEGLOBND_2 : calculee primrest = 0 : "" ;
INR_SOMMELOC_2 : calculee primrest = 0 : "" ;
INR_SOMMEMOND_2 : calculee primrest = 0 : "" ;
INR_SOMMERCM_2 : calculee primrest = 0 : "" ;
INR_SOMMERF_2 : calculee primrest = 0 : "" ;
INR_SOMMERI_2 : calculee primrest = 0 : "" ;

# Données de Reference
D2042_NB : calculee primrest = 0 : "" ;
DEFAUT : calculee primrest = 0 : "" ;
DEFAUT10 : calculee primrest = 0 : "" ;
DEFAUT11 : calculee primrest = 0 : "" ;
DEFAUT1011 : calculee primrest = 0 : "" ;
RETARD : calculee primrest = 0 : "" ;
RETARD07 : calculee primrest = 0 : "" ;
RETARD08 : calculee primrest = 0 : "" ;
RETARD0718 : calculee primrest = 0 : "" ;
RETARD101718 : calculee primrest = 0 : "" ;
RETARD22 : calculee primrest = 0 : "" ;
RETARD99 : calculee primrest = 0 : "" ;
RECTIF : calculee primrest = 0 : "" ;
RECTIF_MAJO : calculee primrest = 0 : "" ;
MENTION_EXP : calculee primrest = 0 : "" ;
CORR_RJLJ : calculee primrest = 0 : "" ;
CODE_PENA : calculee primrest = 0 : "" ;
CODE_PENA_ISF : calculee primrest = 0 : "" ;
DATE : calculee primrest = 0 : "" ;
SF_INITIALE : calculee primrest = 0 : "" ;
SF_COURANTE : calculee primrest = 0 : "" ;
SF_PRIMITIF : calculee primrest = 0 : "" ;
R_TARDIF : calculee primrest = 0 : "" ;
LIMELIGHT : calculee primrest = 0 : "" ;
ISF_PRIM : calculee primrest = 0 : "" ;
PRESENT_R10 : calculee primrest = 0 : "" ;
PRESENT_R30 : calculee primrest = 0 : "" ;
PRESENT_R32 : calculee primrest = 0 : "" ;
PREM_8_11 : calculee primrest = 0 : "" ;
PENA_994 : calculee primrest = 0 : "" ;
FLAGMENC : calculee primrest = 0 : "" ;

# tl
TL_D2042_NB : calculee primrest = 0 : "" ;
TL_D2042_INIT_NB : calculee primrest = 0 : "" ;
TL_D2042_RECT_NB : calculee primrest = 0 : "" ;
TL_BASE_TL : calculee primrest = 0 : "" ;
TL_BASE_TL_INIT : calculee primrest = 0 : "" ;
TL_BASE_TL_RECT : calculee primrest = 0 : "" ;
TL_BASE_TL_TBTC : calculee primrest = 0 : "" ;
TL_BASE_TL_TBTC_INIT : calculee primrest = 0 : "" ;
TL_BASE_TL_TBTC_RECT : calculee primrest = 0 : "" ;
TL_NON_ACQUISE : calculee primrest = 0 : "" ;

# tl mauvaise foi
TL_MF_MFCDIS : calculee primrest = 0 : "" ;
TL_MF_MFCHR : calculee primrest = 0 : "" ;
TL_MF_MFCHR7 : calculee primrest = 0 : "" ;
TL_MF_MFCS : calculee primrest = 0 : "" ;
TL_MF_MFCSAL : calculee primrest = 0 : "" ;
TL_MF_MFCVN : calculee primrest = 0 : "" ;
TL_MF_MFGAIN : calculee primrest = 0 : "" ;
TL_MF_MFGLO : calculee primrest = 0 : "" ;
TL_MF_MFIFI : calculee primrest = 0 : "" ;
TL_MF_MFIR : calculee primrest = 0 : "" ;
TL_MF_MFLOY : calculee primrest = 0 : "" ;
TL_MF_MFMCSG820 : calculee primrest = 0 : "" ;
TL_MF_MFPCAP : calculee primrest = 0 : "" ;
TL_MF_MFPS : calculee primrest = 0 : "" ;
TL_MF_MFPSOL : calculee primrest = 0 : "" ;
TL_MF_MFRD : calculee primrest = 0 : "" ;
TL_MF_MFREGV : calculee primrest = 0 : "" ;
TL_MF_MFRSE1 : calculee primrest = 0 : "" ;
TL_MF_MFRSE2 : calculee primrest = 0 : "" ;
TL_MF_MFRSE3 : calculee primrest = 0 : "" ;
TL_MF_MFRSE4 : calculee primrest = 0 : "" ;
TL_MF_MFRSE5 : calculee primrest = 0 : "" ;
TL_MF_MFRSE6 : calculee primrest = 0 : "" ;
TL_MF_MFRSE7 : calculee primrest = 0 : "" ;
TL_MF_MFTAXAGA : calculee primrest = 0 : "" ;

# inr
INR_NB_MOIS : calculee primrest = 0 : "" ;
INR_NB_MOIS2 : calculee primrest = 0 : "" ;
INR_NB_MOIS_ISF : calculee primrest = 0 : "" ;
INR_ANNEE_COR : calculee primrest = 0 : "" ;
INR_PASDINR : calculee primrest = 0 : "" ;

# majo
MAJO_D2042_STRATE_NB : calculee primrest = 0 : "" ;
MAJO_D2042_P_NB : calculee primrest = 0 : "" ;
MAJO_TAUX_STRATE : calculee primrest = 0 : "" ;
MAJO_CODE_STRATE : calculee primrest = 0 : "" ;
MAJO_TARDIF_EVT2 : calculee primrest = 0 : "" ;
MAJO_STR_TR_00 : calculee primrest = 0 : "" ;
MAJO_STR_TR_01 : calculee primrest = 0 : "" ;
MAJO_STR_TR_02 : calculee primrest = 0 : "" ;
MAJO_STR_TR_03 : calculee primrest = 0 : "" ;
MAJO_STR_TR_04 : calculee primrest = 0 : "" ;
MAJO_STR_TR_05 : calculee primrest = 0 : "" ;
MAJO_STR_TR_06 : calculee primrest = 0 : "" ;
MAJO_STR_TR_07 : calculee primrest = 0 : "" ;
MAJO_STR_TR_08 : calculee primrest = 0 : "" ;
MAJO_STR_TR_09 : calculee primrest = 0 : "" ;
MAJO_STR_TR_10 : calculee primrest = 0 : "" ;
MAJO_STR_TR_11 : calculee primrest = 0 : "" ;
MAJO_STR_TR_12 : calculee primrest = 0 : "" ;
MAJO_STR_TR_13 : calculee primrest = 0 : "" ;
MAJO_STR_TR_14 : calculee primrest = 0 : "" ;
MAJO_STR_TR_15 : calculee primrest = 0 : "" ;
MAJO_STR_TR_16 : calculee primrest = 0 : "" ;
MAJO_STR_TR_17 : calculee primrest = 0 : "" ;
MAJO_STR_TR_18 : calculee primrest = 0 : "" ;
MAJO_STR_TR_19 : calculee primrest = 0 : "" ;
MAJO_STR_TR_20 : calculee primrest = 0 : "" ;
MAJO_STR_TR_21 : calculee primrest = 0 : "" ;
MAJO_STR_TR_22 : calculee primrest = 0 : "" ;
MAJO_STR_TR_23 : calculee primrest = 0 : "" ;
MAJO_STR_TR_24 : calculee primrest = 0 : "" ;
MAJO_STR_TR_25 : calculee primrest = 0 : "" ;
MAJO_PREM_STR : calculee primrest = 0 : "" ;
MAJO_NB_STR_TR : calculee primrest = 0 : "" ;
MAJO_DERN_STR_TR : calculee primrest = 0 : "" ;
MAJO_NUM_STRATE : calculee primrest = 0 : "" ;

# prorata
MAJO_T_RABP : calculee primrest = 0 : "" ;
MAJO_T_RABP07 : calculee primrest = 0 : "" ;
MAJO_T_RABP08 : calculee primrest = 0 : "" ;
MAJO_T_RABP09 : calculee primrest = 0 : "" ;
MAJO_T_RABP10 : calculee primrest = 0 : "" ;
MAJO_T_RABP11 : calculee primrest = 0 : "" ;
MAJO_T_RABP12 : calculee primrest = 0 : "" ;
MAJO_T_RABP17 : calculee primrest = 0 : "" ;
MAJO_T_RABP31 : calculee primrest = 0 : "" ;
MAJO_T_RABPPS : calculee primrest = 0 : "" ;
MAJO_T_RABPPS07 : calculee primrest = 0 : "" ;
MAJO_T_RABPPS08 : calculee primrest = 0 : "" ;
MAJO_T_RABPPS09 : calculee primrest = 0 : "" ;
MAJO_T_RABPPS10 : calculee primrest = 0 : "" ;
MAJO_T_RABPPS11 : calculee primrest = 0 : "" ;
MAJO_T_RABPPS12 : calculee primrest = 0 : "" ;
MAJO_T_RABPPS17 : calculee primrest = 0 : "" ;
MAJO_T_RABPPS31 : calculee primrest = 0 : "" ;
MAJO_T_RABPCS : calculee primrest = 0 : "" ;
MAJO_T_RABPCS07 : calculee primrest = 0 : "" ;
MAJO_T_RABPCS08 : calculee primrest = 0 : "" ;
MAJO_T_RABPCS09 : calculee primrest = 0 : "" ;
MAJO_T_RABPCS10 : calculee primrest = 0 : "" ;
MAJO_T_RABPCS11 : calculee primrest = 0 : "" ;
MAJO_T_RABPCS12 : calculee primrest = 0 : "" ;
MAJO_T_RABPCS17 : calculee primrest = 0 : "" ;
MAJO_T_RABPCS31 : calculee primrest = 0 : "" ;
MAJO_T_RABPRD : calculee primrest = 0 : "" ;
MAJO_T_RABPRD07 : calculee primrest = 0 : "" ;
MAJO_T_RABPRD08 : calculee primrest = 0 : "" ;
MAJO_T_RABPRD09 : calculee primrest = 0 : "" ;
MAJO_T_RABPRD10 : calculee primrest = 0 : "" ;
MAJO_T_RABPRD11 : calculee primrest = 0 : "" ;
MAJO_T_RABPRD12 : calculee primrest = 0 : "" ;
MAJO_T_RABPRD17 : calculee primrest = 0 : "" ;
MAJO_T_RABPRD31 : calculee primrest = 0 : "" ;
MAJO_T_RABPCH : calculee primrest = 0 : "" ;
MAJO_T_RABPCH07 : calculee primrest = 0 : "" ;
MAJO_T_RABPCH08 : calculee primrest = 0 : "" ;
MAJO_T_RABPCH09 : calculee primrest = 0 : "" ;
MAJO_T_RABPCH10 : calculee primrest = 0 : "" ;
MAJO_T_RABPCH11 : calculee primrest = 0 : "" ;
MAJO_T_RABPCH12 : calculee primrest = 0 : "" ;
MAJO_T_RABPCH17 : calculee primrest = 0 : "" ;
MAJO_T_RABPCH31 : calculee primrest = 0 : "" ;
MAJO_T_RABPLO : calculee primrest = 0 : "" ;
MAJO_T_RABPLO07 : calculee primrest = 0 : "" ;
MAJO_T_RABPLO08 : calculee primrest = 0 : "" ;
MAJO_T_RABPLO09 : calculee primrest = 0 : "" ;
MAJO_T_RABPLO10 : calculee primrest = 0 : "" ;
MAJO_T_RABPLO11 : calculee primrest = 0 : "" ;
MAJO_T_RABPLO12 : calculee primrest = 0 : "" ;
MAJO_T_RABPLO17 : calculee primrest = 0 : "" ;
MAJO_T_RABPLO31 : calculee primrest = 0 : "" ;

# art1731
ART1731_SOMME_R3032 : calculee primrest = 0 : "" ;
ART1731_PRESENT_R10 : calculee primrest = 0 : "" ;
ART1731_PRESENT_R30 : calculee primrest = 0 : "" ;
ART1731_PRESENT_R32 : calculee primrest = 0 : "" ;

# rappels
NB_RAPPELS : calculee primrest = 0 : "" ;
NB_NOUVEAUX_RAPPELS : calculee primrest = 0 : "" ;
NOUVEAU_RAPPEL : calculee primrest = 0 : "" ;

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

# ???

cible traite_double_liquidation_2_prim:
application: iliad;
calculer cible traite_double_liquidation_2 : espace GLOBAL;

cible traite_double_liquidation_2_corr:
application: iliad;
nettoie_erreurs;
calculer cible traite_double_liquidation_2 : espace CORR;

cible sauve_base_initial_prim:
application: iliad;
calculer cible sauve_base_initial : espace GLOBAL;

cible sauve_base_initial_corr:
application: iliad;
calculer cible sauve_base_initial : espace CORR;

cible sauve_base_1728_prim:
application: iliad;
calculer cible sauve_base_1728 : espace GLOBAL;

cible sauve_base_1728_corr:
application: iliad;
calculer cible sauve_base_1728 : espace CORR;

cible sauve_base_anterieure_prim:
application: iliad;
calculer cible sauve_base_anterieure : espace GLOBAL;

cible sauve_base_anterieure_corr:
application: iliad;
calculer cible sauve_base_anterieure : espace CORR;

cible sauve_base_anterieure_cor_prim:
application: iliad;
calculer cible sauve_base_anterieure_cor : espace GLOBAL;

cible sauve_base_anterieure_cor_corr:
application: iliad;
calculer cible sauve_base_anterieure_cor : espace CORR;

cible sauve_base_inr_inter22_prim:
application: iliad;
calculer cible sauve_base_inr_inter22 : espace GLOBAL;

cible sauve_base_inr_inter22_corr:
application: iliad;
calculer cible sauve_base_inr_inter22 : espace CORR;

cible verif_saisie_cohe_primitive_prim:
application: iliad;
calculer cible verif_saisie_cohe_primitive : espace GLOBAL;

cible verif_saisie_cohe_primitive_corr:
application: iliad;
calculer cible verif_saisie_cohe_primitive : espace CORR;

cible verif_saisie_cohe_corrective_prim:
application: iliad;
calculer cible verif_saisie_cohe_corrective : espace GLOBAL;

cible verif_saisie_cohe_corrective_corr:
application: iliad;
calculer cible verif_saisie_cohe_corrective : espace CORR;

cible verif_cohe_horizontale_corr:
application: iliad;
calculer cible verif_cohe_horizontale : espace CORR;

cible calcul_primitif_isf_prim:
application: iliad;
calculer cible calcul_primitif_isf : espace GLOBAL;

cible verif_calcul_primitive_isf_prim:
application: iliad;
calculer cible verif_calcul_primitive_isf : espace GLOBAL;

cible ench_tl_corr :
application: iliad;
calculer cible ENCH_TL : espace CORR;

cible sauve_base_inr_r9901_corr :
application: iliad;
calculer cible sauve_base_inr_r9901 : espace CORR;

cible sauve_base_HR_corr :
application: iliad;
calculer cible sauve_base_HR : espace CORR;

cible sauve_base_inr_ref_corr :
application: iliad;
calculer cible sauve_base_inr_ref : espace CORR;

cible sauve_base_inr_ntl_corr :
application: iliad;
calculer cible sauve_base_inr_ntl : espace CORR;

cible sauve_base_abat98_corr :
application: iliad;
calculer cible sauve_base_abat98 : espace CORR;

cible sauve_base_abat99_corr :
application: iliad;
calculer cible sauve_base_abat99 : espace CORR;

cible sauve_base_inr_corr :
application: iliad;
calculer cible sauve_base_inr : espace CORR;

cible sauve_base_inr_intertl_corr :
application: iliad;
calculer cible sauve_base_inr_intertl : espace CORR;

cible sauve_base_inr_ntl22_corr :
application: iliad;
calculer cible sauve_base_inr_ntl22 : espace CORR;

cible sauve_base_inr_ntl24_corr :
application: iliad;
calculer cible sauve_base_inr_ntl24 : espace CORR;

cible sauve_base_inr_tl22_corr :
application: iliad;
calculer cible sauve_base_inr_tl22 : espace CORR;

cible sauve_base_inr_tl24_corr :
application: iliad;
calculer cible sauve_base_inr_tl24 : espace CORR;

cible sauve_base_inr_tl_corr :
application: iliad;
calculer cible sauve_base_inr_tl : espace CORR;

cible sauve_base_majo_corr :
application: iliad;
calculer cible sauve_base_majo : espace CORR;

cible sauve_base_premier_corr :
application: iliad;
calculer cible sauve_base_premier : espace CORR;

cible sauve_base_stratemajo_corr :
application: iliad;
calculer cible sauve_base_stratemajo : espace CORR;

cible sauve_base_tl_corr :
application: iliad;
calculer cible sauve_base_tl : espace CORR;

cible sauve_base_tl_init_corr :
application: iliad;
calculer cible sauve_base_tl_init : espace CORR;

cible sauve_base_tl_rect_corr :
application: iliad;
calculer cible sauve_base_tl_rect : espace CORR;

cible verif_calcul_corrective_corr :
application: iliad;
calculer cible verif_calcul_corrective : espace CORR;

