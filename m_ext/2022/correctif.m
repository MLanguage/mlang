# correctif

MAX_ID_EVT: calculee primrest = 0 : "" ;

TMP_ARG1 : calculee primrest = 0 : "" ;
TMP_ARG2 : calculee primrest = 0 : "" ;
TMP_ARG3 : calculee primrest = 0 : "" ;
TMP_ARG4 : calculee primrest = 0 : "" ;
TMP_ARG5 : calculee primrest = 0 : "" ;
TMP_ARG6 : calculee primrest = 0 : "" ;
TMP_ARG7 : calculee primrest = 0 : "" ;
TMP_ARG8 : calculee primrest = 0 : "" ;
TMP_ARG9 : calculee primrest = 0 : "" ;
TMP_RES : calculee primrest = 0 : "" ;
TMP_RES1 : calculee primrest = 0 : "" ;
TMP_RES2 : calculee primrest = 0 : "" ;
TMP_RES3 : calculee primrest = 0 : "" ;
TMP_RES4 : calculee primrest = 0 : "" ;

N_INDEFINIE : const = 0;
N_REVENU : const = 1;
N_CHARGE : const = 2;

SENS_R : const = 0;
SENS_M : const = 1;
SENS_P : const = 2;
SENS_C : const = 3;

SF_INVALIDE : const = 0;
SF_MARIAGE : const = 1;
SF_CELIBAT : const = 2;
SF_DIVORCE : const = 3;
SF_PACSE : const = 4;
SF_VEUVAGE_TRUE : const = 5;
SF_VEUVAGE_FALSE : const = 6;

TL_TL_ACQUISE : const = 0;
TL_TL_DEFAUT_2042 : const = 1;
TL_TL_MAUVAISE_FOI : const = 2;

MAJ_TL : const = 0;
MAJ_NON_TL : const = 1;
MAJ_TL15 : const = 2;
MAJ_NON_TL15 : const = 3;
MAJ_RAPPEL_C : const = 4;
MAJ_RAPPEL_CP : const = 5;
MAJ_RAPPEL_CP01 : const = 6;
MAJ_RAPPEL_CP22 : const = 7;
MAJ_RAPPEL_CP24 : const = 8;
MAJ_RAPPEL_F : const = 9;
MAJ_RAPPEL_NF : const = 10;
MAJ_RAPPEL_M : const = 11;
MAJ_RAPPEL_MF : const = 12;
MAJ_RAPPEL_NON_M : const = 13;
MAJ_RAPPEL_P : const = 14;
MAJ_RAPPEL_R : const = 15;
MAJ_RAPPEL_R55 : const = 16;
MAJ_1728 : const = 17;
MAJ_ABAT_20 : const = 18;
MAJ_CODE_1729_2A5 : const = 19;
MAJ_CODE_1729_6 : const = 20;
MAJ_CODE_22 : const = 21;
MAJ_CODE_24 : const = 22;
MAJ_CONTEXTE_22 : const = 23;
MAJ_MENTION_EXP_99 : const = 24;
MAJ_MENTION_EXP_99R : const = 25;
MAJ_NON_MENTION_EXP : const = 26;

INR_FLAG_INR_NON_TL : const = 2;
INR_FLAG_INR_TL : const = 3;

ID_SANS_TRAITEMENT : const = 0;
ID_CUMULE_CHAMP_BASE_TL : const = 1;
ID_CUMULE_BASE_TL_INIT : const = 2;
ID_CUMULE_CHAMP_BASE_TL_RECT : const = 3;
ID_TRAITE_MAJO_P : const = 4;


cible init_variables:
application: iliad;
GLOBAL.ANNEE_REVENU = GLOBAL.V_ANREV;
GLOBAL.NB_STRATES = 26;
GLOBAL.MAX_ID_EVT = -2;
# Données globales
GLOBAL.PRESENT_8VV = 0;
GLOBAL.PRESENT_8VW = 0;
GLOBAL.PRESENT_9YT = 0;
GLOBAL.MONTANT_9YT = 0;
GLOBAL.PENALITE_9YT = 0;
GLOBAL.SENS_9YT = SENS_R;
GLOBAL.NUM_EVT_9YT = 0;
GLOBAL.IND_20_9YT = 0;
GLOBAL.BASE_TL_9YT = 0;
GLOBAL.PRESENT_9YU = 0;
GLOBAL.MONTANT_9YU = 0;
GLOBAL.PENALITE_9YU = 0;
GLOBAL.SENS_9YU = SENS_R;
GLOBAL.NUM_EVT_9YU = 0;
GLOBAL.DATE_9YU = 0;
GLOBAL.PRESENT_9XT = 0;
GLOBAL.MONTANT_9XT = 0;
GLOBAL.PENALITE_9XT = 0;
GLOBAL.SENS_9XT = SENS_R;
GLOBAL.NUM_EVT_9XT = 0;
GLOBAL.IND_20_9XT = 0;
GLOBAL.BASE_TL_9XT = 0;
GLOBAL.PRESENT_9XU = 0;
GLOBAL.MONTANT_9XU = 0;
GLOBAL.PENALITE_9XU = 0;
GLOBAL.SENS_9XU = SENS_R;
GLOBAL.NUM_EVT_9XU = 0;
GLOBAL.DATE_9XU = 0;
GLOBAL.PRESENT_REGCO = 0;
GLOBAL.PENALITE_REGCO = 0;
GLOBAL.NUM_EVT_REGCO = 0;
GLOBAL.NUM_RAP_REGCO = 0;
GLOBAL.IND_20_REGCO = 0;
GLOBAL.PREMIER_EVT = 0;
GLOBAL.CODE_9ZA = 0;
GLOBAL.SAUVE_INR_R99 = 0;
GLOBAL.ANNEE_DECES_CONJOINT = 0;
GLOBAL.NB_RAPPELS_RES = 0;
# sommes inr 2
GLOBAL.INR_SOMMEBAND_2 = 0;
GLOBAL.INR_SOMMEBA_2 = 0;
GLOBAL.INR_SOMMEBICND_2 = 0;
GLOBAL.INR_SOMMEBIC_2 = 0;
GLOBAL.INR_SOMMEBNCND_2 = 0;
GLOBAL.INR_SOMMEBNC_2 = 0;
GLOBAL.INR_SOMMEGLOBAL_2 = 0;
GLOBAL.INR_SOMMEGLOBND_2 = 0;
GLOBAL.INR_SOMMELOC_2 = 0;
GLOBAL.INR_SOMMEMOND_2 = 0;
GLOBAL.INR_SOMMERCM_2 = 0;
GLOBAL.INR_SOMMERF_2 = 0;
GLOBAL.INR_SOMMERI_2 = 0;
# Données de Reference
GLOBAL.D2042_NB = 0;
GLOBAL.DEFAUT = 0;
GLOBAL.DEFAUT10 = 0;
GLOBAL.DEFAUT11 = 0;
GLOBAL.DEFAUT1011 = 0;
GLOBAL.RETARD = 0;
GLOBAL.RETARD07 = 0;
GLOBAL.RETARD08 = 0;
GLOBAL.RETARD0718 = 0;
GLOBAL.RETARD101718 = 0;
GLOBAL.RETARD22 = 0;
GLOBAL.RETARD99 = 0;
GLOBAL.RECTIF = 0;
GLOBAL.RECTIF_MAJO = 0;
GLOBAL.MENTION_EXP = 0;
GLOBAL.CORR_RJLJ = 0;
GLOBAL.CODE_PENA = 0;
GLOBAL.CODE_PENA_ISF = 0;
GLOBAL.DATE = 0;
GLOBAL.SF_INITIALE = SF_INVALIDE;
GLOBAL.SF_COURANTE = SF_INVALIDE;
GLOBAL.SF_PRIMITIF = 0;
GLOBAL.R_TARDIF = 0;
GLOBAL.LIMELIGHT = 0;
GLOBAL.ISF_PRIM = 0;
GLOBAL.PRESENT_R10 = 0;
GLOBAL.PRESENT_R30 = 0;
GLOBAL.PRESENT_R32 = 0;
GLOBAL.PREM_8_11 = 0;
GLOBAL.PENA_994 = 0;
GLOBAL.FLAGMENC = 0;
# tl
GLOBAL.TL_D2042_NB = 0;
GLOBAL.TL_D2042_INIT_NB = 0;
GLOBAL.TL_D2042_RECT_NB = 0;
GLOBAL.TL_BASE_TL = 0;
GLOBAL.TL_BASE_TL_INIT = 0;
GLOBAL.TL_BASE_TL_RECT = 0;
GLOBAL.TL_BASE_TL_TBTC = 0;
GLOBAL.TL_BASE_TL_TBTC_INIT = 0;
GLOBAL.TL_BASE_TL_TBTC_RECT = 0;
GLOBAL.TL_NON_ACQUISE = TL_TL_ACQUISE;
# tl mauvaise foi
GLOBAL.TL_MF_MFCDIS = 0;
GLOBAL.TL_MF_MFCHR = 0;
GLOBAL.TL_MF_MFCHR7 = 0;
GLOBAL.TL_MF_MFCS = 0;
GLOBAL.TL_MF_MFCSAL = 0;
GLOBAL.TL_MF_MFCVN = 0;
GLOBAL.TL_MF_MFGAIN = 0;
GLOBAL.TL_MF_MFGLO = 0;
GLOBAL.TL_MF_MFIFI = 0;
GLOBAL.TL_MF_MFIR = 0;
GLOBAL.TL_MF_MFLOY = 0;
GLOBAL.TL_MF_MFMCSG820 = 0;
GLOBAL.TL_MF_MFPCAP = 0;
GLOBAL.TL_MF_MFPS = 0;
GLOBAL.TL_MF_MFPSOL = 0;
GLOBAL.TL_MF_MFRD = 0;
GLOBAL.TL_MF_MFREGV = 0;
GLOBAL.TL_MF_MFRSE1 = 0;
GLOBAL.TL_MF_MFRSE2 = 0;
GLOBAL.TL_MF_MFRSE3 = 0;
GLOBAL.TL_MF_MFRSE4 = 0;
GLOBAL.TL_MF_MFRSE5 = 0;
GLOBAL.TL_MF_MFRSE6 = 0;
GLOBAL.TL_MF_MFRSE7 = 0;
GLOBAL.TL_MF_MFTAXAGA = 0;
# inr
GLOBAL.INR_NB_MOIS = 0;
GLOBAL.INR_NB_MOIS2 = 0;
GLOBAL.INR_NB_MOIS_ISF = 0;
GLOBAL.INR_ANNEE_COR = 0;
GLOBAL.INR_PASDINR = 0;
# majo
GLOBAL.MAJO_D2042_STRATE_NB = 0;
GLOBAL.MAJO_D2042_P_NB = 0;
GLOBAL.MAJO_TAUX_STRATE = 0;
GLOBAL.MAJO_CODE_STRATE = 0;
GLOBAL.MAJO_TARDIF_EVT2 = 0;
GLOBAL.MAJO_STR_TR_00 = 0;
GLOBAL.MAJO_STR_TR_01 = 0;
GLOBAL.MAJO_STR_TR_02 = 0;
GLOBAL.MAJO_STR_TR_03 = 0;
GLOBAL.MAJO_STR_TR_04 = 0;
GLOBAL.MAJO_STR_TR_05 = 0;
GLOBAL.MAJO_STR_TR_06 = 0;
GLOBAL.MAJO_STR_TR_07 = 0;
GLOBAL.MAJO_STR_TR_08 = 0;
GLOBAL.MAJO_STR_TR_09 = 0;
GLOBAL.MAJO_STR_TR_10 = 0;
GLOBAL.MAJO_STR_TR_11 = 0;
GLOBAL.MAJO_STR_TR_12 = 0;
GLOBAL.MAJO_STR_TR_13 = 0;
GLOBAL.MAJO_STR_TR_14 = 0;
GLOBAL.MAJO_STR_TR_15 = 0;
GLOBAL.MAJO_STR_TR_16 = 0;
GLOBAL.MAJO_STR_TR_17 = 0;
GLOBAL.MAJO_STR_TR_18 = 0;
GLOBAL.MAJO_STR_TR_19 = 0;
GLOBAL.MAJO_STR_TR_20 = 0;
GLOBAL.MAJO_STR_TR_21 = 0;
GLOBAL.MAJO_STR_TR_22 = 0;
GLOBAL.MAJO_STR_TR_23 = 0;
GLOBAL.MAJO_STR_TR_24 = 0;
GLOBAL.MAJO_STR_TR_25 = 0;
GLOBAL.MAJO_PREM_STR = 0;
GLOBAL.MAJO_NB_STR_TR = 0;
GLOBAL.MAJO_DERN_STR_TR = 0;
GLOBAL.MAJO_NUM_STRATE = 0;
# prorata
GLOBAL.MAJO_T_RABP = 0;
GLOBAL.MAJO_T_RABP07 = 0;
GLOBAL.MAJO_T_RABP08 = 0;
GLOBAL.MAJO_T_RABP09 = 0;
GLOBAL.MAJO_T_RABP10 = 0;
GLOBAL.MAJO_T_RABP11 = 0;
GLOBAL.MAJO_T_RABP12 = 0;
GLOBAL.MAJO_T_RABP17 = 0;
GLOBAL.MAJO_T_RABP31 = 0;
GLOBAL.MAJO_T_RABPPS = 0;
GLOBAL.MAJO_T_RABPPS07 = 0;
GLOBAL.MAJO_T_RABPPS08 = 0;
GLOBAL.MAJO_T_RABPPS09 = 0;
GLOBAL.MAJO_T_RABPPS10 = 0;
GLOBAL.MAJO_T_RABPPS11 = 0;
GLOBAL.MAJO_T_RABPPS12 = 0;
GLOBAL.MAJO_T_RABPPS17 = 0;
GLOBAL.MAJO_T_RABPPS31 = 0;
GLOBAL.MAJO_T_RABPCS = 0;
GLOBAL.MAJO_T_RABPCS07 = 0;
GLOBAL.MAJO_T_RABPCS08 = 0;
GLOBAL.MAJO_T_RABPCS09 = 0;
GLOBAL.MAJO_T_RABPCS10 = 0;
GLOBAL.MAJO_T_RABPCS11 = 0;
GLOBAL.MAJO_T_RABPCS12 = 0;
GLOBAL.MAJO_T_RABPCS17 = 0;
GLOBAL.MAJO_T_RABPCS31 = 0;
GLOBAL.MAJO_T_RABPRD = 0;
GLOBAL.MAJO_T_RABPRD07 = 0;
GLOBAL.MAJO_T_RABPRD08 = 0;
GLOBAL.MAJO_T_RABPRD09 = 0;
GLOBAL.MAJO_T_RABPRD10 = 0;
GLOBAL.MAJO_T_RABPRD11 = 0;
GLOBAL.MAJO_T_RABPRD12 = 0;
GLOBAL.MAJO_T_RABPRD17 = 0;
GLOBAL.MAJO_T_RABPRD31 = 0;
GLOBAL.MAJO_T_RABPCH = 0;
GLOBAL.MAJO_T_RABPCH07 = 0;
GLOBAL.MAJO_T_RABPCH08 = 0;
GLOBAL.MAJO_T_RABPCH09 = 0;
GLOBAL.MAJO_T_RABPCH10 = 0;
GLOBAL.MAJO_T_RABPCH11 = 0;
GLOBAL.MAJO_T_RABPCH12 = 0;
GLOBAL.MAJO_T_RABPCH17 = 0;
GLOBAL.MAJO_T_RABPCH31 = 0;
GLOBAL.MAJO_T_RABPLO = 0;
GLOBAL.MAJO_T_RABPLO07 = 0;
GLOBAL.MAJO_T_RABPLO08 = 0;
GLOBAL.MAJO_T_RABPLO09 = 0;
GLOBAL.MAJO_T_RABPLO10 = 0;
GLOBAL.MAJO_T_RABPLO11 = 0;
GLOBAL.MAJO_T_RABPLO12 = 0;
GLOBAL.MAJO_T_RABPLO17 = 0;
GLOBAL.MAJO_T_RABPLO31 = 0;
# art1731
GLOBAL.ART1731_SOMME_R3032 = 0;
GLOBAL.ART1731_PRESENT_R10 = 0;
GLOBAL.ART1731_PRESENT_R30 = 0;
GLOBAL.ART1731_PRESENT_R32 = 0;

cible signaler_erreurs:
application: iliad;
si nb_anomalies() > 0 alors
  nettoie_erreurs_finalisees;
  finalise_erreurs;
  exporte_erreurs;
  iterer
  : variable VAR
  : categorie *
  : espace CORR
  : dans (
    GLOBAL.VAR = VAR;
  )
  stop application;
sinon
  finalise_erreurs;
finsi

cible signaler_erreur_ano:
application: iliad;
finalise_erreurs;
exporte_erreurs;
iterer
: variable VAR
: categorie *
: espace CORR
: dans (
  GLOBAL.VAR = VAR;
)
stop application;

cible init_majo_str_tr :
application: iliad;
arguments: I, B;
aiguillage (I) : (
  cas 0: MAJO_STR_TR_00 = B;
  cas 1: MAJO_STR_TR_01 = B;
  cas 2: MAJO_STR_TR_02 = B;
  cas 3: MAJO_STR_TR_03 = B;
  cas 4: MAJO_STR_TR_04 = B;
  cas 5: MAJO_STR_TR_05 = B;
  cas 6: MAJO_STR_TR_06 = B;
  cas 7: MAJO_STR_TR_07 = B;
  cas 8: MAJO_STR_TR_08 = B;
  cas 9: MAJO_STR_TR_09 = B;
  cas 10: MAJO_STR_TR_10 = B;
  cas 11: MAJO_STR_TR_11 = B;
  cas 12: MAJO_STR_TR_12 = B;
  cas 13: MAJO_STR_TR_13 = B;
  cas 14: MAJO_STR_TR_14 = B;
  cas 15: MAJO_STR_TR_15 = B;
  cas 16: MAJO_STR_TR_16 = B;
  cas 17: MAJO_STR_TR_17 = B;
  cas 18: MAJO_STR_TR_18 = B;
  cas 19: MAJO_STR_TR_19 = B;
  cas 20: MAJO_STR_TR_20 = B;
  cas 21: MAJO_STR_TR_21 = B;
  cas 22: MAJO_STR_TR_22 = B;
  cas 23: MAJO_STR_TR_23 = B;
  cas 24: MAJO_STR_TR_24 = B;
  cas 25: MAJO_STR_TR_25 = B;
)

cible set_majo_str_tr:
application: iliad;
arguments: I;
variables_temporaires: B;
B = 1;
calculer cible init_majo_str_tr : avec I, B;

cible set_majo_str_tr_proc:
application: iliad;
variables_temporaires: I;
I = TMP_ARG1;
calculer cible set_majo_str_tr : avec I;

cible unset_majo_str_tr:
application: iliad;
arguments: I;
variables_temporaires: B;
B = 0;
calculer cible init_majo_str_tr : avec I, B;

cible unset_majo_str_tr_proc:
application: iliad;
variables_temporaires: I;
I = TMP_ARG1;
calculer cible unset_majo_str_tr : avec I;

cible clear_majo_str_tr:
application: iliad;
iterer : variable I : entre 0..25 increment 1 : dans (
  calculer cible unset_majo_str_tr : avec I;
)

cible affect_str_tr:
application: iliad;
CORR.STR_TR00 = MAJO_STR_TR_00;
CORR.STR_TR01 = MAJO_STR_TR_01;
CORR.STR_TR02 = MAJO_STR_TR_02;
CORR.STR_TR03 = MAJO_STR_TR_03;
CORR.STR_TR04 = MAJO_STR_TR_04;
CORR.STR_TR05 = MAJO_STR_TR_05;
CORR.STR_TR06 = MAJO_STR_TR_06;
CORR.STR_TR07 = MAJO_STR_TR_07;
CORR.STR_TR08 = MAJO_STR_TR_08;
CORR.STR_TR09 = MAJO_STR_TR_09;
CORR.STR_TR10 = MAJO_STR_TR_10;
CORR.STR_TR11 = MAJO_STR_TR_11;
CORR.STR_TR12 = MAJO_STR_TR_12;
CORR.STR_TR13 = MAJO_STR_TR_13;
CORR.STR_TR14 = MAJO_STR_TR_14;
CORR.STR_TR15 = MAJO_STR_TR_15;
CORR.STR_TR16 = MAJO_STR_TR_16;
CORR.STR_TR17 = MAJO_STR_TR_17;
CORR.STR_TR18 = MAJO_STR_TR_18;
CORR.STR_TR19 = MAJO_STR_TR_19;
CORR.STR_TR20 = MAJO_STR_TR_20;
CORR.STR_TR21 = MAJO_STR_TR_21;
CORR.STR_TR22 = MAJO_STR_TR_22;
CORR.STR_TR23 = MAJO_STR_TR_23;
CORR.STR_TR24 = MAJO_STR_TR_24;
CORR.STR_TR25 = MAJO_STR_TR_25;

cible enchaine_calcul_prim:
application: iliad;
si GLOBAL.V_IND_TRAIT = 4 alors
  calculer cible effacer_base_etc : espace GLOBAL;
  calculer cible traite_double_liquidation_2_prim;
  calculer cible sauve_base_initial_prim;
  calculer cible sauve_base_1728_prim;
  calculer cible sauve_base_anterieure_prim;
  calculer cible sauve_base_anterieure_cor_prim;
  calculer cible sauve_base_inr_inter22_prim;
sinon
  GLOBAL.V_ACO_MTAP = 0;
  GLOBAL.V_NEGACO = 0;
  calculer cible traite_double_liquidation_2_prim;
finsi

cible enchaine_calcul_corr:
application: iliad;
si CORR.V_IND_TRAIT = 1 alors
  calculer cible effacer_base_etc : espace CORR;
  calculer cible traite_double_liquidation_2_corr;
  calculer cible sauve_base_initial_corr;
  calculer cible sauve_base_1728_corr;
  calculer cible sauve_base_anterieure_corr;
  calculer cible sauve_base_anterieure_cor_corr;
  calculer cible sauve_base_inr_inter22_corr;
sinon
  CORR.V_ACO_MTAP = 0;
  CORR.V_NEGACO = 0;
  calculer cible traite_double_liquidation_2_corr;
finsi

cible enchaine_verification_prim:
application: iliad;
si GLOBAL.V_IND_TRAIT = 4 alors
  calculer cible verif_saisie_cohe_primitive_prim;
sinon
  calculer cible verif_saisie_cohe_corrective_prim;
finsi

cible enchaine_verification_corr:
application: iliad;
si CORR.V_IND_TRAIT = 4 alors
  calculer cible verif_saisie_cohe_primitive_corr;
sinon
  calculer cible verif_saisie_cohe_corrective_corr;
finsi

cible reset_saisie_calc:
application: iliad;
variables_temporaires: IND_TRAIT_SAV;
IND_TRAIT_SAV = CORR.V_IND_TRAIT;
iterer
: variable V
: categorie saisie *, calculee
: espace CORR
: dans (
  V = indefini;
)
CORR.V_IND_TRAIT = IND_TRAIT_SAV;

cible reset_codes_rappel:
application: iliad;
CORR.PEN_RAPPEL = indefini;
CORR.MOIS_RAPPEL = indefini;
CORR.ANNEE_RAPPEL = indefini;
CORR.SENS_RAPPEL = indefini;
CORR.MONT_RAPPEL = indefini;
CORR.NUM_RAPPEL = indefini;
CORR.AB20_RAPPEL = indefini;
CORR.EVT_RAPPEL = indefini;
CORR.BTOL_RAPPEL = indefini;
CORR.COD_RAPPEL = indefini;

cible add_majo_T_RABP:
application: iliad;
arguments: PENA, MONTANT;
aiguillage (PENA): (
  cas 7: GLOBAL.MAJO_T_RABP07 = GLOBAL.MAJO_T_RABP07 + MONTANT;
  cas 8: GLOBAL.MAJO_T_RABP08 = GLOBAL.MAJO_T_RABP08 + MONTANT;
  cas 9: GLOBAL.MAJO_T_RABP09 = GLOBAL.MAJO_T_RABP09 + MONTANT;
  cas 10: GLOBAL.MAJO_T_RABP10 = GLOBAL.MAJO_T_RABP10 + MONTANT;
  cas 11: GLOBAL.MAJO_T_RABP11 = GLOBAL.MAJO_T_RABP11 + MONTANT;
  cas 12: GLOBAL.MAJO_T_RABP12 = GLOBAL.MAJO_T_RABP12 + MONTANT;
  cas 17: GLOBAL.MAJO_T_RABP17 = GLOBAL.MAJO_T_RABP17 + MONTANT;
  cas 31: GLOBAL.MAJO_T_RABP31 = GLOBAL.MAJO_T_RABP31 + MONTANT;
)

cible add_majo_T_RABPPS:
application: iliad;
arguments: PENA, MONTANT;
aiguillage (PENA): (
  cas 7: GLOBAL.MAJO_T_RABPPS07 = GLOBAL.MAJO_T_RABPPS07 + MONTANT;
  cas 8: GLOBAL.MAJO_T_RABPPS08 = GLOBAL.MAJO_T_RABPPS08 + MONTANT;
  cas 9: GLOBAL.MAJO_T_RABPPS09 = GLOBAL.MAJO_T_RABPPS09 + MONTANT;
  cas 10: GLOBAL.MAJO_T_RABPPS10 = GLOBAL.MAJO_T_RABPPS10 + MONTANT;
  cas 11: GLOBAL.MAJO_T_RABPPS11 = GLOBAL.MAJO_T_RABPPS11 + MONTANT;
  cas 12: GLOBAL.MAJO_T_RABPPS12 = GLOBAL.MAJO_T_RABPPS12 + MONTANT;
  cas 17: GLOBAL.MAJO_T_RABPPS17 = GLOBAL.MAJO_T_RABPPS17 + MONTANT;
  cas 31: GLOBAL.MAJO_T_RABPPS31 = GLOBAL.MAJO_T_RABPPS31 + MONTANT;
)

cible add_majo_T_RABPCS:
application: iliad;
arguments: PENA, MONTANT;
aiguillage (PENA) : (
  cas 7: GLOBAL.MAJO_T_RABPCS07 = GLOBAL.MAJO_T_RABPCS07 + MONTANT;
  cas 8: GLOBAL.MAJO_T_RABPCS08 = GLOBAL.MAJO_T_RABPCS08 + MONTANT;
  cas 9: GLOBAL.MAJO_T_RABPCS09 = GLOBAL.MAJO_T_RABPCS09 + MONTANT;
  cas 10: GLOBAL.MAJO_T_RABPCS10 = GLOBAL.MAJO_T_RABPCS10 + MONTANT;
  cas 11: GLOBAL.MAJO_T_RABPCS11 = GLOBAL.MAJO_T_RABPCS11 + MONTANT;
  cas 12: GLOBAL.MAJO_T_RABPCS12 = GLOBAL.MAJO_T_RABPCS12 + MONTANT;
  cas 17: GLOBAL.MAJO_T_RABPCS17 = GLOBAL.MAJO_T_RABPCS17 + MONTANT;
  cas 31: GLOBAL.MAJO_T_RABPCS31 = GLOBAL.MAJO_T_RABPCS31 + MONTANT;
)

cible add_majo_T_RABPRD:
application: iliad;
arguments: PENA, MONTANT;
aiguillage (PENA) : (
  cas 7: GLOBAL.MAJO_T_RABPRD07 = GLOBAL.MAJO_T_RABPRD07 + MONTANT;
  cas 8: GLOBAL.MAJO_T_RABPRD08 = GLOBAL.MAJO_T_RABPRD08 + MONTANT;
  cas 9: GLOBAL.MAJO_T_RABPRD09 = GLOBAL.MAJO_T_RABPRD09 + MONTANT;
  cas 10: GLOBAL.MAJO_T_RABPRD10 = GLOBAL.MAJO_T_RABPRD10 + MONTANT;
  cas 11: GLOBAL.MAJO_T_RABPRD11 = GLOBAL.MAJO_T_RABPRD11 + MONTANT;
  cas 12: GLOBAL.MAJO_T_RABPRD12 = GLOBAL.MAJO_T_RABPRD12 + MONTANT;
  cas 17: GLOBAL.MAJO_T_RABPRD17 = GLOBAL.MAJO_T_RABPRD17 + MONTANT;
  cas 31: GLOBAL.MAJO_T_RABPRD31 = GLOBAL.MAJO_T_RABPRD31 + MONTANT;
)

cible add_majo_T_RABPCH:
application: iliad;
arguments: PENA, MONTANT;
aiguillage (PENA) : (
  cas 7: GLOBAL.MAJO_T_RABPCH07 = GLOBAL.MAJO_T_RABPCH07 + MONTANT;
  cas 8: GLOBAL.MAJO_T_RABPCH08 = GLOBAL.MAJO_T_RABPCH08 + MONTANT;
  cas 9: GLOBAL.MAJO_T_RABPCH09 = GLOBAL.MAJO_T_RABPCH09 + MONTANT;
  cas 10: GLOBAL.MAJO_T_RABPCH10 = GLOBAL.MAJO_T_RABPCH10 + MONTANT;
  cas 11: GLOBAL.MAJO_T_RABPCH11 = GLOBAL.MAJO_T_RABPCH11 + MONTANT;
  cas 12: GLOBAL.MAJO_T_RABPCH12 = GLOBAL.MAJO_T_RABPCH12 + MONTANT;
  cas 17: GLOBAL.MAJO_T_RABPCH17 = GLOBAL.MAJO_T_RABPCH17 + MONTANT;
  cas 31: GLOBAL.MAJO_T_RABPCH31 = GLOBAL.MAJO_T_RABPCH31 + MONTANT;
)
cible add_majo_T_RABPLO:
application: iliad;
arguments: PENA, MONTANT;
aiguillage (PENA) : (
  cas 7: GLOBAL.MAJO_T_RABPLO07 = GLOBAL.MAJO_T_RABPLO07 + MONTANT;
  cas 8: GLOBAL.MAJO_T_RABPLO08 = GLOBAL.MAJO_T_RABPLO08 + MONTANT;
  cas 9: GLOBAL.MAJO_T_RABPLO09 = GLOBAL.MAJO_T_RABPLO09 + MONTANT;
  cas 10: GLOBAL.MAJO_T_RABPLO10 = GLOBAL.MAJO_T_RABPLO10 + MONTANT;
  cas 11: GLOBAL.MAJO_T_RABPLO11 = GLOBAL.MAJO_T_RABPLO11 + MONTANT;
  cas 12: GLOBAL.MAJO_T_RABPLO12 = GLOBAL.MAJO_T_RABPLO12 + MONTANT;
  cas 17: GLOBAL.MAJO_T_RABPLO17 = GLOBAL.MAJO_T_RABPLO17 + MONTANT;
  cas 31: GLOBAL.MAJO_T_RABPLO31 = GLOBAL.MAJO_T_RABPLO31 + MONTANT;
)

cible contexte_2042_INR:
application: iliad;
CORR.SOMMEBAND_2 = GLOBAL.INR_SOMMEBAND_2;
CORR.SOMMEBA_2 = GLOBAL.INR_SOMMEBA_2;
CORR.SOMMEBICND_2 = GLOBAL.INR_SOMMEBICND_2;
CORR.SOMMEBIC_2 = GLOBAL.INR_SOMMEBIC_2;
CORR.SOMMEBNCND_2 = GLOBAL.INR_SOMMEBNCND_2;
CORR.SOMMEBNC_2 = GLOBAL.INR_SOMMEBNC_2;
CORR.SOMMEGLOBAL_2 = GLOBAL.INR_SOMMEGLOBAL_2;
CORR.SOMMEGLOBND_2 = GLOBAL.INR_SOMMEGLOBND_2;
CORR.SOMMELOC_2 = GLOBAL.INR_SOMMELOC_2;
CORR.SOMMEMOND_2 = GLOBAL.INR_SOMMEMOND_2;
CORR.SOMMERCM_2 = GLOBAL.INR_SOMMERCM_2;
CORR.SOMMERF_2 = GLOBAL.INR_SOMMERF_2;
CORR.SOMMERI_2 = GLOBAL.INR_SOMMERI_2;

cible contexte_2042_TL_Ref:
application: iliad;
CORR.SOMMEBAND_2 = 0;
CORR.SOMMEBA_2 = 0;
CORR.SOMMEBICND_2 = 0;
CORR.SOMMEBIC_2 = 0;
CORR.SOMMEBNCND_2 = 0;
CORR.SOMMEBNC_2 = 0;
CORR.SOMMEGLOBAL_2 = 0;
CORR.SOMMEGLOBND_2 = 0;
CORR.SOMMELOC_2 = 0;
CORR.SOMMEMOND_2 = 0;
CORR.SOMMERCM_2 = 0;
CORR.SOMMERF_2 = 0;
CORR.SOMMERI_2 = 0;

cible affect_code:
application: iliad;
variables_temporaires: PENA, I;
  si RETARD > 0 alors
    PENA = GLOBAL.CODE_PENA;
  sinon
    PENA = GLOBAL.MAJO_CODE_STRATE;
  finsi
  aiguillage (PENA) : (
    cas 1 :
      I = 25;
      calculer cible set_majo_str_tr : avec I;
    cas 2 :
      I = 23;
      calculer cible set_majo_str_tr : avec I;
    cas 3 :
      I = 14;
      calculer cible set_majo_str_tr : avec I;
    cas 4 :
      I = 9;
      calculer cible set_majo_str_tr : avec I;
    cas 5 :
      I = 7;
      calculer cible set_majo_str_tr : avec I;
    cas 6 :
      I = 3;
      calculer cible set_majo_str_tr : avec I;
    cas 7 : cas 18 :
      I = 17;
      calculer cible set_majo_str_tr : avec I;
    cas 8 :
      I = 12;
      calculer cible set_majo_str_tr : avec I;
    cas 10 :
      I = 16;
      calculer cible set_majo_str_tr : avec I;
    cas 11 :
      I = 11;
      calculer cible set_majo_str_tr : avec I;
    cas 17 :
      I = 15;
      calculer cible set_majo_str_tr : avec I;
    cas 22 :
      I = 24;
      calculer cible set_majo_str_tr : avec I;
    cas 30 :
      I = 10;
      calculer cible set_majo_str_tr : avec I;
    cas 31 :
      I = 6;
      calculer cible set_majo_str_tr : avec I;
    cas 32 :
      I = 5;
      calculer cible set_majo_str_tr : avec I;
    cas 35 :
      I = 9;
      calculer cible set_majo_str_tr : avec I;
    cas 55 :
      I = 13;
      calculer cible set_majo_str_tr : avec I;
    cas 99:
      I = 0;
      calculer cible set_majo_str_tr : avec I;
  )

cible remplit_tgv_d2042:
application: iliad;
variables_temporaires: UN_NEG;
UN_NEG = 0;
iterer
: variable VAR
: categorie saisie *
: espace D2042
: dans (
  si present(VAR) et VAR >= 0 alors
    CORR.VAR = VAR;
  sinon_si UN_NEG = 0 alors
    leve_erreur A72207;
    UN_NEG = 1;
  finsi
)

cible remplit_tgv_d2042_rect:
application: iliad;
variables_temporaires: UN_NEG;
UN_NEG = 0;
iterer
: variable VAR
: categorie saisie *
: espace D2042_RECT
: dans (
  si present(VAR) et VAR >= 0 alors
    CORR.VAR = VAR;
  sinon_si UN_NEG = 0 alors
    leve_erreur A72207;
    UN_NEG = 1;
  finsi
)

cible remplit_tgv_inr_d2042:
application: iliad;
variables_temporaires: UN_NEG;
UN_NEG = 0;
iterer
: variable VAR
: categorie saisie *
: espace INR_D2042
: dans (
  si present(VAR) et VAR >= 0 alors
    CORR.VAR = VAR;
  sinon_si UN_NEG = 0 alors
    leve_erreur A72207;
    UN_NEG = 1;
  finsi
)

cible remplit_tgv_majo_d2042_p:
application: iliad;
variables_temporaires: UN_NEG;
UN_NEG = 0;
iterer
: variable VAR
: categorie saisie *
: espace MAJO_D2042_P
: dans (
  si present(VAR) et VAR >= 0 alors
    CORR.VAR = VAR;
  sinon_si UN_NEG = 0 alors
    leve_erreur A72207;
    UN_NEG = 1;
  finsi
)

cible remplit_tgv_majo_d2042_strate:
application: iliad;
variables_temporaires: UN_NEG;
UN_NEG = 0;
iterer
: variable VAR
: categorie saisie *
: espace MAJO_D2042_STRATE
: dans (
  si present(VAR) et VAR >= 0 alors
    CORR.VAR = VAR;
  sinon_si UN_NEG = 0 alors
    leve_erreur A72207;
    UN_NEG = 1;
  finsi
)

cible remplit_tgv_tl_d2042:
application: iliad;
variables_temporaires: UN_NEG;
UN_NEG = 0;
iterer
: variable VAR
: categorie saisie *
: espace TL_D2042
: dans (
  si present(VAR) et VAR >= 0 alors
    CORR.VAR = VAR;
  sinon_si UN_NEG = 0 alors
    leve_erreur A72207;
    UN_NEG = 1;
  finsi
)

cible remplit_tgv_tl_d2042_init:
application: iliad;
variables_temporaires: UN_NEG;
UN_NEG = 0;
iterer
: variable VAR
: categorie saisie *
: espace TL_D2042_INIT
: dans (
  si present(VAR) et VAR >= 0 alors
    CORR.VAR = VAR;
  sinon_si UN_NEG = 0 alors
    leve_erreur A72207;
    UN_NEG = 1;
  finsi
)

cible remplit_tgv_tl_d2042_rect:
application: iliad;
variables_temporaires: UN_NEG;
UN_NEG = 0;
iterer
: variable VAR
: categorie saisie *
: espace TL_D2042_RECT
: dans (
  si present(VAR) et VAR >= 0 alors
    CORR.VAR = VAR;
  sinon_si UN_NEG = 0 alors
    leve_erreur A72207;
    UN_NEG = 1;
  finsi
)

cible set_rappel_ifi_prim:
application: iliad;
si
  GLOBAL.COD9AA != 0 ou GLOBAL.COD9AB != 0 ou GLOBAL.COD9AC != 0
  ou GLOBAL.COD9AD != 0 ou GLOBAL.COD9AE != 0 ou GLOBAL.COD9BA != 0
  ou GLOBAL.COD9BB != 0 ou GLOBAL.COD9CA != 0 ou GLOBAL.COD9GF != 0
  ou GLOBAL.COD9GH != 0 ou GLOBAL.COD9GL != 0 ou GLOBAL.COD9GM != 0
  ou GLOBAL.COD9GN != 0 ou GLOBAL.COD9NC != 0 ou GLOBAL.COD9NG != 0
  ou GLOBAL.COD9PR != 0 ou GLOBAL.COD9PX != 0 ou GLOBAL.COD9RS != 0
  ou GLOBAL.CMAJ_ISF != 0 # 9XT
  ou GLOBAL.MOISAN_ISF != 0 # 9XU
alors
  GLOBAL.ISF_PRIM = 1;
finsi

cible calcul_1731:
application: iliad;
calculer cible range_base_corr_corrige;
CORR.VARR30R32 = GLOBAL.ART1731_SOMME_R3032;
CORR.VARR10 = GLOBAL.ART1731_PRESENT_R10;
CORR.VARR30 = GLOBAL.ART1731_PRESENT_R30;
CORR.VARR32 = GLOBAL.ART1731_PRESENT_R32;

cible init_1731:
application: iliad;
CORR.VARR30R32 = 0;
CORR.FLAG_INR_REF = 0;
CORR.PREM8_11 = GLOBAL.PREM_8_11;
CORR.PENA994 = GLOBAL.PENA_994;

cible empty_art1731:
application: iliad;
GLOBAL.ART1731_SOMME_R3032 = 0;
GLOBAL.ART1731_PRESENT_R10 = 0;
GLOBAL.ART1731_PRESENT_R30 = 0;
GLOBAL.ART1731_PRESENT_R32 = 0;

fonction vers_nature:
application: iliad;
arguments: ATTR;
resultat: NAT;
si ATTR = 0 alors
  NAT = N_REVENU;
sinon_si ATTR = 1 alors
  NAT = N_CHARGE;
sinon
  NAT = N_INDEFINIE;
finsi

cible get_nature:
application: iliad;
arguments: NATURE, VAR;
NATURE = 0;
si
  dans_domaine(VAR, saisie famille)
  ou dans_domaine(VAR, saisie revenu)
  ou dans_domaine(VAR, saisie revenu corrective)
alors
  si dans_domaine(VAR, saisie contexte) alors
    NATURE = vers_nature(attribut(VAR, modcat));
  sinon_si
    dans_domaine(VAR, saisie famille)
    ou dans_domaine(VAR, saisie revenu)
    ou dans_domaine(VAR, saisie revenu corrective)
  alors
    NATURE = vers_nature(attribut(VAR, nat_code));
  sinon_si dans_domaine(VAR, calculee *) alors
    NATURE = vers_nature(attribut(VAR, nat_code));
  sinon
    NATURE = N_INDEFINIE;
  finsi
sinon_si dans_domaine(VAR, saisie contexte) alors
  si meme_variable(VAR, V_REGCO) alors
    NATURE = N_REVENU;
  sinon_si 
    meme_variable(VAR, V_EAG)
    ou meme_variable(VAR, V_EAD)
    ou meme_variable(VAR, V_CNR)
    ou meme_variable(VAR, V_CNR2)
    ou meme_variable(VAR, V_CR2)
  alors
    NATURE = N_CHARGE;
  sinon
    NATURE = N_REVENU;
  finsi
sinon_si
  dans_domaine(VAR, saisie variation)
  ou dans_domaine(VAR, saisie penalite)
  ou dans_domaine(VAR, calculee *)
alors
  NATURE = N_REVENU;
finsi

cible get_abat:
application: iliad;
arguments: ABAT, VAR;
si
  dans_domaine(VAR, saisie revenu)
  ou dans_domaine(VAR, saisie revenu corrective)
alors
  ABAT = attribut(VAR, ind_abat);
sinon
  ABAT = 0;
finsi

cible calcul_cum_p:
application: iliad;
variables_temporaires: NATURE;
iterer
: variable VAR
: categorie saisie *
: espace MAJO_D2042_P
: avec
  present(VAR)
  et non (
    meme_variable(VAR, DAT) ou meme_variable(VAR, ANTCR)
    ou meme_variable(VAR, ANTIR) ou meme_variable(VAR, ANTRE)
    ou meme_variable(VAR, CSANT) ou meme_variable(VAR, FORCA)
    ou meme_variable(VAR, FORPA) ou meme_variable(VAR, FORVA)
    ou meme_variable(VAR, IDANT) ou meme_variable(VAR, NIMPA)
    ou meme_variable(VAR, NOTRAIT) ou meme_variable(VAR, PSANT)
    ou meme_variable(VAR, RDANT) ou meme_variable(VAR, ANC_BAR)
    ou meme_variable(VAR, ACO_MTAP) ou meme_variable(VAR, INDG)
    ou meme_variable(VAR, CHRANT) ou meme_variable(VAR, CODILIAD)
  )
: dans (
  calculer cible get_nature : avec NATURE, VAR;
  si NATURE = N_REVENU alors
    si
      attribut(VAR, cotsoc) = 1
      et attribut(VAR, categorie_TL) dans (20, 21)
    alors
      GLOBAL.MAJO_T_RABP = GLOBAL.MAJO_T_RABP + VAR;
      GLOBAL.MAJO_T_RABPPS = GLOBAL.MAJO_T_RABPPS + VAR;
      GLOBAL.MAJO_T_RABPCS = GLOBAL.MAJO_T_RABPCS + VAR;
      GLOBAL.MAJO_T_RABPRD = GLOBAL.MAJO_T_RABPRD + VAR;
      GLOBAL.MAJO_T_RABPCH = GLOBAL.MAJO_T_RABPCH + VAR;
    sinon_si
      attribut(VAR, cotsoc) = 5
      et attribut(VAR, categorie_TL) dans (20, 21)
    alors
      GLOBAL.MAJO_T_RABP = GLOBAL.MAJO_T_RABP + VAR;
      GLOBAL.MAJO_T_RABPCH = GLOBAL.MAJO_T_RABPCH + VAR;
    sinon_si attribut(VAR, cotsoc) = 9 alors
      GLOBAL.MAJO_T_RABPCH = GLOBAL.MAJO_T_RABPCH + VAR;
    sinon_si attribut(VAR, cotsoc) = 10 alors
      GLOBAL.MAJO_T_RABPPS = GLOBAL.MAJO_T_RABPPS + VAR;
      GLOBAL.MAJO_T_RABPCS = GLOBAL.MAJO_T_RABPCS + VAR;
      GLOBAL.MAJO_T_RABPRD = GLOBAL.MAJO_T_RABPRD + VAR;
    sinon_si
      attribut(VAR, cotsoc) dans (11, 12, 13, 14, 19, 20, 21)
    alors
      GLOBAL.MAJO_T_RABPRD = GLOBAL.MAJO_T_RABPRD + VAR;
    sinon_si attribut(VAR, cotsoc) = 16 alors
      GLOBAL.MAJO_T_RABPLO = GLOBAL.MAJO_T_RABPLO + VAR;
    sinon_si
      attribut(VAR, cotsoc) = 18
      et attribut(VAR, categorie_TL) dans (20, 21)
    alors
      GLOBAL.MAJO_T_RABP = GLOBAL.MAJO_T_RABP + VAR;
      GLOBAL.MAJO_T_RABPRD = GLOBAL.MAJO_T_RABPRD + VAR;
      GLOBAL.MAJO_T_RABPCH = GLOBAL.MAJO_T_RABPCH + VAR;
    finsi
  finsi
)

cible process_penalite:
application: iliad;
arguments: PENA_CODE, P;
variables_temporaires: P_MAX, P_OLD, P_MULTI;
P_MAX = inf(PENA_CODE / 1000) % 100;
P_OLD = inf(PENA_CODE / 10) % 100;
P_MULTI = inf(PENA_CODE) % 10;
si P >= P_MAX alors
  P_MAX = P;
finsi
si P != P_OLD alors
  P_MULTI = si (P_OLD != 0 ou positif(P_MULTI)) alors (1) sinon (0) finsi;
  P_OLD = P;
finsi
PENA_CODE = (1000 * P_MAX) + (10 * P_OLD) + P_MULTI;

fonction get_pMax:
application: iliad;
arguments: CODE;
resultat: R;
R = inf(CODE / 1000);

fonction get_pMulti:
application: iliad;
arguments: CODE;
resultat: R;
R = positif(inf(CODE) % 10);

cible detecte_penalites:
application: iliad;
variables_temporaires:
  PENA_CODE_I, PENA_CODE_1728, PENA_CODE_ICRP, PENA_CODE_1728CRP,
  PENA_CODE_C, PENA_CODE_R, PENA_CODE_P, PENA_CODE_ITAXA,
  PENA_CODE_1758AIR, PENA_CODE_1758ATA, PENA_CODE_ICSAL, PENA_CODE_1728CSAL,
  PENA_CODE_CSAL, PENA_CODE_ICDIS, PENA_CODE_1728CDIS, PENA_CODE_CDIS,
  PENA_CODE_ICAP, PENA_CODE_1728CAP, PENA_CODE_CAP, PENA_CODE_1758ACAP,
  PENA_CODE_ICHR, PENA_CODE_1728CHR, PENA_CODE_CHR, PENA_CODE_1758ACHR,
  PENA_CODE_IISF, PENA_CODE_1728ISF, PENA_CODE_ISF, PENA_CODE_IGAIN,
  PENA_CODE_1728GAIN, PENA_CODE_GAIN, PENA_CODE_IRSE1, PENA_CODE_1728RSE1,
  PENA_CODE_RSE1, PENA_CODE_IRSE2, PENA_CODE_1728RSE2, PENA_CODE_RSE2,
  PENA_CODE_IRSE3, PENA_CODE_1728RSE3, PENA_CODE_RSE3, PENA_CODE_IRSE4,
  PENA_CODE_1728RSE4, PENA_CODE_RSE4, PENA_CODE_ICVN, PENA_CODE_1728CVN,
  PENA_CODE_CVN, PENA_CODE_IGLO, PENA_CODE_1728GLO, PENA_CODE_GLO,
  PENA_CODE_IRSE5, PENA_CODE_1728RSE5, PENA_CODE_RSE5, PENA_CODE_IRSE6,
  PENA_CODE_1728RSE6, PENA_CODE_RSE6, PENA_CODE_IRSE7, PENA_CODE_1728RSE7,
  PENA_CODE_RSE7, PENA_CODE_IC820, PENA_CODE_1728C820, PENA_CODE_C820,
  PENA_CODE_IRSE8, PENA_CODE_1728RSE8, PENA_CODE_RSE8, PENA_CODE_REGV,
  CS, SENS, PENA, P;
PENA_CODE_I = 0;
PENA_CODE_1728 = 0;
PENA_CODE_ICRP = 0;
PENA_CODE_1728CRP = 0;
PENA_CODE_C = 0;
PENA_CODE_R = 0;
PENA_CODE_P = 0;
PENA_CODE_ITAXA = 0;
PENA_CODE_1758AIR = 0;
PENA_CODE_1758ATA = 0;
PENA_CODE_ICSAL = 0;
PENA_CODE_1728CSAL = 0;
PENA_CODE_CSAL = 0;
PENA_CODE_ICDIS = 0;
PENA_CODE_1728CDIS = 0;
PENA_CODE_CDIS = 0;
PENA_CODE_ICAP = 0;
PENA_CODE_1728CAP = 0;
PENA_CODE_CAP = 0;
PENA_CODE_1758ACAP = 0;
PENA_CODE_ICHR = 0;
PENA_CODE_1728CHR = 0;
PENA_CODE_CHR = 0;
PENA_CODE_1758ACHR = 0;
PENA_CODE_IISF = 0;
PENA_CODE_1728ISF = 0;
PENA_CODE_ISF = 0;
PENA_CODE_IGAIN = 0;
PENA_CODE_1728GAIN = 0;
PENA_CODE_GAIN = 0;
PENA_CODE_IRSE1 = 0;
PENA_CODE_1728RSE1 = 0;
PENA_CODE_RSE1 = 0;
PENA_CODE_IRSE2 = 0;
PENA_CODE_1728RSE2 = 0;
PENA_CODE_RSE2 = 0;
PENA_CODE_IRSE3 = 0;
PENA_CODE_1728RSE3 = 0;
PENA_CODE_RSE3 = 0;
PENA_CODE_IRSE4 = 0;
PENA_CODE_1728RSE4 = 0;
PENA_CODE_RSE4 = 0;
PENA_CODE_ICVN = 0;
PENA_CODE_1728CVN = 0;
PENA_CODE_CVN = 0;
PENA_CODE_IGLO = 0;
PENA_CODE_1728GLO = 0;
PENA_CODE_GLO = 0;
PENA_CODE_IRSE5 = 0;
PENA_CODE_1728RSE5 = 0;
PENA_CODE_RSE5 = 0;
PENA_CODE_IRSE6 = 0;
PENA_CODE_1728RSE6 = 0;
PENA_CODE_RSE6 = 0;
PENA_CODE_IRSE7 = 0;
PENA_CODE_1728RSE7 = 0;
PENA_CODE_RSE7 = 0;
PENA_CODE_IC820 = 0;
PENA_CODE_1728C820 = 0;
PENA_CODE_C820 = 0;
PENA_CODE_IRSE8 = 0;
PENA_CODE_1728RSE8 = 0;
PENA_CODE_RSE8 = 0;
PENA_CODE_REGV = 0;
iterer
: variable R
: entre 0 .. (nb_evenements() - 1) increment 1
: dans (
  CS = inf(attribut(champ_evenement(R, code), cotsoc));
  SENS = inf(champ_evenement(R, sens));
  PENA = inf(champ_evenement(R, penalite));
  si (GLOBAL.DEFAUT ou GLOBAL.RETARD) et SENS = SENS_R et PENA = 99
  alors
    P = GLOBAL.CODE_PENA;
  sinon
    P = PENA;
  finsi
  si non (P dans (0, 1, 99)) alors
    si P < 7 ou P dans (22, 24, 30, 32, 35, 55) alors
      calculer cible process_penalite : avec PENA_CODE_I, P;
    finsi
    si non (P < 7 ou P dans (22, 24, 30, 32, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728, P; 
    finsi
    si 
      CS dans (1, 6, 7, 10, 11, 12, 13, 14, 18, 19, 20, 21)
      et (P < 7 ou P dans (22, 24, 30, 32, 35, 55))
    alors
      calculer cible process_penalite : avec PENA_CODE_ICRP, P; 
    finsi
    si 
      CS dans (1, 6, 7, 10, 11, 12, 13, 14, 18, 19, 20, 21, 22, 23)
      et non (P < 7 ou P dans (22, 24, 30, 32, 35, 55))
    alors
      calculer cible process_penalite : avec PENA_CODE_1728CRP, P; 
    finsi
    si CS dans (17, 18) et (P < 7 ou P dans (22, 24, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_ICVN, P; 
    finsi
    si CS dans (17, 18) et non (P < 7 ou P dans (22, 24, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728CVN, P; 
    finsi
    si CS = 18 et (P < 7 ou P dans (22, 24, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_IGLO, P;
    finsi
    si CS = 18 et non (P < 7 ou P dans (22, 24, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728GLO, P;
    finsi
    si CS dans (19, 20) et (P < 7 ou P dans (22, 24, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_IRSE5, P;
    finsi
    si CS dans (19, 20) et non (P < 7 ou P dans (22, 24, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728RSE5, P;
    finsi
    si CS = 3 alors
      calculer cible process_penalite : avec PENA_CODE_ITAXA, P;
    finsi
    si P dans (2, 10, 17) alors
      calculer cible process_penalite : avec PENA_CODE_1758AIR, P;
    finsi
    si  CS = 3 et P dans (2, 10, 17) alors
      calculer cible process_penalite : avec PENA_CODE_1758ATA, P;
    finsi
    si CS = 4 et (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_ICSAL, P;
    finsi
    si CS = 4 et non (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728CSAL, P;
    finsi
    si CS = 8 alors
      calculer cible process_penalite : avec PENA_CODE_ICHR, P;
    finsi
    si CS = 8 et (P dans (2, 10, 17)) alors
      calculer cible process_penalite : avec PENA_CODE_1758ACHR, P;
    finsi
    si CS = 8 et (P < 7 ou P dans (22, 24, 30, 32, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_ICHR, P;
    finsi
    si CS = 8 et non (P < 7 ou P dans (22, 24, 30, 32, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728CHR, P;
    finsi
    si CS = 9 alors
      calculer cible process_penalite : avec PENA_CODE_ICAP, P;
    finsi
    si CS = 9 et (P dans (2, 10, 17)) alors
      calculer cible process_penalite : avec PENA_CODE_1758ACAP, P;
    finsi
    si CS = 9 et (P < 7 ou P dans (22, 24, 35, 55 )) alors
      calculer cible process_penalite : avec PENA_CODE_ICAP, P;
    finsi
    si CS = 9 et non (P < 7 ou P dans (22, 24, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728CAP, P;
    finsi
    si CS = 2 alors
      calculer cible process_penalite : avec PENA_CODE_ICDIS, P;
    finsi
    si CS = 2 et (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_ICDIS, P;
    finsi
    si CS = 2 et non (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728CDIS, P;
    finsi
    si CS = 15 et (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_IGAIN, P;
    finsi
    si CS = 15 et non (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728GAIN, P;
    finsi
    si CS = 11 et (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_IRSE1, P;
    finsi
    si CS = 11 et non (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728RSE1, P;
    finsi
    si CS dans (12, 21) et (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_IRSE2, P;
    finsi
    si CS dans (12, 21) et non (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728RSE2, P;
    finsi
    si CS = 13 et (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_IRSE3, P;
    finsi
    si CS = 13 et non (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728RSE3, P;
    finsi
    si CS = 14 et (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_IRSE4, P;
    finsi
    si CS = 14 et non (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728RSE4, P;
    finsi
    si CS dans (20, 21, 22) et (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_IRSE6, P;
    finsi
    si CS dans (20, 21, 22) et non (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728RSE6, P;
    finsi
    si CS = 23 et (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_IC820, P;
    finsi
    si CS = 23 et non (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728C820, P;
    finsi
    si CS = 22 et (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_IRSE8, P;
    finsi
    si CS = 22 et non (P < 7 ou P dans (22, 24, 30, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728RSE8, P;
    finsi
    si (P < 7 ou P = 35) alors
      calculer cible process_penalite : avec PENA_CODE_IISF, P;
    finsi
    si (P dans (3, 4, 5, 7, 8, 10, 11, 17, 18, 22, 24, 35, 55)) alors
      calculer cible process_penalite : avec PENA_CODE_1728ISF, P;
    finsi
    si CS dans (1, 7, 10, 23) alors
      calculer cible process_penalite : avec PENA_CODE_C, P;
    finsi
    si CS dans (1, 6, 7, 10, 11, 12, 13, 14, 18, 19, 21, 22, 23) alors
      calculer cible process_penalite : avec PENA_CODE_R, P;
    finsi
    si CS dans (1, 10, 23) alors
      calculer cible process_penalite : avec PENA_CODE_P, P;
    finsi
    si CS dans (17, 18) alors
      calculer cible process_penalite : avec PENA_CODE_CVN, P;
    finsi
    si CS = 7 alors
      calculer cible process_penalite : avec PENA_CODE_REGV, P;
    finsi
    si CS = 4 alors
      calculer cible process_penalite : avec PENA_CODE_CSAL, P;
    finsi
    si CS = 2 alors
      calculer cible process_penalite : avec PENA_CODE_CDIS, P;
    finsi
    si CS = 11 alors
      calculer cible process_penalite : avec PENA_CODE_RSE1, P;
    finsi
    si CS dans (12, 21) alors
      calculer cible process_penalite : avec PENA_CODE_RSE2, P;
    finsi
    si CS = 13 alors
      calculer cible process_penalite : avec PENA_CODE_RSE3, P;
    finsi
    si CS = 14 alors
      calculer cible process_penalite : avec PENA_CODE_RSE4, P;
    finsi
    si CS dans (19, 20) alors
      calculer cible process_penalite : avec PENA_CODE_RSE5, P;
    finsi
    si CS dans (20, 21, 22) alors
      calculer cible process_penalite : avec PENA_CODE_RSE6, P;
    finsi
    si CS dans (20, 21, 22) alors
      calculer cible process_penalite : avec PENA_CODE_RSE7, P;
    finsi
    si CS = 23 alors
      calculer cible process_penalite : avec PENA_CODE_C820, P;
    finsi
    si CS = 22 alors
      calculer cible process_penalite : avec PENA_CODE_RSE8, P;
    finsi
    si CS = 15 alors
      calculer cible process_penalite : avec PENA_CODE_GAIN, P;
    finsi
    si CS = 18  alors
      calculer cible process_penalite : avec PENA_CODE_GLO, P;
    finsi
  finsi
)
CORR.V_CODPF1728 = get_pMax(PENA_CODE_1728);
CORR.V_CODPF1728C820 = get_pMax(PENA_CODE_1728C820);
CORR.V_CODPF1728CAP = get_pMax(PENA_CODE_1728CAP);
CORR.V_CODPF1728CDIS = get_pMax(PENA_CODE_1728CDIS);
CORR.V_CODPF1728CHR = get_pMax(PENA_CODE_1728CHR);
CORR.V_CODPF1728CRP = get_pMax(PENA_CODE_1728CRP);
CORR.V_CODPF1728CSAL = get_pMax(PENA_CODE_1728CSAL);
CORR.V_CODPF1728CVN = get_pMax(PENA_CODE_1728CVN);
CORR.V_CODPF1728GAIN = get_pMax(PENA_CODE_1728GAIN);
CORR.V_CODPF1728GLO = get_pMax(PENA_CODE_1728GLO);
CORR.V_CODPF1728ISF = get_pMax(PENA_CODE_1728ISF);
CORR.V_CODPF1728RSE1 = get_pMax(PENA_CODE_1728RSE1);
CORR.V_CODPF1728RSE2 = get_pMax(PENA_CODE_1728RSE2);
CORR.V_CODPF1728RSE3 = get_pMax(PENA_CODE_1728RSE3);
CORR.V_CODPF1728RSE4 = get_pMax(PENA_CODE_1728RSE4);
CORR.V_CODPF1728RSE5 = get_pMax(PENA_CODE_1728RSE5);
CORR.V_CODPF1728RSE6 = get_pMax(PENA_CODE_1728RSE6);
CORR.V_CODPF1728RSE7 = get_pMax(PENA_CODE_1728RSE7);
CORR.V_CODPF1728RSE8 = get_pMax(PENA_CODE_1728RSE8);
CORR.V_CODPFC = get_pMax(PENA_CODE_C);
CORR.V_CODPFC820 = get_pMax(PENA_CODE_C820);
CORR.V_CODPFCAP = get_pMax(PENA_CODE_CAP);
CORR.V_CODPFCDIS = get_pMax(PENA_CODE_CDIS);
CORR.V_CODPFCHR = get_pMax(PENA_CODE_CHR);
CORR.V_CODPFCSAL = get_pMax(PENA_CODE_CSAL);
CORR.V_CODPFCVN = get_pMax(PENA_CODE_CVN);
CORR.V_CODPFGAIN = get_pMax(PENA_CODE_GAIN);
CORR.V_CODPFGLO = get_pMax(PENA_CODE_GLO);
CORR.V_CODPFI = get_pMax(PENA_CODE_I);
CORR.V_CODPFIC820 = get_pMax(PENA_CODE_IC820);
CORR.V_CODPFICAP = get_pMax(PENA_CODE_ICAP);
CORR.V_CODPFICDIS = get_pMax(PENA_CODE_ICDIS);
CORR.V_CODPFICHR = get_pMax(PENA_CODE_ICHR);
CORR.V_CODPFICRP = get_pMax(PENA_CODE_ICRP);
CORR.V_CODPFICSAL = get_pMax(PENA_CODE_ICSAL);
CORR.V_CODPFICVN = get_pMax(PENA_CODE_ICVN);
CORR.V_CODPFIGAIN = get_pMax(PENA_CODE_IGAIN);
CORR.V_CODPFIGLO = get_pMax(PENA_CODE_IGLO);
CORR.V_CODPFIISF = get_pMax(PENA_CODE_IISF);
CORR.V_CODPFIRSE1 = get_pMax(PENA_CODE_IRSE1);
CORR.V_CODPFIRSE2 = get_pMax(PENA_CODE_IRSE2);
CORR.V_CODPFIRSE3 = get_pMax(PENA_CODE_IRSE3);
CORR.V_CODPFIRSE4 = get_pMax(PENA_CODE_IRSE4);
CORR.V_CODPFIRSE5 = get_pMax(PENA_CODE_IRSE5);
CORR.V_CODPFIRSE6 = get_pMax(PENA_CODE_IRSE6);
CORR.V_CODPFIRSE7 = get_pMax(PENA_CODE_IRSE7);
CORR.V_CODPFIRSE8 = get_pMax(PENA_CODE_IRSE8);
CORR.V_CODPFISF = get_pMax(PENA_CODE_ISF);
CORR.V_CODPFITAXA = get_pMax(PENA_CODE_ITAXA);
CORR.V_CODPFP = get_pMax(PENA_CODE_P);
CORR.V_CODPFR = get_pMax(PENA_CODE_R);
CORR.V_CODPFRSE1 = get_pMax(PENA_CODE_RSE1);
CORR.V_CODPFRSE2 = get_pMax(PENA_CODE_RSE2);
CORR.V_CODPFRSE3 = get_pMax(PENA_CODE_RSE3);
CORR.V_CODPFRSE4 = get_pMax(PENA_CODE_RSE4);
CORR.V_CODPFRSE5 = get_pMax(PENA_CODE_RSE5);
CORR.V_CODPFRSE6 = get_pMax(PENA_CODE_RSE6);
CORR.V_CODPFRSE7 = get_pMax(PENA_CODE_RSE7);
CORR.V_CODPFRSE8 = get_pMax(PENA_CODE_RSE8);
CORR.V_NBCOD1728 = get_pMulti(PENA_CODE_1728);
CORR.V_NBCOD1728C820 = get_pMulti(PENA_CODE_1728C820);
CORR.V_NBCOD1728CAP = get_pMulti(PENA_CODE_1728CAP);
CORR.V_NBCOD1728CDIS = get_pMulti(PENA_CODE_1728CDIS);
CORR.V_NBCOD1728CHR = get_pMulti(PENA_CODE_1728CHR);
CORR.V_NBCOD1728CRP = get_pMulti(PENA_CODE_1728CRP);
CORR.V_NBCOD1728CSAL = get_pMulti(PENA_CODE_1728CSAL);
CORR.V_NBCOD1728CVN = get_pMulti(PENA_CODE_1728CVN);
CORR.V_NBCOD1728GAIN = get_pMulti(PENA_CODE_1728GAIN);
CORR.V_NBCOD1728GLO = get_pMulti(PENA_CODE_1728GLO);
CORR.V_NBCOD1728ISF = get_pMulti(PENA_CODE_1728ISF);
CORR.V_NBCOD1728RSE1 = get_pMulti(PENA_CODE_1728RSE1);
CORR.V_NBCOD1728RSE2 = get_pMulti(PENA_CODE_1728RSE2);
CORR.V_NBCOD1728RSE3 = get_pMulti(PENA_CODE_1728RSE3);
CORR.V_NBCOD1728RSE4 = get_pMulti(PENA_CODE_1728RSE4);
CORR.V_NBCOD1728RSE5 = get_pMulti(PENA_CODE_1728RSE5);
CORR.V_NBCOD1728RSE6 = get_pMulti(PENA_CODE_1728RSE6);
CORR.V_NBCOD1728RSE7 = get_pMulti(PENA_CODE_1728RSE7);
CORR.V_NBCOD1728RSE8 = get_pMulti(PENA_CODE_1728RSE8);
CORR.V_NBCOD1758ACAP = get_pMulti(PENA_CODE_1758ACAP);
CORR.V_NBCOD1758ACHR = get_pMulti(PENA_CODE_1758ACHR);
CORR.V_NBCOD1758AIR = get_pMulti(PENA_CODE_1758AIR);
CORR.V_NBCOD1758ATA = get_pMulti(PENA_CODE_1758ATA);
CORR.V_NBCODC = get_pMulti(PENA_CODE_C);
CORR.V_NBCODC820 = get_pMulti(PENA_CODE_C820);
CORR.V_NBCODCDIS = get_pMulti(PENA_CODE_CDIS);
CORR.V_NBCODCSAL = get_pMulti(PENA_CODE_CSAL);
CORR.V_NBCODCVN = get_pMulti(PENA_CODE_CVN);
CORR.V_NBCODGAIN = get_pMulti(PENA_CODE_GAIN);
CORR.V_NBCODGLO = get_pMulti(PENA_CODE_GLO);
CORR.V_NBCODI = get_pMulti(PENA_CODE_I);
CORR.V_NBCODIC820 = get_pMulti(PENA_CODE_IC820);
CORR.V_NBCODICAP = get_pMulti(PENA_CODE_ICAP);
CORR.V_NBCODICDIS = get_pMulti(PENA_CODE_ICDIS);
CORR.V_NBCODICHR = get_pMulti(PENA_CODE_ICHR);
CORR.V_NBCODICRP = get_pMulti(PENA_CODE_ICRP);
CORR.V_NBCODICSAL = get_pMulti(PENA_CODE_ICSAL);
CORR.V_NBCODICVN = get_pMulti(PENA_CODE_ICVN);
CORR.V_NBCODIGAIN = get_pMulti(PENA_CODE_IGAIN);
CORR.V_NBCODIGLO = get_pMulti(PENA_CODE_IGLO);
CORR.V_NBCODIISF = get_pMulti(PENA_CODE_IISF);
CORR.V_NBCODIRSE1 = get_pMulti(PENA_CODE_IRSE1);
CORR.V_NBCODIRSE2 = get_pMulti(PENA_CODE_IRSE2);
CORR.V_NBCODIRSE3 = get_pMulti(PENA_CODE_IRSE3);
CORR.V_NBCODIRSE4 = get_pMulti(PENA_CODE_IRSE4);
CORR.V_NBCODIRSE5 = get_pMulti(PENA_CODE_IRSE5);
CORR.V_NBCODIRSE6 = get_pMulti(PENA_CODE_IRSE6);
CORR.V_NBCODIRSE7 = get_pMulti(PENA_CODE_IRSE7);
CORR.V_NBCODIRSE8 = get_pMulti(PENA_CODE_IRSE8);
CORR.V_NBCODISF = get_pMulti(PENA_CODE_ISF);
CORR.V_NBCODITAXA = get_pMulti(PENA_CODE_ITAXA);
CORR.V_NBCODP = get_pMulti(PENA_CODE_P);
CORR.V_NBCODR = get_pMulti(PENA_CODE_R);
CORR.V_NBCODRSE1 = get_pMulti(PENA_CODE_RSE1);
CORR.V_NBCODRSE2 = get_pMulti(PENA_CODE_RSE2);
CORR.V_NBCODRSE3 = get_pMulti(PENA_CODE_RSE3);
CORR.V_NBCODRSE4 = get_pMulti(PENA_CODE_RSE4);
CORR.V_NBCODRSE5 = get_pMulti(PENA_CODE_RSE5);
CORR.V_NBCODRSE6 = get_pMulti(PENA_CODE_RSE6);
CORR.V_NBCODRSE7 = get_pMulti(PENA_CODE_RSE7);
CORR.V_NBCODRSE8 = get_pMulti(PENA_CODE_RSE8);

fonction is_1728_defaut:
application: iliad;
arguments: PENA;
resultat: R;
R = (PENA dans (10, 11, 12));

EST_CODE_ISF : calculee primrest = 0 : "" ;

cible est_code_isf:
application: iliad;
arguments: RESULTAT, VAR;
RESULTAT =
  meme_variable(VAR, COD9AA)
  ou meme_variable(VAR, COD9AB)
  ou meme_variable(VAR, COD9AC)
  ou meme_variable(VAR, COD9AD)
  ou meme_variable(VAR, COD9AE)
  ou meme_variable(VAR, COD9BA)
  ou meme_variable(VAR, COD9BB)
  ou meme_variable(VAR, COD9CA)
  ou meme_variable(VAR, COD9GF)
  ou meme_variable(VAR, COD9GH)
  ou meme_variable(VAR, COD9GL)
  ou meme_variable(VAR, COD9GM)
  ou meme_variable(VAR, COD9GN)
  ou meme_variable(VAR, COD9GY)
  ou meme_variable(VAR, COD9NC)
  ou meme_variable(VAR, COD9NG)
  ou meme_variable(VAR, COD9PR)
  ou meme_variable(VAR, COD9PX)
  ou meme_variable(VAR, COD9RS)
  ou meme_variable(VAR, CMAJ_ISF)
  ou meme_variable(VAR, MOISAN_ISF);

cible est_rappel_1728:
application: iliad;
arguments: RESULTAT, PENA, VAR;
calculer cible est_code_isf : avec RESULTAT, VAR;
si positif(RESULTAT) alors
  RESULTAT = (PENA dans (7, 8, 9, 10, 11, 12, 17, 18, 31, 99));
sinon
  RESULTAT = (PENA dans (2, 3, 7, 8, 9, 10, 11, 12, 17, 18, 22, 31, 99));
finsi

cible controle_defaut_retard_2042:
application: iliad;
variables_temporaires: RAP_1728_SF, PENA, EST_1728, EST_ISF, NUM_EVT;
RAP_1728_SF = indefini;
iterer
: variable R
: entre 0 .. (nb_evenements() - 1) increment 1
: dans (
  PENA = champ_evenement(R, penalite);
  calculer cible est_rappel_1728 : avec EST_1728, PENA, champ_evenement(R, code);
  calculer cible est_code_isf : avec EST_ISF, champ_evenement(R, code);
  si non present(RAP_1728_SF) alors
    si   
      (
        positif(EST_1728)
        et (
          meme_variable(champ_evenement(R, code), V_0AM)
          ou meme_variable(champ_evenement(R, code), V_0AC)
          ou meme_variable(champ_evenement(R, code), V_0AD)
          ou meme_variable(champ_evenement(R, code), V_0AO)
          ou meme_variable(champ_evenement(R, code), V_0AV)
        )
        et non (
          positif(GLOBAL.V_0AM + 0)
          ou positif(GLOBAL.V_0AC + 0)
          ou positif(GLOBAL.V_0AD + 0)
          ou positif(GLOBAL.V_0AO + 0)
          ou positif(GLOBAL.V_0AV + 0)
        )
      )
      ou (positif(EST_ISF) et GLOBAL.ISF_PRIM + 0 = 0)
    alors
      RAP_1728_SF = R;
    finsi
  finsi
)
si present(RAP_1728_SF) alors
  PENA = champ_evenement(RAP_1728_SF, penalite);
  GLOBAL.CODE_PENA = PENA;
  GLOBAL.DATE  = champ_evenement(RAP_1728_SF, date);
  si PENA dans (10, 11, 12) alors
    GLOBAL.DEFAUT = 1;
    GLOBAL.DEFAUT10 = (PENA = 10);
    GLOBAL.DEFAUT11 = (PENA = 11);
    GLOBAL.DEFAUT1011 = (PENA dans (10, 11));
  sinon
    GLOBAL.RETARD = 1;
    GLOBAL.RETARD101718 = (PENA dans (10, 17, 18));
    GLOBAL.RETARD0718 = (PENA dans (7, 8, 17, 18));
    GLOBAL.RETARD08 = (PENA = 8);
    GLOBAL.RETARD07 = (PENA dans (1, 7, 99));
    GLOBAL.RETARD22 = (PENA = 22);
    GLOBAL.RETARD99 = (PENA dans (1, 99));
  finsi
finsi
si positif(GLOBAL.PRESENT_9YT) alors
  GLOBAL.CODE_PENA = GLOBAL.MONTANT_9YT;
  GLOBAL.DATE = GLOBAL.DATE_9YU;
  NUM_EVT = champ_evenement(0, numero);
  si GLOBAL.NUM_EVT_9YT != NUM_EVT et GLOBAL.MONTANT_9YT != 0 alors
    si GLOBAL.CODE_PENA dans (10, 11, 12) alors
      GLOBAL.DEFAUT = 1;
      GLOBAL.DEFAUT10 = (GLOBAL.CODE_PENA = 10 ou positif(GLOBAL.DEFAUT10));
      GLOBAL.DEFAUT11 = (GLOBAL.CODE_PENA = 11 ou positif(GLOBAL.DEFAUT11));
      GLOBAL.RETARD = 0;
      GLOBAL.RETARD08 = 0;
      GLOBAL.RETARD07 = 0;
      GLOBAL.RETARD22 = 0;
      GLOBAL.RETARD99 = 0;
    sinon
      GLOBAL.RETARD = 1;
      GLOBAL.RETARD07 = (GLOBAL.CODE_PENA = 7);
      GLOBAL.RETARD08 = (GLOBAL.CODE_PENA = 8 ou positif(GLOBAL.RETARD08));
      GLOBAL.RETARD22 = (GLOBAL.CODE_PENA = 22 ou positif(GLOBAL.RETARD22));
      GLOBAL.RETARD99 = (GLOBAL.CODE_PENA = 99);
      GLOBAL.DEFAUT = 0;
      GLOBAL.DEFAUT10 = 0;
      GLOBAL.DEFAUT11 = 0;
    finsi
  finsi
finsi
si positif(GLOBAL.PRESENT_9XT) alors
  GLOBAL.CODE_PENA = GLOBAL.MONTANT_9XT;
  GLOBAL.DATE = GLOBAL.DATE_9XU;
  NUM_EVT = champ_evenement(0, numero);
  si GLOBAL.NUM_EVT_9XT != NUM_EVT et GLOBAL.MONTANT_9XT != 0 alors
    si GLOBAL.CODE_PENA dans (10, 11, 12) alors
      GLOBAL.DEFAUT = 1;
      GLOBAL.DEFAUT10 = (GLOBAL.CODE_PENA = 10 ou positif(GLOBAL.DEFAUT10));
      GLOBAL.DEFAUT11 = (GLOBAL.CODE_PENA = 11 ou positif(GLOBAL.DEFAUT11));
      GLOBAL.RETARD = 0;
      GLOBAL.RETARD08 = 0;
      GLOBAL.RETARD07 = 0;
      GLOBAL.RETARD22 = 0;
      GLOBAL.RETARD99 = 0;
    sinon
      GLOBAL.RETARD = 1;
      GLOBAL.RETARD07 = (GLOBAL.CODE_PENA = 7 ou positif(GLOBAL.RETARD07));
      GLOBAL.RETARD08 = (GLOBAL.CODE_PENA = 8 ou positif(GLOBAL.RETARD08));
      GLOBAL.RETARD22 = (GLOBAL.CODE_PENA = 22 ou positif(GLOBAL.RETARD22));
      GLOBAL.RETARD99 = (GLOBAL.CODE_PENA = 99 ou positif(GLOBAL.RETARD99));
      GLOBAL.DEFAUT = 0;
      GLOBAL.DEFAUT10 = 0;
      GLOBAL.DEFAUT11 = 0;
    finsi
  finsi
finsi
CORR.FLAG_RETARD = GLOBAL.RETARD;
CORR.FLAG_DEFAUT = GLOBAL.DEFAUT;

cible prepare_tl:
application: iliad;
variables_temporaires: COMMENCE_PAR_7;
si positif(GLOBAL.DEFAUT) alors
  GLOBAL.TL_NON_ACQUISE = TL_TL_DEFAUT_2042;
  CORR.IND_TL_MF = 1;
sinon
  GLOBAL.TL_D2042_NB = 0;
  GLOBAL.TL_D2042_INIT_NB = 0;
  GLOBAL.TL_D2042_RECT_NB = 0;
  iterer
  : variable VAR
  : categorie saisie *
  : dans (
    si present(D2042.VAR) alors
      TL_D2042.VAR = D2042.VAR;
      GLOBAL.TL_D2042_NB = GLOBAL.TL_D2042_NB + 1;
      TL_D2042_INIT.VAR = D2042.VAR;
      GLOBAL.TL_D2042_INIT_NB = GLOBAL.TL_D2042_INIT_NB + 1;
      TL_D2042_RECT.VAR = D2042.VAR;
      GLOBAL.TL_D2042_RECT_NB = GLOBAL.TL_D2042_RECT_NB + 1;
    sinon
      TL_D2042.VAR = indefini;
      TL_D2042_INIT.VAR = indefini;
      TL_D2042_RECT.VAR = indefini;
    finsi
    si present(D2042_ABAT.VAR) alors
      TL_D2042_ABAT.VAR = D2042_ABAT.VAR;
      TL_D2042_ABAT_INIT.VAR = D2042_ABAT.VAR;
      TL_D2042_ABAT_RECT.VAR = D2042_ABAT.VAR;
    sinon
      TL_D2042_ABAT.VAR = indefini;
      TL_D2042_ABAT_INIT.VAR = indefini;
      TL_D2042_ABAT_RECT.VAR = indefini;
    finsi
  )
  iterer
  : variable VAR
  : categorie saisie *
  : avec present(VAR) et attribut(VAR, categorie_TL) dans (40, 50)
  : espace D2042
  : dans (
    calculer cible alias_commence_par_7 : avec COMMENCE_PAR_7, VAR;
    si positif(COMMENCE_PAR_7) alors
      GLOBAL.TL_BASE_TL_INIT = GLOBAL.TL_BASE_TL_INIT + VAR;
      GLOBAL.TL_BASE_TL_TBTC_INIT = GLOBAL.TL_BASE_TL_TBTC_INIT + VAR;
    finsi
  )
finsi

cible prepare_majo:
application: iliad;
GLOBAL.MAJO_T_RABP = 0;
GLOBAL.MAJO_T_RABP07 = 0;
GLOBAL.MAJO_T_RABP08 = 0;
GLOBAL.MAJO_T_RABP09 = 0;
GLOBAL.MAJO_T_RABP10 = 0;
GLOBAL.MAJO_T_RABP11 = 0;
GLOBAL.MAJO_T_RABP12 = 0;
GLOBAL.MAJO_T_RABP17 = 0;
GLOBAL.MAJO_T_RABP31 = 0;
GLOBAL.MAJO_T_RABPPS = 0;
GLOBAL.MAJO_T_RABPPS07 = 0;
GLOBAL.MAJO_T_RABPPS08 = 0;
GLOBAL.MAJO_T_RABPPS09 = 0;
GLOBAL.MAJO_T_RABPPS10 = 0;
GLOBAL.MAJO_T_RABPPS11 = 0;
GLOBAL.MAJO_T_RABPPS12 = 0;
GLOBAL.MAJO_T_RABPPS17 = 0;
GLOBAL.MAJO_T_RABPPS31 = 0;
GLOBAL.MAJO_T_RABPCS = 0;
GLOBAL.MAJO_T_RABPCS07 = 0;
GLOBAL.MAJO_T_RABPCS08 = 0;
GLOBAL.MAJO_T_RABPCS09 = 0;
GLOBAL.MAJO_T_RABPCS10 = 0;
GLOBAL.MAJO_T_RABPCS11 = 0;
GLOBAL.MAJO_T_RABPCS12 = 0;
GLOBAL.MAJO_T_RABPCS17 = 0;
GLOBAL.MAJO_T_RABPCS31 = 0;
GLOBAL.MAJO_T_RABPRD = 0;
GLOBAL.MAJO_T_RABPRD07 = 0;
GLOBAL.MAJO_T_RABPRD08 = 0;
GLOBAL.MAJO_T_RABPRD09 = 0;
GLOBAL.MAJO_T_RABPRD10 = 0;
GLOBAL.MAJO_T_RABPRD11 = 0;
GLOBAL.MAJO_T_RABPRD12 = 0;
GLOBAL.MAJO_T_RABPRD17 = 0;
GLOBAL.MAJO_T_RABPRD31 = 0;
GLOBAL.MAJO_T_RABPCH = 0;
GLOBAL.MAJO_T_RABPCH07 = 0;
GLOBAL.MAJO_T_RABPCH08 = 0;
GLOBAL.MAJO_T_RABPCH09 = 0;
GLOBAL.MAJO_T_RABPCH10 = 0;
GLOBAL.MAJO_T_RABPCH11 = 0;
GLOBAL.MAJO_T_RABPCH12 = 0;
GLOBAL.MAJO_T_RABPCH17 = 0;
GLOBAL.MAJO_T_RABPCH31 = 0;
GLOBAL.MAJO_T_RABPLO = 0;
GLOBAL.MAJO_T_RABPLO07 = 0;
GLOBAL.MAJO_T_RABPLO08 = 0;
GLOBAL.MAJO_T_RABPLO09 = 0;
GLOBAL.MAJO_T_RABPLO10 = 0;
GLOBAL.MAJO_T_RABPLO11 = 0;
GLOBAL.MAJO_T_RABPLO12 = 0;
GLOBAL.MAJO_T_RABPLO17 = 0;
GLOBAL.MAJO_T_RABPLO31 = 0;

cible prepare_inr:
application: iliad;
iterer
: variable VAR
: categorie saisie *
: espace D2042
: dans (
  INR_D2042.VAR = VAR;
)

fonction taux_penalite:
application: iliad;
arguments: PENA;
resultat: TAUX;
si PENA dans (2, 7, 10, 17) alors
  TAUX = 10;
sinon_si PENA dans (3, 8, 11, 30, 55) alors
  TAUX = 40;
sinon_si PENA dans (4, 5, 9, 12, 31, 32) alors
  TAUX = 80;
sinon_si PENA = 6 alors
  TAUX = 100;
sinon
  TAUX = 0;
finsi

cible calcul_tl:
application: iliad;
arguments: EST_PREMIER;
si GLOBAL.TL_D2042_INIT_NB > 0 alors
  calculer cible reset_saisie_calc;
  calculer cible remplit_tgv_tl_d2042_init;
  si GLOBAL.TL_NON_ACQUISE != TL_TL_ACQUISE alors
    CORR.IND_TL_MF = 1;
  finsi
  calculer cible init_1731;
  calculer cible enchaine_calcul_corr;
  calculer cible sauve_base_tl_init_corr;
finsi
si GLOBAL.TL_D2042_NB > 0 alors
  calculer cible reset_saisie_calc;
  calculer cible remplit_tgv_tl_d2042;
  calculer cible init_1731;
  calculer cible enchaine_calcul_corr;
  calculer cible signaler_erreurs;
  calculer cible sauve_base_tl_corr;
finsi
si GLOBAL.TL_D2042_RECT_NB > 0 alors
  calculer cible reset_saisie_calc;
  calculer cible remplit_tgv_tl_d2042_rect;
  calculer cible init_1731;
  calculer cible enchaine_calcul_corr;
  calculer cible signaler_erreurs;
  calculer cible sauve_base_tl_rect_corr;
finsi
CORR.MFCDIS = GLOBAL.TL_MF_MFCDIS;
CORR.MFCHR = GLOBAL.TL_MF_MFCHR;
CORR.MFCHR7 = GLOBAL.TL_MF_MFCHR7;
CORR.MFCS = GLOBAL.TL_MF_MFCS;
CORR.MFCSAL = GLOBAL.TL_MF_MFCSAL;
CORR.MFCVN = GLOBAL.TL_MF_MFCVN;
CORR.MFGAIN = GLOBAL.TL_MF_MFGAIN;
CORR.MFGLO = GLOBAL.TL_MF_MFGLO;
CORR.MFIFI = GLOBAL.TL_MF_MFIFI;
CORR.MFIR = GLOBAL.TL_MF_MFIR;
CORR.MFMCSG820 = GLOBAL.TL_MF_MFMCSG820;
CORR.MFPCAP = GLOBAL.TL_MF_MFPCAP;
CORR.MFPS = GLOBAL.TL_MF_MFPS;
CORR.MFPSOL = GLOBAL.TL_MF_MFPSOL;
CORR.MFRD = GLOBAL.TL_MF_MFRD;
CORR.MFREGV = GLOBAL.TL_MF_MFREGV;
CORR.MFRSE1 = GLOBAL.TL_MF_MFRSE1;
CORR.MFRSE2 = GLOBAL.TL_MF_MFRSE2;
CORR.MFRSE3 = GLOBAL.TL_MF_MFRSE3;
CORR.MFRSE4 = GLOBAL.TL_MF_MFRSE4;
CORR.MFRSE5 = GLOBAL.TL_MF_MFRSE5;
CORR.MFRSE6 = GLOBAL.TL_MF_MFRSE6;
CORR.MFRSE7 = GLOBAL.TL_MF_MFRSE7;
CORR.MFTAXAGA = GLOBAL.TL_MF_MFTAXAGA;
CORR.RAPCI_TL = GLOBAL.TL_BASE_TL;
CORR.RAPCI_INIT = GLOBAL.TL_BASE_TL_INIT;
CORR.RAPCI_RECT = GLOBAL.TL_BASE_TL_RECT;
CORR.CI_TL = GLOBAL.TL_BASE_TL_TBTC;
CORR.CI_INIT = GLOBAL.TL_BASE_TL_TBTC_INIT;
CORR.CI_RECT = GLOBAL.TL_BASE_TL_TBTC_RECT;
calculer cible ench_tl_corr;

cible recherche_C22R02:
application: iliad;
arguments: RESULTAT, INDICE_EVT;
variables_temporaires: TROUVE0, TROUVE1;
TROUVE0 = 0;
TROUVE1 = 0;
iterer
: variable R0
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si
    champ_evenement(R0, id_evt) = INDICE_EVT
    et champ_evenement(R0, sens) = SENS_C
    et champ_evenement(R0, penalite) dans (22, 24)
  alors
    TROUVE0 = 1;
  finsi
)
iterer
: variable R1
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si
    champ_evenement(R1, id_evt) = INDICE_EVT
    et champ_evenement(R1, sens) = SENS_R
    et champ_evenement(R1, penalite) = 2
    et positif(TROUVE0)
  alors
    TROUVE1 = 1;
  finsi
)
RESULTAT = TROUVE1;


cible recherche_C22R02_proc:
application: iliad;
variables_temporaires: RESULTAT, INDICE_EVT;
INDICE_EVT = TMP_ARG1;
calculer cible recherche_C22R02 : avec RESULTAT, INDICE_EVT;
TMP_RES = RESULTAT;

cible verif_code_prem_evt:
application: iliad;
arguments: BRES, R;
variables_temporaires: B;
BRES = 0;
B = 1;
iterer
: variable RR
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si
    champ_evenement(RR, id_evt) != 0
    et R != RR
    et meme_variable(champ_evenement(R, code), champ_evenement(RR, code))
  alors
    B = 0;
  finsi
)
si B = 1 alors
  BRES = 1;
sinon
  iterer
  : variable RR
  : entre 0..(nb_evenements() - 1) increment 1
  : dans (
    si
      champ_evenement(RR, id_evt) = 0
      et  R != RR
      et meme_variable(champ_evenement(R, code), champ_evenement(RR, code))
      et champ_evenement(RR, sens) = SENS_R
      et champ_evenement(R, montant) <= champ_evenement(RR, montant)
    alors
      BRES = 1;
    finsi
  )
finsi

cible verif_code_prem_evt_proc:
application: iliad;
calculer cible verif_code_prem_evt : avec TMP_RES, TMP_ARG1;

cible est_code_tax_init:
application: iliad;
arguments: RESULTAT, VAR;
RESULTAT = (
  meme_variable(VAR, COA) ou meme_variable(VAR, COB) ou meme_variable(VAR, COD)
  ou meme_variable(VAR, COE) ou meme_variable(VAR, COF) ou meme_variable(VAR, COG)
  ou meme_variable(VAR, COH) ou meme_variable(VAR, COJ) ou meme_variable(VAR, COL)
  ou meme_variable(VAR, COM) ou meme_variable(VAR, COO) ou meme_variable(VAR, COP)
  ou meme_variable(VAR, COQ) ou meme_variable(VAR, COR) ou meme_variable(VAR, COT)
  ou meme_variable(VAR, COU) ou meme_variable(VAR, COV) ou meme_variable(VAR, COX)
);

cible denature_rappels:
application: iliad;
variables_temporaires:
  TAUX_PENA_2042, PENA, TAUX_PENA_RAPPEL,
  EST_1728, EST_ISF, EST_TAX_INIT, VERIF_PREM_EVT;
si positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD) alors
  si positif(GLOBAL.PRESENT_9YT) et GLOBAL.MONTANT_9YT != 0 alors
    iterer
    : variable R
    : entre 0..(nb_evenements() - 1) increment 1
    : dans (
      si champ_evenement(R, numero) <= GLOBAL.PREMIER_EVT alors
        champ_evenement(R, penalite) = GLOBAL.MONTANT_9YT;
        champ_evenement(R, date) = GLOBAL.DATE_9YU;
      finsi
    )
  finsi
  si positif(GLOBAL.PRESENT_9XT) et GLOBAL.MONTANT_9XT != 0 alors
    iterer
    : variable R
    : entre 0..(nb_evenements() - 1) increment 1
    : dans (
      si champ_evenement(R, numero) <= GLOBAL.PREMIER_EVT alors
        champ_evenement(R, penalite) = GLOBAL.MONTANT_9XT;
        champ_evenement(R, date) = GLOBAL.DATE_9XU;
      finsi
    )
  finsi
  TAUX_PENA_2042 = taux_penalite(GLOBAL.CODE_PENA);
  iterer
  : variable R
  : entre 0..(nb_evenements() - 1) increment 1
  : dans (
    TAUX_PENA_RAPPEL = taux_penalite(champ_evenement(R, penalite));
    si champ_evenement(R, sens) != SENS_M alors
      si positif(GLOBAL.RETARD0718) alors
        PENA = champ_evenement(R, penalite);      
        calculer cible est_rappel_1728 : avec EST_1728, PENA, champ_evenement(R, code);
        si
          non (
            (non positif(EST_1728))
            ou (
              champ_evenement(R, sens) dans (SENS_R, SENS_C)
              et champ_evenement(R, penalite) dans (10, 17)
              et GLOBAL.CODE_PENA dans (7, 18)
            )
            ou champ_evenement(R, penalite) dans (22, 99)
            ou TAUX_PENA_2042 < TAUX_PENA_RAPPEL
          )
        alors
          si champ_evenement(R, sens) = SENS_P alors
            champ_evenement(R, penalite) = 0;
          sinon
            champ_evenement(R, penalite) = GLOBAL.CODE_PENA;
            champ_evenement(R, 2042_rect) = 0;
          finsi
        finsi
        si
          champ_evenement(R, sens) dans (SENS_R, SENS_C)
          et champ_evenement(R, penalite) = 2
          et GLOBAL.CODE_PENA dans (7, 18)
        alors
          champ_evenement(R, anc_penalite) = champ_evenement(R, penalite);
          champ_evenement(R, penalite) = 7;
        sinon_si
          champ_evenement(R, penalite) dans (1, 2, 3, 4, 5, 6, 30, 32, 55)
          et (GLOBAL.DEFAUT1011 ou GLOBAL.RETARD0718)
          et TAUX_PENA_RAPPEL <= TAUX_PENA_2042
        alors
          si
            champ_evenement(R, penalite) = 2 et GLOBAL.CODE_PENA dans (7, 18)
          alors
            GLOBAL.LIMELIGHT = 1;
          sinon
            champ_evenement(R, 2042_rect) = 0;
          finsi
          champ_evenement(R, penalite) = GLOBAL.CODE_PENA;
        finsi
      finsi
      si positif(GLOBAL.DEFAUT) alors
        PENA = champ_evenement(R, penalite);
        calculer cible est_rappel_1728 : avec EST_1728, PENA, champ_evenement(R, code);
        si
          champ_evenement(R, penalite) = 99
          ou (
            champ_evenement(R, sens) = SENS_C
            et champ_evenement(R, penalite) < 1
          )
        alors
          si non meme_variable(champ_evenement(R, code), ACODELAISINR) alors
            champ_evenement(R, date) = GLOBAL.DATE;
            champ_evenement(R, penalite) = GLOBAL.CODE_PENA;
          finsi
        sinon_si
          positif(EST_1728) et champ_evenement(R, sens) != SENS_P
        alors
          calculer cible est_code_isf : avec EST_ISF, champ_evenement(R, code);
          si
            (champ_evenement(R, penalite) dans (2, 3) et non positif(EST_ISF))
            ou (
              TAUX_PENA_RAPPEL <= TAUX_PENA_2042
              et champ_evenement(R, penalite) != 22
            )
          alors
            champ_evenement(R, penalite) = GLOBAL.CODE_PENA;
            champ_evenement(R, 2042_rect) = 0;
          finsi
        sinon_si
          non (champ_evenement(R, penalite) dans (6, 22, 24, 30, 32, 35))
          et champ_evenement(R, sens) != SENS_P
        alors
          champ_evenement(R, penalite) = GLOBAL.CODE_PENA;
          champ_evenement(R, 2042_rect) = 0;
        finsi
      finsi
    sinon
      calculer cible est_code_tax_init : avec EST_TAX_INIT, champ_evenement(R, code);
      si
        non (
          positif(EST_TAX_INIT)
          ou meme_variable(champ_evenement(R, code), 9GF)
          ou meme_variable(champ_evenement(R, code), 9GH)
          ou meme_variable(champ_evenement(R, code), 9NC)
          ou meme_variable(champ_evenement(R, code), 9NG)
          ou meme_variable(champ_evenement(R, code), 9GY)
          ou meme_variable(champ_evenement(R, code), 9PR)
          ou meme_variable(champ_evenement(R, code), 9RS)
          ou meme_variable(champ_evenement(R, code), REGCO)
        )
      alors
        champ_evenement(R, sens) = SENS_C;
        calculer cible verif_code_prem_evt : avec VERIF_PREM_EVT, R;
        si positif(VERIF_PREM_EVT) alors
          champ_evenement(R, penalite) =  0;
        finsi
        si champ_evenement(R, sens) = SENS_M alors
          CORR.V_FLAGMENC = 1;
        finsi
      finsi
    finsi
  )
finsi

cible mauvaise_foi:
application: iliad;
arguments: INDICE_EVT;
variables_temporaires:
  MF_DEF, MONTANT_RECT, NATURE, MONTANT,
  COMMENCE_PAR_7, COMMENCE_PAR_H, EST_ISF,
  COTSOC;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si champ_evenement(R, id_evt) = INDICE_EVT alors
    MF_DEF = 0;
    si
      champ_evenement(R, sens) != SENS_P
      et non (champ_evenement(R, penalite) dans (0, 2, 22, 24, 99))
    alors
      MONTANT_RECT = present(D2042_RECT.champ_evenement(R, code));
      calculer cible get_nature : avec NATURE, champ_evenement(R, code);
      si NATURE = N_CHARGE alors
        MONTANT = present(D2042.champ_evenement(R, code));
        si inf(MONTANT_RECT) < inf (MONTANT) alors
          MF_DEF = 1;
        sinon_si inf(MONTANT_RECT) = 0 alors
          MF_DEF = -1;
        finsi
      sinon_si NATURE = N_REVENU alors
        MONTANT = 0;
        si inf(MONTANT_RECT) > inf(MONTANT) alors
          MF_DEF = 1;
        sinon_si inf (MONTANT_RECT) = 0 alors
          MF_DEF = -1;
        finsi
      finsi
    finsi
    si MF_DEF != 0 alors
      calculer cible alias_commence_par_7 : avec COMMENCE_PAR_7, champ_evenement(R, code);
      calculer cible alias_commence_par_H : avec COMMENCE_PAR_H, champ_evenement(R, code);
      calculer cible est_code_isf : avec EST_ISF, champ_evenement(R, code);
      si
        positif(COMMENCE_PAR_7) ou positif(COMMENCE_PAR_H)
        # codes_credit_imp
        ou meme_variable(champ_evenement(R, code), 8TA)
        ou meme_variable(champ_evenement(R, code), 8TB)
        ou meme_variable(champ_evenement(R, code), 8TC)
        ou meme_variable(champ_evenement(R, code), 8TE)
        ou meme_variable(champ_evenement(R, code), 8TG)
        ou meme_variable(champ_evenement(R, code), 8TH)
        ou meme_variable(champ_evenement(R, code), 8TL)
        ou meme_variable(champ_evenement(R, code), 8TO)
        ou meme_variable(champ_evenement(R, code), 8TS)
        ou meme_variable(champ_evenement(R, code), 8UW)
        ou meme_variable(champ_evenement(R, code), 8UY)
        ou meme_variable(champ_evenement(R, code), 8UZ)
      alors
        GLOBAL.TL_MF_MFIR = GLOBAL.TL_MF_MFIR + MF_DEF;
      sinon_si positif(EST_ISF) alors
        GLOBAL.TL_MF_MFIFI = GLOBAL.TL_MF_MFIFI + MF_DEF;
      sinon
        COTSOC = attribut(champ_evenement(R, code), cotsoc);
        si COTSOC = 2 alors
          GLOBAL.TL_MF_MFCDIS = GLOBAL.TL_MF_MFCDIS + MF_DEF;
        sinon_si COTSOC = 3 alors
          GLOBAL.TL_MF_MFTAXAGA = GLOBAL.TL_MF_MFTAXAGA + MF_DEF;
        sinon_si COTSOC = 4 alors
          GLOBAL.TL_MF_MFCSAL = GLOBAL.TL_MF_MFCSAL + MF_DEF;
        sinon_si COTSOC = 5 alors
          GLOBAL.TL_MF_MFIR = GLOBAL.TL_MF_MFIR + MF_DEF;
        sinon_si COTSOC = 6 alors
          GLOBAL.TL_MF_MFGAIN = GLOBAL.TL_MF_MFGAIN + MF_DEF;
        sinon_si COTSOC = 7 alors
          GLOBAL.TL_MF_MFREGV = GLOBAL.TL_MF_MFREGV + MF_DEF;
        sinon_si COTSOC = 8 alors
          GLOBAL.TL_MF_MFCHR = GLOBAL.TL_MF_MFCHR + MF_DEF;
        sinon_si COTSOC = 9 alors
          GLOBAL.TL_MF_MFPCAP = GLOBAL.TL_MF_MFPCAP + MF_DEF;
        sinon_si COTSOC = 10 alors
          GLOBAL.TL_MF_MFCS = GLOBAL.TL_MF_MFCS + MF_DEF;
          GLOBAL.TL_MF_MFRD = GLOBAL.TL_MF_MFRD + MF_DEF;
          GLOBAL.TL_MF_MFPS = GLOBAL.TL_MF_MFPS + MF_DEF;
          GLOBAL.TL_MF_MFPSOL = GLOBAL.TL_MF_MFPSOL + MF_DEF;
        sinon_si COTSOC = 11 alors
          GLOBAL.TL_MF_MFRSE1 = GLOBAL.TL_MF_MFRSE1 + MF_DEF;
          GLOBAL.TL_MF_MFRD = GLOBAL.TL_MF_MFRD + MF_DEF;
        sinon_si COTSOC = 12 alors
          GLOBAL.TL_MF_MFRSE2 = GLOBAL.TL_MF_MFRSE2 + MF_DEF;
          GLOBAL.TL_MF_MFRD = GLOBAL.TL_MF_MFRD + MF_DEF;
        sinon_si COTSOC = 13 alors
          GLOBAL.TL_MF_MFRSE3 = GLOBAL.TL_MF_MFRSE3 + MF_DEF;
          GLOBAL.TL_MF_MFRD = GLOBAL.TL_MF_MFRD + MF_DEF;
        sinon_si COTSOC = 14 alors
          GLOBAL.TL_MF_MFRSE4 = GLOBAL.TL_MF_MFRSE4 + MF_DEF;
          GLOBAL.TL_MF_MFRD = GLOBAL.TL_MF_MFRD + MF_DEF;
        sinon_si COTSOC = 16 alors
          GLOBAL.TL_MF_MFLOY = GLOBAL.TL_MF_MFLOY + MF_DEF;
        sinon_si COTSOC = 17 alors
          GLOBAL.TL_MF_MFCVN = GLOBAL.TL_MF_MFCVN + MF_DEF;
        sinon_si COTSOC = 18 alors
          GLOBAL.TL_MF_MFGLO = GLOBAL.TL_MF_MFGLO + MF_DEF;
          GLOBAL.TL_MF_MFIR = GLOBAL.TL_MF_MFIR + MF_DEF;
          GLOBAL.TL_MF_MFRD = GLOBAL.TL_MF_MFRD + MF_DEF;
          GLOBAL.TL_MF_MFCVN = GLOBAL.TL_MF_MFCVN + MF_DEF;
        sinon_si COTSOC = 19 alors
          GLOBAL.TL_MF_MFRSE5 = GLOBAL.TL_MF_MFRSE5 + MF_DEF;
          GLOBAL.TL_MF_MFRD = GLOBAL.TL_MF_MFRD + MF_DEF;
        sinon_si COTSOC = 20 alors
          GLOBAL.TL_MF_MFRSE1 = GLOBAL.TL_MF_MFRSE1 + MF_DEF;
          GLOBAL.TL_MF_MFRSE6 = GLOBAL.TL_MF_MFRSE6 + MF_DEF;
          GLOBAL.TL_MF_MFRD = GLOBAL.TL_MF_MFRD + MF_DEF;
        sinon_si COTSOC = 21 alors
          GLOBAL.TL_MF_MFRSE2 = GLOBAL.TL_MF_MFRSE2 + MF_DEF;
          GLOBAL.TL_MF_MFRSE6 = GLOBAL.TL_MF_MFRSE6 + MF_DEF;
          GLOBAL.TL_MF_MFRD = GLOBAL.TL_MF_MFRD + MF_DEF;
        sinon_si COTSOC = 22 alors
          GLOBAL.TL_MF_MFRSE7 = GLOBAL.TL_MF_MFRSE7 + MF_DEF;
          GLOBAL.TL_MF_MFRD = GLOBAL.TL_MF_MFRD + MF_DEF;
        sinon_si COTSOC = 1 alors
          GLOBAL.TL_MF_MFCS = GLOBAL.TL_MF_MFCS + MF_DEF;
          GLOBAL.TL_MF_MFRD = GLOBAL.TL_MF_MFRD + MF_DEF;
          GLOBAL.TL_MF_MFPS = GLOBAL.TL_MF_MFPS + MF_DEF;
          GLOBAL.TL_MF_MFPSOL = GLOBAL.TL_MF_MFPSOL + MF_DEF;
          GLOBAL.TL_MF_MFIR = GLOBAL.TL_MF_MFIR + MF_DEF;
        sinon
          GLOBAL.TL_MF_MFIR = GLOBAL.TL_MF_MFIR + MF_DEF;
        finsi
      finsi
    finsi
  finsi
)

cible recherche_cpena:
application: iliad;
arguments: CPENA, R;
variables_temporaires: PREM_RAPPEL, MONTANT_INITIAL, PRESENT_2042, MONTANT, TROUVE, NATURE;
PREM_RAPPEL = 0;
si present(D2042.champ_evenement(R, code)) alors
  MONTANT_INITIAL = D2042.champ_evenement(R, code);
  PRESENT_2042 = 1;
sinon
  MONTANT_INITIAL = 0;
  PRESENT_2042 = 0;
finsi
MONTANT = MONTANT_INITIAL;
TROUVE = 0;
iterer
: variable RR
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si
    non (positif(GLOBAL.RETARD) et champ_evenement(RR, numero) = champ_evenement(PREM_RAPPEL, numero))
    et champ_evenement(RR, numero) <= champ_evenement(R, numero)
    et meme_variable(champ_evenement(R, code), champ_evenement(RR, code))
    et champ_evenement(R, penalite) = champ_evenement(RR, penalite)
    et champ_evenement(RR, sens) dans (SENS_R, SENS_C)
  alors
    MONTANT = MONTANT + champ_evenement(RR, montant);
    TROUVE = 1;
  finsi
)
si positif(TROUVE) alors
  si MONTANT < champ_evenement(R, montant) alors
    CPENA = 4;
  sinon_si
    positif(PRESENT_2042)
    et MONTANT - MONTANT_INITIAL < champ_evenement(R, montant)
  alors
    CPENA = 5;
  sinon
    CPENA = 2;
  finsi
sinon_si
  (positif(GLOBAL.RETARD) ou positif(GLOBAL.DEFAUT))
  et GLOBAL.CODE_PENA = champ_evenement(R, penalite)
alors
  CPENA = 6;
sinon_si MONTANT < champ_evenement(R, montant) alors
  si champ_evenement(R, penalite) != champ_evenement(R, anc_penalite) alors
    CPENA = 6;
  sinon
    CPENA = 4;
  finsi
sinon
  CPENA = 5;
finsi
calculer cible get_nature : avec NATURE, champ_evenement(R, code);
si NATURE = N_CHARGE et CPENA dans (3, 4) alors
  CPENA = 1;
finsi

cible transf_rappels_prim:
application: iliad;
variables_temporaires: NATURE, SENS, PENALITE;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si
    champ_evenement(R, numero) >= 0
    et champ_evenement(R, numero) <= GLOBAL.PREMIER_EVT
    et non (
      meme_variable(champ_evenement(R, code), 9YT)
      ou meme_variable(champ_evenement(R, code), 9YU)
      ou meme_variable(champ_evenement(R, code), 9XT)
      ou meme_variable(champ_evenement(R, code), 9XU)
    )
  alors
    GLOBAL.champ_evenement(R, code) =  champ_evenement(R, montant);
    calculer cible get_nature : avec NATURE, champ_evenement(R, code);
    si NATURE = N_REVENU alors
      SENS = SENS_R;
      PENALITE = 99;
    sinon_si NATURE = N_CHARGE alors
      SENS = SENS_C;
      PENALITE = 0;
    sinon # interdit
      SENS = SENS_R; # = 0
      PENALITE = 0;        
    finsi
    champ_evenement(R, code) reference ANREV;
    champ_evenement(R, montant) = 0;
    champ_evenement(R, sens) = SENS;
    champ_evenement(R, penalite) = PENALITE;
  finsi
)

cible transfo_pena_regco:
application: iliad;
arguments: R;
si meme_variable(champ_evenement(R, code), REGCO) alors
  GLOBAL.PRESENT_REGCO = 1;
  GLOBAL.PENALITE_REGCO = champ_evenement(R, penalite);
  GLOBAL.NUM_EVT_REGCO = champ_evenement(R, numero);
  GLOBAL.NUM_RAP_REGCO = champ_evenement(R, rappel);
  GLOBAL.IND_20_REGCO = champ_evenement(R, 2042_rect);
finsi

fonction get_taux_penalite:
application: iliad;
arguments: PENA;
resultat: TAUX;
si PENA dans (2, 7, 10, 17) alors
  TAUX = 10;
sinon_si PENA dans (3, 8, 11, 30, 55) alors
  TAUX = 40;
sinon_si PENA dans (4, 5, 9, 12, 31, 32) alors
  TAUX = 80;
sinon_si PENA = 6 alors
  TAUX = 100;
sinon
  TAUX = 0;
finsi

fonction is_minoration_sf:
application: iliad;
arguments: INITIALE, COURANTE;
resultat: EST_MIN_SF;
si INITIALE = SF_MARIAGE ou INITIALE = SF_PACSE alors
  EST_MIN_SF = COURANTE = SF_VEUVAGE_TRUE ou COURANTE = SF_PACSE;
sinon_si INITIALE = SF_VEUVAGE_TRUE alors
  EST_MIN_SF = COURANTE = SF_MARIAGE ou COURANTE = SF_PACSE;
sinon
  EST_MIN_SF = 1;
finsi

fonction get_strate_pena:
application: iliad;
arguments: STR;
resultat: PENA;
si STR = 0 alors
  PENA = 99;
sinon_si STR = 1 alors
  PENA = -1;
sinon_si STR dans (2, 3) alors
  PENA = 6;
sinon_si STR = 4 alors
  PENA = 12;
sinon_si STR = 5 alors
  PENA = 32;
sinon_si STR = 6 alors
  PENA = 31;
sinon_si STR = 7 alors
  PENA = 5;
sinon_si STR = 8 alors
  PENA = 4;
sinon_si STR = 9 alors
  PENA = 35;
sinon_si STR = 10 alors
  PENA = 30;
sinon_si STR = 11 alors
  PENA = 11;
sinon_si STR = 12 alors
  PENA = 8;
sinon_si STR = 13 alors
  PENA = 55;
sinon_si STR = 14 alors
  PENA = 3;
sinon_si STR = 15 alors
  PENA = 17;
sinon_si STR = 16 alors
  PENA = 10;
sinon_si STR = 17 alors
  PENA = 7;
sinon_si STR dans (18, 19, 20, 21, 22, 23) alors
  PENA = 2;
sinon_si STR = 24 alors
  PENA = 22;
sinon_si STR = 25 alors
  PENA = 24;
sinon
  PENA = -1;
finsi

fonction get_strate_taux:
application: iliad;
arguments: STR;
resultat: PENA;
si STR = 0 alors
  PENA = 99;
sinon_si STR = 1 alors
  PENA = -1;
sinon_si STR dans (2, 3) alors
  PENA = 6;
sinon_si STR = 4 alors
  PENA = 12;
sinon_si STR = 5 alors
  PENA = 32;
sinon_si STR = 6 alors
  PENA = 31;
sinon_si STR = 7 alors
  PENA = 5;
sinon_si STR = 8 alors
  PENA = 4;
sinon_si STR = 9 alors
  PENA = 35;
sinon_si STR = 10 alors
  PENA = 30;
sinon_si STR = 11 alors
  PENA = 11;
sinon_si STR = 12 alors
  PENA = 8;
sinon_si STR = 13 alors
  PENA = 55;
sinon_si STR = 14 alors
  PENA = 3;
sinon_si STR = 15 alors
  PENA = 17;
sinon_si STR = 16 alors
  PENA = 10;
sinon_si STR = 17 alors
  PENA = 7;
sinon_si STR dans (18, 19, 20, 21, 22, 23) alors
  PENA = 2;
sinon_si STR = 24 alors
  PENA = 22;
sinon_si STR = 25 alors
  PENA = 24;
sinon
  PENA = -1;
finsi

fonction get_strate_taux:
application: iliad;
arguments: STR;
resultat: TAUX;
si STR dans (0, 1) alors
  TAUX = -1;
sinon_si STR dans (2, 18, 19, 20, 21, 22, 23, 24, 25) alors
  TAUX = 0;
sinon_si STR = 3 alors
  TAUX = 100;
sinon_si STR dans (4, 5, 6, 7, 8, 9) alors
  TAUX = 80;
sinon_si STR dans (10, 11, 12, 13, 14) alors
  TAUX = 40;
sinon_si STR dans (15, 16, 17) alors
  TAUX = 10;
sinon
  TAUX = -1;
finsi

cible get_code_situation_famille:
application: iliad;
arguments: RESULTAT, CORR, VAR;
si meme_variable(VAR, 0AM) alors
  RESULTAT = SF_MARIAGE;
sinon_si meme_variable(VAR, 0AC) alors
  RESULTAT = SF_CELIBAT;
sinon_si meme_variable(VAR, 0AD) alors
  RESULTAT = SF_DIVORCE;
sinon_si meme_variable(VAR, 0AO) alors
  RESULTAT = SF_PACSE;
sinon_si meme_variable(VAR, 0AV) alors
  si positif(CORR) et GLOBAL.ANNEE_DECES_CONJOINT = GLOBAL.ANNEE_REVENU alors
    RESULTAT = SF_VEUVAGE_TRUE;
  sinon
    RESULTAT = SF_VEUVAGE_FALSE;
  finsi
sinon
  RESULTAT = SF_INVALIDE;
finsi

cible is_rappel_strate:
application: iliad;
arguments: RESULTAT, R, S;
variables_temporaires: TAUX_STR, CODE_SIT_FAM, CORR;
si S = 0 alors
  RESULTAT = (
    champ_evenement(R, penalite) = 99
    ou (champ_evenement(R, sens) = SENS_C et champ_evenement(R, penalite) = 0)
  );
  si positif(RESULTAT) alors
    GLOBAL.MAJO_TAUX_STRATE = get_taux_penalite(GLOBAL.CODE_PENA);
  finsi
sinon_si S = 1 alors
  RESULTAT = (
    dans_domaine(champ_evenement(R, code), saisie contexte)
    et non positif(GLOBAL.IND_20_REGCO)
    et non (
      meme_variable(champ_evenement(R, code), ANREV)
      ou meme_variable(champ_evenement(R, code), ANCSDED)
      ou meme_variable(champ_evenement(R, code), CALCULIR)
      ou meme_variable(champ_evenement(R, code), IND_TRAIT)
      ou meme_variable(champ_evenement(R, code), ROLCSG)
    )
  );
  si positif(RESULTAT) alors
    TAUX_STR = get_taux_penalite(GLOBAL.CODE_PENA);
    si TAUX_STR != 0 alors
      GLOBAL.MAJO_TAUX_STRATE = TAUX_STR;
    finsi
    si champ_evenement(R, penalite) != 0 alors
      GLOBAL.MAJO_CODE_STRATE = champ_evenement(R, penalite);
    finsi
  finsi
sinon_si S = 2 alors
  CORR = 0;
  calculer cible get_code_situation_famille : avec CODE_SIT_FAM, CORR, champ_evenement(R, code);
  si
    GLOBAL.SF_COURANTE != SF_INVALIDE
    et (
      CODE_SIT_FAM != SF_INVALIDE
      ou (
        meme_variable(champ_evenement(R, code), 0DA)
        ou meme_variable(champ_evenement(R, code), 0DB)
      )
    )
  alors
    RESULTAT = is_minoration_sf(GLOBAL.SF_INITIALE, GLOBAL.SF_COURANTE);
  sinon
    RESULTAT = (
      champ_evenement(R, sens) = SENS_M
      ou (
        positif(GLOBAL.DEFAUT)
        et champ_evenement(R, sens) = SENS_R
        et (
          meme_variable(champ_evenement(R, code), 0AM)
          ou meme_variable(champ_evenement(R, code), 0AC)
          ou meme_variable(champ_evenement(R, code), 0AD)
          ou meme_variable(champ_evenement(R, code), 0AO)
          ou meme_variable(champ_evenement(R, code), 0AV)
        )
      )
    );
  finsi
sinon_si
  champ_evenement(R, numero) = GLOBAL.NUM_EVT_REGCO
  et meme_variable(champ_evenement(R, code), REGCO)
alors
  RESULTAT = (
    champ_evenement(R, penalite) = get_strate_pena(S)
  );
sinon_si S = 18 alors
  RESULTAT = (
    champ_evenement(R, penalite) = 2
    et positif(champ_evenement(R, 2042_rect))
    et attribut(champ_evenement(R, code), categorie_TL) = 10
  );
sinon_si S = 19 alors
  RESULTAT = (
    champ_evenement(R, penalite) = 2
    et (non positif(champ_evenement(R, 2042_rect)))
    et attribut(champ_evenement(R, code), categorie_TL) = 10
  );
sinon_si S = 20 alors
  RESULTAT = (
    champ_evenement(R, penalite) = 2
    et positif(champ_evenement(R, 2042_rect))
    et attribut(champ_evenement(R, code), categorie_TL) = 15
  );
sinon_si S = 21 alors
  RESULTAT = (
    champ_evenement(R, penalite) = 2
    et (non positif(champ_evenement(R, 2042_rect)))
    et attribut(champ_evenement(R, code), categorie_TL) = 15
  );
sinon_si S = 22 alors
  RESULTAT = (
    champ_evenement(R, penalite) = 2
    et positif(champ_evenement(R, 2042_rect))
    et non (attribut(champ_evenement(R, code), categorie_TL) dans (10, 15))
  );
sinon_si S = 23 alors
  RESULTAT = (
    champ_evenement(R, penalite) = 2
    et (non positif(champ_evenement(R, 2042_rect)))
    et non (attribut(champ_evenement(R, code), categorie_TL) dans (10, 15))
  );
sinon
  RESULTAT = (
    champ_evenement(R, penalite) = get_strate_pena(S)
  );
finsi

cible get_strate:
application: iliad;
arguments: RESULTAT, R;
variables_temporaires: TROUVE;
si champ_evenement(R, sens) = SENS_P et champ_evenement(R, penalite) = 0 alors
  RESULTAT = 24;
sinon
  RESULTAT = -1;
  iterer : variable S : entre 0..(GLOBAL.NB_STRATES - 1) increment 1 : dans (
    si RESULTAT = -1 alors
      calculer cible is_rappel_strate : avec TROUVE, R, S;
      si RESULTAT = -1 et positif(TROUVE) alors
        RESULTAT = S;
      finsi
    finsi
  )
finsi

fonction vers_mois:
application: iliad;
arguments: D;
resultat: M;
M = inf(D / 10000);

fonction vers_annee:
application: iliad;
arguments: D;
resultat: A;
A = inf(D % 10000);

fonction vers_date:
application: iliad;
arguments: M, A;
resultat: D;
D = inf(M) * 10000 + inf(A);

cible traite_inr:
application: iliad;
arguments: INDICE_EVT;
variables_temporaires: MOIS_DEP, ANNEE_DEP, R_EVT;
GLOBAL.MENTION_EXP = 0;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si
    champ_evenement(R, sens) = SENS_R
    et champ_evenement(R, penalite) = 1
    et (
      meme_variable(champ_evenement(R, code), 0AM)
      ou meme_variable(champ_evenement(R, code), 0AC)
      ou meme_variable(champ_evenement(R, code), 0AD)
      ou meme_variable(champ_evenement(R, code), 0AO)
      ou meme_variable(champ_evenement(R, code), 0AV)
    )
  alors
    GLOBAL.MENTION_EXP = 1;
  finsi
)
MOIS_DEP = 7;
ANNEE_DEP = GLOBAL.ANNEE_REVENU + 1;
R_EVT = -1;
iterer
: variable R
: entre (nb_evenements() - 1)..0 increment -1
: dans (
  si R_EVT = -1 et champ_evenement(R, id_evt) = INDICE_EVT alors
    R_EVT = R;
  finsi
)
si R_EVT >= 0 alors
  GLOBAL.INR_ANNEE_COR = vers_annee(champ_evenement(R_EVT, date));
  si vers_annee(champ_evenement(R_EVT, date)) < 2018 alors
    GLOBAL.INR_NB_MOIS =
      12 * (vers_annee(champ_evenement(R_EVT, date)) - ANNEE_DEP)
      + (vers_mois(champ_evenement(R_EVT, date)) - MOIS_DEP) + 1;
    GLOBAL.INR_NB_MOIS2 = 0;
  sinon_si ANNEE_DEP > 2018 alors
    GLOBAL.INR_NB_MOIS = 0;
    GLOBAL.INR_NB_MOIS2 =
      12 * (vers_annee(champ_evenement(R_EVT, date)) - ANNEE_DEP)
      + (vers_mois(champ_evenement(R_EVT, date)) - MOIS_DEP) + 1;
  sinon
    GLOBAL.INR_NB_MOIS = 12 * (2018 - ANNEE_DEP) + (1 - MOIS_DEP);
    GLOBAL.INR_NB_MOIS2 =
      12 * (vers_annee(champ_evenement(R_EVT, date)) - 2018)
      + vers_mois(champ_evenement(R_EVT, date));
  finsi
finsi

cible is_rappel_abat_20:
application: iliad;
variables_temporaires: RESULTAT, R, NATURE, ABAT;
R = TMP_ARG1;
si
  champ_evenement(R, sens) = SENS_M
  ou (
    champ_evenement(R, sens) = SENS_C
    et champ_evenement(R, penalite) = 0
  )
alors
  calculer cible get_nature : avec NATURE, champ_evenement(R, code);
  RESULTAT = (NATURE = N_REVENU);
sinon_si positif(GLOBAL.LIMELIGHT) alors
  RESULTAT = champ_evenement(R, 2042_rect);
sinon_si positif(champ_evenement(R, 2042_rect)) alors
  RESULTAT = 1;
sinon
  calculer cible get_abat : avec ABAT, champ_evenement(R, code);
  si positif(ABAT) alors
    RESULTAT = (
      champ_evenement(R, penalite) = 1
      ou (
        champ_evenement(R, sens) = SENS_R
        et champ_evenement(R, penalite) dans (7, 99)
        et (
          non (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
          ou GLOBAL.CODE_PENA = 7
        )
      )
    );
  sinon
    RESULTAT = 0;
  finsi
finsi
TMP_RES = RESULTAT;

cible is_rappel_abat_20_proc:
application: iliad;
arguments: RESULTAT, R;
variables_temporaires: NATURE, ABAT;
si
  champ_evenement(R, sens) = SENS_M
  ou (
    champ_evenement(R, sens) = SENS_C
    et champ_evenement(R, penalite) = 0
  )
alors
  calculer cible get_nature : avec NATURE, champ_evenement(R, code);
  RESULTAT = (NATURE = N_REVENU);
sinon_si positif(GLOBAL.LIMELIGHT) alors
  RESULTAT = champ_evenement(R, 2042_rect);
sinon_si positif(champ_evenement(R, 2042_rect)) alors
  RESULTAT = 1;
sinon
  calculer cible get_abat : avec ABAT, champ_evenement(R, code);
  si positif(ABAT) alors
    RESULTAT = (
      champ_evenement(R, penalite) = 1
      ou (
        champ_evenement(R, sens) = SENS_R
        et champ_evenement(R, penalite) dans (7, 99)
        et (
          non (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
          ou GLOBAL.CODE_PENA = 7
        )
      )
    );
  sinon
    RESULTAT = 0;
  finsi
finsi


cible est_code_sf_naiss:
application: iliad;
arguments: RESULTAT, VAR;
RESULTAT = (
  meme_variable(VAR, 0AM) ou meme_variable(VAR, 0AC) ou meme_variable(VAR, 0AD)
  ou meme_variable(VAR, 0AO) ou meme_variable(VAR, 0AV) ou meme_variable(VAR, 0DA)
  ou meme_variable(VAR, 0DB)
);

cible is_rappel_autorise_maj:
application: iliad;
arguments: RESULTAT, R, MAJ;
variables_temporaires: EST_SF_NAISS, EST_TAX_INIT;
si MAJ = MAJ_TL alors
  RESULTAT = (attribut(champ_evenement(R, code), categorie_TL) != 10);
sinon_si MAJ = MAJ_NON_TL alors
  RESULTAT = (attribut(champ_evenement(R, code), categorie_TL) = 10);
sinon_si MAJ = MAJ_TL15 alors
  RESULTAT = (attribut(champ_evenement(R, code), categorie_TL) != 15);
sinon_si MAJ = MAJ_NON_TL15 alors
  RESULTAT = (attribut(champ_evenement(R, code), categorie_TL) = 15);
sinon_si MAJ = MAJ_RAPPEL_C alors
  RESULTAT = (
    champ_evenement(R, sens) = SENS_C
    et (champ_evenement(R, penalite) < 1 ou champ_evenement(R, penalite) = 99)
    et GLOBAL.CODE_PENA != 22
  );
sinon_si MAJ = MAJ_RAPPEL_CP alors
  RESULTAT = (
    champ_evenement(R, sens) = SENS_C 
    et champ_evenement(R, penalite) > 1
  );
sinon_si MAJ = MAJ_RAPPEL_CP01 alors
  RESULTAT = (
    champ_evenement(R, sens) = SENS_C
    et champ_evenement(R, penalite) = 1
  );
sinon_si MAJ = MAJ_RAPPEL_CP22 alors
  RESULTAT = (
    champ_evenement(R, sens) = SENS_C
    et champ_evenement(R, penalite) = 22
  );
sinon_si MAJ = MAJ_RAPPEL_CP24 alors
  RESULTAT = (
    champ_evenement(R, sens) = SENS_C
    et champ_evenement(R, penalite) = 24
  );
sinon_si MAJ = MAJ_RAPPEL_F alors
  calculer cible est_code_sf_naiss : avec EST_SF_NAISS, champ_evenement(R, code);
  RESULTAT = (
    champ_evenement(R, sens) = SENS_R
    et champ_evenement(R, penalite) = 1
    et positif(EST_SF_NAISS)
  );
sinon_si MAJ = MAJ_RAPPEL_NF alors
  calculer cible est_code_sf_naiss : avec EST_SF_NAISS, champ_evenement(R, code);
  RESULTAT = (
    champ_evenement(R, sens) = SENS_R
    et champ_evenement(R, penalite) = 1
    et (non positif(EST_SF_NAISS))
  );
sinon_si MAJ = MAJ_RAPPEL_M alors
  calculer cible est_code_sf_naiss : avec EST_SF_NAISS, champ_evenement(R, code);
  RESULTAT = (
    champ_evenement(R, sens) = SENS_M
    et (non positif(EST_SF_NAISS))
    et non (
      meme_variable(champ_evenement(R, code), REGCO)
      et (GLOBAL.PENALITE_REGCO dans (1, 22, 24, 99))
    )
  );
sinon_si MAJ = MAJ_RAPPEL_MF alors
  calculer cible est_code_sf_naiss : avec EST_SF_NAISS, champ_evenement(R, code);
  RESULTAT = (champ_evenement(R, sens) = SENS_M et positif(EST_SF_NAISS));
sinon_si MAJ = MAJ_RAPPEL_NON_M alors
  RESULTAT = (champ_evenement(R, sens) != SENS_M);
sinon_si MAJ = MAJ_RAPPEL_P alors
  RESULTAT = (champ_evenement(R, sens) = SENS_P);
sinon_si MAJ = MAJ_RAPPEL_R alors
  RESULTAT = (champ_evenement(R, sens) = SENS_R);
sinon_si MAJ = MAJ_RAPPEL_R55 alors
  RESULTAT = (
    meme_variable(champ_evenement(R, code), REGCO)
    et (GLOBAL.PENALITE_REGCO dans (1, 99))
  );
sinon_si MAJ = MAJ_1728 alors
  RESULTAT = (champ_evenement(R, penalite) dans (7, 8, 10, 11, 17, 18, 31));
sinon_si MAJ = MAJ_ABAT_20 alors
  calculer cible is_rappel_abat_20_proc : avec RESULTAT, R;
sinon_si MAJ = MAJ_CODE_1729_2A5 alors
  RESULTAT = (champ_evenement(R, penalite) dans (2, 3, 4, 5, 30, 32, 35, 55));
sinon_si MAJ = MAJ_CODE_1729_6 alors 
  RESULTAT = (champ_evenement(R, penalite) = 6);
sinon_si MAJ = MAJ_CODE_22 alors
  RESULTAT = (
    champ_evenement(R, penalite) = 22
    et (
      champ_evenement(R, sens) = SENS_R
      ou meme_variable(champ_evenement(R, code), REGCO)
    )
  );
sinon_si MAJ = MAJ_CODE_24 alors
  RESULTAT = (
    champ_evenement(R, penalite) = 24
    et (
      champ_evenement(R, sens) = SENS_R
      ou meme_variable(champ_evenement(R, code), REGCO)
    )
  );
sinon_si MAJ = MAJ_CONTEXTE_22 alors
  calculer cible est_code_tax_init : avec EST_TAX_INIT, champ_evenement(R, code);
  RESULTAT = (GLOBAL.CODE_PENA = 22 et positif(EST_TAX_INIT));
sinon_si MAJ = MAJ_MENTION_EXP_99 alors
  calculer cible est_code_tax_init : avec EST_TAX_INIT, champ_evenement(R, code);
  RESULTAT = (
    champ_evenement(R, penalite) = 99
    et (non positif(GLOBAL.DEFAUT))
    et non (
      meme_variable(champ_evenement(R, code), REGCO)
      ou positif(EST_TAX_INIT)
    )
  );
sinon_si MAJ = MAJ_MENTION_EXP_99R alors
  calculer cible est_code_tax_init : avec EST_TAX_INIT, champ_evenement(R, code);
  RESULTAT = (
    champ_evenement(R, penalite) = 99
    et GLOBAL.CODE_PENA != 22
    et (non positif(GLOBAL.DEFAUT))
    et (
      meme_variable(champ_evenement(R, code), REGCO)
      ou positif(EST_TAX_INIT)
    )
  );
sinon_si MAJ = MAJ_NON_MENTION_EXP alors
  RESULTAT = 1;
sinon
  RESULTAT = 0;
finsi

cible is_rappel_autorise:
application: iliad;
arguments: RESULTAT, R, MAJ0, MAJ1, MAJ2, MAJ3;
variables_temporaires: RES0, RES1, RES2, RES3;
RES0 = 1;
RES1 = 1;
RES2 = 1;
RES3 = 1;
si present(MAJ0) alors
  calculer cible is_rappel_autorise_maj : avec RES0, R, MAJ0;
finsi
si present(MAJ1) alors
  calculer cible is_rappel_autorise_maj : avec RES1, R, MAJ1;
finsi
si present(MAJ2) alors
  calculer cible is_rappel_autorise_maj : avec RES2, R, MAJ2;
finsi
si present(MAJ3) alors
  calculer cible is_rappel_autorise_maj : avec RES3, R, MAJ3;
finsi
RESULTAT = (RES0 et RES1 et RES2 et RES3);

cible is_rappel_autorise_proc:
application: iliad;
variables_temporaires: RESULTAT, R, MAJ0, MAJ1, MAJ2, MAJ3;
R = TMP_ARG1;
MAJ0 = TMP_ARG2;
MAJ1 = TMP_ARG3;
MAJ2 = TMP_ARG4;
MAJ3 = TMP_ARG5;
calculer cible is_rappel_autorise : avec RESULTAT, R, MAJ0, MAJ1, MAJ2, MAJ3;
TMP_RES = RESULTAT;

cible is_init_1731:
application: iliad;
arguments: RESULTAT, INDICE_EVT;
calculer cible init_1731;
RESULTAT = 0;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si
    champ_evenement(R, id_evt) = INDICE_EVT
    et champ_evenement(R, sens) dans (SENS_R, SENS_C)
  alors
    RESULTAT = 1;
  finsi
)

cible calcul_majo_P:
application: iliad;
arguments: NB_RAPPELS_P;
si GLOBAL.MAJO_D2042_P_NB > 0 et NB_RAPPELS_P != 0 alors
  calculer cible reset_saisie_calc;
  calculer cible remplit_tgv_majo_d2042_p;
  CORR.PASS_TLIR = CORR.TL_IR + 0;
  CORR.PASS_TLIFI = CORR.TL_IFI + 0;
  CORR.PASS_TLCS = CORR.TL_CS + 0;
  CORR.PASS_TLTAXAGA = CORR.TL_TAXAGA + 0;
  CORR.PASS_TLCDIS = CORR.TL_CDIS + 0;
  si GLOBAL.MAJO_T_RABP != 0 alors
    CORR.T_RABP = GLOBAL.MAJO_T_RABP;
    CORR.T_RABP07 = GLOBAL.MAJO_T_RABP07;
    CORR.T_RABP08 = GLOBAL.MAJO_T_RABP08;
    CORR.T_RABP09 = GLOBAL.MAJO_T_RABP09;
    CORR.T_RABP10 = GLOBAL.MAJO_T_RABP10;
    CORR.T_RABP11 = GLOBAL.MAJO_T_RABP11;
    CORR.T_RABP12 = GLOBAL.MAJO_T_RABP12;
    CORR.T_RABP17 = GLOBAL.MAJO_T_RABP17;
    CORR.T_RABP31 = GLOBAL.MAJO_T_RABP31;
  finsi
  si GLOBAL.MAJO_T_RABPPS != 0 alors
    CORR.T_RABPPS = GLOBAL.MAJO_T_RABPPS;
    CORR.T_RABPPS07 = GLOBAL.MAJO_T_RABPPS07;
    CORR.T_RABPPS08 = GLOBAL.MAJO_T_RABPPS08;
    CORR.T_RABPPS09 = GLOBAL.MAJO_T_RABPPS09;
    CORR.T_RABPPS10 = GLOBAL.MAJO_T_RABPPS10;
    CORR.T_RABPPS11 = GLOBAL.MAJO_T_RABPPS11;
    CORR.T_RABPPS12 = GLOBAL.MAJO_T_RABPPS12;
    CORR.T_RABPPS17 = GLOBAL.MAJO_T_RABPPS17;
    CORR.T_RABPPS31 = GLOBAL.MAJO_T_RABPPS31;
  finsi
  si GLOBAL.MAJO_T_RABPCS != 0 alors
    CORR.T_RABPCS = GLOBAL.MAJO_T_RABPCS;
    CORR.T_RABPCS07 = GLOBAL.MAJO_T_RABPCS07;
    CORR.T_RABPCS08 = GLOBAL.MAJO_T_RABPCS08;
    CORR.T_RABPCS09 = GLOBAL.MAJO_T_RABPCS09;
    CORR.T_RABPCS10 = GLOBAL.MAJO_T_RABPCS10;
    CORR.T_RABPCS11 = GLOBAL.MAJO_T_RABPCS11;
    CORR.T_RABPCS12 = GLOBAL.MAJO_T_RABPCS12;
    CORR.T_RABPCS17 = GLOBAL.MAJO_T_RABPCS17;
    CORR.T_RABPCS31 = GLOBAL.MAJO_T_RABPCS31;
  finsi
  si GLOBAL.MAJO_T_RABPRD != 0 alors
    CORR.T_RABPRD = GLOBAL.MAJO_T_RABPRD;
    CORR.T_RABPRD07 = GLOBAL.MAJO_T_RABPRD07;
    CORR.T_RABPRD08 = GLOBAL.MAJO_T_RABPRD08;
    CORR.T_RABPRD09 = GLOBAL.MAJO_T_RABPRD09;
    CORR.T_RABPRD10 = GLOBAL.MAJO_T_RABPRD10;
    CORR.T_RABPRD11 = GLOBAL.MAJO_T_RABPRD11;
    CORR.T_RABPRD12 = GLOBAL.MAJO_T_RABPRD12;
    CORR.T_RABPRD17 = GLOBAL.MAJO_T_RABPRD17;
    CORR.T_RABPRD31 = GLOBAL.MAJO_T_RABPRD31;
  finsi
  si GLOBAL.MAJO_T_RABPCH != 0 alors
    CORR.T_RABPCH = GLOBAL.MAJO_T_RABPCH;
    CORR.T_RABPCH07 = GLOBAL.MAJO_T_RABPCH07;
    CORR.T_RABPCH08 = GLOBAL.MAJO_T_RABPCH08;
    CORR.T_RABPCH09 = GLOBAL.MAJO_T_RABPCH09;
    CORR.T_RABPCH10 = GLOBAL.MAJO_T_RABPCH10;
    CORR.T_RABPCH11 = GLOBAL.MAJO_T_RABPCH11;
    CORR.T_RABPCH12 = GLOBAL.MAJO_T_RABPCH12;
    CORR.T_RABPCH17 = GLOBAL.MAJO_T_RABPCH17;
    CORR.T_RABPCH31 = GLOBAL.MAJO_T_RABPCH31;
  finsi
  si GLOBAL.MAJO_T_RABPLO != 0 alors
    CORR.T_RABPLO = GLOBAL.MAJO_T_RABPLO;
    CORR.T_RABPLO07 = GLOBAL.MAJO_T_RABPLO07;
    CORR.T_RABPLO08 = GLOBAL.MAJO_T_RABPLO08;
    CORR.T_RABPLO09 = GLOBAL.MAJO_T_RABPLO09;
    CORR.T_RABPLO10 = GLOBAL.MAJO_T_RABPLO10;
    CORR.T_RABPLO11 = GLOBAL.MAJO_T_RABPLO11;
    CORR.T_RABPLO12 = GLOBAL.MAJO_T_RABPLO12;
    CORR.T_RABPLO17 = GLOBAL.MAJO_T_RABPLO17;
    CORR.T_RABPLO31 = GLOBAL.MAJO_T_RABPLO31;
  finsi
  calculer cible enchaine_verification_corr;
  calculer cible signaler_erreurs;
  CORR.FLAG_TRMAJOP = 1;
  CORR.FLAG_RETARD = GLOBAL.RETARD;
  CORR.FLAG_RETARD08 = GLOBAL.RETARD08;
  CORR.FLAG_RETARD07 = GLOBAL.RETARD07;
  CORR.FLAG_RETARD99 = GLOBAL.RETARD99;
  CORR.FLAG_RETARD22 = GLOBAL.RETARD22;
  CORR.FLAG_RETARD101718 = GLOBAL.RETARD101718;
  CORR.FLAG_RETARD0718 = GLOBAL.RETARD0718;
  CORR.IND_RJLJ = GLOBAL.CORR_RJLJ;
  calculer cible init_1731;
  calculer cible enchaine_calcul_corr;
  calculer cible verif_calcul_corrective_corr;
  calculer cible signaler_erreurs;
  calculer cible sauve_base_stratemajo_corr;
  CORR.FLAG_TRMAJOP = 0;
  CORR.FLAG_RETARD = 0;
  CORR.FLAG_RETARD08 = 0;
  calculer cible sauve_base_majo_corr;
finsi

cible cumule_base_tl_aux:
application: iliad;
arguments: M_TL, M_TL_TBTC, R;
variables_temporaires: MONTANT, COMMENCE_PAR_7, CAT_TL;
calculer cible alias_commence_par_7 : avec COMMENCE_PAR_7, champ_evenement(R, code);
si positif(COMMENCE_PAR_7) alors
  MONTANT = champ_evenement(R, montant);
sinon
  MONTANT = champ_evenement(R, base_tl);
finsi
si champ_evenement(R, sens) != SENS_R alors
  MONTANT = -MONTANT;
finsi
CAT_TL = attribut(champ_evenement(R, code), categorie_TL);
si CAT_TL = 40 alors
  M_TL = MONTANT;
  M_TL_TBTC = 0;
sinon_si CAT_TL = 50 alors
  M_TL = 0;
  M_TL_TBTC = MONTANT;
sinon
  M_TL = 0;
  M_TL_TBTC = 0;
finsi

cible cumule_base_tl_init:
application: iliad;
variables_temporaires: R, M_TL, M_TL_TBTC;
R = TMP_ARG1;
si GLOBAL.TL_NON_ACQUISE = TL_TL_MAUVAISE_FOI alors
  M_TL = 0;
  M_TL_TBTC = 0;
sinon
  calculer cible cumule_base_tl_aux : avec M_TL, M_TL_TBTC, R;
finsi
GLOBAL.TL_BASE_TL_INIT = GLOBAL.TL_BASE_TL_INIT + M_TL;
GLOBAL.TL_BASE_TL_TBTC_INIT = GLOBAL.TL_BASE_TL_TBTC_INIT + M_TL_TBTC;

cible cumule_champ_base_tl:
application: iliad;
variables_temporaires: R, M_TL, M_TL_TBTC;
R = TMP_ARG1;
calculer cible cumule_base_tl_aux : avec M_TL, M_TL_TBTC, R;
GLOBAL.TL_BASE_TL = GLOBAL.TL_BASE_TL + M_TL;
GLOBAL.TL_BASE_TL_TBTC = GLOBAL.TL_BASE_TL_TBTC + M_TL_TBTC;

cible cumule_champ_base_tl_rect:
application: iliad;
variables_temporaires: R, M_TL, M_TL_TBTC;
R = TMP_ARG1;
calculer cible cumule_base_tl_aux : avec M_TL, M_TL_TBTC, R;
GLOBAL.TL_BASE_TL_RECT = GLOBAL.TL_BASE_TL_RECT + M_TL;
GLOBAL.TL_BASE_TL_TBTC_RECT = GLOBAL.TL_BASE_TL_TBTC_RECT + M_TL_TBTC;

cible traite_majo_P:
application: iliad;
variables_temporaires: R, NATURE, PENA, COTSOC, MONTANT;
R = TMP_ARG1;
calculer cible get_nature : avec NATURE, champ_evenement(R, code);
PENA = champ_evenement(R, penalite);
COTSOC = attribut(champ_evenement(R, code), cotsoc);
MONTANT = champ_evenement(R, montant);
si
  NATURE = N_REVENU
  et ((7 <= PENA et PENA <= 12) ou PENA = 17 ou PENA = 31)
alors
  si COTSOC = 1 alors
    calculer cible add_majo_T_RABP : avec PENA, MONTANT;
    calculer cible add_majo_T_RABPPS : avec PENA, MONTANT;
    calculer cible add_majo_T_RABPCS : avec PENA, MONTANT;
    calculer cible add_majo_T_RABPRD : avec PENA, MONTANT;
    calculer cible add_majo_T_RABPCH : avec PENA, MONTANT;
  sinon_si COTSOC = 5 alors
    calculer cible add_majo_T_RABP : avec PENA, MONTANT;
    calculer cible add_majo_T_RABPCH : avec PENA, MONTANT;
  sinon_si COTSOC = 9 alors
    calculer cible add_majo_T_RABPCH : avec PENA, MONTANT;
  sinon_si COTSOC = 10 alors
    calculer cible add_majo_T_RABPPS : avec PENA, MONTANT;
    calculer cible add_majo_T_RABPCS : avec PENA, MONTANT;
    calculer cible add_majo_T_RABPRD : avec PENA, MONTANT;
  sinon_si COTSOC dans (11, 12, 13, 14, 19, 20, 21) alors
    calculer cible add_majo_T_RABPRD : avec PENA, MONTANT;
  sinon_si COTSOC = 16 alors
    calculer cible add_majo_T_RABPLO : avec PENA, MONTANT;
  sinon_si COTSOC = 18 alors
    calculer cible add_majo_T_RABP : avec PENA, MONTANT;
    calculer cible add_majo_T_RABPRD : avec PENA, MONTANT;
    calculer cible add_majo_T_RABPCH : avec PENA, MONTANT;
  finsi
finsi

cible calculer_sommes_2042:
application: iliad;
arguments: R;
variables_temporaires: S, N, MONTANT;
S = attribut(champ_evenement(R, code), sanction);
calculer cible get_nature : avec N, champ_evenement(R, code);
si champ_evenement(R, sens) = SENS_R alors
  MONTANT = champ_evenement(R, montant);
sinon
  MONTANT = -champ_evenement(R, montant);
finsi
si S = 1 alors
  GLOBAL.INR_SOMMERF_2 = GLOBAL.INR_SOMMERF_2 + MONTANT;
finsi
si S = 2 ou S = 12 alors
  GLOBAL.INR_SOMMEBA_2 = GLOBAL.INR_SOMMEBA_2 + MONTANT;
finsi
si S = 3 ou S = 13 alors
  GLOBAL.INR_SOMMEBIC_2 = GLOBAL.INR_SOMMEBIC_2 + MONTANT;
finsi
si S = 4 ou S = 14 alors
  GLOBAL.INR_SOMMEBNC_2 = GLOBAL.INR_SOMMEBNC_2 + MONTANT;
finsi
si S = 5 ou S = 15 alors
  GLOBAL.INR_SOMMELOC_2 = GLOBAL.INR_SOMMELOC_2 + MONTANT;
finsi
si S = 6 ou S = 16 alors
  GLOBAL.INR_SOMMERCM_2 = GLOBAL.INR_SOMMERCM_2 + MONTANT;
finsi
si (1 <= S et S <= 8) ou (12 <= S et S <= 16) alors
  GLOBAL.INR_SOMMEMOND_2 = GLOBAL.INR_SOMMEMOND_2 + MONTANT;
finsi
si (1 <= S et S <= 9) ou (12 <= S et S <= 16) alors
  GLOBAL.INR_SOMMERI_2 = GLOBAL.INR_SOMMERI_2 + MONTANT;
finsi
si (1 <= S et S <= 6) ou S = 8 ou (12 <= S et S <= 16) alors
  GLOBAL.INR_SOMMEGLOBAL_2 = GLOBAL.INR_SOMMEGLOBAL_2 + MONTANT;
finsi
si S = 8 et N = N_REVENU alors
  GLOBAL.INR_SOMMEGLOBND_2 = GLOBAL.INR_SOMMEGLOBND_2 + MONTANT;
finsi
si S = 2 et N = N_REVENU alors
  GLOBAL.INR_SOMMEBAND_2 = GLOBAL.INR_SOMMEBAND_2 + MONTANT;
finsi
si S = 3 et N = N_REVENU alors
  GLOBAL.INR_SOMMEBICND_2 = GLOBAL.INR_SOMMEBICND_2 + MONTANT;
finsi
si S = 4 et N = N_REVENU alors
  GLOBAL.INR_SOMMEBNCND_2 = GLOBAL.INR_SOMMEBNCND_2 + MONTANT;
finsi

cible calculer_sommes_2042_proc:
application: iliad;
variables_temporaires: R;
R = TMP_ARG1;
calculer cible calculer_sommes_2042 : avec R;

cible maj_elt_2042:
application: iliad;
arguments: MONTANT_2042, R;
variables_temporaires: NATURE;
si
  # codes_dates
  meme_variable(MONTANT_2042, 0AX)
  ou meme_variable(MONTANT_2042, 0AY)
  ou meme_variable(MONTANT_2042, 0AZ)
  ou meme_variable(MONTANT_2042, 9YD)
  ou meme_variable(MONTANT_2042, 9YR)
alors
  MONTANT_2042 = champ_evenement(R, montant);
sinon
  calculer cible get_nature : avec NATURE, MONTANT_2042;
  si NATURE = N_REVENU alors
    si
      champ_evenement(R, sens) = SENS_R
      et (
        non type(MONTANT_2042, BOOLEEN)
        ou MONTANT_2042 + champ_evenement(R, montant) + 0 dans (0, 1)
      )
    alors
      MONTANT_2042 = MONTANT_2042 + champ_evenement(R, montant);
    sinon_si
      champ_evenement(R, sens) dans (SENS_M, SENS_C)
      et (
        non type(MONTANT_2042, BOOLEEN)
        ou MONTANT_2042 - champ_evenement(R, montant) + 0 dans (0, 1)
      )
    alors
      MONTANT_2042 = MONTANT_2042 - champ_evenement(R, montant);
    finsi
  sinon_si
    champ_evenement(R, sens) dans (SENS_M, SENS_C)
    et (
      non type(MONTANT_2042, BOOLEEN)
      ou MONTANT_2042 + champ_evenement(R, montant) + 0 dans (0, 1)
    )
  alors
    MONTANT_2042 = MONTANT_2042 + champ_evenement(R, montant);
  sinon_si
    non (champ_evenement(R, sens) dans (SENS_M, SENS_C))
    et (
      non type(MONTANT_2042, BOOLEEN)
      ou MONTANT_2042 - champ_evenement(R, montant) + 0 dans (0, 1)
    )
  alors
    MONTANT_2042 = MONTANT_2042 - champ_evenement(R, montant);
  finsi
finsi

cible set_rappel:
application: iliad;
arguments: MONTANT, R;
variables_temporaires: NATURE;
si present(MONTANT) alors
  calculer cible maj_elt_2042 : avec MONTANT, R;
sinon
  calculer cible get_nature : avec NATURE, MONTANT;
  si champ_evenement(R, sens) dans (SENS_M, SENS_C) alors
    si NATURE = N_REVENU alors
      MONTANT = - champ_evenement(R, montant);
    sinon
      MONTANT = champ_evenement(R, montant);
    finsi
  sinon
    si
      NATURE = N_REVENU
      ou (
        # codes_dates
        meme_variable(MONTANT, 0AX)
        ou meme_variable(MONTANT, 0AY)
        ou meme_variable(MONTANT, 0AZ)
        ou meme_variable(MONTANT, 9YD)
        ou meme_variable(MONTANT, 9YR)
      )
    alors
      MONTANT = champ_evenement(R, montant);
    sinon
      MONTANT = - champ_evenement(R, montant);
    finsi
  finsi
finsi

cible met_a_jour_2042_INR_evt:
application: iliad;
arguments: INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
variables_temporaires: RAP_AUTH;
GLOBAL.NB_RAPPELS_RES = 0;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si champ_evenement(R, id_evt) = INDICE_EVT alors
    calculer cible is_rappel_autorise : avec RAP_AUTH, R, MAJ0, MAJ1, MAJ2, MAJ3;
    si positif(RAP_AUTH) alors
      calculer cible set_rappel : avec INR_D2042.champ_evenement(R, code), R : espace GLOBAL;
      si
        non (
          positif(IS_PREMIER)
          et (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
        )
        et GLOBAL.CODE_9ZA = 0
        et champ_evenement(R, sens) != SENS_P
        et (
          # penalites_1731bis
          champ_evenement(R, penalite) dans (3, 4, 5, 6, 8, 11, 30, 31, 32, 35, 55)
        )
      alors
        calculer cible calculer_sommes_2042 : avec R;
      finsi
      GLOBAL.NB_RAPPELS_RES = GLOBAL.NB_RAPPELS_RES + 1;
    finsi
  finsi
)
si GLOBAL.NB_RAPPELS_RES > 0 alors
  CORR.SOMMEBAND_2 = GLOBAL.INR_SOMMEBAND_2;
  CORR.SOMMEBA_2 = GLOBAL.INR_SOMMEBA_2;
  CORR.SOMMEBICND_2 = GLOBAL.INR_SOMMEBICND_2;
  CORR.SOMMEBIC_2 = GLOBAL.INR_SOMMEBIC_2;
  CORR.SOMMEBNCND_2 = GLOBAL.INR_SOMMEBNCND_2;
  CORR.SOMMEBNC_2 = GLOBAL.INR_SOMMEBNC_2;
  CORR.SOMMEGLOBAL_2 = GLOBAL.INR_SOMMEGLOBAL_2;
  CORR.SOMMEGLOBND_2 = GLOBAL.INR_SOMMEGLOBND_2;
  CORR.SOMMELOC_2 = GLOBAL.INR_SOMMELOC_2;
  CORR.SOMMEMOND_2 = GLOBAL.INR_SOMMEMOND_2;
  CORR.SOMMERCM_2 = GLOBAL.INR_SOMMERCM_2;
  CORR.SOMMERF_2 = GLOBAL.INR_SOMMERF_2;
  CORR.SOMMERI_2 = GLOBAL.INR_SOMMERI_2;
finsi

cible met_a_jour_2042_INR_evt_proc:
application: iliad;
variables_temporaires: INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
INDICE_EVT = TMP_ARG1;
IS_PREMIER = TMP_ARG2;
MAJ0 = TMP_ARG3;
MAJ1 = TMP_ARG4;
MAJ2 = TMP_ARG5;
MAJ3 = TMP_ARG6;
calculer cible met_a_jour_2042_INR_evt
: avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

cible met_a_jour_2042_evt:
application: iliad;
arguments: INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3;
variables_temporaires: RAP_AUTH;
GLOBAL.NB_RAPPELS_RES = 0;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si champ_evenement(R, id_evt) = INDICE_EVT alors
    calculer cible is_rappel_autorise
    : avec RAP_AUTH, R, MAJ0, MAJ1, MAJ2, MAJ3
    : espace GLOBAL;
    si positif(RAP_AUTH) alors
      calculer cible set_rappel : avec champ_evenement(R, code), R : espace GLOBAL;
      GLOBAL.TMP_ARG1 = R;
      si F_TRAITEMENT = 1 alors
        calculer cible cumule_champ_base_tl : espace GLOBAL;
      sinon_si F_TRAITEMENT = 2 alors
        calculer cible cumule_base_tl_init : espace GLOBAL;
      sinon_si F_TRAITEMENT = 3 alors
        calculer cible cumule_champ_base_tl_rect : espace GLOBAL;
      sinon_si F_TRAITEMENT = 4 alors
        calculer cible traite_majo_P : espace GLOBAL;
      finsi
      GLOBAL.NB_RAPPELS_RES = GLOBAL.NB_RAPPELS_RES + 1;
    finsi
  finsi
)

cible met_a_jour_2042_evt_TL_D2042:
application: iliad;
variables_temporaires: INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3;
INDICE_EVT = TMP_ARG1;
F_TRAITEMENT = TMP_ARG2;
MAJ0 = TMP_ARG3;
MAJ1 = TMP_ARG4;
MAJ2 = TMP_ARG5;
MAJ3 = TMP_ARG6;
calculer cible met_a_jour_2042_evt
: avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
: espace TL_D2042;

cible met_a_jour_2042_evt_TL_D2042_INIT:
application: iliad;
variables_temporaires: INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3;
INDICE_EVT = TMP_ARG1;
F_TRAITEMENT = TMP_ARG2;
MAJ0 = TMP_ARG3;
MAJ1 = TMP_ARG4;
MAJ2 = TMP_ARG5;
MAJ3 = TMP_ARG6;
calculer cible met_a_jour_2042_evt
: avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
: espace TL_D2042_INIT;

cible met_a_jour_2042_evt_TL_D2042_RECT:
application: iliad;
variables_temporaires: INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3;
INDICE_EVT = TMP_ARG1;
F_TRAITEMENT = TMP_ARG2;
MAJ0 = TMP_ARG3;
MAJ1 = TMP_ARG4;
MAJ2 = TMP_ARG5;
MAJ3 = TMP_ARG6;
calculer cible met_a_jour_2042_evt
: avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
: espace TL_D2042_RECT;

cible met_a_jour_2042_evt_TL_D2042_ABAT:
application: iliad;
variables_temporaires: INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3;
INDICE_EVT = TMP_ARG1;
F_TRAITEMENT = TMP_ARG2;
MAJ0 = TMP_ARG3;
MAJ1 = TMP_ARG4;
MAJ2 = TMP_ARG5;
MAJ3 = TMP_ARG6;
calculer cible met_a_jour_2042_evt
: avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
: espace TL_D2042_ABAT;

cible met_a_jour_2042_evt_TL_D2042_ABAT_INIT:
application: iliad;
variables_temporaires: INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3;
INDICE_EVT = TMP_ARG1;
F_TRAITEMENT = TMP_ARG2;
MAJ0 = TMP_ARG3;
MAJ1 = TMP_ARG4;
MAJ2 = TMP_ARG5;
MAJ3 = TMP_ARG6;
calculer cible met_a_jour_2042_evt
: avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
: espace TL_D2042_ABAT_INIT;

cible met_a_jour_2042_evt_TL_D2042_ABAT_RECT:
application: iliad;
variables_temporaires: INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3;
INDICE_EVT = TMP_ARG1;
F_TRAITEMENT = TMP_ARG2;
MAJ0 = TMP_ARG3;
MAJ1 = TMP_ARG4;
MAJ2 = TMP_ARG5;
MAJ3 = TMP_ARG6;
calculer cible met_a_jour_2042_evt
: avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
: espace TL_D2042_ABAT_RECT;

cible met_a_jour_2042_evt_D2042_RECT:
application: iliad;
variables_temporaires: INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3;
INDICE_EVT = TMP_ARG1;
F_TRAITEMENT = TMP_ARG2;
MAJ0 = TMP_ARG3;
MAJ1 = TMP_ARG4;
MAJ2 = TMP_ARG5;
MAJ3 = TMP_ARG6;
calculer cible met_a_jour_2042_evt
: avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
: espace D2042_RECT;

cible met_a_jour_2042_evt_D2042_ABAT:
application: iliad;
variables_temporaires: INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3;
INDICE_EVT = TMP_ARG1;
F_TRAITEMENT = TMP_ARG2;
MAJ0 = TMP_ARG3;
MAJ1 = TMP_ARG4;
MAJ2 = TMP_ARG5;
MAJ3 = TMP_ARG6;
calculer cible met_a_jour_2042_evt
: avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
: espace D2042_ABAT;

cible met_a_jour_2042_evt_MAJO_D2042_P:
application: iliad;
variables_temporaires: INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3;
INDICE_EVT = TMP_ARG1;
F_TRAITEMENT = TMP_ARG2;
MAJ0 = TMP_ARG3;
MAJ1 = TMP_ARG4;
MAJ2 = TMP_ARG5;
MAJ3 = TMP_ARG6;
calculer cible met_a_jour_2042_evt
: avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
: espace MAJO_D2042_P;

cible met_a_jour_2042_strate:
application: iliad;
arguments: NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3;
variables_temporaires: NUM_PREM_RAPPEL, RAP_AUTH;
NUM_PREM_RAPPEL = champ_evenement(0, numero);
GLOBAL.NB_RAPPELS_RES  = 0;
arranger_evenements
: trier R1, R2
: avec
  si (champ_evenement(R1, strate) != -1 et champ_evenement(R2, strate) = -1) alors (1)
  sinon (si (champ_evenement(R1, strate) = -1 et champ_evenement(R2, strate) != -1) alors (0)
  sinon (si (champ_evenement(R1, strate) < champ_evenement(R2, strate)) alors (1)
  sinon (si (champ_evenement(R1, strate) > champ_evenement(R2, strate)) alors (0)
  sinon (si (champ_evenement(R1, numero) < champ_evenement(R2, numero)) alors (1)
  sinon (si (champ_evenement(R1, numero) > champ_evenement(R2, numero)) alors (0)
  sinon (si (champ_evenement(R1, rappel) < champ_evenement(R2, rappel)) alors (1)
  sinon (si (champ_evenement(R1, rappel) > champ_evenement(R2, rappel)) alors (0)
  sinon (1) finsi) finsi) finsi) finsi) finsi) finsi) finsi) finsi
: dans (
  iterer
  : variable R
  : entre 0..(nb_evenements() - 1) increment 1
  : dans (
    si champ_evenement(R, strate) = NUM_STRATE alors
      calculer cible is_rappel_autorise
      : avec RAP_AUTH, R, MAJ0, MAJ1, MAJ2, MAJ3
      : espace GLOBAL;
      si
        non (
          positif(GLOBAL.RETARD)
          et champ_evenement(R, numero) = NUM_PREM_RAPPEL
        )
        et positif(RAP_AUTH)
      alors
        calculer cible set_rappel : avec champ_evenement(R, code), R : espace GLOBAL;
        GLOBAL.NB_RAPPELS_RES = GLOBAL.NB_RAPPELS_RES + 1;
      finsi
    finsi
  )
)

cible met_a_jour_2042_strate_MAJO_D2042_P:
application: iliad;
variables_temporaires: NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3;
NUM_STRATE = TMP_ARG1;
MAJ0 = TMP_ARG2;
MAJ1 = TMP_ARG3;
MAJ2 = TMP_ARG4;
MAJ3 = TMP_ARG5;
calculer cible met_a_jour_2042_strate
: avec NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3
: espace MAJO_D2042_P;

cible met_a_jour_2042_strate_MAJO_D2042_ABAT_P:
application: iliad;
variables_temporaires: NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3;
NUM_STRATE = TMP_ARG1;
MAJ0 = TMP_ARG2;
MAJ1 = TMP_ARG3;
MAJ2 = TMP_ARG4;
MAJ3 = TMP_ARG5;
calculer cible met_a_jour_2042_strate
: avec NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3
: espace MAJO_D2042_ABAT_P;

cible met_a_jour_2042_strate_MAJO_D2042_STRATE:
application: iliad;
variables_temporaires: NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3;
NUM_STRATE = TMP_ARG1;
MAJ0 = TMP_ARG2;
MAJ1 = TMP_ARG3;
MAJ2 = TMP_ARG4;
MAJ3 = TMP_ARG5;
calculer cible met_a_jour_2042_strate
: avec NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3
: espace MAJO_D2042_STRATE;

cible met_a_jour_2042_strate_MAJO_D2042_ABAT_STRATE:
application: iliad;
variables_temporaires: NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3;
NUM_STRATE = TMP_ARG1;
MAJ0 = TMP_ARG2;
MAJ1 = TMP_ARG3;
MAJ2 = TMP_ARG4;
MAJ3 = TMP_ARG5;
calculer cible met_a_jour_2042_strate
: avec NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3
: espace MAJO_D2042_ABAT_STRATE;

cible set_rappel_1731bis:
application: iliad;
arguments: R, IS_PREMIER;
variables_temporaires: MONTANT, NATURE;
calculer cible get_nature : avec NATURE, champ_evenement(R, code);
si
  attribut(champ_evenement(R, code), sanction) = 9
  et (
    (
      (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
      et GLOBAL.CODE_PENA dans (3, 8, 11)
    )
    ou (
      # penalites_1731bis
      champ_evenement(R, penalite) dans (3, 4, 5, 6, 8, 11, 30, 31, 32, 35, 55)
    )
  )
alors
  CORR.SOMMERI1731 = 1;
finsi
MONTANT = GLOBAL.champ_evenement(R, code) + 0;
si
  (positif(IS_PREMIER) ou champ_evenement(R, sens) = SENS_C)
  et (positif(GLOBAL.RETARD) ou positif(GLOBAL.DEFAUT))
alors
  si
    (
      champ_evenement(R, sens) = SENS_R
      et champ_evenement(R, penalite) dans (
        2, 3, 7, 8, 10, 11, 17, 18, 22, 24, 99
      )
    )
    ou champ_evenement(R, sens) = SENS_M
  alors
    GLOBAL.champ_evenement(R, code) = champ_evenement(R, montant);
  sinon_si champ_evenement(R, sens) = SENS_C et NATURE = N_CHARGE alors
    si MONTANT + champ_evenement(R, montant) >= 0 alors
      GLOBAL.champ_evenement(R, code) = MONTANT + champ_evenement(R, montant);
    finsi
  sinon_si champ_evenement(R, sens) = SENS_C et NATURE = N_REVENU alors
    si MONTANT - champ_evenement(R, montant) >= 0 alors
      GLOBAL.champ_evenement(R, code) = MONTANT - champ_evenement(R, montant);
    finsi
  finsi
finsi
si
  non (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
  ou (
    (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
    et non positif(IS_PREMIER)
    et champ_evenement(R, sens) != SENS_C
  )
alors
  si NATURE = N_REVENU alors
    si champ_evenement(R, sens) dans (SENS_M, SENS_C) alors
      si MONTANT - champ_evenement(R, montant) >= 0 alors
        GLOBAL.champ_evenement(R, code) = MONTANT - champ_evenement(R, montant);
      finsi
    sinon_si
      champ_evenement(R, sens) = SENS_R
      et (
        champ_evenement(R, penalite) dans (2, 7, 10, 17, 18, 22, 24, 99)
        ou (
          non positif(IS_PREMIER)
          et champ_evenement(R, penalite) dans (10, 11)
          et champ_evenement(R, date) = 0
        )
      )
    alors
      GLOBAL.champ_evenement(R, code) = MONTANT + champ_evenement(R, montant);
    finsi
  sinon_si NATURE = N_CHARGE alors
    si champ_evenement(R, sens) dans (SENS_M, SENS_C) alors
      GLOBAL.champ_evenement(R, code) = MONTANT + champ_evenement(R, montant);
    sinon_si
      champ_evenement(R, sens) = SENS_R
      et (
        champ_evenement(R, penalite) dans (
          2, 3, 4, 5, 6, 7, 8, 10, 11, 17, 18, 22, 24, 30, 31, 32, 35, 55, 99
        )
        ou (
          non positif(IS_PREMIER)
          et champ_evenement(R, penalite) dans (10, 11)
          et champ_evenement(R, date) = 0
        )
      )
    alors
      si MONTANT - champ_evenement(R, montant) >= 0 alors
        GLOBAL.champ_evenement(R, code) = MONTANT - champ_evenement(R, montant);
      finsi
    finsi
  finsi
finsi

cible set_rappel_1731bis_proc:
application: iliad;
variables_temporaires: R, IS_PREMIER;
R = TMP_ARG1;
IS_PREMIER = TMP_ARG2;
calculer cible set_rappel_1731bis : avec R, IS_PREMIER;

cible prepare_1731_aux:
application: iliad;
arguments: R, IS_PREMIER, MAJ_TGV_COPIE;
si positif(MAJ_TGV_COPIE) alors
  calculer cible set_rappel_1731bis : avec R, IS_PREMIER;
finsi
si champ_evenement(R, penalite) = 30 alors
  si champ_evenement(R, sens) != SENS_R alors
    GLOBAL.ART1731_SOMME_R3032 = GLOBAL.ART1731_SOMME_R3032 - champ_evenement(R, montant);
  sinon
    CORR.VARR30 = 1;
    GLOBAL.ART1731_SOMME_R3032 = GLOBAL.ART1731_SOMME_R3032 + champ_evenement(R, montant);
    GLOBAL.ART1731_PRESENT_R30 = 1;
  finsi
finsi
si champ_evenement(R, penalite) = 32 alors
  si champ_evenement(R, sens) != SENS_R alors
    GLOBAL.ART1731_SOMME_R3032 = GLOBAL.ART1731_SOMME_R3032 - champ_evenement(R, montant);
  sinon
    CORR.VARR32 = 1;
    GLOBAL.ART1731_SOMME_R3032 = GLOBAL.ART1731_SOMME_R3032 + champ_evenement(R, montant);
    GLOBAL.ART1731_PRESENT_R32 = 1;
  finsi
finsi
si champ_evenement(R, sens) = SENS_R et champ_evenement(R, penalite) = 10 alors
  GLOBAL.ART1731_PRESENT_R10 = 1;
finsi

cible prepare_1731_aux_proc:
application: iliad;
variables_temporaires: R, IS_PREMIER, MAJ_TGV_COPIE;
R = TMP_ARG1;
IS_PREMIER = TMP_ARG2;
MAJ_TGV_COPIE = TMP_ARG3;
calculer cible prepare_1731_aux : avec R, IS_PREMIER, MAJ_TGV_COPIE;

cible prepare_1731_majo_aux:
application: iliad;
arguments: R, IS_PREMIER;
variables_temporaires: NATURE, MAJ_TGV_COPIE;
calculer cible get_nature : avec NATURE, champ_evenement(R, code);
MAJ_TGV_COPIE = (
  champ_evenement(R, sens) dans (SENS_M, SENS_C)
  ou (
    champ_evenement(R, sens) = SENS_R
    et champ_evenement(R, penalite) dans (1, 2, 7, 10, 17, 18, 22, 24, 99)
  )
  ou (
    NATURE = N_CHARGE
    et champ_evenement(R, sens) = SENS_R
    et champ_evenement(R, penalite) dans (
      1, 2, 7, 10, 17, 18, 22, 24, 99, 3,
      4, 5, 6, 8, 11, 30, 31, 32, 35, 55
    )
  )
);
calculer cible prepare_1731_aux : avec R, IS_PREMIER, MAJ_TGV_COPIE;

cible prepare_1731_majo:
application: iliad;
arguments: NUM_STRATE;
variables_temporaires: NUM_EVT_PREMIER, IS_PREMIER, NUM_STRATE_COURANTE;
NUM_EVT_PREMIER = champ_evenement(0, numero);
restaurer
: variable V
: categorie *
: espace GLOBAL
: apres (
  calculer cible empty_art1731;
  si positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD) alors
    iterer
    : variable R
    : entre 0..(nb_evenements() - 1) increment 1
    : dans (
      IS_PREMIER = (champ_evenement(R, numero) = NUM_EVT_PREMIER);
      si positif(IS_PREMIER) ou champ_evenement(R, sens) = SENS_C alors
        IS_PREMIER = (positif(IS_PREMIER) ou champ_evenement(R, sens) = SENS_C);
        calculer cible prepare_1731_majo_aux : avec R, IS_PREMIER;
      finsi
    )
  finsi
  arranger_evenements
  : trier R1, R2
  : avec
    si (champ_evenement(R1, strate) != -1 et champ_evenement(R2, strate) = -1) alors (1)
    sinon (si (champ_evenement(R1, strate) = -1 et champ_evenement(R2, strate) != -1) alors (0)
    sinon (si (champ_evenement(R1, strate) < champ_evenement(R2, strate)) alors (1)
    sinon (si (champ_evenement(R1, strate) > champ_evenement(R2, strate)) alors (0)
    sinon (si (champ_evenement(R1, numero) < champ_evenement(R2, numero)) alors (1)
    sinon (si (champ_evenement(R1, numero) > champ_evenement(R2, numero)) alors (0)
    sinon (si (champ_evenement(R1, rappel) < champ_evenement(R2, rappel)) alors (1)
    sinon (si (champ_evenement(R1, rappel) > champ_evenement(R2, rappel)) alors (0)
    sinon (1) finsi) finsi) finsi) finsi) finsi) finsi) finsi) finsi
  : dans (
    iterer
    : variable R
    : entre 0..(nb_evenements() - 1) increment 1
    : dans (
      NUM_STRATE_COURANTE = champ_evenement(R, strate);
      IS_PREMIER = champ_evenement(R, numero) = NUM_EVT_PREMIER;
      si 
        (
          (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
          et non positif(IS_PREMIER)
          et NUM_STRATE_COURANTE <= NUM_STRATE
          et champ_evenement(R, sens) != SENS_C
        )
        ou (
          non (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
          et NUM_STRATE_COURANTE <= NUM_STRATE
        )
      alors
        calculer cible prepare_1731_majo_aux : avec R, IS_PREMIER;
      finsi
    )
  )
  calculer cible enchaine_calcul_prim;
  calculer cible calcul_1731;
)

cible prepare_1731_majo_proc:
application: iliad;
variables_temporaires: NUM_STRATE;
NUM_STRATE = TMP_ARG1;
calculer cible prepare_1731_majo : avec NUM_STRATE;

cible calcul_inr_aux:
application: iliad;
arguments: IS_PREMIER, INDICE_EVT, FLG_INR;
variables_temporaires: FLAG, NINR;
calculer cible reset_saisie_calc;
calculer cible remplit_tgv_inr_d2042;
CORR.PASS_TLIR = CORR.TL_IR + 0;
CORR.PASS_TLIFI = CORR.TL_IFI + 0;
CORR.PASS_TLTAXAGA = CORR.TL_TAXAGA + 0;
CORR.PASS_TLCDIS = CORR.TL_CDIS + 0;
FLAG = 0;
iterer : variable R : entre 0..(nb_evenements() - 1) increment 1 : dans (
  si
    champ_evenement(R, id_evt) = INDICE_EVT
    et champ_evenement(R, sens) = SENS_C
    et champ_evenement(R, penalite) dans (22, 24)
  alors
    FLAG = 1;
  finsi
)
CORR.FLAG_C22 = FLAG;
FLAG = 0;
iterer : variable R : entre 0..(nb_evenements() - 1) increment 1 : dans (
  si
    champ_evenement(R, id_evt) = INDICE_EVT
    et champ_evenement(R, sens) = SENS_C
    et champ_evenement(R, penalite) = 2
  alors
    FLAG = 1;
  finsi
)
CORR.FLAG_C02 = FLAG;
FLAG = 0;
iterer : variable R : entre 0..(nb_evenements() - 1) increment 1 : dans (
  si champ_evenement(R, penalite) = 99 alors
    FLAG = 1;
  finsi
)
CORR.FLAG_99 = si (positif(FLAG)) alors (4) sinon (0) finsi;
FLAG = 0;
iterer : variable R : entre 0..(nb_evenements() - 1) increment 1 : dans (
  si champ_evenement(R, penalite) = 7 alors
    FLAG = 1;
  finsi
)
CORR.FLAG_07 = si (positif(FLAG)) alors (5) sinon (0) finsi;
FLAG = 0;
iterer : variable R : entre 0..(nb_evenements() - 1) increment 1 : dans (
  si champ_evenement(R, penalite) = 24 alors
    FLAG = 1;
  finsi
)
CORR.FLAG_24 = si (positif(FLAG)) alors (6) sinon (0) finsi;
NINR = 1;
iterer : variable R : entre 0..(nb_evenements() - 1) increment 1 : dans (
  si
    non (
      champ_evenement(R, penalite) = 99
      ou (
        champ_evenement(R, sens) = SENS_C
        et champ_evenement(R, penalite) < 2
      )
      ou (
        champ_evenement(R, sens) = SENS_M
        et non meme_variable(champ_evenement(R, code), REGCO)
      )
    )
  alors
    NINR = 0;
  finsi
)
NINR = ((non positif(GLOBAL.DEFAUT)) et nb_evenements() != 0 et positif(NINR));
CORR.FLAG_NINR = NINR;
CORR.FLAG_RETARD = GLOBAL.RETARD;
CORR.FLAG_RETARD07 = GLOBAL.RETARD07;
CORR.FLAG_RETARD08 = GLOBAL.RETARD08;
CORR.FLAG_RETARD0718 = GLOBAL.RETARD0718;
CORR.FLAG_RETARD101718 = GLOBAL.RETARD101718;
CORR.FLAG_RETARD22 = GLOBAL.RETARD22;
CORR.FLAG_RETARD99 = GLOBAL.RETARD99;
CORR.FLAG_DEFAUT = GLOBAL.DEFAUT;
CORR.FLAG_DEFAUT10 = GLOBAL.DEFAUT10;
CORR.FLAG_DEFAUT11 = GLOBAL.DEFAUT11;
CORR.FLAG_RECTIF = GLOBAL.RECTIF;
CORR.FLAG_RECTIFMAJO = GLOBAL.RECTIF_MAJO;
CORR.FLAG_PRIM = GLOBAL.SF_PRIMITIF;
CORR.FLAG_9YT = GLOBAL.R_TARDIF;
CORR.FLAG_R99 = GLOBAL.SAUVE_INR_R99;
CORR.IND_RJLJ = GLOBAL.CORR_RJLJ;
CORR.CODE_2042 = GLOBAL.CODE_PENA;
CORR.ANNEECOR = GLOBAL.INR_ANNEE_COR;
CORR.NBMOISI = GLOBAL.INR_NB_MOIS;
CORR.NBMOISI2 = GLOBAL.INR_NB_MOIS2;
CORR.NBMOIS2ISF = 0;
CORR.FLAG_INR = FLG_INR;
calculer cible enchaine_calcul_corr;
calculer cible signaler_erreurs;

cible calcul_inr_aux_proc:
application: iliad;
variables_temporaires: IS_PREMIER, INDICE_EVT, FLG_INR;
IS_PREMIER = TMP_ARG1;
INDICE_EVT = TMP_ARG2;
FLG_INR = TMP_ARG3;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, FLG_INR;

cible traite_tl:
application: iliad;
arguments: IS_PREMIER, INDICE_EVT;
variables_temporaires: F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3;
calculer cible mauvaise_foi : avec INDICE_EVT;
GLOBAL.TL_MF_MFCDIS = si (GLOBAL.TL_MF_MFCDIS > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFCHR = si (GLOBAL.TL_MF_MFCHR > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFCHR7 = si (GLOBAL.TL_MF_MFCHR7 > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFCS = si (GLOBAL.TL_MF_MFCS > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFCSAL = si (GLOBAL.TL_MF_MFCSAL > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFCVN = si (GLOBAL.TL_MF_MFCVN > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFGAIN = si (GLOBAL.TL_MF_MFGAIN > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFGLO = si (GLOBAL.TL_MF_MFGLO > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFIFI = si (GLOBAL.TL_MF_MFIFI > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFIR = si (GLOBAL.TL_MF_MFIR > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFLOY = si (GLOBAL.TL_MF_MFLOY > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFMCSG820 = si (GLOBAL.TL_MF_MFMCSG820 > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFPCAP = si (GLOBAL.TL_MF_MFPCAP > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFPS = si (GLOBAL.TL_MF_MFPS > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFPSOL = si (GLOBAL.TL_MF_MFPSOL > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFRD = si (GLOBAL.TL_MF_MFRD > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFREGV = si (GLOBAL.TL_MF_MFREGV > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFRSE1 = si (GLOBAL.TL_MF_MFRSE1 > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFRSE2 = si (GLOBAL.TL_MF_MFRSE2 > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFRSE3 = si (GLOBAL.TL_MF_MFRSE3 > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFRSE4 = si (GLOBAL.TL_MF_MFRSE4 > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFRSE5 = si (GLOBAL.TL_MF_MFRSE5 > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFRSE6 = si (GLOBAL.TL_MF_MFRSE6 > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFRSE7 = si (GLOBAL.TL_MF_MFRSE7 > 0) alors (1) sinon (0) finsi;
GLOBAL.TL_MF_MFTAXAGA = si (GLOBAL.TL_MF_MFTAXAGA > 0) alors (1) sinon (0) finsi;
si
  GLOBAL.TL_MF_MFCDIS > 0 ou GLOBAL.TL_MF_MFCHR > 0 ou GLOBAL.TL_MF_MFCHR7 > 0
  ou GLOBAL.TL_MF_MFCS > 0 ou GLOBAL.TL_MF_MFCSAL > 0 ou GLOBAL.TL_MF_MFCVN > 0
  ou GLOBAL.TL_MF_MFGAIN > 0 ou GLOBAL.TL_MF_MFGLO > 0 ou GLOBAL.TL_MF_MFIFI > 0
  ou GLOBAL.TL_MF_MFIR > 0 ou GLOBAL.TL_MF_MFLOY > 0 ou GLOBAL.TL_MF_MFMCSG820 > 0
  ou GLOBAL.TL_MF_MFPCAP > 0 ou GLOBAL.TL_MF_MFPS > 0 ou GLOBAL.TL_MF_MFPSOL > 0
  ou GLOBAL.TL_MF_MFRD > 0 ou GLOBAL.TL_MF_MFREGV > 0 ou GLOBAL.TL_MF_MFRSE1 > 0
  ou GLOBAL.TL_MF_MFRSE2 > 0 ou GLOBAL.TL_MF_MFRSE3 > 0 ou GLOBAL.TL_MF_MFRSE4 > 0
  ou GLOBAL.TL_MF_MFRSE5 > 0 ou GLOBAL.TL_MF_MFRSE6 > 0 ou GLOBAL.TL_MF_MFRSE7 > 0
  ou GLOBAL.TL_MF_MFTAXAGA > 0
alors
  GLOBAL.TL_NON_ACQUISE = TL_TL_MAUVAISE_FOI;
sinon_si GLOBAL.TL_NON_ACQUISE != TL_TL_DEFAUT_2042 alors
  GLOBAL.TL_NON_ACQUISE = TL_TL_ACQUISE;
finsi
si
  non (
    positif(GLOBAL.DEFAUT)
    ou (positif(GLOBAL.RETARD) et positif(IS_PREMIER))
  )
alors
  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_CUMULE_CHAMP_BASE_TL;
  MAJ0 = MAJ_RAPPEL_R;
  MAJ1 = MAJ_NON_MENTION_EXP;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_CUMULE_CHAMP_BASE_TL;
  MAJ0 = MAJ_RAPPEL_C;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_CUMULE_CHAMP_BASE_TL;
  MAJ0 = MAJ_RAPPEL_CP;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_CUMULE_CHAMP_BASE_TL;
  MAJ0 = MAJ_RAPPEL_CP01;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_CUMULE_CHAMP_BASE_TL;
  MAJ0 = MAJ_RAPPEL_M;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_CUMULE_BASE_TL_INIT;
  MAJ0 = MAJ_NON_TL;
  MAJ1 = MAJ_NON_MENTION_EXP;
  MAJ2 = MAJ_RAPPEL_NON_M;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_INIT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_CUMULE_BASE_TL_INIT;
  MAJ0 = MAJ_TL;
  MAJ1 = MAJ_RAPPEL_C;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_INIT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_CUMULE_BASE_TL_INIT;
  MAJ0 = MAJ_TL;
  MAJ1 = MAJ_MENTION_EXP_99;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_INIT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_CUMULE_CHAMP_BASE_TL_RECT;
  MAJ0 = indefini;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_RECT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_SANS_TRAITEMENT;
  MAJ0 = MAJ_NON_MENTION_EXP;
  MAJ1 = MAJ_ABAT_20;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_ABAT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_SANS_TRAITEMENT;
  MAJ0 = MAJ_RAPPEL_C;
  MAJ1 = MAJ_ABAT_20;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_ABAT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_SANS_TRAITEMENT;
  MAJ0 = MAJ_RAPPEL_CP;
  MAJ1 = MAJ_ABAT_20;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_ABAT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_SANS_TRAITEMENT;
  MAJ0 = MAJ_RAPPEL_M;
  MAJ1 = MAJ_ABAT_20;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_ABAT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_SANS_TRAITEMENT;
  MAJ0 = MAJ_NON_TL;
  MAJ1 = MAJ_NON_MENTION_EXP;
  MAJ2 = MAJ_ABAT_20;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_ABAT_INIT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_SANS_TRAITEMENT;
  MAJ0 = MAJ_RAPPEL_C;
  MAJ1 = MAJ_ABAT_20;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_ABAT_INIT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_SANS_TRAITEMENT;
  MAJ0 = MAJ_NON_TL;
  MAJ1 = MAJ_RAPPEL_CP;
  MAJ2 = MAJ_ABAT_20;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_ABAT_INIT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_SANS_TRAITEMENT;
  MAJ0 = MAJ_RAPPEL_CP01;
  MAJ1 = MAJ_ABAT_20;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_ABAT_INIT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_SANS_TRAITEMENT;
  MAJ0 = MAJ_MENTION_EXP_99;
  MAJ1 = MAJ_ABAT_20;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_ABAT_INIT;

  calculer cible contexte_2042_TL_Ref;
  F_TRAITEMENT = ID_SANS_TRAITEMENT;
  MAJ0 = MAJ_ABAT_20;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, F_TRAITEMENT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace TL_D2042_ABAT_RECT;
finsi

cible traite_tl_proc:
application: iliad;
variables_temporaires: IS_PREMIER, INDICE_EVT;
IS_PREMIER = TMP_ARG1;
INDICE_EVT = TMP_ARG2;
calculer cible traite_tl : avec IS_PREMIER, INDICE_EVT;

cible is_def_ret_not_8_11:
application: iliad;
arguments: RESULTAT, R;
RESULTAT = (
  (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
  et non (
    champ_evenement(R, sens) = SENS_R
    et champ_evenement(R, penalite) dans (8, 11)
  )
);

cible is_code_rappel_13:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: NOT_8_11, EST_SF_NAISS;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
calculer cible est_code_sf_naiss : avec EST_SF_NAISS, champ_evenement(R, code);
RESULTAT = (
  positif(NOT_8_11)
  ou (
    champ_evenement(R, sens) = SENS_R
    et champ_evenement(R, penalite) = 1
    et non positif(EST_SF_NAISS)
  )
);

cible is_code_rappel_12:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: HAS_C22R02, NOT_8_11;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
calculer cible recherche_C22R02 : avec HAS_C22R02, INDICE_EVT;
RESULTAT = (
  positif(NOT_8_11)
  ou (
    non (attribut(champ_evenement(R, code), categorie_TL) dans (10, 15))
    et non (
      (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD)) et positif(IS_PREMIER)
    )
    et positif(HAS_C22R02)
    et champ_evenement(R, sens) dans (SENS_R, SENS_C)
    et champ_evenement(R, penalite) = 24
  )
);

cible is_code_rappel_11:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: HAS_C22R02, NOT_8_11, EST_TAX_INIT;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
calculer cible recherche_C22R02 : avec HAS_C22R02, INDICE_EVT;
calculer cible est_code_tax_init : avec EST_TAX_INIT, champ_evenement(R, code);
RESULTAT = (
  positif(NOT_8_11)
  ou (
    non (attribut(champ_evenement(R, code), categorie_TL) dans (10, 15))
    et non (
      (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD)) et positif(IS_PREMIER)
    )
    et (
      (GLOBAL.CODE_PENA = 22 et positif(EST_TAX_INIT))
      ou (
        champ_evenement(R, penalite) = 22
        et (
          champ_evenement(R, sens) = SENS_C
          ou (
            champ_evenement(R, sens) = SENS_R
            et meme_variable(champ_evenement(R, code), REGCO)
          )
          ou (
            champ_evenement(R, sens) dans (SENS_R, SENS_C)
            et positif(HAS_C22R02)
          )
        )
      )
    )
  )
);

cible is_code_rappel_10:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: HAS_C22R02, NOT_8_11;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
calculer cible recherche_C22R02 : avec HAS_C22R02, INDICE_EVT;
RESULTAT = (
  positif(NOT_8_11)
  ou (
    non (attribut(champ_evenement(R, code), categorie_TL) dans (10, 15))
    et (
      (
        non (
          (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD)) et positif(IS_PREMIER)
        )
        et champ_evenement(R, sens) = SENS_R
        et (
          # is_rappel_2A5
          champ_evenement(R, penalite) dans (2, 3, 4, 5, 30, 32, 35, 55)
        )
      )
      ou (
        positif(HAS_C22R02)
        et champ_evenement(R, sens) dans (SENS_R, SENS_C)
        et champ_evenement(R, penalite) dans (22, 24)
      )
    )
  )
);

cible is_code_rappel_09:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: NOT_8_11;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
RESULTAT = (
  positif(NOT_8_11)
  ou (
    non (attribut(champ_evenement(R, code), categorie_TL) dans (10, 15))
    et (
      (
        champ_evenement(R, sens) = SENS_C
        et champ_evenement(R, penalite) dans (22, 24)
      )
      ou (
        champ_evenement(R, sens) = SENS_R
        et champ_evenement(R, penalite) dans (22, 24)
      )
      ou (
        champ_evenement(R, sens) = SENS_R
        et (
          # is_rappel_2A5
          champ_evenement(R, penalite) dans (2, 3, 4, 5, 30, 32, 35, 55)
        )
      )
    )
  )
);

cible is_code_rappel_08:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: NOT_8_11;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
RESULTAT = (
  positif(NOT_8_11)
  ou (
    non (
      (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD)) et positif(IS_PREMIER)
    )
    et attribut(champ_evenement(R, code), categorie_TL) = 15
    et champ_evenement(R, penalite) = 24
    et (
      champ_evenement(R, sens) dans (SENS_R, SENS_C)
      ou meme_variable(champ_evenement(R, code), REGCO)
    )
  )
);

cible is_code_rappel_07:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: NOT_8_11;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
RESULTAT = (
  positif(NOT_8_11)
  ou (
    non (
      (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD)) et positif(IS_PREMIER)
    )
    et attribut(champ_evenement(R, code), categorie_TL) = 15
    et champ_evenement(R, penalite) = 22
    et (
      champ_evenement(R, sens) dans (SENS_R, SENS_C)
      ou meme_variable(champ_evenement(R, code), REGCO)
    )
  )
);

cible is_code_rappel_06:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: NOT_8_11, HAS_C22R02;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
calculer cible recherche_C22R02 : avec HAS_C22R02, INDICE_EVT;
RESULTAT = (
  positif(NOT_8_11)
  ou (
    attribut(champ_evenement(R, code), categorie_TL) = 10
    et champ_evenement(R, penalite) dans (22, 24)
    et (
      (
        champ_evenement(R, sens) = SENS_C
        et non positif(HAS_C22R02)
      )
      ou champ_evenement(R, sens) = SENS_R
      ou meme_variable(champ_evenement(R, code), REGCO)
    )
  )
);

cible is_code_rappel_05:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: NOT_8_11;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
RESULTAT = (
  positif(NOT_8_11)
  ou (
    attribut(champ_evenement(R, code), categorie_TL) = 15
    et champ_evenement(R, sens) = SENS_R
    et (
      # is_rappel_2A5
      champ_evenement(R, penalite) dans (2, 3, 4, 5, 30, 32, 35, 55)
    )
  )
);

cible is_code_rappel_04:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: HAS_C22R02, NOT_8_11;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
calculer cible recherche_C22R02 : avec HAS_C22R02, INDICE_EVT;
RESULTAT = (
  positif(NOT_8_11)
  ou (
    (
      champ_evenement(R, sens) = SENS_C
      et champ_evenement(R, penalite) < 1
    )
    ou (
      non (
        (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD)) et positif(IS_PREMIER)
      )
      et (
        (
          champ_evenement(R, sens) = SENS_C
          et (
            (
              # is_rappel_1728_X
              champ_evenement(R, penalite) dans (7, 8, 10, 11, 17, 18, 31)
            )
            ou champ_evenement(R, penalite) = 6
            ou (
              # is_rappel_2A5
              champ_evenement(R, penalite) dans (2, 3, 4, 5, 30, 32, 35, 55)
            )
            ou (
              champ_evenement(R, penalite) = 22
              et attribut(champ_evenement(R, code), categorie_TL) = 10
              et positif(HAS_C22R02)
            )
          )
        )
        ou (
          champ_evenement(R, sens) = SENS_R
          et (
            champ_evenement(R, penalite) = 6
            ou (
              (
                # is_rappel_2A5
                champ_evenement(R, penalite) dans (2, 3, 4, 5, 30, 32, 35, 55)
              )
              et attribut(champ_evenement(R, code), categorie_TL) = 10
            )
          )
        )
        ou (
          champ_evenement(R, sens) = SENS_M
          et (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
          et non positif(IS_PREMIER)
        )
      )
    )
  )
);

cible is_code_rappel_03:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: NOT_8_11, EST_TAX_INIT;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
calculer cible est_code_tax_init : avec EST_TAX_INIT, champ_evenement(R, code);
RESULTAT = (
  positif(NOT_8_11)
  ou (
    champ_evenement(R, penalite) = 99
    et non positif(GLOBAL.DEFAUT)
    et GLOBAL.CODE_PENA != 22
    et (
      meme_variable(champ_evenement(R, code), REGCO)
      ou positif(EST_TAX_INIT)
    )
  )
);

cible is_code_rappel_02:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: NOT_8_11, EST_SF_NAISS;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
calculer cible est_code_sf_naiss : avec EST_SF_NAISS, champ_evenement(R, code);
RESULTAT = (
  positif(NOT_8_11)
  ou (
    (
      positif(EST_SF_NAISS)
      et (
        (champ_evenement(R, sens) = SENS_R et champ_evenement(R, penalite) = 1)
        ou (champ_evenement(R, sens) = SENS_M et positif(GLOBAL.MENTION_EXP))
        ou (
          champ_evenement(R, sens) = SENS_M
          et non meme_variable(champ_evenement(R, code), REGCO)
          et GLOBAL.PENALITE_REGCO dans (1, 22, 24, 99)
        )
      )
    )
    ou (
      champ_evenement(R, sens) = SENS_C
      et GLOBAL.CODE_PENA != 22
      et (champ_evenement(R, penalite) < 1 ou champ_evenement(R, penalite) = 99)
    )
    ou (
      champ_evenement(R, sens) = SENS_M
      et non (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
    )
  )
);

cible is_code_rappel_01:
application: iliad;
arguments: RESULTAT, R, INDICE_EVT, IS_PREMIER;
variables_temporaires: NOT_8_11, EST_SF_NAISS, EST_TAX_INIT;
calculer cible is_def_ret_not_8_11 : avec NOT_8_11, R;
calculer cible est_code_sf_naiss : avec EST_SF_NAISS, champ_evenement(R, code);
calculer cible est_code_tax_init : avec EST_TAX_INIT, champ_evenement(R, code);
RESULTAT = (
  positif(NOT_8_11)
  ou (
    non positif(IS_PREMIER)
    et (
      (
        champ_evenement(R, sens) = SENS_R
        et champ_evenement(R, penalite) = 1
        et positif(EST_SF_NAISS)
      )
      ou (
        champ_evenement(R, sens) = SENS_M
        et positif(GLOBAL.MENTION_EXP)
        et positif(EST_SF_NAISS)
      )
      ou (
        champ_evenement(R, penalite) = 99
        et non positif(GLOBAL.DEFAUT)
        et GLOBAL.CODE_PENA != 22
        et (
          meme_variable(champ_evenement(R, code), REGCO)
          ou positif(EST_TAX_INIT)
        )
      )
    )
  )
);

cible prepare_1731_inr:
application: iliad;
arguments: IS_PREMIER, INDICE_EVT, CAS_INR;
variables_temporaires: NUM_PREM_RAPPEL_EVT, NUM_EVT_PREMIER, PREM_EVT, IS_CODE, MAJ_TGV_COPIE;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si non present(NUM_PREM_RAPPEL_EVT) et champ_evenement(R, id_evt) = INDICE_EVT alors
    NUM_PREM_RAPPEL_EVT = champ_evenement(R, numero);
  finsi
)
NUM_EVT_PREMIER = champ_evenement(0, numero);
restaurer : variable V : categorie * : espace GLOBAL : apres (
  calculer cible empty_art1731;
  iterer
  : variable R
  : entre 0..(nb_evenements() - 1) increment 1
  : dans (
    si champ_evenement(R, numero) <= NUM_PREM_RAPPEL_EVT alors
      PREM_EVT = (champ_evenement(R, numero) = NUM_EVT_PREMIER);
      IS_CODE = 0;
      MAJ_TGV_COPIE = 0;
      calculer cible is_code_rappel_13 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 13 et positif(IS_CODE)));
      calculer cible is_code_rappel_12 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 12 et positif(IS_CODE)));
      calculer cible is_code_rappel_11 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 11 et positif(IS_CODE)));
      calculer cible is_code_rappel_10 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 10 et positif(IS_CODE)));
      calculer cible is_code_rappel_09 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 9 et positif(IS_CODE)));
      calculer cible is_code_rappel_08 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 8 et positif(IS_CODE)));
      calculer cible is_code_rappel_07 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 7 et positif(IS_CODE)));
      calculer cible is_code_rappel_06 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 6 et positif(IS_CODE)));
      calculer cible is_code_rappel_05 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 5 et positif(IS_CODE)));
      calculer cible is_code_rappel_04 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 4 et positif(IS_CODE)));
      calculer cible is_code_rappel_03 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 3 et positif(IS_CODE)));
      calculer cible is_code_rappel_02 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 2 et positif(IS_CODE)));
      calculer cible is_code_rappel_01 : avec IS_CODE, R, INDICE_EVT, IS_PREMIER;
      MAJ_TGV_COPIE = (positif(MAJ_TGV_COPIE) ou (CAS_INR >= 1 et positif(IS_CODE)));
      calculer cible prepare_1731_aux : avec R, PREM_EVT, MAJ_TGV_COPIE;
    finsi
  )
  calculer cible enchaine_calcul_prim;
  calculer cible calcul_1731;
)

cible prepare_1731_inr_proc:
application: iliad;
variables_temporaires: IS_PREMIER, INDICE_EVT, CAS_INR;
IS_PREMIER = TMP_ARG1;
INDICE_EVT = TMP_ARG2;
CAS_INR = TMP_ARG3;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;

cible calcul_inr:
application: iliad;
arguments: IS_PREMIER, INDICE_EVT;
variables_temporaires: DEFAUT_RETARD_PREMIER, HAS_C22R02, MAJ0, MAJ1, MAJ2, MAJ3, INR_FLAG, CAS_INR;
DEFAUT_RETARD_PREMIER = (
  (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD)) et positif(IS_PREMIER)
);
calculer cible recherche_C22R02 : avec HAS_C22R02, INDICE_EVT;
iterer
: variable VAR
: categorie saisie *
: espace INR_D2042
: dans (
  INR_D2042_PROV_ANT.VAR = VAR;
)
si INDICE_EVT < 2 alors
  si positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD) alors
    iterer
    : variable VAR
    : categorie saisie *
    : espace INR_D2042_R9901_ANT
    : dans (
      INR_D2042.VAR = VAR;
    )
  finsi

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_F;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  si positif(GLOBAL.MENTION_EXP) alors
    calculer cible contexte_2042_INR;
    MAJ0 = MAJ_RAPPEL_MF;
    MAJ1 = indefini;
    MAJ2 = indefini;
    MAJ3 = indefini;
    calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
  finsi

  si non (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD)) alors
    calculer cible contexte_2042_INR;
    MAJ0 = MAJ_RAPPEL_C;
    MAJ1 = indefini;
    MAJ2 = indefini;
    MAJ3 = indefini;
    calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
  finsi

  si non (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD)) alors
    calculer cible contexte_2042_INR;
    MAJ0 = MAJ_MENTION_EXP_99;
    MAJ1 = indefini;
    MAJ2 = indefini;
    MAJ3 = indefini;
    calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
  finsi
  INR_FLAG = INR_FLAG_INR_NON_TL;
  calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
  calculer cible sauve_base_inr_r9901_corr;
finsi
iterer
: variable VAR
: categorie saisie *
: espace INR_D2042
: dans (
  INR_D2042_R9901_ANT.VAR = VAR;
)
iterer
: variable VAR
: categorie saisie *
: espace INR_D2042_PROV_ANT
: dans (
  INR_D2042.VAR = VAR;
)
si non positif(IS_PREMIER) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_F;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  si positif(GLOBAL.MENTION_EXP) alors
    calculer cible contexte_2042_INR;
    MAJ0 = MAJ_RAPPEL_MF;
    MAJ1 = indefini;
    MAJ2 = indefini;
    MAJ3 = indefini;
    calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
  finsi

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_C;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_MENTION_EXP_99R;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_MENTION_EXP_99;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
finsi
CAS_INR = 1;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
CORR.REF1731 = 1;
INR_FLAG = INR_FLAG_INR_NON_TL;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
calculer cible sauve_base_HR_corr;
CORR.REF1731 = 0;
iterer
: variable VAR
: categorie saisie *
: espace INR_D2042_PROV_ANT
: dans (
  INR_D2042.VAR = VAR;
)
si non positif(IS_PREMIER) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_F;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  si positif(GLOBAL.MENTION_EXP) alors
    calculer cible contexte_2042_INR;
    MAJ0 = MAJ_RAPPEL_MF;
    MAJ1 = indefini;
    MAJ2 = indefini;
    MAJ3 = indefini;
    calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
  finsi

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_C;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_MENTION_EXP_99;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
finsi
iterer
: variable VAR
: categorie saisie *
: espace INR_D2042
: dans (
  INR_D2042_REFR99R_ANT.VAR = VAR;
)

si positif(GLOBAL.MENTION_EXP) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_MF;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
finsi

si
  non (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
  et positif(IS_PREMIER)
alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_C;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
finsi

si
  non (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
  et positif(IS_PREMIER)
alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_MENTION_EXP_99;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
finsi

calculer cible contexte_2042_INR;
MAJ0 = MAJ_RAPPEL_R55;
MAJ1 = indefini;
MAJ2 = indefini;
MAJ3 = indefini;
calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

si  non (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD)) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_M;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
finsi

CAS_INR = 2;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
CORR.REF1731 = 1;
INR_FLAG = INR_FLAG_INR_NON_TL;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
calculer cible sauve_base_inr_ref_corr;
CORR.REF1731 = 0;
si non positif(IS_PREMIER) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_MENTION_EXP_99R;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  CAS_INR = 3;
  calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
  INR_FLAG = INR_FLAG_INR_NON_TL;
  calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
  calculer cible sauve_base_inr_ntl_corr;
finsi
si non positif(DEFAUT_RETARD_PREMIER) et positif(HAS_C22R02) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_NON_TL;
  MAJ1 = MAJ_RAPPEL_CP22;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
  INR_FLAG = INR_FLAG_INR_NON_TL;
  calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
  calculer cible sauve_base_abat98_corr;
finsi
si non positif(DEFAUT_RETARD_PREMIER) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_MF;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_CP;
  MAJ1 = MAJ_1728;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_CP;
  MAJ1 = MAJ_CODE_1729_2A5;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_CP;
  MAJ1 = MAJ_CODE_1729_6;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_R;
  MAJ1 = MAJ_1728;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_NON_TL;
  MAJ1 = MAJ_RAPPEL_R;
  MAJ2 = MAJ_CODE_1729_2A5;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_R;
  MAJ1 = MAJ_CODE_1729_6;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
finsi
si (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD)) et non positif(IS_PREMIER) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_RAPPEL_M;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
finsi
CAS_INR = 4;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
INR_FLAG = INR_FLAG_INR_NON_TL;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
calculer cible sauve_base_inr_ntl_corr;

si non positif(DEFAUT_RETARD_PREMIER) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_NON_TL15;
  MAJ1 = MAJ_RAPPEL_R;
  MAJ2 = MAJ_CODE_1729_2A5;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
finsi

CAS_INR = 5;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
INR_FLAG = INR_FLAG_INR_NON_TL;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
calculer cible sauve_base_inr_intertl_corr;

si non positif(DEFAUT_RETARD_PREMIER) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_NON_TL;
  MAJ1 = MAJ_CONTEXTE_22;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_NON_TL;
  MAJ1 = MAJ_CODE_22;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_NON_TL;
  MAJ1 = MAJ_CODE_24;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  si non positif(HAS_C22R02) alors
    calculer cible contexte_2042_INR;
    MAJ0 = MAJ_NON_TL;
    MAJ1 = MAJ_RAPPEL_CP22;
    MAJ2 = indefini;
    MAJ3 = indefini;
    calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
  finsi

  si non positif(HAS_C22R02) alors
    calculer cible contexte_2042_INR;
    MAJ0 = MAJ_NON_TL;
    MAJ1 = MAJ_RAPPEL_CP24;
    MAJ2 = indefini;
    MAJ3 = indefini;
    calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
  finsi
finsi

CAS_INR = 6;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
INR_FLAG = INR_FLAG_INR_NON_TL;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
calculer cible sauve_base_inr_ntl22_corr;

si non positif(DEFAUT_RETARD_PREMIER) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_NON_TL15;
  MAJ1 = MAJ_CODE_22;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_NON_TL15;
  MAJ1 = MAJ_RAPPEL_CP22;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
finsi
CAS_INR = 7;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
INR_FLAG = INR_FLAG_INR_NON_TL;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
calculer cible sauve_base_inr_corr;

calculer cible contexte_2042_INR;
MAJ0 = MAJ_NON_TL15;
MAJ1 = MAJ_CODE_24;
MAJ2 = indefini;
MAJ3 = indefini;
calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

calculer cible contexte_2042_INR;
MAJ0 = MAJ_NON_TL15;
MAJ1 = MAJ_RAPPEL_CP24;
MAJ2 = indefini;
MAJ3 = indefini;
calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

CAS_INR = 8;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
INR_FLAG = INR_FLAG_INR_NON_TL;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
calculer cible sauve_base_inr_ntl24_corr;

CAS_INR = 9;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
INR_FLAG = INR_FLAG_INR_TL;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
calculer cible sauve_base_inr_tl_corr;

si non positif(DEFAUT_RETARD_PREMIER) et positif(HAS_C22R02) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_TL;
  MAJ1 = MAJ_TL15;
  MAJ2 = MAJ_RAPPEL_R;
  MAJ3 = MAJ_CODE_22;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_TL;
  MAJ1 = MAJ_TL15;
  MAJ2 = MAJ_RAPPEL_CP22;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  INR_FLAG = INR_FLAG_INR_TL;
  calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
  calculer cible sauve_base_abat99_corr;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_TL;
  MAJ1 = MAJ_TL15;
  MAJ2 = MAJ_RAPPEL_R;
  MAJ3 = MAJ_CODE_24;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_TL;
  MAJ1 = MAJ_TL15;
  MAJ2 = MAJ_RAPPEL_CP24;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  INR_FLAG = INR_FLAG_INR_TL;
  calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
  calculer cible sauve_base_abat99_corr;
finsi
si non positif(DEFAUT_RETARD_PREMIER) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_TL;
  MAJ1 = MAJ_TL15;
  MAJ2 = MAJ_RAPPEL_R;
  MAJ3 = MAJ_CODE_1729_2A5;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
finsi

CAS_INR = 10;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
INR_FLAG = INR_FLAG_INR_TL;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
calculer cible sauve_base_inr_tl_corr;

si non positif(DEFAUT_RETARD_PREMIER) alors
  calculer cible contexte_2042_INR;
  MAJ0 = MAJ_TL;
  MAJ1 = MAJ_TL15;
  MAJ2 = MAJ_CONTEXTE_22;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

  si non positif(HAS_C22R02) alors
    calculer cible contexte_2042_INR;
    MAJ0 = MAJ_TL;
    MAJ1 = MAJ_TL15;
    MAJ2 = MAJ_RAPPEL_R;
    MAJ3 = MAJ_CODE_22;
    calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
  finsi

  si non positif(HAS_C22R02) alors
    calculer cible contexte_2042_INR;
    MAJ0 = MAJ_TL;
    MAJ1 = MAJ_TL15;
    MAJ2 = MAJ_RAPPEL_CP22;
    MAJ3 = indefini;
    calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
  finsi
finsi

CAS_INR = 11;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
INR_FLAG = INR_FLAG_INR_TL;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
calculer cible sauve_base_inr_tl22_corr;

si non positif(DEFAUT_RETARD_PREMIER) alors
  si non positif(HAS_C22R02) alors
    calculer cible contexte_2042_INR;
    MAJ0 = MAJ_TL;
    MAJ1 = MAJ_TL15;
    MAJ2 = MAJ_RAPPEL_R;
    MAJ3 = MAJ_CODE_24;
    calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
  finsi

  si non positif(HAS_C22R02) alors
    calculer cible contexte_2042_INR;
    MAJ0 = MAJ_TL;
    MAJ1 = MAJ_TL15;
    MAJ2 = MAJ_RAPPEL_CP24;
    MAJ3 = indefini;
    calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;
  finsi
finsi

CAS_INR = 12;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
INR_FLAG = INR_FLAG_INR_TL;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
calculer cible sauve_base_inr_tl24_corr;

calculer cible contexte_2042_INR;
MAJ0 = MAJ_RAPPEL_NF;
MAJ1 = indefini;
MAJ2 = indefini;
MAJ3 = indefini;
calculer cible met_a_jour_2042_INR_evt : avec INDICE_EVT, IS_PREMIER, MAJ0, MAJ1, MAJ2, MAJ3;

CAS_INR = 13;
calculer cible prepare_1731_inr : avec IS_PREMIER, INDICE_EVT, CAS_INR;
INR_FLAG = INR_FLAG_INR_TL;
calculer cible calcul_inr_aux : avec IS_PREMIER, INDICE_EVT, INR_FLAG;
calculer cible sauve_base_inr_inter22_corr;

cible calcul_inr_proc:
application: iliad;
variables_temporaires: IS_PREMIER, INDICE_EVT;
IS_PREMIER = TMP_ARG1;
INDICE_EVT = TMP_ARG2;
calculer cible calcul_inr : avec IS_PREMIER, INDICE_EVT;

cible traite_reference:
application: iliad;
arguments: IS_PREMIER, INDICE_EVT;
variables_temporaires: ID_TRAIT, MAJ0, MAJ1, MAJ2, MAJ3;
si
  non (
    (
      positif(GLOBAL.DEFAUT) # positif(GLOBAL.RETARD)
      ou positif(GLOBAL.RETARD)
    )
    et positif(IS_PREMIER)
  )
alors
  calculer cible contexte_2042_TL_Ref;
  ID_TRAIT = ID_SANS_TRAITEMENT;
  MAJ0 = indefini;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, ID_TRAIT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace D2042_RECT;

  calculer cible contexte_2042_TL_Ref;
  ID_TRAIT = ID_SANS_TRAITEMENT;
  MAJ0 = MAJ_ABAT_20;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, ID_TRAIT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace D2042_ABAT;
finsi

cible traite_reference_proc:
application: iliad;
variables_temporaires: IS_PREMIER, INDICE_EVT;
IS_PREMIER = TMP_ARG1;
INDICE_EVT = TMP_ARG2;
calculer cible traite_reference : avec IS_PREMIER, INDICE_EVT;

cible calcul_majo_normal:
application: iliad;
arguments:
  IS_PREMIER, INDICE_EVT, NUM_STRATE,
  NB_RAPPELS_STRATE, PROCHAINE_STRATE;
variables_temporaires: TAUX_PENALITE;
TAUX_PENALITE = get_strate_taux(NUM_STRATE);
si TAUX_PENALITE = -1 alors
  TAUX_PENALITE = GLOBAL.MAJO_TAUX_STRATE;
finsi
si NUM_STRATE = 0 alors
  GLOBAL.MAJO_DERN_STR_TR = 0;
  GLOBAL.MAJO_PREM_STR = 1;
  GLOBAL.MAJO_NB_STR_TR = 0;
finsi
si
  (NB_RAPPELS_STRATE != 0 et GLOBAL.MAJO_D2042_STRATE_NB > 0)
  ou NUM_STRATE = GLOBAL.NB_STRATES - 1
alors
  # if taux_penalite < 0 then failwith "Taux pénalite négatif";
  calculer cible reset_saisie_calc;
  CORR.FLAG_1STRATE = GLOBAL.MAJO_PREM_STR;
  calculer cible remplit_tgv_majo_d2042_strate;
  CORR.PASS_TLIR = CORR.TL_IR + 0;
  CORR.PASS_TLIFI = CORR.TL_IFI + 0;
  CORR.PASS_TLCS = CORR.TL_CS + 0;
  CORR.PASS_TLTAXAGA = CORR.TL_TAXAGA + 0;
  CORR.PASS_TLCDIS = CORR.TL_CDIS + 0;
  calculer cible set_majo_str_tr : avec NUM_STRATE;
  CORR.X = NUM_STRATE;
  CORR.TAUX_STRATE = TAUX_PENALITE;
  CORR.FLAG_NBSTRTR = GLOBAL.MAJO_NB_STR_TR;
  CORR.FLAG_DERSTTR = GLOBAL.MAJO_DERN_STR_TR;
  CORR.CSTRATE1 = GLOBAL.MAJO_CODE_STRATE;
  CORR.FLAG_RETARD = GLOBAL.RETARD;
  CORR.FLAG_RETARD07 = GLOBAL.RETARD07;
  CORR.FLAG_RETARD08 = GLOBAL.RETARD08;
  CORR.FLAG_RETARD0718 = GLOBAL.RETARD0718;
  CORR.FLAG_RETARD101718 = GLOBAL.RETARD101718;
  CORR.FLAG_RETARD22 = GLOBAL.RETARD22;
  CORR.FLAG_RETARD99 = GLOBAL.RETARD99;
  CORR.FLAG_DEFAUT = GLOBAL.DEFAUT;
  CORR.FLAG_DEFAUT10 = GLOBAL.DEFAUT10;
  CORR.FLAG_DEFAUT11 = GLOBAL.DEFAUT11;
  CORR.FLAG_RECTIF = GLOBAL.RECTIF;
  CORR.FLAG_RECTIFMAJO = GLOBAL.RECTIF_MAJO;
  CORR.FLAG_PRIM = GLOBAL.SF_PRIMITIF;
  CORR.IND_RJLJ = GLOBAL.CORR_RJLJ;
  CORR.TARDIFEVT2 = GLOBAL.MAJO_TARDIF_EVT2;
  si positif(GLOBAL.RETARD) et GLOBAL.CODE_PENA dans (2, 3, 7, 8, 9, 17, 22) alors
    CORR.CSTRATE99 = GLOBAL.CODE_PENA;
  finsi
  CORR.CODE_2042 = GLOBAL.CODE_PENA;
  CORR.TAUX_2042 = get_taux_penalite(GLOBAL.CODE_PENA);
  calculer cible detecte_penalites;
  si NUM_STRATE = 1 alors
    calculer cible unset_majo_str_tr : avec NUM_STRATE;
    calculer cible affect_code;
  finsi
  calculer cible affect_str_tr;
  si
    NUM_STRATE = GLOBAL.NB_STRATES - 1
    et positif(GLOBAL.RETARD)
    et GLOBAL.CODE_PENA dans (2, 3, 7, 8, 9, 17, 22)
  alors
    CORR.FLAG_TRDEGTR = GLOBAL.CODE_PENA;
  sinon
    CORR.FLAG_TRDEGTR = 0;
  finsi
  calculer cible init_1731;
  calculer cible prepare_1731_majo : avec PROCHAINE_STRATE;
  calculer cible enchaine_calcul_corr;
  si NUM_STRATE = GLOBAL.NB_STRATES - 1 alors
    calculer cible verif_calcul_corrective_corr;
    calculer cible signaler_erreurs;
  finsi
  calculer cible sauve_base_stratemajo_corr;
  CORR.FLAG_RETARD = 0;
  CORR.FLAG_RETARD08 = 0;
  GLOBAL.MAJO_DERN_STR_TR = NUM_STRATE;
  GLOBAL.MAJO_PREM_STR = 0;
  GLOBAL.MAJO_NB_STR_TR = GLOBAL.MAJO_NB_STR_TR + 1;
finsi
si NUM_STRATE = GLOBAL.NB_STRATES - 1 alors
  GLOBAL.MAJO_DERN_STR_TR = 0;
  calculer cible enchaine_verification_corr;
  calculer cible signaler_erreurs;
finsi

cible calcul_majo_normal_proc:
application: iliad;
variables_temporaires:
  IS_PREMIER, INDICE_EVT, NUM_STRATE,
  NB_RAPPELS_STRATE, PROCHAINE_STRATE;
IS_PREMIER = TMP_ARG1;
INDICE_EVT = TMP_ARG2;
NUM_STRATE = TMP_ARG3;
NB_RAPPELS_STRATE = TMP_ARG4;
PROCHAINE_STRATE = TMP_ARG5;
calculer cible calcul_majo_normal
: avec IS_PREMIER, INDICE_EVT, NUM_STRATE, NB_RAPPELS_STRATE, PROCHAINE_STRATE;

cible calcul_majo_tardif:
application: iliad;
arguments: IS_PREMIER, INDICE_EVT, NUM_STRATE, TAUX_PENALITE;
variables_temporaires: STRATE_0;
si GLOBAL.D2042_NB > 0 alors
  calculer cible reset_saisie_calc;
  calculer cible init_1731;
  STRATE_0 = 0;
  calculer cible prepare_1731_majo : avec STRATE_0;
  calculer cible remplit_tgv_d2042;
  CORR.PASS_TLIR = CORR.TL_IR + 0;
  CORR.PASS_TLIFI = CORR.TL_IFI + 0;
  CORR.PASS_TLCS = CORR.TL_CS + 0;
  CORR.PASS_TLTAXAGA = CORR.TL_TAXAGA + 0;
  CORR.PASS_TLCDIS = CORR.TL_CDIS + 0;
  CORR.TAUX_2042 = TAUX_PENALITE;
  CORR.CODE_2042 = GLOBAL.CODE_PENA;
  CORR.FLAG_RETARD = GLOBAL.RETARD;
  CORR.FLAG_RETARD08 = GLOBAL.RETARD08;
  CORR.FLAG_RETARD07 = GLOBAL.RETARD07;
  CORR.FLAG_RETARD99 = GLOBAL.RETARD99;
  CORR.FLAG_RETARD22 = GLOBAL.RETARD22;
  CORR.FLAG_RETARD101718 = GLOBAL.RETARD101718;
  CORR.FLAG_RETARD0718 = GLOBAL.RETARD0718;
  CORR.IND_RJLJ = GLOBAL.CORR_RJLJ;
  CORR.FLAG_TRTARDIF = 1;
  CORR.FLAG_1STRATE = 0;
  CORR.FLAG_DERSTTR = 0;
  calculer cible affect_code;
  calculer cible affect_str_tr;
  calculer cible detecte_penalites;
  calculer cible enchaine_verification_corr;
  calculer cible signaler_erreurs;
  calculer cible enchaine_calcul_corr;
  calculer cible verif_calcul_corrective_corr;
  calculer cible signaler_erreurs;
  calculer cible sauve_base_stratemajo_corr;
  calculer cible sauve_base_anterieure_corr;
  CORR.FLAG_TRTARDIF = 0;
  CORR.FLAG_RETARD = 0;
  CORR.FLAG_RETARD08 = 0;
finsi
calculer cible sauve_base_anterieure_corr;

cible calcul_majo_tardif_proc:
application: iliad;
variables_temporaires: IS_PREMIER, INDICE_EVT, NUM_STRATE, TAUX_PENALITE;
IS_PREMIER = TMP_ARG1;
INDICE_EVT = TMP_ARG2;
NUM_STRATE = TMP_ARG3;
TAUX_PENALITE = TMP_ARG4;
calculer cible calcul_majo_tardif
: avec IS_PREMIER, INDICE_EVT, NUM_STRATE, TAUX_PENALITE;

cible calcul_majo:
application: iliad;
arguments: IS_PREMIER, INDICE_EVT;
variables_temporaires:
  NUM_STRATE, TAUX_PENALITE, ID_TRAIT, MAJ0, MAJ1, MAJ2, MAJ3,
  NB_RAPPELS_P, NB_RAPPELS_STRATE, PROCHAINE_STRATE;
si non positif(GLOBAL.RETARD) alors
  calculer cible clear_majo_str_tr;
finsi
calculer cible remplit_tgv_d2042_rect;
calculer cible enchaine_calcul_corr;
si present(CORR.IRBASE) alors
  CORR.IRBASE2042_FIC = CORR.IRBASE;
finsi
si present(CORR.IRBASE_I) alors
  CORR.IRBASE_IRECT = CORR.IRBASE_I;
finsi
si present(CORR.CSBASE_MAJO) alors
  CORR.CSBASE2042_FIC = CORR.CSBASE_MAJO;
finsi
si present(CORR.RDBASE_MAJO) alors
  CORR.RDBASE2042_FIC = CORR.RDBASE_MAJO;
finsi
si present(CORR.PSBASE_MAJO) alors
  CORR.PSBASE2042_FIC = CORR.PSBASE_MAJO;
finsi
si present(CORR.TAXABASE_MAJO) alors
  CORR.TAXABASE2042_FIC = CORR.TAXABASE_MAJO;
finsi
si present(CORR.TAXABASE_I) alors
  CORR.TAXABASE_IRECT = CORR.TAXABASE_I;
finsi
si present(CORR.CSALBASE_MAJO) alors
  CORR.CSALBASE2042_FIC = CORR.CSALBASE_MAJO;
finsi
si present(CORR.CDISBASE_MAJO) alors
  CORR.CDISBASE2042_FIC = CORR.CDISBASE_MAJO;
finsi
si present(CORR.CAPBASE_MAJO) alors
  CORR.CAPBASE2042_FIC = CORR.CAPBASE_MAJO;
finsi
si present(CORR.CAPBASE_I) alors
  CORR.CAPBASE_IRECT = CORR.CAPBASE_I;
finsi
si present(CORR.HRBASE_MAJO) alors
  CORR.HRBASE2042_FIC = CORR.HRBASE_MAJO;
finsi
si present(CORR.HRBASE_I) alors
  CORR.HRBASE_IRECT = CORR.HRBASE_I;
finsi
si present(CORR.GAINBASE_MAJO) alors
  CORR.GAINBASE2042_FIC = CORR.GAINBASE_MAJO;
finsi
si present(CORR.RSE1BASE_MAJO) alors
  CORR.RSE1BASE2042_FIC = CORR.RSE1BASE_MAJO;
finsi
si present(CORR.RSE2BASE_MAJO) alors
  CORR.RSE2BASE2042_FIC = CORR.RSE2BASE_MAJO;
finsi
si present(CORR.RSE3BASE_MAJO) alors
  CORR.RSE3BASE2042_FIC = CORR.RSE3BASE_MAJO;
finsi
si present(CORR.RSE4BASE_MAJO) alors
  CORR.RSE4BASE2042_FIC = CORR.RSE4BASE_MAJO;
finsi
si present(CORR.RSE5BASE_MAJO) alors
  CORR.RSE5BASE2042_FIC = CORR.RSE5BASE_MAJO;
finsi
si present(CORR.RSE6BASE_MAJO) alors
  CORR.RSE6BASE2042_FIC = CORR.RSE6BASE_MAJO;
finsi
si present(CORR.RSE8BASE_MAJO) alors
  CORR.RSE8BASE2042_FIC = CORR.RSE8BASE_MAJO;
finsi
si present(CORR.CVNBASE_MAJO) alors
  CORR.CVNBASE2042_FIC = CORR.CVNBASE_MAJO;
finsi
si present(CORR.GLOBASE_MAJO) alors
  CORR.GLOBASE2042_FIC = CORR.GLOBASE_MAJO;
finsi
si present(CORR.PSOLBASE_MAJO) alors
  CORR.PSOLBASE2042_FIC = CORR.PSOLBASE_MAJO;
finsi
si present(CORR.C820BASE_MAJO) alors
  CORR.C820BASE2042_FIC = CORR.C820BASE_MAJO;
finsi
si present(CORR.IFI4BASE) alors
  CORR.IFI4BASE2042_FIC = CORR.IFI4BASE;
finsi
si
  positif(IS_PREMIER)
  et (non positif(GLOBAL.DEFAUT))
  et positif(GLOBAL.RETARD)
alors
  TAUX_PENALITE = get_taux_penalite(GLOBAL.CODE_PENA);
  NUM_STRATE = GLOBAL.MAJO_NUM_STRATE;
  calculer cible calcul_majo_tardif
  : avec IS_PREMIER, INDICE_EVT, NUM_STRATE, TAUX_PENALITE;
sinon
  ID_TRAIT = ID_TRAITE_MAJO_P;
  MAJ0 = MAJ_RAPPEL_P;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_evt
  : avec INDICE_EVT, ID_TRAIT, MAJ0, MAJ1, MAJ2, MAJ3
  : espace MAJO_D2042_P;

  NB_RAPPELS_P = GLOBAL.NB_RAPPELS_RES;

  NUM_STRATE = 0;
  MAJ0 = indefini;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_strate
  : avec NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3
  : espace MAJO_D2042_P;

  NUM_STRATE = 1;
  MAJ0 = indefini;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_strate
  : avec NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3
  : espace MAJO_D2042_P;

  NUM_STRATE = 0;
  MAJ0 = MAJ_ABAT_20;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_strate
  : avec NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3
  : espace MAJO_D2042_ABAT_P;

  NUM_STRATE = 1;
  MAJ0 = MAJ_ABAT_20;
  MAJ1 = indefini;
  MAJ2 = indefini;
  MAJ3 = indefini;
  calculer cible met_a_jour_2042_strate
  : avec NUM_STRATE, MAJ0, MAJ1, MAJ2, MAJ3
  : espace MAJO_D2042_ABAT_P;

  si NB_RAPPELS_P != 0 alors
    calculer cible calcul_cum_p;
  finsi
  calculer cible  calcul_majo_P : avec NB_RAPPELS_P;
  calculer cible clear_majo_str_tr;
  iterer
  : variable NUM_STR
  : entre 0 ..(GLOBAL.NB_STRATES + 0 - 1) increment 1
  : dans (
    arranger_evenements
    : trier R1, R2
    : avec
      si (champ_evenement(R1, strate) != -1 et champ_evenement(R2, strate) = -1) alors (1)
      sinon (si (champ_evenement(R1, strate) = -1 et champ_evenement(R2, strate) != -1) alors (0)
      sinon (si (champ_evenement(R1, strate) < champ_evenement(R2, strate)) alors (1)
      sinon (si (champ_evenement(R1, strate) > champ_evenement(R2, strate)) alors (0)
      sinon (si (champ_evenement(R1, numero) < champ_evenement(R2, numero)) alors (1)
      sinon (si (champ_evenement(R1, numero) > champ_evenement(R2, numero)) alors (0)
      sinon (si (champ_evenement(R1, rappel) < champ_evenement(R2, rappel)) alors (1)
      sinon (si (champ_evenement(R1, rappel) > champ_evenement(R2, rappel)) alors (0)
      sinon (1) finsi) finsi) finsi) finsi) finsi) finsi) finsi) finsi
    : dans (
      si NUM_STR = 0 alors
        PROCHAINE_STRATE = champ_evenement(0, strate);
      sinon
        PROCHAINE_STRATE = indefini;
        iterer
        : variable R
        : entre 0..(nb_evenements() - 1) increment 1
        : dans (
          si
            (non present(PROCHAINE_STRATE))
            et champ_evenement(R, strate) = NUM_STR
          alors
            PROCHAINE_STRATE = champ_evenement(R, strate);
          finsi
        )
        si non present(PROCHAINE_STRATE) alors
          PROCHAINE_STRATE = champ_evenement(nb_evenements() - 1, strate);
        finsi
      finsi
    )
    si NUM_STR dans (20, 21, 22) alors
      MAJ0 = MAJ_TL;
      MAJ1 = indefini;
      MAJ2 = indefini;
      MAJ3 = indefini;
      calculer cible met_a_jour_2042_strate
      : avec NUM_STR, MAJ0, MAJ1, MAJ2, MAJ3
      : espace MAJO_D2042_STRATE;
    sinon
      MAJ0 = indefini;
      MAJ1 = indefini;
      MAJ2 = indefini;
      MAJ3 = indefini;
      calculer cible met_a_jour_2042_strate
      : avec NUM_STR, MAJ0, MAJ1, MAJ2, MAJ3
      : espace MAJO_D2042_STRATE;
    finsi
    NB_RAPPELS_STRATE = GLOBAL.NB_RAPPELS_RES;
    MAJ0 = MAJ_ABAT_20;
    MAJ1 = indefini;
    MAJ2 = indefini;
    MAJ3 = indefini;
    calculer cible met_a_jour_2042_strate
    : avec NUM_STR, MAJ0, MAJ1, MAJ2, MAJ3
    : espace MAJO_D2042_ABAT_STRATE;
    GLOBAL.MAJO_NUM_STRATE = NUM_STR;
    calculer cible calcul_majo_normal
    : avec IS_PREMIER, INDICE_EVT, NUM_STR, NB_RAPPELS_STRATE, PROCHAINE_STRATE;
  )
finsi
CORR.FLAG_RETARD = GLOBAL.RETARD;
CORR.FLAG_RETARD07 = GLOBAL.RETARD07;
CORR.FLAG_RETARD08 = GLOBAL.RETARD08;
CORR.FLAG_RETARD0718 = GLOBAL.RETARD0718;
CORR.FLAG_RETARD101718 = GLOBAL.RETARD101718;
CORR.FLAG_RETARD22 = GLOBAL.RETARD22;
CORR.FLAG_RETARD99 = GLOBAL.RETARD99;
CORR.FLAG_DEFAUT = GLOBAL.DEFAUT;
CORR.FLAG_DEFAUT10 = GLOBAL.DEFAUT10;
CORR.FLAG_DEFAUT11 = GLOBAL.DEFAUT11;
CORR.FLAG_RECTIF = GLOBAL.RECTIF;
CORR.FLAG_RECTIFMAJO = GLOBAL.RECTIF_MAJO;
calculer cible sauve_base_majo_corr;
calculer cible sauve_base_anterieure_corr;

cible calcul_majo_proc:
application: iliad;
variables_temporaires: IS_PREMIER, INDICE_EVT;
IS_PREMIER = TMP_ARG1;
INDICE_EVT = TMP_ARG2;
calculer cible calcul_majo : avec IS_PREMIER, INDICE_EVT;

cible recherche_CR02:
application: iliad;
arguments: RESULTAT, R;
variables_temporaires: C0, R0, C1, R1, IM41, PREM_R02, NUM_EVT, NUM_RAP;
C0 = 0;
R0 = 0;
C1 = 0;
R1 = 0;
IM41 = 0;
PREM_R02 = 1;
NUM_EVT = 0;
NUM_RAP = 0;
iterer
: variable RR
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si
    meme_variable(champ_evenement(R, code), champ_evenement(RR, code))
    et champ_evenement(RR, penalite) = 2
  alors
    si
      champ_evenement(RR, sens) = SENS_C
      et non positif(champ_evenement(RR, 2042_rect))
    alors
      C0 = C0 + champ_evenement(RR, montant);
    finsi
    si
      champ_evenement(RR, sens) = SENS_R
      et non positif(champ_evenement(RR, 2042_rect))
    alors
      R0 = R0 + champ_evenement(RR, montant);
    finsi
    si
      champ_evenement(RR, sens) = SENS_C
      et positif(champ_evenement(RR, 2042_rect))
    alors
      C1 = C1 + champ_evenement(RR, montant);
    finsi
    si
      champ_evenement(RR, sens) = SENS_R
      et positif(champ_evenement(RR, 2042_rect))
    alors
      R1 = R1 + champ_evenement(RR, montant);
    finsi
  finsi      
  si
    meme_variable(champ_evenement(R, code), champ_evenement(RR, code))
    et champ_evenement(RR, sens) = SENS_R
    et positif(champ_evenement(RR, 2042_rect))
    et positif(PREM_R02)
  alors
    PREM_R02 = 0;
    NUM_EVT = champ_evenement(RR, numero);
    NUM_RAP = champ_evenement(RR, rappel);
  finsi
  IM41 = (
    positif(IM41)
    ou (
      meme_variable(champ_evenement(R, code), champ_evenement(RR, code))
      et (
        champ_evenement(RR, penalite) != 2
        ou (
          champ_evenement(RR, penalite) = 2
          et non positif(champ_evenement(RR, 2042_rect))
        )
      )
      et champ_evenement(RR, sens) dans (SENS_C, SENS_M)
      et (non positif(PREM_R02))
      et (
        champ_evenement(RR, numero) > NUM_EVT
        ou (
          champ_evenement(RR, numero) = NUM_EVT
          et champ_evenement(RR, rappel) > NUM_RAP
        )
      )
    )
  );
)
si positif(IM41) alors
  RESULTAT = 3;
sinon_si positif(champ_evenement(R, 2042_rect)) et C1 > R1 alors
  RESULTAT = 2;
sinon_si (non positif(champ_evenement(R, 2042_rect))) et C0 > R0 alors
  RESULTAT = 1;
sinon
  RESULTAT = 0;
finsi

cible recherche_CR02_proc:
application: iliad;
variables_temporaires: RESULTAT, R;
R = TMP_ARG1;
calculer cible recherche_CR02 : avec RESULTAT, R;
TMP_RES = RESULTAT;

cible is_code_situation_famille:
application: iliad;
arguments: RES_SF, VAR;
si meme_variable(VAR, 0AM) alors
  RES_SF = SF_MARIAGE;
sinon_si meme_variable(VAR, 0AC) alors
  RES_SF = SF_CELIBAT;
sinon_si meme_variable(VAR, 0AD) alors
  RES_SF = SF_DIVORCE;
sinon_si meme_variable(VAR, 0AO) alors
  RES_SF = SF_PACSE;
sinon_si meme_variable(VAR, 0AV) alors
  si GLOBAL.ANNEE_DECES_CONJOINT = GLOBAL.ANNEE_REVENU alors
    RES_SF = SF_VEUVAGE_TRUE;
  sinon
    RES_SF = SF_VEUVAGE_FALSE;
  finsi
sinon
  RES_SF = SF_INVALIDE;
finsi

cible is_code_situation_famille_r:
application: iliad;
variables_temporaires: SF, R;
R = TMP_ARG1;
calculer cible is_code_situation_famille : avec SF, champ_evenement(R, code);
TMP_RES = SF;

cible traite_majo:
application: iliad;
arguments: IS_PREMIER, INDICE_EVT;
variables_temporaires: RAP_SF, RAP_DA, RAP_DB, PENALITE, SF, STR;
RAP_SF = indefini;
RAP_DA = indefini;
RAP_DB = indefini;
PENALITE = -1;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si champ_evenement(R, id_evt) = INDICE_EVT alors
    calculer cible is_code_situation_famille : avec SF, champ_evenement(R, code);
    si SF != SF_INVALIDE alors
      si
        non (
          (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
          et positif(IS_PREMIER)
        )
      alors
        si champ_evenement(R, sens) dans (SENS_M, SENS_C) alors
          si
            GLOBAL.SF_INITIALE dans (SF_MARIAGE, SF_VEUVAGE_TRUE, SF_PACSE)
          alors
            RAP_SF = R;
          finsi
        sinon
          GLOBAL.SF_COURANTE = SF;
          PENALITE = champ_evenement(R, penalite);
        finsi
      sinon_si positif(IS_PREMIER) alors
        GLOBAL.SF_INITIALE = SF;
      finsi
    finsi
    si meme_variable(champ_evenement(R, code), 0DA) alors
      RAP_DA = R;
    finsi
    si meme_variable(champ_evenement(R, code), 0DB) alors
      RAP_DB = R;
    finsi
    si INDICE_EVT >= 1 et positif(GLOBAL.RETARD) alors
      si champ_evenement(R, sens) dans (SENS_M, SENS_C) alors
        GLOBAL.MAJO_TARDIF_EVT2 = 1;
      finsi
    sinon
      GLOBAL.MAJO_TARDIF_EVT2 = 0;
    finsi
  finsi
)
si PENALITE >= 0 alors
  si present(RAP_SF) alors
    champ_evenement(RAP_SF, penalite) = PENALITE;
  finsi
  si present(RAP_DA) alors
    champ_evenement(RAP_DA, penalite) = PENALITE;
  finsi
  si present(RAP_DB) alors
    champ_evenement(RAP_DB, penalite) = PENALITE;
  finsi
finsi
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si champ_evenement(R, id_evt) = INDICE_EVT alors
    si INDICE_EVT >= 1 et positif(GLOBAL.RETARD) alors
      si champ_evenement(R, sens) dans (SENS_M, SENS_C) alors
        GLOBAL.MAJO_TARDIF_EVT2 = 1;
      finsi
    sinon
      GLOBAL.MAJO_TARDIF_EVT2 = 0;
    finsi
  finsi
)
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si champ_evenement(R, id_evt) <= INDICE_EVT alors
    calculer cible get_strate : avec STR, R;
    champ_evenement(R, strate) = STR;
  sinon
    champ_evenement(R, strate) = -1;
  finsi
)
si positif(GLOBAL.DEFAUT) alors
  GLOBAL.MAJO_D2042_STRATE_NB = 0;
  iterer
  : variable VAR
  : categorie saisie *
  : espace D2042_CTXT
  : dans (
    MAJO_D2042_STRATE.VAR = VAR;
    MAJO_D2042_ABAT_STRATE.VAR = VAR;
    si present(VAR) alors
      GLOBAL.MAJO_D2042_STRATE_NB = GLOBAL.MAJO_D2042_STRATE_NB + 1;
    finsi
  )
sinon_si positif(GLOBAL.RETARD) alors
  GLOBAL.MAJO_D2042_STRATE_NB = 0;
  GLOBAL.MAJO_D2042_P_NB = 0;
  iterer
  : variable VAR
  : categorie saisie *
  : espace D2042
  : dans (
    MAJO_D2042_STRATE.VAR = VAR;
    MAJO_D2042_P.VAR = VAR;
    si present(VAR) alors
      GLOBAL.MAJO_D2042_STRATE_NB = GLOBAL.MAJO_D2042_STRATE_NB + 1;
      GLOBAL.MAJO_D2042_P_NB = GLOBAL.MAJO_D2042_P_NB + 1;
    finsi
  )
  iterer
  : variable VAR
  : categorie saisie *
  : espace MAJO_D2042_REF_ABAT
  : dans (
    MAJO_D2042_ABAT_STRATE.VAR = VAR;
    MAJO_D2042_ABAT_P.VAR = VAR;
  )
sinon
  GLOBAL.MAJO_D2042_STRATE_NB = 0;
  GLOBAL.MAJO_D2042_P_NB = 0;
  iterer
  : variable VAR
  : categorie saisie *
  : espace D2042
  : dans (
    MAJO_D2042_STRATE.VAR = VAR;
    MAJO_D2042_ABAT_STRATE.VAR = VAR;
    MAJO_D2042_P.VAR = VAR;
    MAJO_D2042_ABAT_P.VAR = VAR;
    si present(VAR) alors
      GLOBAL.MAJO_D2042_STRATE_NB = GLOBAL.MAJO_D2042_STRATE_NB + 1;
      GLOBAL.MAJO_D2042_P_NB = GLOBAL.MAJO_D2042_P_NB + 1;
    finsi
  )
finsi

cible traite_majo_proc:
application: iliad;
variables_temporaires: IS_PREMIER, INDICE_EVT;
IS_PREMIER = TMP_ARG1;
INDICE_EVT = TMP_ARG2;
calculer cible traite_majo : avec IS_PREMIER, INDICE_EVT;

cible controle:
application: iliad;
arguments: QUELLE_SP, INDICE_EVT;
variables_temporaires: NATURE, ELEM_2042, MONTANT_2042, NAT_RECH, COMMENCE_PAR_5;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si champ_evenement(R, id_evt) = INDICE_EVT alors
    calculer cible get_nature : avec NATURE, champ_evenement(R, code);
    si QUELLE_SP = 0 alors
      ELEM_2042 = D2042.champ_evenement(R, code);
    sinon
      ELEM_2042 = INR_D2042.champ_evenement(R, code);
    finsi
    MONTANT_2042 = ELEM_2042 + 0;
    si champ_evenement(R, sens) = SENS_C et champ_evenement(R, penalite) != 0 alors
      calculer cible recherche_cpena : avec NAT_RECH, R;
    sinon
      NAT_RECH = 0;
    finsi
    si NAT_RECH = 5 alors
      nettoie_erreurs_finalisees;
      leve_erreur A72207;
      calculer cible signaler_erreur_ano;
    finsi
    si
      champ_evenement(R, penalite) = 32
      et non meme_variable(champ_evenement(R, code), 8VX)
    alors
      nettoie_erreurs_finalisees;
      leve_erreur A045;
      calculer cible signaler_erreur_ano;
    finsi
    si
      non (champ_evenement(R, penalite) dans (0, 32))
      et meme_variable(champ_evenement(R, code), 8VX)
    alors
      nettoie_erreurs_finalisees;
      leve_erreur A044;
      calculer cible signaler_erreur_ano;
    finsi
    si
      champ_evenement(R, sens) != SENS_M
      et non (champ_evenement(R, penalite) dans (0, 7, 8, 10, 11, 17, 18, 99))
    alors
      si meme_variable(champ_evenement(R, code), 8VW) alors
        nettoie_erreurs_finalisees;
        leve_erreur A042;
        calculer cible signaler_erreur_ano;
      sinon_si meme_variable(champ_evenement(R, code), 8VV) alors
        nettoie_erreurs_finalisees;
        leve_erreur A041;
        calculer cible signaler_erreur_ano;
      finsi
    finsi
    si
      non (champ_evenement(R, penalite) dans (30, 35, 99))
      et meme_variable(champ_evenement(R, code), 8WW)
    alors
      nettoie_erreurs_finalisees;
      leve_erreur A046;
      calculer cible signaler_erreur_ano;
    finsi
    calculer cible alias_commence_par_5 : avec COMMENCE_PAR_5, champ_evenement(R, code);
    si
      champ_evenement(R, penalite) = 31
      et (
        meme_variable(champ_evenement(R, code), 5QM)
        ou meme_variable(champ_evenement(R, code), 5RM)
        ou non positif(COMMENCE_PAR_5)
      )
    alors
      nettoie_erreurs_finalisees;
      leve_erreur A048;
      calculer cible signaler_erreur_ano;
    finsi
    si
      champ_evenement(R, sens) = SENS_M
      et champ_evenement(R, penalite) != 0
      et non meme_variable(champ_evenement(R, code), REGCO)
    alors
      nettoie_erreurs_finalisees;
      leve_erreur A970;
      calculer cible signaler_erreur_ano;
    finsi
    si GLOBAL.NUM_EVT_9XT > GLOBAL.NUM_EVT_9XU alors
      nettoie_erreurs_finalisees;
      leve_erreur A96903;
      calculer cible signaler_erreur_ano;
    finsi
    si GLOBAL.NUM_EVT_9XU > GLOBAL.NUM_EVT_9XT alors
      nettoie_erreurs_finalisees;
      leve_erreur A96904;
      calculer cible signaler_erreur_ano;
    finsi
    si GLOBAL.MONTANT_9XT = 0 et GLOBAL.MONTANT_9XU != 0 alors
      nettoie_erreurs_finalisees;
      leve_erreur A96803;
      calculer cible signaler_erreur_ano;
    finsi
    si GLOBAL.MONTANT_9XU = 0 et GLOBAL.MONTANT_9XT != 0 alors
      nettoie_erreurs_finalisees;
      leve_erreur A96804;
      calculer cible signaler_erreur_ano;
    finsi
    si GLOBAL.NUM_EVT_9YT > GLOBAL.NUM_EVT_9YU alors
      nettoie_erreurs_finalisees;
      leve_erreur A96901;
      calculer cible signaler_erreur_ano;
    finsi
    si GLOBAL.NUM_EVT_9YU > GLOBAL.NUM_EVT_9YT alors
      nettoie_erreurs_finalisees;
      leve_erreur A96902;
      calculer cible signaler_erreur_ano;
    finsi
    si GLOBAL.MONTANT_9YT = 0 et GLOBAL.MONTANT_9YU != 0 alors
      nettoie_erreurs_finalisees;
      leve_erreur A96801;
      calculer cible signaler_erreur_ano;
    finsi
    si GLOBAL.MONTANT_9YU = 0 et GLOBAL.MONTANT_9YT != 0 alors
      nettoie_erreurs_finalisees;
      leve_erreur A96802;
      calculer cible signaler_erreur_ano;
    finsi
    si champ_evenement(R, sens) = SENS_R et champ_evenement(R, penalite) = 0 alors
      nettoie_erreurs_finalisees;
      leve_erreur A971;
      calculer cible signaler_erreur_ano;
    finsi
    si present(ELEM_2042) alors
      si
        type(champ_evenement(R, code), BOOLEEN)
        et non (
          (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
          et champ_evenement(R, numero) = champ_evenement(0, numero)
        )
      alors
        si NATURE = N_REVENU alors
          si ELEM_2042 <= 0 et champ_evenement(R, sens) = SENS_M alors
            nettoie_erreurs_finalisees;
            leve_erreur A72001;
            calculer cible signaler_erreur_ano;
          sinon_si ELEM_2042 <= 0 et champ_evenement(R, sens) = SENS_C alors
            nettoie_erreurs_finalisees;
            leve_erreur A72002;
            calculer cible signaler_erreur_ano;
          sinon_si ELEM_2042 >= 1 et champ_evenement(R, sens) = SENS_R alors
            nettoie_erreurs_finalisees;
            leve_erreur A72003;
            calculer cible signaler_erreur_ano;
          finsi
        sinon_si NATURE = N_CHARGE alors
          si ELEM_2042 >= 1 et champ_evenement(R, sens) = SENS_M alors
            nettoie_erreurs_finalisees;
            leve_erreur A72001;
            calculer cible signaler_erreur_ano;
          sinon_si ELEM_2042 >= 1 et champ_evenement(R, sens) = SENS_C alors
            nettoie_erreurs_finalisees;
            leve_erreur A72002;
            calculer cible signaler_erreur_ano;
          sinon_si ELEM_2042 <= 0 et champ_evenement(R, sens) = SENS_R alors
            nettoie_erreurs_finalisees;
            leve_erreur A72003;
            calculer cible signaler_erreur_ano;
          finsi
        finsi
      finsi
    finsi
    si
      NAT_RECH dans (0, 6)
      et NATURE = N_REVENU
      et champ_evenement(R, sens) = SENS_C
      et non present(ELEM_2042)
    alors
      nettoie_erreurs_finalisees;
      leve_erreur A72002;
      calculer cible signaler_erreur_ano;
    finsi
    si
      non (
        meme_variable(champ_evenement(R, code), 0AX)
        ou meme_variable(champ_evenement(R, code), 0AY)
        ou meme_variable(champ_evenement(R, code), 0AZ)
      )
      et NATURE = N_CHARGE
      et champ_evenement(R, sens) = SENS_R
      et non present(ELEM_2042)
    alors
      nettoie_erreurs_finalisees;
      leve_erreur A72003;
      calculer cible signaler_erreur_ano;
    finsi
    si champ_evenement(R, sens) = SENS_P et non present(ELEM_2042) alors
      nettoie_erreurs_finalisees;
      leve_erreur A721;
      calculer cible signaler_erreur_ano;
    finsi
    si
      non (
        meme_variable(champ_evenement(R, code), 0AX)
        ou meme_variable(champ_evenement(R, code), 0AY)
        ou meme_variable(champ_evenement(R, code), 0AZ)
      )
    alors
      si
        NATURE = N_CHARGE
        et champ_evenement(R, sens) = SENS_R
        et champ_evenement(R, montant) > MONTANT_2042
      alors
        si champ_evenement(R, penalite) = 99 alors
          nettoie_erreurs_finalisees;
          leve_erreur A72204;
          calculer cible signaler_erreur_ano;
        sinon
          nettoie_erreurs_finalisees;
          leve_erreur A72201;
          calculer cible signaler_erreur_ano;
        finsi
      finsi
      si
        NATURE = N_REVENU
        et champ_evenement(R, sens) = SENS_M
        et (
          non present(ELEM_2042)
          ou champ_evenement(R, montant) > MONTANT_2042
        )
      alors
        nettoie_erreurs_finalisees;
        leve_erreur A72202;
        calculer cible signaler_erreur_ano;
      finsi
      CORR.V_FLAG8OT = positif(GLOBAL.COD8OT + 0);
      si
        NATURE = N_REVENU
        et champ_evenement(R, sens) = SENS_C
        et champ_evenement(R, penalite) = 0
        et present(ELEM_2042)
        et champ_evenement(R, montant) > MONTANT_2042
      alors
        nettoie_erreurs_finalisees;
        leve_erreur A72203;
        calculer cible signaler_erreur_ano;
      finsi
    finsi
    si
      champ_evenement(R, sens) dans (SENS_R, SENS_P)
      et champ_evenement(R, penalite) dans (9, 12)
      et vers_annee(champ_evenement(R, date)) >= 2006
    alors
      nettoie_erreurs_finalisees;
      leve_erreur A724;
      calculer cible signaler_erreur_ano;
    finsi
    si
      (positif(GLOBAL.DEFAUT1011) ou positif(GLOBAL.RETARD0718))
      et champ_evenement(R, numero) != champ_evenement(0, numero)
      et champ_evenement(R, penalite) = 22
      et non positif(CORR.V_FLAGANO726 + 0)
    alors
      nettoie_erreurs_finalisees;
      leve_erreur A725;
      calculer cible signaler_erreur_ano;
    finsi
    si
      (positif(GLOBAL.DEFAUT1011) ou positif(GLOBAL.RETARD0718))
      et non (
        # codes_sf
        meme_variable(champ_evenement(R, code), 0AM)
        ou meme_variable(champ_evenement(R, code), 0AC)
        ou meme_variable(champ_evenement(R, code), 0AD)
        ou meme_variable(champ_evenement(R, code), 0AO)
        ou meme_variable(champ_evenement(R, code), 0AV)
      )
      et champ_evenement(R, penalite) != 99
      et positif(CORR.V_FLAGANO726 + 0)
    alors
      nettoie_erreurs_finalisees;
      leve_erreur A726;
      calculer cible signaler_erreur_ano;
    finsi
    si
      champ_evenement(R, numero) = champ_evenement(nb_evenements() - 1, numero)
      et champ_evenement(R, sens) dans (SENS_R, SENS_C, SENS_M)
    alors
      calculer cible recherche_CR02 : avec NAT_RECH, R;
      si NAT_RECH = 2 alors
        nettoie_erreurs_finalisees;
        leve_erreur A72301;
        calculer cible signaler_erreur_ano;
      sinon_si NAT_RECH = 3 et non positif(GLOBAL.ISF_PRIM) alors
        leve_erreur IM41;
        finalise_erreurs;
      finsi
    finsi
  finsi
)

cible controle_proc:
application: iliad;
variables_temporaires: QUELLE_SP, INDICE_EVT;
QUELLE_SP = TMP_ARG1;
INDICE_EVT = TMP_ARG2;
calculer cible controle : avec QUELLE_SP, INDICE_EVT;

cible make_2042_of_tgv:
application: iliad;
arguments: VAR;
variables_temporaires: SF;
si present(GLOBAL.VAR) alors
  calculer cible is_code_situation_famille : avec SF, VAR;
  si SF != SF_INVALIDE alors
    GLOBAL.SF_INITIALE = SF;
  finsi
  si meme_variable(VAR, RJLJ) et GLOBAL.VAR != 0 alors
    GLOBAL.CORR_RJLJ = 1;
  finsi
  D2042.VAR = GLOBAL.VAR;
  GLOBAL.D2042_NB = GLOBAL.D2042_NB + 1;
finsi

cible is_rappel_1728:
application: iliad;
arguments: RESULTAT, PENA, VAR;
variables_temporaires: EST_ISF;
calculer cible est_code_isf : avec EST_ISF, VAR;
si positif(EST_ISF) alors
  RESULTAT = (
    PENA dans (7, 8, 9, 10, 11, 12, 17, 18, 31, 99)
  );
sinon
  RESULTAT = (
    PENA dans (2, 3, 7, 8, 9, 10, 11, 12, 17, 18, 22, 31, 99)
  );
finsi


cible prepare_reference:
application: iliad;
variables_temporaires:
  MAKE_2042, MONTANT, NATURE, LEVE_A989, EST_ISF, PENA, EST_RAPPEL_1728;
si non (positif(GLOBAL.PRESENT_9YT) ou positif(GLOBAL.PRESENT_9XT)) alors
  iterer
  : variable VAR
  : categorie saisie famille
  : avec present(VAR)
  : espace GLOBAL
  : dans (
      SF_PRIMITIF = 1;
  )
finsi
GLOBAL.D2042_NB = 0;
si non (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD)) alors
  iterer
  : variable VAR
  : categorie saisie *
  : dans (
    D2042_ABAT.VAR = indefini;
    D2042_RECT.VAR = indefini;
    si dans_domaine(VAR, saisie contexte) alors
      MAKE_2042 = (non meme_variable(VAR, IND_TRAIT));
    sinon
      MAKE_2042 = (
        dans_domaine(VAR, saisie famille)
        ou dans_domaine(VAR, saisie revenu)
        ou dans_domaine(VAR, saisie revenu corrective)
        ou dans_domaine(VAR, saisie variation)
      );
    finsi
    si positif(MAKE_2042) alors
      calculer cible make_2042_of_tgv : avec VAR;
    sinon
      D2042.VAR = indefini;
    finsi
  )
  iterer
  : variable VAR
  : categorie saisie *
  : avec present(VAR)
  : espace D2042
  : dans (
    D2042_ABAT.VAR = VAR;
    D2042_RECT.VAR = VAR;
  )
sinon
  iterer
  : variable VAR
  : categorie saisie *
  : dans (
    D2042_ABAT.VAR = indefini;
    D2042_RECT.VAR = indefini;
    D2042_CTXT.VAR = indefini;
    si dans_domaine(VAR, saisie contexte) alors
      MAKE_2042 = (non meme_variable(VAR, IND_TRAIT));
    sinon
      MAKE_2042 = (dans_domaine(VAR, saisie revenu));
    finsi
    si positif(MAKE_2042) alors
      calculer cible make_2042_of_tgv : avec VAR;
    sinon
      D2042.VAR = indefini;
    finsi
  )
  iterer
  : variable VAR
  : categorie saisie *
  : espace D2042
  : dans (
    si present(VAR) alors
      D2042_ABAT.VAR = VAR;
      D2042_RECT.VAR = VAR;
      D2042_CTXT.VAR = VAR;
      MAJO_D2042_REF_ABAT.VAR = VAR;
    sinon
      MAJO_D2042_REF_ABAT.VAR = indefini;
    finsi
  )
  si positif(GLOBAL.PRESENT_9YT) alors
    iterer
    : variable R
    : entre 0..(nb_evenements() - 1) increment 1
    : dans (
      si champ_evenement(R, numero) <= GLOBAL.PREMIER_EVT alors
        champ_evenement(R, penalite) = GLOBAL.MONTANT_9YT;
        champ_evenement(R, date) = GLOBAL.DATE_9YU;
      finsi
      si non (GLOBAL.MONTANT_9YT dans (8, 11)) alors
        GLOBAL.PREM_8_11 = 0;
        CORR.PREM8_11 = 0;
      finsi
      si non (GLOBAL.MONTANT_9YT dans (7, 18)) alors
        champ_evenement(R, 2042_rect) = 0;
      finsi
    )
  finsi
  si positif(GLOBAL.PRESENT_9XT) alors
    iterer
    : variable R
    : entre 0..(nb_evenements() - 1) increment 1
    : dans (
      si champ_evenement(R, numero) <= GLOBAL.PREMIER_EVT alors
        champ_evenement(R, penalite) = GLOBAL.MONTANT_9XT;
        champ_evenement(R, date) = GLOBAL.DATE_9XU;
      finsi
      si non (GLOBAL.MONTANT_9XT dans (8, 11)) alors
        GLOBAL.PREM_8_11 = 0;
        CORR.PREM8_11 = 0;
      finsi
      si non (GLOBAL.MONTANT_9XT dans (7, 18)) alors
        champ_evenement(R, 2042_rect) = 0;
      finsi
    )
  finsi
  iterer
  : variable R
  : entre 0..(nb_evenements() - 1) increment 1
  : dans (
    si champ_evenement(R, id_evt) = 0 alors
      si present(D2042.champ_evenement(R, code)) alors
        MONTANT = D2042.champ_evenement(R, code) + champ_evenement(R, montant);
        si
          type(champ_evenement(R, code), BOOLEEN)
          et non (MONTANT dans (0, 1))
        alors
          MONTANT = D2042.champ_evenement(R, code);
        finsi
      sinon
        MONTANT = champ_evenement(R, montant);
      finsi
      D2042.champ_evenement(R, code) = MONTANT;
      D2042_RECT.champ_evenement(R, code) = MONTANT;
      si positif(GLOBAL.RETARD) et champ_evenement(R, penalite) = 7 alors
        D2042_ABAT.champ_evenement(R, code) = MONTANT;
        MAJO_D2042_REF_ABAT.champ_evenement(R, code) = MONTANT;
      finsi
      calculer cible get_nature : avec NATURE, champ_evenement(R, code);
      si NATURE = N_CHARGE alors
        champ_evenement(R, sens) = SENS_M;
        champ_evenement(R, penalite) = 0;
      finsi
    finsi
  )
finsi
LEVE_A989 = 0;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si LEVE_A989 = 0 alors
    calculer cible est_code_isf : avec EST_ISF, champ_evenement(R, code);
    PENA = champ_evenement(R, penalite);
    calculer cible is_rappel_1728
    : avec EST_RAPPEL_1728, PENA, champ_evenement(R, code);
    si positif(EST_ISF) et positif(EST_RAPPEL_1728) alors
      si
        (positif(GLOBAL.ISF_PRIM) et GLOBAL.CODE_PENA_ISF = 1)
        ou ((non positif(GLOBAL.ISF_PRIM)) et GLOBAL.CODE_PENA_ISF = 0)
      alors
        GLOBAL.CODE_PENA_ISF = champ_evenement(R, penalite);
      finsi
      si
        non (
          champ_evenement(R, penalite) dans (1, 2, 3, 22, 99, GLOBAL.CODE_PENA_ISF)
        )
      alors
        LEVE_A989 = 1;
      finsi
    finsi
  finsi
)
si positif(LEVE_A989) alors
  nettoie_erreurs_finalisees;
  leve_erreur A989;
  calculer cible signaler_erreur_ano;
finsi

cible calcul_aux:
application: iliad;
variables_temporaires:
  HUIT, PENA_0DA, EST_TAX_INIT, VERIF_CODE,
  QUELLE_SP, INDICE_EVT_CTRL, ANO_994_1, ANO_994_2,
  IS_PREMIER, IS_DERNIER, EXISTE_SENS_RC;
GLOBAL.SAUVE_INR_R99 = 0;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si GLOBAL.SAUVE_INR_R99 = 0 alors
    si
      champ_evenement(R, sens) = SENS_R
      et champ_evenement(R, penalite) = 99
    alors
      GLOBAL.SAUVE_INR_R99 = 1;
    finsi
  finsi
)
HUIT = 0;
PENA_0DA = 0;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si meme_variable(champ_evenement(R, code), 9ZA) alors
    GLOBAL.CODE_9ZA = (champ_evenement(R, montant) = 1);
  finsi
  HUIT = (positif(HUIT) ou champ_evenement(R, penalite) = 8);
  si champ_evenement(R, penalite) = 18 alors
    champ_evenement(R, penalite) = 7;
  sinon_si champ_evenement(R, penalite) = 34 alors
    si non positif(HUIT) alors
      CORR.V_FLAGR34 = 1;
    finsi
    champ_evenement(R, penalite) = 8;
  sinon_si champ_evenement(R, sens) = SENS_R et champ_evenement(R, penalite) = 1 alors
    champ_evenement(R, penalite) = 99;
  sinon_si champ_evenement(R, sens) = SENS_C et champ_evenement(R, penalite) = 1 alors
    champ_evenement(R, penalite) = 0;
  sinon_si champ_evenement(R, penalite) = 24 alors
    CORR.V_FLAGANO726 = 1;
    CORR.V_FLAGR24 = 1;
  sinon_si champ_evenement(R, penalite) = 22 alors
    CORR.V_FLAGR22 = 1;
  finsi
  si
    meme_variable(champ_evenement(R, code), 0DA)
    et champ_evenement(R, sens) = SENS_R
    et (3 <= champ_evenement(R, penalite) et champ_evenement(R, penalite) <= 98)
  alors
    PENA_0DA = champ_evenement(R, penalite);
  finsi
  calculer cible est_code_tax_init : avec EST_TAX_INIT, champ_evenement(R, code);
  si
    champ_evenement(R, sens) = SENS_M
    et non (
      meme_variable(champ_evenement(R, code), 9GF)
      ou meme_variable(champ_evenement(R, code), 9GH)
      ou meme_variable(champ_evenement(R, code), 9NC)
      ou meme_variable(champ_evenement(R, code), 9NG)
      ou meme_variable(champ_evenement(R, code), 9GY)
      ou meme_variable(champ_evenement(R, code), 9PR)
      ou meme_variable(champ_evenement(R, code), 9RS)
      ou meme_variable(champ_evenement(R, code), REGCO)
      ou positif(EST_TAX_INIT)
    )
  alors
    CORR.V_FLAGMENC = 1;
    GLOBAL.FLAGMENC = 1;
    champ_evenement(R, sens) = SENS_C;
    champ_evenement(R, penalite) = 0;
  sinon_si
    champ_evenement(R, sens) = SENS_C
    et champ_evenement(R, penalite) = PENA_0DA
    et non (champ_evenement(R, penalite) dans (2, 22))
  alors
    champ_evenement(R, sens) = SENS_C;
    calculer cible verif_code_prem_evt : avec VERIF_CODE, R;
    si positif(VERIF_CODE) alors
      champ_evenement(R, penalite) = 0;
    finsi
  finsi
)
calculer cible controle_defaut_retard_2042;
GLOBAL.PREM_8_11 = 1;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si positif(GLOBAL.PREM_8_11) alors
    si
      non (
        champ_evenement(R, id_evt) != 0
        ou champ_evenement(R, penalite) dans (8, 11)
      )
    alors
      GLOBAL.PREM_8_11 = 0;
    finsi
  finsi
)
GLOBAL.PREM_8_11 = (
  (GLOBAL.PREM_8_11 ou GLOBAL.MONTANT_9XT dans (8, 11))
  et (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
);
CORR.PREM8_11 = GLOBAL.PREM_8_11;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  calculer cible est_code_tax_init : avec EST_TAX_INIT, champ_evenement(R, code);
  si
    (champ_evenement(R, sens) = SENS_C et champ_evenement(R, penalite) = 0)
    ou champ_evenement(R, penalite) = 99
    ou positif(EST_TAX_INIT)
  alors
    GLOBAL.RECTIF = 0;
  finsi
  si
    (champ_evenement(R, sens) = SENS_C et champ_evenement(R, penalite) = 0)
    ou champ_evenement(R, penalite) dans (1, 99)
  alors
    GLOBAL.RECTIF_MAJO = 1;
  finsi
  si
    (positif(GLOBAL.DEFAUT) ou positif(GLOBAL.RETARD))
    et (
      (champ_evenement(R, sens) = SENS_C et champ_evenement(R, penalite) = 0)
      ou champ_evenement(R, penalite) = 99
    )
  alors
    champ_evenement(R, date) = champ_evenement(0, date);
  finsi
  si
    champ_evenement(R, penalite) = 30
    et non meme_variable(champ_evenement(R, code), 8WW)
  alors
    nettoie_erreurs_finalisees;
    leve_erreur A047;
    calculer cible signaler_erreur_ano;
  finsi
)
arranger_evenements
: trier R1, R2
: avec
  si (champ_evenement(R1, numero) < champ_evenement(R2, numero)) alors (1)
  sinon (si (champ_evenement(R1, numero) > champ_evenement(R2, numero)) alors (0)
  sinon (si (vers_annee(champ_evenement(R1, date)) < vers_annee(champ_evenement(R2, date))) alors (1)
  sinon (si (vers_annee(champ_evenement(R1, date)) > vers_annee(champ_evenement(R2, date))) alors (0)
  sinon (si (vers_mois(champ_evenement(R1, date)) < vers_mois(champ_evenement(R2, date))) alors (1)
  sinon (si (vers_mois(champ_evenement(R1, date)) > vers_mois(champ_evenement(R2, date))) alors (0)
  sinon (si (champ_evenement(R1, rappel) < champ_evenement(R2, rappel)) alors (1)
  sinon (si (champ_evenement(R1, rappel) > champ_evenement(R2, rappel)) alors (0)
  sinon (1) finsi) finsi) finsi) finsi) finsi) finsi) finsi) finsi
: dans (
  calculer cible prepare_reference;
  calculer cible prepare_tl;
  calculer cible prepare_majo;
  calculer cible prepare_inr;
  QUELLE_SP = 0;
  INDICE_EVT_CTRL = 0;
  calculer cible controle : avec QUELLE_SP, INDICE_EVT_CTRL;
  calculer cible denature_rappels;
  ANO_994_1 = 0;
  iterer
  : variable R
  : entre 0..(nb_evenements() - 1) increment 1
  : dans (
    si non positif(ANO_994_1) alors
      si
        # penalites_1731bis
        champ_evenement(R, penalite) dans (3, 4, 5, 6, 8, 11, 30, 31, 32, 35, 55)
      alors
        ANO_994_1 = 1;
      finsi
    finsi
  )
  ANO_994_2 = 0;
  iterer
  : variable R
  : entre 0..(nb_evenements() - 1) increment 1
  : dans (
    si non positif(ANO_994_2) alors
      si
        champ_evenement(R, penalite) dans (2, 7, 10, 17, 18, 22)
      alors
        ANO_994_2 = 1;
      finsi
    finsi
  )
  GLOBAL.PENA_994 = (positif(ANO_994_1) et positif(ANO_994_2));
  CORR.PENA994 = GLOBAL.PENA_994;
  CORR.V_FLAGMENC = 0;
  iterer
  : variable INDICE_EVT
  : entre 0..GLOBAL.MAX_ID_EVT increment 1
  : dans (
    IS_PREMIER = (INDICE_EVT = 0);
    IS_DERNIER = (INDICE_EVT = GLOBAL.MAX_ID_EVT);
    CORR.V_FLAGR22 = 0;
    iterer
    : variable R
    : entre 0..(nb_evenements() - 1) increment 1
    : dans (
      si non positif(CORR.V_FLAGR22) alors
        si
          champ_evenement(R, id_evt) = INDICE_EVT
          et champ_evenement(R, penalite) = 22
        alors
          CORR.V_FLAGR22 = 1;
        finsi
      finsi
    )
    CORR.V_FLAGR24 = 0;
    iterer
    : variable R
    : entre 0..(nb_evenements() - 1) increment 1
    : dans (
      si non positif(CORR.V_FLAGR24) alors
        si
          champ_evenement(R, id_evt) = INDICE_EVT
          et champ_evenement(R, penalite) = 24
        alors
          CORR.V_FLAGR24 = 1;
        finsi
      finsi
    )
    calculer cible traite_reference : avec IS_PREMIER, INDICE_EVT;
    calculer cible traite_tl : avec IS_PREMIER, INDICE_EVT;
    calculer cible traite_inr : avec INDICE_EVT;
    calculer cible traite_majo : avec IS_PREMIER, INDICE_EVT;
    CORR.FLAGPREM = IS_PREMIER;
    GLOBAL.FLAGPREM = IS_PREMIER;
    si positif(IS_DERNIER) alors
      CORR.FLAGDERNIE = IS_DERNIER;
      GLOBAL.FLAGDERNIE = IS_DERNIER;
    finsi
    calculer cible init_1731;
    EXISTE_SENS_RC = 0;
    iterer
    : variable R
    : entre 0..(nb_evenements() - 1) increment 1
    : dans (
      si non positif(EXISTE_SENS_RC) alors
        si
          champ_evenement(R, id_evt) = INDICE_EVT
          et champ_evenement(R, sens) dans (SENS_R, SENS_C)
        alors
          EXISTE_SENS_RC = 1;
        finsi
      finsi
    )
    si positif(EXISTE_SENS_RC) alors
      CORR.ART1731BIS = 0;
      GLOBAL.ART1731BIS = 0;
    finsi
    iterer
    : variable VAR
    : categorie calculee
    : espace CORR
    : dans (
      VAR = indefini;
    )
    si positif(GLOBAL.FLAGMENC) alors
      CORR.FLAGMENC = 1;
    finsi
    calculer cible calcul_tl : avec IS_PREMIER;
    calculer cible calcul_inr : avec IS_PREMIER, INDICE_EVT;
    si
      positif(IS_DERNIER)
      ou (
        positif(IS_PREMIER)
        et non positif(GLOBAL.DEFAUT)
        et positif(GLOBAL.RETARD)
      )
    alors
      calculer cible calcul_majo : avec IS_PREMIER, INDICE_EVT;
    finsi
    si
      positif(IS_DERNIER)
      et positif(GLOBAL.PRESENT_8VV)
      et positif(GLOBAL.PRESENT_8VW)
      et CORR.RE168 + 0 > 0
      et CORR.TAX1649 + 0 > 0
    alors
      nettoie_erreurs_finalisees;
      leve_erreur A043;
      calculer cible signaler_erreur_ano;
    finsi
    si non positif(IS_DERNIER) alors
      QUELLE_SP = 1;
      INDICE_EVT_CTRL = INDICE_EVT + 1;
      calculer cible controle : avec QUELLE_SP, INDICE_EVT_CTRL;
      calculer cible sauve_base_anterieure_cor_corr;
      si positif(IS_PREMIER) alors
        calculer cible sauve_base_premier_corr;
      finsi
    finsi
    CORR.V_FLAGR22 = 0;
    CORR.V_FLAGR24 = 0;
  )
)

cible nb_transf_2042_rappels:
application: iliad;
variables_temporaires: EST_ISF;
iterer
: variable VAR
: categorie saisie revenu, saisie revenu corrective, saisie variation, saisie penalite, calculee *
: avec present(VAR)
: espace CORR
: dans (
  calculer cible est_code_isf : avec EST_ISF, VAR;
  si positif(EST_ISF) alors
    si positif(GLOBAL.PRESENT_9XT) alors
      GLOBAL.NB_NOUVEAUX_RAPPELS = GLOBAL.NB_NOUVEAUX_RAPPELS + 1;
    finsi
  sinon
    si positif(GLOBAL.PRESENT_9YT) alors
      GLOBAL.NB_NOUVEAUX_RAPPELS = GLOBAL.NB_NOUVEAUX_RAPPELS + 1;
    finsi
  finsi
)

cible prepare_rappels:
application: iliad;
variables_temporaires: FIN, MOIS, ANNEE;
GLOBAL.DATEINR = 0;
CORR.DATEINR = 0;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si
    meme_variable(champ_evenement(R, code), 9YA)
    et champ_evenement(R, numero) != champ_evenement(nb_evenements() - 1, numero)
  alors
    nettoie_erreurs_finalisees;
    leve_erreur A99302;
    calculer cible signaler_erreur_ano;
  finsi
  si
    type(champ_evenement(R, code), BOOLEEN)
    et non (champ_evenement(R, montant) dans (0, 1))
  alors
    nettoie_erreurs_finalisees;
    leve_erreur A910;
    calculer cible signaler_erreur_ano;
  finsi
)
calculer cible set_rappel_ifi_prim;
FIN = 0;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si
    non positif(FIN)
    et meme_variable(champ_evenement(R, code), REGCO)
  alors
    calculer cible transfo_pena_regco : avec R;
    si present(GLOBAL.REGCO) alors
      si GLOBAL.REGCO - champ_evenement(R, montant) < 0 alors
        champ_evenement(R, sens) = SENS_R;
        champ_evenement(R, montant) = champ_evenement(R, montant) - GLOBAL.REGCO;
      sinon
        champ_evenement(R, sens) = SENS_M;
        champ_evenement(R, montant) = GLOBAL.REGCO - champ_evenement(R, montant);
      finsi
      FIN = 1;
    finsi
  finsi
)
si non present(GLOBAL.CMAJ) alors
  GLOBAL.PREMIER_EVT = champ_evenement(0, numero);
  iterer
  : variable R
  : entre 0..(nb_evenements() - 1) increment 1
  : dans (
    si meme_variable(champ_evenement(R, code), 8VV) alors
      GLOBAL.PRESENT_8VV = 1;
    sinon_si meme_variable(champ_evenement(R, code), 8VW) alors
      GLOBAL.PRESENT_8VW = 1;
    sinon_si meme_variable(champ_evenement(R, code), 9YT) alors
      GLOBAL.PRESENT_9YT = 1;
      si champ_evenement(R, montant) = 18 alors
        GLOBAL.MONTANT_9YT = 7;
      sinon
        GLOBAL.MONTANT_9YT = champ_evenement(R, montant);
      finsi
      GLOBAL.PENALITE_9YT = champ_evenement(R, penalite);
      GLOBAL.NUM_EVT_9YT = champ_evenement(R, numero);
      CORR.CMAJ2 = champ_evenement(R, montant);
      GLOBAL.CMAJ2 = champ_evenement(R, montant);
      si GLOBAL.MONTANT_9YT != 0 alors
        GLOBAL.SENS_9YT = champ_evenement(R, sens);
        GLOBAL.IND_20_9YT = champ_evenement(R, 2042_rect);
        GLOBAL.BASE_TL_9YT = champ_evenement(R, base_tl);
        GLOBAL.R_TARDIF = 1;
      finsi
    sinon_si meme_variable(champ_evenement(R, code), 9YU) alors
      GLOBAL.PRESENT_9YU = 1;
      GLOBAL.MONTANT_9YU = champ_evenement(R, montant);
      GLOBAL.PENALITE_9YU = champ_evenement(R, penalite);
      GLOBAL.NUM_EVT_9YU = champ_evenement(R, numero);
      GLOBAL.SENS_9YU = champ_evenement(R, sens);
      MOIS = vers_mois(champ_evenement(R, montant));
      ANNEE = vers_annee(champ_evenement(R, montant));
      CORR.DATEINR = ANNEE * 10000 + MOIS * 100 + 1;
      GLOBAL.DATEINR = CORR.DATEINR;
      CORR.MOISAN2 = champ_evenement(R, montant);
      GLOBAL.MOISAN2 = champ_evenement(R, montant);
      GLOBAL.DATE_9YU = vers_date(MOIS, ANNEE);
    finsi
  )
finsi
si
  (positif(GLOBAL.PRESENT_9YT) ou positif(GLOBAL.PRESENT_9YU))
  et GLOBAL.MONTANT_9YT != 0
  et GLOBAL.MONTANT_9YU != 0
alors
  GLOBAL.R_TARDIF = 1;
finsi
si non present(GLOBAL.CMAJ) alors
  GLOBAL.PREMIER_EVT = champ_evenement(0, numero);
  iterer
  : variable R
  : entre 0..(nb_evenements() - 1) increment 1
  : dans (
    si meme_variable(champ_evenement(R, code), 9XT) alors
      GLOBAL.PRESENT_9XT = 1;
      si champ_evenement(R, montant) = 34 alors
        GLOBAL.MONTANT_9XT = 8;
      sinon_si champ_evenement(R, montant) = 18 alors
        GLOBAL.MONTANT_9XT = 7;
      sinon
        GLOBAL.MONTANT_9XT = champ_evenement(R, montant);
      finsi
      si champ_evenement(R, montant) = 34 alors
        CORR.V_FLAGR34 = 1;
      finsi
      GLOBAL.PENALITE_9XT  = champ_evenement(R, penalite);
      GLOBAL.NUM_EVT_9XT = champ_evenement(R, numero);
      si GLOBAL.MONTANT_9XT != 0 alors
        GLOBAL.SENS_9XT = champ_evenement(R, sens);
        GLOBAL.IND_20_9XT = champ_evenement(R, 2042_rect);
        GLOBAL.BASE_TL_9XT = champ_evenement(R, base_tl);
        GLOBAL.R_TARDIF = 1;
      finsi
    sinon_si meme_variable(champ_evenement(R, code), 9XU) alors
      GLOBAL.PRESENT_9XU = 1;
      GLOBAL.MONTANT_9XU = champ_evenement(R, montant);
      GLOBAL.PENALITE_9XU = champ_evenement(R, penalite);
      GLOBAL.NUM_EVT_9XU = champ_evenement(R, numero);
      GLOBAL.SENS_9XU = champ_evenement(R, sens);
      MOIS = vers_mois(champ_evenement(R, montant));
      ANNEE = vers_annee(champ_evenement(R, montant));
      GLOBAL.DATE_9XU = vers_date(MOIS, ANNEE);
    finsi
  )
finsi
si
  (positif(GLOBAL.PRESENT_9XT) ou positif(GLOBAL.PRESENT_9XU))
  et GLOBAL.MONTANT_9XT != 0
  et GLOBAL.MONTANT_9XU != 0
alors
  GLOBAL.R_TARDIF = 1;
finsi
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si
    meme_variable(champ_evenement(R, code), 9YT)
    ou meme_variable(champ_evenement(R, code), 9YU)
    ou meme_variable(champ_evenement(R, code), 9XT)
    ou meme_variable(champ_evenement(R, code), 9XU)
  alors
    champ_evenement(R, numero) = -1;
  sinon
    si
      positif(GLOBAL.PRESENT_9YT)
      et champ_evenement(R, numero) >= GLOBAL.NUM_EVT_9YT
    alors
      champ_evenement(R, numero) = champ_evenement(R, numero) + 1;
    finsi
    si
      positif(GLOBAL.PRESENT_9XT)
      et champ_evenement(R, numero) >= GLOBAL.NUM_EVT_9XT
    alors
       champ_evenement(R, numero) = champ_evenement(R, numero) + 1;
    finsi
  finsi
)
NB_RAPPELS = nb_evenements();
NB_NOUVEAUX_RAPPELS = 0;
si
  positif(GLOBAL.PRESENT_9YT)
  et positif(GLOBAL.PRESENT_9YU)
  et GLOBAL.MONTANT_9YT != 0
  et GLOBAL.MONTANT_9YU != 0
alors
  calculer cible nb_transf_2042_rappels;
finsi
si
  positif(GLOBAL.PRESENT_9XT)
  et positif(GLOBAL.PRESENT_9XU)
  et GLOBAL.MONTANT_9XT != 0
  et GLOBAL.MONTANT_9XU != 0
alors
  calculer cible nb_transf_2042_rappels;
finsi

cible set_id_evt:
application: iliad;
variables_temporaires: ID_EVT, NUM_EVT, NUM;
ID_EVT = 0;
GLOBAL.MAX_ID_EVT = 0;
NUM_EVT = champ_evenement(0, numero);
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  NUM = champ_evenement(R, numero);
  si NUM != NUM_EVT alors
    ID_EVT = ID_EVT + 1;
    GLOBAL.MAX_ID_EVT = ID_EVT;
    NUM_EVT = NUM;
  finsi
  champ_evenement(R, id_evt) = ID_EVT;
)

cible test_9YT:
application: iliad;
arguments: RESULTAT;
RESULTAT = (
  (GLOBAL.MONTANT_9YT dans (0, 7, 8, 10, 11, 17, 18))
  et positif(CORR.APPLI_ILIAD + 0)
  et (
    positif(CORR.CALCULIR + 0)
    ou (
      (CORR.NOTRAIT + 0) dans (16, 23, 26, 33, 36, 43, 46, 53, 56)
    )
  )
);

cible test_9XT:
application: iliad;
arguments: RESULTAT;
RESULTAT = (
  (GLOBAL.MONTANT_9XT dans (0, 7, 8, 10, 11, 17, 18, 34))
  et positif(CORR.APPLI_ILIAD + 0)
  et (positif(CORR.CALCULIR + 0) ou CORR.NOTRAIT + 0 >= 16)
);

cible test_mois_9XU:
application: iliad;
arguments: RESULTAT;
RESULTAT = (CORR.NOTRAIT + 0 >= 16 et vers_mois(GLOBAL.DATE_9XU) <= 12);

cible test_an_9XU:
application: iliad;
arguments: RESULTAT;
RESULTAT = (
  vers_annee(GLOBAL.DATE_9XU) = 0
  ou (
    vers_annee(GLOBAL.DATE_9XU) > GLOBAL.ANNEE_REVENU
    et vers_annee(GLOBAL.DATE_9XU) < GLOBAL.ANNEE_REVENU + 7
  )
);

cible transf_2042_rappels:
application: iliad;
variables_temporaires: NUM_RAP, EST_ISF;
NUM_RAP = 0;
iterer
: variable VAR
: categorie *
: avec present(VAR)
: espace CORR
: dans (
  si dans_domaine(VAR, saisie famille) alors
    GLOBAL.SF_PRIMITIF = 1;
  sinon_si dans_domaine(VAR, saisie contexte) alors
    GLOBAL.VAR = VAR;
  sinon
    calculer cible est_code_isf : avec EST_ISF, VAR;
    si positif(EST_ISF) alors
      si positif(GLOBAL.PRESENT_9XT) alors
        GLOBAL.VAR = indefini;
        GLOBAL.ISF_PRIM = 0;
        champ_evenement(NOUVEAU_RAPPEL, numero) = GLOBAL.PREMIER_EVT;
        champ_evenement(NOUVEAU_RAPPEL, rappel) = NUM_RAP;
        champ_evenement(NOUVEAU_RAPPEL, code) reference VAR;
        champ_evenement(NOUVEAU_RAPPEL, montant) = VAR;
        champ_evenement(NOUVEAU_RAPPEL, sens) = GLOBAL.SENS_9XT;
        champ_evenement(NOUVEAU_RAPPEL, penalite) = GLOBAL.MONTANT_9XT;
        champ_evenement(NOUVEAU_RAPPEL, date) = GLOBAL.DATE_9XU;
        champ_evenement(NOUVEAU_RAPPEL, 2042_rect) = GLOBAL.IND_20_9XT;
        champ_evenement(NOUVEAU_RAPPEL, base_tl) = GLOBAL.BASE_TL_9XT;
        champ_evenement(NOUVEAU_RAPPEL, anc_penalite) = 0;
        champ_evenement(NOUVEAU_RAPPEL, id_evt) = -1;
        champ_evenement(NOUVEAU_RAPPEL, strate) = -1;
        NUM_RAP = NUM_RAP + 1;
        NOUVEAU_RAPPEL = NOUVEAU_RAPPEL + 1;
      finsi
    sinon
      si positif(GLOBAL.PRESENT_9YT) alors
        GLOBAL.VAR = indefini;
        champ_evenement(NOUVEAU_RAPPEL, numero) = GLOBAL.PREMIER_EVT;
        champ_evenement(NOUVEAU_RAPPEL, rappel) = NUM_RAP;
        champ_evenement(NOUVEAU_RAPPEL, code) reference VAR;
        champ_evenement(NOUVEAU_RAPPEL, montant) = VAR;
        champ_evenement(NOUVEAU_RAPPEL, sens) = GLOBAL.SENS_9YT;
        champ_evenement(NOUVEAU_RAPPEL, penalite) = GLOBAL.MONTANT_9YT;
        champ_evenement(NOUVEAU_RAPPEL, date) = GLOBAL.DATE_9YU;
        champ_evenement(NOUVEAU_RAPPEL, 2042_rect) = GLOBAL.IND_20_9YT;
        champ_evenement(NOUVEAU_RAPPEL, base_tl) = GLOBAL.BASE_TL_9YT;
        champ_evenement(NOUVEAU_RAPPEL, anc_penalite) = 0;
        champ_evenement(NOUVEAU_RAPPEL, id_evt) = -1;
        champ_evenement(NOUVEAU_RAPPEL, strate) = -1;
        NUM_RAP = NUM_RAP + 1;
        NOUVEAU_RAPPEL = NOUVEAU_RAPPEL + 1;
      finsi
    finsi
  finsi
)

cible prepare_rappels_puis_calcul:
application: iliad;
variables_temporaires: TEST_9YT, TEST_9XT, TEST_MOIS_9XU, TEST_AN_9XU;
arranger_evenements
: ajouter NB_NOUVEAUX_RAPPELS
: dans (
  iterer
  : variable R
  : entre NB_RAPPELS..(nb_evenements() - 1) increment 1
  : dans (
    champ_evenement(R, numero) = -1;
  )
  NOUVEAU_RAPPEL = NB_RAPPELS;
  si positif(GLOBAL.PRESENT_9YT) et positif(GLOBAL.PRESENT_9YU) alors
    si GLOBAL.MONTANT_9YT != 0 et GLOBAL.MONTANT_9YU != 0 alors
      calculer cible transf_2042_rappels;
    sinon
      calculer cible transf_rappels_prim;
    finsi
  finsi
  si positif(GLOBAL.PRESENT_9XT) et positif(GLOBAL.PRESENT_9XU) alors
    si GLOBAL.MONTANT_9XT != 0 et GLOBAL.MONTANT_9XU != 0 alors
      calculer cible transf_2042_rappels;
    sinon
      calculer cible transf_rappels_prim;
    finsi
  finsi

  arranger_evenements
  : trier R1, R2 : avec
    si (champ_evenement(R1, numero) < champ_evenement(R2, numero)) alors (1)
    sinon (si (champ_evenement(R1, numero) > champ_evenement(R2, numero)) alors (0)
    sinon (si (champ_evenement(R1, rappel) < champ_evenement(R2, rappel)) alors (1)
    sinon (si (champ_evenement(R1, rappel) > champ_evenement(R2, rappel)) alors (0)
    sinon (1) finsi) finsi) finsi) finsi
  : filtrer R : avec
    champ_evenement(R, numero) >= 0
  : dans (
    calculer cible set_id_evt;
    iterer
    : variable R
    : entre 0..(nb_evenements() - 1) increment 1
    : dans (
      champ_evenement(R, strate) = -1;
    )
    si positif(GLOBAL.PRESENT_9YT) et positif(GLOBAL.PRESENT_9YU) alors
      calculer cible test_9YT : avec TEST_9YT;
      si non positif(TEST_9YT) alors
        nettoie_erreurs_finalisees;
        leve_erreur A96102;
        calculer cible signaler_erreur_ano;
      finsi
    finsi
    si positif(GLOBAL.PRESENT_9XT) et positif(GLOBAL.PRESENT_9XU) alors
      calculer cible test_9XT : avec TEST_9XT;
      si non positif(TEST_9XT) alors
        nettoie_erreurs_finalisees;
        leve_erreur A96103;
        calculer cible signaler_erreur_ano;
      finsi
      calculer cible test_mois_9XU : avec TEST_MOIS_9XU;
      si non positif(TEST_MOIS_9XU) alors
        nettoie_erreurs_finalisees;
        leve_erreur A96303;
        calculer cible signaler_erreur_ano;
      finsi
      calculer cible test_an_9XU : avec TEST_AN_9XU;
      si non positif(TEST_AN_9XU) alors
        nettoie_erreurs_finalisees;
        leve_erreur A96304;
        calculer cible signaler_erreur_ano;
      finsi
    finsi
    calculer cible calcul_aux;
  )
)

cible range_rappel:
application: iliad;
arguments: R;
calculer cible reset_codes_rappel;
CORR.PEN_RAPPEL = champ_evenement(R, penalite);
calculer cible verif_cohe_horizontale_corr;
calculer cible reset_codes_rappel;
si nb_anomalies() + nb_discordances() + nb_informatives() > 0 alors
  iterer
  : variable VAR
  : categorie *
  : espace CORR
  : dans (
    GLOBAL.VAR = VAR;
  )
  stop application;
finsi

cible calcul_avec_rappels:
application: iliad;
variables_temporaires: TROUVE_0AZ;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  calculer cible range_rappel : avec R;
)
TROUVE_0AZ = 0;
iterer
: variable R
: entre 0..(nb_evenements() - 1) increment 1
: dans (
  si
    non positif(TROUVE_0AZ)
    et meme_variable(champ_evenement(R, code), 0AZ)
  alors
    GLOBAL.ANNEE_DECES_CONJOINT = champ_evenement(R, montant);
    TROUVE_0AZ = 1;
  finsi
)
calculer cible prepare_rappels;
calculer cible prepare_rappels_puis_calcul;

cible calcul:
application: iliad;
calculer cible init_variables;
iterer
: variable VAR
: categorie *
: espace GLOBAL
: dans (
  CORR.VAR = VAR;
)
CORR.IND_TRAIT = 5; # correctif
si nb_evenements() = 0 alors
  arranger_evenements
  : ajouter 1
  : dans (
    champ_evenement(0, rappel) = 1;
    champ_evenement(0, numero) = 1;
    champ_evenement(0, code) reference ANREV;
    champ_evenement(0, sens) = SENS_R;
    champ_evenement(0, montant) = 0;
    champ_evenement(0, penalite) = 1;
    champ_evenement(0, base_tl) = 0;
    champ_evenement(0, date) = vers_date(1, GLOBAL.ANNEE_REVENU + 1);
    champ_evenement(0, 2042_rect) = 0;
    champ_evenement(0, anc_penalite) = 0;
    champ_evenement(0, id_evt) = -1;
    champ_evenement(0, strate) = -1;
    calculer cible calcul_avec_rappels;
 )
sinon
  calculer cible calcul_avec_rappels;
finsi
exporte_erreurs;
iterer
: variable VAR
: categorie *
: espace CORR
: dans (
  GLOBAL.VAR = VAR;
)

#{ tester l'existence des attributs dans les fonctions attribut(...),
   meme_variable(...), dans_domaine(...) }#
#{ contrôler les espaces dans l'usage des références }#
#{ dans_espace(...), dans_tableau(...) }#
#{ tester si la variable d'itération existe déjà ailleurs }#
#{ compléter exec_verif avec les fonctions sur les variables
   (attribut(...), etc.) }#
#{ cas des tableaux statiques avec les fonctions sur les variables
   (attribut(...), etc.) }#
#{ déplacer les expansions dans mir.ml }#

