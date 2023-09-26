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

cible ENCH_TL:
application: iliad;
calculer enchaineur ENCH_TL;

cible verif_calcul_primitive_isf_raw:
application: iliad;
verifier domaine isf : avec nb_categorie(calculee *) > 0;

cible verif_calcul_primitive_raw:
application: iliad;
calculer cible verif_calcul_primitive_isf_raw;
verifier domaine primitive : avec nb_categorie(calculee *) > 0;

cible verif_calcul_corrective_raw:
application: iliad;
calculer cible calcul_primitif_isf;
calculer cible verif_calcul_primitive_isf_raw;
verifier domaine corrective : avec nb_categorie(calculee *) > 0;

cible verif_saisie_cohe_primitive_isf_raw:
application: iliad;
verifier domaine isf
: avec nb_categorie(saisie *) > 0 et nb_categorie(calculee *) = 0;

cible verif_saisie_cohe_primitive_raw:
application: iliad;
calculer cible verif_saisie_cohe_primitive_isf_raw;
calculer cible calcul_primitif_isf;
calculer cible verif_calcul_primitive_isf_raw;
verifier domaine primitive
: avec nb_categorie(saisie *) > 0 et nb_categorie(calculee *) = 0;

cible verif_saisie_cohe_corrective_raw:
application: iliad;
calculer cible verif_saisie_cohe_primitive_isf_raw;
verifier domaine corrective
: avec nb_categorie(saisie *) > 0 et nb_categorie(calculee *) = 0;

cible verif_cohe_horizontale_raw:
application: iliad;
verifier domaine horizontale corrective;

cible verif_contexte_cohe_primitive_raw:
application: iliad;
verifier domaine primitive
: avec nb_categorie(saisie contexte) > 0 et nb_categorie(calculee *) = 0;

cible verif_contexte_cohe_corrective_raw:
application: iliad;
verifier domaine corrective
: avec nb_categorie(saisie contexte) > 0 et nb_categorie(calculee *) = 0;

cible verif_famille_cohe_primitive_raw:
application: iliad;
verifier domaine primitive
: avec nb_categorie(saisie famille) > 0 et nb_categorie(calculee *) = 0;

cible verif_famille_cohe_corrective_raw:
application: iliad;
verifier domaine corrective
: avec nb_categorie(saisie famille) > 0 et nb_categorie(calculee *) = 0;

cible verif_revenu_cohe_primitive_raw:
application: iliad;
verifier domaine primitive
: avec nb_categorie(saisie revenu) > 0 et nb_categorie(calculee *) = 0;

cible verif_revenu_cohe_corrective_raw:
application: iliad;
verifier domaine corrective
: avec nb_categorie(saisie revenu) > 0 et nb_categorie(calculee *) = 0;

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

cible article_1731_bis:
application : iliad;
si (V_IND_TRAIT = 4) alors # PRIMITIF
  si (CMAJ dans (8, 11)) alors
    ART1731BIS = 1;
    PREM8_11 = 1;
  sinon
    ART1731BIS = 0;
  finsi
finsi

cible compute_benefits:
application: iliad;
variable temporaire: EXISTE_VAR_ACOMPTE, SAUV_IRE, SAUV_INE, SAUV_IAD11;
EXISTE_VAR_ACOMPTE = 0;
iterer
: variable ITC : categorie saisie * : avec attribut(ITC, acompte) = 1
: dans (
  EXISTE_VAR_ACOMPTE = 1;
)
si (EXISTE_VAR_ACOMPTE = 1) alors
  restaurer
  : variable RESTREV
  : categorie saisie revenu
  : avec attribut(RESTREV, avfisc) = 1 et present(RESTREV) = 1
  : apres (
    iterer
    : variable ITREV
    : categorie saisie revenu, saisie revenu corrective
    : avec attribut(ITREV, avfisc) = 1 et present(ITREV) = 1
    : dans (
      ITREV = indefini;
    )
    V_INDTEO = 1;
    V_CALCUL_NAPS = 1;
    calculer cible calcul_primitif;
    V_CALCUL_NAPS = 0;
    SAUV_IAD11 = IAD11 + 0;
    SAUV_INE = INE + 0;
    SAUV_IRE = IRE + 0;
    PREM8_11 = PREM8_11 + 0;
  )
  V_IAD11TEO = SAUV_IAD11;
  V_IRETEO = SAUV_IRE;
  V_INETEO = SAUV_INE;
finsi

cible compute_double_liquidation3:
application: iliad;
variable temporaire: EXISTE_VAR_AVFISC, V_8GZ;
FLAG_ACO = 0;
V_NEGACO = 0;
V_AVFISCOPBIS = 0;
V_DIFTEOREEL = 0;
PREM8_11 = 0;
calculer cible article_1731_bis;
EXISTE_VAR_AVFISC = 0;
iterer
: variable ITC
: categorie saisie *
: avec attribut(ITC, avfisc) = 1
: dans (
  EXISTE_VAR_AVFISC = 1;
)
V_8GZ = IRANT;
si (present(IRANT) et EXISTE_VAR_AVFISC = 1) alors
  IRANT = indefini;
finsi
si (EXISTE_VAR_AVFISC = 1) alors
  V_AVFISCOPBIS = 0;
  V_DIFTEOREEL = 0;
  V_INDTEO = 1;
  calculer cible compute_benefits;
  V_INDTEO = 0;
  V_NEGREEL = 1;
  V_NAPREEL = 0;
finsi
IRANT = V_8GZ;
V_ACO_MTAP = 0;
V_NEGACO = 0;
calculer cible calcul_primitif_isf;
calculer cible calcul_primitif;
calculer cible calcul_primitif_taux;

cible compute_double_liquidation_exit_taxe:
application: iliad;
variable temporaire: ANNEE_FIXME;
ANNEE_FIXME = 2018;
si (present(PVIMPOS) ou present(CODRWB)) alors
  FLAG_EXIT = 1;
  FLAG_3WBNEG = 0;
  calculer cible compute_double_liquidation3;
  si (present(NAPTIR)) alors
    si (NAPTIR < 0) alors
      FLAG_3WBNEG = 1;
    finsi
    V_NAPTIR3WB = abs(NAPTIR);
  finsi
  si (ANNEE_FIXME >= 2017 et present(IHAUTREVT)) alors
    V_CHR3WB = IHAUTREVT;
  finsi
  si (ANNEE_FIXME >= 2018 et present(ID11)) alors
    V_ID113WB = ID11;
  finsi
  FLAG_EXIT = 0;
finsi
si (present(PVSURSI) ou present(CODRWA)) alors
  FLAG_3WANEG = 0;
  FLAG_EXIT = 2;
  calculer cible compute_double_liquidation3;
  si (present(NAPTIR)) alors
    si (NAPTIR < 0) alors
      FLAG_3WANEG = 1;
    finsi
    V_NAPTIR3WA = abs(NAPTIR);
  finsi
  si (ANNEE_FIXME >= 2017 et present(IHAUTREVT)) alors
    V_CHR3WA = IHAUTREVT;
  finsi
  si (ANNEE_FIXME >= 2018 et present(ID11)) alors
    V_ID113WA = ID11;
  finsi
  FLAG_EXIT = 0;
finsi
si (ANNEE_FIXME >= 2018) alors
  FLAG_BAREM = 1;
  calculer cible compute_double_liquidation3;
  si (present(RASTXFOYER)) alors
    V_BARTXFOYER = RASTXFOYER;
  finsi
  si (present(RASTXDEC1)) alors
    V_BARTXDEC1 = RASTXDEC1;
  finsi
  si (present(RASTXDEC2)) alors
    V_BARTXDEC2 = RASTXDEC2;
  finsi
  si (present(INDTAZ)) alors
    V_BARINDTAZ = INDTAZ;
  finsi
  si (IITAZIR >= 0) alors
    FLAG_BARIITANEG = 0;
    V_BARIITAZIR = IITAZIR;
  sinon
    FLAG_BARIITANEG = 1;
    V_BARIITAZIR = - IITAZIR;
  finsi
  si (present(IRTOTAL)) alors
    V_BARIRTOTAL = IRTOTAL;
  finsi
  FLAG_BAREM = 0;
  calculer cible compute_double_liquidation3;
finsi

cible compute_double_liquidation_pvro:
application: iliad;
APPLI_OCEANS = 0;
V_IND_TRAIT = 4;
si (present(COD3WG)) alors
  FLAG_PVRO = 1;
  calculer cible compute_double_liquidation_exit_taxe;
  si (present(IAD11)) alors
    V_IPVRO = IAD11;
  finsi
finsi
FLAG_PVRO = 0;
calculer cible compute_double_liquidation_exit_taxe;

# primitif ml

cible effacer_base_etc:
application : iliad;
iterer
: variable ITBASE
: categorie calculee base *
: dans (
  ITBASE = indefini;
)

cible effacer_calculee_etc:
application : iliad;
iterer
: variable ITCAL
: categorie calculee, calculee restituee
: dans (
  ITCAL = indefini;
)

cible calcule_avfiscal:
application: iliad;
variable temporaire: EXISTE_AVFISC, SAUV_IAD11, SAUV_INE, SAUV_IRE, SAUV_ART1731BIS, SAUV_PREM8_11;
EXISTE_AVFISC = 0;
iterer
: variable ITREV
: categorie saisie revenu, saisie revenu corrective
  : avec attribut(ITREV, avfisc) = 1 et present(ITREV)  
: dans (
  EXISTE_AVFISC = 1;
)
si (EXISTE_AVFISC = 1) alors
  restaurer
  : variable RESTREV
  : categorie saisie revenu, saisie revenu corrective
  : avec attribut(RESTREV, avfisc) = 1 et present(RESTREV)
  : apres (
    iterer
    : variable ITREV
    : categorie saisie revenu, saisie revenu corrective
    : avec attribut(ITREV, avfisc) = 1 et present(ITREV)
    : dans (
      ITREV = indefini;
    )
    V_INDTEO = 1;
    V_CALCUL_NAPS = 1;
    si (V_IND_TRAIT = 4) alors # PRIMITIF
      calculer cible calcul_primitif;
    sinon
      calculer cible calcul_correctif;
    finsi
    V_CALCUL_NAPS = 0;
    SAUV_ART1731BIS = ART1731BIS + 0;
    SAUV_PREM8_11 = PREM8_11 + 0;
    SAUV_IAD11 = IAD11;
    SAUV_INE = INE;
    SAUV_IRE = IRE;
    calculer cible effacer_calculee_etc;
    si (V_IND_TRAIT = 4) alors # PRIMITIF
      calculer cible effacer_base_etc;
      ART1731BIS = SAUV_ART1731BIS;
      PREM8_11 = SAUV_PREM8_11;
    finsi
  )
  V_IAD11TEO = SAUV_IAD11;
  V_IRETEO = SAUV_IRE;
  V_INETEO = SAUV_INE;
sinon
  iterer
  : variable ITREV
  : categorie saisie revenu, saisie revenu corrective
  : avec attribut(ITREV, avfisc) = 1 et present(ITREV)
  : dans (
    ITREV = indefini;
  )
finsi

# debug

cible toto:
application: iliad;
afficher "toto " "FLAG_PVRO=" (FLAG_PVRO) " tutu" "\n";
afficher_erreur "toto " nom(FLAG_PVRO) " " alias(FLAG_PVRO) "+27.745=" (FLAG_PVRO + 27.745) " tutu " (indefini) "\n";
afficher_erreur "toto " "27.745=" (0 + 27.745) : 0 .. 2 " tutu " (3 * indefini) "\n";

cible tutu:
application: iliad;
iterer
: variable ITC
: categorie saisie revenu
: avec attribut(ITC, acompte) = 0
: dans (
  afficher_erreur "tutu0 " nom(ITC) " (" alias(ITC) ") = " (ITC) : 0..2 "\n";
  afficher_erreur "tutu1 attribut(" nom(ITC) ", acompte) = " (attribut(ITC, acompte)) : 0 "\n";
  afficher_erreur "tutu1 attribut(" nom(V_VAR7WZ) ", acompte) = " (attribut(V_VAR7WZ, acompte)) : 0 "\n";
)

cible titi:
application : iliad;
variable temporaire: TOTO tableau[3];
TOTO[0] = 0;
TOTO[1] = 1 + TOTO[0];
TOTO[2] = 2 + TOTO[1];
afficher_erreur "titi debut\n";
afficher_erreur "titi0 TOTO[0] = " (TOTO[0]) " TOTO[1] = " (TOTO[1]) " TOTO[2] = " (TOTO[2]) "\n";
afficher_erreur "titi0 " nom(FLAG_PVRO) " = " (FLAG_PVRO) "\n";
iterer
: variable ITC : categorie saisie contexte : avec present(ITC)
: dans (
  afficher_erreur "titi0 " nom(ITC) " = " (ITC) "\n";
)
afficher_erreur "\n";
restaurer
: FLAG_PVRO
: TOTO
: variable RESTREV : categorie saisie contexte : avec present(RESTREV)
: apres (
  FLAG_PVRO = indefini;
  afficher_erreur "titi1 " nom(FLAG_PVRO) " = " (FLAG_PVRO) "\n";
  TOTO[0] = indefini;
  TOTO[1] = indefini;
  TOTO[2] = indefini;
  afficher_erreur "titi1 TOTO[0] = " (TOTO[0]) " TOTO[1] = " (TOTO[1]) " TOTO[2] = " (TOTO[2]) "\n";
  iterer
  : variable ITC : categorie saisie contexte : avec present(ITC)
  : dans (
    ITC = indefini;
    afficher_erreur "titi1 " nom(ITC) " = " (ITC) "\n";
  )
)
afficher_erreur "\n";
afficher_erreur "toiti2 TOTO[0] = " (TOTO[0]) " TOTO[1] = " (TOTO[1]) " TOTO[2] = " (TOTO[2]) "\n";
afficher_erreur "titi2 " nom(FLAG_PVRO) " = " (FLAG_PVRO) "\n";
iterer
: variable ITC : categorie saisie contexte : avec present(ITC)
: dans (
  afficher_erreur "titi2 " nom(ITC) " = " (ITC) "\n";
)
afficher_erreur "titi fin\n\n";


