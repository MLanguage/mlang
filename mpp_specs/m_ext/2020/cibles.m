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

cible article_1731_bis:
application : iliad;
# calculer cible toto;
si (V_IND_TRAIT = 4) alors # PRIMITIF
  si (CMAJ dans (8, 11)) alors
    ART1731BIS = 1;
    PREM8_11 = 1;
  sinon
    ART1731BIS = 0;
  finsi
finsi

cible toto:
application: iliad;
afficher "toto " "FLAG_PVRO=" (FLAG_PVRO) " tutu" "\n";
afficher_erreur "toto " "FLAG_PVRO+27.745=" (FLAG_PVRO + 27.745) " tutu " (indefini) "\n";
afficher_erreur "toto " "27.745=" (0 + 27.745) : 0 .. 2 " tutu " (3 * indefini) "\n";

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

