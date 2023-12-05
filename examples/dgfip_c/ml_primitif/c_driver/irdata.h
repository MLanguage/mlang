/****** LICENCE CECIL *****/

#ifndef _IRDATA_H_
#define _IRDATA_H_

#include "conf.h"
#include "annee.h"

#if defined(__GNUC__) || defined(__STDC__)
#define _PROTS(X) X
#else
#define _PROTS(X) ()
#endif

#define TYPE_CONTEXTE       0
#define TYPE_FAMILLE        1
#define TYPE_REVENU         2
#define TYPE_REVENU_CORREC  3
#define TYPE_VARIATION      4
#define TYPE_RESTITUEE      5
#define TYPE_RESTITUEE_P    6
#define TYPE_RESTITUEE_C    7
#define TYPE_PENALITE       8
#define TYPE_BASE           10

#ifdef FLG_DEBUG
#define TYPE_DEBUG  6
#endif /* FLG_DEBUG */

/* Gestion des erreurs */
#define ERREUR_DELIMITEUR  " / "
#define ERREUR_SEPARATEUR  "-"

/* Couleurs */
#define NOIR          30
#define ROUGE         31
#define VERT          32
#define JAUNE         33
#define BLEU          34
#define MAGENTA       35
#define CYAN          36
#define BLANC         37
#define FOND_ROUGE    41
#define FOND_VERT     42
#define FOND_JAUNE    43
#define FOND_BLEU     44
#define FOND_MAGENTA  45
#define FOND_CYAN     46
#define FOND_BLANC    47
#define NORMAL        0
#define GRAS          1
#define STANDOUT      3
#define SOULIGNE      4

typedef int T_indice;
typedef long T_type;
typedef int T_classe;
typedef int T_priorite;
typedef int T_categorie_TL;
typedef int T_cotsoc;
typedef int T_ind_abat;
typedef int T_acompte;
typedef int T_avfisc;
typedef int T_rapcat;
typedef int T_sanction;
typedef int T_modcat;
typedef int T_nat_code;
typedef int T_typezone2042;

typedef struct S_irdata T_irdata;
typedef struct S_discord T_discord;
typedef struct S_erreur T_erreur;

extern int IRDATA_annee_revenu(void);

extern struct S_irdata * IRDATA_new_irdata _PROTS((void));
extern void IRDATA_delete_irdata _PROTS((struct S_irdata *irdata));

#ifdef FLG_MULTITHREAD
extern void IRDATA_reset_erreur _PROTS((struct S_irdata *irdata));
extern void IRDATA_set_max_bloquantes _PROTS((struct S_irdata *irdata, const int max_ano));
#endif /* FLG_MULTITHREAD */

extern void IRDATA_reset_irdata _PROTS((struct S_irdata *irdata));
extern void IRDATA_reset_light _PROTS((struct S_irdata *irdata));
extern void IRDATA_reset_base _PROTS((struct S_irdata *irdata));
extern void IRDATA_reset_calculee _PROTS((struct S_irdata *irdata));

#ifdef FLG_CORRECTIF
extern void IRDATA_recopie_irdata _PROTS((const struct S_irdata *irdata_src, struct S_irdata *irdata_dst));
extern void IRDATA_recopie_irdata_light _PROTS((const struct S_irdata *irdata_src, struct S_irdata *irdata_dst));
#endif /* FLG_CORRECTIF */

extern void IRDATA_efface _PROTS((struct S_irdata *irdata, T_var_irdata var_irdata));
extern void IRDATA_efface_tableau _PROTS((struct S_irdata *irdata, T_var_irdata var_irdata, int ind));

extern struct S_discord * IRDATA_range _PROTS((struct S_irdata *irdata, T_var_irdata var_irdata, double valeur));
extern void IRDATA_range_base _PROTS((struct S_irdata *irdata, T_var_irdata var_irdata, double valeur));
extern struct S_discord * IRDATA_range_tableau _PROTS((struct S_irdata *irdata, T_var_irdata var_irdata, int ind, double valeur));

extern void IRDATA_add_valeur _PROTS((struct S_irdata *irdata, T_var_irdata var_irdata, double valeur));

extern struct S_discord * IRDATA_controle_valeur _PROTS((struct S_irdata *irdata, T_var_irdata var_irdata, double valeur));

extern double * IRDATA_extrait _PROTS((struct S_irdata *irdata, T_var_irdata var_irdata));
extern double * IRDATA_extrait_special _PROTS((struct S_irdata *irdata, T_var_irdata var_irdata));
extern double * IRDATA_extrait_tableau _PROTS((struct S_irdata *irdata, T_var_irdata var_irdata, int ind));

#ifdef FLG_EXTRACTION
typedef struct S_extraction T_extraction;
extern T_extraction * IRDATA_init_extraction(void);
extern void IRDATA_detruit_extraction _PROTS((T_extraction *));
extern T_var_irdata IRDATA_extraction_suivante _PROTS((T_extraction *, int type));
extern void IRDATA_reset_extraction(void);
#endif /* FLG_EXTRACTION */

typedef struct S_table_extraction T_table_extraction;
extern T_table_extraction *IRDATA_init_table_extraction _PROTS((int));
extern void IRDATA_detruit_table_extraction _PROTS((T_table_extraction *));
extern T_var_irdata IRDATA_table_extraction_suiv _PROTS((T_table_extraction *));



extern char * IRDATA_cherche_revenu _PROTS((const char *nom));
extern T_var_irdata IRDATA_get_var_irdata _PROTS((const char *nom, T_typezone2042 type));
extern int IRDATA_est_du_type _PROTS((T_var_irdata desc, T_typezone2042 type));
extern char * IRDATA_get_nom _PROTS((T_var_irdata var_irdata));
extern char * IRDATA_get_lib _PROTS((T_var_irdata var_irdata));
extern T_classe IRDATA_get_classe _PROTS((T_var_irdata var_irdata));
extern T_priorite IRDATA_get_priorite _PROTS((T_var_irdata var_irdata));
extern T_priorite IRDATA_get_categorie_TL _PROTS((T_var_irdata var_irdata));
extern T_cotsoc IRDATA_get_cotsoc _PROTS((T_var_irdata var_irdata));
extern T_ind_abat IRDATA_get_ind_abat _PROTS((T_var_irdata var_irdata));
extern T_acompte IRDATA_get_acompte _PROTS((T_var_irdata var_irdata));
extern T_avfisc IRDATA_get_avfisc _PROTS((T_var_irdata var_irdata));
extern T_rapcat IRDATA_get_rapcat _PROTS((T_var_irdata var_irdata));
extern T_sanction IRDATA_get_sanction _PROTS((T_var_irdata var_irdata));
extern T_modcat IRDATA_get_modcat _PROTS((T_var_irdata var_irdata));
extern T_nat_code IRDATA_get_nat_code _PROTS((T_var_irdata var_irdata));
extern T_var_irdata IRDATA_get_liee _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_get_type _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_booleen _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_numerique _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_date _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_date_JJMMAAAA _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_date_MMAAAA _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_date_AAAA _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_date_JJMM _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_date_MM _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_entier _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_reel _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_reel1 _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_reel2 _PROTS((T_var_irdata var_irdata));
extern T_type IRDATA_est_reel3 _PROTS((T_var_irdata var_irdata));



typedef struct S_desc_ench *T_ench_calcul;
extern T_ench_calcul IRDATA_get_ench_calcul _PROTS((const char *ref));
extern void IRDATA_exec_ench _PROTS((struct S_irdata *irdata, T_ench_calcul p_ench));

#ifdef FLG_DEBUG
typedef struct S_desc_call *T_regle_calcul;
extern T_regle_calcul IRDATA_get_regle_calcul _PROTS((int ref));
extern void IRDATA_exec_calcul _PROTS((struct S_irdata *irdata, T_regle_calcul p_calcul));
#endif /* FLG_DEBUG */

#if defined(FLG_DEBUG) || defined(FLG_CONTROLE_IMMEDIAT)
typedef struct S_desc_verif *T_regle_verif;
extern T_regle_verif IRDATA_get_regle_verif _PROTS((int ref));
extern struct S_discord * IRDATA_exec_verif _PROTS((struct S_irdata *irdata, T_regle_verif p_verif));
#endif /* FLG_DEBUG || FLG_CONTROLE_IMMEDIAT */



/* Gestion des erreurs */

char IRDATA_GetFamille_erreur _PROTS((const T_discord *discord));
const char * IRDATA_GetCodeBo_erreur _PROTS((T_discord *discord));
const char * IRDATA_GetSousCode_erreur _PROTS((T_discord *discord));
char IRDATA_GetIsIsf_erreur _PROTS((const T_discord *discord));
const char * IRDATA_GetLibelle_erreur _PROTS((T_discord *discord));
const char * IRDATA_GetCodeInterne_erreur _PROTS((T_discord *discord));
short IRDATA_GetType_erreur _PROTS((const T_discord * pp_discord));

extern char * IRDATA_lib_erreur _PROTS((struct S_discord *discord));
extern char * IRDATA_codebo_erreur _PROTS((struct S_discord *discord));
extern short IRDATA_type_erreur _PROTS((const struct S_discord *discord));
extern int IRDATA_erreur_bloquante _PROTS((const struct S_discord *discord));
extern char * IRDATA_nom_erreur _PROTS((struct S_discord *discord));
extern struct S_discord * IRDATA_erreur_suivante _PROTS((struct S_discord *discord));

#ifdef FLG_DEBUG
extern struct S_desc_err * IRDATA_get_desc_err _PROTS((const char *nom));
extern char * IRDATA_desc_err_lib_erreur _PROTS((struct S_desc_err *desc_err));
extern char * IRDATA_desc_err_codebo_erreur _PROTS((struct S_desc_err *desc_err));
extern short IRDATA_desc_err_type_erreur _PROTS((const struct S_desc_err *desc_err));
extern int IRDATA_desc_err_erreur_bloquante _PROTS((const struct S_desc_err *desc_err));
extern char * IRDATA_desc_err_nom_erreur _PROTS((struct S_desc_err *desc));
#endif /* FLG_DEBUG */



/* Fonctions définies dans enchain.c */

#if ANNEE_REVENU > 2005
extern struct S_discord * err_NEGATIF _PROTS((T_irdata *irdata));
extern void nettoie_erreur _PROTS((T_irdata *irdata));
#endif /* ANNEE_REVENU > 2005 */

#if ANNEE_REVENU > 2010
extern struct S_discord * err_BOOLEEN _PROTS((T_irdata *irdata));
#endif /* ANNEE_REVENU > 2010 */

extern void initialisation_primitive _PROTS((T_irdata *irdata));

#ifdef FLG_CORRECTIF
extern void initialisation_corrective _PROTS((T_irdata *irdata));
#endif /* FLG_CORRECTIF */

extern struct S_discord * verif_calcul_primitive _PROTS((T_irdata *irdata));
extern struct S_discord * verif_calcul_primitive_isf _PROTS((T_irdata *irdata));

#ifdef FLG_CORRECTIF
extern struct S_discord * verif_calcul_corrective _PROTS((T_irdata *irdata));
#endif /* FLG_CORRECTIF */

#ifndef FLG_CONTROLE_IMMEDIAT

extern struct S_discord * verif_saisie_cohe_primitive _PROTS((T_irdata *irdata));
extern struct S_discord * verif_saisie_cohe_primitive_isf _PROTS((T_irdata *irdata, int appel));

#ifdef FLG_CONTROLE_SEPARE
#define CONTROLE_SEPARE
extern struct S_discord * verif_contexte_cohe_primitive _PROTS((T_irdata *irdata));
extern struct S_discord * verif_famille_cohe_primitive _PROTS((T_irdata *irdata));
extern struct S_discord * verif_revenu_cohe_primitive _PROTS((T_irdata *irdata));
#endif /* CONTROLE_SEPARE */

#ifdef FLG_CORRECTIF
extern struct S_discord * verif_saisie_cohe_corrective _PROTS((T_irdata *irdata));

#ifdef FLG_CONTROLE_SEPARE
extern struct S_discord * verif_contexte_cohe_corrective _PROTS((T_irdata *irdata));
extern struct S_discord * verif_famille_cohe_corrective _PROTS((T_irdata *irdata));
extern struct S_discord * verif_revenu_cohe_corrective _PROTS((T_irdata *irdata));
#endif /* FLG_CONTROLE_SEPARE */

#endif /* FLG_CORRECTIF */

#endif /* FLG_CONTROLE_IMMEDIAT */

#ifdef FLG_CORRECTIF
extern struct S_discord * verif_cohe_horizontale _PROTS((T_irdata *irdata));
#endif /* FLG_CORRECTIF */

#endif /* _IRDATA_H_ */

