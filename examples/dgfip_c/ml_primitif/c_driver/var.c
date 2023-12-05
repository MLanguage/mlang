/****** LICENCE CECIL *****/

#include "var.h"

int taille_saisie = TAILLE_SAISIE;
int taille_calculee = TAILLE_CALCULEE;
int taille_base = TAILLE_BASE;
int taille_totale = TAILLE_TOTALE;

int nb_contexte = NB_CONTEXTE;
int nb_famille = NB_FAMILLE;
int nb_revenu = NB_REVENU;
int nb_revenu_correc = NB_REVENU_CORREC;
int nb_variation = NB_VARIATION;
int nb_penalite = NB_PENALITE;
int nb_restituee = NB_RESTITUEE;
int nb_enchaine = NB_ENCH;

int color = 37;
int typo = 0;

#ifdef FLG_DEBUG
int nb_err = NB_ERR;
#if NB_DEBUG_C <= 0
int nb_debug = NB_DEBUG;
#else
int nb_debug01 = NB_DEBUG01;
int nb_debug02 = NB_DEBUG02;
int nb_debug03 = NB_DEBUG03;
int nb_debug04 = NB_DEBUG04;
#endif /* NB_DEBUG_C <= 0 */
int nb_call = NB_CALL;
#endif

#if defined (FLG_DEBUG) || defined (FLG_CONTROLE_IMMEDIAT)
int nb_verif = NB_VERIF;
#endif /* FLG_DEBUG || FLG_CONTROLE_IMMEDIAT */

T_discord * une_verif(T_irdata *irdata, struct S_discord *(*proc)(T_irdata *irdata)) {
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  (*proc)(irdata);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  (*proc)(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}

int change_couleur(int couleur, int typographie) {
  color = couleur;
  typo = typographie;
  return 0;
}

int get_couleur(void) {
  return color;
}

struct S_discord *verif_saisie_cohe_primitive_isf(T_irdata *irdata, int ret) {
  return verif_saisie_cohe_primitive_isf_raw(irdata);
}

T_discord * err_NEGATIF(T_irdata *irdata) {
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  add_erreur(irdata, &erreur_A000, NULL);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  add_erreur(irdata, &erreur_A000, NULL);
  return discords;

#endif /* FLG_MULTITHREAD */
}

