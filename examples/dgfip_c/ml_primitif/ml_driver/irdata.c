
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "irdata.h"

T_irdata *IRDATA_new_irdata(void) {
  return cree_irdata();
}

void IRDATA_delete_irdata(T_irdata *irdata) {
  detruis_irdata(irdata);
}

void IRDATA_reset_irdata(T_irdata *irdata) {
  init_saisie(irdata);
  init_calculee(irdata);
  init_base(irdata);
  init_erreur(irdata);
}

void IRDATA_reset_base(T_irdata *irdata) {
  init_base(irdata);
}

void IRDATA_reset_light(T_irdata *irdata) {
  init_saisie(irdata);
  init_calculee(irdata);
}

void IRDATA_reset_calculee(T_irdata *irdata) {
  init_calculee(irdata);
}

void IRDATA_recopie_irdata(T_irdata *irdata_src, T_irdata *irdata_dst) {
  recopie_saisie(irdata_src, irdata_dst);
  recopie_calculee(irdata_src, irdata_dst);
  recopie_base(irdata_src, irdata_dst);
}

void IRDATA_reset_erreur(T_irdata *irdata) {
  init_erreur(irdata);
}
/*
void IRDATA_range_base(T_irdata *irdata, T_var_irdata desc, double val) {
  T_desc_var *desc_var = (T_desc_var *)desc;
  int est_type = desc_var->indice & EST_MASQUE;
  int idx = desc_var->indice & INDICE_VAL;

  switch (est_type) {
    case EST_SAISIE:
      ecris_saisie(irdata, idx, 1, val);
      break;
    case EST_CALCULEE:
      ecris_calculee(irdata, idx, 1, val);
      break;
    case EST_BASE:
      ecris_base(irdata, idx, 1, val);
      break;
  }
}
*/

void IRDATA_range_base(T_irdata *irdata, T_var_irdata p_desc, double valeur) {
  T_desc_var *desc = (T_desc_var *)p_desc;
  int indice = desc->indice & INDICE_VAL;
  switch (desc->indice & EST_MASQUE) {
    case EST_SAISIE:
      irdata->def_saisie[indice] = 1;
      irdata->saisie[indice] = valeur;
      break;
    case EST_CALCULEE:
      irdata->def_calculee[indice] = 1;
      irdata->calculee[indice] = valeur;
      break;
    case EST_BASE:
      irdata->def_base[indice] = 1;
      irdata->base[indice] = valeur;
      break;
  }
}
/*
void IRDATA_efface(T_irdata *irdata, T_var_irdata desc) {
  T_desc_var *desc_var = (T_desc_var *)desc;
  int est_type = desc_var->indice & EST_MASQUE;
  int idx = desc_var->indice & INDICE_VAL;

  if (est_type != EST_SAISIE) return;
  ecris_saisie(irdata, idx, 0, 0);
}
*/

void IRDATA_efface(T_irdata *irdata, T_var_irdata p_desc) {
  T_desc_var *desc = (T_desc_var *)p_desc;
  int indice = 0;
  indice = desc->indice & INDICE_VAL;
  if ((desc->indice & EST_MASQUE) != EST_SAISIE) {
    return;
  }
  irdata->saisie[indice] = 0;
  irdata->def_saisie[indice] = 0;
  return;
}
/*
double *IRDATA_extrait_special(T_irdata *irdata, T_var_irdata desc) {
  double *retour = NULL;
  T_desc_var *desc_var = (T_desc_var *)desc;
  int est_type = desc_var->indice & EST_MASQUE;
  int idx = desc_var->indice & INDICE_VAL;

  switch (est_type) {
    case EST_SAISIE:
      if (lis_saisie_def(irdata, idx) == 0) retour = NULL;
      else retour = lis_saisie_val_ref(irdata, idx);
      break;
    case EST_CALCULEE:
      if (lis_calculee_def(irdata, idx) == 0) retour = NULL;
      else retour = lis_calculee_val_ref(irdata, idx);
      break;
    case EST_BASE:
      if (lis_base_def(irdata, idx) == 0) retour = NULL;
      else retour = lis_base_val_ref(irdata, idx);
      break;
    default:
      retour = NULL;
      break;
  }
  return retour;
}
*/

double *IRDATA_extrait_special(T_irdata *irdata, T_var_irdata p_desc) {
  T_desc_var *desc = (T_desc_var *)p_desc;
  double *res = NULL;
  int indice = desc->indice & INDICE_VAL;
  switch (desc->indice & EST_MASQUE) {
    case EST_SAISIE:
      res = (irdata->def_saisie[indice] == 0) ? NULL : &irdata->saisie[indice];
      break;
    case EST_CALCULEE:
      res = (irdata->def_calculee[indice] == 0) ? NULL : &irdata->calculee[indice];
      break;
    case EST_BASE:
      res = (irdata->def_base[indice] == 0) ? NULL : &irdata->base[indice];
      break;
    default:
      res = NULL;
      break;
  }
  return res;
}
/*
double *IRDATA_extrait_tableau(T_irdata *irdata, T_var_irdata desc, int ind) {
  double *retour = NULL;
  T_desc_var *desc_var = (T_desc_var *)desc;
  int est_type = desc_var->indice & EST_MASQUE;
  int idx = (desc_var->indice & INDICE_VAL) + ind;

  switch (est_type) {
    case EST_SAISIE:
      if (lis_saisie_def(irdata, idx) == 0) retour = NULL;
      else retour = lis_saisie_val_ref(irdata, idx);
      break;
    case EST_CALCULEE:
      if (lis_calculee_def(irdata, idx) == 0) retour = NULL;
      else retour = lis_calculee_val_ref(irdata, idx);
      break;
    case EST_BASE:
      if (lis_base_def(irdata, idx) == 0) retour = NULL;
      else retour = lis_base_val_ref(irdata, idx);
      break;
    default:
      retour = NULL;
      break;
  }
  return retour;
}
*/

double *IRDATA_extrait_tableau(T_irdata *irdata, T_var_irdata p_desc, int ind) {
  T_desc_var *desc = (T_desc_var *)p_desc;
  double *res = NULL;
  int indice = (desc->indice & INDICE_VAL) + ind;
  switch (desc->indice & EST_MASQUE) {
    case EST_SAISIE:
      res = (irdata->def_saisie[indice] == 0) ? NULL : &irdata->saisie[indice];
      break;
    case EST_CALCULEE:
      res = (irdata->def_calculee[indice] == 0) ? NULL : &irdata->calculee[indice];
      break;
    case EST_BASE:
      res = (irdata->def_base[indice] == 0) ? NULL : &irdata->base[indice];
      break;
    default:
      res = NULL;
      break;
  }
  return res;
}

/* Gestion des erreurs */

/*
 * # ajouter_espace(sz, tab, nb) #
 *
 * tab est un tableau dynamique de taille sz.
 * On demande à disposer de nb cases dans tab.
 * Si il n'y a pas assez de place dans tab, on double sa taille.
 */
void ajouter_espace(int *sz, char ***tab, int nb) {
  if (nb >= *sz) {
    int i = 0;
    int old_sz = *sz;
    if (*sz == 0) {
      *sz = 128;
    } else {
      while (nb >= *sz) {
        *sz *= 2;
      }
    }
    if (*tab == NULL) {
      *tab = (char **)malloc(*sz * (sizeof (char *)));      
    } else {
      *tab = (char **)realloc(*tab, *sz * (sizeof (char *)));
    }
    for (i = old_sz; i < *sz; i++) {
      (*tab)[i] = NULL;
    }
  }
}

void finalise_erreur(T_irdata *irdata) {
  int i = 0;
  int trouve = 0;
  T_discord *pDisco = irdata->discords;
  irdata->nb_err_finalise = 0;

  while (pDisco != NULL) {
    trouve = 0;
    for (i = 0; i < irdata->nb_err_archive && ! trouve; i++) {
      if (strcmp(pDisco->erreur->nom, irdata->err_archive[i]) == 0) {
        trouve = 1;
      }
    }
    if (! trouve) {
      ajouter_espace(&irdata->sz_err_archive, &irdata->err_archive, irdata->nb_err_archive);
      irdata->err_archive[irdata->nb_err_archive] = pDisco->erreur->nom;
      irdata->nb_err_archive++;
      ajouter_espace(&irdata->sz_err_finalise, &irdata->err_finalise, irdata->nb_err_finalise);
      irdata->err_finalise[irdata->nb_err_finalise] = pDisco->erreur->nom;
      irdata->nb_err_finalise++;
    }
    pDisco = pDisco->suivant;
  }
}

void exporte_erreur(T_irdata *irdata) {
  int i = 0;

  for (i = 0; i < irdata->sz_err_finalise && irdata->err_finalise[i] != NULL; i++) {
    ajouter_espace(&irdata->sz_err_sortie, &irdata->err_sortie, irdata->nb_err_sortie);
    irdata->err_sortie[irdata->nb_err_sortie] = irdata->err_finalise[i];
    irdata->nb_err_sortie++;
  }
}

