/****** LICENCE CECIL *****/

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <float.h>

#include "mlang.h"

#ifndef isinf
int isinf(double d) {
  return d < -DBL_MAX || DBL_MAX < d;
}
#endif /* isinf */

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

void nettoie_erreurs_finalisees(T_irdata *irdata) {
  int i = 0;

  for (i = 0; i < irdata->sz_err_finalise && irdata->err_finalise[i] != NULL; i++) {
    irdata->err_finalise[i] = NULL;
  }
  irdata->nb_err_finalise = 0;
}

void finalise_erreur_corr(T_irdata *irdata) {
  int i = 0;
  int trouve = 0;
  int ano = 0;
  T_discord *pDisco = irdata->discords;

  while (pDisco != NULL && ano == 0) {
    trouve = 0;
    for (i = 0; i < irdata->nb_err_finalise && ! trouve; i++) {
      if (strcmp(pDisco->erreur->nom, irdata->err_finalise[i]) == 0) {
        trouve = 1;
      }
    }
    if (! trouve) {
      ajouter_espace(&irdata->sz_err_finalise, &irdata->err_finalise, irdata->nb_err_finalise);
      irdata->err_finalise[irdata->nb_err_finalise] = pDisco->erreur->nom;
      irdata->nb_err_finalise++;
      if (pDisco->erreur->type == ANOMALIE) ano = 1;
    }
    pDisco = pDisco->suivant;
  }
}

void exporte_erreur_corr(T_irdata *irdata) {
  int i = 0;
  int j = 0;
  int trouve = 0;

  for (i = 0; i < irdata->nb_err_finalise; i++) {
    trouve = 0;
    for (j = 0; j < irdata->nb_err_archive && ! trouve; j++) {
      if (strcmp(irdata->err_finalise[i], irdata->err_archive[j]) == 0) {
        trouve = 1;
      }
    }
    if (! trouve) {
      ajouter_espace(&irdata->sz_err_archive, &irdata->err_archive, irdata->nb_err_archive);
      irdata->err_archive[irdata->nb_err_archive] = irdata->err_finalise[i];
      irdata->nb_err_archive++;
      ajouter_espace(&irdata->sz_err_sortie, &irdata->err_sortie, irdata->nb_err_sortie);
      irdata->err_sortie[irdata->nb_err_sortie] = irdata->err_finalise[i];
      irdata->nb_err_sortie++;
      irdata->err_finalise[i] = NULL;
    }
  }
  nettoie_erreurs_finalisees(irdata);
}

void finalise_erreur_prim(T_irdata *irdata) {
  int i = 0;
  int trouve = 0;
  T_discord *pDisco = irdata->discords;

  nettoie_erreurs_finalisees(irdata);
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

void exporte_erreur_prim(T_irdata *irdata) {
  int i = 0;

  for (i = 0; i < irdata->nb_err_finalise; i++) {
    ajouter_espace(&irdata->sz_err_sortie, &irdata->err_sortie, irdata->nb_err_sortie);
    irdata->err_sortie[irdata->nb_err_sortie] = irdata->err_finalise[i];
    irdata->nb_err_sortie++;
  }
  nettoie_erreurs_finalisees(irdata);
}

void finalise_erreur(T_irdata *irdata) {
  T_varinfo *mode_corr = cherche_varinfo(irdata, "MODE_CORR");
  char def = 0;
  double val = 0.0;
  if (mode_corr == NULL) {
    finalise_erreur_prim(irdata);
    return;
  }
  lis_varinfo(irdata, ESPACE_PAR_DEFAUT, mode_corr, &def, &val);
  if (def == 1 && val == 1.0) {
    finalise_erreur_corr(irdata);
  } else {
    finalise_erreur_prim(irdata);
  }
}

void exporte_erreur(T_irdata *irdata) {
  T_varinfo *mode_corr = cherche_varinfo(irdata, "MODE_CORR");
  char def = 0;
  double val = 0.0;
  if (mode_corr == NULL) {
    exporte_erreur_prim(irdata);
    return;
  }
  lis_varinfo(irdata, ESPACE_PAR_DEFAUT, mode_corr, &def, &val);
  if (def == 1 && val == 1.0) {
    exporte_erreur_corr(irdata);
  } else {
    exporte_erreur_prim(irdata);
  }
}

