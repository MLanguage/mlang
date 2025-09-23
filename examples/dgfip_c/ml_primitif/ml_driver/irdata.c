#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "mlang.h"

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

void nettoie_erreurs_finalisees(T_irdata *irdata) {
  int i = 0;

  for (i = 0; i < irdata->sz_err_finalise && irdata->err_finalise[i] != NULL; i++) {
    irdata->err_finalise[i] = NULL;
  }
  irdata->nb_err_finalise = 0;
}

void exporte_erreur(T_irdata *irdata) {
  int i = 0;

  for (i = 0; i < irdata->sz_err_finalise && irdata->err_finalise[i] != NULL; i++) {
    ajouter_espace(&irdata->sz_err_sortie, &irdata->err_sortie, irdata->nb_err_sortie);
    irdata->err_sortie[irdata->nb_err_sortie] = irdata->err_finalise[i];
    irdata->nb_err_sortie++;
  }
}

