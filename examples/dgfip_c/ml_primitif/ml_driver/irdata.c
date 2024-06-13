
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "irdata.h"

static int alloc_tab(double **tab, char **def_tab, int taille)
{
  if ((tab == NULL) || (def_tab == NULL)) {
    return FALSE;
  }
  if (taille > 0) {
    *tab = (double *)malloc(taille * sizeof(double));
    if (*tab == NULL) {
      return FALSE;
    }
    *def_tab = (char *)malloc(taille * sizeof(char));
    if (*def_tab == NULL) {
      free(*tab);
      *tab = NULL;
      return FALSE;
    }
  }
  return TRUE;
}

T_irdata * IRDATA_new_irdata(void)
{
  T_irdata *irdata = NULL;
  if ((irdata = (T_irdata *)malloc(sizeof(T_irdata))) == NULL) {
    return NULL;
  }
  irdata->saisie = NULL;
  irdata->calculee = NULL;
  irdata->base = NULL;
  irdata->tmps = NULL;
  irdata->ref = NULL;
  irdata->def_saisie = NULL;
  irdata->def_calculee = NULL;
  irdata->def_base = NULL;
  irdata->def_tmps = NULL;
  irdata->def_ref = NULL;
  irdata->info_tmps = NULL;
  irdata->info_ref = NULL;
  if (alloc_tab(&irdata->saisie, &irdata->def_saisie, TAILLE_SAISIE) == FALSE) {
    IRDATA_delete_irdata(irdata);
    return NULL;
  }
  if (alloc_tab(&irdata->calculee, &irdata->def_calculee, TAILLE_CALCULEE) == FALSE) {
    IRDATA_delete_irdata(irdata);
    return NULL;
  }
  if (alloc_tab(&irdata->base, &irdata->def_base, TAILLE_BASE) == FALSE) {
    IRDATA_delete_irdata(irdata);
    return NULL;
  }
  if (alloc_tab(&irdata->tmps, &irdata->def_tmps, TAILLE_TMP_VARS) == FALSE) {
    IRDATA_delete_irdata(irdata);
    return NULL;
  }
  irdata->ref = (double **)malloc(TAILLE_REFS * (sizeof (double *)));
  if (irdata->ref == NULL) {
    IRDATA_delete_irdata(irdata);
    return NULL;
  }
  irdata->def_ref = (char **)malloc(TAILLE_REFS * (sizeof (char *)));
  if (irdata->def_ref == NULL) {
    IRDATA_delete_irdata(irdata);
    return NULL;
  }
  irdata->info_tmps = (T_varinfo *)malloc(TAILLE_TMP_VARS * (sizeof (T_varinfo)));
  if (irdata->info_tmps == NULL) {
    IRDATA_delete_irdata(irdata);
    return NULL;
  }
  irdata->info_ref = (T_varinfo **)malloc(TAILLE_REFS * (sizeof (T_varinfo *)));
  if (irdata->info_ref == NULL) {
    IRDATA_delete_irdata(irdata);
    return NULL;
  }
  irdata->tmps_org = 0;
  irdata->ref_org = 0;
  irdata->discords = NULL;
  irdata->tas_discord = NULL;
  irdata->p_discord = &irdata->discords;
  irdata->nb_bloqs = 0;
  irdata->max_bloqs = 4;
  irdata->sz_err_finalise = 0;
  irdata->err_finalise = NULL;
  irdata->nb_err_finalise = 0;
  irdata->sz_err_sortie = 0;
  irdata->err_sortie = NULL;
  irdata->nb_err_sortie = 0;
  irdata->sz_err_archive = 0;
  irdata->err_archive = NULL;
  irdata->nb_err_archive = 0;
  irdata->ctx_pr_out.indent = 0;
  irdata->ctx_pr_out.is_newline = 1;
  irdata->ctx_pr_err.indent = 0;
  irdata->ctx_pr_err.is_newline = 1;
  IRDATA_reset_irdata(irdata);
  return irdata;
}

void IRDATA_delete_irdata(T_irdata *irdata)
{
  if (irdata != NULL) {
    if (irdata->saisie != NULL) free(irdata->saisie);
    if (irdata->calculee != NULL) free(irdata->calculee);
    if (irdata->base != NULL) free(irdata->base);
    if (irdata->tmps != NULL) free(irdata->tmps);
    if (irdata->ref != NULL) free(irdata->ref);
    if (irdata->def_saisie != NULL) free(irdata->def_saisie);
    if (irdata->def_calculee != NULL) free(irdata->def_calculee);
    if (irdata->def_base != NULL) free(irdata->def_base);
    if (irdata->def_tmps != NULL) free(irdata->def_tmps);
    if (irdata->def_ref != NULL) free(irdata->def_ref);
    if (irdata->info_tmps != NULL) free(irdata->info_tmps);
    if (irdata->info_ref != NULL) free(irdata->info_ref);
    IRDATA_reset_erreur(irdata);
    while (irdata->tas_discord != NULL) {
      *(irdata->p_discord) = irdata->tas_discord;
      irdata->tas_discord = (irdata->tas_discord)->suivant;
      free(*irdata->p_discord);
    }
    free(irdata);
  }
}

static void reset_tab(double *p_double, char *p_char, int nb)
{
  memset(p_double, 0, sizeof(double) * nb);
  memset(p_char, 0, nb);
}

void IRDATA_reset_irdata(T_irdata *irdata)
{
  reset_tab(irdata->saisie, irdata->def_saisie, TAILLE_SAISIE);
  reset_tab(irdata->calculee, irdata->def_calculee, TAILLE_CALCULEE);
  reset_tab(irdata->base, irdata->def_base, TAILLE_BASE);
  IRDATA_reset_erreur(irdata);
}

void IRDATA_reset_base(T_irdata *irdata)
{
  reset_tab(irdata->base, irdata->def_base, TAILLE_BASE);
}

void IRDATA_reset_light(irdata)
T_irdata *irdata ;
{
reset_tab(irdata->saisie, irdata->def_saisie, TAILLE_SAISIE) ;
reset_tab(irdata->calculee, irdata->def_calculee, TAILLE_CALCULEE) ;
}

void IRDATA_reset_calculee(T_irdata *irdata)
{
  reset_tab(irdata->calculee, irdata->def_calculee, TAILLE_CALCULEE);
}

void IRDATA_recopie_irdata(irdata_src, irdata_dst)
T_irdata *irdata_src ;
T_irdata *irdata_dst ;
{
memcpy(irdata_dst->saisie, irdata_src->saisie, TAILLE_SAISIE * sizeof(double)) ;
memcpy(irdata_dst->def_saisie, irdata_src->def_saisie, TAILLE_SAISIE) ;
memcpy(irdata_dst->calculee, irdata_src->calculee, TAILLE_CALCULEE * sizeof(double)) ;
memcpy(irdata_dst->def_calculee, irdata_src->def_calculee, TAILLE_CALCULEE) ;
memcpy(irdata_dst->base, irdata_src->base, TAILLE_BASE * sizeof(double)) ;
memcpy(irdata_dst->def_base, irdata_src->def_base, TAILLE_BASE) ;
}

void IRDATA_reset_erreur(T_irdata *irdata)
{
  *irdata->p_discord = irdata->tas_discord;
  irdata->tas_discord = irdata->discords;
  irdata->discords = 0;
  irdata->p_discord = &irdata->discords;
  irdata->nb_bloqs = 0;
}

void IRDATA_range_base(T_irdata *irdata, T_var_irdata p_desc, double valeur)
{
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

void IRDATA_efface(T_irdata *irdata, T_var_irdata p_desc)
{
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

double * IRDATA_extrait_special(T_irdata *irdata, T_var_irdata p_desc)
{
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

double * IRDATA_extrait_tableau(T_irdata *irdata, T_var_irdata p_desc, int ind)
{
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
    *tab = (char **)realloc(*tab, *sz * (sizeof (char *)));    
    for (i = old_sz; i < *sz; i++) {
      (*tab)[i] = NULL;
    }
  }
}

void finalise_erreur(irdata)
T_irdata *irdata;
{
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

void exporte_erreur(irdata)
T_irdata *irdata;
{
  int i;
  for (i = 0; i < irdata->sz_err_finalise && irdata->err_finalise[i] != NULL; i++) {
    ajouter_espace(&irdata->sz_err_sortie, &irdata->err_sortie, irdata->nb_err_sortie);
    irdata->err_sortie[irdata->nb_err_sortie] = irdata->err_finalise[i];
    irdata->nb_err_sortie++;
  }
}

