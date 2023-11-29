
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "irdata.h"
#include "var.h"

void env_sauvegarder_un(T_env_sauvegarde *liste, char *oDef, double *oVal) {
  T_env_sauvegarde nouveau = (T_env_sauvegarde)malloc(sizeof (struct S_env_sauvegarde));
  nouveau->sauv_def = *oDef;
  nouveau->sauv_val = *oVal;
  nouveau->orig_def = oDef;
  nouveau->orig_val = oVal;
  nouveau->suite = *liste;
  *liste = nouveau;
}

void env_sauvegarder(T_env_sauvegarde *liste, char *oDef, double *oVal, int sz) {
  int i;
  for (i = 0; i < sz; i++) {
    env_sauvegarder_un(liste, oDef + i, oVal + i);
  }
}

void env_restaurer(T_env_sauvegarde *liste) {
  T_env_sauvegarde courant;

  while (*liste != NULL) {
    courant = *liste;
    *liste = courant-> suite;
    *(courant->orig_def) = courant->sauv_def;
    *(courant->orig_val) = courant->sauv_val;
    free(courant);
  }
}

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
#ifndef FLG_COMPACT
  irdata->saisie = NULL;
  irdata->def_saisie = NULL;
  irdata->calculee = NULL;
  irdata->def_calculee = NULL;
  irdata->base = NULL;
  irdata->def_base = NULL;
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
#ifdef FLG_MULTITHREAD
  irdata->discords = NULL;
  irdata->tas_discord = NULL;
  irdata->p_discord = &irdata->discords;
  irdata->nb_bloquantes = 0;
  irdata->max_bloquantes = 0;
#endif /* FLG_MULTITHREAD */
#endif /* !FLG_COMPACT */
  IRDATA_reset_irdata(irdata);
  return irdata;
}

void IRDATA_delete_irdata(T_irdata *irdata)
{
  if (irdata != NULL) {
#ifndef FLG_COMPACT
    if (irdata->saisie != NULL) free(irdata->saisie);
    if (irdata->calculee != NULL) free(irdata->calculee);
    if (irdata->base != NULL) free(irdata->base);
    if (irdata->def_saisie != NULL) free(irdata->def_saisie);
    if (irdata->def_calculee != NULL) free(irdata->def_calculee);
    if (irdata->def_base != NULL) free(irdata->def_base);
#endif /* FLG_COMPACT */
#ifdef FLG_MULTITHREAD
    IRDATA_reset_erreur(irdata);
    while (irdata->tas_discord != NULL) {
      *(irdata->p_discord) = irdata->tas_discord;
      irdata->tas_discord = (irdata->tas_discord)->suivant;
      free(*irdata->p_discord);
    }
#endif /* FLG_MULTITHREAD */
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
#ifdef FLG_COMPACT
  reset_tab(irdata->valeurs, irdata->defs, TAILLE_TOTALE);
#else
  reset_tab(irdata->saisie, irdata->def_saisie, TAILLE_SAISIE);
  reset_tab(irdata->calculee, irdata->def_calculee, TAILLE_CALCULEE);
  reset_tab(irdata->base, irdata->def_base, TAILLE_BASE);
#endif /* FLG_COMPACT */
#ifdef FLG_MULTITHREAD
  IRDATA_reset_erreur(irdata);
#endif /* FLG_MULTITHREAD */
}

void IRDATA_reset_base(T_irdata *irdata)
{
#ifdef FLG_COMPACT
  reset_tab(irdata->valeurs + TAILLE_SAISIE + TAILLE_CALCULEE, irdata->defs + TAILLE_SAISIE + TAILLE_CALCULEE, TAILLE_BASE);
#else
  reset_tab(irdata->base, irdata->def_base, TAILLE_BASE);
#endif /* FLG_COMPACT */
}

void IRDATA_reset_light(irdata)
T_irdata *irdata ;
{
reset_tab(irdata->saisie, irdata->def_saisie, TAILLE_SAISIE) ;
reset_tab(irdata->calculee, irdata->def_calculee, TAILLE_CALCULEE) ;
}

void IRDATA_reset_calculee(T_irdata *irdata)
{
#ifdef FLG_COMPACT
  reset_tab(irdata->valeurs + TAILLE_SAISIE, irdata->defs + TAILLE_SAISIE, TAILLE_CALCULEE);
#else
  reset_tab(irdata->calculee, irdata->def_calculee, TAILLE_CALCULEE);
#endif /* FLG_COMPACT */
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
#ifdef FLG_MULTITHREAD
  *irdata->p_discord = irdata->tas_discord;
  irdata->tas_discord = irdata->discords;
  irdata->discords = 0;
  irdata->p_discord = &irdata->discords;
  irdata->nb_bloquantes = 0;
#endif /* FLG_MULTITHREAD */
}

void IRDATA_range_base(T_irdata *irdata, T_var_irdata p_desc, double valeur)
{
  T_desc_var *desc = (T_desc_var *)p_desc;
#ifdef FLG_COMPACT
  int indice = desc->indice;
  irdata->defs[indice] = 1;
  irdata->valeurs[indice] = valeur;
#else
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
#endif /* FLG_COMPACT */
}

void IRDATA_efface(T_irdata *irdata, T_var_irdata p_desc)
{
  T_desc_var *desc = (T_desc_var *)p_desc;
  int indice = 0;
#ifdef FLG_COMPACT
  indice = desc->indice;
  irdata->valeurs[indice] = 0;
  irdata->defs[indice] = 0;
#else
  indice = desc->indice & INDICE_VAL;
  if ((desc->indice & EST_MASQUE) != EST_SAISIE) {
    return;
  }
  irdata->saisie[indice] = 0;
  irdata->def_saisie[indice] = 0;
#endif /* FLG_COMPACT */
  return;
}

double * IRDATA_extrait_special(T_irdata *irdata, T_var_irdata p_desc)
{
  T_desc_var *desc = (T_desc_var *)p_desc;
  double *res = NULL;
#ifdef FLG_COMPACT
  int indice = desc->indice;
  res = (irdata->defs[indice] == 0) ? NULL : &irdata->valeurs[indice];
#else
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
#endif /* FLG_COMPACT */
  return res;
}

double * IRDATA_extrait_tableau(T_irdata *irdata, T_var_irdata p_desc, int ind)
{
  T_desc_var *desc = (T_desc_var *)p_desc;
  double *res = NULL;
#ifdef FLG_COMPACT
  int indice = desc->indice + ind;
  res = (irdata->defs[indice] == 0) ? NULL : &irdata->valeurs[indice];
#else
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
#endif /* FLG_COMPACT */
  return res;
}

void pr(int i) {
  fprintf(stderr, "toto %d\n", i);
}

void print_double(FILE *std, double f, int pmin, int pmax) {
  if (pmin < 0) {
    pmin = 0;
  }
  if (pmax < 0) {
    pmax = 0;
  }
  if (pmax < pmin) {
    pmax = pmin;
  }
  if (20 < pmin) {
    pmin = 20;
  }
  if (20 < pmax) {
    pmax = 20;
  }
  if (isnan(f)) {
    fprintf(std, "incorrect");
  } else if (isinf(f)) {
    if (f >= 0.0) {
      fprintf(std, "+infini");
    } else {
      fprintf(std, "-infini");
    }
  } else {
    size_t sz;
    char *buf;
    char *ptr_dot;
    char *ptr;
    int p;

    sz = (size_t)ceil(log10(fabs(f) + 1)) + 21;
    buf = malloc(sz + 1);
    sz = sprintf(buf, "%.*f", pmax, f);
    ptr_dot = &buf[sz - 1];
    while (ptr_dot != buf && *ptr_dot != '.') ptr_dot--;
    if (*ptr_dot == '.') {
      *ptr_dot = ',';
      p = 0;
      while (p < pmin && *ptr_dot != 0) {
        ptr_dot++;
        p++;
      }
      ptr = ptr_dot;
      while (p < pmax && *ptr != 0) {
        ptr++;
        p++;
      }
      if (*ptr == 0) ptr--;
      while (*ptr == '0' && pmin <= p) {
        *ptr = 0;
        ptr--;
        p--;
      }
      if (*ptr == ',') *ptr = 0;
    }
    fprintf(std, "%s", buf);
    free(buf);
  }
}

void nettoie_erreur(irdata)
T_irdata *irdata;
{
#ifdef FLG_MULTITHREAD
  *irdata->p_discord = irdata->tas_discord;
  irdata->tas_discord = irdata->discords;
  irdata->discords = 0;
  irdata->p_discord = &irdata->discords;
  irdata->nb_bloquantes = 0;
#endif /* FLG_MULTITHREAD */
}

