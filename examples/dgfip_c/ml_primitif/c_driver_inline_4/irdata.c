/****** LICENCE CECIL *****/

#include "conf.h"

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <setjmp.h>
#include <math.h>
#include "irdata.h"
#include "var.h"



#ifdef FLG_TRACE_IRDATA

#define AFF(nom) case nom: (void) fprintf(stderr, "nom"); break

static void trace_type(int n)
{
  switch (n) {
    AFF(TYPE_CONTEXTE);
    AFF(TYPE_FAMILLE);
    AFF(TYPE_REVENU);
    AFF(TYPE_REVENU_CORREC);
    AFF(TYPE_VARIATION);
    AFF(TYPE_RESTITUEE);
#ifdef FLG_DEBUG
    AFF(TYPE_DEBUG);
#endif /* FLG_DEBUG */
    default: (void)fprintf(stderr, "%d!!!", n);
  }
}

static void trace_var_irdata(T_var_irdata desc)
{
  if (desc == NULL) {
    fprintf(stderr, "NULL");
  } else {
    int delta;
    fprintf(stderr, "(\"%s\":", ((T_desc_contexte *)desc)->nom);
    delta = ((T_desc_contexte *)desc) - desc_contexte;
    if ((delta >= 0) && (delta < nb_contexte)) trace_type(TYPE_CONTEXTE);
    else {
      delta = ((T_desc_famille *)desc) - desc_famille;
      if ((delta >= 0) && (delta < nb_famille)) trace_type(TYPE_FAMILLE);
      else {
        delta = ((T_desc_revenu *)desc) - desc_revenu;
        if ((delta >= 0) && (delta < nb_revenu)) trace_type(TYPE_REVENU);
        else {
          delta = ((T_desc_revenu_correc *)desc) - desc_revenu_correc;
          if ((delta >= 0) && (delta < nb_revenu_correc)) trace_type(TYPE_REVENU_CORREC);
          else {
            delta = ((T_desc_variation *)desc) - desc_variation;
            if ((delta >= 0) && (delta < nb_variation)) trace_type(TYPE_VARIATION);
            else {
              delta = ((T_desc_restituee *)desc) - desc_restituee;
              if ((delta >= 0) && (delta < nb_restituee)) trace_type(TYPE_RESTITUEE);
#ifdef FLG_DEBUG
#if NB_DEBUG_C <= 0
              else {
                delta = ((T_desc_debug *)desc) - desc_debug;
                if ((delta >= 0) && (delta < nb_debug)) trace_type(TYPE_DEBUG);
                else fprintf(stderr, "???");
              }
#else
              else {
                delta = ((T_desc_debug *)desc) - desc_debug01;
                if ((delta >= 0) && (delta < nb_debug01)) trace_type(TYPE_DEBUG);
              else {
                delta = ((T_desc_debug *)desc) - desc_debug02;
                if ((delta >= 0) && (delta < nb_debug02)) trace_type(TYPE_DEBUG);
              else {
                delta = ((T_desc_debug *)desc) - desc_debug03;
                if ((delta >= 0) && (delta < nb_debug03)) trace_type(TYPE_DEBUG);
              else {
                delta = ((T_desc_debug *)desc) - desc_debug04;
                if ((delta >= 0) && (delta < nb_debug04)) trace_type(TYPE_DEBUG);
                else fprintf(stderr, "???");
              } } } }
#endif /* NB_DEBUG_C <= 0 */
#endif /* FLG_DEBUG */
            }
          }
        }
      }
    }
    putc(')', stderr);
  }
}

#ifdef FLG_DEBUG

static void trace_regle_calcul(T_regle_calcul desc)
{
  if (desc == NULL) {
    fprintf(stderr, "NULL");
  } else {
    int delta;
    delta = ((T_regle_calcul)desc) - desc_call;
    if ((delta >= 0) && (delta < nb_call))
      fprintf(stderr, "(\"regle_%d\")", ((T_regle_calcul)desc)->num);
    else fprintf(stderr, "(inconnue)");
  }
}

#endif /* FLG_DEBUG */

#if defined(FLG_DEBUG) || defined(FLG_CONTROLE_IMMEDIAT)

static void trace_regle_verif(T_regle_verif desc)
{
  if (desc == NULL) {
    fprintf(stderr, "NULL");
  } else {
    int delta;
    delta = ((T_regle_verif)desc) - desc_verif;
    if ((delta >= 0) && (delta < nb_verif))
      fprintf(stderr, "(\"regle_%d\")", ((T_regle_verif)desc)->num);
    else fprintf(stderr, "(inconnue)");
  }
}

#endif /* FLG_DEBUG || FLG_CONTROLE_IMMEDIAT */

static void trace_discord(T_discord *discord)
{
  if (discord == NULL) {
    fprintf(stderr, "NULL");
  } else if ((discord->erreur->type == ANOMALIE)) {
    fprintf(stderr, "anomalie \"%s\"", discord->erreur->nom);
  } else if ((discord->erreur->type == DISCORDANCE)) {
    fprintf(stderr, "discordance \"%s\"", discord->erreur->nom);
  } else if ((discord->erreur->type == INFORMATIVE)) {
    fprintf(stderr, "informative \"%s\"", discord->erreur->nom);
  }
}

#endif /* FLG_TRACE_IRDATA */



int IRDATA_annee_revenu(void)
{
  return ANNEE_REVENU;
}



T_irdata * IRDATA_new_irdata(void)
{
  T_irdata *irdata;
  if ((irdata = (T_irdata *)malloc(sizeof(T_irdata))) == NULL) return NULL;
#ifndef FLG_COMPACT
  irdata->saisie = NULL;
  irdata->def_saisie = NULL;
  irdata->calculee = NULL;
  irdata->def_calculee = NULL;
  irdata->base = NULL;
  irdata->def_base = NULL;
  if (taille_saisie > 0) {
    if ((irdata->saisie = (double *)malloc(taille_saisie * sizeof(double))) == NULL) {
      IRDATA_delete_irdata(irdata);
      return NULL;
    }
    if ((irdata->def_saisie = (char *)malloc(taille_saisie * sizeof(char))) == NULL) {
      IRDATA_delete_irdata(irdata);
      return NULL;
    }
  }
  if (taille_calculee > 0) {
    if ((irdata->calculee = (double *)malloc(taille_calculee * sizeof(double))) == NULL) {
      IRDATA_delete_irdata(irdata);
      return NULL;
    }
    if ((irdata->def_calculee = (char *)malloc(taille_calculee * sizeof(char))) == NULL) {
      IRDATA_delete_irdata(irdata);
      return NULL;
    }
  }
  if (taille_base > 0) {
    if ((irdata->base = (double *)malloc(taille_base * sizeof(double))) == NULL) {
      IRDATA_delete_irdata(irdata);
      return NULL;
    }
    if ((irdata->def_base = (char *)malloc(taille_base * sizeof(char))) == NULL) {
      IRDATA_delete_irdata(irdata);
      return NULL;
    }
  }
#ifdef FLG_MULTITHREAD
  irdata->discords = NULL;
  irdata->tas_discord = NULL;
  irdata->p_discord = &irdata->discords;
  irdata->nb_bloquantes = 0;
  irdata->max_bloquantes = 0;
#endif /* FLG_MULTITHREAD */
#endif /* !FLG_COMPACT */
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
      free (*irdata->p_discord);
    }
#endif /* FLG_MULTITHREAD */
    free(irdata);
  }
}



#ifdef FLG_MULTITHREAD

void IRDATA_reset_erreur(T_irdata *irdata)
{
  *irdata->p_discord = irdata->tas_discord;
  irdata->tas_discord = irdata->discords;
  irdata->discords = 0;
  irdata->p_discord = &irdata->discords;
  irdata->nb_bloquantes = 0;
}

void IRDATA_set_max_bloquantes(struct S_irdata *irdata, const int max_ano)
{
  irdata->max_bloquantes = max_ano;
}

#endif /* FLG_MULTITHREAD */



static void reset_tab(double *p_double, char *p_char, int nb)
{
#ifdef FLG_GCOS
  memset(p_double, 0, sizeof(double)*nb);
  memset(p_char, 0, nb);
#else
  while (nb > 0) {
    *p_double++ = 0;
    *p_char++ = 0;
    nb--;
  }
#endif /* FLG_GCOS */
}

void IRDATA_reset_irdata(T_irdata *irdata)
{
#ifdef FLG_COMPACT
  reset_tab(irdata->valeurs, irdata->defs, taille_totale);
#else
  reset_tab(irdata->saisie, irdata->def_saisie, taille_saisie);
  reset_tab(irdata->calculee, irdata->def_calculee, taille_calculee);
  reset_tab(irdata->base, irdata->def_base, taille_base);
#endif /* FLG_COMPACT */
#ifdef FLG_MULTITHREAD
  IRDATA_reset_erreur(irdata);
#endif /* FLG_MULTITHREAD */
}

void IRDATA_reset_light(T_irdata *irdata)
{
#ifdef FLG_COMPACT
  reset_tab(irdata->valeurs, irdata->defs, taille_saisie + taille_calculee);
#else
  reset_tab(irdata->saisie, irdata->def_saisie, taille_saisie);
  reset_tab(irdata->calculee, irdata->def_calculee, taille_calculee);
#endif /* FLG_COMPACT */
}

void IRDATA_reset_base(T_irdata *irdata)
{
#ifdef FLG_COMPACT
  reset_tab(irdata->valeurs + taille_saisie + taille_calculee, irdata->defs + taille_saisie + taille_calculee, taille_base);
#else
  reset_tab(irdata->base, irdata->def_base, taille_base);
#endif /* FLG_COMPACT */
}

void IRDATA_reset_calculee(T_irdata *irdata)
{
#ifdef FLG_COMPACT
  reset_tab(irdata->valeurs + taille_saisie, irdata->defs + taille_saisie, taille_calculee);
#else
  reset_tab(irdata->calculee, irdata->def_calculee, taille_calculee);
#endif /* FLG_COMPACT */
}



#ifdef FLG_CORRECTIF

void IRDATA_recopie_irdata(const T_irdata *irdata_src, T_irdata *irdata_dst)
{
#ifdef FLG_COMPACT
  memcpy(irdata_dst->valeurs, irdata_src->valeurs, taille_totale * sizeof(double));
  memcpy(irdata_dst->defs, irdata_src->defs, taille_totale);
#else
  memcpy(irdata_dst->saisie, irdata_src->saisie, taille_saisie * sizeof(double));
  memcpy(irdata_dst->def_saisie, irdata_src->def_saisie, taille_saisie);
  memcpy(irdata_dst->calculee, irdata_src->calculee, taille_calculee * sizeof(double));
  memcpy(irdata_dst->def_calculee, irdata_src->def_calculee, taille_calculee);
  memcpy(irdata_dst->base, irdata_src->base, taille_base * sizeof(double));
  memcpy(irdata_dst->def_base, irdata_src->def_base, taille_base);
#endif /* FLG_COMPACT */
}

void IRDATA_recopie_irdata_light(const T_irdata *irdata_src, T_irdata *irdata_dst)
{
#ifdef FLG_COMPACT
  memcpy(irdata_dst->valeurs, irdata_src->valeurs, (taille_saisie + taille_calculee) * sizeof(double));
  memcpy(irdata_dst->defs, irdata_src->defs, (taille_saisie + taille_calculee));
#else
  memcpy(irdata_dst->saisie, irdata_src->saisie, taille_saisie * sizeof(double));
  memcpy(irdata_dst->def_saisie, irdata_src->def_saisie, taille_saisie);
  memcpy(irdata_dst->calculee, irdata_src->calculee, taille_calculee * sizeof(double));
  memcpy(irdata_dst->def_calculee, irdata_src->def_calculee, taille_calculee);
#endif /* FLG_COMPACT */
}

#endif /* FLG_CORRECTIF */



void IRDATA_efface(T_irdata *irdata, T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "efface(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
#endif /* FLG_TRACE_IRDATA */
#ifdef FLG_COMPACT
  T_indice indice = ((T_desc_contexte *)desc)->indice;
  irdata->valeurs[indice] = 0;
  irdata->defs[indice] = 0;
#else
  T_indice indice = ((T_desc_contexte *)desc)->indice & INDICE_VAL;
  if ((((T_desc_contexte *)desc)->indice & EST_MASQUE) != EST_SAISIE)
    return;
  irdata->saisie[indice] = 0;
  irdata->def_saisie[indice] = 0;
#endif /* FLG_COMPACT */
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "efface = void\n");
#endif /* FLG_TRACE_IRDATA */
}

void IRDATA_efface_tableau(T_irdata *irdata, T_var_irdata desc, int ind)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "efface_tableau(");
  trace_var_irdata(desc);
  fprintf(stderr, "[%d])\n", ind);
#endif /* FLG_TRACE_IRDATA */
#ifdef FLG_COMPACT
  T_indice indice = ((T_desc_contexte *)desc)->indice + ind;
  irdata->valeurs[indice] = 0;
  irdata->defs[indice] = 0;
#else
  T_indice indice = (((T_desc_contexte *)desc)->indice & INDICE_VAL) + ind;
  if ((((T_desc_contexte *)desc)->indice & EST_MASQUE) != EST_SAISIE)
    return;
  irdata->saisie[indice] = 0;
  irdata->def_saisie[indice] = 0;
#endif /* FLG_COMPACT */
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "efface_tableau = void\n");
#endif /* FLG_TRACE_IRDATA */
}



struct S_discord * IRDATA_range(T_irdata *irdata, T_var_irdata desc, double valeur)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "range(");
  trace_var_irdata(desc);
  fprintf(stderr, ", %.3f)\n", valeur);
#endif /* FLG_TRACE_IRDATA */
  T_discord *discord = NULL;
#ifdef FLG_MULTITHREAD
  double old_valeur;
  int old_val_def;
#else
  static double old_valeur;
  static int old_val_def;
#endif /* FLG_MULTITHREAD */
#ifdef FLG_COMPACT
  T_indice indice = ((T_desc_contexte *)desc)->indice;
  old_valeur = irdata->valeurs[indice];
  old_val_def = irdata->defs[indice];
  irdata->valeurs[indice] = valeur;
  irdata->defs[indice] = 1;
#else
  T_indice indice = ((T_desc_contexte *)desc)->indice & INDICE_VAL;
#ifdef FLG_DEBUG
  switch (((T_desc_debug *)desc)->indice & EST_MASQUE) {
    case EST_SAISIE:
      old_valeur = irdata->saisie[indice];
      old_val_def = irdata->def_saisie[indice];
      irdata->saisie[indice] = valeur;
      irdata->def_saisie[indice] = 1;
      break;
    case EST_CALCULEE:
      old_valeur = irdata->calculee[indice];
      old_val_def = irdata->def_calculee[indice];
      irdata->calculee[indice] = valeur;
      irdata->def_calculee[indice] = 1;
      break;
    case EST_BASE:
      old_valeur = irdata->base[indice];
      old_val_def = irdata->def_base[indice];
      irdata->base[indice] = valeur;
      irdata->def_base[indice] = 1;
      break;
  }
#else
  if ((((T_desc_contexte *)desc)->indice & EST_MASQUE) != EST_SAISIE)
    return (0);
  old_valeur = irdata->saisie[indice];
  old_val_def = irdata->def_saisie[indice];
  irdata->saisie[indice] = valeur;
  irdata->def_saisie[indice] = 1;
#endif /* FLG_DEBUG */
#endif /* FLG_COMPACT */
#if ANNEE_REVENU > 2005
  if (valeur < 0) {
    discord = err_NEGATIF(irdata);
  } else if (discord != NULL) {
    discord = (*((T_desc_contexte *)desc)->verif)(irdata);
  }
#else
  discord = (*((T_desc_contexte *)desc)->verif)(irdata);
#endif /* ANNEE_REVENU > 2005 */
#ifndef FLG_GCOS
  if ((discord != NULL) && (discord->erreur->type == ANOMALIE)) {
#ifdef FLG_COMPACT
    irdata->valeurs[indice] = old_valeur;
    irdata->defs[indice] = old_val_def;
#else
#ifdef FLG_DEBUG
    switch (((T_desc_debug *)desc)->indice & EST_MASQUE) {
      case EST_SAISIE:
        irdata->saisie[indice] = old_valeur;
        irdata->def_saisie[indice] = old_val_def;
        break;
      case EST_CALCULEE:
        irdata->calculee[indice] = old_valeur;
        irdata->def_calculee[indice] = old_val_def;
        break;
      case EST_BASE:
        irdata->base[indice] = old_valeur;
        irdata->def_base[indice] = old_val_def;
        break;
    }
#else
    irdata->saisie[indice] = old_valeur;
    irdata->def_saisie[indice] = old_val_def;
#endif /* FLG_DEBUG */
#endif /* FLG_COMPACT */
  }
#endif /* !FLG_GCOS */
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "range = ");
  trace_discord(discord);
  putc('\n', stderr);
#endif /* FLG_TRACE_IRDATA */
  return discord;
}

void IRDATA_range_base(T_irdata *irdata, T_var_irdata desc, double valeur)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "range(");
  trace_var_irdata(desc);
  fprintf(stderr, ", %.3f)\n", valeur);
#endif /* FLG_TRACE_IRDATA */
#ifdef FLG_COMPACT
  T_indice indice = ((T_desc_contexte *)desc)->indice;
  irdata->valeurs[indice] = valeur;
  irdata->defs[indice] = 1;
#else
  T_indice indice = ((T_desc_contexte *)desc)->indice & INDICE_VAL;
  switch (((T_desc_contexte *)desc)->indice & EST_MASQUE) {
    case EST_SAISIE:
      irdata->saisie[indice] = valeur;
      irdata->def_saisie[indice] = 1;
      break;
    case EST_CALCULEE:
      irdata->calculee[indice] = valeur;
      irdata->def_calculee[indice] = 1;
      break;
    case EST_BASE:
      irdata->base[indice] = valeur;
      irdata->def_base[indice] = 1;
      break;
  }
#endif /* FLG_COMPACT */
}

struct S_discord * IRDATA_range_tableau(T_irdata *irdata, T_var_irdata desc, int ind, double valeur)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "range_tableau(");
  trace_var_irdata(desc) ;
  fprintf(stderr, "[%d], %.3f)\n", ind, valeur);
#endif /* FLG_TRACE_IRDATA */
  T_discord *discord;
#ifdef FLG_MULTITHREAD
  double old_valeur;
  int old_val_def;
#else
  static double old_valeur;
  static int old_val_def;
#endif /* FLG_MULTITHREAD */
#ifdef FLG_COMPACT
  T_indice indice = ((T_desc_contexte *)desc)->indice + ind;
  old_valeur = irdata->valeurs[indice];
  old_val_def = irdata->defs[indice];
  irdata->valeurs[indice] = valeur;
  irdata->defs[indice] = 1;
#else
  T_indice indice = (((T_desc_contexte *)desc)->indice & INDICE_VAL) + ind;
#ifdef FLG_DEBUG
  switch (((T_desc_debug *)desc)->indice & EST_MASQUE) {
    case EST_SAISIE:
      old_valeur = irdata->saisie[indice];
      old_val_def = irdata->def_saisie[indice];
      irdata->saisie[indice] = valeur;
      irdata->def_saisie[indice] = 1;
      break;
    case EST_CALCULEE:
      old_valeur = irdata->calculee[indice];
      old_val_def = irdata->def_calculee[indice];
      irdata->calculee[indice] = valeur;
      irdata->def_calculee[indice] = 1;
      break;
    case EST_BASE:
      old_valeur = irdata->base[indice];
      old_val_def = irdata->def_base[indice];
      irdata->base[indice] = valeur;
      irdata->def_base[indice] = 1;
      break;
  }
#else
  if ((((T_desc_contexte *)desc)->indice & EST_MASQUE) != EST_SAISIE)
    return (0);
  old_valeur = irdata->saisie[indice];
  old_val_def = irdata->def_saisie[indice];
  irdata->saisie[indice] = valeur;
  irdata->def_saisie[indice] = 1;
#endif /* FLG_DEBUG */
#endif /* FLG_COMPACT */
  discord = (*((T_desc_contexte *)desc)->verif)(irdata);
  if ((discord != NULL) && (discord->erreur->type == ANOMALIE)) {
#ifdef FLG_COMPACT
    irdata->valeurs[indice] = old_valeur;
    irdata->defs[indice] = old_val_def;
#else
#ifdef FLG_DEBUG
    switch (((T_desc_debug *)desc)->indice & EST_MASQUE) {
      case EST_SAISIE:
        irdata->saisie[indice] = old_valeur;
        irdata->def_saisie[indice] = old_val_def;
        break;
      case EST_CALCULEE:
        irdata->calculee[indice] = old_valeur;
        irdata->def_calculee[indice] = old_val_def;
        break;
      case EST_BASE:
        irdata->base[indice] = old_valeur;
        irdata->def_base[indice] = old_val_def;
        break;
    }
#else
    irdata->saisie[indice] = old_valeur;
    irdata->def_saisie[indice] = old_val_def;
#endif /* FLG_DEBUG */
#endif /* FLG_COMPACT */
  }
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "range_tableau = ");
  trace_discord(discord);
  putc('\n', stderr);
#endif /* FLG_TRACE_IRDATA */
  return discord;
}



void IRDATA_add_valeur(T_irdata *irdata, T_var_irdata desc, double valeur)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "add_valeur(");
  trace_var_irdata(desc);
  fprintf(stderr, ", %.3f)\n", valeur);
#endif /* FLG_TRACE_IRDATA */
#ifdef FLG_COMPACT
  fprintf(file, "T_indice indice = ((T_desc_contexte *)desc)->indice ;\n") ;
  fprintf(file, "irdata->valeurs[indice] += valeur ;\n") ;
  fprintf(file, "irdata->defs[indice] = 1 ;\n") ;
#else
  T_indice indice = ((T_desc_contexte *)desc)->indice & INDICE_VAL;
#ifdef FLG_DEBUG
  switch (((T_desc_debug *)desc)->indice & EST_MASQUE) {
    case EST_SAISIE:
      irdata->saisie[indice] += valeur;
      irdata->def_saisie[indice] = 1;
      break;
    case EST_CALCULEE:
      irdata->calculee[indice] += valeur;
      irdata->def_calculee[indice] = 1;
      break;
    case EST_BASE:
      irdata->base[indice] += valeur;
      irdata->def_base[indice] = 1;
      break;
  }
#else
  if ((((T_desc_contexte *)desc)->indice & EST_MASQUE) != EST_SAISIE)
    return;
  irdata->saisie[indice] += valeur;
  irdata->def_saisie[indice] = 1;
#endif /* FLG_DEBUG */
#endif /* FLG_COMPACT */
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "add_valeur = void\n");
#endif /* FLG_TRACE_IRDATA */
}



struct S_discord * IRDATA_controle_valeur(T_irdata *irdata, T_var_irdata desc, double valeur)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "controle_valeur(");
  trace_var_irdata(desc);
  fprintf(stderr, ", %.3f)\n", valeur);
#endif /* FLG_TRACE_IRDATA */
  T_discord *discord;
#ifdef FLG_MULTITHREAD
  double old_valeur;
  int old_val_def;
#else /* FLG_MULTITHREAD */
  static double old_valeur;
  static int old_val_def;
#endif /* FLG_MULTITHREAD */
#ifdef FLG_COMPACT
  T_indice indice = ((T_desc_contexte *)desc)->indice;
  old_valeur = irdata->valeurs[indice];
  old_val_def = irdata->defs[indice];
  irdata->valeurs[indice] = valeur;
  irdata->defs[indice] = 1;
#else
  T_indice indice = ((T_desc_contexte *)desc)->indice & INDICE_VAL;
  if ((((T_desc_contexte *)desc)->indice & EST_MASQUE) != EST_SAISIE)
    return (0);
  old_valeur = irdata->saisie[indice];
  old_val_def = irdata->def_saisie[indice];
  irdata->saisie[indice] = valeur;
  irdata->def_saisie[indice] = 1;
#endif /* FLG_COMPACT */
  discord = (*((T_desc_contexte *)desc)->verif)(irdata);
#ifdef FLG_COMPACT
  irdata->valeurs[indice] = old_valeur;
  irdata->defs[indice] = old_val_def;
#else
  irdata->saisie[indice] = old_valeur;
  irdata->def_saisie[indice] = old_val_def;
#endif /* FLG_COMPACT */
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "controle_valeur = ");
  trace_discord(discord);
  putc('\n', stderr);
#endif /* FLG_TRACE_IRDATA */
  return discord;
}



double * IRDATA_extrait(T_irdata *irdata, T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "extrait(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
#endif /* FLG_TRACE_IRDATA */
  double *retour;
#ifdef FLG_COMPACT
  T_indice indice = ((T_desc_restituee *)desc)->indice;
  if (!irdata->defs[indice]) retour = NULL;
  else retour = &irdata->valeurs[indice];
#else
  T_indice indice = ((T_desc_restituee *)desc)->indice & INDICE_VAL;
  switch (((T_desc_restituee *)desc)->indice & EST_MASQUE) {
    case EST_SAISIE:
      if (!irdata->def_saisie[indice]) retour = NULL;
      else retour = &irdata->saisie[indice];
      break;
    case EST_CALCULEE:
      if (!irdata->def_calculee[indice]) retour = NULL;
      else retour = &irdata->calculee[indice];
      break;
    case EST_BASE:
      if (!irdata->def_base[indice]) retour = NULL;
      else retour = &irdata->base[indice];
      break;
    default:
      retour = NULL;
      break;
  }
#endif /* FLG_COMPACT */
#ifdef FLG_EXTRACTION
  if ((desc >= (T_var_irdata)desc_restituee) && (desc < (T_var_irdata)desc_restituee + sizeof(T_desc_restituee) * nb_restituee)) {
    ((T_desc_restituee *)desc)->est_extraite = 1;
  }
#endif /* FLG_EXTRACTION */
#ifdef FLG_TRACE_IRDATA
  if (retour == NULL) {
    fprintf(stderr, "extrait = undef\n");
  } else {
    fprintf(stderr, "extrait = %.3f\n", *retour);
  }
#endif /* FLG_TRACE_IRDATA */
  return retour;
}

double * IRDATA_extrait_special(T_irdata *irdata, T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "extrait_special(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
#endif /* FLG_TRACE_IRDATA */
  double *retour;
#ifdef FLG_COMPACT
  T_indice indice = ((T_desc_restituee *)desc)->indice;
  if (!irdata->defs[indice]) retour = NULL;
  else retour = &irdata->valeurs[indice];
#else
  T_indice indice = ((T_desc_restituee *)desc)->indice & INDICE_VAL;
  switch (((T_desc_restituee *)desc)->indice & EST_MASQUE) {
    case EST_SAISIE:
      if (!irdata->def_saisie[indice]) retour = NULL;
      else retour = &irdata->saisie[indice];
      break;
    case EST_CALCULEE:
      if (!irdata->def_calculee[indice]) retour = NULL;
      else retour = &irdata->calculee[indice];
      break;
    case EST_BASE:
      if (!irdata->def_base[indice]) retour = NULL;
      else retour = &irdata->base[indice];
      break;
    default:
      retour = NULL;
      break;
  }
#endif /* FLG_COMPACT */
#ifdef FLG_TRACE_IRDATA
  if (retour == NULL) {
    fprintf(stderr, "extrait_special = undef\n");
  } else {
    fprintf(stderr, "extrait_special = %.3f\n", *retour);
  }
#endif /* FLG_TRACE_IRDATA */
  return retour;
}

double * IRDATA_extrait_tableau(T_irdata *irdata, T_var_irdata desc, int ind)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "extrait_tableau(");
  trace_var_irdata(desc);
  fprintf(stderr, "[%d])\n", ind);
#endif /* FLG_TRACE_IRDATA */
  double *retour;
#ifdef FLG_COMPACT
  T_indice indice = ((T_desc_restituee *)desc)->indice + ind;
  if (!irdata->defs[indice]) retour = NULL;
  else retour = &irdata->valeurs[indice];
#else
  T_indice indice = (((T_desc_restituee *)desc)->indice & INDICE_VAL) + ind;
  switch (((T_desc_restituee *)desc)->indice & EST_MASQUE) {
    case EST_SAISIE:
      if (!irdata->def_saisie[indice]) retour = NULL;
      else retour = &irdata->saisie[indice];
      break;
    case EST_CALCULEE:
      if (!irdata->def_calculee[indice]) retour = NULL;
      else retour = &irdata->calculee[indice];
      break;
    case EST_BASE:
      if (!irdata->def_base[indice]) retour = NULL;
      else retour = &irdata->base[indice];
      break;
    default:
      retour = NULL;
      break;
  }
#endif /* FLG_COMPACT */
#ifdef FLG_EXTRACTION
  if ((desc >= (T_var_irdata)desc_restituee) && (desc < (T_var_irdata)desc_restituee + sizeof(T_desc_restituee) * nb_restituee)) {
    ((T_desc_restituee *)desc)->est_extraite = 1;
  }
#endif /* FLG_EXTRACTION */
#ifdef FLG_TRACE_IRDATA
  if (retour == NULL) {
    fprintf(stderr, "extrait_tableau = undef\n");
  } else {
    fprintf(stderr, "extrait_tableau = %.3f\n", *retour);
  }
#endif /* FLG_TRACE_IRDATA */
  return retour;
}



#ifdef FLG_EXTRACTION

struct S_extraction
{
  int nb;
#ifdef FLG_DEBUG
  T_desc_debug *desc;
#if NB_DEBUG_C > 0
  int num_bank;
#endif /* NB_DEBUG_BANK_C > 0 */
#else
  T_desc_restituee *desc;
#endif /* FLG_DEBUG */
};

struct S_extraction * IRDATA_init_extraction(void)
{
  struct S_extraction *retour = (struct S_extraction *)malloc(sizeof(struct S_extraction));
#ifdef FLG_DEBUG
#if NB_DEBUG_C <= 0
  retour->nb = nb_debug;
  retour->desc = desc_debug;
#else
  retour->nb = nb_debug01;
  retour->desc = desc_debug01;
  retour->num_bank = 1;
#endif /* NB_DEBUG_C <= 0 */
#else
  retour->nb = nb_restituee;
  retour->desc = desc_restituee;
#endif /* FLG_DEBUG */
  return retour;
}

void IRDATA_detruit_extraction(struct S_extraction *extraction)
{
  free(extraction);
}

T_var_irdata IRDATA_extraction_suivante(struct S_extraction *extraction, int type)
{
  for (;;) {
#if defined(FLG_DEBUG) && NB_DEBUG_C > 0
    while (extraction->nb == 0) {
      switch (++extraction->num_bank) {
        case 1:
          extraction->nb = nb_debug01;
          extraction->desc = desc_debug01;
          break;
        case 2:
          extraction->nb = nb_debug02;
          extraction->desc = desc_debug02;
          break;
        case 3:
          extraction->nb = nb_debug03;
          extraction->desc = desc_debug03;
          break;
        case 4:
          extraction->nb = nb_debug04;
          extraction->desc = desc_debug04;
          break;
        default: return NULL;
      }
    }
#else
    if (extraction->nb == 0) return NULL;
#endif /* FLG_DEBUG && NB_DEBUG_C > 0 */
    extraction->nb--;
    if (!extraction->desc->est_extraite
#ifndef FLG_DEBUG
        && ((extraction->desc->type == type) || (extraction->desc->type == RESTITUEE))
#endif /* FLG_DEBUG */
    ) {
      T_var_irdata retour = (T_var_irdata)extraction->desc;
      extraction->desc++;
      return retour;
    }
    extraction->desc++;
  }
}

void IRDATA_reset_extraction(void)
{
#ifdef FLG_DEBUG
#if NB_DEBUG_C <= 0
  T_desc_debug *desc = desc_debug;
  int cnt = nb_debug;
#else
  T_desc_debug *desc = desc_debug01;
  int cnt = nb_debug01;
  int num_bank = 1;
#endif /* NB_DEBUG_C <= 0 */
#else
  T_desc_restituee *desc = desc_restituee;
  int cnt = nb_restituee;
#endif /* FLG_DEBUG */
  for (;;) {
#if defined(FLG_DEBUG) && NB_DEBUG_C > 0
    while (cnt == 0) {
      switch (++num_bank) {
        case 2:
          cnt = nb_debug02;
          desc = desc_debug02;
          break;
        case 3:
          cnt = nb_debug03;
          desc = desc_debug03;
          break;
        case 4:
          cnt = nb_debug04;
          desc = desc_debug04;
          break;
        default: return;
      }
    }
#else
    if (cnt == 0) return;
#endif /* FLG_DEBUG && NB_DEBUG_C > 0 */
    desc->est_extraite = 0;
    desc++;
    cnt--;
  }
}

#endif /* FLG_EXTRACTION */

struct S_table_extraction
{
  int nb;
  int type;
  union {
    T_desc_contexte *contexte;
    T_desc_famille *famille;
    T_desc_revenu *revenu;
    T_desc_revenu_correc *revenu_correc;
    T_desc_variation *variation;
    T_desc_restituee *restituee;
  } desc;
};

struct S_table_extraction * IRDATA_init_table_extraction(int type_table)
{
  struct S_table_extraction *retour = (struct S_table_extraction *)malloc(sizeof(struct S_table_extraction));
  retour->type = type_table;
  switch (type_table) {
    case TYPE_CONTEXTE:
      retour->nb = nb_contexte;
      retour->desc.contexte = desc_contexte;
      break;
    case TYPE_FAMILLE:
      retour->nb = nb_famille;
      retour->desc.famille = desc_famille;
      break;
    case TYPE_REVENU:
      retour->nb = nb_revenu;
      retour->desc.revenu = desc_revenu;
      break;
    case TYPE_REVENU_CORREC:
      retour->nb = nb_revenu_correc;
      retour->desc.revenu_correc = desc_revenu_correc;
      break;
    case TYPE_VARIATION:
      retour->nb = nb_variation;
      retour->desc.variation = desc_variation;
      break;
    case TYPE_RESTITUEE:
      retour->nb = nb_restituee;
      retour->desc.restituee = desc_restituee;
      break;
    default:
      free(retour);
      retour = (struct S_table_extraction *)NULL;
  }
  return retour;
}

void IRDATA_detruit_table_extraction(struct S_table_extraction *extraction)
{
  free(extraction);
}

T_var_irdata IRDATA_table_extraction_suiv(struct S_table_extraction *extraction)
{
  T_var_irdata retour;
  if (extraction->nb == 0) return NULL;
  extraction->nb--;
  switch (extraction->type) {
    case TYPE_CONTEXTE:
      retour = (T_var_irdata)extraction->desc.contexte;
      extraction->desc.contexte++;
      break;
    case TYPE_FAMILLE:
      retour = (T_var_irdata)extraction->desc.famille;
      extraction->desc.famille++;
      break;
    case TYPE_REVENU:
      retour = (T_var_irdata)extraction->desc.revenu;
      extraction->desc.revenu++;
      break;
    case TYPE_REVENU_CORREC:
      retour = (T_var_irdata)extraction->desc.revenu_correc;
      extraction->desc.revenu_correc++;
      break;
    case TYPE_VARIATION:
      retour = (T_var_irdata)extraction->desc.variation;
      extraction->desc.variation++;
      break;
    case TYPE_RESTITUEE:
      retour = (T_var_irdata)extraction->desc.restituee;
      extraction->desc.restituee++;
      break;
  }
  return retour;
}



char * IRDATA_cherche_revenu(const char *nom)
{
  char *retour = NULL;
  int fin = 0, i = 0;
  while ((fin == 0) && (i < NB_REVENU)) {
    if (strcmp(nom, desc_revenu[i].code) == 0) {
      retour = desc_revenu[i].nom;
      fin = 1;
    }
    i++;
  }
  return retour;
}

static char * cherche_nom(const char *nom, char *table, int taille, int sup)
{
  int inf = 0;
  for (;;) {
    int res;
    int millieu;
    char *pt_cmp;
    if (inf == sup) return NULL;
    millieu = (inf + sup) / 2;
    pt_cmp = table + (taille * millieu);
    res = strcmp(nom, ((T_desc_contexte *)pt_cmp)->nom);
    if (res == 0) return pt_cmp;
    if (res < 0) sup = millieu;
    else inf = millieu + 1;
  }
}

T_var_irdata IRDATA_get_var_irdata(const char *nom, T_typezone2042 type)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_desc(%s,", nom);
  trace_type(type);
  fprintf(stderr, ")\n");
#endif /* FLG_TRACE_IRDATA */
  char *retour;
  switch (type) {
    case TYPE_CONTEXTE:
      retour = cherche_nom(nom, (char *)desc_contexte, sizeof(T_desc_contexte), nb_contexte);
      break;
    case TYPE_FAMILLE:
      retour = cherche_nom(nom, (char *)desc_famille, sizeof(T_desc_famille), nb_famille);
      break;
    case TYPE_REVENU:
      retour = cherche_nom(nom, (char *)desc_revenu, sizeof(T_desc_revenu), nb_revenu);
      break;
    case TYPE_REVENU_CORREC:
      retour = cherche_nom(nom, (char *)desc_revenu, sizeof(T_desc_revenu), nb_revenu);
      if (retour == NULL)
        retour = cherche_nom(nom, (char *)desc_revenu_correc, sizeof(T_desc_revenu_correc), nb_revenu_correc);
      break;
    case TYPE_VARIATION:
      retour = cherche_nom(nom, (char *)desc_variation, sizeof(T_desc_variation), nb_variation);
      break;
    case TYPE_RESTITUEE:
      retour = cherche_nom(nom, (char *)desc_restituee, sizeof(T_desc_restituee), nb_restituee);
#if defined(FLG_DEBUG) && defined(FLG_EXTRACTION)
      if (retour == NULL) break;
      /* FALL INFO */
#else
      break;
#endif /* FLG_DEBUG && FLG_EXTRACTION */
#ifdef FLG_DEBUG
    case TYPE_DEBUG:
#if NB_DEBUG_C <= 0
      retour = cherche_nom(nom, (char *)desc_debug, sizeof(T_desc_debug), nb_debug);
#else
      retour = cherche_nom(nom, (char *)desc_debug01, sizeof(T_desc_debug), nb_debug01);
      if (retour == NULL) retour = cherche_nom(nom, (char *)desc_debug02, sizeof(T_desc_debug), nb_debug02);
      if (retour == NULL) retour = cherche_nom(nom, (char *)desc_debug03, sizeof(T_desc_debug), nb_debug03);
      if (retour == NULL) retour = cherche_nom(nom, (char *)desc_debug04, sizeof(T_desc_debug), nb_debug04);
#endif /* NB_DEBUG_C <= 0 */
      break;
#endif /* FLG_DEBUG */
    default:
      retour = NULL;
      break;
  }
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_desc = ");
  trace_var_irdata(retour);
  putc('\n', stderr);
#endif /* FLG_TRACE_IRDATA */
  return retour;
}

int IRDATA_est_du_type(T_var_irdata desc, T_typezone2042 type)
{
  switch (type) {
    case TYPE_CONTEXTE:
      return ((desc >= (T_var_irdata)desc_contexte) && (desc < (T_var_irdata)desc_contexte + sizeof(T_desc_contexte)*nb_contexte));
    case TYPE_FAMILLE:
      return ((desc >= (T_var_irdata)desc_famille) && (desc < (T_var_irdata)desc_famille + sizeof(T_desc_famille)*nb_famille));
    case TYPE_REVENU:
      return ((desc >= (T_var_irdata)desc_revenu) && (desc < (T_var_irdata)desc_revenu + sizeof(T_desc_revenu)*nb_revenu));
    case TYPE_REVENU_CORREC:
      return ((desc >= (T_var_irdata)desc_revenu_correc) && (desc < (T_var_irdata)desc_revenu_correc + sizeof(T_desc_revenu_correc)*nb_revenu_correc));
    case TYPE_VARIATION:
      return ((desc >= (T_var_irdata)desc_variation) && (desc < (T_var_irdata)desc_variation + sizeof(T_desc_variation)*nb_variation));
    case TYPE_RESTITUEE:
      return ((desc >= (T_var_irdata)desc_restituee) && (desc < (T_var_irdata)desc_restituee + sizeof(T_desc_restituee)*nb_restituee));
    case TYPE_BASE:
#ifdef FLG_COMPACT
      return ((((T_desc_contexte *)desc)->indice >= taille_saisie+taille_calculee) && (((T_desc_contexte *)desc)->indice < taille_saisie+taille_calculee+taille_base));
#else
      return ((((T_desc_contexte *)desc)->indice & EST_MASQUE) == EST_BASE);
#endif /* FLG_COMPACT */
    default:
      return FALSE;
  }
}

char * IRDATA_get_nom(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_nom(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_nom = %s\n", ((T_desc_revenu *)desc)->nom);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_revenu *)desc)->nom;
}

char * IRDATA_get_lib(T_var_irdata desc)
{
  char *libelle = NULL;

#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_lib(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_lib = \"%s\"\n", ((T_desc_revenu *)desc)->libelle);
#endif /* FLG_TRACE_IRDATA */

  if ((IRDATA_est_du_type(desc, TYPE_REVENU)) || (IRDATA_est_du_type(desc, TYPE_REVENU_CORREC)) || (IRDATA_est_du_type(desc, TYPE_FAMILLE))) {
    libelle = ((T_desc_revenu *)desc)->libelle;
  }

  if ((IRDATA_est_du_type(desc, TYPE_CONTEXTE))) {
    libelle = ((T_desc_contexte *)desc)->libelle;
  }

#ifdef FLG_GENERE_LIBELLE_RESTITUEE
  if (IRDATA_est_du_type(desc, TYPE_RESTITUEE)) {
    libelle = ((T_desc_restituee *)desc)->libelle;
  }
#endif /* FLG_GENERE_LIBELLE_RESTITUEE */

  return (libelle);
}

T_classe IRDATA_get_classe(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_classe(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_classe = %d\n", ((T_desc_revenu *)desc)->classe);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_revenu *)desc)->classe;
}

T_priorite IRDATA_get_priorite(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_priorite(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_priorite = %d\n", ((T_desc_revenu *)desc)->priorite);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_revenu *)desc)->priorite;
}

T_categorie_TL IRDATA_get_categorie_TL(T_var_irdata p_Descripteur)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_categorie_TL(");
  trace_var_irdata(p_Descripteur);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_categorie_TL = %d\n", ((T_desc_revenu *)p_Descripteur)->categorie_TL);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_revenu *)p_Descripteur)->categorie_TL;
}

T_cotsoc IRDATA_get_cotsoc(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_cotsoc(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_cotsoc = %d\n", ((T_desc_revenu *)desc)->cotsoc);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_revenu *)desc)->cotsoc;
}

T_ind_abat IRDATA_get_ind_abat(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_ind_abat(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_ind_abat = %d\n", ((T_desc_revenu *)desc)->ind_abat);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_revenu *)desc)->ind_abat;
}

T_acompte IRDATA_get_acompte(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_acompte(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_acompte = %d\n", ((T_desc_revenu *)desc)->acompte);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_revenu *)desc)->acompte;
}

T_avfisc IRDATA_get_avfisc(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_avfisc(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_avfisc = %d\n", ((T_desc_revenu *)desc)->afisc);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_revenu *)desc)->avfisc;
}

T_rapcat IRDATA_get_rapcat(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_rapcat(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_rapcat = %d\n", ((T_desc_revenu *)desc)->rapcat);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_revenu *)desc)->rapcat;
}

T_sanction IRDATA_get_sanction(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_sanction(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_sanction = %d\n", ((T_desc_revenu *)desc)->sanction);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_revenu *)desc)->sanction;
}

T_modcat IRDATA_get_modcat(T_var_irdata desc)
{
  if ((IRDATA_est_du_type(desc, TYPE_REVENU)) || (IRDATA_est_du_type(desc, TYPE_REVENU_CORREC)))
    return ((T_desc_revenu *)desc)->modcat;

  if ((IRDATA_est_du_type(desc, TYPE_CONTEXTE)))
    return ((T_desc_contexte *)desc)->modcat;

  if ((IRDATA_est_du_type(desc, TYPE_FAMILLE)))
    return ((T_desc_famille *)desc)->modcat;

  return -1;
}

T_nat_code IRDATA_get_nat_code(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_nat_code(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_nat_code = %d\n", ((T_desc_revenu *)desc)->nat_code);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_revenu *)desc)->nat_code;
}


T_var_irdata IRDATA_get_liee(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_liee(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_liee = %s\n", ((T_desc_revenu *)desc)->liee);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_revenu *)desc)->liee;
}

T_type IRDATA_get_type(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_type(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_type = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((T_desc_contexte *)desc)->type_donnee;
}

T_type IRDATA_est_booleen(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_est_booleen(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "get_est_booleen = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & BOOLEEN));
}

T_type IRDATA_est_numerique(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "IRDATA_est_numerique(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "IRDATA_est_numerique = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & NUMERIQUE));
}

T_type IRDATA_est_date(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "IRDATA_est_date(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "IRDATA_est_date = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & DATE));
}

T_type IRDATA_est_entier(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "IRDATA_est_entier(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "IRDATA_est_entier = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & ENTIER));
}

T_type IRDATA_est_reel(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "IRDATA_est_reel(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "IRDATA_est_reel = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & REEL));
}

T_type IRDATA_est_reel1(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "IRDATA_est_reel1(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "IRDATA_est_reel1 = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & REEL1));
}

T_type IRDATA_est_reel2(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "IRDATA_est_reel2(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "IRDATA_est_reel2 = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & REEL2));
}

T_type IRDATA_est_reel3(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "IRDATA_est_reel3(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "IRDATA_est_reel3 = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & REEL3));
}

T_type IRDATA_est_date_JJMMAAAA(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "IRDATA_est_date_JJMMAAAA(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "IRDATA_est_date_JJMMAAAA = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & DATE_JJMMAAAA));
}

T_type IRDATA_est_date_AAAA(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "IRDATA_est_date_AAAA(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "IRDATA_est_date_AAAA = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & DATE_AAAA));
}

T_type IRDATA_est_date_MMAAAA(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "IRDATA_est_date_MMAAAA(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "IRDATA_est_date_MMAAAA = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & DATE_MMAAAA));
}

T_type IRDATA_est_date_JJMM(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "IRDATA_est_date_JJMM(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "IRDATA_est_date_JJMM = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & DATE_JJMM));
}

T_type IRDATA_est_date_MM(T_var_irdata desc)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "IRDATA_est_date_MM(");
  trace_var_irdata(desc);
  fprintf(stderr, ")\n");
  fprintf(stderr, "IRDATA_est_date_MM = %d\n", ((T_desc_contexte *)desc)->type_donnee);
#endif /* FLG_TRACE_IRDATA */
  return ((IRDATA_get_type(desc) & DATE_MM));
}



T_ench_calcul IRDATA_get_ench_calcul(const char *ref)
{
#if FLG_TRACE_IRDATA
  fprintf(stderr, "get_ench_calcul(%s)\n", ref);
#endif /* FLG_TRACE_IRDATA */
  int cnt;
  T_desc_ench *desc = desc_ench;
  for (cnt = 0;; cnt++) {
    if (cnt == nb_enchaine) { desc = NULL; break; }
    if (strcmp(desc->nom , ref) == 0) break;
    desc++;
  }
#if FLG_TRACE_IRDATA
  fprintf(stderr, "get_ench_calcul = ");
  /*trace_regle_calcul(desc) ;*/
  putc('\n', stderr);
#endif /* FLG_TRACE_IRDATA */
  return desc;
}

void IRDATA_exec_ench(T_irdata *irdata, T_ench_calcul p_ench)
{
#if FLG_TRACE_IRDATA
  fprintf(stderr, "exec_ench(");
  /*trace_regle_calcul(p_calcul) ;*/
  fprintf(stderr, ")\n");
#endif /* FLG_TRACE_IRDATA */
  (*p_ench->proc)(irdata);
#if FLG_TRACE_IRDATA
  fprintf(stderr, "exec_ench = void\n");
#endif /* FLG_TRACE_IRDATA */
}

#ifdef FLG_DEBUG

T_regle_calcul IRDATA_get_regle_calcul(int ref)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_regle_calcul(%d)\n", ref);
#endif /* FLG_TRACE_IRDATA */
  int cnt;
  T_desc_call *desc = desc_call;
  for (cnt = 0;; cnt++) {
    if (cnt == nb_call) { desc = NULL; break; }
    if (desc->num == ref) break;
    desc++;
  }
#ifdef FLG_TRACE_IRDATA
    fprintf(stderr, "get_regle_calcul = ");
    trace_regle_calcul(desc);
    putc('\n', stderr);
#endif /* FLG_TRACE_IRDATA */
  return desc;
}

void IRDATA_exec_calcul(T_irdata *irdata, T_regle_calcul p_calcul)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "exec_calcul(");
  trace_regle_calcul(p_calcul);
  fprintf(stderr, ")\n");
#endif /* FLG_TRACE_IRDATA */
  (*p_calcul->proc)(irdata);
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "exec_calcul = void\n");
#endif /* FLG_TRACE_IRDATA */
}

#endif /* FLG_DEBUG */

#if defined(FLG_DEBUG) || defined(FLG_CONTROLE_IMMEDIAT)

T_regle_verif IRDATA_get_regle_verif(int ref)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_regle_verif(%d)\n", ref);
#endif /* FLG_TRACE_IRDATA */
  int cnt;
  T_desc_verif *desc = desc_verif;
  for (cnt = 0;; cnt++) {
    if (cnt == nb_verif) { desc = NULL; break; }
    if (desc->num == ref) break;
    desc++;
  }
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "get_regle_verif = ");
  trace_regle_verif(desc);
  putc('\n', stderr);
#endif /* FLG_TRACE_IRDATA */
  return desc;
}

T_discord * IRDATA_exec_verif(T_irdata *irdata, T_regle_verif p_verif)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "exec_verif(");
  trace_regle_verif(p_verif);
  fprintf(stderr, ")\n");
#endif /* FLG_TRACE_IRDATA */
  T_discord *retour;
  if (p_verif->proc == NULL) retour = NULL;
  else retour = une_verif(irdata, p_verif->proc);
#ifdef FLG_TRACE_IRDATA
  if (flg_trace_irdata) {
    fprintf(stderr, "exec_verif = ");
    trace_discord(retour);
    putc('\n', stderr);
  }
  return retour;
#endif /* FLG_TRACE_IRDATA */
}

#endif /* FLG_DEBUG || FLG_CONTROLE_IMMEDIAT */



/****************************************************************/
/* NOUVELLES FONCTIONS DE MANIPULATION DES ERREURS              */
/* A UTILISER DE PREFERENCE DES LA CAMPAGNE 2006.               */
/****************************************************************/
char IRDATA_GetFamille_erreur(const T_discord *pp_discord)
{
  return pp_discord->erreur->nom[0];
}

const char * IRDATA_GetCodeBO_erreur(T_discord *pp_discord)
{
  return pp_discord->erreur->codebo;
}

const char * IRDATA_GetSousCode_erreur(T_discord *pp_discord)
{
  return pp_discord->erreur->souscode;
}

char IRDATA_GetIsIsf_erreur(const T_discord *pp_discord)
{
  char *retour;
  retour = pp_discord->erreur->isisf;
  return retour[0];
}

const char * IRDATA_GetLibelle_erreur(T_discord *pp_discord)
{
  /*variables locales*/
  char	*l_libelle_seul;

  l_libelle_seul = strstr(pp_discord->erreur->message, ERREUR_DELIMITEUR);
  return (l_libelle_seul ? l_libelle_seul : pp_discord->erreur->message);
}

const char * IRDATA_GetCodeInterne_erreur(T_discord *pp_discord)
{
  return pp_discord->erreur->nom;
}

short IRDATA_GetType_erreur(const T_discord *pp_discord)
{
  return pp_discord->erreur->type;
}



/****************************************************************/
/* ANCIENNES FONCTIONS DE MANIPULATION DES ERREURS              */
/* A NE PLUS UTILISER DE PREFERENCE APRES LA CAMPAGNE 2005.     */
/****************************************************************/
char * IRDATA_lib_erreur(struct S_discord *discord)
{
  return discord->erreur->message;
}

char * IRDATA_codebo_erreur(struct S_discord *discord)
{
  return discord->erreur->codebo;
}

short IRDATA_type_erreur(const struct S_discord *discord)
{
  return discord->erreur->type;
}

int IRDATA_erreur_bloquante(const struct S_discord *discord)
{
  return (discord->erreur->type == ANOMALIE);
}

char * IRDATA_nom_erreur(struct S_discord *discord)
{
  return discord->erreur->nom;
}

struct S_discord * IRDATA_erreur_suivante(struct S_discord *discord)
{
#ifdef FLG_TRACE_IRDATA
  fprintf(stderr, "err_suivante = ");
  trace_discord(discord->suivant);
  putc('\n', stderr);
#endif /* FLG_TRACE_IRDATA */
  return discord->suivant;
}



#ifdef FLG_DEBUG

T_desc_err * IRDATA_get_desc_err(const char *nom)
{
  return (T_desc_err *)cherche_nom(nom, (char *)desc_err, sizeof(T_desc_err), nb_err);
}

char * IRDATA_desc_err_lib_erreur(struct S_desc_err *desc_err)
{
  return desc_err->erreur->message;
}

short IRDATA_desc_err_type_erreur(const struct S_desc_err *desc_err)
{
  return desc_err->erreur->type;
}

char * IRDATA_desc_err_codebo_erreur(struct S_desc_err *desc_err)
{
  return desc_err->erreur->codebo;
}

int IRDATA_desc_err_erreur_bloquante(const struct S_desc_err *desc_err)
{
  return (desc_err->erreur->type == ANOMALIE);
}

char * IRDATA_desc_err_nom_erreur(struct S_desc_err *desc_err)
{
  return desc_err->erreur->nom;
}

#endif /* FLG_DEBUG */

double cherche_varinfo(T_irdata *irdata, char *nom) {
#define CHERCHE_VARINFO(TYP,TAB) \
  for (i = 0; i < NB_##TYP; i++) { \
    T_varinfo_##TYP *info = &(varinfo_##TYP[i]); \
    if ( \
      strcmp(nom, info->name) == 0 \
      || (info->alias != NULL && strcmp(nom, info->alias) == 0) \
    ) { \
      if (irdata->def_##TAB [info->idx] == 0) { \
        return NAN; \
      } else { \
        return irdata->TAB[info->idx]; \
      } \
    } \
  } \

  int i;
  CHERCHE_VARINFO(calculee,calculee)
  CHERCHE_VARINFO(calculee_base,base)
  CHERCHE_VARINFO(calculee_base_restituee,base)
  CHERCHE_VARINFO(calculee_restituee,calculee)
  CHERCHE_VARINFO(saisie_contexte,saisie)
  CHERCHE_VARINFO(saisie_famille,saisie)
  CHERCHE_VARINFO(saisie_penalite,saisie)
  CHERCHE_VARINFO(saisie_revenu,saisie)
  CHERCHE_VARINFO(saisie_variation,saisie)
#undef CHERCHE_VARINFO
  return -1;
}

void pr_var(T_irdata *irdata, char *prefix, char *nom) {
  double val = cherche_varinfo(irdata, nom);
  if (isinf(val)) {
    fprintf(stderr, "XXX %s %s inconnue\n", prefix, nom);
  } else if (isnan(val)) {
    fprintf(stderr, "XXX %s %s = indefini\n", prefix, nom);
  } else {
    fprintf(stderr, "XXX %s %s = ", prefix, nom);
    print_double(stderr, &(irdata->ctx_pr_err), val, 0, 30);
    fprintf(stderr, "\n");
  }
}

/* Gestion des erreurs */

void finalise_erreur(T_irdata *irdata) {

}

void exporte_erreur(T_irdata *irdata) {

}


