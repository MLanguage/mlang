
#include "conf.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <math.h>
#include <limits.h>
#include "var.h"

extern FILE * fd_trace_dialog;

static void add_erreur_code(T_erreur *erreur, const char *code)
{
  size_t len = 0;
  char *new_message = NULL;
  char *debut = NULL;

  if (code != NULL) {
    debut = strstr(erreur->message," ((");
    if (debut != NULL) {
      len = strlen(erreur->message) - strlen(debut);
    } else {
      len = strlen(erreur->message);
    }

    new_message = (char *)malloc((len + 10) * sizeof(char));
    memset(new_message, '\0', (len + 10) * sizeof(char));
    strncpy(new_message, erreur->message, len);
    strcat(new_message, " ((");
    strcat(new_message, code);
    strcat(new_message, "))\0");
    erreur->message = new_message;
  }
}

#ifdef FLG_MULTITHREAD

double my_floor(double a)
{
  return floor(a + 0.000001);
}

double my_ceil(double a)
{
  return ceil(a - 0.000001);
}

double my_arr(double a)
{
  double v1, v2;
  v1 = floor(a);
  v2 = floor((a - v1) * 100000. + 0.5) / 100000.;
  return floor(v1 + v2 + 0.5);
}

static void init_erreur(T_irdata *irdata)
{
  if (irdata->nb_bloquantes >= irdata->max_bloquantes) {
    IRDATA_reset_erreur(irdata);
  }
}

void add_erreur(T_irdata *irdata, T_erreur *erreur, char *code)
{
  T_discord *new_discord = NULL;

  if (irdata->tas_discord == 0) {
    new_discord = (T_discord *)malloc(sizeof(T_discord));
  } else {
    new_discord = irdata->tas_discord;
    irdata->tas_discord = new_discord->suivant;
  }

  add_erreur_code(erreur, code);

  new_discord->erreur = erreur;
  new_discord->suivant = 0;
  *irdata->p_discord = new_discord;
  irdata->p_discord = &new_discord->suivant;

  if (strcmp(erreur->isisf, "O")) {
    if ((erreur->type == ANOMALIE) && (++irdata->nb_bloquantes >= irdata->max_bloquantes)) {
      longjmp(irdata->jmp_bloq, 1);
    }
  }
}

void free_erreur()
{
}

#else

static T_discord *discords = 0;
static T_discord *tas_discord = 0;
static T_discord **p_discord = &discords;
static jmp_buf jmp_bloq;

static void init_erreur(void)
{
  *p_discord = tas_discord;
  tas_discord = discords;
  discords = 0;
  p_discord = &discords;
}

void add_erreur(T_erreur *erreur, char *code)
{
  T_discord *new_discord = NULL;

  if (tas_discord == 0) {
    new_discord = (T_discord *)malloc(sizeof(T_discord));
  } else {
    new_discord = tas_discord;
    tas_discord = new_discord->suivant;
  }

  add_erreur_code(erreur, code);

  new_discord->erreur = erreur;
  new_discord->suivant = 0;
  *p_discord = new_discord;
  p_discord = &new_discord->suivant;

  if (strcmp(erreur->isisf, "O")) {
    if (erreur->type == ANOMALIE) {
      longjmp(jmp_bloq, 1);
    }
  }
}

void free_erreur()
{
  T_discord *temp_discords = discords;
  T_discord *dd = NULL;
  char *debut = NULL;
  int i = 0;

  while (temp_discords != NULL) {
    dd = temp_discords;
    temp_discords = temp_discords->suivant;
    if (dd->erreur->message != NULL) {
      debut = strstr(dd->erreur->message, " ((");
      if (debut != NULL) {
        free(dd->erreur->message);
      }
      dd->erreur->message = NULL;
    }
  }
}

#endif /* FLG_MULTITHREAD */



#ifdef FLG_TRACE

int niv_trace = 3;

#ifdef FLG_API
#define TRACE_FILE fd_trace_dialog
#else
#define TRACE_FILE stderr
#endif /* FLG_API */

void aff_val(const char *nom, const T_irdata *irdata, int indice, int niv, const char *chaine, int is_tab, int expr, int maxi)
{
  double valeur;
  int def;
  if (expr < 0) {
    if (niv_trace >= niv) {
#ifdef FLG_COLOR
      fprintf(TRACE_FILE, "\033[%d;%dm%s[%d] %s 0\033[0m\n",
              color, typo, nom, expr, chaine);
#else
      fprintf(TRACE_FILE, "%s[%d] %s 0m\n", nom, expr, chaine);
#endif /* FLG_COLOR */
    }
    return;
  } else if (expr >= maxi) {
#ifdef FLG_COLOR
    fprintf(TRACE_FILE,
            "\033[%d;%dmerreur: indice (%d) superieur au maximum (%d)\033[0m\n",
            color, typo, expr, maxi);
#else
    fprintf(TRACE_FILE, "erreur: indice (%d) superieur au maximum (%d)\n",
            expr, maxi);
#endif /* FLG_COLOR */
    expr = 0;
  }
#ifdef FLG_COMPACT
  valeur = irdata->valeurs[indice + expr];
  def = irdata->defs[indice + expr];
#else
  switch (indice & EST_MASQUE) {
    case EST_SAISIE:
      valeur = irdata->saisie[(indice & INDICE_VAL) + expr];
      def = irdata->def_saisie[(indice & INDICE_VAL) + expr];
      break;
    case EST_CALCULEE:
      valeur = irdata->calculee[(indice & INDICE_VAL) + expr];
      def = irdata->def_calculee[(indice & INDICE_VAL) + expr];
      break;
    case EST_BASE:
      valeur = irdata->base[(indice & INDICE_VAL) + expr];
      def = irdata->def_base[(indice & INDICE_VAL) + expr];
      break;
  }
#endif /* FLG_COMPACT */
  if (is_tab) {
    if (def == 0) {
      if (valeur != 0) {
#ifdef FLG_COLOR
        fprintf(TRACE_FILE, "\033[%d;%dm%s[%d] : erreur undef = %lf\033[0m\n",
                color, typo, nom, expr, valeur);
#else
        fprintf(TRACE_FILE, "%s[%d] : erreur undef = %lf\n", nom, expr, valeur);
#endif /* FLG_COLOR */
      } else if (niv_trace >= niv) {
#ifdef FLG_COLOR
        fprintf(TRACE_FILE, "\033[%d;%dm%s[%d] %s undef\033[0m\n",
                color, typo, nom, expr, chaine);
#else
        fprintf(TRACE_FILE, "%s[%d] %s undef\n", nom, expr, chaine);
#endif /* FLG_COLOR */
      }
    } else if (def != 1) {
#ifdef FLG_COLOR
      fprintf(TRACE_FILE, "\033[%d;%dm%s[%d] : erreur flag def = %d\033[0m\n",
              color, typo, nom, expr, def);
#else
      fprintf(TRACE_FILE, "%s[%d] : erreur flag def = %d\n", nom, expr, def);
#endif /* FLG_COLOR */
    } else if (niv_trace >= niv) {
#ifdef FLG_COLOR
      fprintf(TRACE_FILE, "\033[%d;%dm%s[%d] %s %lf\033[0m\n",
              color, typo, nom, expr, chaine, valeur);
#else
      fprintf(TRACE_FILE, "%s[%d] %s %lf\n", nom, expr, chaine, valeur);
#endif /* FLG_COLOR */
    }
  } else {
    if (def == 0) {
      if (valeur != 0) {
#ifdef FLG_COLOR
        fprintf(TRACE_FILE, "\033[%d;%dm%s : erreur undef = %lf\033[0m\n",
                color, typo, nom, valeur);
#else
        fprintf(TRACE_FILE, "%s : erreur undef = %lf\n", nom, valeur);
#endif /* FLG_COLOR */
      } else if (niv_trace >= niv) {
#ifdef FLG_COLOR
        fprintf(TRACE_FILE, "\033[%d;%dm%s %s undef\033[0m\n",
                color, typo, nom, chaine);
#else
        fprintf(TRACE_FILE, "%s %s undef\n", nom, chaine);
#endif /* FLG_COLOR */
      }
    } else if (def != 1) {
#ifdef FLG_COLOR
      fprintf(TRACE_FILE, "\033[%d;%dm%s : erreur flag def = %d\033[0m\n",
              color, typo, nom, def);
#else
      fprintf(TRACE_FILE, "%s : erreur flag def = %d\n", nom, def);
#endif /* FLG_COLOR */
    } else if (niv_trace >= niv) {
#ifdef FLG_COLOR
      fprintf(TRACE_FILE, "\033[%d;%dm%s %s %lf\033[0m\n",
              color, typo, nom, chaine, valeur);
#else
      fprintf(TRACE_FILE, "%s %s %lf\n", nom, chaine, valeur);
#endif /* FLG_COLOR */
    }
  }
}

#endif /* FLG_TRACE */



T_discord * no_error(T_irdata *irdata)
{
  return NULL;
}

T_discord * err_NEGATIF(T_irdata *irdata)
{
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
  add_erreur(&erreur_A000, NULL);
  return discords;
#endif /* FLG_MULTITHREAD */
}

double floor_g(double a)
{
  if (fabs(a) <= (double)LONG_MAX) {
    return floor(a);
  } else {
    return a;
  }
}

double ceil_g(double a)
{
  if (fabs(a) <= (double)LONG_MAX) {
    return ceil(a);
  } else {
    return a;
  }
}

int multimax_def(int nbopd, char *var)
{
  int i = 0;
  for (i = 0; i < nbopd; i++) {
    if (var[i] == 1) return 1;
  }
  return 0;
}

double multimax(double nbopd, double *var)
{
  int i = 0;
  double s = 0.0;
  for (i = 0; i < (int)nbopd; i++) {
    if (var[i] >= s) s = var[i];
  }
  return s;
}

int modulo_def(int a, int b)
{
  return a;
}

double modulo(double a, double b)
{
  return (double)(((int)a) % ((int)b));
}





/* Temporary wrapper functions for verifications */

struct S_discord * verif_saisie_cohe_primitive_raw(T_irdata *irdata);
struct S_discord * verif_saisie_cohe_primitive(T_irdata *irdata)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  verif_saisie_cohe_primitive_raw(irdata);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  verif_saisie_cohe_primitive_raw(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}

struct S_discord * verif_saisie_cohe_primitive_isf_raw(T_irdata *irdata);
struct S_discord * verif_saisie_cohe_primitive_isf(T_irdata *irdata, int appel)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if ((appel != 1) && (setjmp(irdata->jmp_bloq) != 0)) {
    return irdata->discords;
  }
  verif_saisie_cohe_primitive_isf_raw(irdata);
  return irdata->discords;
#else
  init_erreur();
  if ((appel != 1) && (setjmp(jmp_bloq) != 0)) {
    return discords;
  }
  verif_saisie_cohe_primitive_isf_raw(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}

struct S_discord * verif_saisie_cohe_corrective_raw(T_irdata *irdata);
struct S_discord * verif_saisie_cohe_corrective(T_irdata *irdata)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  verif_saisie_cohe_corrective_raw(irdata);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  verif_saisie_cohe_corrective_raw(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}

struct S_discord * verif_cohe_horizontale_raw(T_irdata *irdata);
struct S_discord * verif_cohe_horizontale(T_irdata *irdata)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  verif_cohe_horizontale_raw(irdata);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  verif_cohe_horizontale_raw(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}

struct S_discord * verif_contexte_cohe_primitive_raw(T_irdata *irdata);
struct S_discord * verif_contexte_cohe_primitive(T_irdata *irdata)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  verif_contexte_cohe_primitive_raw(irdata);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  verif_contexte_cohe_primitive_raw(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}

struct S_discord * verif_contexte_cohe_corrective_raw(T_irdata *irdata);
struct S_discord * verif_contexte_cohe_corrective(T_irdata *irdata)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  verif_contexte_cohe_corrective_raw(irdata);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  verif_contexte_cohe_corrective_raw(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}

struct S_discord * verif_famille_cohe_primitive_raw(T_irdata *irdata);
struct S_discord * verif_famille_cohe_primitive(T_irdata *irdata)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  verif_famille_cohe_primitive_raw(irdata);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  verif_famille_cohe_primitive_raw(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}

struct S_discord * verif_famille_cohe_corrective_raw(T_irdata *irdata);
struct S_discord * verif_famille_cohe_corrective(T_irdata *irdata)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  verif_famille_cohe_corrective_raw(irdata);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  verif_famille_cohe_corrective_raw(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}

struct S_discord * verif_revenu_cohe_primitive_raw(T_irdata *irdata);
struct S_discord * verif_revenu_cohe_primitive(T_irdata *irdata)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  verif_revenu_cohe_primitive_raw(irdata);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  verif_revenu_cohe_primitive_raw(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}

struct S_discord * verif_revenu_cohe_corrective_raw(T_irdata *irdata);
struct S_discord * verif_revenu_cohe_corrective(T_irdata *irdata)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  verif_revenu_cohe_corrective_raw(irdata);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  verif_revenu_cohe_corrective_raw(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}

struct S_discord * verif_calcul_primitive_raw(T_irdata *irdata);
struct S_discord * verif_calcul_primitive(T_irdata *irdata)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  verif_calcul_primitive_raw(irdata);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  verif_calcul_primitive_raw(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}

struct S_discord * verif_calcul_primitive_isf_raw(T_irdata *irdata);
struct S_discord * verif_calcul_primitive_isf(T_irdata *irdata)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
#else
  init_erreur();
#endif /* FLG_MULTITHREAD */
  return verif_calcul_primitive_isf_raw(irdata);
}

struct S_discord * verif_calcul_corrective_raw(T_irdata *irdata);
struct S_discord * verif_calcul_corrective(T_irdata *irdata)
{
#ifdef FLG_MULTITHREAD
  init_erreur(irdata);
  if (setjmp(irdata->jmp_bloq) != 0) {
    return irdata->discords;
  }
  verif_calcul_corrective_raw(irdata);
  return irdata->discords;
#else
  init_erreur();
  if (setjmp(jmp_bloq) != 0) {
    return discords;
  }
  verif_calcul_corrective_raw(irdata);
  return discords;
#endif /* FLG_MULTITHREAD */
}
