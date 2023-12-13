
#ifndef _CONST_H_
#define _CONST_H_

#include <setjmp.h>

#include "conf.h"

#ifdef FLG_COMPACT

struct S_irdata
{
  double valeurs[NB_VARS];
  char defs[NB_VARS];
};

#define S_ irdata->valeurs
#define C_ irdata->valeurs
#define B_ irdata->valeurs
#define DS_ irdata->defs
#define DC_ irdata->defs
#define DB_ irdata->defs

#else

struct S_irdata
{
  double *saisie;
  double *calculee;
  double *base;
  char *def_saisie;
  char *def_calculee;
  char *def_base;
#ifdef FLG_MULTITHREAD
  T_discord *discords;
  T_discord *tas_discord;
  T_discord **p_discord;
  int nb_bloquantes;
  int max_bloquantes;
  jmp_buf jmp_bloq;
#endif /* FLG_MULTITHREAD */
};

#define S_ irdata->saisie
#define C_ irdata->calculee
#define B_ irdata->base
#define DS_ irdata->def_saisie
#define DC_ irdata->def_calculee
#define DB_ irdata->def_base

#define EST_SAISIE    0x0000
#define EST_CALCULEE  0x4000
#define EST_BASE      0x8000
#define EST_MASQUE    0xc000
#define INDICE_VAL    0x3fff

#endif /* FLG_COMPACT */

#define MAX_ANO  4

#define RESTITUEE    5
#define RESTITUEE_P  6
#define RESTITUEE_C  7

struct S_erreur
{
  char *message;
  char *codebo;
  char *souscode;
  char *isisf;
  char *nom;
  short type;
};

struct S_discord
{
  struct S_discord *suivant;
  T_erreur *erreur;
};

#ifdef FLG_MULTITHREAD
extern void add_erreur(T_irdata *irdata, T_erreur *erreur, char *code);
extern void free_erreur();
#else
extern void add_erreur(T_erreur *erreur, char *code);
extern void free_erreur();
#endif /* FLG_MULTITHREAD */

#define fabs(a) (((a) < 0.0) ? -(a) : (a))
#define min(a,b)	(((a) <= (b)) ? (a) : (b))
#define max(a,b)	(((a) >= (b)) ? (a) : (b))

#define EPSILON 0.000001
#define GT_E(a,b) ((a) > (b) + EPSILON)
#define LT_E(a,b) ((a) + EPSILON < (b))
#define GE_E(a,b) ((a) > (b) - EPSILON)
#define LE_E(a,b) ((a) - EPSILON < (b))
#define EQ_E(a,b) (fabs((a) - (b)) < EPSILON)
#define NEQ_E(a,b) (fabs((a) - (b)) >= EPSILON)
#define my_floor(a) (floor_g((a) + EPSILON))
#define my_ceil(a) (ceil_g((a) - EPSILON));
#define my_arr(a) (((a) < 0) ? ceil_g((a) - EPSILON - 0.5) : floor_g((a) + EPSILON + 0.5))
#define divd(a,b)	(NEQ_E((b),0.0) ? (a / b) : 0.0)

extern double floor_g(double);
extern double ceil_g(double);
extern int multimax_def(int, char *);
extern double multimax(double, double *);
extern int modulo_def(int, int);
extern double modulo(double, double);

#endif /* _CONST_H_ */
