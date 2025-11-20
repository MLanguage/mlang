#ifndef __IRJ_H__
#define __IRJ_H__

#include <mem.h>
#include <fichiers.h>

#define LNG_IRJ_BUF 256
#define IRJ_LNG_MAX 1024

#define IRJ_CODE_VIDE (-1)
#define IRJ_INVALIDE 0
#define IRJ_DEBUT 1
#define IRJ_NOM_DEBUT 2
#define IRJ_NOM 3
#define IRJ_NOM_FIN 4
#define IRJ_ENTREES_PRIMITIF_DEBUT 5
#define IRJ_ENTREES_PRIMITIF_FIN 6
#define IRJ_CONTROLES_PRIMITIF_DEBUT 7
#define IRJ_CONTROLES_PRIMITIF_FIN 8
#define IRJ_RESULTATS_PRIMITIF_DEBUT 9
#define IRJ_RESULTATS_PRIMITIF_FIN 10
#define IRJ_ENTREES_CORRECTIF_DEBUT 11
#define IRJ_ENTREES_CORRECTIF_FIN 12
#define IRJ_CONTROLES_CORRECTIF_DEBUT 13
#define IRJ_CONTROLES_CORRECTIF_FIN 14
#define IRJ_RESULTATS_CORRECTIF_DEBUT 15
#define IRJ_RESULTATS_CORRECTIF_FIN 16
#define IRJ_ENTREES_RAPPELS_DEBUT 17
#define IRJ_ENTREES_RAPPELS_FIN 18
#define IRJ_CONTROLES_RAPPELS_DEBUT 19
#define IRJ_CONTROLES_RAPPELS_FIN 20
#define IRJ_RESULTATS_RAPPELS_DEBUT 21
#define IRJ_RESULTATS_RAPPELS_FIN 22
#define IRJ_IGNORE 23
#define IRJ_DEF_VAR 24
#define IRJ_DEF_ANO 25
#define IRJ_DEF_RAP 26
#define IRJ_DEF_CORR 27
#define IRJ_FIN 28

struct S_irj_def_var;
typedef struct S_irj_def_var S_irj_def_var;
typedef S_irj_def_var * T_irj_def_var;
struct S_irj_def_var {
  char *var;
  double val;
};

struct S_irj_def_ano;
typedef struct S_irj_def_ano S_irj_def_ano;
typedef S_irj_def_ano * T_irj_def_ano;
struct S_irj_def_ano {
  char *ano;
};

struct S_irj_def_rap;
typedef struct S_irj_def_rap S_irj_def_rap;
typedef S_irj_def_rap * T_irj_def_rap;
struct S_irj_def_rap {
  double numero;
  double rappel;
  char *code;
  double montant;
  double sens;
  double penalite;
  double base_tl;
  double date;
  double _2042_rect;
};

struct S_irj;
typedef struct S_irj S_irj;
typedef S_irj * T_irj;
struct S_irj {
  int code;
  int section;
  union {
    char *nom; /* IRJ_NOM */
    S_irj_def_var defVar; /* variable = valeur */
    S_irj_def_ano defAno; /* anomalie */
    S_irj_def_rap defRap; /* rappel */
    int err;
  } args;
  int strict;
  int codePri;
  int ligne;
  char *buf;
  int pos;
  int lng;
  T_tas tas;
};

extern T_irj creeIrj(T_tas tas, int strict);
extern void lisIrj(T_fich fich, T_irj irj);
extern int codeIrj(T_irj irj);
extern char *codeIrjVersStr(int code);
extern void detruisIrj(T_irj irj);

#endif /* __IRJ_H__ */
