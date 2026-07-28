#ifndef __OPTIONS_H__
#define __OPTIONS_H__

#include <mem.h>
#include <liste.h>
#include <commun.h>

typedef struct S_options_trt {
  T_mode mode;
  int annee;
  int recursif;
  int strict;
  L_S_varVal defs;
  L_char fichiers;
} S_options_trt;

typedef struct S_options_fmt {
  int recursif;
  int strict;
  L_char fichiers;
} S_options_fmt;

typedef struct S_options_aid {
  char *cat;
  int err;
} S_options_aid;

#define ACT_TRT 0
#define ACT_FMT 1
#define ACT_AID 2

typedef struct S_options {
  int action;
  union {
    S_options_trt trt;
    S_options_fmt fmt;
    S_options_aid aid;
  } args;
  char *exe;  
} S_options;

typedef struct S_options * T_options;

extern T_options analyseLdc(T_tas tas, int argc, char **argv);

#endif /* __OPTIONS_H__ */
