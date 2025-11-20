#ifndef __COMMUN_H__
#define __COMMUN_H__

#include <mem.h>
#include <liste.h>
#include <irj.h>

#include <mlang.h>

typedef enum { Primitif, Correctif } T_mode;

struct S_varVal;
typedef struct S_varVal S_varVal;
typedef S_varVal * T_varVal;
struct S_varVal {
  T_varinfo *varinfo;
  double val;
};

TYPEDEF_LISTE(S_varVal)

extern T_varVal creeVarVal(T_tas tas, char *nom, double val);
extern void libereVarVal(T_varVal vv);

#endif /* __COMMUN_H__ */
