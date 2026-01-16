#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <utils.h>
#include <mem.h>
#include <liste.h>
#include <chaine.h>
#include <commun.h>

#include <mlang.h>

T_varVal creeVarVal(T_tas tas, char *nom, double val) {
  T_varVal vv = NULL;

  assert(nom != NULL);
  vv = memAlloue(tas, sizeof (S_varVal));
  vv->varinfo = cherche_varinfo_statique(nom);
  if (vv->varinfo == NULL) return NULL;
  vv->val = val;
  return vv;
}

void libereVarVal(T_varVal vv) {
  if (vv == NULL) return;
  memLibere(vv);
}
