
#ifndef _IRDATA_H_
#define _IRDATA_H_

#include "mlang.h"

struct S_desc_var {
  char *nom;
  int indice;
  long type_donnee;
  T_discord * (*verif)(T_irdata *);
};

typedef struct S_desc_var T_desc_var;

extern int sz_err_finalise;
extern char **err_finalise;
extern int nb_err_finalise;
extern int sz_err_sortie;
extern char **err_sortie;
extern int nb_err_sortie;
extern int sz_err_archive;
extern char **err_archive;
extern int nb_err_archive;

extern T_irdata * IRDATA_new_irdata(void);
extern void IRDATA_delete_irdata(T_irdata *irdata);
extern void IRDATA_reset_irdata(T_irdata *irdata);
extern void IRDATA_reset_erreur(T_irdata *irdata);
extern void IRDATA_reset_base(T_irdata *irdata);
extern void IRDATA_reset_light(T_irdata *irdata);
extern void IRDATA_reset_calculee(T_irdata *irdata);
extern void IRDATA_recopie_irdata(T_irdata *src_irdata, T_irdata *dst_irdata);
extern void IRDATA_range_base(T_irdata *irdata, T_var_irdata desc, double valeur);
extern void IRDATA_efface(T_irdata *irdata, T_var_irdata desc);
extern double * IRDATA_extrait_special (T_irdata *irdata, T_var_irdata desc);
extern double * IRDATA_extrait_tableau(T_irdata *irdata, T_var_irdata desc, int ind);

extern void finalise_erreur(T_irdata *irdata);
extern void exporte_erreur(T_irdata *irdata);

#endif /* _IRDATA_H_ */

