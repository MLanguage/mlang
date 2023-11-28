
#ifndef _IRDATA_H_
#define _IRDATA_H_

#include "mlang.h"
#include "desc_inv.h"

extern T_irdata * IRDATA_new_irdata(void);
extern void IRDATA_delete_irdata(T_irdata *irdata);
extern void IRDATA_reset_irdata(T_irdata *irdata);

extern void IRDATA_reset_base(T_irdata *irdata);
extern void IRDATA_reset_light(T_irdata *irdata);
extern void IRDATA_reset_calculee(T_irdata *irdata);

extern void IRDATA_recopie_irdata(T_irdata *src_irdata, T_irdata *dst_irdata);

extern void IRDATA_reset_erreur(T_irdata *irdata);

extern T_discord * IRDATA_range(T_irdata *irdata, T_var_irdata desc, double valeur);
extern void IRDATA_range_base(T_irdata *irdata, T_var_irdata desc, double valeur);
struct S_discord * IRDATA_range_tableau(T_irdata *irdata, T_var_irdata desc, int ind, double valeur);

extern void IRDATA_efface(T_irdata *irdata, T_var_irdata desc);
extern void IRDATA_efface_tableau(T_irdata *irdata, T_var_irdata desc, int ind);

extern double * IRDATA_extrait_special (T_irdata *irdata, T_var_irdata desc);
double * IRDATA_extrait_tableau(T_irdata *irdata, T_var_irdata desc, int ind);

extern T_var_irdata IRDATA_cherche_desc_var(const char *nom);

extern T_discord * err_NEGATIF(T_irdata *irdata);

#endif /* _IRDATA_H_ */
