
#ifndef _IRDATA_H_
#define _IRDATA_H_

#include "conf.h"

#define _PROTS(X) X

#define ANOMALIE     1
#define DISCORDANCE  2
#define INFORMATIVE  4

#define BOOLEEN        0x1
#define ENTIER         0x100
#define REEL           0x200
#define REEL1          0x400
#define REEL2          0x800
#define REEL3          0x1000
#define DATE_JJMMAAAA  0x10000
#define DATE_MMAAAA    0x20000
#define DATE_AAAA      0x40000
#define DATE_JJMM      0x80000
#define DATE_MM        0x100000
#define DATE           (DATE_JJMMAAAA|DATE_MMAAAA|DATE_AAAA|DATE_JJMM|DATE_MM)
#define NUMERIQUE      (ENTIER|REEL|REEL1|REEL2|REEL3)

#define T_var_irdata T_desc_var *

typedef struct S_desc_var T_desc_var;
typedef struct S_irdata T_irdata;
typedef struct S_discord T_discord;
typedef struct S_erreur T_erreur;

extern T_irdata * IRDATA_new_irdata(void);
extern void IRDATA_delete_irdata(T_irdata *irdata);
extern void IRDATA_reset_irdata(T_irdata *irdata);

extern void IRDATA_reset_base(T_irdata *irdata);
extern void IRDATA_reset_calculee(T_irdata *irdata);

extern void IRDATA_reset_erreur(T_irdata *irdata);

extern T_discord * IRDATA_range(T_irdata *irdata, T_desc_var *desc, double valeur);
extern void IRDATA_range_base(T_irdata *irdata, T_desc_var *desc, double valeur);
struct S_discord *IRDATA_range_tableau(T_irdata *irdata, T_desc_var *desc, int ind, double valeur);

extern double * IRDATA_extrait_special (T_irdata *irdata, T_desc_var *desc);
double * IRDATA_extrait_tableau(T_irdata *irdata, T_desc_var *desc, int ind);

extern T_desc_var * IRDATA_cherche_desc_var(const char *nom);

extern T_discord * err_NEGATIF(T_irdata *irdata);

#endif /* _IRDATA_H_ */
