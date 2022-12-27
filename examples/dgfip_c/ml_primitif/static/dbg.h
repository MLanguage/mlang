/****** LICENCE CECIL *****/

#ifndef _DEBUG_H_
#define _DEBUG_H_

#include "conf.h"

#ifdef FLG_TRACE

extern int niv_trace;

extern void aff_val (const char *nom, const T_irdata *irdata, int indice, int niv, const char *chaine, int is_tab, int expr, int maxi);

#define aff2(nom, irdata, indice) aff_val(nom, irdata, indice, 2, "<-", 0, 0, 1)

#define aff3(nom, irdata, indice) aff_val(nom, irdata, indice, 3, ":", 0, 0, 1)

#endif /* FLG_TRACE */

#endif /* _DEBUG_H_ */
