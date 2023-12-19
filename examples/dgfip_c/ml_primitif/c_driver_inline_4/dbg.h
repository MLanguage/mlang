/****** LICENCE CECIL *****/

#ifndef _DEBUG_H_
#define _DEBUG_H_

#include "conf.h"

#ifdef FLG_DEBUG
extern int nb_err;
#if NB_DEBUG_C <= 0
extern int nb_debug;
#else
extern int nb_debug01;
extern int nb_debug02;
extern int nb_debug03;
extern int nb_debug04;
#endif /* NB_DEBUG_C <= 0 */
extern int nb_call;
#endif /* FLG_DEBUG */

#if defined(FLG_DEBUG) || defined(FLG_CONTROLE_IMMEDIAT)
extern int nb_verif;
#endif /* FLG_DEBUG || FLG_CONTROLE_IMMEDIAT */

#ifdef FLG_TRACE
extern int niv_trace;
extern void aff1 _PROTS((const char *nom));
extern void aff_val _PROTS((const char *nom, const T_irdata *irdata, int indice, int niv, const char *chaine, int is_tab, int expr, int maxi));
extern void aff_double _PROTS((const char *nom, double valeur));
#define aff2(nom,irdata,indice) aff_val(nom,irdata,indice,2,"<-", 0, 0, 1)
#define aff2_tab(nom,irdata,indice,expr,maxi) aff_val(nom,irdata,indice,2,"<-", 1, expr, maxi)
#define aff3(nom,irdata,indice) aff_val(nom,irdata,indice,3,":", 0, 0, 1)
#define aff3_tab(nom,irdata,indice,expr,maxi) aff_val(nom,irdata,indice,3,":", 1, expr, maxi)
#endif /* FLG_TRACE */

#endif /* _DEBUG_H_ */

