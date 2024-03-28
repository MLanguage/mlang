/****** LICENCE CECIL *****/

#ifndef _VAR_H_
#define _VAR_H_

#include "mlang.h"
#include "compir.h"
#include "compir_desc.h"
#include "compir_desc_inv.h"
#include "irdata.h"

extern int taille_saisie;
extern int taille_calculee;
extern int taille_base;
extern int taille_totale;

extern int nb_contexte;
extern int nb_famille;
extern int nb_revenu;
extern int nb_revenu_correc;
extern int nb_variation;
extern int nb_penalite;
extern int nb_restituee;
extern int nb_enchaine;

extern int color;
extern int typo;

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
#endif

#if defined (FLG_DEBUG) || defined (FLG_CONTROLE_IMMEDIAT)
extern int nb_verif;
#endif /* FLG_DEBUG || FLG_CONTROLE_IMMEDIAT */

extern T_discord *une_verif _PROTS((T_irdata *irdata, struct S_discord *(*proc)(T_irdata *irdata)));
#endif

