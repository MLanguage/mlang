#ifndef __TRAITEMENT_H__
#define __TRAITEMENT_H__

#include <stdint.h>
#include <mem.h>
#include <options.h>

extern L_char erreursVersListe(T_tas tas, T_irdata *tgv);
extern int traitementAux(T_tas tasTrt, char *chemin, T_options opts, T_irdata *tgv);
extern int traitement(char *chemin, T_options opts);

#endif /* __TRAITEMENT_H__ */
