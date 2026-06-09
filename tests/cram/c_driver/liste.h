#ifndef __LISTE_H__
#define __LISTE_H__

#include <stdlib.h>
#include <stdio.h>

#include <mem.h>

#define TYPEDEF_LISTE(type) \
  struct SL_##type; \
  typedef struct SL_##type SL_##type; \
  typedef SL_##type * L_##type; \
  struct SL_##type { \
    L_##type queue; \
    type *tete; \
  };

TYPEDEF_LISTE(void)

extern void libereBlocs(size_t taille);

extern L_void nil(void);

extern L_void cons(T_tas tas, void *e, L_void l);

extern void libereCons(L_void l);

extern void libereListe(L_void l);

extern long longueur(L_void l);

extern void **versTableau(T_tas tas, L_void l);

extern void concat(L_void *res, L_void l0, L_void l1);

extern void retourne(L_void *res, L_void l);

extern L_void copie(T_tas tas, L_void l);

extern void triSurPlace(L_void *res, L_void l, int cmp(void *, void *));

extern void elimDoublons(L_void l, int egal(void *, void *));

#define NIL(type) ((L_##type)nil())
#define CONS(tas, type, e, l) ((L_##type)cons(tas, (void *)e, (L_void)l))
#define TETE(type, l) ((type *)(l->tete))
#define QUEUE(type, l) ((L_##type)(l->queue))
#define LIBERE_CONS(l) libereCons((L_void)l)
#define LONGUEUR(type, l) (longueur((L_void)l))
#define LIBERE_LISTE(type, l) libereListe((L_void)l)
#define VERS_TABLEAU(type, l) ((type **)versTableau((L_void)l))
#define CONCAT(type, res, l0, l1) concat((L_void *)res, (L_void)l0, (L_void)l1)
#define RETOURNE(type, res, l) retourne((L_void *)res, (L_void)l)
#define COPIE(tas, type, l) ((L_##type)copie(tas, (L_void)l))
#define TRI_SUR_PLACE(type, res, l, cmp) triSurPlace((L_void *)res, (L_void)l, (int (*)(void *, void *))cmp)
#define ELIM_DOUBLONS(type, l, egal) elimDoublons((L_void)l, (int (*)(void *, void *))egal)

TYPEDEF_LISTE(char)

#endif /* __LISTE_H__ */

