#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <utils.h>
#include <mem.h>
#include <liste.h>

static SL_void nilVal = {&nilVal, NULL};
L_void nilPtr = &nilVal;

static L_void allocCons(T_tas tas) {
  L_void l = NULL;

  l = (L_void)memAlloue(tas, sizeof (SL_void));
  if (l == NULL) {
    return NULL;
  }
  l->tete = NULL;
  l->queue = nilPtr;
  return l;
}

void libereCons(L_void l) {
  if (l == NULL || l == nilPtr) {
    return;
  }
  memLibere(l);
}

L_void nil(void) {
  return nilPtr;
}

L_void cons(T_tas tas, void *e, L_void l) {
  L_void res = NULL;

  assert(l != NULL);
  res = allocCons(tas);
  res->tete = e;
  res->queue = l;
  return res;
}

void libereListe(L_void l) {
  assert(l != NULL);
  while (l != nilPtr) {
    L_void ll = l->queue;

    libereCons(l);
    l = ll;
  }
}

long longueur(L_void l) {
  long lng = 0;

  assert(l != NULL);
  while (l != nilPtr) {
    lng++;
    l = l->queue;
  }
  return lng;
}

void **versTableau(T_tas tas, L_void l) {
  long lng = 0;
  void **res = NULL;
  int i = 0;

  assert(l != NULL);
  if (l == nilPtr) {
    return NULL;
  }
  lng = longueur(l);
  res = (void **)memAlloue(tas, lng * (sizeof (void *)));
  for (i = 0; i < lng; i++) {
    res[i] = l->tete;
    l = l->queue;
  }
  return res;
}

void concat(L_void *res, L_void l0, L_void l1) {
  L_void l = l0;

  assert(l0 != NULL);
  assert(l1 != NULL);
  if (l0 == nilPtr) {
    *res = l1;
    return;
  }
  while (l->queue != nilPtr) {
    l = l->queue;
  }
  l->queue = l1;
  *res = l0;
}

void retourne(L_void *res, L_void l) {
  L_void lp = nilPtr;

  assert(l != NULL);
  while (l != nilPtr) {
    L_void queue = l->queue;

    l->queue = lp;
    lp = l;
    l = queue;
  }
  *res = lp;
}

L_void copie(T_tas tas, L_void l) {
  L_void res = nilPtr;

  assert(l != NULL);
  while (l != nilPtr) {
    res = cons(tas, l->tete, res);
    l = l->queue;
  }
  retourne(&res, res);
  return res;
}

void triSurPlace(L_void *res, L_void l, int cmp(void *, void *)) {
  assert(l != NULL);
  if (l == nilPtr) {
    *res = l;
    return;
  } else {
    long lng = 1;

    while (1) {
      L_void *lp = &l;
      L_void l0 = l;

      while (l0 != nilPtr) {
        long k0 = 0;
        long k1 = 0;
        L_void l1 = l0;

        while (k0 < lng && l1 != nilPtr) {
          l1 = l1->queue;
          k0++;
        }
        if (l0 == l && l1 == nilPtr) {
          *res = l;
          return;
        }
        k0 = 0;
        while (k0 < lng && k1 < lng && l1 != nilPtr) {
          if (cmp(l0->tete, l1->tete)) {
            *lp = l0;
            lp = &(l0->queue);
            l0 = l0->queue;
            k0++;
          } else {
            *lp = l1;
            lp = &(l1->queue);
            l1 = l1->queue;
            k1++;
          }
        }
        while (k0 < lng && l0 != nilPtr) {
          *lp = l0;
          lp = &(l0->queue);
          l0 = l0->queue;
          k0++;
        }
        while (k1 < lng && l1 != nilPtr) {
          *lp = l1;
          lp = &(l1->queue);
          l1 = l1->queue;
          k1++;
        }
        *lp = l1;
        l0 = l1;
      }
      lng *= 2;
    }
  }
}

void elimDoublons(L_void l, int egal(void *, void *)) {
  L_void lc = NULL;
  L_void ll = NULL;

  lc = l;
  while (lc != nilPtr) {
    ll = lc;
    while (ll != nilPtr && egal(lc->tete, ll->tete)) {
      ll = ll->queue;
    }
    lc->queue = ll;
    lc = ll;
  }
}

