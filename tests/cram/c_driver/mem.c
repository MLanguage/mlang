#include <stdlib.h>
#include <stdio.h>

struct S_mem;
typedef struct S_mem S_mem;
typedef S_mem *T_mem;

struct S_tas;
typedef struct S_tas S_tas;
typedef S_tas *T_tas;

struct S_mem {
  char occ;
  size_t taille;
  T_tas tas;
  T_mem prec;
  T_mem suiv;
};

struct S_tas {
  T_mem mem;
  size_t taille;
  long nb;
  T_tas prec;
  T_tas suiv;
};

#define TAILLE_MEM (sizeof (S_mem))

static T_tas sauvTas = NULL;

T_tas memCreeTas(void) {
  T_tas tas = NULL;

  tas = (T_tas)malloc(sizeof (S_tas));
  if (tas == NULL) {
    return NULL;
  }
  tas->mem = NULL;
  tas->taille = 0;
  tas->nb = 0;
  tas->prec = NULL;
  tas->suiv = sauvTas;
  if (sauvTas != NULL) {
    sauvTas->prec = tas;
  }
  sauvTas = tas;
  return tas;
}

void *memAlloue(T_tas tas, size_t t) {
  T_mem m = NULL;
  char *z = NULL;

  if (tas == NULL || t <= 0) {
    return NULL;
  }
  z = (char *)malloc(TAILLE_MEM + t);
  if (z == NULL) {
    return NULL;
  }
  m = (T_mem)z;
  m->occ = 2;
  m->taille = t;
  m->tas = tas;
  m->prec = NULL;
  m->suiv = tas->mem;
  if (tas->mem != NULL) {
    tas->mem->prec = m;
  }
  tas->mem = m;
  tas->taille += t;
  tas->nb++;
  return (void *)(z + TAILLE_MEM);
}

void *memRealloue(void *p, size_t t) {
  T_tas tas = NULL;
  T_mem m = NULL;
  size_t taille = 0;
  T_mem mbis = NULL;

  if (p == NULL || t == 0) {
    return NULL;
  }
  m = (T_mem)(((char *)p) - TAILLE_MEM);
  tas = m->tas;
  taille = m->taille;
  if (taille == t) {
    return p;
  }
  mbis = realloc(m, TAILLE_MEM + t);
  if (mbis == NULL) {
    return NULL;
  } 
  tas->taille = tas->taille - taille + t;
  mbis->taille = t;
  if (mbis != m) {
    mbis->occ = 2;
    if (mbis->prec != NULL) {
      mbis->prec->suiv = mbis;
    }
    if (mbis->suiv != NULL) {
      mbis->suiv->prec = mbis;
    }
    if (tas->mem == m) {
      tas->mem = mbis;
    }
  }
  return (void *)(((char *)mbis) + TAILLE_MEM);
}

void memLibere(void *p) {
  T_tas tas = NULL;
  T_mem m = NULL;

  if (p == NULL) {
    return;
  }
  m = (T_mem)(((char *)p) - TAILLE_MEM);
  tas = m->tas;
  if (m->prec != NULL) {
    m->prec->suiv = m->suiv;
  }
  if (m->suiv != NULL) {
    m->suiv->prec = m->prec;
  }
  if (m == tas->mem) {
    tas->mem = m->suiv;
  }
  m->occ = 3;
  m->prec = NULL;
  m->suiv = NULL;
  tas->taille -= m->taille;
  tas->nb--;
  free(m);
}

void memLibereTas(T_tas tas) {
  if (tas == NULL) {
    return;
  }
  while (tas->mem != NULL) {
    T_mem suiv = tas->mem->suiv;

    free(tas->mem);
    tas->mem = suiv;
  }
  if (tas->prec != NULL) {
    tas->prec->suiv = tas->suiv;
  }
  if (tas->suiv != NULL) {
    tas->suiv->prec = tas->prec;
  }
  if (tas == sauvTas) {
    sauvTas = tas->suiv;
  }
  free(tas);
}

void memLibereTout(void) {
  while (sauvTas != NULL) {
    memLibereTas(sauvTas);
  }
}

