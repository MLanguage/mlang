#ifndef __FICHIERS_H__
#define __FICHIERS_H__

#include <stdlib.h>
#include <stdio.h>
#include <dirent.h>
#include <mem.h>
#include <liste.h>

#define LNG_BUF 1024
#define ERR_FICH '\xFF'

typedef struct S_fich * T_fich;
typedef struct S_fich {
  FILE *file;
  int lin;
  int col;
  size_t pos;
  size_t lng;
  char buf[1024];
} S_fich;

extern int estRep(char *chemin);
extern int estReg(char *chemin);
extern int estLien(char *chemin);
extern L_char contenuRep(T_tas tas, char *chemin);
extern L_char contenuRepPrefix(T_tas tas, char *chemin);
extern T_fich ouvreFich(T_tas tas, char *chemin);
extern void fermeFich(T_fich fich);
extern char lisFich(T_fich fich);
extern char incFich(T_fich fich);

#endif /* __FICHIERS_H__ */
