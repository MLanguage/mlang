#ifndef __UTILS_H__
#define __UTILS_H__

#include <mem.h>

#define VRAI 1
#define FAUX 0

typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;

extern const char *enteteDgfip(char couleur);
double arrondi(double d);
extern size_t strLng(char *str);
extern char *strCopie(T_tas tas, char *str);
extern int anneeCourante(void);
extern void ignore(int i);
extern int cmpChar(char *s0, char *s1);
extern int egalChar(char *s0, char *s1);
extern int strVersUint(char *s, int *res);
extern int strVersNum(char *s, double *res);
extern char *strApresDernier(char c, char *s);

#endif /* __UTILS_H__ */
