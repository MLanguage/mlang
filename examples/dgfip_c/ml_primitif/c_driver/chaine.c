#include <stdlib.h>
#include <utils.h>
#include <mem.h>
#include <liste.h>
#include <chaine.h>

char *strConcat(T_tas tas, char *s0, char *s1) {
  size_t lng = 0;
  char *res = NULL;
  int i = 0;
  int j = 0;

  if (s0 == NULL && s1 == NULL) return strCopie(tas, "");
  if (s0 == NULL) return strCopie(tas, s1);
  if (s1 == NULL) return strCopie(tas, s0);
  lng = strLng(s0) + strLng(s1);
  if (lng == 0) return strCopie(tas, "");
  res = (char *)memAlloue(tas, lng + 1);
  i = 0;
  for (j = 0; s0[j] != 0; j++, i++) {
    res[i] = s0[j];
  }
  for (j = 0; s1[j] != 0; j++, i++) {
    res[i] = s1[j];
  }
  res[i] = 0;
  return res;
}

char *strConcatListe(T_tas tas, L_char lstr) {
  size_t lng = 0;
  L_char l = NULL;
  char *res = NULL;
  int i = 0;

  if (lstr == NULL) return strCopie(tas, "");
  for (l = lstr; l != NIL(char); l = QUEUE(char, l)) {
    char *str = NULL;

    str = TETE(char, l);
    if (str == NULL) return strCopie(tas, "");
    lng += strLng(str);
  }
  if (lng == 0) return strCopie(tas, "");
  res = (char *)memAlloue(tas, lng + 1);
  for (l = lstr; l != NIL(char); l = QUEUE(char, l)) {
    char *str = NULL;
    int j = 0;

    str = TETE(char, l);
    for (j = 0; str[j] != 0; j++, i++) {
      res[i] = str[j];
    }
  }
  res[i] = 0;
  return res;
}

