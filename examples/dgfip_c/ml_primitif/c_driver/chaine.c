#include <stdlib.h>
#include <utils.h>
#include <mem.h>
#include <liste.h>
#include <chaine.h>

char *strConcatListe(T_tas tas, L_char lstr) {
  size_t lng = 0;
  L_char l = NULL;
  char *res = NULL;
  int i = 0;

  if (lstr == NULL) return "";
  for (l = lstr; l != NIL(char); l = QUEUE(char, l)) {
    char *str = NULL;

    str = TETE(char, l);
    if (str == NULL) return "";
    lng += strLng(str);
  }
  if (lng == 0) return "";
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
