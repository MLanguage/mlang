#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <math.h>
#include <limits.h>

#include <mem.h>
#include <utils.h>

#define E0 "\033[0m"
#define E30 "\033[30m"
#define E33 "\033[33m"
#define E94 "\033[94m"
#define E107 "\033[107m"

const char *enteteDgfipCouleurTab[] = {
  E107     E94 "  .____." E107 E30
  " .___________________________________________. " E0 "\n",
  E107 " " E94 " | " E33 "._ " E94 "'" E107 E30
  " |                                           | " E0 "\n",
  E107 " " E94 " | " E33 "| \\ " E107 E30
  " | Direction générale des Finances publiques | " E0 "\n",
  E107     E94 ".-+-+-' " E107 E30
  " |                                           | " E0 "\n",
  E107 " " E94 " | " E33 "|   " E107 E30
  " |          calculette - interface C         | " E0 "\n",
  E107 " " E94 " ! " E33 "\\   " E107 E30
  " !___________________________________________! " E0 "\n",
  E107
  "                                                       " E0 "\n",
  NULL
};

char enteteDgfipCouleur[1024] = "";

const char enteteDgfipNB[] =
"  .____. .___________________________________________.\n\
  | ._ ' |                                           |\n\
  | | \\  | Direction générale des Finances publiques |\n\
.-+-+-'  |                                           |\n\
  | |    |          calculette - interface C         |\n\
  ! \\    !___________________________________________!\n\n";

const char *enteteDgfip(char couleur) {
  if (couleur) {
    if (enteteDgfipCouleur[0] == 0) {
      int i = 0, j = 0, k = 0;

      while (enteteDgfipCouleurTab[i] != NULL) {
        j = 0;
        while (enteteDgfipCouleurTab[i][j] != 0) {
          enteteDgfipCouleur[k] = enteteDgfipCouleurTab[i][j];
          k++;
          j++;
        }
        i++;
      }
      enteteDgfipCouleur[k] = 0;
    }
    return enteteDgfipCouleur;
  }
  return enteteDgfipNB;
}

double arrondi(double d) {
  if (fabs(d) <= (double)LONG_MAX) {
    if (d < 0.0) {
      return ceil(d - 0.5);
    } else {
      return floor(d + 0.5);
    }
  } else {
    return d;
  }
}

size_t strLng(char *str) {
  size_t len = 0;

  if (str == NULL) {
    return 0;
  }
  while (str[len] != 0) len++;
  return len;
}

char *strCopie(T_tas tas, char *str) {
  size_t len = 0;
  char *res = NULL;
  int i = 0;

  if (tas == NULL || str == NULL) {
    return NULL;
  }
  len = strLng(str) + 1;
  res = (char *)memAlloue(tas, len);
  for (i = 0; i < len; i++) {
    res[i] = str[i];
  }
  return res;
}

int anneeCourante(void) {
  time_t t = time(NULL);
  struct tm tm = *localtime(&t);

  return 1900 + tm.tm_year;
}

void ignore(int i) {}

int cmpChar(char *s0, char *s1) {
  return strcmp(s0, s1) <= 0;
}

int egalChar(char *s0, char *s1) {
  return strcmp(s0, s1) == 0;
}

int strVersUint(char *s, int *res) {
  int i = 0;
  char c = 0;

  assert(res != NULL);
  *res = 0;
  if (s == NULL || s[i] == 0) {
    return 2;
  }
  for (c = s[i]; c != 0; i++, c = s[i]) {
    if (c < '0' || '9' < c) {
      return -i;
    }
    *res = 10 * *res + (c - '0');
  }
  return 1;
}

int strVersNum(char *s, double *res) {
  int i = 0;
  char c = 0;
  double sg = 1.0;

  assert(res != NULL);
  *res = 0.0;
  if (s == NULL || s[i] == 0) {
    return 2;
  }
  if (s[i] == '-') {
    sg = -1.0;
    i++;
  } else if (s[i] < '0' || '9' < s[i]) {
    return -i;
  }
  for (c = s[i]; c != 0 && c != '.'; i++, c = s[i]) {
    if (c < '0' || '9' < c) {
      return -i;
    }
    *res = 10.0 * *res + (double)(c - '0');
  }
  if (c == 0) {
    *res = sg * *res;
    return 1;
  } else if (c == '.') {
    double dec = 1.0;

    i++;
    if (s[i] == 0) {
      return -i;
    }
    for (c = s[i]; c != 0; i++, c = s[i]) {
      if (c < '0' || '9' < c) {
        return -i;
      }
      *res = 10.0 * *res + (double)(c - '0');
      dec = dec * 10.0;
    }
    *res = sg * *res / dec;
    return 1;
  } else {
    return -i;
  }
}

char *strApresDernier(char c, char *s) {
  char *res = s;

  if (s == NULL) {
    return res;
  }
  while (*s != 0) {
    res = s;
    while (*s != 0 && *s != c) {
      s++;
    }
    if (*s == c) {
      s++;
    }
  }
  return res;

}

