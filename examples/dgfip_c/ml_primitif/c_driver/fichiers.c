#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h> /* open */
#include <unistd.h> /* clode */
#include <sys/stat.h> /* fstat */
#include <errno.h>
#include <string.h>
#include <mem.h>
#include <fichiers.h>

int estRep(char *chemin) {
  struct stat st = {0};

  if (chemin == NULL) {
    errno = EINVAL;
    return -1;
  }
  if (lstat(chemin, &st) == -1) {
    return -1;
  }
  return S_ISDIR(st.st_mode) & ! S_ISLNK(st.st_mode);
}

int estReg(char *chemin) {
  struct stat st = {0};

  if (chemin == NULL) {
    errno = EINVAL;
    return -1;
  }
  if (lstat(chemin, &st) == -1) {
    return -1;
  }
  return S_ISREG(st.st_mode) & ! S_ISLNK(st.st_mode);
}

int estLien(char *chemin) {
  struct stat st = {0};

  if (chemin == NULL) {
    errno = EINVAL;
    return -1;
  }
  if (lstat(chemin, &st) == -1) {
    return -1;
  }
  return S_ISLNK(st.st_mode);
}

L_char contenuRep(T_tas tas, char *chemin) {
  DIR* dir = NULL;
  L_char res = NULL;
  struct dirent *dirent = NULL;
  char *nom = NULL;

  if (chemin == NULL || strcmp(chemin, "") == 0) {
    errno = EINVAL;
    return NULL;
  }

  dir = opendir(chemin);
  if (dir == NULL) {
    return NULL;
  }

  res = NIL(char);
  while ((dirent = readdir(dir)) != NULL) {
    if (strcmp(dirent->d_name, ".") != 0 && strcmp(dirent->d_name, "..") != 0) {
      nom = (char *)memAlloue(tas, strlen(dirent->d_name) + 1);
      strcpy(nom, dirent->d_name);
      res = CONS(tas, char, nom, res);
    }
  }     

  closedir(dir);
  return res;
}

L_char contenuRepPrefix(T_tas tas, char *chemin) {
  DIR* dir = NULL;
  L_char res = NULL;
  struct dirent *dirent = NULL;
  char *nom = NULL;
  int lenChemin = 0;
  int contientSep = 0;

  if (chemin == NULL || strcmp(chemin, "") == 0) {
    errno = EINVAL;
    return NULL;
  }

  dir = opendir(chemin);
  if (dir == NULL) {
    return NULL;
  }

  lenChemin = strlen(chemin);
  contientSep = (chemin[lenChemin - 1] == '/');
  res = NIL(char);
  while ((dirent = readdir(dir)) != NULL) {
    if (strcmp(dirent->d_name, ".") != 0 && strcmp(dirent->d_name, "..") != 0) {
      int len = lenChemin + (contientSep ? 0 : 1) + strlen(dirent->d_name);
      nom = (char *)memAlloue(tas, len + 1);
      nom[0] = 0;
      strcat(nom, chemin);
      if (! contientSep) {
        strcat(nom, "/");
      }
      strcat(nom, dirent->d_name);
      res = CONS(tas, char, nom, res);
    }
  }

  closedir(dir);
  return res;
}

extern T_fich ouvreFich(T_tas tas, char *chemin) {
  T_fich fich = NULL;
  FILE *file;

  if (chemin == NULL || strcmp(chemin, "") == 0) {
    errno = EINVAL;
    return NULL;
  }
  file = fopen(chemin, "r");
  if (file == NULL) {
    return NULL;
  }
  fich = (T_fich)memAlloue(tas, sizeof (S_fich));
  fich->file = file;
  fich->lin = 1; 
  fich->col = 1;
  fich->pos = 0;
  fich->lng = 0;
  return fich;
}

void fermeFich(T_fich fich) {
  if (fich != NULL) {
    fclose(fich->file);
    memLibere(fich);
  }
}

int remplisBuf(T_fich fich) {
  if (fich == NULL) {
    return 0;
  }
  if (fich->pos < fich->lng) {
    return 1;
  }
  fich->lng = fread(&(fich->buf), 1, LNG_BUF, fich->file);
  fich->pos = 0;
  if (fich->lng == 0 && ferror(fich->file)) {
    return 0;
  } else {
    return 1;
  }
}

char lisFich(T_fich fich) {
  char c = 0;
  
  if (! remplisBuf(fich)) return ERR_FICH;
  if (fich->pos == fich->lng) return 0;
  c = fich->buf[fich->pos];
  if (c == 0) return ERR_FICH;
  return c;
}

char incFich(T_fich fich) {
  char c = 0;
  
  c = lisFich(fich);
  if (c == 0 || c == ERR_FICH) return c;
  fich->pos++;
  if (c == '\n') {
    fich->lin++;
    fich->col = 1;
  } else {
    fich->col++;
  }
  return c;
}

