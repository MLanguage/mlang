#include <stdlib.h>
#include <mem.h>
#include <fichiers.h>
#include <liste.h>
#include <string.h>
#include <irj.h>

T_irj creeIrj(T_tas tas, int strict) {
  T_irj irj = NULL;

  irj = (T_irj)memAlloue(tas, sizeof (S_irj));
  irj->code = IRJ_CODE_VIDE;
  irj->section = IRJ_CODE_VIDE;
  irj->codePri = IRJ_CODE_VIDE;
  irj->strict = (strict != 0);
  irj->ligne = 0;
  irj->lng = LNG_IRJ_BUF;
  irj->buf = (char *)memAlloue(tas, irj->lng);
  irj->pos = 0;
  irj->tas = tas;
  return irj;
}

void nettoieIrj(T_irj irj) { 
  if (irj == NULL) return;
  switch (irj->code) {
    case IRJ_NOM:
      memLibere(irj->args.nom);
      break;
    case IRJ_DEF_VAR:
      memLibere(irj->args.defVar.var);
      break;
    case IRJ_DEF_ANO:
      memLibere(irj->args.defAno.ano);
      break;
    case IRJ_DEF_RAP:
      memLibere(irj->args.defRap.code);
      break;
  }
}

void detruisIrj(T_irj irj) {
  if (irj == NULL) return;
  memLibere(irj->buf);
  memLibere(irj);
}

void initBuf(T_irj irj) {
  irj->pos = 0;
}

void ajBuf(T_irj irj, char c) {
  if (irj->pos >= irj->lng) {
    irj->lng = irj->lng * 2;
    irj->buf = (char *)memRealloue(irj->buf, irj->lng);
  }
  irj->buf[irj->pos] = c;
  irj->pos++;
}

void rtBuf(T_irj irj) {
  if (irj->pos > 0) {
    irj->pos--;
  }
}

char lsBuf(T_irj irj) {
  if (irj->pos <= 0) {
    return 0;
  }
  return irj->buf[irj->pos - 1];
}

char *dsBuf(T_irj irj) {
  return irj->buf;
}

char *lisMot(T_fich fich, T_irj irj) {
  char c = 0;
  int cpt = 0;
  
  initBuf(irj);
  c = lisFich(fich);
  cpt++;
  if (cpt > IRJ_LNG_MAX) return NULL;
  while (c == ' ' || c == '\r') {
    incFich(fich);
    c = lisFich(fich);
    cpt++;
    if (cpt > IRJ_LNG_MAX) return NULL;
  }
  while (c != '\n' && c != 0 && c != '/') {
    ajBuf(irj, c);
    incFich(fich);
    c = lisFich(fich);
    cpt++;
    if (cpt > IRJ_LNG_MAX) return NULL;
  }
  while (lsBuf(irj) == ' ' || lsBuf(irj) == '\r') rtBuf(irj);
  ajBuf(irj, 0);
  return irj->buf;
}

void ligneSuiv(T_fich fich) {
  char c = 0;
  
  c = lisFich(fich);
  while (c != '\n' && c != 0) {
    incFich(fich);
    c = lisFich(fich);
  }
  incFich(fich);
}

char *cpBuf(T_irj irj) {
  char *res = NULL;
  int i = 0;

  res = (char *)memAlloue(irj->tas, irj->lng);
  for (i = 0; i < irj->pos; i++) {
    res[i] = irj->buf[i];
  }
  return res;
}

int estDigit(char c) {
  return '0' <= c && c <= '9';
}

int versDigit(char c) {
  return (int)(c - '0');
}

double dblBuf(T_irj irj) {
  double res = 0.0;
  double neg = 1.0;
  int i = 0;

  while (i < irj->pos && irj->buf[i] == ' ') i++;
  if (i < irj->pos && irj->buf[i] == '-') {
    neg = -1.0;
    i++;
    while (i < irj->pos && irj->buf[i] == ' ') i++;
  }
  while (i < irj->pos && estDigit(irj->buf[i])) {
    res = res * 10.0 + (double)versDigit(irj->buf[i]);
    i++;
    while (i < irj->pos && irj->buf[i] == ' ') i++;
  }
  if (i < irj->pos && irj->buf[i] == '.') {
    double den = 1.0;

    i++;
    while (i < irj->pos && irj->buf[i] == ' ') i++;
    while (i < irj->pos && estDigit(irj->buf[i])) {
      res = res * 10.0 + (double)versDigit(irj->buf[i]);
      den *= 10.0;
      i++;
      while (i < irj->pos && irj->buf[i] == ' ') i++;
    }
    res = res / den;
  }
  return neg * res;
}

double sensBuf(T_irj irj) {
  if (strlen(irj->buf) != 1) return -1.0;
  switch (irj->buf[0]) {
    case 'R':
    case 'r':
      return 0.0;
    case 'M':
    case 'm':
      return 1.0;
    case 'P':
    case 'p':
      return 2.0;
    case 'C':
    case 'c':
      return 3.0;
    default:
      return -1.0;
  }
}

int estFinLigne(T_fich fich) {
  char c = 0;

  c = lisFich(fich);
  return c == '\n' || c == 0;
}

int estFinFich(T_fich fich) {
  return lisFich(fich) == 0;
}

int finSection(T_irj irj) {
/* nom */
  if (irj->code == IRJ_NOM_DEBUT) {
    irj->code = IRJ_INVALIDE;
    irj->args.err = __LINE__;
    return 0;
  }
  if (irj->code == IRJ_NOM) {
    irj->codePri = IRJ_NOM_FIN;
    return 1;
  }

/* primitif */
  if (irj->section == IRJ_ENTREES_PRIMITIF_DEBUT) {
    if (irj->code == IRJ_ENTREES_PRIMITIF_DEBUT || irj->code == IRJ_DEF_VAR) {
      irj->codePri = IRJ_ENTREES_PRIMITIF_FIN;
      return 1;
    } else {
      irj->code = IRJ_INVALIDE;
      irj->args.err = __LINE__;
      return 0;
    }
  }

  if (irj->section == IRJ_CONTROLES_PRIMITIF_DEBUT) {
    if (irj->code == IRJ_CONTROLES_PRIMITIF_DEBUT || irj->code == IRJ_DEF_ANO) {
      irj->codePri = IRJ_CONTROLES_PRIMITIF_FIN;
      return 1;
    } else {
      irj->code = IRJ_INVALIDE;
      irj->args.err = __LINE__;
      return 0;
    }
  }

  if (irj->section == IRJ_RESULTATS_PRIMITIF_DEBUT) {
    if (irj->code == IRJ_RESULTATS_PRIMITIF_DEBUT || irj->code == IRJ_DEF_VAR) {
      irj->codePri = IRJ_RESULTATS_PRIMITIF_FIN;
      return 1;
    } else {
      irj->code = IRJ_INVALIDE;
      irj->args.err = __LINE__;
      return 0;
    }
  }

/* correctif */
  if (irj->section == IRJ_ENTREES_CORRECTIF_DEBUT) {
    if (irj->code == IRJ_ENTREES_CORRECTIF_DEBUT || irj->code == IRJ_DEF_CORR) {
      irj->codePri = IRJ_ENTREES_CORRECTIF_FIN;
      return 1;
    } else {
      irj->code = IRJ_INVALIDE;
      irj->args.err = __LINE__;
      return 0;
    }
  }

  if (irj->section == IRJ_CONTROLES_CORRECTIF_DEBUT) {
    if (irj->code == IRJ_CONTROLES_CORRECTIF_DEBUT || irj->code == IRJ_DEF_ANO) {
      irj->codePri = IRJ_CONTROLES_CORRECTIF_FIN;
      return 1;
    } else {
      irj->code = IRJ_INVALIDE;
      irj->args.err = __LINE__;
      return 0;
    }
  }

  if (irj->section == IRJ_RESULTATS_CORRECTIF_DEBUT) {
    if (irj->code == IRJ_RESULTATS_CORRECTIF_DEBUT || irj->code == IRJ_DEF_VAR) {
      irj->codePri = IRJ_RESULTATS_CORRECTIF_FIN;
      return 1;
    } else {
      irj->code = IRJ_INVALIDE;
      irj->args.err = __LINE__;
      return 0;
    }
  }

/* rappels */
  if (irj->section == IRJ_ENTREES_RAPPELS_DEBUT) {
    if (irj->code == IRJ_ENTREES_RAPPELS_DEBUT || irj->code == IRJ_DEF_RAP) {
      irj->codePri = IRJ_ENTREES_RAPPELS_FIN;
      return 1;
    } else {
      irj->code = IRJ_INVALIDE;
      irj->args.err = __LINE__;
      return 0;
    }
  }

  if (irj->section == IRJ_CONTROLES_RAPPELS_DEBUT) {
    if (irj->code == IRJ_CONTROLES_RAPPELS_DEBUT || irj->code == IRJ_DEF_ANO) {
      irj->codePri = IRJ_CONTROLES_RAPPELS_FIN;
      return 1;
    } else {
      irj->code = IRJ_INVALIDE;
      irj->args.err = __LINE__;
      return 0;
    }
  }

  if (irj->section == IRJ_RESULTATS_RAPPELS_DEBUT) {
    if (irj->code == IRJ_RESULTATS_RAPPELS_DEBUT || irj->code == IRJ_DEF_VAR) {
      irj->codePri = IRJ_RESULTATS_RAPPELS_FIN;
      return 1;
    } else {
      irj->code = IRJ_INVALIDE;
      irj->args.err = __LINE__;
      return 0;
    }
  }

/* défaut */
  irj->code = IRJ_INVALIDE;
  irj->args.err = __LINE__;
  return 0;  
}

void lisIrj(T_fich fich, T_irj irj) {
  char *mot = NULL;
  
  if (irj == NULL) return;
  if (irj->code == IRJ_INVALIDE) return;
  nettoieIrj(irj);
  if (fich == NULL) {
    irj->code = IRJ_INVALIDE;
    irj->args.err = __LINE__;
    return;
  }
  if (irj->codePri != IRJ_CODE_VIDE) {
    irj->codePri = IRJ_CODE_VIDE;
    return;
  }
  irj->codePri = IRJ_CODE_VIDE;
  irj->ligne = fich->lin;
  mot = lisMot(fich, irj);
  if (mot == NULL) {
    irj->code = IRJ_INVALIDE;
    irj->args.err = __LINE__;
    return;
  }
  while (strcmp(mot, "") == 0 && estFinLigne(fich) && ! estFinFich(fich)) {
    ligneSuiv(fich);
    irj->ligne = fich->lin;
    mot = lisMot(fich, irj);
    if (mot == NULL) {
      irj->code = IRJ_INVALIDE;
      irj->args.err = __LINE__;
      return;
    }
  }
  if (estFinFich(fich)) {
    if (irj->strict) {
      irj->code = IRJ_INVALIDE;
      irj->args.err = __LINE__;
    } else {
      if (! finSection(irj)) return;      
      irj->code = IRJ_FIN;
      irj->section = IRJ_CODE_VIDE;
    }
    return;
  }
  if (estFinLigne(fich)) {
    if (strcmp(mot, "#NOM") == 0) {
      irj->code = IRJ_NOM_DEBUT;
      irj->section = IRJ_NOM_DEBUT;
      ligneSuiv(fich);
      return;
    }
    if (strcmp(mot, "#ENTREES-PRIMITIF") == 0) {
      if (! finSection(irj)) return;
      irj->code = IRJ_ENTREES_PRIMITIF_DEBUT;
      irj->section = IRJ_ENTREES_PRIMITIF_DEBUT;
      ligneSuiv(fich);
      return;
    }
    if (
      strcmp(mot, "#CONTROLES-PRIMITIF") == 0
      || (! irj->strict && strcmp(mot, "CONTROLES-PRIMITIF") == 0)
    ) {
      if (! finSection(irj)) return;
      irj->code = IRJ_CONTROLES_PRIMITIF_DEBUT;
      irj->section = IRJ_CONTROLES_PRIMITIF_DEBUT;
      ligneSuiv(fich);
      return;
    }
    if (strcmp(mot, "CONTROLES-PRIMITIF") == 0) {
      if (irj->strict) {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
      } else {
        if (! finSection(irj)) return;
        irj->code = IRJ_CONTROLES_PRIMITIF_DEBUT;
        irj->section = IRJ_CONTROLES_PRIMITIF_DEBUT;
        ligneSuiv(fich);
        return;
      }
    }
    if (strcmp(mot, "#RESULTATS-PRIMITIF") == 0) {
      if (! finSection(irj)) return;
      irj->code = IRJ_RESULTATS_PRIMITIF_DEBUT;
      irj->section = IRJ_RESULTATS_PRIMITIF_DEBUT;
      ligneSuiv(fich);
      return;
    }
    if (strcmp(mot, "#ENTREES-CORRECTIF") == 0) {
      if (! finSection(irj)) return;
      irj->code = IRJ_ENTREES_CORRECTIF_DEBUT;
      irj->section = IRJ_ENTREES_CORRECTIF_DEBUT;
      ligneSuiv(fich);
      return;
    }
    if (strcmp(mot, "#CONTROLES-CORRECTIF") == 0) {
      if (! finSection(irj)) return;
      irj->code = IRJ_CONTROLES_CORRECTIF_DEBUT;
      irj->section = IRJ_CONTROLES_CORRECTIF_DEBUT;
      ligneSuiv(fich);
      return;
    }
    if (strcmp(mot, "#RESULTATS-CORRECTIF") == 0) {
      if (! finSection(irj)) return;
      irj->code = IRJ_RESULTATS_CORRECTIF_DEBUT;
      irj->section = IRJ_RESULTATS_CORRECTIF_DEBUT;
      ligneSuiv(fich);
      return;
    }
    if (strcmp(mot, "#ENTREES-RAPPELS") == 0) {
      if (! finSection(irj)) return;
      irj->code = IRJ_ENTREES_RAPPELS_DEBUT;
      irj->section = IRJ_ENTREES_RAPPELS_DEBUT;
      ligneSuiv(fich);
      return;
    }
    if (strcmp(mot, "#CONTROLES-RAPPELS") == 0) {
      if (! finSection(irj)) return;
      irj->code = IRJ_CONTROLES_RAPPELS_DEBUT;
      irj->section = IRJ_CONTROLES_RAPPELS_DEBUT;
      ligneSuiv(fich);
      return;
    }
    if (strcmp(mot, "#RESULTATS-RAPPELS") == 0) {
      if (! finSection(irj)) return;
      irj->code = IRJ_RESULTATS_RAPPELS_DEBUT;
      irj->section = IRJ_RESULTATS_RAPPELS_DEBUT;
      ligneSuiv(fich);
      return;
    }
    if (strcmp(mot, "##") == 0) {
      if (! finSection(irj)) return;
      irj->code = IRJ_FIN;
      irj->section = IRJ_CODE_VIDE;
      return;
    }
  }
  switch (irj->section) {
    case IRJ_NOM_DEBUT:
      if (! irj->strict || irj->code == IRJ_NOM_DEBUT) {
        irj->code = IRJ_NOM;
        irj->args.nom = cpBuf(irj);
        ligneSuiv(fich);
      } else {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
      }
      return;
    case IRJ_ENTREES_PRIMITIF_DEBUT:
    case IRJ_RESULTATS_PRIMITIF_DEBUT:
    case IRJ_RESULTATS_CORRECTIF_DEBUT:
    case IRJ_RESULTATS_RAPPELS_DEBUT:
      if (lisFich(fich) == '/' || (! irj->strict && estFinLigne(fich))) {
        irj->code = IRJ_DEF_VAR;
        irj->args.defVar.var = cpBuf(irj);
        if (lisFich(fich) == '/') {
          incFich(fich);
          mot = lisMot(fich, irj);
          if (mot == NULL) {
            irj->code = IRJ_INVALIDE;
            irj->args.err = __LINE__;
            return;
          }
          irj->args.defVar.val = dblBuf(irj);
        } else {
          irj->args.defVar.val = 0.0;
        }
        ligneSuiv(fich);
      } else {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
      }
      return;
    case IRJ_CONTROLES_PRIMITIF_DEBUT:
    case IRJ_CONTROLES_CORRECTIF_DEBUT:
    case IRJ_CONTROLES_RAPPELS_DEBUT:
      if (estFinLigne(fich)) {
        irj->code = IRJ_DEF_ANO;
        irj->args.defAno.ano = cpBuf(irj);
        ligneSuiv(fich);
      } else {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
      }
      return;
    case IRJ_ENTREES_RAPPELS_DEBUT:
      irj->code = IRJ_DEF_RAP;
      irj->args.defRap.numero = dblBuf(irj);
      if (lisFich(fich) != '/') {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      incFich(fich);
      mot = lisMot(fich, irj);
      if (mot == NULL) {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      irj->args.defRap.rappel = dblBuf(irj);
      if (lisFich(fich) != '/') {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      incFich(fich);
      mot = lisMot(fich, irj);
      if (mot == NULL) {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      if (strcmp(mot, "") == 0) {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      irj->args.defRap.code = cpBuf(irj);
      if (lisFich(fich) != '/') {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      incFich(fich);
      mot = lisMot(fich, irj);
      if (mot == NULL) {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      irj->args.defRap.montant = dblBuf(irj);
      if (lisFich(fich) != '/') {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      incFich(fich);
      mot = lisMot(fich, irj);
      if (mot == NULL) {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      irj->args.defRap.sens = sensBuf(irj);
      if (irj->args.defRap.sens == -1.0) {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      if (lisFich(fich) != '/') {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      incFich(fich);
      mot = lisMot(fich, irj);
      if (mot == NULL) {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      irj->args.defRap.penalite = dblBuf(irj);
      if (lisFich(fich) != '/') {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      incFich(fich);
      mot = lisMot(fich, irj);
      if (mot == NULL) {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      irj->args.defRap.base_tl = dblBuf(irj);
      if (lisFich(fich) != '/') {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      incFich(fich);
      mot = lisMot(fich, irj);
      if (mot == NULL) {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      irj->args.defRap.date = dblBuf(irj);
      if (lisFich(fich) != '/')  {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      incFich(fich);
      mot = lisMot(fich, irj);
      if (mot == NULL) {
        irj->code = IRJ_INVALIDE;
        irj->args.err = __LINE__;
        return;
      }
      irj->args.defRap._2042_rect = dblBuf(irj);
      ligneSuiv(fich);
      return;
    case IRJ_ENTREES_CORRECTIF_DEBUT:
      irj->code = IRJ_DEF_CORR;
      /* vieux format de rappel, ignoré */
      ligneSuiv(fich);
      return;
    default:
      irj->code = IRJ_INVALIDE;
      irj->args.err = __LINE__;
      return;
  }
  irj->code = IRJ_INVALIDE;
  irj->args.err = __LINE__;
  return;
}

int codeIrj(T_irj irj) {
  if (irj == NULL) return IRJ_CODE_VIDE;
  if (irj->codePri == IRJ_CODE_VIDE) return irj->code;
  return irj->codePri;  
}

char *codeIrjVersStr(int code) {
  switch (code) {
    case IRJ_DEBUT: return "début";
    case IRJ_NOM_DEBUT: return "début nom";
    case IRJ_NOM: return "nom";
    case IRJ_NOM_FIN: return "fin nom";
    case IRJ_ENTREES_PRIMITIF_DEBUT: return "début entrées primitif";
    case IRJ_ENTREES_PRIMITIF_FIN: return "fin entrées primitif";
    case IRJ_CONTROLES_PRIMITIF_DEBUT: return "début contrôles primitif";
    case IRJ_CONTROLES_PRIMITIF_FIN: return "fin contrôles primitif";
    case IRJ_RESULTATS_PRIMITIF_DEBUT: return "début résultats primitif";
    case IRJ_RESULTATS_PRIMITIF_FIN: return "fin résultats primitif";
    case IRJ_ENTREES_CORRECTIF_DEBUT: return "début entrées correctif";
    case IRJ_ENTREES_CORRECTIF_FIN: return "fin entrées correctif";
    case IRJ_CONTROLES_CORRECTIF_DEBUT: return "début contrôles correctif";
    case IRJ_CONTROLES_CORRECTIF_FIN: return "fin contrôles correctif";
    case IRJ_RESULTATS_CORRECTIF_DEBUT: return "début résultats correctif";
    case IRJ_RESULTATS_CORRECTIF_FIN: return "fin résultats correctif";
    case IRJ_ENTREES_RAPPELS_DEBUT: return "début entrées rappels";
    case IRJ_ENTREES_RAPPELS_FIN: return "fin entrées rappels";
    case IRJ_CONTROLES_RAPPELS_DEBUT: return "début contrôles rappels";
    case IRJ_CONTROLES_RAPPELS_FIN: return "fin contrôles rappels";
    case IRJ_RESULTATS_RAPPELS_DEBUT: return "début résultats rappels";
    case IRJ_RESULTATS_RAPPELS_FIN: return "fin résultats rappels";
    case IRJ_IGNORE: return "ignoré";
    case IRJ_DEF_VAR: return "définition de variable";
    case IRJ_DEF_ANO: return "définition d'anomalie";
    case IRJ_DEF_RAP: return "définition de rappel";
    case IRJ_DEF_CORR: return "définition de rappel correctif";
    case IRJ_FIN: return "fin";
    case IRJ_INVALIDE: return "invalide";
    case IRJ_CODE_VIDE: return "code vide";
    default: return "code invalide";
  }
}

