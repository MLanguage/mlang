#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <time.h>

#include <utils.h>
#include <mem.h>
#include <liste.h>
#include <chaine.h>
#include <fichiers.h>
#include <irj.h>
#include <commun.h>
#include <options.h>
#include <ida.h>
#include <traitement.h>
#include <completion.h>

#include <mlang.h>


char *nomSansIrj(T_tas tas, char *fich) {
  size_t lng = 0;
  char *res = NULL;
  char *ext = NULL;
  int i = 0;

  res = strCopie(tas, fich);
  ext = strApresDernier('.', res);
  if (strcmp(ext, "irj") == 0 || strcmp(ext, "IRJ") == 0) {
    for (i = strLng(res); i >= 0 && res[i] != '.'; i--);
    if (i > 0) {
      res[i] = 0;
    }
  }
  return res;
}

char *nomDestinationPrefix(T_tas tas, T_options opts, char *chemin) {
  char *nomFich = NULL;
  char *dest = NULL;
  L_char destL = NULL;

  nomFich = strApresDernier('/', chemin); /* pointeur dans chemin */
  destL = NIL(char);
  dest = opts->args.trt.dest;
  destL = CONS(tas, char, dest, destL);
  if (dest[strLng(dest) - 1] != '/') {
    destL = CONS(tas, char, strCopie(tas, "/"), destL);
  }
  destL = CONS(tas, char, nomSansIrj(tas, nomFich), destL);
  RETOURNE(char, &destL, destL);
  dest = strConcatListe(tas, destL);
  LIBERE_LISTE(char, destL);
  return dest;
}

char *nomDestination(T_tas tas, T_options opts, char *chemin) {
  char *destPre = "";
  char *dest = "";
  char buf[8 * (sizeof (unsigned int)) + 5];
  unsigned int idx = 1;

  destPre = nomDestinationPrefix(tas, opts, chemin);
  dest = strConcat(tas, destPre, ".irj");
  while (estReg(dest) == 1) {
    if (idx == 0) {
      return NULL;
    }
    memLibere(dest);
    sprintf(buf, "_%d.irj", idx);
    dest = strConcat(tas, destPre, buf);
    idx++;
  }
  return dest;
}

void extraitAnos(T_tas tas, FILE *destFile, T_irdata *tgv) {
  L_char errs = NULL;
  L_char errsSav = NULL;

  errs = erreursVersListe(tas, tgv);
  while (errs != NIL(char)) {
    fprintf(destFile, "%s\n", TETE(char, errs));
    memLibere(TETE(char, errs));
    errsSav = errs;
    errs = QUEUE(char, errs);
    LIBERE_CONS(errsSav);
  }
}

void extraitTgv(T_options opts, FILE *destFile, T_irdata *tgv) {
  int lng = 0;
  int i = 0;

  lng = NB_variable + NB_saisie;
  for (i = 0; i < lng; i++) {
    T_varinfo *info = NULL;

    info = varinfo[i].info;
    if (! opts->args.trt.strict || info->est_restituee) {
      char def = 0;
      double val = 0.0;

      lis_varinfo(tgv, ESPACE_PAR_DEFAUT, info, &def, &val);
      val = arrondi(val * 100.0) / 100.0;
      fprintf(destFile, "%s/%0.2f\n", info->name, val);
    }
  }
}

int completionAux(T_tas tas, T_options opts, char *chemin, char *dest, T_irdata *tgv) {
  T_fich fich = NULL;
  T_irj irj = NULL;
  int code = IRJ_CODE_VIDE;
  L_char lnom = NULL;
  char *nom = NULL;
  int ok = 1;
  FILE *destFile = NULL;

  destFile = fopen(dest, "w");
  if (destFile == NULL) {
    discoFichier(dest, -1);
    ok = -1;
    goto fin;
  }
  fich = ouvreFich(tas, chemin);
  if (fich == NULL) {
    discoFichier(chemin, -1);
    ok = -1;
    goto fin;
  }
  irj = creeIrj(tas, opts->args.trt.strict);
  code = codeIrj(irj);
  lnom = NIL(char);
  while (code != IRJ_FIN && code != IRJ_INVALIDE) {
    lisIrj(fich, irj);
    code = codeIrj(irj);
    switch (code) {
      case IRJ_NOM:
        lnom = CONS(tas, char, strCopie(tas, irj->args.nom), lnom);
        break;
      case IRJ_NOM_FIN:
        RETOURNE(char, &lnom, lnom);
        nom = strConcatListe(tas, lnom);
        fprintf(destFile, "#NOM\n%s\n", nom);
        break;
      case IRJ_ENTREES_PRIMITIF_DEBUT:
        fprintf(destFile, "#ENTREES-PRIMITIF\n");
        break;
      case IRJ_CONTROLES_PRIMITIF_DEBUT:
        fprintf(destFile, "#CONTROLES-PRIMITIF\n");
        switch (opts->args.trt.mode) {
          case Primitif:
            extraitAnos(tas, destFile, tgv);
            break;
          case Correctif:
            break;
        }
        break;
      case IRJ_RESULTATS_PRIMITIF_DEBUT:
        fprintf(destFile, "#RESULTATS-PRIMITIF\n");
        switch (opts->args.trt.mode) {
          case Primitif:
            extraitTgv(opts, destFile, tgv);
            break;
          case Correctif:
            break;
        }
        break;
      case IRJ_ENTREES_CORRECTIF_DEBUT:
      case IRJ_CONTROLES_CORRECTIF_DEBUT:
      case IRJ_RESULTATS_CORRECTIF_DEBUT:
        /* ignorés */
        break;
      case IRJ_ENTREES_RAPPELS_DEBUT:
        fprintf(destFile, "#ENTREES-RAPPELS\n");
        break;
      case IRJ_CONTROLES_RAPPELS_DEBUT:
        fprintf(destFile, "#CONTROLES-RAPPELS\n");
        switch (opts->args.trt.mode) {
          case Primitif:
            break;
          case Correctif:
            extraitAnos(tas, destFile, tgv);
            break;
        }
        break;
      case IRJ_RESULTATS_RAPPELS_DEBUT:
        fprintf(destFile, "#RESULTATS-RAPPELS\n");
        switch (opts->args.trt.mode) {
          case Primitif:
            break;
          case Correctif:
            extraitTgv(opts, destFile, tgv);
            break;
        }
        break;
      case IRJ_DEF_VAR:
        switch (irj->section) {
          case IRJ_ENTREES_PRIMITIF_DEBUT: {
            double val = 0.0;

            val = arrondi(irj->args.defVar.val * 100.0) / 100.0;
            fprintf(destFile, "%s/%0.2f\n", irj->args.defVar.var, val);
            break;
          }
          case IRJ_RESULTATS_PRIMITIF_DEBUT:
          case IRJ_RESULTATS_CORRECTIF_DEBUT:
          case IRJ_RESULTATS_RAPPELS_DEBUT:
            /* */
            break;
          default:
            ok = -1;
            goto fin;
        }
        break;
      case IRJ_DEF_ANO:
        switch (irj->section) {
          case IRJ_CONTROLES_PRIMITIF_DEBUT:
          case IRJ_CONTROLES_CORRECTIF_DEBUT:
          case IRJ_CONTROLES_RAPPELS_DEBUT:
            /* */
            break;
          default:
            ok = -1;
            goto fin;
        }
        break;
      case IRJ_DEF_RAP:
        if (irj->section == IRJ_ENTREES_RAPPELS_DEBUT) {
          double val = 0.0;

          fprintf(destFile, "%0.0f/", irj->args.defRap.numero);
          fprintf(destFile, "%0.0f/", irj->args.defRap.rappel);
          fprintf(destFile, "%s/", irj->args.defRap.code);
          val = arrondi(irj->args.defRap.montant * 100.0) / 100.0;
          fprintf(destFile, "%0.2f/", irj->args.defRap.montant);
          if (irj->args.defRap.sens == 0.0) {
            fprintf(destFile, "R/");
          } else if  (irj->args.defRap.sens == 1.0) {
            fprintf(destFile, "M/");
          } else if  (irj->args.defRap.sens == 2.0) {
            fprintf(destFile, "P/");
          } else if  (irj->args.defRap.sens == 3.0) {
            fprintf(destFile, "C/");
          }
          fprintf(destFile, "%0.0f/", irj->args.defRap.penalite);
          fprintf(destFile, "%0.0f/", irj->args.defRap.base_tl);
          fprintf(destFile, "%06.0f/", irj->args.defRap.date);
          fprintf(destFile, "%0.0f\n", irj->args.defRap._2042_rect);
        } else {
          ok = -1; 
          goto fin;
        }
        break;
      case IRJ_INVALIDE:
      case IRJ_CODE_VIDE:
        ok = -1;      
        goto fin;
      default:
        break;
    }
  }
  if (code == IRJ_FIN) {
    fprintf(destFile, "##\n");
  }

fin:
  memLibere(nom);
  fermeFich(fich);
  if (destFile != NULL) {
    fclose(destFile);
    if (ok != 1) {
      remove(dest);
    }
  }
  return ok;
}

int completion(char *chemin, T_options opts) {
  T_tas tasCpl = NULL;
  int ok = 0;
  T_irdata *tgv = NULL;

  tasCpl = memCreeTas();
  tgv = cree_irdata();
  if (traitementAux(tasCpl, chemin, opts, tgv)) {
    char *dest = NULL;

    dest = nomDestination(tasCpl, opts, chemin);
    if (dest == NULL) {
      dest = nomDestinationPrefix(tasCpl, opts, chemin);
      dest = strConcat(tasCpl, dest, ".irj");
      anoLimNbFich(dest);
      goto fin;
    }
    ok = completionAux(tasCpl, opts, chemin, dest, tgv);
  }

fin:
  detruis_irdata(tgv);  
  memLibereTas(tasCpl);
  return ok;
}

