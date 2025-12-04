#include <stdlib.h>
#include <stdio.h>

#include <commun.h>
#include <ida.h>

/* infos */

void infoActVide(void) {
  fprintf(stdout, "IACT000 | aide par défaut\n");
}

void infoActAide(void) {
  fprintf(stdout, "IACT001 | aide de l'application\n");
}

void infoActFmt(void) {
  fprintf(stdout, "IACT002 | vérification du format des fichiers IRJ\n");
}

void infoActTrt(void) {
  fprintf(stdout, "IACT003 | tests IRJ\n");
}

void infoRep(char *nom) {
  fprintf(stdout, "IACT004 | parcours du repertoire \"%s\"\n", nom);
}

void infoReg(char *nom) {
  fprintf(stdout, "IACT005 | traitement du fichier \"%s\"\n", nom);
}

void infoOk(char *fich) {
  fprintf(stdout, "IACT006 | \"%s\" OK\n", fich);
}

void infoKo(char *fich) {
  fprintf(stdout, "IACT007 | \"%s\" KO\n", fich);
}

void infoKc(char *fich) {
  fprintf(stdout, "IACT008 | \"%s\" invalide\n", fich);
}

void infoNbOk(int ok, int tot) {
  fprintf(stdout, "IACT009 | %d/%d fichiers corrects\n", ok, tot);
}

void infoNbKo(int ko, int tot) {
  fprintf(stdout, "IACT010 | %d/%d fichiers incorrects\n", ko, tot);
}

void infoNbKc(int kc, int tot) {
  fprintf(stdout, "IACT011 | %d/%d fichiers invalides\n", kc, tot);
}

void infoNonRec(char *dir) {
  fprintf(stdout, "IACT012 | le répertoire \"%s\" est ignoré\n", dir);
}

void infoLien(char *nom) {
  fprintf(stdout, "IACT013 | le lien \"%s\" est ignoré\n", nom);
}

/* discos */

int discoOptsRecDup(int b) {
  if (b) {
    fprintf(stdout, "DLDC000 | récursivité spécifiée plusieurs fois\n");
    return 1;
  }
  return 0;
}

int discoOptsStrictDup(int b) {
  if (b) {
    fprintf(stdout, "DLDC001 | rigueur (strict) spécifiée plusieurs fois\n");
    return 1;
  }
  return 0;
}

int discoOptsAnneeParDefaut(int annee, int b) {
  if (b) {
    fprintf(stdout, "DLDC002 | année par défaut (année revenu + 1: %d)\n", annee);
    return 1;
  }
  return 0;
}

int discoOptsModeDup(int b) {
  if (b) {
    fprintf(stdout, "DLDC003 | mode spécifié plusieurs fois\n");
    return 1;
  }
  return 0;
}

int discoOptsAnneeDup(int b) {
  if (b) {
    fprintf(stdout, "DLDC004 | année spécifiée plusieurs fois\n");
    return 1;
  }
  return 0;
}

int discoRep(char *nom, L_char l) {
  if (l == NULL) {
    fprintf(stdout, "DACT001 | repertoire \"%s\" inaccessible\n", nom);
    return 1;
  }
  return 0;
}

int discoStat(char *nom, int b) {
  if (b == -1) {
    fprintf(stdout, "DACT002 | fichier \"%s\" inaccessible\n", nom);
    return 1;
  }
  return 0;
}

int discoFichier(char *nom, int b) {
  if (b == -1) {
    fprintf(stdout, "DACT003 | fichier \"%s\" illisible\n", nom);
    return 1;
  }
  return 0;
}

int discoAnneeRevenu(int anneeCalc, int anneeRevenu) {
  if (anneeCalc != anneeRevenu) {
    fprintf(stdout, "DTRT000 | année calculette (%d) <> année revenu (%d)\n", anneeCalc, anneeRevenu);
    return 1;
  }
  return 0;
}

/* anos */

void anoOptsExe(void) {
  fprintf(stdout, "ALDC000 | nom de l'exécutable inconnu\n");
}

void anoOptsTrop(char *arg) {
  fprintf(stdout, "ALDC001 | trop de paramètres : %s ...\n", arg);
}

void anoOptsAction(char *act) {
  fprintf(stdout, "ALDC002 | action inconnue : %s\n", act);
}

void anoOptsCat(char *cat) {
  fprintf(stdout, "ALDC003 | catégorie d'aide inconnue : %s\n", cat);
}

void anoOptsInc(char *arg) {
  fprintf(stdout, "ALDC004 | paramètre inconnu  : %s\n", arg);
}

void anoOptsModeAbs(void) {
  fprintf(stdout, "ALDC005 | mode manquant (-mode ? | -m ?)\n");
}

void anoOptsModeArg(char *arg) {
  fprintf(stdout, "ALDC006 | mode inconnu  : %s\n", arg);
}

void anoOptsModeAbsent(void) {
  fprintf(stdout, "ALDC007 | mode non spécifié\n");
}

void anoOptsModeDup(T_mode mode0, T_mode mode1) {
  char *m0 = (mode0 == Primitif ? "primitif" : "correctif");
  char *m1 = (mode1 == Primitif ? "primitif" : "correctif");
  fprintf(stdout, "ALDC008 | modes surnuméraires contradictoires (%s <> %s)\n", m0, m1);
}

void anoOptsAnneeAbs(void) {
  fprintf(stdout, "ALDC009 | année manquante (-annee ? | -a ?)\n");
}

void anoOptsAnneeArg(char *arg) {
  fprintf(stdout, "ALDC010 | année invalide : %s\n", arg);
}

void anoOptsAnneeDup(int annee0, int annee1) {
  fprintf(stdout, "ALDC011 | années surnuméraires contradictoires (%d <> %d)\n", annee0, annee1);
}

void anoOptsDefValArg(char *arg) {
  fprintf(stdout, "ALDC012 | nombre invalide : %s\n", arg);
}

void anoOptsDefVarArg(char *arg) {
  fprintf(stdout, "ALDC013 | variable indefinissable : %s\n", arg);
}

void anoLigneInvalide(int ligne, int err) {
  fprintf(stdout, "AFMT000 | ligne %d invalide (%d)\n", ligne, err);
}

void anoCodeVide(int ligne) {
  fprintf(stdout, "AFMT001 | ligne %d : code vide\n", ligne);
}

void anoVarAbs(char *var) {
  fprintf(stdout, "AFMT002 | variable inconnue : %s\n", var);
}

void anoVarNonRestituee(char *var) {
  fprintf(stdout, "AFMT003 | variable testee non-restituee : %s\n", var);
}

void anoErrEnTrop(char *err) {
  fprintf(stdout, "ATRT000 | KO | %s recu en trop\n", err);
}

void anoErrNonRecue(char *err) {
  fprintf(stdout, "ATRT001 | KO | %s attendue non recue\n", err);
}

void anoValeurFausse(char *nom, double val, double valRes) {
  fprintf(stdout, "ATRT002 | KO | %s = %f au lieu de %f\n", nom, val, valRes);
}

