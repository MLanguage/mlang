#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <utils.h>
#include <mem.h>
#include <liste.h>
#include <chaine.h>
#include <fichiers.h>
#include <irj.h>
#include <options.h>
#include <ida.h>
#include <aide.h>
#include <format.h>
#include <traitement.h>

T_tas tasGbl = NULL;

void itereFichiers(
  L_char lf, int rec,
  int (*traiteFich)(char *, T_options), T_options opts,
  int *nbOk, int *nbKo, int *nbKc
) {
  *nbOk = 0;
  *nbKo = 0;
  *nbKc = 0;
  if (lf == NULL || opts == NULL) return;
  while (lf != NIL(char)) {
    L_char lfSav = NULL;
    char *fich = NULL;
    char *nomFich = NULL;
    int estl = 0, estd = 0, estr = 0;

    fich = TETE(char, lf);
    nomFich = strApresDernier('/', fich); /* pointeur dans fich */
    estl = estLien(fich);
    if (! discoStat(nomFich, estl) && estl) {
      infoLien(nomFich);      
      lf = QUEUE(char, lf);
      memLibere(fich);
      LIBERE_CONS(lfSav);
      continue;
    }
    estd = estRep(fich);
    if (! discoStat(nomFich, estd) && estd) {
      if (rec) {
        L_char lff = NULL;

        infoRep(nomFich);
        lff = contenuRepPrefix(tasGbl, fich);
        if (! discoRep(nomFich, lff)) {
          TRI_SUR_PLACE(char, &lff, lff, cmpChar);
          CONCAT(char, &lf, lff, QUEUE(char, lf));
        } else {
          lf = QUEUE(char, lf);
        }
      } else {
        infoNonRec(nomFich);
        lf = QUEUE(char, lf);
      }
      memLibere(fich);
      LIBERE_CONS(lfSav);
      continue;
    }
    estr = estReg(fich);
    if (! discoStat(fich, estr) && estr) {
      infoReg(nomFich);
/* traitement */
      switch (traiteFich(fich, opts)) {
        case 1:
          (*nbOk)++;
          infoOk(nomFich);
          break;
        case 0:
          (*nbKo)++;
          infoKo(nomFich);
          break;
        case -1:
          (*nbKc)++;
          infoKc(nomFich);
      }
/* fin traitement */
      lf = QUEUE(char, lf);
      memLibere(fich);
      LIBERE_CONS(lfSav);
      continue;
    }
    discoFichier(nomFich, -1);
    lf = QUEUE(char, lf);      
    memLibere(fich);
    LIBERE_CONS(lfSav);
  }
}

int main(int argc, char **argv) {
  T_options opts = NULL;
  int res = 0;

  tasGbl = memCreeTas();
  opts = analyseLdc(tasGbl, argc, argv);
  switch (opts->action) {
    case ACT_TRT: {
      int nbOk = 0, nbKo = 0, nbKc = 0;

      itereFichiers(
        opts->args.trt.fichiers, opts->args.trt.recursif,
        traitement, opts,
        &nbOk, &nbKo, &nbKc
      );
      infoNbOk(nbOk, nbOk + nbKo);
      infoNbKo(nbKo, nbOk + nbKo);
      infoNbKc(nbKc, nbOk + nbKo + nbKc);
      res = (nbKo == 0);
      break;
    }
    case ACT_FMT: {
      int nbOk = 0, nbKo = 0, nbKc = 0;

      itereFichiers(
        opts->args.fmt.fichiers, opts->args.fmt.recursif,
        verifieFormat, opts,
        &nbOk, &nbKo, &nbKc
      );
      infoNbOk(nbOk, nbOk + nbKc);
      infoNbKc(nbKc, nbOk + nbKc);
      res = (nbKc == 0);
      break;
    }
    case ACT_AID:
      aide(stdout, opts);
      res = ! opts->args.aid.err;
      break;
  }
  memLibereTout();
  return (res ? EXIT_SUCCESS : EXIT_FAILURE);
}

