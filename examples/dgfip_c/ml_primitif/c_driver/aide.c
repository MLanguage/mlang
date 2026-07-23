#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <utils.h>
#include <options.h>
#include <ida.h>
#include <aide.h>

void aide_aid(FILE *sortie, T_options opts) {
  fprintf(sortie, "Affichage de l'aide :\n");
  fprintf(sortie, "  %s -aide (-?) [catégorie d'aide] (optionnelle)\n", opts->exe);
  fprintf(sortie, "\n");
  fprintf(sortie, "Catégories d'aide :\n");
  fprintf(sortie, "  traitement : aide sur le traitement des fichiers IRJ;\n");
  fprintf(sortie, "  format : aide sur le validateur de fichiers IRJ;\n");
  fprintf(sortie, "  completion : aide sur la complétion des fichiers IRJ;\n");
  fprintf(sortie, "  aide : aide sur l'aide (catégorie par défaut);\n");
  fprintf(sortie, "  tout : toutes les aides.\n");
  fflush(sortie);
}

void aide_trt(FILE *sortie, T_options opts) {
  fprintf(sortie, "Traitement des fichiers IRJ :\n");
  fprintf(sortie, "  %s\n", opts->exe);
  fprintf(sortie, "    -mode [primitif|correctif] (-m [p|c])\n");
  fprintf(sortie, "    -annee [année] (-a [année])\n");
  fprintf(sortie, "    -def [variable] [valeur] (-D [variable] [valeur])\n");
  fprintf(sortie, "      notation alternative:\n");
  fprintf(sortie, "        --def [variable]=[valeur]\n");
  fprintf(sortie, "        -D [variable]=[valeur])\n");
  fprintf(sortie, "    -recursif (-r)\n");
  fprintf(sortie, "    -strict (-s)\n");
  fprintf(sortie, "    [fichiers IRJ] ([fichiers IRJ ne commençant pas par '-'])\n");
  fprintf(sortie, "    -- [fichiers IRJ]\n");
  fflush(sortie);
}

void aide_fmt(FILE *sortie, T_options opts) {
  fprintf(sortie, "Vérification du format des fichiers IRJ :\n");
  fprintf(sortie, "  %s -format (-f)\n", opts->exe);
  fprintf(sortie, "    -recursif (-r)\n");
  fprintf(sortie, "    -strict (-s)\n");
  fprintf(sortie, "    [fichiers IRJ] ([fichiers IRJ ne commençant pas par '-'])\n");
  fprintf(sortie, "    -- [fichiers IRJ]\n");
  fflush(sortie);
}

void aide_cpl(FILE *sortie, T_options opts) {
  fprintf(sortie, "Complétion des fichiers IRJ (résultats dans [répertoire]) :\n");
  fprintf(sortie, "  %s -completion [répertoire] (-c [répertoire])\n", opts->exe);
  fprintf(sortie, "    -mode [primitif|correctif] (-m [p|c])\n");
  fprintf(sortie, "    -annee [année] (-a [année])\n");
  fprintf(sortie, "    -def [variable] [valeur] (-d [variable] [valeur])\n");
  fprintf(sortie, "    -recursif (-r)\n");
  fprintf(sortie, "    -strict (-s)\n");
  fprintf(sortie, "    [fichiers IRJ] ([fichiers IRJ ne commençant pas par '-'])\n");
  fprintf(sortie, "    -- [fichiers IRJ]\n");
  fflush(sortie);
}

void aide(FILE *sortie, T_options opts) {
  fprintf(sortie, "\n");
  if (opts->args.aid.cat == NULL || strcmp(opts->args.aid.cat, "aide") == 0) {
    aide_aid(sortie, opts);    
    return;
  }
  if (strcmp(opts->args.aid.cat, "traitement") == 0) {
    aide_trt(sortie, opts);
    return;
  }
  if (strcmp(opts->args.aid.cat, "format") == 0) {
    aide_fmt(sortie, opts);
    return;
  }
  if (strcmp(opts->args.aid.cat, "completion") == 0) {
    aide_cpl(sortie, opts);
    return;
  }
  if (strcmp(opts->args.aid.cat, "tout") == 0) {
    aide_trt(sortie, opts);
    fprintf(sortie, "\n");
    aide_fmt(sortie, opts);
    fprintf(sortie, "\n");
    aide_cpl(sortie, opts);
    fprintf(sortie, "\n");
    aide_aid(sortie, opts);
    return;
  }
  opts->args.aid.err = VRAI;
  anoOptsCat(opts->args.aid.cat);
  fprintf(sortie, "\n");
  aide_aid(sortie, opts);
}
