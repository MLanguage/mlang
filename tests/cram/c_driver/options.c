#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <utils.h>
#include <mem.h>
#include <liste.h>
#include <ida.h>
#include <options.h>

T_options analyseLdcSans(T_options opts) {
  infoActVide();
  opts->action = ACT_AID;
  opts->args.aid.cat = NULL;
  opts->args.aid.err = FAUX;
  return opts;
}

T_options analyseLdcAide(T_options opts, int argc, char **argv, int i) {
  infoActAide();
  opts->action = ACT_AID;
  opts->args.aid.cat = NULL;
  i++;
  if (argc <= i) return opts;
  opts->args.aid.cat = argv[i];
  i++;
  if (argc > i) {
    anoOptsTrop(argv[i]);
    opts->args.aid.cat = NULL;
    opts->args.aid.err = VRAI;
  }
  return opts;
}

#define FMT_ERR(ano) \
  ano; \
  LIBERE_LISTE(char, opts->args.fmt.fichiers); \
  opts->action = ACT_AID; \
  opts->args.aid.cat = NULL; \
  opts->args.aid.err = VRAI; \
  return opts;

T_options analyseLdcFormat(T_tas tas, T_options opts, int argc, char **argv, int i) {
  int nbRec = 0;
  int nbStrict = 0;

  infoActFmt();
  opts->action = ACT_FMT;
  opts->args.fmt.recursif = FAUX;
  opts->args.fmt.strict = FAUX;
  opts->args.fmt.fichiers = NIL(char);
  i++;
  while (i < argc) {
    if (strcmp(argv[i], "-recursif") == 0 || strcmp(argv[i], "-r") == 0) {
      nbRec++;
      opts->args.fmt.recursif = VRAI;
      i++;
    } else if (strcmp(argv[i], "-strict") == 0 || strcmp(argv[i], "-s") == 0) {
      nbStrict++;
      opts->args.fmt.strict = VRAI;
      i++;
    } else if (strcmp(argv[i], "--") == 0) {
      i++;
      while (i < argc) {
        opts->args.fmt.fichiers = CONS(tas, char, strCopie(tas, argv[i]), opts->args.fmt.fichiers);
        i++;
      }
      RETOURNE(char, &(opts->args.fmt.fichiers), opts->args.fmt.fichiers);
      goto fin;
    } else if (strcmp(argv[i], "") != 0 && argv[i][0] != '-') {
      opts->args.fmt.fichiers = CONS(tas, char, strCopie(tas, argv[i]), opts->args.fmt.fichiers);
      i++;
    } else {
      FMT_ERR(anoOptsInc(argv[i]))
    }
  }
  RETOURNE(char, &(opts->args.fmt.fichiers), opts->args.fmt.fichiers);

fin:
  discoOptsRecDup(nbRec > 1);
  discoOptsStrictDup(nbStrict > 1);
  return opts;
}

#define TRT_ERR(ano) \
  ano; \
  LIBERE_LISTE(char, opts->args.trt.defs); \
  LIBERE_LISTE(char, opts->args.trt.fichiers); \
  opts->action = ACT_AID; \
  opts->args.aid.cat = NULL; \
  opts->args.aid.err = VRAI; \
  return opts;

T_options analyseLdcTraitement(T_tas tas, T_options opts, int argc, char **argv, int i) {
  int nbMode = 0;
  int nbAnnee = 0;
  int nbRec = 0;
  int nbStrict = 0;

  infoActTrt();
  opts->action = ACT_TRT;
  opts->args.trt.mode = Primitif;
  opts->args.trt.annee = ANNEE_REVENU + 1;
  opts->args.trt.recursif = FAUX;
  opts->args.trt.strict = FAUX;
  opts->args.trt.defs = NIL(S_varVal);
  opts->args.trt.fichiers = NIL(char);
  while (i < argc) {
    if (strcmp(argv[i], "-mode") == 0 || strcmp(argv[i], "-m") == 0) {
      T_mode mode = Primitif;

      i++;
      nbMode++;
      if (argc <= i || strcmp(argv[i], "") == 0 || argv[i][0] == '-') {
        TRT_ERR(anoOptsModeAbs())
      }
      if (strcmp(argv[i], "primitif") == 0 || strcmp(argv[i], "p") == 0) {
        mode = Primitif;
        i++;
      } else if (strcmp(argv[i], "correctif") == 0 || strcmp(argv[i], "c") == 0) {
        mode = Correctif;
        i++;
      } else {
        TRT_ERR(anoOptsModeArg(argv[i]))
      }
      if (nbMode > 1 && opts->args.trt.mode != mode) {
        TRT_ERR(anoOptsModeDup(opts->args.trt.mode, mode))
      } else {
        opts->args.trt.mode = mode;
      }
    } else if (strcmp(argv[i], "-annee") == 0 || strcmp(argv[i], "-a") == 0) {
      int annee = 0;

      i++;
      nbAnnee++;
      if (argc <= i || strcmp(argv[i], "") == 0 || argv[i][0] == '-') {
        TRT_ERR(anoOptsAnneeAbs())
      }
      if (strVersUint(argv[i], &annee) != 1) {
        TRT_ERR(anoOptsAnneeArg(argv[i]))
      }
      if (nbAnnee > 1 && opts->args.trt.annee != annee) {
        TRT_ERR(anoOptsAnneeDup(opts->args.trt.annee, annee))
      } else {
        opts->args.trt.annee = annee;
      }
      i++;
    } else if (strcmp(argv[i], "-recursif") == 0 || strcmp(argv[i], "-r") == 0) {
      nbRec++;
      opts->args.trt.recursif = VRAI;
      i++;
    } else if (strcmp(argv[i], "-strict") == 0 || strcmp(argv[i], "-s") == 0) {
      nbStrict++;
      opts->args.trt.strict = VRAI;
      i++;
    } else if (strcmp(argv[i], "-def") == 0 || strcmp(argv[i], "-d") == 0) {
      char *nom = NULL;
      double val = 0.0;
      T_varVal vv = NULL;

      i++;
      nom = argv[i];
      i++;
      if (strVersNum(argv[i], &val) != 1) {
        TRT_ERR(anoOptsDefValArg(argv[i]))
      }
      i++;
      vv = creeVarVal(tas, nom, val);
      if (vv == NULL) {
        if (opts->args.trt.strict) {
          TRT_ERR(anoOptsDefVarArg(nom))
        }
      } else {
        opts->args.trt.defs = CONS(tas, S_varVal, vv, opts->args.trt.defs);
      }
    } else if (strcmp(argv[i], "--") == 0) {
      i++;
      while (i < argc) {
        opts->args.trt.fichiers = CONS(tas, char, strCopie(tas, argv[i]), opts->args.trt.fichiers);
        i++;
      }
      RETOURNE(S_varval, &(opts->args.trt.defs), opts->args.trt.defs);
      RETOURNE(char, &(opts->args.trt.fichiers), opts->args.trt.fichiers);
      goto fin;
    } else if (strcmp(argv[i], "") != 0 && argv[i][0] != '-') {
      opts->args.trt.fichiers = CONS(tas, char, strCopie(tas, argv[i]), opts->args.trt.fichiers);
      i++;
    } else { 
      TRT_ERR(anoOptsInc(argv[i]))
    }
  }
  RETOURNE(S_varval, &(opts->args.trt.defs), opts->args.trt.defs);
  RETOURNE(char, &(opts->args.trt.fichiers), opts->args.trt.fichiers);

fin:
  if (nbMode == 0) {
    TRT_ERR(anoOptsModeAbsent())
  }
  discoOptsModeDup(nbMode > 1);  
  discoOptsAnneeParDefaut(opts->args.trt.annee, nbAnnee == 0);
  discoOptsAnneeDup(nbAnnee > 1);  
  discoOptsRecDup(nbRec > 1);
  discoOptsStrictDup(nbStrict > 1);
  return opts;
}

T_options analyseLdc(T_tas tas, int argc, char **argv) {
  T_options opts = NULL;
  int i = 0;

  opts = (T_options)memAlloue(tas, sizeof (S_options));
  if (argv == NULL || argc <= i) {
    anoOptsExe();
    opts->exe = "commande";
    opts->action = ACT_AID;
    opts->args.aid.cat = NULL;
    opts->args.aid.err = VRAI;
    return opts;
  }
  opts->exe = argv[i];
  i++;

  /* sans argument */
  if (argc <= i) {
    return analyseLdcSans(opts);
  }

  /* aide */
  if (strcmp(argv[i], "-aide") == 0 || strcmp(argv[i], "-?") == 0) {
    return analyseLdcAide(opts, argc, argv, i);
  }

  /* format */
  if (strcmp(argv[i], "-format") == 0 || strcmp(argv[i], "-f") == 0) {
    return analyseLdcFormat(tas, opts, argc, argv, i);
  }

  /* traitement */
  return analyseLdcTraitement(tas, opts, argc, argv, i);
}
