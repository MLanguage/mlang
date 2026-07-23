#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <utils.h>
#include <mem.h>
#include <liste.h>
#include <ida.h>
#include <options.h>

#define TEST_ARG(arg,arg_long,arg_court) \
  ( \
    strcmp(arg, "-" arg_long) == 0 \
    || strcmp(arg, "-" arg_court) == 0 \
    || strcmp(arg, "--" arg_long) == 0 \
    || strcmp(arg, "--" arg_court) == 0 \
  )

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
    if (TEST_ARG(argv[i], "recursif", "r")) {
      nbRec++;
      opts->args.fmt.recursif = VRAI;
      i++;
    } else if (TEST_ARG(argv[i], "strict", "s")) {
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
  if (opts->args.trt.defs != NULL) { \
    LIBERE_LISTE(char, opts->args.trt.defs); \
  } \
  if (opts->args.trt.fichiers != NULL) { \
    LIBERE_LISTE(char, opts->args.trt.fichiers); \
  } \
  opts->action = ACT_AID; \
  opts->args.aid.cat = NULL; \
  opts->args.aid.err = VRAI; \
  return opts;

T_options analyseLdcTrt(T_tas tas, T_options opts, int argc, char **argv, int i) {
  int nbMode = 0;
  int nbAnnee = 0;
  int nbRec = 0;
  int nbStrict = 0;

  opts->action = ACT_TRT;
  opts->args.trt.mode = Primitif;
  opts->args.trt.annee = ANNEE_REVENU + 1;
  opts->args.trt.recursif = FAUX;
  opts->args.trt.strict = FAUX;
  opts->args.trt.defs = NIL(S_varVal);
  opts->args.trt.fichiers = NIL(char);
  opts->args.trt.dest = NULL;
  while (i < argc) {
    if (TEST_ARG(argv[i], "mode", "m")) {
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
    } else if (TEST_ARG(argv[i], "annee", "a")) {
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
    } else if (TEST_ARG(argv[i], "recursif", "r")) {
      nbRec++;
      opts->args.trt.recursif = VRAI;
      i++;
    } else if (TEST_ARG(argv[i], "strict", "s")) {
      nbStrict++;
      opts->args.trt.strict = VRAI;
      i++;
    } else if (TEST_ARG(argv[i], "def", "D")) {
      char *nom = NULL;
      int j = 0;
      char *valStr = NULL;
      double val = 0.0;
      T_varVal vv = NULL;

      i++;
      nom = strCopie(tas, argv[i]);
      i++;
      for (j = 0; nom[j] != '\0' && nom[j] != '='; j++);
      if (nom[j] == '=') {
        nom[j] = '\0';
        valStr = &(nom[j + 1]);
      } else {
        valStr = argv[i];
        i++;
      }
      if (strcmp(valStr, "defaut") == 0) {
        val = INF;
      } else if (strcmp(valStr, "indefini") == 0) {
        val = NAN;
      } else if (strVersNum(valStr, &val) != 1) {
        TRT_ERR(anoOptsDefValArg(valStr))
      }
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

T_options analyseLdcTraitement(T_tas tas, T_options opts, int argc, char **argv, int i) {
  infoActTrt();
  return analyseLdcTrt(tas, opts, argc, argv, i);
}

T_options analyseLdcCompletion(T_tas tas, T_options opts, int argc, char **argv, int i) {
  char *dest = "";
  int estr = 0;

  infoActCpl();
  i++;
  if (argc <= i || strcmp(argv[i], "") == 0 || argv[i][0] == '-') {
    TRT_ERR(anoOptsDstAbs())
  }
  dest = strCopie(tas, argv[i]);
  analyseLdcTrt(tas, opts, argc, argv, i);
  opts->action = ACT_CPL;
  opts->args.trt.dest = dest;
  estr = estRep(opts->args.trt.dest);
  if (estr == -1 || ! estr) {
    TRT_ERR(anoOptsDstRep(opts->args.trt.dest))
  }
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
  if (TEST_ARG(argv[i], "aide", "?")) {
    return analyseLdcAide(opts, argc, argv, i);
  }

  /* format */
  if (TEST_ARG(argv[i], "format", "f")) {
    return analyseLdcFormat(tas, opts, argc, argv, i);
  }

  /* completion */
  if (strcmp(argv[i], "-completion") == 0 || strcmp(argv[i], "-c") == 0) {
    return analyseLdcCompletion(tas, opts, argc, argv, i);
  }

  /* traitement */
  return analyseLdcTraitement(tas, opts, argc, argv, i);
}

