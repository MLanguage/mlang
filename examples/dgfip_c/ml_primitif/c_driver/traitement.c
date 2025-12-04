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
#include <commun.h>
#include <options.h>
#include <ida.h>
#include <traitement.h>

#include <mlang.h>

char *creeAno(T_tas tas, T_irj_def_ano da) {
  if (da == NULL) return NULL;
  return strCopie(tas, da->ano);
}

void libereAno(char *a) {
  memLibere(a);
}

struct S_rap;
typedef struct S_rap S_rap;
typedef S_rap * T_rap;
struct S_rap {
  double numero;
  double rappel;
  T_varinfo *code;
  double montant;
  double sens;
  double penalite;
  double base_tl;
  double date;
  double _2042_rect;
};

T_rap creeRap(T_tas tas, T_irdata *tgv, T_irj_def_rap dr) {
  T_rap r = NULL;
  
  if (dr == NULL) return NULL;
  r = (T_rap)memAlloue(tas, sizeof (S_rap));
  r->numero = dr->numero;
  r->rappel = dr->rappel;
  r->code = cherche_varinfo(tgv, dr->code);
  if (r->code == NULL) return NULL;
  r->montant = dr->montant;
  r->sens = dr->sens;
  r->penalite = dr->penalite;
  r->base_tl = dr->base_tl;
  r->date = dr->date;
  r->_2042_rect = dr->_2042_rect;
  return r;
}

void libereRap(T_rap r) {
  if (r == NULL) return;
  memLibere(r);
}

TYPEDEF_LISTE(S_rap)

int ecrisVar(T_irdata *tgv, char *nom, int def, double val) {
  T_varinfo *varinfo = cherche_varinfo(tgv, nom);
  if (varinfo == NULL) {
    return 0;
  }
  if (def == 0) {
    ecris_varinfo(tgv, ESPACE_PAR_DEFAUT, varinfo, 0, 0.0);
  } else {
    ecris_varinfo(tgv, ESPACE_PAR_DEFAUT, varinfo, 1, val);
  }
  return 1;
}

int lisAnneeRevenu(T_irdata *tgv, int anneeCalc) {
  T_varinfo *varinfo = NULL;
  char def = 0;
  double val = 0.0;

  varinfo = cherche_varinfo(tgv, "ANREV");
  if (varinfo == NULL) {
    return anneeCalc;
  }
  lis_varinfo(tgv, ESPACE_PAR_DEFAUT, varinfo, &def, &val);
  if (def == 0) {
    return anneeCalc;
  } else {
    return (int)val;
  }
}

L_char erreursVersListe(T_tas tas, T_irdata *tgv) {
  L_char res = NULL;
  char **errs = NULL;
  int i = 0;

  res = NIL(char);
  errs = tgv->err_sortie;
  for (i = 0; i < tgv->nb_err_sortie; i++) {
    res = CONS(tas, char, strCopie(tas, errs[i]), res);
  }
  TRI_SUR_PLACE(char, &res, res, cmpChar);
  ELIM_DOUBLONS(char, res, egalChar);

  return res;
}

int controleResultat(T_tas tas, T_irdata *tgv, L_S_varVal res, L_char ctl) {
  L_char errs = NULL;
  int cas = 0; /* 0: OK, 1: KO trop, 2: KO non recue*/
  int ok = 1;

  errs = erreursVersListe(tas, tgv);
  while (errs != NIL(char) || ctl != NIL(char)) {
    if (errs == NIL(char)) cas = 2;
    else if (ctl == NIL(char)) cas = 1;
    else {
      int cmp = strcmp(TETE(char, ctl), TETE(char, errs));
      if (cmp < 0) cas = 1;
      else if (cmp > 0) cas = 2;
      else cas = 0;
    }
    switch (cas) {
      case 1:
        anoErrEnTrop(TETE(char, errs));
        errs = QUEUE(char, errs);
        ok = 0;
        break;
      case 2:
        anoErrNonRecue(TETE(char, ctl));
        ctl = QUEUE(char, ctl);
        ok = 0;
        break;
      default:
        ctl = QUEUE(char, ctl);
        errs = QUEUE(char, errs);
        break;
    }
  }

  while (res != NIL(S_varVal)) {
    S_varVal *vv = TETE(S_varVal, res);
    char def = 0;
    double val = 0.0;
    double val100 = 0.0;
    double valRes100 = 0.0;
    int lng = strlen(vv->varinfo->name);

    if (
      ! (
        strncmp(vv->varinfo->name, "NBPT", 4) == 0
        || strncmp(vv->varinfo->name, "RETX", 4) == 0
        || (strncmp(vv->varinfo->name, "NATMAJ", 6) == 0 && (lng == 7 || lng == 9 || lng == 10))
        || strncmp(vv->varinfo->name, "TL_", 3) == 0
      )
      && vv->varinfo->est_restituee
    ) {
      lis_varinfo(tgv, ESPACE_PAR_DEFAUT, vv->varinfo, &def, &val);
      val100 = arrondi(val * 100.0);
      valRes100 = arrondi(vv->val * 100.0);
      if (fabs(val100 - valRes100) > 0.0) {
        anoValeurFausse(vv->varinfo->name, val, vv->val);
        ok = 0;
      }
    }
    res = QUEUE(S_varVal, res);
  }

  return ok;
}

#define ECRIS_VAR(nom, val) \
  if (! ecrisVar(tgv, nom, 1, val)) { \
    anoVarAbs(nom); \
    ok = -1; \
    goto fin; \
  }

int ecrisRappels(T_irdata *tgv, L_S_rap rappels) {
  tgv->nb_events = LONGUEUR(S_rap, rappels);
  if (tgv->nb_events == 0) {
    tgv->events = NULL;
  } else {
    int i = 0;

    tgv->events = (T_event **)malloc(tgv->nb_events * sizeof (T_event *));
    while (rappels != NIL(S_rap)) {
      S_rap *r = NULL;
      T_event *evt = NULL;

      r = TETE(S_rap, rappels);
      evt = (T_event *)malloc(sizeof (T_event));
      tgv->events[i] = evt;

      evt->field_numero_def = 1;
      evt->field_numero_val = r->numero;

      evt->field_rappel_def = 1;
      evt->field_rappel_val = r->rappel;

      evt->field_code_var = r->code;

      evt->field_montant_def = 1;
      evt->field_montant_val = r->montant;

      evt->field_sens_def = 1;
      evt->field_sens_val = r->sens;

      evt->field_penalite_def = 1;
      evt->field_penalite_val = r->penalite;
   
      evt->field_base_tl_def = 1;
      evt->field_base_tl_val = r->base_tl;

      evt->field_date_def = 1;
      evt->field_date_val = r->date;

      evt->field_2042_rect_def = 1;
      evt->field_2042_rect_val = r->_2042_rect;

      evt->field_anc_penalite_def = 0;
      evt->field_anc_penalite_val = 0.0;

      evt->field_id_evt_def = 0;
      evt->field_id_evt_val = 0.0;

      evt->field_strate_def = 0;
      evt->field_strate_val = 0.0;

      rappels = QUEUE(S_rap, rappels);
      i++;
    }
  }

  return 1;
}

void initDefs(T_irdata *tgv, L_S_varVal defs) {
  L_S_varVal l = NULL;

  for (l = defs; l != NIL(S_varVal); l = QUEUE(S_varVal, l)) {
    T_varVal vv = TETE(S_varVal, l);
    
    ecris_varinfo(tgv, ESPACE_PAR_DEFAUT, vv->varinfo, 1, vv->val);
  }
}

int traitement(char *chemin, T_options opts) {
  T_tas tasTrt = NULL;
  T_fich fich = NULL;
  T_irj irj = NULL;
  int code = IRJ_CODE_VIDE;
  L_char lnom = NULL;
  char *nom = NULL;
  int estCorr = 0;
  T_irdata *tgv = NULL;
  L_S_varVal resPrim = NULL;
  L_char ctlPrim = NULL;
  L_S_rap rappels = NULL;
  L_S_varVal resRap = NULL;
  L_char ctlRap = NULL;
  int anneeCalc = 0;
  int anneeRevenu = 0;
  int ok = 1;

  tasTrt = memCreeTas();
  estCorr = FAUX;
  resPrim = NIL(S_varVal);
  ctlPrim = NIL(char);
  rappels = NIL(S_rap);
  resRap = NIL(S_varVal);
  ctlRap = NIL(char);
  fich = ouvreFich(tasTrt, chemin);
  if (fich == NULL) {
    discoFichier(chemin, -1);
    ok = -1;
    goto fin;
  }
  irj = creeIrj(tasTrt, opts->args.trt.strict);
  code = codeIrj(irj);
  lnom = NIL(char);
  tgv = cree_irdata();
  while (code != IRJ_FIN && code != IRJ_INVALIDE) {
    lisIrj(fich, irj);
    code = codeIrj(irj);
    switch (code) {
      case IRJ_NOM:
        lnom = CONS(tasTrt, char, strCopie(tasTrt, irj->args.nom), lnom);
        break;
      case IRJ_NOM_FIN:
        RETOURNE(char, &lnom, lnom);
        nom = strConcatListe(tasTrt, lnom);
        break;
      case IRJ_DEF_VAR:
        switch (irj->section) {
          case IRJ_ENTREES_PRIMITIF_DEBUT:
            ECRIS_VAR(irj->args.defVar.var, irj->args.defVar.val);
            break;
          case IRJ_RESULTATS_PRIMITIF_DEBUT: {
            T_varVal vv = creeVarVal(tasTrt, irj->args.defVar.var, irj->args.defVar.val);

            if (vv == NULL) {
              if (opts->args.trt.strict) {
                anoVarAbs(irj->args.defVar.var);
                ok = -1;
                goto fin;
              }
            } else {
              resPrim = CONS(tasTrt, S_varVal, vv, resPrim);
            }
            break;
          }
          case IRJ_RESULTATS_CORRECTIF_DEBUT:
          case IRJ_RESULTATS_RAPPELS_DEBUT: {
            T_varVal vv = creeVarVal(tasTrt, irj->args.defVar.var, irj->args.defVar.val);

            if (vv == NULL) {
              if (opts->args.trt.strict) {
                anoVarAbs(irj->args.defVar.var);
                ok = -1; 
                goto fin;
              }
            } else {
              if (opts->args.trt.strict && ! vv->varinfo->est_restituee) {
                anoVarNonRestituee(irj->args.defVar.var);
                ok = -1; 
                goto fin;
              } else if (vv->varinfo->est_restituee) {
                resRap = CONS(tasTrt, S_varVal, vv, resRap);
              }
            }
            estCorr = VRAI;
            break;
          }
          default:
            ok = -1;
            goto fin;
        }
        break;
      case IRJ_DEF_ANO:
        switch (irj->section) {
          case IRJ_CONTROLES_PRIMITIF_DEBUT:
            ctlPrim = CONS(tasTrt, char, creeAno(tasTrt, &(irj->args.defAno)), ctlPrim);
            break;
          case IRJ_CONTROLES_CORRECTIF_DEBUT:
          case IRJ_CONTROLES_RAPPELS_DEBUT:
            ctlRap = CONS(tasTrt, char, creeAno(tasTrt, &(irj->args.defAno)), ctlRap);
            estCorr = VRAI;
            break;
          default:
            ok = -1;
            goto fin;
        }
        break;
      case IRJ_DEF_RAP:
        if (irj->section == IRJ_ENTREES_RAPPELS_DEBUT) {
          T_rap rap = creeRap(tasTrt, tgv, &(irj->args.defRap));

          if (rap == NULL) {
            anoVarAbs(irj->args.defRap.code);
            ok = -1; 
            goto fin;
          }
          rappels = CONS(tasTrt, S_rap, rap, rappels);
          estCorr = VRAI;
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

  RETOURNE(S_varVal, &resPrim, resPrim);
  TRI_SUR_PLACE(char, &ctlPrim, ctlPrim, cmpChar);
  ELIM_DOUBLONS(char, ctlPrim, egalChar);  
  RETOURNE(S_rap, &rappels, rappels);
  RETOURNE(S_varVal, &resRap, resRap);
  TRI_SUR_PLACE(char, &ctlRap, ctlRap, cmpChar);
  ELIM_DOUBLONS(char, ctlRap, egalChar);
  ignore((int)estCorr);

  anneeCalc = ANNEE_REVENU;
  anneeRevenu = lisAnneeRevenu(tgv, anneeCalc);
  discoAnneeRevenu(anneeCalc, anneeRevenu);
  if (! ecrisRappels(tgv, rappels)) {
    ok = -1;
    goto fin;
  }
  ecrisVar(tgv, "ANCSDED", 1, opts->args.trt.annee);
  ecrisVar(tgv, "V_MILLESIME", 1, anneeCalc);
  switch (opts->args.trt.mode) {
    case Primitif:
      initDefs(tgv, opts->args.trt.defs);
      enchainement_primitif_interpreteur(tgv);
      ok = controleResultat(tasTrt, tgv, resPrim, ctlPrim);
      break;
    case Correctif:
      ecrisVar(tgv, "MODE_CORR", 1, 1.0);
      initDefs(tgv, opts->args.trt.defs);
      enchaineur_primitif(tgv);
      enchaineur_correctif(tgv);
      ok = controleResultat(tasTrt, tgv, resRap, ctlRap);
      break;
  }

fin:
  detruis_irdata(tgv);
  memLibere(nom);
  fermeFich(fich);
  memLibereTas(tasTrt);
  return ok;
}
