#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#define CAML_NAME_SPACE
#include "caml/version.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/fail.h"
#include "caml/custom.h"

#include "mlang.h"

#if OCAML_VERSION < 41200

#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)

CAMLexport value caml_alloc_some(value v) {
  CAMLparam1(v);
  value some = caml_alloc_small(1, 0);
  Field(some, 0) = v;
  CAMLreturn(some);
}

#endif

#define Tgv_val(v) (*((T_irdata **) Data_custom_val(v)))

static void finalize_tgv(value tgv_block) {
  CAMLparam1(tgv_block);
  T_irdata *tgv = Tgv_val(tgv_block);
  detruis_irdata(tgv);
  CAMLreturn0;
}

static struct custom_operations tgv_block_ops = {
  "tgv.custom.block.ops",
  *finalize_tgv,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value ml_tgv_alloc(void){
  CAMLparam0();
  CAMLlocal1(mlTgv);
  T_irdata *tgv = cree_irdata();
  mlTgv = caml_alloc_custom(&tgv_block_ops, sizeof(T_irdata *), 0, 1);
  Tgv_val(mlTgv) = tgv;
  CAMLreturn(mlTgv);
}

#define Varinfo_val(v) (*((T_varinfo **) Data_custom_val(v)))

static struct custom_operations varinfo_block_ops = {
  "varinfo.custom.block.ops",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value alloc_varinfo(T_varinfo *info) {
  value v = caml_alloc_custom(&varinfo_block_ops, sizeof(T_varinfo *), 0, 1);
  Varinfo_val(v) = info;
  return v;
}

CAMLprim value ml_init_erreurs(value mlTgv) {
  CAMLparam1(mlTgv);
  T_irdata *tgv = Tgv_val(mlTgv);
  for (int i = 0; i < tgv->sz_err_finalise; i++) {
    tgv->err_finalise[i] = NULL;
  }
  tgv->nb_err_finalise = 0;
  for (int i = 0; i < tgv->sz_err_sortie; i++) {
    tgv->err_sortie[i] = NULL;
  }
  tgv->nb_err_sortie = 0;
  for (int i = 0; i < tgv->sz_err_archive; i++) {
    tgv->err_archive[i] = NULL;
  }
  tgv->nb_err_archive = 0;
  CAMLreturn(Val_unit);
}

CAMLprim value ml_get_discords(value mlTgv) {
  CAMLparam1(mlTgv);
  CAMLlocal2(mlErrListTemp, mlErrListOut);

  T_irdata *tgv = Tgv_val(mlTgv);

  struct S_discord * erreurs = tgv->discords;

  mlErrListOut = Val_emptylist;
  while (erreurs != NULL) {
    if (erreurs->erreur != NULL) {
      mlErrListTemp = caml_alloc_small(2, Tag_cons); // add code ?
      Field(mlErrListTemp, 0) = caml_copy_string(erreurs->erreur->nom);
      Field(mlErrListTemp, 1) = mlErrListOut;
      mlErrListOut = mlErrListTemp;
    }
    erreurs = erreurs->suivant;
  }

  CAMLreturn(mlErrListOut);
}

CAMLprim value ml_exporte_erreurs(value mlTgv) {
  CAMLparam1(mlTgv);
  T_irdata *tgv = Tgv_val(mlTgv);
  exporte_erreur(tgv);
  CAMLreturn(Val_unit);
}

CAMLprim value ml_get_erreurs(value mlTgv) {
  CAMLparam1(mlTgv);
  CAMLlocal2(mlErrListTemp, mlErrListOut);

  T_irdata *tgv = Tgv_val(mlTgv);

  char **errs = tgv->err_sortie;

  mlErrListOut = Val_emptylist;
  for (int i = 0; i < tgv->nb_err_sortie; i++) {
    mlErrListTemp = caml_alloc_small(2, Tag_cons); // add code ?
    Field(mlErrListTemp, 0) = caml_copy_string(errs[i]);
    Field(mlErrListTemp, 1) = mlErrListOut;
    mlErrListOut = mlErrListTemp;
  }

  CAMLreturn(mlErrListOut);
}

static T_varinfo *cherche_var(T_irdata *tgv, const char *code) {
  T_varinfo *varinfo = cherche_varinfo(tgv, code);
  if (varinfo == NULL) {
    fprintf(stderr, "La variable %s n'existe pas\n", code);
    exit(1);
  }
  return varinfo;
}

CAMLprim value ml_cherche_var_opt(value mlTgv, value mlCode) {
  CAMLparam2(mlTgv, mlCode);
  CAMLlocal1(mlRes);
  T_irdata *tgv = Tgv_val(mlTgv);
  const char *code = String_val(mlCode);
  T_varinfo *info = cherche_varinfo(tgv, code);
  if (info == NULL) {
    mlRes = Val_none;
  } else {
    mlRes = caml_alloc_some(alloc_varinfo(info));
  }
  CAMLreturn(mlRes);
}

CAMLprim value ml_nom_var(value mlInfo) {
  CAMLparam1(mlInfo);
  CAMLlocal1(mlNom);  
  T_varinfo *info = Varinfo_val(mlInfo);
  if (info == NULL) {
    fprintf(stderr, "La structure T_varinfo est absente\n");
    exit(1);
  }
  mlNom = caml_copy_string(info->name);
  CAMLreturn(mlNom);
}

CAMLprim value ml_alias_var(value mlInfo) {
  CAMLparam1(mlInfo);
  CAMLlocal1(mlAlias);  
  T_varinfo *info = Varinfo_val(mlInfo);
  if (info == NULL) {
    fprintf(stderr, "La structure T_varinfo est absente\n");
    exit(1);
  }
  if (info->alias[0] == 0) {
    mlAlias = Val_none;
  } else {
    mlAlias = caml_alloc_some(caml_copy_string(info->alias));
  }
  CAMLreturn(mlAlias);
}

CAMLprim value ml_get_var_opt(value mlTgv, value mlInfo) {
  CAMLparam2(mlTgv, mlInfo);
  CAMLlocal1(optOut);

  T_irdata *tgv = Tgv_val(mlTgv);
  T_varinfo *info = Varinfo_val(mlInfo);
  char def;
  double val;
  lis_varinfo(tgv, ESPACE_PAR_DEFAUT, info, &def, &val);
  if (def == 0) {
    optOut = Val_none;
  } else {
    optOut = caml_alloc_some(caml_copy_double(val));
  }

  CAMLreturn(optOut);
}

CAMLprim value ml_set_var_opt(value mlTgv, value mlInfo, value mlVal) {
  CAMLparam3(mlTgv, mlInfo, mlVal);

  T_irdata *tgv = Tgv_val(mlTgv);
  T_varinfo *info = Varinfo_val(mlInfo);
  if (mlVal == Val_none) {
    ecris_varinfo(tgv, ESPACE_PAR_DEFAUT, info, 0, 0.0);
  } else {
    ecris_varinfo(tgv, ESPACE_PAR_DEFAUT, info, 1, Double_val(Field(mlVal, 0)));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_annee_calc(value mlUnit) {
  CAMLparam1(mlUnit);
  CAMLreturn(Val_int(ANNEE_REVENU));
}

#define DECL_ENCH(ench) \
  CAMLprim value ml_##ench(value mlTgv) { \
    CAMLparam1(mlTgv); \
    T_irdata *tgv = Tgv_val(mlTgv); \
    ench(tgv); \
    CAMLreturn(Val_unit); \
  }

DECL_ENCH(enchainement_primitif_interpreteur)
DECL_ENCH(enchaineur_primitif)
DECL_ENCH(enchaineur_correctif)

CAMLprim value ml_set_evt_list(value mlTgv, value mlEvtList) {
  CAMLparam2(mlTgv, mlEvtList);
  CAMLlocal3(mlList, mlEvt, mlField);

  T_irdata *tgv = Tgv_val(mlTgv);
  int len = 0;
  mlList = mlEvtList;
  while (mlList != Val_emptylist) {
    len++;
    mlList = Field(mlList, 1);
  }
  if (len > 0) {
    tgv->events = (T_event **)malloc(len * sizeof (T_event *));
  } else {
    tgv->events = NULL;
  }
  tgv->nb_events = len;

  int i = 0;
  mlList = mlEvtList;
  while (mlList != Val_emptylist) {
    T_event *evt = (T_event *)malloc(sizeof (T_event));
    tgv->events[i] = evt;
    mlEvt = Field(mlList, 0);

    evt->field_numero_def = 1;
    evt->field_numero_val = Double_val(Field(mlEvt, 0));

    evt->field_rappel_def = 1;
    evt->field_rappel_val = Double_val(Field(mlEvt, 1));

    evt->field_code_var = cherche_var(tgv, String_val(Field(mlEvt, 2)));

    evt->field_montant_def = 1;
    evt->field_montant_val = Double_val(Field(mlEvt, 3));

    evt->field_sens_def = 1;
    evt->field_sens_val = Double_val(Field(mlEvt, 4));

    evt->field_penalite_def = 1;
    evt->field_penalite_val = Double_val(Field(mlEvt, 5));
 
    evt->field_base_tl_def = 1;
    evt->field_base_tl_val = Double_val(Field(mlEvt, 6));

    evt->field_date_def = 1;
    evt->field_date_val = Double_val(Field(mlEvt, 7));

    evt->field_2042_rect_def = 1;
    evt->field_2042_rect_val = Double_val(Field(mlEvt, 8));

    evt->field_anc_penalite_def = 0;
    evt->field_anc_penalite_val = 0.0;

    evt->field_id_evt_def = 0;
    evt->field_id_evt_val = 0.0;

    evt->field_strate_def = 0;
    evt->field_strate_val = 0.0;

    i++;
    mlList = Field(mlList, 1);
  }
  CAMLreturn(Val_unit);
}

