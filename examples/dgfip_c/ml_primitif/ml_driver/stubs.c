
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

#include "annee.h"
#include "conf.h"
#include "irdata.h"
#include "const.h"
#include "var.h"
#include "enchain.h"

#if OCAML_VERSION < 41200

#define Val_none Val_int(0)

CAMLexport value caml_alloc_some(value v)
{
  CAMLparam1(v);
  value some = caml_alloc_small(1, 0);
  Field(some, 0) = v;
  CAMLreturn(some);
}

#endif

// Non exportés dans headers standards
extern T_desc_penalite desc_penalite[];
extern T_desc_debug desc_debug01[];
#if NB_DEBUG_C >= 2
extern T_desc_debug desc_debug02[];
#endif
#if NB_DEBUG_C >= 3
extern T_desc_debug desc_debug03[];
#endif
#if NB_DEBUG_C >= 4
extern T_desc_debug desc_debug04[];
#endif
#if NB_DEBUG_C >= 5
#error "Ne fonctionne qu'avec NB_DEBUG_C compris entre 1 et 4"
#endif

struct S_desc_var
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord * (*verif)(T_irdata *);
};

typedef struct S_desc_var T_desc_var;

#define T_var_irdata T_desc_var *

typedef void (*ench_fun)(T_irdata *);

typedef struct ench_t {
  char *name;
  ench_fun function;
} ench_t;

static ench_t enchaineurs[] = {
  { "calcul_primitif", calcul_primitif },
  { "calcul_primitif_isf", calcul_primitif_isf },
  { "calcul_primitif_taux", calcul_primitif_taux },
  { "calcul_correctif", calcul_correctif },

  { "sauve_base_initial", sauve_base_initial },
  { "sauve_base_1728", sauve_base_1728 },
  { "sauve_base_anterieure_cor", sauve_base_anterieure_cor },
  { "sauve_base_premier", sauve_base_premier },

  { "sauve_base_tl_init", sauve_base_tl_init },
  { "sauve_base_tl", sauve_base_tl },
  { "sauve_base_tl_rect", sauve_base_tl_rect },
  { "sauve_base_tlnunv", sauve_base_tlnunv },

  { "sauve_base_inr_r9901", sauve_base_inr_r9901 },
  { "sauve_base_inr_cimr99", sauve_base_inr_cimr99 },
  { "sauve_base_HR", sauve_base_HR },
  { "sauve_base_inr_cimr07", sauve_base_inr_cimr07 },
  { "sauve_base_inr_tlcimr07", sauve_base_inr_tlcimr07 },
  { "sauve_base_inr_cimr24", sauve_base_inr_cimr24 },
  { "sauve_base_inr_tlcimr24", sauve_base_inr_tlcimr24 },
  { "sauve_base_inr_ref", sauve_base_inr_ref },
  { "sauve_base_inr_ntl", sauve_base_inr_ntl },
  { "sauve_base_abat98", sauve_base_abat98 },
  { "sauve_base_inr_ntl", sauve_base_inr_ntl },
  { "sauve_base_inr_intertl", sauve_base_inr_intertl },
  { "sauve_base_inr_ntl22", sauve_base_inr_ntl22 },
  { "sauve_base_inr", sauve_base_inr },
  { "sauve_base_inr_ntl24", sauve_base_inr_ntl24 },
  { "sauve_base_inr_tl", sauve_base_inr_tl },
  { "sauve_base_abat99", sauve_base_abat99 },
  { "sauve_base_inr_tl", sauve_base_inr_tl },
  { "sauve_base_inr_tl22", sauve_base_inr_tl22 },
  { "sauve_base_inr_tl24", sauve_base_inr_tl24 },
  { "sauve_base_inr_inter22", sauve_base_inr_inter22 },

  { "sauve_base_majo", sauve_base_majo },
  { "sauve_base_anterieure", sauve_base_anterieure },
  { "sauve_base_stratemajo", sauve_base_stratemajo },

  { "ENCH_TL", ENCH_TL },

  { "article_1731_bis", article_1731_bis }
};

extern struct S_discord * verif_calcul_primitive(T_irdata *irdata);
extern struct S_discord * verif_calcul_primitive_isf(T_irdata *irdata);
extern struct S_discord * verif_calcul_corrective(T_irdata *irdata);
extern struct S_discord * verif_saisie_cohe_primitive(T_irdata *irdata);
extern struct S_discord * verif_saisie_cohe_primitive_isf(T_irdata *irdata, int appel);
extern struct S_discord * verif_saisie_cohe_corrective(T_irdata *irdata);
extern struct S_discord * verif_cohe_horizontale(T_irdata *irdata);

struct S_discord * verif_saisie_cohe_primitive_isf_stub(T_irdata *irdata)
{
  return verif_saisie_cohe_primitive_isf(irdata, 0);
}

typedef struct S_discord * (*verif_fun)(T_irdata *);

typedef struct verif_t {
  char *name;
  verif_fun function;
} verif_t;

static verif_t verifications[] = {
  { "verif_calcul_primitive", verif_calcul_primitive },
  { "verif_calcul_primitive_isf",  verif_calcul_primitive_isf },
  { "verif_calcul_corrective", verif_calcul_corrective },
  { "verif_saisie_cohe_primitive", verif_saisie_cohe_primitive },
  { "verif_saisie_cohe_primitive_isf", verif_saisie_cohe_primitive_isf_stub },
  { "verif_saisie_cohe_corrective", verif_saisie_cohe_corrective },
  { "verif_cohe_horizontale", verif_cohe_horizontale },
};

typedef enum genre_t {
  G_SAISIE = 1,
  G_CALCULEE = 2,
  G_BASE = 3,
} genre_t;

typedef enum domaine_t {
  D_INDEFINI = -1,
  D_CONTEXTE = 1,
  D_FAMILLE = 2,
  D_REVENU = 3,
  D_REVENU_CORR = 4,
  D_VARIATION = 5,
  D_PENALITE = 6,
} domaine_t;

typedef enum type_t {
  T_REEL = 1,
  T_BOOLEEN = 2,
  T_DATE = 3,
} type_t;

typedef enum nature_t {
  N_INDEFINIE = -1,
  N_REVENU = 1,
  N_CHARGE = 2,
} nature_t;

typedef struct var_t {
  char *code;
  char *alias;
  genre_t genre;
  domaine_t domaine;
  type_t type;
  nature_t nature;
  int classe;
  int cat_tl;
  int cot_soc;
  bool ind_abat;
  int rap_cat;
  int sanction;
  int cat_1731b;
  int indice_tab;
  bool acompte;
  int avfisc;
  bool restituee;
  T_desc_var *desc;
} var_t;

static var_t var[TAILLE_TOTALE] = { NULL };

typedef struct var_entry_t {
  const char *code; // including alias
  var_t *var;
} var_entry_t;

static var_entry_t * var_index = NULL;
static size_t var_index_entries = 0;
static size_t var_index_size = 0;

static bool var_chargees = false;

#define Tgv_val(v) (*((T_irdata **) Data_custom_val(v)))

static void finalize_tgv(value tgv_block){
  CAMLparam1(tgv_block);
  T_irdata *tgv = Tgv_val(tgv_block);
  IRDATA_delete_irdata(tgv);
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

static value alloc_tgv()
{
  T_irdata *tgv = IRDATA_new_irdata();
  value v = caml_alloc_custom(&tgv_block_ops, sizeof(T_irdata *), 0, 1);
  Tgv_val(v) = tgv;
  return v;
}

CAMLprim value
ml_tgv_alloc(void){
  CAMLparam0();
  CAMLlocal1(mlTgv);
  mlTgv = alloc_tgv();
  CAMLreturn(mlTgv);
}

static genre_t convert_genre(int indice)
{
  switch (indice & EST_MASQUE) {
    case EST_SAISIE: return G_SAISIE;
    case EST_CALCULEE: return G_CALCULEE;
    case EST_BASE: return G_BASE;
    default:
      fprintf(stderr, "Genre %8X invalide\n", indice & EST_MASQUE);
      exit(1);
      return -1;
  }
}

static type_t convert_type(long type_donnee)
{
  switch (type_donnee) {
    case BOOLEEN: return T_BOOLEEN;
    case ENTIER: return T_REEL; // T_ENTIER; TODO
    case REEL: return T_REEL;
    //case REEL1: return T_REEL;
    //case REEL2: return T_REEL;
    //case REEL3: return T_REEL;
    case DATE_JJMMAAAA: return T_DATE;
    case DATE_MMAAAA: return T_DATE;
    case DATE_AAAA: return T_DATE;
    case DATE_JJMM: return T_DATE;
    case DATE_MM: return T_DATE;
    default:
      fprintf(stderr, "Type %8lX invalide\n", type_donnee);
      exit(1);
      return -1;
  }
}

static nature_t convert_nature(int nat_code)
{
  switch (nat_code) {
    case -1: return N_INDEFINIE;
    case 0: return N_REVENU;
    case 1: return N_CHARGE;
    default:
      fprintf(stderr, "Code nature %d invalide\n", nat_code);
      exit(1);
      return -1;
  }
}

static void add_var_to_index(const char *code, var_t *var)
{
  if ((code == NULL) || (var == NULL)) {
    fprintf(stderr, "Invalid argument to add_var_to_index");
    exit(1);
  }
  if (var_index == NULL) {
    var_index_entries = 0;
    var_index_size = 1024;
    var_index = calloc(var_index_size, sizeof(var_entry_t));
  } else {
    if (var_index_entries >= var_index_size) {
      var_index_size += var_index_size / 4;
      var_index = realloc(var_index, var_index_size * sizeof(var_entry_t));
    }
  }
  var_index[var_index_entries].code = code;
  var_index[var_index_entries].var = var;
  var_index_entries++;
}

static void sort_index_aux(int first, int last)
{
   int i, j, pivot;
   var_entry_t temp;
   if (first < last) {
     pivot = first;
     i = first;
     j = last;
     while (i < j) {
       while (strcmp(var_index[i].code, var_index[pivot].code) <= 0 && i < last)
         i++;
       while (strcmp(var_index[j].code, var_index[pivot].code) > 0)
         j--;
       if (i < j) {
         temp = var_index[i];
         var_index[i] = var_index[j];
         var_index[j] = temp;
       }
     }
     temp = var_index[pivot];
     var_index[pivot] = var_index[j];
     var_index[j] = temp;
     sort_index_aux(first, j - 1);
     sort_index_aux(j + 1, last);
   }
}

static void sort_index(void)
{
  sort_index_aux(0, var_index_entries - 1);
}

static void init_var_dict_debug(T_desc_debug desc_debug[], int nb_debug)
{
  for (size_t i = 0; i < nb_debug; ++i) {
    genre_t genre = convert_genre(desc_debug[i].indice);
    size_t id = desc_debug[i].indice & INDICE_VAL;
    if (genre == G_CALCULEE) id += TAILLE_SAISIE;
    else if (genre == G_BASE) id += TAILLE_SAISIE + TAILLE_CALCULEE;
    if (genre == G_SAISIE) {
      if (var[id].desc == NULL) {
        fprintf(stderr,
                "Variable saisie à l'indice %ld sans définition (%s)\n",
                id, desc_debug[i].nom);
        exit(1);
      }
      if (var[id].alias == NULL) {
        fprintf(stderr,
                "Variable saisie à l'indice %ld sans alias (%s)\n",
                id, desc_debug[i].nom);
        exit(1);
      }
      if (strcmp(var[id].alias, desc_debug[i].nom) != 0) {
        var[id].code = desc_debug[i].nom;
        add_var_to_index(var[id].code, &var[id]);
      }
    } else {
      if (var[id].desc != NULL) {
        fprintf(stderr,
                "Variable base/calculée à l'indice %ld existe déjà (%s/%s)\n",
                id, var[id].code, desc_debug[i].nom);
        exit(1);
      }
      var[id].code = desc_debug[i].nom;
      var[id].alias = NULL;
      var[id].genre = genre;
      var[id].domaine = D_INDEFINI;
      var[id].type = convert_type(desc_debug[i].type_donnee);
      var[id].nature = convert_nature(desc_debug[i].nat_code);
      var[id].classe = desc_debug[i].classe;
      var[id].cat_tl = desc_debug[i].categorie_TL;
      var[id].cot_soc = desc_debug[i].cotsoc;
      var[id].ind_abat = desc_debug[i].ind_abat != 0;
      var[id].rap_cat = desc_debug[i].rapcat;
      var[id].sanction = desc_debug[i].sanction;
      var[id].cat_1731b = -1;
      var[id].indice_tab = -1;
      var[id].acompte = false;
      var[id].avfisc = -1;
      var[id].restituee = false;
      var[id].desc = (T_desc_var *)&desc_debug[i];
      add_var_to_index(var[id].code, &var[id]);
    }
  }
}

static void init_var_dict(void)
{
  if (var_chargees == true) {
    return;
  }
  var_chargees = true;

  //printf("Chargement des variables contexte\n");
  for (size_t i = 0; i < NB_CONTEXTE; ++i) {
    size_t id = desc_contexte[i].indice & INDICE_VAL;
    if (var[id].desc != NULL) {
      fprintf(stderr,
              "Variable saisie (contexte) à l'indice %ld existe déjà (%s/%s)\n",
              id, var[id].code, desc_contexte[i].nom);
      exit(1);
    }
    var[id].code = NULL;
    var[id].alias = desc_contexte[i].nom;
    var[id].genre = G_SAISIE;
    var[id].domaine = D_CONTEXTE;
    var[id].type = convert_type(desc_contexte[i].type_donnee);
    var[id].nature = convert_nature(desc_contexte[i].modcat); // n'existe pas pour ce domaine
    var[id].classe = desc_contexte[i].classe;
    var[id].cat_tl = desc_contexte[i].categorie_TL;
    var[id].cot_soc = -1;
    var[id].ind_abat = false;
    var[id].rap_cat = -1;
    var[id].sanction = -1;
    var[id].cat_1731b = -1;
    var[id].indice_tab = -1;
    var[id].acompte = false;
    var[id].avfisc = -1;
    var[id].restituee = false;
    var[id].desc = (T_desc_var *)&desc_contexte[i];
    add_var_to_index(var[id].alias, &var[id]);
  }

  //printf("Chargement des variables famille\n");
  for (size_t i = 0; i < NB_FAMILLE; ++i) {
    size_t id = desc_famille[i].indice & INDICE_VAL;
    if (var[id].desc != NULL) {
      fprintf(stderr,
              "Variable saisie (famille) à l'indice %ld existe déjà (%s/%s)\n",
              id, var[id].code, desc_famille[i].nom);
      exit(1);
    }
    var[id].code = NULL;
    var[id].alias = desc_famille[i].nom;
    var[id].genre = G_SAISIE;
    var[id].domaine = D_FAMILLE;
    var[id].type = convert_type(desc_famille[i].type_donnee);
    var[id].nature = convert_nature(desc_famille[i].nat_code);
    var[id].classe = desc_famille[i].classe;
    var[id].cat_tl = desc_famille[i].categorie_TL;
    var[id].cot_soc = -1;
    var[id].ind_abat = false;
    var[id].rap_cat = -1;
    var[id].sanction = -1;
    var[id].cat_1731b = -1;
    var[id].indice_tab = -1;
    var[id].acompte = false;
    var[id].avfisc = -1;
    var[id].restituee = false;
    var[id].desc = (T_desc_var *)&desc_famille[i];
    add_var_to_index(var[id].alias, &var[id]);
  }

  //printf("Chargement des variables revenu\n");
  for (size_t i = 0; i < NB_REVENU; ++i) {
    size_t id = desc_revenu[i].indice & INDICE_VAL;
    if (var[id].desc != NULL) {
      fprintf(stderr,
              "Variable saisie (revenu) à l'indice %ld existe déjà (%s/%s)\n",
              id, var[id].code, desc_revenu[i].nom);
      exit(1);
    }
    var[id].code = NULL;
    var[id].alias = desc_revenu[i].nom;
    var[id].genre = G_SAISIE;
    var[id].domaine = D_REVENU;
    var[id].type = convert_type(desc_revenu[i].type_donnee);
    var[id].nature = convert_nature(desc_revenu[i].nat_code);
    var[id].classe = desc_revenu[i].classe;
    var[id].cat_tl = desc_revenu[i].categorie_TL;
    var[id].cot_soc = desc_revenu[i].cotsoc;
    var[id].ind_abat = desc_revenu[i].ind_abat != 0;
    var[id].rap_cat = desc_revenu[i].rapcat;
    var[id].sanction = desc_revenu[i].sanction;
    var[id].cat_1731b = -1;
    var[id].indice_tab = -1;
    var[id].acompte = desc_revenu[i].acompte != 0;
    var[id].avfisc = desc_revenu[i].avfisc;
    var[id].restituee = false;
    var[id].desc = (T_desc_var *)&desc_revenu[i];
    add_var_to_index(var[id].alias, &var[id]);
  }

  //printf("Chargement des variables revcor\n");
  for (size_t i = 0; i < NB_REVENU_CORREC; ++i) {
    size_t id = desc_revenu_correc[i].indice & INDICE_VAL;
    if (var[id].desc != NULL) {
      fprintf(stderr,
              "Variable saisie (revcor) à l'indice %ld existe déjà (%s/%s)\n",
              id, var[id].code, desc_revenu_correc[i].nom);
      exit(1);
    }
    var[id].code = NULL;
    var[id].alias = desc_revenu_correc[i].nom;
    var[id].genre = G_SAISIE;
    var[id].domaine = D_REVENU_CORR;
    var[id].type = convert_type(desc_revenu_correc[i].type_donnee);
    var[id].nature = convert_nature(desc_revenu_correc[i].nat_code);
    var[id].classe = desc_revenu_correc[i].classe;
    var[id].cat_tl = desc_revenu_correc[i].categorie_TL;
    var[id].cot_soc = desc_revenu_correc[i].cotsoc;
    var[id].ind_abat = desc_revenu_correc[i].ind_abat != 0;
    var[id].rap_cat = desc_revenu_correc[i].rapcat;
    var[id].sanction = desc_revenu_correc[i].sanction;
    var[id].cat_1731b = -1;
    var[id].indice_tab = -1;
    var[id].acompte = desc_revenu_correc[i].acompte != 0;
    var[id].avfisc = desc_revenu_correc[i].avfisc;
    var[id].restituee = false;
    var[id].desc = (T_desc_var *)&desc_revenu_correc[i];
    add_var_to_index(var[id].alias, &var[id]);
  }

  //printf("Chargement des variables variation\n");
  for (size_t i = 0; i < NB_VARIATION; ++i) {
    size_t id = desc_variation[i].indice & INDICE_VAL;
    if (var[id].desc != NULL) {
      fprintf(stderr,
              "Variable saisie (revenucor) à l'indice %ld existe déjà (%s/%s)\n",
              id, var[id].code, desc_variation[i].nom);
      exit(1);
    }
    var[id].code = NULL;
    var[id].alias = desc_variation[i].nom;
    var[id].genre = G_SAISIE;
    var[id].domaine = D_VARIATION;
    var[id].type = convert_type(desc_variation[i].type_donnee);
    var[id].nature = convert_nature(-1); // does not exist fot this domain
    var[id].classe = desc_variation[i].classe;
    var[id].cat_tl = -1;
    var[id].cot_soc = -1;
    var[id].ind_abat = -1;
    var[id].rap_cat = -1;
    var[id].sanction = -1;
    var[id].cat_1731b = -1;
    var[id].indice_tab = -1;
    var[id].acompte = false;
    var[id].avfisc = -1;
    var[id].restituee = false;
    var[id].desc = (T_desc_var *)&desc_variation[i];
    add_var_to_index(var[id].alias, &var[id]);
  }

  //printf("Chargement des variables penalite\n");
  for (size_t i = 0; i < NB_PENALITE; ++i) {
    size_t id = desc_penalite[i].indice & INDICE_VAL;
    if (var[id].desc != NULL) {
      fprintf(stderr,
              "Variable saisie (revenucor) à l'indice %ld existe déjà (%s/%s)\n",
              id, var[id].code, desc_penalite[i].nom);
      exit(1);
    }
    var[id].code = NULL;
    var[id].alias = desc_penalite[i].nom;
    var[id].genre = G_SAISIE;
    var[id].domaine = D_PENALITE;
    var[id].type = convert_type(desc_penalite[i].type_donnee);
    var[id].nature = convert_nature(-1); // N'existe pas pour ce domaine
    var[id].classe = -1;
    var[id].cat_tl = -1;
    var[id].cot_soc = -1;
    var[id].ind_abat = -1;
    var[id].rap_cat = -1;
    var[id].sanction = -1;
    var[id].cat_1731b = -1;
    var[id].indice_tab = -1;
    var[id].acompte = false;
    var[id].avfisc = -1;
    var[id].restituee = false;
    var[id].desc = (T_desc_var *)&desc_penalite[i];
    add_var_to_index(var[id].alias, &var[id]);
  }

  //printf("Chargement des variables calculée/base\n");
  init_var_dict_debug(desc_debug01, NB_DEBUG01);
#if NB_DEBUG_C >= 2
  init_var_dict_debug(desc_debug02, NB_DEBUG02);
#endif
#if NB_DEBUG_C >= 3
  init_var_dict_debug(desc_debug03, NB_DEBUG03);
#endif
#if NB_DEBUG_C >= 4
  init_var_dict_debug(desc_debug04, NB_DEBUG04);
#endif

  //printf("Chargement des variables restituées\n");
  for (size_t i = 0; i < NB_RESTITUEE; ++i) {
    genre_t genre = convert_genre(desc_restituee[i].indice);
    size_t id = desc_restituee[i].indice & INDICE_VAL;
    if (genre == G_CALCULEE) id += TAILLE_SAISIE;
    else if (genre == G_BASE) id += TAILLE_SAISIE + TAILLE_CALCULEE;
    if (var[id].desc == NULL) {
      fprintf(stderr,
              "Variable restituée à l'indice %ld sans définition (%s)\n",
              id, desc_restituee[i].nom);
      exit(1);
    }
    var[id].restituee = true;
  }

  //printf("Ajustement des tableaux\n");
  size_t i = 0;
  while (i < TAILLE_TOTALE) {
    size_t j = i + 1;
    while ((j < TAILLE_TOTALE) && (var[j].code == NULL)) {
      memcpy(&var[j], &var[i], sizeof(var_t));
      var[j].indice_tab = (j - i);
      ++j;
    }
    if ((j - i) > 1) {
      var[i].indice_tab = 0;
      //printf("Tableau %s de taille %ld (indices %ld-%ld)\n", var[i].code, (j - i), i, j - 1);
    }
    i = j;
  }

  //printf("Chargement des variables terminé\n");

  sort_index();
}

static var_t *
cherche_var(
  const char *code)
{
  var_entry_t var_entry;
  int res = -1, inf = 0, sup = var_index_entries, millieu;
  while ((res != 0) && (inf < sup)) {
    millieu = (inf + sup) / 2;
    var_entry = var_index[millieu];
    res = strcmp(code, var_entry.code);
    if (res < 0) sup = millieu;
    else if (res > 0) inf = millieu + 1;
  }
  if (res == 0)
    return var_entry.var;
  else
    return NULL;

  return NULL;
}

CAMLprim value
ml_charge_vars(void)
{
  CAMLparam0();
  CAMLlocal4(mlListTemp, mlListOut, mlTemp, mlTemp2);

  init_var_dict();

  mlListOut = Val_emptylist;
  size_t nb_vars = sizeof(var) / ((void *)(var + 1) - (void *)var);
  for (size_t i = 0; i < nb_vars; ++i) {
    if (var[i].code == NULL) {
      fprintf(stderr, "Code indéfini indice %ld\n", i);
      exit(1);
    } else {
      mlTemp = caml_alloc_tuple(16);
      Store_field(mlTemp, 0, caml_copy_string(var[i].code));
      if (var[i].alias == NULL) mlTemp2 = Val_none;
      else mlTemp2 = caml_alloc_some(caml_copy_string(var[i].alias));
      Store_field(mlTemp, 1, mlTemp2);
      Store_field(mlTemp, 2, Val_int(var[i].genre));
      Store_field(mlTemp, 3, Val_int(var[i].domaine));
      Store_field(mlTemp, 4, Val_int(var[i].type));
      Store_field(mlTemp, 5, Val_int(var[i].nature));
      Store_field(mlTemp, 6, Val_int(var[i].classe));
      Store_field(mlTemp, 7, Val_int(var[i].cat_tl));
      Store_field(mlTemp, 8, Val_int(var[i].cot_soc));
      Store_field(mlTemp, 9, Val_bool(var[i].ind_abat));
      Store_field(mlTemp, 10, Val_int(var[i].rap_cat));
      Store_field(mlTemp, 11, Val_int(var[i].sanction));
      Store_field(mlTemp, 12, Val_int(var[i].indice_tab));
      Store_field(mlTemp, 13, Val_bool(var[i].acompte));
      Store_field(mlTemp, 14, Val_int(var[i].avfisc));
      Store_field(mlTemp, 15, Val_bool(var[i].restituee));
      mlListTemp = caml_alloc_small(2, Tag_cons);
      Field(mlListTemp, 0) = mlTemp;
      Field(mlListTemp, 1) = mlListOut;
      mlListOut = mlListTemp;
    }
  }

  CAMLreturn(mlListOut);
}

CAMLprim value
ml_tgv_defined(value mlTgv, value mlCode)
{
  CAMLparam2(mlTgv,mlCode);

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *code = String_val(mlCode);
  var_t *var = cherche_var(code);
  if (var == NULL) {
    fprintf(stderr, "La variable %s n'existe pas (alias ?)\n", code);
    exit(1);
  }
  double *montant = IRDATA_extrait_special(tgv, var->desc);

  CAMLreturn(Val_int(montant != NULL));
}

CAMLprim value
ml_tgv_reset(value mlTgv, value mlCode)
{
  CAMLparam2(mlTgv,mlCode);

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *code = String_val(mlCode);
  var_t *var = cherche_var(code);
  if (var == NULL) {
    fprintf(stderr, "La variable %s n'existe pas (alias ?)\n", code);
    exit(1);
  }
  IRDATA_efface(tgv, var->desc);

  CAMLreturn(Val_unit);
}

CAMLprim value
ml_tgv_get(value mlTgv, value mlCode)
{
  CAMLparam2(mlTgv,mlCode);
  CAMLlocal1(optOut);

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *code = String_val(mlCode);
  var_t *var = cherche_var(code);
  if (var == NULL) {
    fprintf(stderr, "La variable %s n'existe pas (alias ?)\n", code);
    exit(1);
  }
  double *montant = IRDATA_extrait_special(tgv, var->desc);
  if(montant == NULL) optOut = Val_none;
  else optOut = caml_alloc_some(caml_copy_double(*montant));

  CAMLreturn(optOut);
}

CAMLprim value
ml_tgv_get_array(value mlTgv, value mlCode, value mlIdx)
{
  CAMLparam3(mlTgv,mlCode, mlIdx);
  CAMLlocal1(optOut);

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *code = String_val(mlCode);
  int idx = Int_val(mlIdx);
  var_t *var = cherche_var(code);
  if (var == NULL) {
    fprintf(stderr, "La variable %s n'existe pas (alias ?)\n", code);
    exit(1);
  }
  double *montant = IRDATA_extrait_tableau(tgv, var->desc, idx);
  if(montant == NULL) optOut = Val_none;
  else optOut = caml_alloc_some(*montant);

  CAMLreturn(optOut);
}

CAMLprim value
ml_tgv_set(value mlTgv, value mlCode, value mlMontant)
{
  CAMLparam3(mlTgv, mlCode, mlMontant);

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *code = String_val(mlCode);
  double montant = Double_val(mlMontant);
  var_t *var = cherche_var(code);
  if (var == NULL) {
    fprintf(stderr, "La variable %s n'existe pas (alias ?)\n", code);
    exit(1);
  }
  IRDATA_range_base(tgv, var->desc, montant);

  CAMLreturn(Val_unit);
}

CAMLprim value
ml_tgv_reset_calculee(value mlTgv)
{
  CAMLparam1(mlTgv);
  T_irdata *tgv = Tgv_val(mlTgv);
  IRDATA_reset_calculee(tgv);
  CAMLreturn(Val_unit);
}

CAMLprim value
ml_tgv_reset_saisie_calculee(value mlTgv)
{
  CAMLparam1(mlTgv);
  T_irdata *tgv = Tgv_val(mlTgv);
  IRDATA_reset_light(tgv);
  CAMLreturn(Val_unit);
}

CAMLprim value
ml_tgv_reset_base(value mlTgv)
{
  CAMLparam1(mlTgv);
  T_irdata *tgv = Tgv_val(mlTgv);
  IRDATA_reset_base(tgv);
  CAMLreturn(Val_unit);
}

CAMLprim value
ml_tgv_copy(value mlSTgv, value mlDTgv)
{
  CAMLparam2(mlSTgv, mlDTgv);

  T_irdata *stgv = Tgv_val(mlSTgv);
  T_irdata *dtgv = Tgv_val(mlDTgv);
  IRDATA_recopie_irdata(stgv, dtgv);
  CAMLreturn(Val_unit);
}

CAMLprim value
ml_exec_ench(
  value mlEnch,
  value mlTgv)
{
  CAMLparam2(mlEnch, mlTgv);

  init_var_dict();

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *ench = String_val(mlEnch);

  size_t nb_ench = sizeof(enchaineurs) / ((void *)(enchaineurs + 1) - (void *)enchaineurs);
  size_t i = 0;
  while (i < nb_ench) {
    if (strcmp(ench, enchaineurs[i].name) == 0)
      break;
    ++i;
  }
  if (i >= nb_ench) {
    fprintf(stderr, "L'enchaineur %s n'existe pas\n", ench);
    exit(1);
  }

  enchaineurs[i].function(tgv);

  CAMLreturn(Val_unit);
}

CAMLprim value
ml_exec_verif(
  value mlVerif,
  value mlTgv)
{
  CAMLparam2(mlVerif, mlTgv);
  CAMLlocal2(mlErrListTemp, mlErrListOut);

  init_var_dict();

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *verif = String_val(mlVerif);

  size_t nb_verif = sizeof(verifications) / ((void *)(verifications + 1) - (void *)verifications);
  size_t i = 0;
  while (i < nb_verif) {
    if (strcmp(verif, verifications[i].name) == 0)
      break;
    ++i;
  }
  if (i >= nb_verif) {
    fprintf(stderr, "La verification %s n'existe pas\n", verif);
    exit(1);
  }

  struct S_discord * erreurs = verifications[i].function(tgv);

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

CAMLprim value
ml_annee_calc(void)
{
  CAMLparam0();
  CAMLreturn(Val_int(ANNEE_REVENU));
}





static void
dump_vars(
  void)
{
  static bool dumped = false;

  if (dumped == true) {
    return;
  }
  dumped = true;

  size_t nb_vars = sizeof(var) / ((void *)(var + 1) - (void *)var);

  char *dump = calloc(nb_vars, 32);

  for (size_t i = 0; i < nb_vars; ++i) {
    if (var[i].code != NULL) {

      int indice = var[i].desc->indice & INDICE_VAL;
      if (var[i].indice_tab > 0) indice += var[i].indice_tab;
      switch (var[i].desc->indice & EST_MASQUE) {
        case EST_SAISIE: break;
        case EST_CALCULEE: indice += TAILLE_SAISIE; break;
        case EST_BASE: indice += TAILLE_SAISIE + TAILLE_CALCULEE; break;
        default: fprintf(stderr, "Code indéfini indice %ld\n", i); exit(1);
      }

      if (var[i].indice_tab < 0) {
        snprintf(&dump[indice * 32], 32, "%05x %s", indice * 8, var[i].code);
      } else {
        snprintf(&dump[indice * 32], 32, "%05x %s[%d]", indice * 8,
                 var[i].code, var[i].indice_tab);
      }

    } else {
      fprintf(stderr, "Code indéfini indice %ld\n", i);
      exit(1);
    }
  }

  for (int i = 0; i < nb_vars * 32; ++i) {
    if (dump[i] == '\0') {
      dump[i] = ' ';
    }
  }
  for (int i = 0; i < nb_vars * 32; i += 32) {
    dump[i+31] = '\n';
  }

  FILE *f = fopen("vars.txt", "wb");
  fwrite(dump, 32, nb_vars, f);
  fclose(f);

  free(dump);
}

static void
dump_array(
  char *def,
  double *val,
  size_t size,
  FILE *f)
{
  static const char undef[16] =
    { 0xDE, 0xAD, 0xBE, 0xEF, 0xDE, 0xAD, 0xBE, 0xEF,
      0xDE, 0xAD, 0xBE, 0xEF, 0xDE, 0xAD, 0xBE, 0xEF };
  for (size_t i = 0; i < size; ++i) {
    if (def[i]) {
      fwrite(&val[i], sizeof(double), 1, f);
    } else {
      fwrite(undef, 1, sizeof(double), f);
    }
  }
}

CAMLprim value
ml_dump_raw_tgv(
  value mlFilename,
  value mlTgv,
  value mlErrList)
{
  CAMLparam3(mlFilename, mlTgv, mlErrList);
  CAMLlocal1(mlErrListTemp);

  init_var_dict();
  dump_vars();

  const char *filename = String_val(mlFilename);
  T_irdata *tgv = Tgv_val(mlTgv);

  const char undef[8] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xDE, 0xAD, 0xBE, 0xEF };
  FILE *f = fopen(filename, "wb");
  if (f == NULL) {
    printf("Can't open file %s\n", filename);
    CAMLreturn(Val_unit);
  }
  dump_array(tgv->def_saisie, tgv->saisie, TAILLE_SAISIE, f);
  dump_array(tgv->def_calculee, tgv->calculee, TAILLE_CALCULEE, f);
  dump_array(tgv->def_base, tgv->base, TAILLE_BASE, f);

  mlErrListTemp = mlErrList;
  while (mlErrListTemp != Val_emptylist) {
    const char *err = String_val(Field(mlErrListTemp, 0));
    char data[32] = { 0 };
    memcpy(data, err, strlen(err));
    fwrite(data, sizeof(char), 8, f);
    mlErrListTemp = Field(mlErrListTemp, 1);
  }

  fclose(f);

  CAMLreturn(Val_unit);
}
