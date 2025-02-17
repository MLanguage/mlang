(* Copyright (C) 2019 Inria, contributor: David Declerck
   <david.declerck@ocamlpro.com>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

let open_file filename =
  let folder = Filename.dirname !Cli.output_file in
  let oc = open_out (Filename.concat folder filename) in
  let fmt = Format.formatter_of_out_channel oc in
  (oc, fmt)

let gen_table_varinfo vars cat Com.CatVar.{ id_int; id_str; attributs; _ }
    (stats, var_map) =
  let oc, fmt = open_file (Pp.spr "varinfo_%s.c" id_str) in
  Format.fprintf fmt {|/****** LICENCE CECIL *****/

#include "mlang.h"

|};
  Format.fprintf fmt "T_varinfo_%s varinfo_%s[NB_%s + 1] = {\n" id_str id_str
    id_str;
  let nb, var_map =
    StrMap.fold
      (fun _ var (nb, var_map) ->
        if Com.CatVar.compare (Com.Var.cat var) cat = 0 then (
          let loc_tgv = Com.Var.loc_tgv var in
          let name = Com.Var.name_str var in
          let alias = Com.Var.alias_str var in
          let idx = loc_tgv.loc_idx in
          let size = Com.Var.size var in
          let loc_cat =
            match loc_tgv.loc_cat with
            | Com.CatVar.LocComputed -> "EST_CALCULEE"
            | Com.CatVar.LocBase -> "EST_BASE"
            | Com.CatVar.LocInput -> "EST_SAISIE"
          in
          let attrs = Com.Var.attrs var in
          Format.fprintf fmt "  { \"%s\", \"%s\", %d, %d, %d, %s" name alias idx
            size id_int loc_cat;
          StrMap.iter
            (fun _ av -> Format.fprintf fmt ", %d" (Pos.unmark av))
            attrs;
          Format.fprintf fmt " },\n";
          let var_addr =
            Format.sprintf "(T_varinfo *)&(varinfo_%s[%d])" id_str nb
          in
          let var_map = StrMap.add (Com.Var.name_str var) var_addr var_map in
          let var_map =
            match Com.Var.alias var with
            | None -> var_map
            | Some m_alias -> StrMap.add (Pos.unmark m_alias) var_addr var_map
          in
          (nb + 1, var_map))
        else (nb, var_map))
      vars (0, var_map)
  in
  Format.fprintf fmt "  NULL\n};\n\n";
  close_out oc;
  let attr_set =
    StrMap.fold (fun an _ res -> StrSet.add an res) attributs StrSet.empty
  in
  (Com.CatVar.Map.add cat (id_str, id_int, nb, attr_set) stats, var_map)

let gen_table_varinfos (cprog : Mir.program) flags =
  let stats_varinfos, var_map =
    Com.CatVar.Map.fold
      (gen_table_varinfo cprog.program_vars)
      cprog.program_var_categories
      (Com.CatVar.Map.empty, StrMap.empty)
  in
  let oc, fmt = open_file "varinfos.c" in
  Format.fprintf fmt {|/****** LICENCE CECIL *****/

#include "mlang.h"

|};
  let attrs =
    Com.CatVar.Map.fold
      (fun _ Com.CatVar.{ attributs; _ } res ->
        StrMap.fold (fun attr _ res -> StrSet.add attr res) attributs res)
      cprog.program_var_categories StrSet.empty
  in
  StrSet.iter
    (fun attr ->
      Pp.fpr fmt "char attribut_%s_def(T_varinfo *vi) {\n" attr;
      Pp.fpr fmt "  if (vi == NULL) return 0;\n";
      Pp.fpr fmt "  switch (vi->cat) {\n";
      Com.CatVar.Map.iter
        (fun _ Com.CatVar.{ id_str; attributs; _ } ->
          if StrMap.mem attr attributs then
            Pp.fpr fmt "    case ID_%s: return 1;\n" id_str)
        cprog.program_var_categories;
      Pp.fpr fmt "  }\n";
      Pp.fpr fmt "  return 0;\n";
      Pp.fpr fmt "}\n\n";
      Pp.fpr fmt "double attribut_%s(T_varinfo *vi) {\n" attr;
      Pp.fpr fmt "  if (vi == NULL) return 0.0;\n";
      Pp.fpr fmt "  switch (vi->cat) {\n";
      Com.CatVar.Map.iter
        (fun _ Com.CatVar.{ id_str; attributs; _ } ->
          if StrMap.mem attr attributs then (
            Pp.fpr fmt "    case ID_%s:\n" id_str;
            Pp.fpr fmt "      return ((T_varinfo_%s *)vi)->attr_%s;\n" id_str
              attr))
        cprog.program_var_categories;
      Pp.fpr fmt "  }\n";
      Pp.fpr fmt "  return 0.0;\n";
      Pp.fpr fmt "}\n\n")
    attrs;
  if flags.Dgfip_options.flg_gcos then
    Format.fprintf fmt "T_varinfo_map varinfo[1] = {NULL};\n\n"
  else (
    Format.fprintf fmt
      "T_varinfo_map varinfo[NB_variable + NB_saisie + 1] = {\n";
    StrMap.iter (Format.fprintf fmt "  { \"%s\", %s },\n") var_map;
    Format.fprintf fmt "  NULL\n};\n\n");
  close_out oc;
  stats_varinfos

let gen_decl_varinfos fmt (cprog : Mir.program) stats =
  Format.fprintf fmt
    {|typedef struct S_varinfo {
  char *name;
  char *alias;
  int idx;
  int size;
  int cat;
  int loc_cat;
} T_varinfo;

typedef struct S_varinfo_map {
  char *name;
  T_varinfo *info;
} T_varinfo_map;

|};
  Com.CatVar.Map.iter
    (fun _ (id_str, _, _, attr_set) ->
      Format.fprintf fmt
        {|typedef struct S_varinfo_%s {
  char *name;
  char *alias;
  int idx;
  int size;
  int cat;
  int loc_cat;
|}
        id_str;
      StrSet.iter (fun an -> Format.fprintf fmt "  int attr_%s;\n" an) attr_set;
      Format.fprintf fmt "} T_varinfo_%s;\n\n" id_str)
    stats;
  Format.fprintf fmt "\n";
  Com.CatVar.Map.iter
    (fun _ (id_str, _, _, _) ->
      Format.fprintf fmt "extern T_varinfo_%s varinfo_%s[];\n" id_str id_str)
    stats;
  Format.fprintf fmt "extern T_varinfo_map varinfo[];\n";
  Format.fprintf fmt "\n";
  Com.CatVar.Map.iter
    (fun _ (id_str, _, nb, _) ->
      Format.fprintf fmt "#define NB_%s %d\n" id_str nb)
    stats;
  let nb_saisie, nb_variable =
    Com.CatVar.Map.fold
      (fun cat (_, _, nb, _) (nb_saisie, nb_variable) ->
        let nb_variable = nb + nb_variable in
        match cat with
        | Input _ -> (nb + nb_saisie, nb_variable)
        | _ -> (nb_saisie, nb_variable))
      stats (0, 0)
  in
  Format.fprintf fmt "#define NB_saisie %d\n" nb_saisie;
  Format.fprintf fmt "#define NB_variable %d\n" nb_variable;
  Format.fprintf fmt "\n";
  let id_tmp =
    Com.CatVar.Map.fold
      (fun _ (id_str, id_int, _, _) id_tmp ->
        Format.fprintf fmt "#define ID_%s %d\n" id_str id_int;
        max (id_int + 1) id_tmp)
      stats (-1)
  in
  Format.fprintf fmt "#define ID_TMP_VARS %d\n" id_tmp;

  let attrs =
    Com.CatVar.Map.fold
      (fun _ Com.CatVar.{ attributs; _ } res ->
        StrMap.fold (fun attr _ res -> StrSet.add attr res) attributs res)
      cprog.program_var_categories StrSet.empty
  in
  StrSet.iter
    (fun attr ->
      Format.fprintf fmt "\nextern char attribut_%s_def(T_varinfo *vi);\n" attr;
      Format.fprintf fmt "extern double attribut_%s(T_varinfo *vi);\n" attr)
    attrs

let is_valid_app apps =
  StrMap.exists (fun app _ -> List.mem app !Cli.application_names) apps

let gen_erreurs_c fmt flags (cprog : Mir.program) =
  Format.fprintf fmt {|/****** LICENCE CECIL *****/

#include "mlang.h"

|};
  (* TODO before 2006, the format is slightly different *)
  StrMap.iter
    (fun _ (e : Com.Error.t) ->
      let terr =
        match e.typ with Anomaly -> 1 | Discordance -> 2 | Information -> 4
      in
      let sous_code_suffix =
        if String.equal (Pos.unmark e.sous_code) "00" then ""
        else "-" ^ Pos.unmark e.sous_code
      in
      Format.fprintf fmt
        "T_erreur erreur_%s = { \"%s%s%s / %s\", \"%s\", \"%s\", \"%s\", \
         \"%s\", %d };\n"
        (Pos.unmark e.name) (Pos.unmark e.famille) (Pos.unmark e.code_bo)
        sous_code_suffix (Pos.unmark e.libelle) (Pos.unmark e.code_bo)
        (Pos.unmark e.sous_code) (Pos.unmark e.is_isf) (Pos.unmark e.name) terr)
    cprog.program_errors;

  if flags.Dgfip_options.flg_pro || flags.flg_iliad then begin
    Format.fprintf fmt "T_erreur *tabErreurs[] = {\n";

    StrMap.iter
      (fun _ (e : Com.Error.t) ->
        Format.fprintf fmt "    &erreur_%s,\n" (Pos.unmark e.name))
      cprog.program_errors;

    Format.fprintf fmt "    NULL\n};\n"
  end

(* Print #defines corresponding to generation options *)
let gen_conf_h fmt (cprog : Mir.program) flags =
  let open Dgfip_options in
  Format.fprintf fmt
    {|/****** LICENCE CECIL *****/

#ifndef _CONF_H_
#define _CONF_H_

|};
  if flags.flg_correctif then Format.fprintf fmt "#define FLG_CORRECTIF\n";
  if flags.flg_iliad then Format.fprintf fmt "#define FLG_ILIAD\n";
  if flags.flg_pro then Format.fprintf fmt "#define FLG_PRO\n";
  if flags.flg_cfir then Format.fprintf fmt "#define FLG_CFIR\n";
  if flags.flg_gcos then Format.fprintf fmt "#define FLG_GCOS\n";
  if flags.flg_tri_ebcdic then Format.fprintf fmt "#define FLG_TRI_EBCDIC\n";
  (* flag is not used *)
  if flags.flg_short then
    Format.fprintf fmt "#define FLG_SHORT /* inutile ? */\n";
  if flags.flg_register then Format.fprintf fmt "#define FLG_REGISTER\n";
  (* flag is not used *)
  if flags.flg_optim_min_max then
    Format.fprintf fmt "#define FLG_OPTIM_MIN_MAX /* inutile ? */\n";
  if flags.flg_extraction then Format.fprintf fmt "#define FLG_EXTRACTION\n";
  if flags.flg_genere_libelle_restituee then
    Format.fprintf fmt "#define FLG_GENERE_LIBELLE_RESTITUEE\n";
  if flags.flg_controle_separe then
    Format.fprintf fmt "#define FLG_CONTROLE_SEPARE\n";
  if flags.flg_controle_immediat then
    Format.fprintf fmt "#define FLG_CONTROLE_IMMEDIAT\n";
  (* does not need to be printed *)
  (*if flags.flg_overlays then Format.fprintf fmt "#define FLG_OVERLAYS\n"; *)
  if flags.flg_colors then Format.fprintf fmt "#define FLG_COLORS\n";
  if flags.flg_ticket then Format.fprintf fmt "#define FLG_TICKET\n";
  if flags.flg_trace then Format.fprintf fmt "#define FLG_TRACE\n";
  (* flag is not used *)
  (*if flags.flg_trace_irdata then Format.fprintf fmt "#define
    FLG_TRACE_IRDATA\n"; *)
  if flags.flg_debug then Format.fprintf fmt "#define FLG_DEBUG\n";
  Format.fprintf fmt "#define NB_DEBUG_C  %d\n" flags.nb_debug_c;
  Format.fprintf fmt "#define EPSILON %f\n" !Cli.comparison_error_margin;
  let count loc =
    StrMap.fold
      (fun _ var nb ->
        nb + if Com.Var.cat_var_loc var = Some loc then Com.Var.size var else 0)
      cprog.program_vars 0
  in
  let nb_saisie = count Com.CatVar.LocInput in
  let nb_calculee = count Com.CatVar.LocComputed in
  let nb_base = count Com.CatVar.LocBase in
  let nb_vars = nb_saisie + nb_calculee + nb_base in
  Format.fprintf fmt "#define NB_VARS  %d\n" nb_vars;
  Format.fprintf fmt {|
#endif /* _CONF_H_ */
|}

let gen_dbg fmt =
  Format.fprintf fmt
    {|int change_couleur(int couleur, int typographie);
int get_couleur();
int get_typo();
    
#ifdef FLG_TRACE
extern int niv_trace;

extern void aff1(char *nom);

extern void aff_val(const char *nom, const T_irdata *irdata, int indice, int niv, const char *chaine, int is_tab, int expr, int maxi);

#define aff2(nom, irdata, indice) aff_val(nom, irdata, indice, 2, "<-", 0, 0, 1)

#define aff3(nom, irdata, indice) aff_val(nom, irdata, indice, 3, ":", 0, 0, 1)
#endif /* FLG_TRACE */
|}

let gen_const fmt (cprog : Mir.program) =
  Format.fprintf fmt
    {|#define FALSE 0
#define TRUE 1

struct S_print_context {
  FILE *std;
  long indent;
  int is_newline;
};

typedef struct S_print_context T_print_context;

typedef void *T_var_irdata;

struct S_erreur {
  char *message;
  char *codebo;
  char *souscode;
  char *isisf;
  char *nom;
  short type;
};

typedef struct S_erreur T_erreur;

typedef struct S_discord T_discord;

struct S_discord {
  T_discord *suivant;
  T_erreur *erreur;
};

typedef struct S_keep_discord T_keep_discord;

struct S_keep_discord {
  T_discord *discord;
  T_keep_discord *suivant;
};

struct S_event {
|};
  IntMap.iter
    (fun _idx fname ->
      let field = StrMap.find fname cprog.program_event_fields in
      if field.is_var then
        Format.fprintf fmt "  T_varinfo *field_%s_var;\n" fname
      else (
        Format.fprintf fmt "  char field_%s_def;\n" fname;
        Format.fprintf fmt "  double field_%s_val;\n" fname))
    cprog.program_event_field_idxs;
  Format.fprintf fmt
    {|};

typedef struct S_event T_event;

struct S_irdata {
  double *saisie;
  double *calculee;
  double *base;
  double *tmps;
  double **ref;
  char *def_saisie;
  char *def_calculee;
  char *def_base;
  char *def_tmps;
  char **def_ref;
  T_varinfo *info_tmps;
  T_varinfo **info_ref;
  int tmps_org;
  int ref_org;
  T_keep_discord *keep_discords;
  T_discord *discords;
  int nb_anos;
  int nb_discos;
  int nb_infos;
  int nb_bloqs;
  int max_bloqs;
  jmp_buf jmp_bloq;
  int sz_err_finalise;
  char **err_finalise;
  int nb_err_finalise;
  int sz_err_sortie;
  char **err_sortie;
  int nb_err_sortie;
  int sz_err_archive;
  char **err_archive;
  int nb_err_archive;
  T_event **events;
  int nb_events;
  T_print_context ctx_pr_out;
  T_print_context ctx_pr_err;
};

typedef struct S_irdata T_irdata;

|};
  StrMap.iter
    (fun f (ef : Com.event_field) ->
      if ef.is_var then
        Pp.fpr fmt
          "extern T_varinfo *event_field_%s_var(T_irdata *irdata, char \
           idx_def, double idx_val);\n"
          f;
      Pp.fpr fmt
        "extern char event_field_%s(T_irdata *irdata, char *res_def, double \
         *res_val, char idx_def, double idx_val);\n"
        f)
    cprog.program_event_fields;
  Format.fprintf fmt
    {|
#define DS_ irdata->def_saisie
#define S_ irdata->saisie

#define DC_ irdata->def_calculee
#define C_ irdata->calculee

#define DB_ irdata->def_base
#define B_ irdata->base

#define I_(cat,idx) ((T_varinfo *)&(varinfo_##cat[idx]))

#define DT_(idx) (irdata->def_tmps[irdata->tmps_org + (idx)])
#define T_(idx) (irdata->tmps[irdata->tmps_org + (idx)])
#define IT_(idx) (&(irdata->info_tmps[irdata->tmps_org + (idx)]))

#define DR_(idx) (irdata->def_ref[irdata->ref_org + (idx)])
#define R_(idx) (irdata->ref[irdata->ref_org + (idx)])
#define IR_(idx) (irdata->info_ref[irdata->ref_org + (idx)])

extern T_event *event(T_irdata *irdata, char idx_def, double idx_val);
extern int size_varinfo(T_varinfo *info, char *res_def, double *res_val);

#define EST_SAISIE     0x00000
#define EST_CALCULEE   0x04000
#define EST_BASE       0x08000
#define EST_TEMPORAIRE 0x10000
#define EST_ARGUMENT   0x20000
#define EST_RESULTAT   0x40000
#define EST_MASQUE     0x3c000
#define INDICE_VAL     0x03fff

#define RESTITUEE    5
#define RESTITUEE_P  6
#define RESTITUEE_C  7

extern void add_erreur(T_irdata *irdata, T_erreur *erreur, char *code);
extern void free_erreur();

#define fabs(a) (((a) < 0.0) ? -(a) : (a))
#define min(a,b)	(((a) <= (b)) ? (a) : (b))
#define max(a,b)	(((a) >= (b)) ? (a) : (b))
|};
  Format.fprintf fmt "#define EPSILON %f" !Cli.comparison_error_margin;
  Format.fprintf fmt
    {|
#define GT_E(a,b) ((a) > (b) + EPSILON)
#define LT_E(a,b) ((a) + EPSILON < (b))
#define GE_E(a,b) ((a) > (b) - EPSILON)
#define LE_E(a,b) ((a) - EPSILON < (b))
#define EQ_E(a,b) (fabs((a) - (b)) < EPSILON)
#define NEQ_E(a,b) (fabs((a) - (b)) >= EPSILON)
#define my_floor(a) (floor_g((a) + EPSILON))
#define my_ceil(a) (ceil_g((a) - EPSILON))
#define my_arr(a) (((a) < 0) ? ceil_g((a) - 0.5 - EPSILON * 50) : floor_g((a) + 0.5 + EPSILON * 50))
#define divd(a,b)	(NEQ_E((b),0.0) ? (a / b) : 0.0)

extern double floor_g(double);
extern double ceil_g(double);
extern int multimax(char *var_def, double *var_val, int size, char nb_def, double nb_val, char *res_def, double *res_val);
extern int multimax_varinfo(T_irdata *irdata, T_varinfo *info, char nb_def, double nb_val, char *res_def, double *res_val);
extern int modulo_def(int, int);
extern double modulo(double, double);
|}

let gen_lib fmt (cprog : Mir.program) flags =
  let count loc =
    StrMap.fold
      (fun _ var nb ->
        nb + if Com.Var.cat_var_loc var = Some loc then Com.Var.size var else 0)
      cprog.program_vars 0
  in
  let taille_saisie = count Com.CatVar.LocInput in
  let taille_calculee = count Com.CatVar.LocComputed in
  let taille_base = count Com.CatVar.LocBase in
  let taille_totale = taille_saisie + taille_calculee + taille_base in
  let nb_ench = StrMap.cardinal cprog.program_chainings in
  let nb_err = StrMap.cardinal cprog.program_errors in
  let nb_call = IntMap.cardinal cprog.program_rules in
  let nb_verif = IntMap.cardinal cprog.program_verifs in

  Format.fprintf fmt
    {|#define TAILLE_SAISIE %d
#define TAILLE_CALCULEE %d
#define TAILLE_BASE %d
#define TAILLE_TOTALE %d
#define NB_ENCH %d

|}
    taille_saisie taille_calculee taille_base taille_totale nb_ench;

  Format.fprintf fmt {|#define TAILLE_TMP_VARS %d
#define TAILLE_REFS %d

|}
    cprog.program_stats.sz_all_tmps cprog.program_stats.nb_all_refs;

  Format.fprintf fmt
    {|#define ANOMALIE     1
#define DISCORDANCE  2
#define INFORMATIVE  4

#define BOOLEEN        0x1
#define ENTIER         0x100
#define REEL           0x200
#define REEL1          0x400
#define REEL2          0x800
#define REEL3          0x1000
#define DATE_JJMMAAAA  0x10000
#define DATE_MMAAAA    0x20000
#define DATE_AAAA      0x40000
#define DATE_JJMM      0x80000
#define DATE_MM        0x100000
#define DATE           (DATE_JJMMAAAA|DATE_MMAAAA|DATE_AAAA|DATE_JJMM|DATE_MM)
#define NUMERIQUE      (ENTIER|REEL|REEL1|REEL2|REEL3)

|};

  Format.fprintf fmt "#define NB_ERR %d\n" nb_err;
  Format.fprintf fmt "#define NB_CALL %d\n" nb_call;
  Format.fprintf fmt "#define NB_VERIF %d\n\n" nb_verif;

  (* TODO external declaration of individual control rules (seems to be no
     longer used) *)
  StrMap.iter
    (fun _ (e : Com.Error.t) ->
      let en = Pos.unmark e.name in
      Format.fprintf fmt "extern T_erreur erreur_%s;\n" en)
    cprog.program_errors;

  (* TODO function declarations (seems to be no longer used) *)
  if flags.Dgfip_options.flg_pro then
    Format.fprintf fmt "extern struct S_erreur *tabErreurs[];\n\n"
  else Format.fprintf fmt "\n";

  Format.fprintf fmt
    {|extern void set_print_indent(FILE *std, T_print_context *pr_ctx, double diff);
extern void print_indent(FILE *std, T_print_context *pr_ctx);
extern void print_string(FILE *std, T_print_context *pr_ctx, char *str);
extern void print_double(FILE *std, T_print_context *pr_ctx, double f, int pmin, int pmax);

typedef struct S_env_sauvegarde {
  char sauv_def;
  double sauv_val;
  char *orig_def;
  double *orig_val;
  struct S_env_sauvegarde *suite;
} T_env_sauvegarde;

typedef struct S_env_sauvegarde_evt {
  T_event sauv_evt;
  T_event *orig_evt;
  struct S_env_sauvegarde_evt *suite;
} T_env_sauvegarde_evt;

extern void env_sauvegarder(T_env_sauvegarde **liste, char *oDef, double *oVal, int sz);
extern void env_restaurer(T_env_sauvegarde **liste);
extern void env_sauvegarder_evt(T_env_sauvegarde_evt **liste, T_event *evt);
extern void env_restaurer_evt(T_env_sauvegarde_evt **liste);
extern int nb_informatives(T_irdata *irdata);
extern int nb_discordances(T_irdata *irdata);
extern int nb_anomalies(T_irdata *irdata);
extern int nb_bloquantes(T_irdata *irdata);
extern void nettoie_erreur _PROTS((T_irdata *irdata ));
extern void finalise_erreur _PROTS((T_irdata *irdata ));
extern void exporte_erreur _PROTS((T_irdata *irdata ));

extern T_irdata *cree_irdata(void);
extern void init_saisie(T_irdata *irdata);
extern void init_calculee(T_irdata *irdata);
extern void init_base(T_irdata *irdata);
extern void init_erreur(T_irdata *irdata);
extern void detruis_irdata(T_irdata *irdata);
extern void set_max_bloquantes(T_irdata *irdata, const int max_ano);
extern void recopie_saisie(T_irdata *irdata_src, T_irdata *irdata_dst);
extern void recopie_calculee(T_irdata *irdata_src, T_irdata *irdata_dst);
extern void recopie_base(T_irdata *irdata_src, T_irdata *irdata_dst);
extern void ecris_saisie(T_irdata *irdata, int idx, char def, double val);
extern void ecris_calculee(T_irdata *irdata, int idx, char def, double val);
extern void ecris_base(T_irdata *irdata, int idx, char def, double val);
extern char lis_saisie_def(T_irdata *irdata, int idx);
extern char lis_calculee_def(T_irdata *irdata, int idx);
extern char lis_base_def(T_irdata *irdata, int idx);
extern double lis_saisie_val(T_irdata *irdata, int idx);
extern double lis_calculee_val(T_irdata *irdata, int idx);
extern double lis_base_val(T_irdata *irdata, int idx);
extern char *lis_saisie_def_ref(T_irdata *irdata, int idx);
extern char *lis_calculee_def_ref(T_irdata *irdata, int idx);
extern char *lis_base_def_ref(T_irdata *irdata, int idx);
extern double *lis_saisie_val_ref(T_irdata *irdata, int idx);
extern double *lis_calculee_val_ref(T_irdata *irdata, int idx);
extern double *lis_base_val_ref(T_irdata *irdata, int idx);
extern T_discord *lis_discords(T_irdata *irdata);
extern T_erreur *lis_erreur(T_discord *discord);
extern T_discord *discord_suivante(T_discord *discord);
extern char *lis_erreur_message(T_erreur *err);
extern char *lis_erreur_code_bo(T_erreur *err);
extern char *lis_erreur_sous_code(T_erreur *err);
extern char *lis_erreur_is_isf(T_erreur *err);
extern char *lis_erreur_nom(T_erreur *err);
extern int lis_erreur_type(T_erreur *err);
extern int nb_evenements(T_irdata *irdata);

extern T_varinfo *cherche_varinfo(T_irdata *irdata, const char *nom);
extern char lis_varinfo_def(T_irdata *irdata, T_varinfo *info);
extern double lis_varinfo_val(T_irdata *irdata, T_varinfo *info);
extern char lis_varinfo_tab_def(T_irdata *irdata, T_varinfo *info, int idx);
extern double lis_varinfo_tab_val(T_irdata *irdata, T_varinfo *info, int idx);
extern int lis_varinfo_tab(T_irdata *irdata, T_varinfo *info, char idx_def, double idx_val, char *res_def, double *res_val);
extern char *lis_varinfo_ptr_def(T_irdata *irdata, T_varinfo *info);
extern double *lis_varinfo_ptr_val(T_irdata *irdata, T_varinfo *info);
extern void ecris_varinfo(T_irdata *irdata, T_varinfo *info, char def, double val);
extern void ecris_varinfo_tab(T_irdata *irdata, T_varinfo *info, int idx, char def, double val);
extern void pr_var(T_print_context *pr_ctx, T_irdata *irdata, char *nom);
extern void pr_out_var(T_irdata *irdata, char *nom);
extern void pr_err_var(T_irdata *irdata, char *nom);
|}

let gen_decl_functions fmt (cprog : Mir.program) =
  let functions = Com.TargetMap.bindings cprog.program_functions in
  let pp_args fmt args =
    List.iteri
      (fun i _ -> Pp.fpr fmt ", char arg_def%d, double arg_val%d" i i)
      args
  in
  Format.fprintf fmt "@[<v 0>%a@]@,"
    (Format.pp_print_list (fun fmt (fn, fd) ->
         Format.fprintf fmt
           "extern int %s(T_irdata* irdata, char *res_def, double *res_val%a);"
           fn pp_args fd.Mir.target_args))
    functions

let gen_decl_targets fmt (cprog : Mir.program) =
  let targets = Com.TargetMap.bindings cprog.program_targets in
  Format.fprintf fmt "@[<v 0>%a@]@,"
    (Format.pp_print_list (fun fmt (name, _) ->
         Format.fprintf fmt "extern struct S_discord *%s(T_irdata* irdata);"
           name))
    targets

let gen_mlang_h fmt cprog flags stats_varinfos =
  let pr form = Format.fprintf fmt form in
  pr "/****** LICENCE CECIL *****/\n\n";
  pr "#ifndef _MLANG_H_\n";
  pr "#define _MLANG_H_\n";
  pr "\n";
  pr "#include <stdlib.h>\n";
  pr "#include <stdio.h>\n";
  pr "#include <math.h>\n";
  pr "#include <string.h>\n";
  pr "#include <limits.h>\n";
  pr "#include <setjmp.h>\n";
  pr "\n";
  pr "#include \"conf.h\"\n";
  pr "\n";
  pr "#define _PROTS(X) X\n";
  pr "\n";
  pr "#define ANNEE_REVENU %04d\n" flags.Dgfip_options.annee_revenu;
  pr "\n";
  gen_decl_varinfos fmt cprog stats_varinfos;
  pr "\n";
  gen_const fmt cprog;
  pr "\n";
  (* The debug functions need T_irdata to be defined so we put them after *)
  gen_dbg fmt;
  pr "\n";
  gen_lib fmt cprog flags;
  pr "\n";
  gen_decl_functions fmt cprog;
  pr "\n";
  gen_decl_targets fmt cprog;
  pr "#endif /* _MLANG_H_ */\n\n"

let gen_mlang_c fmt (cprog : Mir.program) flags =
  Format.fprintf fmt "%s"
    {|/****** LICENCE CECIL *****/

#include "mlang.h"

int color = 37;
int typo = 0;

int change_couleur (int couleur,int typographie) {
#ifdef FLG_COLORS 
  color = couleur;
	typo = typographie;
#endif /* FLG_COLORS */
	return 0;
}

int get_couleur () {
	return color ;
}

int get_typo () {
	return typo ;
}

double floor_g(double a) {
  if (fabs(a) <= (double)LONG_MAX) {
    return floor(a);
  } else {
    return a;
  }
}

double ceil_g(double a) {
  if (fabs(a) <= (double)LONG_MAX) {
    return ceil(a);
  } else {
    return a;
  }
}

extern FILE * fd_trace_dialog;


#define C_STR_ERREUR_DEB " (("
#define C_LEN_ERREUR_DEB (sizeof(C_STR_ERREUR_DEB)-1)

#define C_STR_ERREUR_FIN "))"
#define C_LEN_ERREUR_FIN (sizeof(C_STR_ERREUR_FIN)-1)

static T_erreur *new_erreur(T_erreur *ref_erreur, char *code) {
  T_erreur *pErreur = NULL;
  size_t len = 0;
  char *new_message = NULL;

  if (ref_erreur == NULL || ref_erreur->message == NULL) return NULL;
  pErreur = (T_erreur *)malloc(sizeof(T_erreur));
  if (pErreur == NULL) goto erreur_new_erreur;
  len = strlen(ref_erreur->message) + 1;
  if (code != NULL) {
    len += C_LEN_ERREUR_DEB + strlen(code) + C_LEN_ERREUR_FIN;
  }
  new_message = (char *)malloc(len * sizeof(char));
  if (new_message == NULL) goto erreur_new_erreur;
  strcpy(new_message, ref_erreur->message);
  if (code != NULL) {
    strcat(new_message, C_STR_ERREUR_DEB);
    strcat(new_message, code);
    strcat(new_message, C_STR_ERREUR_FIN);
  }
  pErreur->message = new_message;
  pErreur->codebo = ref_erreur->codebo;
  pErreur->souscode = ref_erreur->souscode;
  pErreur->isisf = ref_erreur->isisf;
  pErreur->nom = ref_erreur->nom;
  pErreur->type = ref_erreur->type;
  return pErreur;

erreur_new_erreur:
  if (pErreur != NULL) free(pErreur);
  if (new_message != NULL) free(new_message);
  return NULL;
}

/* Libération de la mémoire allouée par la fonction new_erreur */
static void free_new_erreur(T_erreur *erreur) {
  if (erreur == NULL) return;
  if (erreur->message != NULL) {
    free(erreur->message);
  }
  free(erreur);
}

T_discord *new_discord(T_irdata *irdata, T_erreur *ref_erreur, char *code) {
  T_discord *pDiscord = NULL;
  T_erreur *pErreur = NULL;
  T_keep_discord *pKeep = NULL;

  if (irdata == NULL || ref_erreur == NULL) return NULL;
  pDiscord = (T_discord *)malloc(sizeof(T_discord));
  if (pDiscord == NULL) goto erreur_new_discord;
  pKeep = malloc(sizeof(T_keep_discord));
  if (pKeep == NULL) goto erreur_new_discord;
  pErreur = new_erreur(ref_erreur, code);
  if (pErreur == NULL) goto erreur_new_discord;

  pKeep->discord = pDiscord;
  pKeep->suivant = irdata->keep_discords;
  irdata->keep_discords = pKeep;

  pDiscord->erreur = pErreur;
  pDiscord->suivant = irdata->discords;
  irdata->discords = pDiscord;

  return pDiscord;

erreur_new_discord:
  if (pDiscord != NULL) free(pDiscord);
  if (pKeep != NULL) free(pKeep);
  if (pErreur != NULL) free_new_erreur(pErreur);
  return NULL;
}

void free_keep_discord(T_irdata *irdata) {
  T_discord *pDiscord = NULL;
  T_keep_discord *pKeep = NULL;
  T_keep_discord *pNext = NULL;

  if (irdata == NULL) return;
  pKeep = irdata->keep_discords;
  while (pKeep != NULL) {
    pDiscord = pKeep->discord;
    if (pDiscord != NULL) {
      free_new_erreur(pDiscord->erreur);
      free(pDiscord);
    }
    pNext = pKeep->suivant;
    free(pKeep);
    pKeep = pNext;
  }
  irdata->keep_discords = NULL;
}

/* Libération de la mémoire allouée par la fonction add_erreur */
#ifdef ANCIEN
void free_erreur() {}
#else
void free_erreur(T_irdata *irdata) {
  init_erreur(irdata);
}
#endif /* ANCIEN */

void add_erreur(T_irdata *irdata, T_erreur *ref_erreur, char *code) {
  T_discord *pDiscord = NULL;

  pDiscord = new_discord(irdata, ref_erreur, code);
  if (pDiscord == NULL) return;
  if (ref_erreur->type == ANOMALIE) irdata->nb_anos++;
  if (ref_erreur->type == DISCORDANCE) irdata->nb_discos++;
  if (ref_erreur->type == INFORMATIVE) irdata->nb_infos++;
|};
  if flags.Dgfip_options.flg_pro || flags.flg_iliad then
    Format.fprintf fmt "%s"
      {|if (strcmp(ref_erreur->isisf, "O") != 0 && ref_erreur->type == ANOMALIE) {
|}
  else Format.fprintf fmt "%s" {|if (ref_erreur->type == ANOMALIE) {
|};
  Format.fprintf fmt "%s"
    {|irdata->nb_bloqs++;
    if (irdata->nb_bloqs >= irdata->max_bloqs) {
      longjmp(irdata->jmp_bloq, 1);
    }
  }
}

int nb_anomalies(T_irdata *irdata) {
  return irdata->nb_anos;
}

int nb_discordances(T_irdata *irdata) {
  return irdata->nb_discos;
}

int nb_informatives(T_irdata *irdata) {
  return irdata->nb_infos;
}

int nb_bloquantes(T_irdata *irdata) {
  return irdata->nb_bloqs;
}

#ifdef FLG_TRACE

/* int niv_trace = 3; */

#ifdef FLG_API
#define TRACE_FILE fd_trace_dialog
#else
#define TRACE_FILE stderr
#endif /* FLG_API */

void aff1(nom)
char *nom ;
{
#ifdef FLG_COLORS
if (niv_trace >= 1) fprintf(stderr, "\033[%d;%dm%s\033[0m", color, typo, nom) ;
#else
if (niv_trace >= 1) fprintf(stderr, "%s \n", nom) ;
#endif
}

void aff_val(const char *nom, const T_irdata *irdata, int indice, int niv, const char *chaine, int is_tab, int expr, int maxi) {
  double valeur;
  int def;
  if (expr < 0) {
    if (niv_trace >= niv) {
#ifdef FLG_COLORS
      fprintf(TRACE_FILE, "\033[%d;%dm%s[%d] %s 0\033[0m\n",
              color, typo, nom, expr, chaine);
#else
      fprintf(TRACE_FILE, "%s[%d] %s 0m\n", nom, expr, chaine);
#endif /* FLG_COLORS */
    }
    return;
  } else if (expr >= maxi) {
#ifdef FLG_COLORS
    fprintf(TRACE_FILE,
            "\033[%d;%dmerreur: indice (%d) superieur au maximum (%d)\033[0m\n",
            color, typo, expr, maxi);
#else
    fprintf(TRACE_FILE, "erreur: indice (%d) superieur au maximum (%d)\n",
            expr, maxi);
#endif /* FLG_COLORS */
    expr = 0;
  }
  switch (indice & EST_MASQUE) {
    case EST_SAISIE:
      valeur = irdata->saisie[(indice & INDICE_VAL) + expr];
      def = irdata->def_saisie[(indice & INDICE_VAL) + expr];
      break;
    case EST_CALCULEE:
      valeur = irdata->calculee[(indice & INDICE_VAL) + expr];
      def = irdata->def_calculee[(indice & INDICE_VAL) + expr];
      break;
    case EST_BASE:
      valeur = irdata->base[(indice & INDICE_VAL) + expr];
      def = irdata->def_base[(indice & INDICE_VAL) + expr];
      break;
    case EST_TEMPORAIRE:
      valeur = irdata->tmps[irdata->tmps_org - (indice & INDICE_VAL) + expr];
      def = irdata->def_tmps[irdata->tmps_org - (indice & INDICE_VAL) + expr];
      break;
  }
  if (is_tab) {
    if (def == 0) {
      if (valeur != 0) {
#ifdef FLG_COLORS
        fprintf(TRACE_FILE, "\033[%d;%dm%s[%d] : erreur undef = %lf\033[0m\n",
                color, typo, nom, expr, valeur);
#else
        fprintf(TRACE_FILE, "%s[%d] : erreur undef = %lf\n", nom, expr, valeur);
#endif /* FLG_COLORS */
      } else if (niv_trace >= niv) {
#ifdef FLG_COLORS
        fprintf(TRACE_FILE, "\033[%d;%dm%s[%d] %s undef\033[0m\n",
                color, typo, nom, expr, chaine);
#else
        fprintf(TRACE_FILE, "%s[%d] %s undef\n", nom, expr, chaine);
#endif /* FLG_COLORS */
      }
    } else if (def != 1) {
#ifdef FLG_COLORS
      fprintf(TRACE_FILE, "\033[%d;%dm%s[%d] : erreur flag def = %d\033[0m\n",
              color, typo, nom, expr, def);
#else
      fprintf(TRACE_FILE, "%s[%d] : erreur flag def = %d\n", nom, expr, def);
#endif /* FLG_COLORS */
    } else if (niv_trace >= niv) {
#ifdef FLG_COLORS
      fprintf(TRACE_FILE, "\033[%d;%dm%s[%d] %s %lf\033[0m\n",
              color, typo, nom, expr, chaine, valeur);
#else
      fprintf(TRACE_FILE, "%s[%d] %s %lf\n", nom, expr, chaine, valeur);
#endif /* FLG_COLORS */
    }
  } else {
    if (def == 0) {
      if (valeur != 0) {
#ifdef FLG_COLORS
        fprintf(TRACE_FILE, "\033[%d;%dm%s : erreur undef = %lf\033[0m\n",
                color, typo, nom, valeur);
#else
        fprintf(TRACE_FILE, "%s : erreur undef = %lf\n", nom, valeur);
#endif /* FLG_COLORS */
      } else if (niv_trace >= niv) {
#ifdef FLG_COLORS
        fprintf(TRACE_FILE, "\033[%d;%dm%s %s undef\033[0m\n",
                color, typo, nom, chaine);
#else
        fprintf(TRACE_FILE, "%s %s undef\n", nom, chaine);
#endif /* FLG_COLORS */
      }
    } else if (def != 1) {
#ifdef FLG_COLORS
      fprintf(TRACE_FILE, "\033[%d;%dm%s : erreur flag def = %d\033[0m\n",
              color, typo, nom, def);
#else
      fprintf(TRACE_FILE, "%s : erreur flag def = %d\n", nom, def);
#endif /* FLG_COLORS */
    } else if (niv_trace >= niv) {
#ifdef FLG_COLORS
      fprintf(TRACE_FILE, "\033[%d;%dm%s %s %lf\033[0m\n",
              color, typo, nom, chaine, valeur);
#else
      fprintf(TRACE_FILE, "%s %s %lf\n", nom, chaine, valeur);
#endif /* FLG_COLORS */
    }
  }
}

#endif /* FLG_TRACE */

T_event *event(T_irdata *irdata, char idx_def, double idx_val) {
  int idx;
  if (idx_def == 0) return NULL;
  idx = (int)idx_val;
  if (idx < 0 || irdata->nb_events <= idx) return NULL;
  return irdata->events[idx];
}

int size_varinfo(T_varinfo *info, char *res_def, double *res_val) {
  *res_def = 0;
  *res_val = 0.0;
  if (info == NULL) {
    return *res_def;
  }
  *res_def = 1;
  *res_val = (double)info->size;
  return *res_def;
}

T_discord *no_error(T_irdata *irdata) {
  return NULL;
}

int multimax(char *var_def, double *var_val, int size, char nb_def, double nb_val, char *res_def, double *res_val) {
  int i;
  int nb;
  *res_def = 0;
  *res_val = 0.0;
  if (var_def == NULL || var_val == NULL) return *res_def;
  if (nb_def == 0) return *res_def;
  nb = (int)nb_val;
  for (i = 0; i < nb && i < size; i++) {
    if (var_def[i] == 1) *res_def = 1;
    if (var_val[i] >= *res_val) *res_val = var_val[i];
  }
  return *res_def;
}

int multimax_varinfo(T_irdata *irdata, T_varinfo *info, char nb_def, double nb_val, char *res_def, double *res_val) {
  char *var_def = lis_varinfo_ptr_def(irdata, info);
  double *var_val = lis_varinfo_ptr_val(irdata, info);
  *res_def = 0;
  *res_val = 0.0;
  if (irdata == NULL || info == NULL) return *res_def;
  return multimax(var_def, var_val, info->size, nb_def, nb_val, res_def, res_val);
}

int modulo_def(int a, int b) {
  return a;
}

double modulo(double a, double b) {
  return (double)(((int)a) % ((int)b));
}

void env_sauvegarder_un(T_env_sauvegarde **liste, char *oDef, double *oVal) {
  T_env_sauvegarde *nouveau = (T_env_sauvegarde *)malloc(sizeof (T_env_sauvegarde));
  nouveau->sauv_def = *oDef;
  nouveau->sauv_val = *oVal;
  nouveau->orig_def = oDef;
  nouveau->orig_val = oVal;
  nouveau->suite = *liste;
  *liste = nouveau;
}

void env_sauvegarder(T_env_sauvegarde **liste, char *oDef, double *oVal, int sz) {
  int i;
  for (i = 0; i < sz; i++) {
    env_sauvegarder_un(liste, oDef + i, oVal + i);
  }
}

void env_restaurer(T_env_sauvegarde **liste) {
  T_env_sauvegarde *courant;

  while (*liste != NULL) {
    courant = *liste;
    *liste = courant->suite;
    *(courant->orig_def) = courant->sauv_def;
    *(courant->orig_val) = courant->sauv_val;
    free(courant);
  }
}

static void copy_evt(T_event *src, T_event *dst) {
|};
  StrMap.iter
    (fun f (ef : Com.event_field) ->
      if ef.is_var then
        Format.fprintf fmt "  dst->field_%s_var = src->field_%s_var;\n" f f
      else (
        Format.fprintf fmt "  dst->field_%s_def = src->field_%s_def;\n" f f;
        Format.fprintf fmt "  dst->field_%s_val = src->field_%s_val;\n" f f))
    cprog.program_event_fields;
  Format.fprintf fmt "%s"
    {|
    }

void env_sauvegarder_evt(T_env_sauvegarde_evt **liste, T_event *evt) {
  T_env_sauvegarde_evt *nouveau = (T_env_sauvegarde_evt *)malloc(sizeof (T_env_sauvegarde_evt));
  copy_evt(evt, &(nouveau->sauv_evt));
  nouveau->orig_evt = evt;
  nouveau->suite = *liste;
  *liste = nouveau;
}

void env_restaurer_evt(T_env_sauvegarde_evt **liste) {
  T_env_sauvegarde_evt *courant;

  while (*liste != NULL) {
    courant = *liste;
    *liste = courant->suite;
    copy_evt(&(courant->sauv_evt), courant->orig_evt);
    free(courant);
  }
}

void set_print_indent(FILE *std, T_print_context *pr_ctx, double diff) {
  long d = (long)floor(diff + 0.5);
  pr_ctx->indent = max(0, pr_ctx->indent + d);
}

void print_indent(FILE *std, T_print_context *pr_ctx) {
  if (pr_ctx->is_newline) {
    int i;
    for (i = 1; i < pr_ctx->indent; i++) {
      fprintf(pr_ctx->std, " ");
    }
    pr_ctx->is_newline = 0;
  }
}

void print_string(FILE *std, T_print_context *pr_ctx, char *str) {
  while (*str != 0) {
    if (*str == '\n') {
      pr_ctx->is_newline = 1;
    } else {
      print_indent(NULL, pr_ctx);
    }
    fprintf(pr_ctx->std, "%c", *str);
    str++;
  }
}

void print_double(FILE *std, T_print_context *pr_ctx, double f, int pmin, int pmax) {
  print_indent(NULL, pr_ctx);
  if (pmin < 0) {
    pmin = 0;
  }
  if (pmax < 0) {
    pmax = 0;
  }
  if (pmax < pmin) {
    pmax = pmin;
  }
  if (20 < pmin) {
    pmin = 20;
  }
  if (20 < pmax) {
    pmax = 20;
  }
  if (isnan(f)) {
    fprintf(pr_ctx->std, "incorrect");
  } else if (isinf(f)) {
    if (f >= 0.0) {
      fprintf(pr_ctx->std, "+infini");
    } else {
      fprintf(pr_ctx->std, "-infini");
    }
  } else {
    size_t sz;
    char buf[1536];
    char *ptr_dot;
    char *ptr;
    int p;

/*    sz = (size_t)ceil(log10(fabs(f) + 1)) + 21;
    buf = malloc(sz + 1); */
    sz = sprintf(buf, "%.*f", pmax, f);
    ptr_dot = &buf[sz - 1];
    while (ptr_dot != buf && *ptr_dot != '.') ptr_dot--;
    if (*ptr_dot == '.') {
      *ptr_dot = ',';
      p = 0;
      while (p < pmin && *ptr_dot != 0) {
        ptr_dot++;
        p++;
      }
      ptr = ptr_dot;
      while (p < pmax && *ptr != 0) {
        ptr++;
        p++;
      }
      if (*ptr == 0) ptr--;
      while (*ptr == '0' && pmin <= p) {
        *ptr = 0;
        ptr--;
        p--;
      }
      if (*ptr == ',') *ptr = 0;
    }
    fprintf(pr_ctx->std, "%s", buf);
/*    free(buf); */
  }
}

void nettoie_erreur(T_irdata *irdata) {
  init_erreur(irdata);
}

static void init_tab(char *p_def, double *p_val, int nb) {
  if (p_val == NULL || p_def == NULL || nb < 0) return;
  memset(p_val, 0, nb * sizeof (double));
  memset(p_def, 0, nb);
}

void init_saisie(T_irdata *irdata) {
  if (irdata == NULL) return;
  init_tab(irdata->def_saisie, irdata->saisie, TAILLE_SAISIE);
}

void init_calculee(T_irdata *irdata) {
  if (irdata == NULL) return;
  init_tab(irdata->def_calculee, irdata->calculee, TAILLE_CALCULEE);
}

void init_base(T_irdata *irdata) {
  if (irdata == NULL) return;
  init_tab(irdata->def_base, irdata->base, TAILLE_BASE);
}

void init_erreur(T_irdata *irdata) {
  if (irdata == NULL) return;
  irdata->discords = NULL;
  irdata->nb_anos = 0;
  irdata->nb_discos = 0;
  irdata->nb_infos = 0;
  irdata->nb_bloqs = 0;
  irdata->max_bloqs = 4;
}

void detruis_irdata(T_irdata *irdata) {
  if (irdata == NULL) return;
  if (irdata->saisie != NULL) free(irdata->saisie);
  if (irdata->def_saisie != NULL) free(irdata->def_saisie);
  if (irdata->calculee != NULL) free(irdata->calculee);
  if (irdata->def_calculee != NULL) free(irdata->def_calculee);
  if (irdata->base != NULL) free(irdata->base);
  if (irdata->def_base != NULL) free(irdata->def_base);
  if (irdata->tmps != NULL) free(irdata->tmps);
  if (irdata->def_tmps != NULL) free(irdata->def_tmps);
  if (irdata->info_tmps != NULL) free(irdata->info_tmps);
  if (irdata->ref != NULL) free(irdata->ref);
  if (irdata->def_ref != NULL) free(irdata->def_ref);
  if (irdata->info_ref != NULL) free(irdata->info_ref);
  init_erreur(irdata);
  if (irdata->err_finalise != NULL) free(irdata->err_finalise);
  if (irdata->err_sortie != NULL) free(irdata->err_sortie);
  if (irdata->err_archive != NULL) free(irdata->err_archive);
  if (irdata->events != NULL) {
    int i = 0;
    for (i = 0; i < irdata->nb_events; i++) {
      if (irdata->events[i] != NULL) free(irdata->events[i]);
    }
    free(irdata->events);
  }
  free(irdata);
}

T_irdata *cree_irdata(void) {
  T_irdata *irdata = NULL;
  
  irdata = (T_irdata *)malloc(sizeof (T_irdata));
  if (irdata == NULL) return NULL;
  irdata->saisie = NULL;
  irdata->def_saisie = NULL;
  if (TAILLE_SAISIE > 0) {
    irdata->saisie = (double *)malloc(TAILLE_SAISIE * sizeof (double));
    if (irdata->saisie == NULL) goto erreur_cree_irdata;
    irdata->def_saisie = (char *)malloc(TAILLE_SAISIE * sizeof (char));
    if (irdata->def_saisie == NULL) goto erreur_cree_irdata;
  }
  init_saisie(irdata);
  irdata->calculee = NULL;
  irdata->def_calculee = NULL;
  if (TAILLE_CALCULEE > 0) {
    irdata->calculee = (double *)malloc(TAILLE_CALCULEE * sizeof (double));
    if (irdata->calculee == NULL) goto erreur_cree_irdata;
    irdata->def_calculee = (char *)malloc(TAILLE_CALCULEE * sizeof (char));
    if (irdata->def_calculee == NULL) goto erreur_cree_irdata;
  }
  init_calculee(irdata);
  irdata->base = NULL;
  irdata->def_base = NULL;
  if (TAILLE_BASE > 0) {
    irdata->base = (double *)malloc(TAILLE_BASE * sizeof (double));
    if (irdata->base == NULL) goto erreur_cree_irdata;
    irdata->def_base = (char *)malloc(TAILLE_BASE * sizeof (char));
    if (irdata->def_base == NULL) goto erreur_cree_irdata;
  }
  init_base(irdata);
  irdata->tmps = NULL;
  irdata->def_tmps = NULL;
  irdata->info_tmps = NULL;
  if (TAILLE_TMP_VARS > 0) {
    irdata->tmps = (double *)malloc(TAILLE_TMP_VARS * sizeof (double));
    if (irdata->tmps == NULL) goto erreur_cree_irdata;
    irdata->def_tmps = (char *)malloc(TAILLE_TMP_VARS * sizeof (char));
    if (irdata->def_tmps == NULL) goto erreur_cree_irdata;
    irdata->info_tmps = (T_varinfo *)malloc(TAILLE_TMP_VARS * sizeof (T_varinfo));    
    if (irdata->info_tmps == NULL) goto erreur_cree_irdata;
  }
  irdata->ref = NULL;
  irdata->def_ref = NULL;
  irdata->info_ref = NULL;
  if (TAILLE_REFS > 0) {
    irdata->ref = (double **)malloc(TAILLE_REFS * (sizeof (double *)));
    if (irdata->ref == NULL) goto erreur_cree_irdata;
    irdata->def_ref = (char **)malloc(TAILLE_REFS * (sizeof (char *)));
    if (irdata->def_ref == NULL) goto erreur_cree_irdata;
    irdata->info_ref = (T_varinfo **)malloc(TAILLE_REFS * (sizeof (T_varinfo *)));
    if (irdata->info_ref == NULL) goto erreur_cree_irdata;
  }
  irdata->tmps_org = 0;
  irdata->ref_org = 0;
  irdata->keep_discords = NULL;
  irdata->discords = NULL;
  irdata->sz_err_finalise = 0;
  irdata->err_finalise = NULL;
  irdata->nb_err_finalise = 0;
  irdata->sz_err_sortie = 0;
  irdata->err_sortie = NULL;
  irdata->nb_err_sortie = 0;
  irdata->sz_err_archive = 0;
  irdata->err_archive = NULL;
  irdata->nb_err_archive = 0;
  init_erreur(irdata);
  irdata->events = NULL;
  irdata->nb_events = 0;
  irdata->ctx_pr_out.std = stdout;
  irdata->ctx_pr_out.indent = 0;
  irdata->ctx_pr_out.is_newline = 1;
  irdata->ctx_pr_err.std = stderr;
  irdata->ctx_pr_err.indent = 0;
  irdata->ctx_pr_err.is_newline = 1;
  return irdata;

erreur_cree_irdata:
  detruis_irdata(irdata);
  return NULL;
}

void set_max_bloquantes(T_irdata *irdata, const int max_ano) {
  if (irdata == NULL) return;
  if (max_ano < 0) {
    irdata->max_bloqs = 0;
  } else {
    irdata->max_bloqs = max_ano;
  }
}

void recopie_saisie(T_irdata *irdata_src, T_irdata *irdata_dst) {
  if (irdata_src == NULL || irdata_dst == NULL) return;
  memcpy(irdata_dst->saisie, irdata_src->saisie, TAILLE_SAISIE * sizeof(double));
  memcpy(irdata_dst->def_saisie, irdata_src->def_saisie, TAILLE_SAISIE);
}

void recopie_calculee(T_irdata *irdata_src, T_irdata *irdata_dst) {
  if (irdata_src == NULL || irdata_dst == NULL) return;
  memcpy(irdata_dst->calculee, irdata_src->calculee, TAILLE_CALCULEE * sizeof(double));
  memcpy(irdata_dst->def_calculee, irdata_src->def_calculee, TAILLE_CALCULEE);
}

void recopie_base(T_irdata *irdata_src, T_irdata *irdata_dst) {
  if (irdata_src == NULL || irdata_dst == NULL) return;
  memcpy(irdata_dst->base, irdata_src->base, TAILLE_BASE * sizeof(double));
  memcpy(irdata_dst->def_base, irdata_src->def_base, TAILLE_BASE);
}

static void ecris_tab(char *t_def, double *t_val, int t_nb, int idx, char def, double val) {
  if (t_val == NULL || t_def == NULL || idx < 0 || t_nb <= idx) return;
  if (def == 0) t_def[idx] = 0;
  else t_def[idx] = 1;
  t_val[idx] = val;
}

void ecris_saisie(T_irdata *irdata, int idx, char def, double val) {
  if (irdata == NULL) return;
  ecris_tab(irdata->def_saisie, irdata->saisie, TAILLE_SAISIE, idx, def, val);
}

void ecris_calculee(T_irdata *irdata, int idx, char def, double val) {
  if (irdata == NULL) return;
  ecris_tab(irdata->def_calculee, irdata->calculee, TAILLE_CALCULEE, idx, def, val);
}

void ecris_base(T_irdata *irdata, int idx, char def, double val) {
  if (irdata == NULL) return;
  ecris_tab(irdata->def_base, irdata->base, TAILLE_BASE, idx, def, val);
}

static char lis_tab_def(char *t_def, int t_nb, int idx) {
  if (t_def == NULL || idx < 0 || t_nb <= idx) return 0;
  return t_def[idx];
}

char lis_saisie_def(T_irdata *irdata, int idx) {
  if (irdata == NULL) return 0;
  return lis_tab_def(irdata->def_saisie, TAILLE_SAISIE, idx);
}

char lis_calculee_def(T_irdata *irdata, int idx) {
  if (irdata == NULL) return 0;
  return lis_tab_def(irdata->def_calculee, TAILLE_CALCULEE, idx);
}

char lis_base_def(T_irdata *irdata, int idx) {
  if (irdata == NULL) return 0;
  return lis_tab_def(irdata->def_base, TAILLE_BASE, idx);
}

static double lis_tab_val(double *t_val, int t_nb, int idx) {
  if (t_val == NULL || idx < 0 || t_nb <= idx) return 0.0;
  return t_val[idx];
}

double lis_saisie_val(T_irdata *irdata, int idx) {
  if (irdata == NULL) return 0.0;
  return lis_tab_val(irdata->saisie, TAILLE_SAISIE, idx);
}

double lis_calculee_val(T_irdata *irdata, int idx) {
  if (irdata == NULL) return 0.0;
  return lis_tab_val(irdata->calculee, TAILLE_CALCULEE, idx);
}

double lis_base_val(T_irdata *irdata, int idx) {
  if (irdata == NULL) return 0.0;
  return lis_tab_val(irdata->base, TAILLE_BASE, idx);
}

static char *lis_tab_def_ref(char *t_def, int t_nb, int idx) {
  if (t_def == NULL || idx < 0 || t_nb <= idx) return NULL;
  return &(t_def[idx]);
}

char *lis_saisie_def_ref(T_irdata *irdata, int idx) {
  if (irdata == NULL) return NULL;
  return lis_tab_def_ref(irdata->def_saisie, TAILLE_SAISIE, idx);
}

char *lis_calculee_def_ref(T_irdata *irdata, int idx) {
  if (irdata == NULL) return NULL;
  return lis_tab_def_ref(irdata->def_calculee, TAILLE_CALCULEE, idx);
}

char *lis_base_def_ref(T_irdata *irdata, int idx) {
  if (irdata == NULL) return NULL;
  return lis_tab_def_ref(irdata->def_base, TAILLE_BASE, idx);
}

static double *lis_tab_val_ref(double *t_val, int t_nb, int idx) {
  if (t_val == NULL || idx < 0 || t_nb <= idx) return NULL;
  return &(t_val[idx]);
}

double *lis_saisie_val_ref(T_irdata *irdata, int idx) {
  if (irdata == NULL) return NULL;
  return lis_tab_val_ref(irdata->saisie, TAILLE_SAISIE, idx);
}

double *lis_calculee_val_ref(T_irdata *irdata, int idx) {
  if (irdata == NULL) return NULL;
  return lis_tab_val_ref(irdata->calculee, TAILLE_CALCULEE, idx);
}

double *lis_base_val_ref(T_irdata *irdata, int idx) {
  if (irdata == NULL) return NULL;
  return lis_tab_val_ref(irdata->base, TAILLE_BASE, idx);
}

T_discord *lis_discords(T_irdata *irdata) {
  if (irdata == NULL) return NULL;
  return irdata->discords;
}

T_erreur *lis_erreur(T_discord *discord) {
  if (discord == NULL) return NULL;
  return discord->erreur;
}

char *lis_erreur_message(T_erreur *err) {
  if (err == NULL) return NULL;
  return err->message;
}

T_discord *discord_suivante(T_discord *discord) {
  if (discord == NULL) return NULL;
  return discord->suivant;
}

char *lis_erreur_code_bo(T_erreur *err) {
  if (err == NULL) return NULL;
  return err->message;
}

char *lis_erreur_sous_code(T_erreur *err) {
  if (err == NULL) return NULL;
  return err->souscode;
}

char *lis_erreur_is_isf(T_erreur *err) {
  if (err == NULL) return NULL;
  return err->isisf;
}

char *lis_erreur_nom(T_erreur *err) {
  if (err == NULL) return NULL;
  return err->nom;
}

int lis_erreur_type(T_erreur *err) {
  if (err == NULL) return 0;
  return err->type;
}

int nb_evenements(T_irdata *irdata) {
  if (irdata == NULL) return 0;
  return irdata->nb_events;
}

T_varinfo *cherche_varinfo(T_irdata *irdata, const char *nom) {
  T_varinfo_map *map = NULL;
  int res = -1;
  int inf = 0;
  int sup = NB_variable + NB_saisie;
  int millieu = 0;

  if (irdata == NULL || nom == NULL) return NULL;
  while ((res != 0) && (inf < sup)) {
    millieu = (inf + sup) / 2;
    map = &(varinfo[millieu]);
    res = strcmp(nom, map->name);
    if (res < 0) {
      sup = millieu;
    } else if (res > 0) {
      inf = millieu + 1;
    }
  }
  if (res == 0) {
    return map->info;
  }
  return NULL;
}

char lis_varinfo_def(T_irdata *irdata, T_varinfo *info) {
  if (irdata == NULL || info == NULL) return 0;
  switch (info->loc_cat) {
    case EST_SAISIE:
      return irdata->def_saisie[info->idx];
    case EST_CALCULEE:
      return irdata->def_calculee[info->idx];
    case EST_BASE:
      return irdata->def_base[info->idx];
    default:
      return 0;
  }
}

double lis_varinfo_val(T_irdata *irdata, T_varinfo *info) {
  if (irdata == NULL || info == NULL) return 0.0;
  switch (info->loc_cat) {
    case EST_SAISIE:
      return irdata->saisie[info->idx];
    case EST_CALCULEE:
      return irdata->calculee[info->idx];
    case EST_BASE:
      return irdata->base[info->idx];
    default:
      return 0.0;
  }
}

char lis_varinfo_tab_def(T_irdata *irdata, T_varinfo *info, int idx) {
  if (irdata == NULL || info == NULL || info->size <= idx) return 0;
  if (idx < 0) return 1;
  switch (info->loc_cat) {
    case EST_SAISIE:
      return irdata->def_saisie[info->idx + idx];
    case EST_CALCULEE:
      return irdata->def_calculee[info->idx + idx];
    case EST_BASE:
      return irdata->def_base[info->idx + idx];
    default:
      return 0;
  }
}

double lis_varinfo_tab_val(T_irdata *irdata, T_varinfo *info, int idx) {
  if (irdata == NULL || info == NULL || idx < 0 || info->size <= idx) return 0.0;
  switch (info->loc_cat) {
    case EST_SAISIE:
      return irdata->saisie[info->idx + idx];
    case EST_CALCULEE:
      return irdata->calculee[info->idx + idx];
    case EST_BASE:
      return irdata->base[info->idx + idx];
    default:
      return 0.0;
  }
}

int lis_varinfo_tab(T_irdata *irdata, T_varinfo *info, char idx_def, double idx_val, char *res_def, double *res_val) {
  int idx;
  *res_def = 0;
  *res_val = 0.0;
  if (irdata == NULL || info == NULL || idx_def == 0) return *res_def;
  idx = (int)idx_val;
  *res_def = lis_varinfo_tab_def(irdata, info, idx);
  *res_val = lis_varinfo_tab_val(irdata, info, idx);
  return *res_def;
}


char *lis_varinfo_ptr_def(T_irdata *irdata, T_varinfo *info) {
  if (irdata == NULL || info == NULL) return NULL;
  switch (info->loc_cat) {
    case EST_SAISIE:
      return &(irdata->def_saisie[info->idx]);
    case EST_CALCULEE:
      return &(irdata->def_calculee[info->idx]);
    case EST_BASE:
      return &(irdata->def_base[info->idx]);
    default:
      return NULL;
  }
}

double *lis_varinfo_ptr_val(T_irdata *irdata, T_varinfo *info) {
  if (irdata == NULL || info == NULL) return NULL;
  switch (info->loc_cat) {
    case EST_SAISIE:
      return &(irdata->saisie[info->idx]);
    case EST_CALCULEE:
      return &(irdata->calculee[info->idx]);
    case EST_BASE:
      return &(irdata->base[info->idx]);
    default:
      return NULL;
  }
}

void ecris_varinfo(T_irdata *irdata, T_varinfo *info, char def, double val) {
  if (irdata == NULL || info == NULL) return;
  if (def == 0) {
    val = 0.0;
  } else {
    def = 1;
  }
  switch (info->loc_cat) {
    case EST_SAISIE:
      irdata->def_saisie[info->idx] = def;
      irdata->saisie[info->idx] = val;
      return;
    case EST_CALCULEE:
      irdata->def_calculee[info->idx] = def;
      irdata->calculee[info->idx] = val;
      return;
    case EST_BASE:
      irdata->def_base[info->idx] = def;
      irdata->base[info->idx] = val;
      return;
    default:
      return;
  }
}

void ecris_varinfo_tab(T_irdata *irdata, T_varinfo *info, int idx, char def, double val) {
  int var_idx = 0;

  if (irdata == NULL || info == NULL || idx < 0 || info->size <= idx) return;
  var_idx = info->idx + idx;
  if (def == 0) {
    val = 0.0;
  } else {
    def = 1;
  }
  switch (info->loc_cat) {
    case EST_SAISIE:
      irdata->def_saisie[var_idx] = def;
      irdata->saisie[var_idx] = val;
      return;
    case EST_CALCULEE:
      irdata->def_calculee[var_idx] = def;
      irdata->calculee[var_idx] = val;
      return;
    case EST_BASE:
      irdata->def_base[var_idx] = def;
      irdata->base[var_idx] = val;
      return;
    default:
      return;
  }
}

/* !!! */
void pr_var(T_print_context *pr_ctx, T_irdata *irdata, char *nom) {
  T_varinfo *info = NULL;

  if (pr_ctx == NULL) return;
  info = cherche_varinfo(irdata, nom);
  if (info == NULL) {
    fprintf(pr_ctx->std, "inconnu");
  } else {
    if (lis_varinfo_def(irdata, info) == 0) {
      fprintf(pr_ctx->std, "indefini");
    } else {
      print_double(NULL, pr_ctx, lis_varinfo_val(irdata, info), 0, 30);
    }
  }
}

void pr_out_var(T_irdata *irdata, char *nom) {
  if (irdata == NULL) return;
  pr_var(&(irdata->ctx_pr_out), irdata, nom);
}

void pr_err_var(T_irdata *irdata, char *nom) {
  if (irdata == NULL) return;
  pr_var(&(irdata->ctx_pr_err), irdata, nom);
}

|};
  StrMap.iter
    (fun f (ef : Com.event_field) ->
      let pr form = Pp.fpr fmt form in
      pr
        "char event_field_%s(T_irdata *irdata, char *res_def, double *res_val, \
         char idx_def, double idx_val) {\n"
        f;
      if ef.is_var then pr "  T_varinfo *info = NULL;\n";
      pr "  int idx = (int)floor(idx_val);\n";
      pr "  if (idx_def != 1 || idx < 0 || irdata->nb_events <= idx) {\n";
      pr "    *res_def = 0;\n";
      pr "    *res_val = 0.0;\n";
      pr "    return 0;\n";
      pr "  }\n";
      if ef.is_var then (
        pr "  info = irdata->events[idx]->field_%s_var;\n" f;
        pr "  *res_def = lis_varinfo_def(irdata, info);\n";
        pr "  *res_val = lis_varinfo_val(irdata, info);\n")
      else (
        pr "  *res_def = irdata->events[idx]->field_%s_def;\n" f;
        pr "  *res_val = irdata->events[idx]->field_%s_val;\n" f);
      pr "  return *res_def;\n";
      pr "}\n\n";

      if ef.is_var then (
        pr
          "T_varinfo *event_field_%s_var(T_irdata *irdata, char idx_def, \
           double idx_val) {\n"
          f;
        pr "  T_varinfo *info = NULL;\n";
        pr "  int idx = (int)floor(idx_val);\n";
        pr "  if (idx_def != 1 || idx < 0 || irdata->nb_events <= idx) {\n";
        pr "    return NULL;\n";
        pr "  }\n";
        pr "  return irdata->events[idx]->field_%s_var;\n" f;
        pr "}\n\n"))
    cprog.program_event_fields

let generate_auxiliary_files flags (cprog : Mir.program) : unit =
  Dgfip_compir_files.generate_compir_files flags cprog;

  let stats_varinfos = gen_table_varinfos cprog flags in

  let oc, fmt = open_file "erreurs.c" in
  gen_erreurs_c fmt flags cprog;
  close_out oc;

  let oc, fmt = open_file "conf.h" in
  gen_conf_h fmt cprog flags;
  close_out oc;

  let oc, fmt = open_file "mlang.h" in
  gen_mlang_h fmt cprog flags stats_varinfos;
  close_out oc;

  let oc, fmt = open_file "mlang.c" in
  gen_mlang_c fmt cprog flags;
  close_out oc
