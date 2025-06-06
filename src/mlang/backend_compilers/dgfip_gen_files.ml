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

let gen_table_varinfo vars cat Com.CatVar.{ id_int; id_str; attributs; _ } stats
    =
  let oc, fmt = open_file (Pp.spr "varinfo_%d.c" id_int) in
  Pp.fpr fmt {|/****** LICENCE CECIL *****/

#include "mlang.h"

|};
  Pp.fpr fmt "T_varinfo_%s varinfo_%s[NB_%s + 1] = {\n" id_str id_str id_str;
  IntMap.iter
    (fun _ var ->
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
      let tab_idx =
        if Com.Var.is_table var then Com.Var.loc_tab_idx var else -1
      in
      Pp.fpr fmt "  { \"%s\", \"%s\", %d, %d, %d, %d, %s" name alias idx tab_idx
        size id_int loc_cat;
      StrMap.iter (fun _ av -> Pp.fpr fmt ", %d" (Pos.unmark av)) attrs;
      Pp.fpr fmt " },\n")
    vars;
  Pp.fpr fmt "  NULL\n};\n\n";
  close_out oc;
  let nb = IntMap.cardinal vars in
  let attr_set =
    StrMap.fold (fun an _ res -> StrSet.add an res) attributs StrSet.empty
  in
  Com.CatVar.Map.add cat (id_str, id_int, nb, attr_set) stats

let gen_table_tmp_varinfo (cprog : Mir.program) fmt =
  let vars = IntMap.filter (fun _ v -> Com.Var.is_temp v) cprog.program_dict in
  Pp.fpr fmt "T_varinfo tmp_varinfo[%d] = {\n" (IntMap.cardinal vars + 1);
  IntMap.iter
    (fun _ var ->
      let name = Com.Var.name_str var in
      let idx = Com.Var.loc_idx var in
      let tab_idx =
        if Com.Var.is_table var then Com.Var.loc_tab_idx var else -1
      in
      let size = Com.Var.size var in
      Pp.fpr fmt
        "  { \"%s\", \"\", %d, %d, %d, ID_TMP_VARS, EST_TEMPORAIRE },\n" name
        idx tab_idx size)
    vars;
  Pp.fpr fmt "  NULL\n};\n\n"

let gen_table_tab_varinfo (cprog : Mir.program) fmt =
  let table_map = cprog.program_stats.table_map in
  Pp.fpr fmt "T_varinfo *tab_varinfo[TAILLE_TAB_VARINFO + 1] = {\n";
  IntMap.iter
    (fun _ var ->
      let idx = Com.Var.loc_cat_idx var in
      let name = Com.Var.name_str var in
      if Com.Var.is_tgv var then
        let loc = Com.Var.loc_tgv var in
        Pp.fpr fmt "  (T_varinfo *)&(varinfo_%s[%d]), /* %s */\n"
          loc.loc_cat_str idx name
      else Pp.fpr fmt "  &(tmp_varinfo[%d]), /* %s */\n" idx name)
    table_map;
  Pp.fpr fmt "  NULL\n};\n\n"

let gen_table_varinfos (cprog : Mir.program) flags =
  let stats_varinfos =
    let fold cv data res =
      let vars =
        let foldVars _ var vars =
          if Com.CatVar.compare (Com.Var.cat var) cv = 0 then
            IntMap.add (Com.Var.loc_cat_idx var) var vars
          else vars
        in
        StrMap.fold foldVars cprog.program_vars IntMap.empty
      in
      gen_table_varinfo vars cv data res
    in
    Com.CatVar.Map.fold fold cprog.program_var_categories Com.CatVar.Map.empty
  in
  let oc, fmt = open_file "varinfos.c" in
  Pp.fpr fmt {|/****** LICENCE CECIL *****/

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
    Pp.fpr fmt "T_varinfo_map varinfo[1] = {NULL};\n\n"
  else (
    Pp.fpr fmt "T_varinfo_map varinfo[NB_variable + NB_saisie + 1] = {\n";
    let var_map =
      let fold name var var_map =
        let var_map = StrMap.add name var var_map in
        match Com.Var.alias var with
        | Some m_alias -> StrMap.add (Pos.unmark m_alias) var var_map
        | None -> var_map
      in
      StrMap.fold fold cprog.program_vars StrMap.empty
    in
    let iter name var =
      let id_str =
        let cv = Com.Var.cat var in
        Com.CatVar.((Map.find cv cprog.program_var_categories).id_str)
      in
      let idx = Com.Var.loc_cat_idx var in
      let var_addr = Pp.spr "(T_varinfo *)&(varinfo_%s[%d])" id_str idx in
      Pp.fpr fmt "  { \"%s\", %s },\n" name var_addr
    in
    StrMap.iter iter var_map;
    Pp.fpr fmt "  NULL\n};\n\n");
  gen_table_tmp_varinfo cprog fmt;
  gen_table_tab_varinfo cprog fmt;
  close_out oc;
  stats_varinfos

let gen_decl_varinfos fmt (cprog : Mir.program) stats =
  Pp.fpr fmt
    {|typedef struct S_varinfo {
  char *name;
  char *alias;
  int idx;
  int tab_idx;
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
      Pp.fpr fmt
        {|typedef struct S_varinfo_%s {
  char *name;
  char *alias;
  int idx;
  int tab_idx;
  int size;
  int cat;
  int loc_cat;
|}
        id_str;
      StrSet.iter (fun an -> Pp.fpr fmt "  int attr_%s;\n" an) attr_set;
      Pp.fpr fmt "} T_varinfo_%s;\n\n" id_str)
    stats;
  Pp.fpr fmt "\n";
  Com.CatVar.Map.iter
    (fun _ (id_str, _, _, _) ->
      Pp.fpr fmt "extern T_varinfo_%s varinfo_%s[];\n" id_str id_str)
    stats;
  Pp.fpr fmt "extern T_varinfo_map varinfo[];\n";
  Pp.fpr fmt "extern T_varinfo tmp_varinfo[];\n";
  Pp.fpr fmt "extern T_varinfo *tab_varinfo[];\n";
  Pp.fpr fmt "\n";
  Com.CatVar.Map.iter
    (fun _ (id_str, _, nb, _) -> Pp.fpr fmt "#define NB_%s %d\n" id_str nb)
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
  Pp.fpr fmt "#define NB_saisie %d\n" nb_saisie;
  Pp.fpr fmt "#define NB_variable %d\n" nb_variable;
  Pp.fpr fmt "\n";
  let id_tmp =
    Com.CatVar.Map.fold
      (fun _ (id_str, id_int, _, _) id_tmp ->
        Pp.fpr fmt "#define ID_%s %d\n" id_str id_int;
        max (id_int + 1) id_tmp)
      stats (-1)
  in
  Pp.fpr fmt "#define ID_TMP_VARS %d\n" id_tmp;

  let attrs =
    Com.CatVar.Map.fold
      (fun _ Com.CatVar.{ attributs; _ } res ->
        StrMap.fold (fun attr _ res -> StrSet.add attr res) attributs res)
      cprog.program_var_categories StrSet.empty
  in
  StrSet.iter
    (fun attr ->
      Pp.fpr fmt "\nextern char attribut_%s_def(T_varinfo *vi);\n" attr;
      Pp.fpr fmt "extern double attribut_%s(T_varinfo *vi);\n" attr)
    attrs

let is_valid_app apps =
  StrMap.exists (fun app _ -> List.mem app !Cli.application_names) apps

let gen_erreurs_c fmt flags (cprog : Mir.program) =
  Pp.fpr fmt {|/****** LICENCE CECIL *****/

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
      Pp.fpr fmt
        "T_erreur erreur_%s = { \"%s%s%s / %s\", \"%s\", \"%s\", \"%s\", \
         \"%s\", %d };\n"
        (Pos.unmark e.name) (Pos.unmark e.famille) (Pos.unmark e.code_bo)
        sous_code_suffix
        (Strings.sanitize_c_str (Pos.unmark e.libelle))
        (Pos.unmark e.code_bo) (Pos.unmark e.sous_code) (Pos.unmark e.is_isf)
        (Pos.unmark e.name) terr)
    cprog.program_errors;

  if flags.Dgfip_options.flg_pro || flags.flg_iliad then begin
    Pp.fpr fmt "T_erreur *tabErreurs[] = {\n";

    StrMap.iter
      (fun _ (e : Com.Error.t) ->
        Pp.fpr fmt "    &erreur_%s,\n" (Pos.unmark e.name))
      cprog.program_errors;

    Pp.fpr fmt "    NULL\n};\n"
  end

(* Print #defines corresponding to generation options *)
let gen_conf_h fmt (cprog : Mir.program) flags =
  let open Dgfip_options in
  Pp.fpr fmt
    {|/****** LICENCE CECIL *****/

#ifndef _CONF_H_
#define _CONF_H_

|};
  if flags.flg_correctif then Pp.fpr fmt "#define FLG_CORRECTIF\n";
  if flags.flg_iliad then Pp.fpr fmt "#define FLG_ILIAD\n";
  if flags.flg_pro then Pp.fpr fmt "#define FLG_PRO\n";
  if flags.flg_cfir then Pp.fpr fmt "#define FLG_CFIR\n";
  if flags.flg_gcos then Pp.fpr fmt "#define FLG_GCOS\n";
  if flags.flg_tri_ebcdic then Pp.fpr fmt "#define FLG_TRI_EBCDIC\n";
  (* flag is not used *)
  if flags.flg_short then Pp.fpr fmt "#define FLG_SHORT /* inutile ? */\n";
  if flags.flg_register then Pp.fpr fmt "#define FLG_REGISTER\n";
  (* flag is not used *)
  if flags.flg_optim_min_max then
    Pp.fpr fmt "#define FLG_OPTIM_MIN_MAX /* inutile ? */\n";
  if flags.flg_extraction then Pp.fpr fmt "#define FLG_EXTRACTION\n";
  if flags.flg_genere_libelle_restituee then
    Pp.fpr fmt "#define FLG_GENERE_LIBELLE_RESTITUEE\n";
  if flags.flg_controle_separe then Pp.fpr fmt "#define FLG_CONTROLE_SEPARE\n";
  if flags.flg_controle_immediat then
    Pp.fpr fmt "#define FLG_CONTROLE_IMMEDIAT\n";
  (* does not need to be printed *)
  (*if flags.flg_overlays then Pp.fpr fmt "#define FLG_OVERLAYS\n"; *)
  if flags.flg_colors then Pp.fpr fmt "#define FLG_COLORS\n";
  if flags.flg_ticket then Pp.fpr fmt "#define FLG_TICKET\n";
  if flags.flg_trace then Pp.fpr fmt "#define FLG_TRACE\n";
  (* flag is not used *)
  (*if flags.flg_trace_irdata then Pp.fpr fmt "#define
    FLG_TRACE_IRDATA\n"; *)
  if flags.flg_debug then Pp.fpr fmt "#define FLG_DEBUG\n";
  Pp.fpr fmt "#define NB_DEBUG_C  %d\n" flags.nb_debug_c;
  Pp.fpr fmt "#define EPSILON %f\n" !Cli.comparison_error_margin;
  let count loc =
    StrMap.fold
      (fun _ var nb ->
        nb + if Com.Var.cat_var_loc var = loc then Com.Var.size var else 0)
      cprog.program_vars 0
  in
  let nb_saisie = count Com.CatVar.LocInput in
  let nb_calculee = count Com.CatVar.LocComputed in
  let nb_base = count Com.CatVar.LocBase in
  let nb_vars = nb_saisie + nb_calculee + nb_base in
  Pp.fpr fmt "#define NB_VARS  %d\n" nb_vars;
  Pp.fpr fmt {|
#endif /* _CONF_H_ */
|}

let gen_dbg fmt =
  Pp.fpr fmt
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
  Pp.fpr fmt
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
      if field.is_var then Pp.fpr fmt "  T_varinfo *field_%s_var;\n" fname
      else (
        Pp.fpr fmt "  char field_%s_def;\n" fname;
        Pp.fpr fmt "  double field_%s_val;\n" fname))
    cprog.program_event_field_idxs;
  Pp.fpr fmt
    {|};

typedef struct S_event T_event;

struct S_var_space {
  int id;
  char *name;
  char *def_saisie;
  double *saisie;
  char *def_calculee;
  double *calculee;
  char *def_base;
  double *base;
};

typedef struct S_var_space T_var_space;

|};
  Pp.fpr fmt "#define NB_ESPACES_VARIABLES %d@\n@\n"
    (IntMap.cardinal cprog.program_var_spaces_idx);
  Pp.fpr fmt
    {|
struct S_irdata {
  char *def_saisie;
  double *saisie;
  char *def_calculee;
  double *calculee;
  char *def_base;
  double *base;
|};
  IntMap.iter
    (fun _ (vsd : Com.variable_space) ->
      let sp = Pos.unmark vsd.vs_name in
      Com.CatVar.LocMap.iter
        (fun loc _ ->
          match loc with
          | Com.CatVar.LocInput ->
              Pp.fpr fmt "  char *def_saisie_%s;@\n" sp;
              Pp.fpr fmt "  double *saisie_%s;@\n" sp
          | Com.CatVar.LocComputed ->
              Pp.fpr fmt "  char *def_calculee_%s;@\n" sp;
              Pp.fpr fmt "  double *calculee_%s;@\n" sp
          | Com.CatVar.LocBase ->
              Pp.fpr fmt "  char *def_base_%s;@\n" sp;
              Pp.fpr fmt "  double *base_%s;@\n" sp)
        vsd.vs_cats)
    cprog.program_var_spaces_idx;
  Pp.fpr fmt
    {|  T_var_space var_spaces[NB_ESPACES_VARIABLES + 1];
  char *def_tmps;
  double *tmps;
  int tmps_org;
  int nb_tmps_target;
  T_varinfo **info_tmps;
  char **def_ref;
  double **ref;
  T_varinfo **info_ref;
  int ref_org;
  int nb_refs_target;
  char **ref_name;
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
  Pp.fpr fmt
    {|
#define DS_(sp,idx) irdata->def_saisie_##sp[idx]
#define S_(sp,idx) irdata->saisie_##sp[idx]

#define DC_(sp,idx) irdata->def_calculee_##sp[idx]
#define C_(sp,idx) irdata->calculee_##sp[idx]

#define DB_(sp,idx) irdata->def_base_##sp[idx]
#define B_(sp,idx) irdata->base_##sp[idx]

#define I_(cat,idx) ((T_varinfo *)&(varinfo_##cat[idx]))

#define DT_(idx) (irdata->def_tmps[irdata->tmps_org + (idx)])
#define T_(idx) (irdata->tmps[irdata->tmps_org + (idx)])
#define IT_(idx) (irdata->info_tmps[irdata->tmps_org + (idx)])

#define DR_(idx) (irdata->def_ref[irdata->ref_org + (idx)])
#define R_(idx) (irdata->ref[irdata->ref_org + (idx)])
#define NR_(idx) (irdata->ref_name[irdata->ref_org + (idx)])
#define IR_(idx) (irdata->info_ref[irdata->ref_org + (idx)])

extern T_event *event(T_irdata *irdata, char idx_def, double idx_val);
extern int size_varinfo(T_varinfo *info, char *res_def, double *res_val);

#define EST_SAISIE     0x00000
#define EST_CALCULEE   0x04000
#define EST_BASE       0x08000
#define EST_TEMPORAIRE 0x10000
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
  Pp.fpr fmt "#define EPSILON %f" !Cli.comparison_error_margin;
  Pp.fpr fmt
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

extern int multimax_varinfo(
  T_irdata *irdata, T_varinfo *info,
  char nb_def, double nb_val,
  char *res_def, double *res_val
);

extern int modulo_def(int, int);
extern double modulo(double, double);
|}

let gen_lib fmt (cprog : Mir.program) flags =
  let count loc =
    StrMap.fold
      (fun _ var nb ->
        nb + if Com.Var.cat_var_loc var = loc then Com.Var.size var else 0)
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

  Pp.fpr fmt
    {|#define TAILLE_SAISIE %d
#define TAILLE_CALCULEE %d
#define TAILLE_BASE %d
#define TAILLE_TOTALE %d
#define NB_ENCH %d

|}
    taille_saisie taille_calculee taille_base taille_totale nb_ench;

  Pp.fpr fmt "#define TAILLE_TMP_VARS %d\n" cprog.program_stats.sz_all_tmps;
  Pp.fpr fmt "#define TAILLE_REFS %d\n" cprog.program_stats.nb_all_refs;
  Pp.fpr fmt "#define TAILLE_TAB_VARINFO %d\n"
    (IntMap.cardinal cprog.program_stats.table_map);

  Pp.fpr fmt
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

  Pp.fpr fmt "#define NB_ERR %d\n" nb_err;
  Pp.fpr fmt "#define NB_CALL %d\n" nb_call;
  Pp.fpr fmt "#define NB_VERIF %d\n\n" nb_verif;

  (* TODO external declaration of individual control rules (seems to be no
     longer used) *)
  StrMap.iter
    (fun _ (e : Com.Error.t) ->
      let en = Pos.unmark e.name in
      Pp.fpr fmt "extern T_erreur erreur_%s;\n" en)
    cprog.program_errors;

  (* TODO function declarations (seems to be no longer used) *)
  if flags.Dgfip_options.flg_pro then
    Pp.fpr fmt "extern struct S_erreur *tabErreurs[];\n\n"
  else Pp.fpr fmt "\n";

  Pp.fpr fmt
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

extern char *concat_nom_index(char *nom, const char *fmt, char def, double val);
extern T_varinfo *cherche_varinfo(T_irdata *irdata, const char *nom);

extern char lis_varinfo(
  T_irdata *irdata,
  T_varinfo *info,
  char *res_def, double *res_val
);

extern char lis_varinfo_tab(
  T_irdata *irdata,
  int idx_tab, char idx_def, double idx_val,
  char *res_def, double *res_val
);

extern void ecris_varinfo(T_irdata *irdata, T_varinfo *info, char def, double val);

extern void ecris_varinfo_tab(
  T_irdata *irdata,
  int idx_tab, int idx_def, double idx_val,
  char def, double val
);

extern T_varinfo *lis_tabaccess_varinfo(
  T_irdata *irdata, int idx_tab,
  char idx_def, double idx_val
);

extern char lis_tabaccess(
  T_irdata *irdata, int idx_tab,
  char idx_def, double idx_val,
  char *res_def, double *res_val
);

extern void ecris_tabaccess(
  T_irdata *irdata, int idx_tab,
  char idx_def, double idx_val,
  char def, double val
);

extern char lis_concaccess(
  T_irdata *irdata,
  char *nom, const char *fmt, char idx_def, double idx_val,
  char *res_def, double *res_val
);

extern T_varinfo *lis_concaccess_varinfo(
  T_irdata *irdata,
  char *nom, const char *fmt, char idx_def, double idx_val
);

extern void ecris_concaccess(
  T_irdata *irdata,
  char *nom, const char *fmt, char idx_def, double idx_val,
  char def, double val
);

extern void pr_var(T_print_context *pr_ctx, T_irdata *irdata, char *nom);
extern void pr_out_var(T_irdata *irdata, char *nom);
extern void pr_err_var(T_irdata *irdata, char *nom);

extern char est_variable(
  T_varinfo *info, char *nomCmp, char *res_def, double *res_val
);

extern char est_variable_tabaccess(
  T_irdata *irdata, int idx_tab,
  char idx_def, double idx_val,
  char *nomCmp, char *res_def, double *res_val
);

extern char est_variable_concaccess(
  T_irdata *irdata,
  char *nom, const char *fmt, char idx_def, double idx_val,
  char *nomCmp, char *res_def, double *res_val
);

|}

let gen_decl_functions fmt (cprog : Mir.program) =
  let functions = StrMap.bindings cprog.program_functions in
  let pp_args fmt args =
    List.iteri
      (fun i _ -> Pp.fpr fmt ", char arg_def%d, double arg_val%d" i i)
      args
  in
  Pp.fpr fmt "@[<v 0>%a@]@,"
    (Format.pp_print_list (fun fmt (fn, (fd : Mir.target)) ->
         Pp.fpr fmt
           "extern int %s(T_irdata* irdata, char *res_def, double *res_val%a);"
           fn pp_args fd.target_args))
    functions

let gen_decl_targets fmt (cprog : Mir.program) =
  let targets = StrMap.bindings cprog.program_targets in
  Pp.fpr fmt "@[<v 0>%a@]@,"
    (Format.pp_print_list (fun fmt (name, _) ->
         Pp.fpr fmt "extern struct S_discord *%s(T_irdata* irdata);" name))
    targets

let gen_mlang_h fmt cprog flags stats_varinfos =
  let pr form = Pp.fpr fmt form in
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
  Pp.fpr fmt "%s"
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
    Pp.fpr fmt "%s"
      {|if (strcmp(ref_erreur->isisf, "O") != 0 && ref_erreur->type == ANOMALIE) {
|}
  else Pp.fpr fmt "%s" {|if (ref_erreur->type == ANOMALIE) {
|};
  Pp.fpr fmt "%s"
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

int multimax_varinfo(
  T_irdata *irdata, T_varinfo *info,
  char nb_def, double nb_val,
  char *res_def, double *res_val
) {
  int i;
  int nb = (int)nb_val;
  char def;
  double val;
  *res_def = 0;
  *res_val = 0.0;
  if (irdata == NULL || info == NULL || info->tab_idx < 0 || nb_def == 0) return *res_def;
  for (i = 0; i < nb && i < info->size; i++) {
    lis_tabaccess(irdata, info->tab_idx, 1, (double)i, &def, &val);
    if (def == 1) *res_def = 1;
    if (val >= *res_val) *res_val = val;
  }
  return *res_def;
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
        Pp.fpr fmt "  dst->field_%s_var = src->field_%s_var;\n" f f
      else (
        Pp.fpr fmt "  dst->field_%s_def = src->field_%s_def;\n" f f;
        Pp.fpr fmt "  dst->field_%s_val = src->field_%s_val;\n" f f))
    cprog.program_event_fields;
  Pp.fpr fmt "%s"
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
    for (i = 0; i < pr_ctx->indent; i++) {
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

void init_saisie_espace(char *def, double *val) {
  if (def == NULL || val == NULL) return;
  init_tab(def, val, TAILLE_SAISIE);
}

void init_calculee(T_irdata *irdata) {
  if (irdata == NULL) return;
  init_tab(irdata->def_calculee, irdata->calculee, TAILLE_CALCULEE);
}

void init_calculee_espace(char *def, double *val) {
  if (def == NULL || val == NULL) return;
  init_tab(def, val, TAILLE_CALCULEE);
}

void init_base(T_irdata *irdata) {
  if (irdata == NULL) return;
  init_tab(irdata->def_base, irdata->base, TAILLE_BASE);
}

void init_base_espace(char *def, double *val) {
  if (def == NULL || val == NULL) return;
  init_tab(def, val, TAILLE_BASE);
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
  irdata->def_saisie = NULL;
  irdata->saisie = NULL;
  irdata->def_calculee = NULL;
  irdata->calculee = NULL;
  irdata->def_base = NULL;
  irdata->base = NULL;
|};
  IntMap.iter
    (fun _ (vsd : Com.variable_space) ->
      let sp = Pos.unmark vsd.vs_name in
      Com.CatVar.LocMap.iter
        (fun loc _ ->
          match loc with
          | Com.CatVar.LocInput ->
              Pp.fpr fmt
                "  if (irdata->def_saisie_%s != NULL) \
                 free(irdata->def_saisie_%s);@\n"
                sp sp;
              Pp.fpr fmt
                "  if (irdata->saisie_%s != NULL) free(irdata->saisie_%s);@\n"
                sp sp
          | Com.CatVar.LocComputed ->
              Pp.fpr fmt
                "  if (irdata->def_calculee_%s != NULL) \
                 free(irdata->def_calculee_%s);@\n"
                sp sp;
              Pp.fpr fmt
                "  if (irdata->calculee_%s != NULL) free(irdata->calculee_%s);@\n"
                sp sp
          | Com.CatVar.LocBase ->
              Pp.fpr fmt
                "  if (irdata->def_base_%s != NULL) free(irdata->def_base_%s);@\n"
                sp sp;
              Pp.fpr fmt
                "  if (irdata->base_%s != NULL) free(irdata->base_%s);@\n" sp sp)
        vsd.vs_cats)
    cprog.program_var_spaces_idx;
  IntMap.iter
    (fun id _ ->
      Pp.fpr fmt "  irdata->var_spaces[%d].def_saisie = NULL;@\n" id;
      Pp.fpr fmt "  irdata->var_spaces[%d].saisie = NULL;@\n" id;
      Pp.fpr fmt "  irdata->var_spaces[%d].def_calculee = NULL;@\n" id;
      Pp.fpr fmt "  irdata->var_spaces[%d].calculee = NULL;@\n" id;
      Pp.fpr fmt "  irdata->var_spaces[%d].def_base = NULL;@\n" id;
      Pp.fpr fmt "  irdata->var_spaces[%d].base = NULL;@\n" id)
    cprog.program_var_spaces_idx;
  Pp.fpr fmt
    {|
  if (irdata->tmps != NULL) free(irdata->tmps);
  if (irdata->def_tmps != NULL) free(irdata->def_tmps);
  if (irdata->info_tmps != NULL) free(irdata->info_tmps);
  if (irdata->ref != NULL) free(irdata->ref);
  if (irdata->def_ref != NULL) free(irdata->def_ref);
  if (irdata->ref_name != NULL) free(irdata->ref_name);
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
|};
  IntMap.iter
    (fun _ (vsd : Com.variable_space) ->
      let sp = Pos.unmark vsd.vs_name in
      Com.CatVar.LocMap.iter
        (fun loc _ ->
          let init_loc loc_str nb_str =
            Pp.fpr fmt "  irdata->def_%s_%s = NULL;@\n" loc_str sp;
            Pp.fpr fmt "  irdata->%s_%s = NULL;@\n" loc_str sp;
            Pp.fpr fmt "  if (TAILLE_%s > 0) {@\n" nb_str;
            Pp.fpr fmt
              "    irdata->def_%s_%s = (char *)malloc(TAILLE_%s * sizeof \
               (char));@\n"
              loc_str sp nb_str;
            Pp.fpr fmt
              "    if (irdata->def_%s_%s == NULL) goto erreur_cree_irdata;@\n"
              loc_str sp;
            Pp.fpr fmt
              "    irdata->%s_%s = (double *)malloc(TAILLE_%s * sizeof \
               (double));@\n"
              loc_str sp nb_str;
            Pp.fpr fmt
              "    if (irdata->%s_%s == NULL) goto erreur_cree_irdata;@\n"
              loc_str sp;
            Pp.fpr fmt
              "    init_%s_espace(irdata->def_%s_%s, irdata->%s_%s);@\n" loc_str
              loc_str sp loc_str sp;
            Pp.fpr fmt "  }@\n";
            if vsd.vs_by_default then (
              Pp.fpr fmt "  irdata->def_%s = irdata->def_%s_%s;@\n" loc_str
                loc_str sp;
              Pp.fpr fmt "  irdata->%s = irdata->%s_%s;@\n" loc_str loc_str sp)
          in
          match loc with
          | Com.CatVar.LocInput -> init_loc "saisie" "SAISIE"
          | Com.CatVar.LocComputed -> init_loc "calculee" "CALCULEE"
          | Com.CatVar.LocBase -> init_loc "base" "BASE")
        vsd.vs_cats)
    cprog.program_var_spaces_idx;
  IntMap.iter
    (fun id (vsd : Com.variable_space) ->
      let sp = Pos.unmark vsd.vs_name in
      Pp.fpr fmt "  irdata->var_spaces[%d].id = %d;@\n" id id;
      Pp.fpr fmt "  irdata->var_spaces[%d].name = \"%s\";@\n" id sp;
      Pp.fpr fmt
        "  irdata->var_spaces[%d].def_saisie = irdata->def_saisie_%s;@\n" id sp;
      Pp.fpr fmt "  irdata->var_spaces[%d].saisie = irdata->saisie_%s;@\n" id sp;
      Pp.fpr fmt
        "  irdata->var_spaces[%d].def_calculee = irdata->def_calculee_%s;@\n" id
        sp;
      Pp.fpr fmt "  irdata->var_spaces[%d].calculee = irdata->calculee_%s;@\n"
        id sp;
      Pp.fpr fmt "  irdata->var_spaces[%d].def_base = irdata->def_base_%s;@\n"
        id sp;
      Pp.fpr fmt "  irdata->var_spaces[%d].base = irdata->base_%s;@\n" id sp)
    cprog.program_var_spaces_idx;
  Pp.fpr fmt "%s"
    {|  irdata->tmps = NULL;
  irdata->def_tmps = NULL;
  irdata->info_tmps = NULL;
  if (TAILLE_TMP_VARS > 0) {
    irdata->tmps = (double *)malloc(TAILLE_TMP_VARS * sizeof (double));
    if (irdata->tmps == NULL) goto erreur_cree_irdata;
    irdata->def_tmps = (char *)malloc(TAILLE_TMP_VARS * sizeof (char));
    if (irdata->def_tmps == NULL) goto erreur_cree_irdata;
    irdata->info_tmps = (T_varinfo **)malloc(TAILLE_TMP_VARS * sizeof (T_varinfo *));    
    if (irdata->info_tmps == NULL) goto erreur_cree_irdata;
  }
  irdata->ref = NULL;
  irdata->def_ref = NULL;
  irdata->ref_name = NULL;
  irdata->info_ref = NULL;
  if (TAILLE_REFS > 0) {
    irdata->ref = (double **)malloc(TAILLE_REFS * (sizeof (double *)));
    if (irdata->ref == NULL) goto erreur_cree_irdata;
    irdata->def_ref = (char **)malloc(TAILLE_REFS * (sizeof (char *)));
    if (irdata->def_ref == NULL) goto erreur_cree_irdata;
    irdata->ref_name = (char **)malloc(TAILLE_REFS * (sizeof (char *)));
    if (irdata->ref_name == NULL) goto erreur_cree_irdata;
    irdata->info_ref = (T_varinfo **)malloc(TAILLE_REFS * (sizeof (T_varinfo *)));
    if (irdata->info_ref == NULL) goto erreur_cree_irdata;
  }
  irdata->tmps_org = 0;
  irdata->nb_tmps_target = 0;
  irdata->ref_org = 0;
  irdata->nb_refs_target = 0;
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

char *concat_nom_index(char *nom, const char *fmt, char def, double val) {
  char *res;
  int sz = 0;
  int szNom, szFmt;
  int idx = (int)val;
  int j, k;
  if (nom == NULL || fmt == NULL || def == 0 || idx < 0) return NULL;
  j = idx;
  while (j > 0) {
    j = j / 10;
    sz++;
  }
  szNom = strlen(nom);
  szFmt = strlen(fmt);
  sz = szNom + (szFmt > sz ? szFmt : sz);
  res = (char *)malloc((sz + 1) * (sizeof (char)));
  res[sz] = 0;
  for (k = 0; k < szNom; k++) {
    res[k] = nom[k];
  }
  for (k = 0; k < szFmt; k++) {
    res[szNom + k] = fmt[k];
  }
  j = idx;
  k = sz - 1;
  while (j > 0) {
    switch (j % 10) {
      case 0: res[k] = '0'; break;
      case 1: res[k] = '1'; break;
      case 2: res[k] = '2'; break;
      case 3: res[k] = '3'; break;
      case 4: res[k] = '4'; break;
      case 5: res[k] = '5'; break;
      case 6: res[k] = '6'; break;
      case 7: res[k] = '7'; break;
      case 8: res[k] = '8'; break;
      case 9: res[k] = '9'; break;
    }
    k--;
    j = j / 10;
  }
  return res;
}

T_varinfo *cherche_varinfo(T_irdata *irdata, const char *nom) {
  T_varinfo_map *map = NULL;
  int res = -1;
  int inf = 0;
  int sup = NB_variable + NB_saisie;
  int millieu = 0;
  int i;

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
  for (i = 1; i <= irdata->nb_tmps_target; i++) {
    T_varinfo *info = irdata->info_tmps[irdata->tmps_org - i];
    if (info != NULL && strcmp(nom, info->name) == 0) {
      return info;
    }
  }
  for (i = 1; i <= irdata->nb_refs_target; i++) {
    char *ref_name = irdata->ref_name[irdata->ref_org - i];
    if (strcmp(nom, ref_name) == 0) {
      return irdata->info_ref[irdata->ref_org - i];
    }
  }
  return NULL;
}

char lis_varinfo(
  T_irdata *irdata,
  T_varinfo *info,
  char *res_def, double *res_val
) {
  *res_def = 0;
  *res_val = 0.0;
  if (irdata == NULL || info == NULL) return *res_def;
  switch (info->loc_cat) {
    case EST_SAISIE:
      *res_def = irdata->def_saisie[info->idx];
      *res_val = irdata->saisie[info->idx];
      return *res_def;
    case EST_CALCULEE:
      *res_def = irdata->def_calculee[info->idx];
      *res_val = irdata->calculee[info->idx];
      return *res_def;
    case EST_BASE:
      *res_def = irdata->def_base[info->idx];
      *res_val = irdata->base[info->idx];
      return *res_def;
    case EST_TEMPORAIRE:
      *res_def = irdata->def_tmps[irdata->tmps_org + info->idx];
      *res_val = irdata->tmps[irdata->tmps_org + info->idx];
      return *res_def;
    default:
      return *res_def;
  }
}

char lis_varinfo_tab(
  T_irdata *irdata,
  int idx_tab, char idx_def, double idx_val,
  char *res_def, double *res_val
) {
  int idx = (int)idx_val;
  T_varinfo *info = NULL;
  *res_def = 0;
  *res_val = 0.0;
  if (irdata == NULL || idx_tab < 0 || TAILLE_TAB_VARINFO <= idx_tab) return *res_def;
  info = tab_varinfo[idx_tab];
  if (info == NULL || idx_def == 0 || info->size <= idx) return *res_def;
  if (idx <= 0) {
    *res_def = 1;
    return *res_def;
  }
  switch (info->loc_cat) {
    case EST_SAISIE:
      *res_def = irdata->def_saisie[info->idx + idx];
      *res_val = irdata->saisie[info->idx + idx];
      return *res_def;
    case EST_CALCULEE:
      *res_def = irdata->def_calculee[info->idx + idx];
      *res_val = irdata->calculee[info->idx + idx];
      return *res_def;
    case EST_BASE:
      *res_def = irdata->def_base[info->idx + idx];
      *res_val = irdata->base[info->idx + idx];
      return *res_def;
    case EST_TEMPORAIRE:
      *res_def = irdata->def_tmps[irdata->tmps_org + info->idx + idx];
      *res_val = irdata->tmps[irdata->tmps_org + info->idx + idx];
      return *res_def;
    default:
      return *res_def;
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
    case EST_TEMPORAIRE:
      return irdata->def_tmps[irdata->tmps_org + info->idx + idx];
    default:
      return 0;
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
    case EST_TEMPORAIRE:
      irdata->def_tmps[irdata->tmps_org + info->idx] = def;
      irdata->tmps[irdata->tmps_org + info->idx] = val;
      return;
    default:
      return;
  }
}

void ecris_varinfo_tab(
  T_irdata *irdata,
  int idx_tab, int idx_def, double idx_val,
  char def, double val
) {
  int idx = (int)idx_val;
  T_varinfo *info = NULL;
  if (irdata == NULL || idx_tab < 0 || TAILLE_TAB_VARINFO <= idx_tab) return;
  info = tab_varinfo[idx_tab];
  if (info == NULL || idx_def == 0 || idx < 0 || info->size <= idx) return;
  if (def == 0) {
    val = 0.0;
  } else {
    def = 1;
  }
  switch (info->loc_cat) {
    case EST_SAISIE:
      irdata->def_saisie[info->idx + idx] = def;
      irdata->saisie[info->idx + idx] = val;
      return;
    case EST_CALCULEE:
      irdata->def_calculee[info->idx + idx] = def;
      irdata->calculee[info->idx + idx] = val;
      return;
    case EST_BASE:
      irdata->def_base[info->idx + idx] = def;
      irdata->base[info->idx + idx] = val;
      return;
    case EST_TEMPORAIRE:
      irdata->def_tmps[irdata->tmps_org + info->idx + idx] = def;
      irdata->tmps[irdata->tmps_org + info->idx + idx] = val;
      return;
    default:
      return;
  }
}

T_varinfo *lis_tabaccess_varinfo(
  T_irdata *irdata, int idx_tab,
  char idx_def, double idx_val
) {
  T_varinfo *info = NULL;
  int idx = (int)idx_val;
  if (irdata == NULL || idx_tab < 0 || TAILLE_TAB_VARINFO <= idx_tab) return NULL;
  info = tab_varinfo[idx_tab];
  if (idx_def == 0 || idx < 0 || info->size <= idx) return NULL;
  return tab_varinfo[idx_tab + idx + 1];
}

char lis_tabaccess(
  T_irdata *irdata, int idx_tab,
  char idx_def, double idx_val,
  char *res_def, double *res_val
) {
  T_varinfo *info = lis_tabaccess_varinfo(irdata, idx_tab, idx_def, idx_val);
  int idx = 0;
  if (info == NULL) {
    *res_val = 0.0;
    if (
      irdata != NULL && 0 <= idx_tab && idx_tab < TAILLE_TAB_VARINFO
      && idx_def == 1 && ((int)idx_val) < 0
    ) {
      *res_def = 1;
    } else {
      *res_def = 0;
    }
    return *res_def;
  }
  lis_varinfo(irdata, info, res_def, res_val);
  /* tableau originel */
  {
    char res2_def;
    double res2_val;
    lis_varinfo_tab(irdata, idx_tab, idx_def, idx_val, &res2_def, &res2_val);
    if (*res_def != res2_def || *res_val != res2_val) {
      *res_def = res2_def;
      *res_val = res2_val;
      ecris_varinfo(irdata, info, *res_def, *res_val);
    }
  }
  return *res_def;
}

void ecris_tabaccess(
  T_irdata *irdata, int idx_tab,
  char idx_def, double idx_val,
  char def, double val
) {
  T_varinfo *info = lis_tabaccess_varinfo(irdata, idx_tab, idx_def, idx_val);
  ecris_varinfo(irdata, info, def, val);
  /* tableau originel */
  ecris_varinfo_tab(irdata, idx_tab, idx_def, idx_val, def, val);
}

char lis_concaccess(
  T_irdata *irdata,
  char *nom, const char *fmt, char idx_def, double idx_val,
  char *res_def, double *res_val
) {
  char *vn = concat_nom_index(nom, fmt, idx_def, idx_val);
  T_varinfo *info = cherche_varinfo(irdata, vn);
  *res_def = lis_varinfo(irdata, info, res_def, res_val);
  free(vn);
  return *res_def;
}

T_varinfo *lis_concaccess_varinfo(
  T_irdata *irdata,
  char *nom, const char *fmt, char idx_def, double idx_val
) {
  char *vn = concat_nom_index(nom, fmt, idx_def, idx_val);
  T_varinfo *info = cherche_varinfo(irdata, vn);
  free(vn);
  return info;
}

void ecris_concaccess(
  T_irdata *irdata,
  char *nom, const char *fmt, char idx_def, double idx_val,
  char def, double val
) {
  char *vn = concat_nom_index(nom, fmt, idx_def, idx_val);
  T_varinfo *info = cherche_varinfo(irdata, vn);
  free(vn);
  ecris_varinfo(irdata, info, def, val);
}

/* !!! */
void pr_var(T_print_context *pr_ctx, T_irdata *irdata, char *nom) {
  T_varinfo *info = NULL;
  char res_def = 0;
  double res_val = 0.0;

  if (pr_ctx == NULL) return;
  info = cherche_varinfo(irdata, nom);
  if (info == NULL) {
    fprintf(pr_ctx->std, "inconnu");
  } else {
    lis_varinfo(irdata, info, &res_def, &res_val);
    if (res_def == 0) {
      fprintf(pr_ctx->std, "indefini");
    } else {
      print_double(NULL, pr_ctx, res_val, 0, 30);
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

char est_variable(T_varinfo *info, char *nomCmp, char *res_def, double *res_val) {
  *res_def = 1;
  if (info == NULL || nomCmp == NULL) {
    *res_val = 0.0;
    return *res_def;
  }
  if (
    strcmp(info->name, nomCmp) == 0
    || (info->alias != NULL && strcmp(info->alias, nomCmp) == 0)
  ) {
    *res_val = 1.0;
    return *res_def;
  }
  *res_val = 0.0;
  return *res_def;
}

char est_variable_tabaccess(
  T_irdata *irdata, int idx_tab,
  char idx_def, double idx_val,
  char *nomCmp, char *res_def, double *res_val
) {
  T_varinfo *info = lis_tabaccess_varinfo(irdata, idx_tab, idx_def, idx_val);
  return est_variable(info, nomCmp, res_def, res_val);
}

char est_variable_concaccess(
  T_irdata *irdata,
  char *nom, const char *fmt, char idx_def, double idx_val,
  char *nomCmp, char *res_def, double *res_val
) {
  T_varinfo *info = lis_concaccess_varinfo(irdata, nom, fmt, idx_def, idx_val);
  return est_variable(info, nomCmp, res_def, res_val);
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

void aff_val(
  const char *nom, const T_irdata *irdata, int indice, int niv,
  const char *chaine, int is_tab, int expr, int maxi
) {
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
        pr "  lis_varinfo(irdata, info, res_def, res_val);\n")
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
