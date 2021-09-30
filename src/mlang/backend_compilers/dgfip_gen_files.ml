(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

module StringSet = Set.Make (String)

type flags = {
  (* -A *) nom_application : string;
  (* iliad, pro, oceans, bareme, batch *)
  (* -m *) annee_revenu : int;
  (* -P *) flg_correctif : bool;
  (* flg_correctif true by default, -P makes it false *)
  (* -R *) flg_iliad : bool;
  (* also implied by nom_application = "iliad"; disabled by -U *)
  (* -R *) flg_pro : bool;
  (* also implied by nom_application = "pro"; disabled by -U *)
  (* -U *) flg_cfir : bool;
  (* disabled by -R *)
  (* -b *) flg_gcos : bool;
  (* -b0 and -b1 ; disabled by -U and -R *)
  (* -b *) flg_tri_ebcdic : bool;
  (* -b1 only *)
  (* -M *) flg_multithread : bool;
  (* -s *) flg_short : bool;
  (* -r *) flg_register : bool;
  (* -O *) flg_optim_min_max : bool;
  (* -X *) flg_extraction : bool;
  (* -D *) flg_genere_libelle_restituee : bool;
  (* -S *) flg_controle_separe : bool;
  (* -I *) flg_controle_immediat : bool;
  (* unused *)
  (* -o *) flg_overlays : bool;
  (* -Z *) flg_colors : bool;
  (* -L *) flg_ticket : bool;
  (* -t *) flg_trace : bool;
  (* -g *) flg_debug : bool;
  (* also implied by -t *)
  (* -k *) nb_debug_c : int;
  (* -x *)
  xflg : bool;
      (* Flags to deal with in a particular way : -c compilation mode -l link mode -v specify the
         variable file (tgv.m) -e specify the error file (err.m) *)
      (* Other flags, not used in makefiles -h dir_var_h -i flg_ident -C flg_compact -K
         flg_optim_min_max -G flg_listing (+genere_cre = FALSE) -p flag_phase -f flg_ench_init -E
         cvt_file -g flg_debug -a flg_api -T flg_trace_irdata *)
}

type gen_opt = {
  with_verif : bool;
  with_classe : bool;
  with_priorite : bool;
  with_categorie_TL : bool;
  with_nat_code : bool;
  with_cotsoc : bool;
  with_ind_abat : bool;
  with_acompte : bool;
  with_avfisc : bool;
  with_rapcat : bool;
  with_sanction : bool;
  with_modcat : bool;
  with_liee : bool;
  with_libelle : bool;
  with_alias : bool;
  with_type : bool;
  with_type_donnee : bool;
  with_primrest : bool;
}

type idx = {
  out : int ref;
  comp : int ref;
  base : int ref;
  inp : int ref;
  ctxt : int ref;
  fam : int ref;
  rev : int ref;
  revc : int ref;
  var : int ref;
  pen : int ref;
}

type var_subtype = Context | Family | Income | CorrIncome | Variation | Penality | Base | Computed

type gen_type =
  | Computed of var_subtype option
  | Input of var_subtype option
  | Output (* can be of any subtype *)
  | Debug of int
(* can be of any subtype *)

let default_flags =
  {
    nom_application = "";
    annee_revenu = 0;
    flg_correctif = true;
    flg_iliad = false;
    flg_pro = false;
    flg_cfir = false;
    flg_gcos = false;
    flg_tri_ebcdic = false;
    flg_multithread = false;
    flg_short = false;
    flg_register = false;
    flg_optim_min_max = false;
    flg_extraction = false;
    flg_genere_libelle_restituee = false;
    flg_controle_separe = false;
    flg_controle_immediat = false;
    flg_overlays = false;
    flg_colors = false;
    flg_ticket = false;
    flg_trace = false;
    flg_debug = false;
    nb_debug_c = 0;
    xflg = false;
  }

let is_input st = match st with Base | Computed -> false | _ -> true

let is_computed st = match st with Base | Computed -> true | _ -> false

let input_var_subtype iv : var_subtype =
  match Pos.unmark iv.Mast.input_subtyp with
  | Mast.Context -> Context
  | Family -> Family
  | Penality -> Penality
  | Income -> Income
(* Missing CorrIncome and Variation (actually not used *)

let computed_var_subtype cv : var_subtype =
  let is_base =
    List.exists
      (fun ct -> match Pos.unmark ct with Mast.Base -> true | GivenBack -> false)
      cv.Mast.comp_subtyp
  in
  if is_base then Base else Computed

let computed_var_is_output cv =
  List.exists
    (fun st -> match Pos.unmark st with Mast.GivenBack -> true | Base -> false)
    cv.Mast.comp_subtyp

let subtype_name subtyp =
  match subtyp with
  | Context -> "contexte"
  | Family -> "famille"
  | Income -> "revenu"
  | CorrIncome -> "revenu_correc"
  | Variation -> "variation"
  | Penality -> "penalite"
  | Base -> assert false (* never used *)
  | Computed -> assert false
(* never used *)

let req_type_name req_type =
  match req_type with
  | Computed (Some typ) -> subtype_name typ
  | Computed None -> "calculee"
  | Input (Some typ) -> subtype_name typ
  | Input None -> "saisie"
  | Output -> "restituee"
  | Debug i when i <= 0 -> "debug"
  | Debug i -> Printf.sprintf "debug%02d" i

let new_idx () =
  {
    out = ref 0;
    comp = ref 0;
    base = ref 0;
    inp = ref 0;
    ctxt = ref 0;
    fam = ref 0;
    rev = ref 0;
    revc = ref 0;
    var = ref 0;
    pen = ref 0;
  }

(* Compute the variable indices in the different arrays according to its type *
(* Returns 3 indices :
   1 - Index in the Computed/Base/Input arrays of the TGV
   2 - Index in the Context/Family/Income/... arrays *)
   3 - Index in the Restituee array *)
let next_idx idx kind output size =
  let idxr1, idxr2 =
    match (kind : var_subtype) with
    | Computed -> (idx.comp, idx.comp)
    | Base -> (idx.base, idx.base)
    | Context -> (idx.inp, idx.ctxt)
    | Family -> (idx.inp, idx.fam)
    | Income -> (idx.inp, idx.rev)
    | CorrIncome -> (idx.inp, idx.revc)
    | Variation -> (idx.inp, idx.var)
    | Penality -> (idx.inp, idx.pen)
  in
  let idx1, idx2 = (!idxr1, !idxr2) in
  idxr1 := !idxr1 + size;
  if idxr1 != idxr2 then idxr2 := !idxr2 + size;
  let idxo_opt =
    if output then (
      let idxo = !(idx.out) in
      idx.out := !(idx.out) + size;
      Some idxo)
    else None
  in
  (idx1, idx2, idxo_opt)

let get_attr a attributes =
  let attr_opt = List.find_opt (fun (an, _al) -> Pos.unmark an = a) attributes in
  match attr_opt with
  | None -> if a = "primrest" then 1 else -1
  | Some (_an, al) -> ( match Pos.unmark al with Mast.Float f -> int_of_float f | _ -> 0)

let get_name name alias_opt = match alias_opt with Some alias -> alias | _ -> name

let sort_vars_by_alias vars =
  List.fast_sort
    (fun (_, _, _, _, name1, alias_opt1, _, _, _, _) (_, _, _, _, name2, alias_opt2, _, _, _, _) ->
      let var_name1 = get_name name1 alias_opt1 in
      let var_name2 = get_name name2 alias_opt2 in
      String.compare var_name1 var_name2)
    vars

let sort_vars_by_name vars =
  List.fast_sort
    (fun (_, _, _, _, name1, _, _, _, _, _) (_, _, _, _, name2, _, _, _, _, _) ->
      String.compare name1 name2)
    vars

let get_vars prog =
  let open Mast in
  let idx = new_idx () in
  let vars =
    List.fold_left
      (fun vars file ->
        List.fold_left
          (fun vars item ->
            match Pos.unmark item with
            | VariableDecl (ComputedVar cv) ->
                let cv = Pos.unmark cv in
                let tvar = computed_var_subtype cv in
                (* Base or Computed *)
                let size = match cv.comp_table with Some i -> Pos.unmark i | None -> 1 in
                let idx1, idx2, idxo_opt = next_idx idx tvar (computed_var_is_output cv) size in
                let var =
                  ( tvar,
                    idx1,
                    idx2,
                    idxo_opt,
                    Pos.unmark cv.comp_name,
                    None,
                    Pos.unmark cv.comp_description,
                    cv.comp_typ,
                    cv.comp_attributes,
                    size )
                in
                var :: vars
            | VariableDecl (InputVar iv) ->
                let iv = Pos.unmark iv in
                let tvar = input_var_subtype iv in
                let idx1, idx2, idxo_opt = next_idx idx tvar iv.input_given_back 1 in
                let var =
                  ( tvar,
                    idx1,
                    idx2,
                    idxo_opt,
                    Pos.unmark iv.input_name,
                    Some (Pos.unmark iv.input_alias),
                    Pos.unmark iv.input_description,
                    iv.input_typ,
                    iv.input_attributes,
                    1 )
                in
                var :: vars
            | _ -> vars)
          vars file)
      [] prog
  in

  let vars = sort_vars_by_alias vars in

  let idx = new_idx () in

  (* Recompute the indices of Context/Family/Income/... vars, as they are sorted by alias *)
  List.map
    (fun (tvar, idx1, _idx2, idxo_opt, name, alias_opt, desc, typ_opt, attributes, size) ->
      let _idx1, idx2, _idxo_opt = next_idx idx tvar (idxo_opt <> None) size in
      (tvar, idx1, idx2, idxo_opt, name, alias_opt, desc, typ_opt, attributes, size))
    vars

let get_vars_debug vars =
  sort_vars_by_name
  @@ List.fold_left
       (fun vars var ->
         let tvar, idx1, idx2, idxo_opt, _name, alias_opt, desc, typ_opt, attributes, size = var in
         match alias_opt with
         | Some alias ->
             (tvar, idx1, idx2, idxo_opt, alias, alias_opt, desc, typ_opt, attributes, size)
             :: var :: vars
         | None -> var :: vars)
       [] vars

let split_list lst cnt =
  let size = List.length lst in
  let rec aux l nl s sz nll c =
    let nl, s, sz, nll, c =
      if s < sz then (nl, s, sz, nll, c)
      else
        let sz = ((c + 1) * size / cnt) - (c * size / cnt) in
        ([], 0, sz, List.rev nl :: nll, c + 1)
    in
    match l with
    | [] ->
        assert (nl = []);
        List.rev nll
    | x :: l -> aux l (x :: nl) (s + 1) sz nll c
  in
  if cnt <= 1 then [ lst ]
  else
    let sz = size / cnt in
    aux lst [] 0 sz [] 0

let gen_header fmt =
  Format.fprintf fmt
    {|/****** LICENCE CECIL *****/

#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <math.h>
#include "var.h"

|}

let gen_var fmt req_type opt ~idx ~name ~tvar ~is_output ~typ_opt ~attributes ~desc ~alias_opt =
  let open Mast in
  let var_name = if opt.with_alias then get_name name alias_opt else name in

  (* TODO if flg_compact is used, then handle flat representation of TGV *)
  let kind, is_input =
    match (tvar : var_subtype) with
    | Computed -> ("EST_CALCULEE", false)
    | Base -> ("EST_BASE", false)
    | _ -> ("EST_SAISIE", true)
  in

  let typ = match typ_opt with None -> Real | Some ct -> Pos.unmark ct in

  Format.fprintf fmt "    { \"%s\", %s | %d" var_name kind idx;
  if opt.with_type_donnee then Format.fprintf fmt ", %a" Format_mast.format_value_typ typ;
  if opt.with_verif then
    if is_input && false then Format.fprintf fmt ", err_%s" name (* Note: no alias *)
    else Format.fprintf fmt ", no_error";
  (* does not seem to be used anymore... *)
  if opt.with_classe then Format.fprintf fmt ", %d" (get_attr "classe" attributes);
  if opt.with_priorite then Format.fprintf fmt ", %d" (get_attr "priorite" attributes);
  if opt.with_categorie_TL then Format.fprintf fmt ", %d" (get_attr "categorie_TL" attributes);
  if opt.with_nat_code then Format.fprintf fmt ", %d" (get_attr "nat_code" attributes);
  if opt.with_cotsoc then Format.fprintf fmt ", %d" (get_attr "cotsoc" attributes);
  if opt.with_ind_abat then Format.fprintf fmt ", %d" (get_attr "ind_abat" attributes);
  if opt.with_acompte then Format.fprintf fmt ", %d" (get_attr "acompte" attributes);
  if opt.with_avfisc then Format.fprintf fmt ", %d" (get_attr "avfisc" attributes);
  if opt.with_rapcat then Format.fprintf fmt ", %d" (get_attr "rapcat" attributes);
  if opt.with_sanction then Format.fprintf fmt ", %d" (get_attr "sanction" attributes);
  if opt.with_modcat then Format.fprintf fmt ", %d" (get_attr "modcat" attributes);
  if opt.with_liee then
    if true (* no linked var *) then Format.fprintf fmt ", (T_var_irdata)NULL"
    else Format.fprintf fmt ", desc_%s" (assert false);
  (* only REVENU vars may use this, but they don't... *)
  if opt.with_type && is_output then Format.fprintf fmt ", RESTITUEE";
  (* there also exist RESTITUEE_P and RESTITUEE_C, but they are unused *)
  if opt.with_primrest then Format.fprintf fmt ", %d" (get_attr "primrest" attributes);
  if opt.with_libelle then Format.fprintf fmt ", \"%s\"" desc
  else Format.fprintf fmt " /*\"%s\"*/" desc;
  begin
    match (req_type, tvar) with
    | Input _, Income -> Format.fprintf fmt ", \"%s\"" name
    | _ -> ()
  end;
  Format.fprintf fmt " },\n"

let var_matches req_type var_type is_output =
  match req_type with
  | Computed (Some rt) -> is_computed var_type && rt = var_type
  | Computed None -> is_computed var_type
  | Input (Some rt) -> is_input var_type && rt = var_type
  | Input None -> is_input var_type
  | Output -> is_output
  | Debug _i -> true

let gen_table fmt _flags vars req_type opt =
  gen_header fmt;

  (* if opt.with_verif then *)
  Format.fprintf fmt "extern T_discord *no_error(T_irdata *);\n";

  (* TODO there should be individual var verification functions here, but they do not seem to be
     used (for all kind of input vars as well as output vars and debug tables) *)
  let vars =
    if opt.with_alias then vars (* already sorted by alias *) else sort_vars_by_name vars
  in

  let table_name = req_type_name req_type in
  let table_NAME = String.uppercase_ascii table_name in
  begin
    match req_type with
    | Debug _i -> Format.fprintf fmt "T_desc_debug desc_%s[NB_%s + 1] = {\n" table_name table_NAME
    | _ -> Format.fprintf fmt "T_desc_%s desc_%s[NB_%s + 1] = {\n" table_name table_name table_NAME
  end;

  List.iter
    (fun (tvar, idx1, _idx2, idxo_opt, name, alias_opt, desc, typ_opt, attributes, _size) ->
      let is_output = match idxo_opt with Some _ -> true | _ -> false in
      if var_matches req_type tvar is_output then
        match req_type with
        | Debug _i ->
            (* Special case for debug *)
            let opt = { opt with with_alias = false } in
            gen_var fmt req_type opt ~idx:idx1 ~name ~tvar ~is_output ~typ_opt ~attributes ~desc
              ~alias_opt
        | _ ->
            (* General case*)
            gen_var fmt req_type opt ~idx:idx1 ~name ~tvar ~is_output ~typ_opt ~attributes ~desc
              ~alias_opt)
    vars;

  Format.fprintf fmt "};\n"

let gen_desc fmt vars ~alias_only =
  let vars = sort_vars_by_name vars in

  Format.fprintf fmt {|/****** LICENCE CECIL *****/

#include "desc_static.h"

|};

  List.iter
    (fun (tvar, _idx1, idx2, idxo_opt, name, alias_opt, _desc, _typ_opt, _attributes, _size) ->
      if (not alias_only) || alias_opt <> None then
        let data_opt =
          match (tvar : var_subtype) with
          | Base | Computed -> begin
              (* computed var: only output *)
              match idxo_opt with Some idx -> Some ("restituee", idx) | None -> None
            end
          | _ -> Some (subtype_name tvar, idx2)
        in
        match data_opt with
        | Some (type_name, idx) ->
            let var_name =
              match (alias_only, alias_opt) with true, Some alias -> alias | _ -> name
            in
            (* TODO special handling for debug vars (though it does not seem to happen) *)
            Format.fprintf fmt "#define desc_%s (T_var_irdata)(desc_%s + %d)\n" var_name type_name
              idx
        | None -> ())
    vars

(* TODO when flg_controle_immediat, add per variable verifications (add #define desc_verif) although
   it does not seem to be used anymore *)

let gen_table_output fmt flags vars =
  let opt =
    {
      with_verif = false;
      with_classe = false;
      with_priorite = false;
      with_categorie_TL = false;
      with_nat_code = false;
      with_cotsoc = false;
      with_ind_abat = false;
      with_acompte = false;
      with_avfisc = false;
      with_rapcat = false;
      with_sanction = false;
      with_modcat = false;
      with_liee = false;
      with_libelle = flags.flg_genere_libelle_restituee;
      with_alias = false;
      with_type = true;
      with_type_donnee = true;
      with_primrest = true;
    }
  in

  gen_table fmt flags vars Output opt

let gen_table_context fmt flags vars =
  let opt =
    {
      with_verif = true;
      with_classe = true;
      with_priorite = true;
      with_categorie_TL = true;
      with_nat_code = false;
      with_cotsoc = false;
      with_ind_abat = false;
      with_acompte = false;
      with_avfisc = false;
      with_rapcat = false;
      with_sanction = false;
      with_modcat = true;
      with_liee = false;
      with_libelle = true;
      with_alias = true;
      with_type = false;
      with_type_donnee = true;
      with_primrest = false;
    }
  in

  gen_table fmt flags vars (Input (Some Context)) opt

let gen_table_family fmt flags vars =
  let opt =
    {
      with_verif = true;
      with_classe = true;
      with_priorite = true;
      with_categorie_TL = true;
      with_nat_code = true;
      with_cotsoc = false;
      with_ind_abat = false;
      with_acompte = false;
      with_avfisc = false;
      with_rapcat = false;
      with_sanction = false;
      with_modcat = true;
      with_liee = false;
      with_libelle = flags.flg_pro || flags.flg_iliad;
      with_alias = true;
      with_type = false;
      with_type_donnee = true;
      with_primrest = false;
    }
  in

  gen_table fmt flags vars (Input (Some Family)) opt

let gen_table_income fmt flags vars =
  let opt =
    {
      with_verif = true;
      with_classe = true;
      with_priorite = true;
      with_categorie_TL = true;
      with_nat_code = true;
      with_cotsoc = true;
      with_ind_abat = true;
      with_acompte = true;
      with_avfisc = true;
      with_rapcat = true;
      with_sanction = true;
      with_modcat = true;
      with_liee = true;
      with_libelle = flags.flg_pro || not flags.flg_gcos;
      with_alias = true;
      with_type = false;
      with_type_donnee = true;
      with_primrest = false;
    }
  in

  gen_table fmt flags vars (Input (Some Income)) opt

let gen_table_corrincome fmt flags vars =
  let opt =
    {
      with_verif = true;
      with_classe = true;
      with_priorite = true;
      with_categorie_TL = true;
      with_nat_code = true;
      with_cotsoc = true;
      with_ind_abat = true;
      with_acompte = true;
      with_avfisc = true;
      with_rapcat = true;
      with_sanction = true;
      with_modcat = true;
      with_liee = true;
      with_libelle = true;
      with_alias = true;
      with_type = false;
      with_type_donnee = true;
      with_primrest = false;
    }
  in

  gen_table fmt flags vars (Input (Some CorrIncome)) opt

let gen_table_variation fmt flags vars =
  let opt =
    {
      with_verif = true;
      with_classe = true;
      with_priorite = false;
      with_categorie_TL = false;
      with_nat_code = false;
      with_cotsoc = false;
      with_ind_abat = false;
      with_acompte = false;
      with_avfisc = false;
      with_rapcat = false;
      with_sanction = false;
      with_modcat = false;
      with_liee = false;
      with_libelle = false;
      with_alias = true;
      with_type = false;
      with_type_donnee = true;
      with_primrest = false;
    }
  in

  gen_table fmt flags vars (Input (Some Variation)) opt

let gen_table_penality fmt flags vars =
  let opt =
    {
      with_verif = true;
      with_classe = false;
      with_priorite = false;
      with_categorie_TL = false;
      with_nat_code = false;
      with_cotsoc = false;
      with_ind_abat = false;
      with_acompte = false;
      with_avfisc = false;
      with_rapcat = false;
      with_sanction = false;
      with_modcat = false;
      with_liee = false;
      with_libelle = false;
      with_alias = true;
      with_type = false;
      with_type_donnee = true;
      with_primrest = false;
    }
  in

  gen_table fmt flags vars (Input (Some Penality)) opt

let gen_table_debug fmt flags vars i =
  let opt =
    {
      with_verif = true;
      with_classe = true;
      with_priorite = true;
      with_categorie_TL = true;
      with_nat_code = true;
      with_cotsoc = true;
      with_ind_abat = true;
      with_acompte = true;
      with_avfisc = true;
      with_rapcat = true;
      with_sanction = true;
      with_modcat = true;
      with_liee = true;
      with_libelle = false;
      with_alias = false;
      with_type = false;
      with_type_donnee = true;
      with_primrest = false;
    }
  in

  gen_table fmt flags vars (Debug i) opt

let is_valid_app al = List.exists (fun a -> String.equal (Pos.unmark a) "iliad") al

let get_rules_verif_etc prog =
  let open Mast in
  let rules, verifs, errors, chainings =
    List.fold_left
      (fun (rules, verifs, errors, chainings) file ->
        List.fold_left
          (fun (rules, verifs, errors, chainings) item ->
            match Pos.unmark item with
            | Rule r ->
                let rules, chainings =
                  if is_valid_app r.rule_applications then
                    ( rule_number r.rule_name :: rules,
                      match r.rule_chaining with
                      | None -> chainings
                      | Some cn -> StringSet.add (Pos.unmark cn) chainings )
                  else (rules, chainings)
                in
                (rules, verifs, errors, chainings)
            | Verification v ->
                let verifs =
                  if is_valid_app v.verif_applications then
                    fst
                    @@ List.fold_left
                         (fun (verifs, vn) _vc -> (vn :: verifs, vn + 1))
                         (verifs, verification_number v.verif_name)
                         v.verif_conditions
                  else verifs
                in
                (rules, verifs, errors, chainings)
            | Error e -> (rules, verifs, e :: errors, chainings)
            (* | Chaining (cn, _anl) -> rules, verifs, errors, cn :: chainings *)
            | _ -> (rules, verifs, errors, chainings))
          (rules, verifs, errors, chainings)
          file)
      ([], [], [], StringSet.empty) prog
  in

  let rules = List.fast_sort compare rules in
  let verifs = List.fast_sort compare verifs in
  let errors = List.fast_sort compare errors in

  (* let chainings = List.fast_sort compare chainings in *)
  (rules, verifs, errors, chainings)

let gen_table_call fmt flags vars_debug rules chainings errors =
  let open Mast in
  gen_header fmt;

  if flags.flg_debug then begin
    if flags.nb_debug_c <= 0 then gen_table_debug fmt flags vars_debug 0;

    List.iter (fun rn -> Format.fprintf fmt "extern int regle_%d();\n" rn) rules;

    Format.fprintf fmt "T_desc_call desc_call[NB_CALL + 1] = {\n";
    List.iter (fun rn -> Format.fprintf fmt "    { %d, regle_%d },\n" rn rn) rules;
    Format.fprintf fmt "};\n\n";

    Format.fprintf fmt "T_desc_err desc_err[NB_ERR + 1] = {\n";
    List.iter
      (fun e ->
        let en = Pos.unmark e.error_name in
        Format.fprintf fmt "    { \"%s\", &erreur_%s },\n" en en)
      errors;
    Format.fprintf fmt "};\n\n"
  end;

  StringSet.iter (fun cn -> Format.fprintf fmt "extern void %s();\n" cn) chainings;

  Format.fprintf fmt "T_desc_ench desc_ench[NB_ENCH + 1] = {\n";
  StringSet.iter (fun cn -> Format.fprintf fmt "    { \"%s\", %s },\n" cn cn) chainings;
  Format.fprintf fmt "};\n"

let gen_table_verif fmt flags verifs =
  gen_header fmt;

  if flags.flg_debug || flags.flg_controle_immediat then begin
    (* TODO: when control_immediat, don' put everything (but what ?) *)
    List.iter (fun vn -> Format.fprintf fmt "extern void verif_%d();\n" vn) verifs;

    Format.fprintf fmt "T_desc_verif desc_verif[NB_VERIF + 1] = {\n";
    List.iter (fun vn -> Format.fprintf fmt "    { %d, verif_%d },\n" vn vn) verifs;
    Format.fprintf fmt "};\n\n"
  end

let count vars req_type =
  List.fold_left
    (fun cpt (tvar, _, _, idxo_opt, _, _, _, _, _, size) ->
      let is_output = match idxo_opt with Some _ -> true | _ -> false in
      if var_matches req_type tvar is_output then cpt + size else cpt)
    0 vars

let gen_var_h fmt flags vars vars_debug rules verifs chainings errors =
  let open Mast in
  (* TODO paths may differ if dir_var_h is set *)
  Format.fprintf fmt
    {|/****** LICENCE CECIL *****/

#include "irdata.h"
#include "desc_inv.h"
#include "const.h"
#include "dbg.h"
#include "annee.h"
|};

  let taille_saisie = count vars (Input None) in
  let taille_calculee = count vars (Computed (Some Computed)) in
  let taille_base = count vars (Computed (Some Base)) in
  let taille_totale = taille_saisie + taille_calculee + taille_base in
  let nb_contexte = count vars (Input (Some Context)) in
  let nb_famille = count vars (Input (Some Family)) in
  let nb_revenu = count vars (Input (Some Income)) in
  let nb_revenu_correc = count vars (Input (Some CorrIncome)) in
  let nb_variation = count vars (Input (Some Variation)) in
  let nb_penalite = count vars (Input (Some Penality)) in
  let nb_restituee = count vars Output in
  let nb_ench = StringSet.cardinal chainings in
  let nb_err = List.length errors in
  let nb_debug = List.map List.length vars_debug in
  let nb_call = List.length rules in
  let nb_verif = List.length verifs in

  Format.fprintf fmt
    {|
#define TAILLE_SAISIE %d
#define TAILLE_CALCULEE %d
#define TAILLE_BASE %d
#define TAILLE_TOTALE %d
#define NB_CONTEXTE %d
#define NB_FAMILLE %d
#define NB_REVENU %d
#define NB_REVENU_CORREC %d
#define NB_VARIATION %d
#define NB_PENALITE %d
#define NB_RESTITUEE %d
#define NB_ENCH %d
|}
    taille_saisie taille_calculee taille_base taille_totale nb_contexte nb_famille nb_revenu
    nb_revenu_correc nb_variation nb_penalite nb_restituee nb_ench;

  if flags.flg_debug then begin
    Format.fprintf fmt "#define NB_ERR %d\n" nb_err;
    (if flags.nb_debug_c <= 0 then
     let nb = match nb_debug with [ nb ] -> nb | _ -> assert false in
     Format.fprintf fmt "#define NB_DEBUG %d\n" nb
    else
      let i =
        List.fold_left
          (fun i nb ->
            Format.fprintf fmt "#define NB_DEBUG%02d %d\n" i nb;
            i + 1)
          1 nb_debug
      in
      assert (i = flags.nb_debug_c + 1));
    Format.fprintf fmt "#define NB_CALL %d\n" nb_call
  end;

  if flags.flg_debug || flags.flg_controle_immediat then
    Format.fprintf fmt "#define NB_VERIF %d\n" nb_verif;

  List.iter
    (fun rn -> Format.fprintf fmt "extern int regle_%d _PROTS((struct S_irdata *));\n" rn)
    rules;

  List.iter
    (fun vn -> Format.fprintf fmt "extern void verif_%d _PROTS((struct S_irdata *));\n" vn)
    verifs;

  (* TODO external declaration of individual control rules (seems to be no longer used) *)
  List.iter
    (fun e ->
      let en = Pos.unmark e.error_name in
      Format.fprintf fmt "extern T_erreur erreur_%s;\n" en)
    errors;

  (* TODO function declarations (seems to be no longer used) *)
  if flags.flg_pro then Format.fprintf fmt "extern struct S_erreur *tabErreurs[];\n"

let gen_var_c fmt flags errors =
  let open Mast in
  gen_header fmt;

  Format.fprintf fmt "#include \"var_static.c\"\n\n";

  (* TODO before 2006, the format is slightly different *)
  List.iter
    (fun e ->
      match e.error_descr with
      | [ famille; code_bo; sous_code; libelle; is_isf ] ->
          let terr =
            match Pos.unmark e.error_typ with
            | Anomaly -> 1 (* also called blocking *)
            | Discordance -> 2
            | Information -> 4
          in
          let sous_code_suffix =
            if String.equal (Pos.unmark sous_code) "00" then "" else "-" ^ Pos.unmark sous_code
          in
          Format.fprintf fmt
            "T_erreur erreur_%s = { \"%s%s%s / %s\", \"%s\", \"%s\", \"%s\", \"%s\", %d };\n"
            (Pos.unmark e.error_name) (Pos.unmark famille) (Pos.unmark code_bo) sous_code_suffix
            (Pos.unmark libelle) (Pos.unmark code_bo) (Pos.unmark sous_code) (Pos.unmark is_isf)
            (Pos.unmark e.error_name) terr
      | _ -> failwith "Invalid error description")
    errors;

  if flags.flg_pro || flags.flg_iliad then begin
    Format.fprintf fmt "T_erreur *tabErreurs[] = {\n";

    List.iter (fun e -> Format.fprintf fmt "    &erreur_%s,\n" (Pos.unmark e.error_name)) errors;

    Format.fprintf fmt "    NULL\n};\n"
  end

let gen_annee_h fmt flags =
  Format.fprintf fmt {|/****** LICENCE CECIL *****/

#define ANNEE_REVENU %04d
|} flags.annee_revenu;

  Format.pp_print_flush fmt ()

let gen_conf_h fmt flags vars =
  Format.fprintf fmt
    {|/****** LICENCE CECIL *****/

#ifndef _CONF_H_
#define _CONF_H_

/* Configuration flags; do not change */

|};

  if flags.flg_correctif then Format.fprintf fmt "#define FLG_CORRECTIF\n";
  if flags.flg_iliad then Format.fprintf fmt "#define FLG_ILIAD\n";
  if flags.flg_pro then Format.fprintf fmt "#define FLG_PRO\n";
  if flags.flg_cfir then Format.fprintf fmt "#define FLG_CFIR\n";
  if flags.flg_gcos then Format.fprintf fmt "#define FLG_GCOS\n";
  if flags.flg_tri_ebcdic then Format.fprintf fmt "#define FLG_TRI_EBCDIC\n";
  if flags.flg_multithread then Format.fprintf fmt "#define FLG_MULTITHREAD\n";
  (* flag is not used *)
  (*if flags.flg_compact then Format.fprintf fmt "#define FLG_COMPACT\n"; *)
  if flags.flg_short then Format.fprintf fmt "#define FLG_SHORT\n";
  if flags.flg_register then Format.fprintf fmt "#define FLG_REGISTER\n";
  (* flag is not used *)
  (*if flags.flg_optim_min_max then Format.fprintf fmt "#define FLG_OPTIM_MIN_MAX\n"; *)
  if flags.flg_extraction then Format.fprintf fmt "#define FLG_EXTRACTION\n";
  if flags.flg_genere_libelle_restituee then
    Format.fprintf fmt "#define FLG_GENERE_LIBELLE_RESTITUEE\n";
  if flags.flg_controle_separe then Format.fprintf fmt "#define FLG_CONTROLE_SEPARE\n";
  if flags.flg_controle_immediat then Format.fprintf fmt "#define FLG_CONTROLE_IMMEDIAT\n";
  (* does not need to be printed *)
  (*if flags.flg_overlays then Format.fprintf fmt "#define FLG_OVERLAYS\n"; *)
  if flags.flg_colors then Format.fprintf fmt "#define FLG_COLORS\n";
  if flags.flg_ticket then Format.fprintf fmt "#define FLG_TICKET\n";
  if flags.flg_trace then Format.fprintf fmt "#define FLG_TRACE\n";
  (* flag is not used *)
  (*if flags.flg_trace_irdata then Format.fprintf fmt "#define FLG_TRACE_IRDATA\n"; *)
  if flags.flg_debug then Format.fprintf fmt "#define FLG_DEBUG\n";
  Format.fprintf fmt "#define NB_DEBUG_C  %d\n" flags.nb_debug_c;

  let nb_saisie = count vars (Input None) in
  let nb_calculee = count vars (Computed (Some Computed)) in
  let nb_base = count vars (Computed (Some Base)) in
  let nb_vars = nb_saisie + nb_calculee + nb_base in
  Format.fprintf fmt "#define NB_VARS  %d\n" nb_vars;

  Format.fprintf fmt "#endif _CONF_H_\n"


let extract_var_ids vars =
  let open Dgfip_varid in
  let process_var ~alias
      (tvar, idx1, _idx2, _idxo_opt, name, alias_opt, _desc, _typ_opt, _attributes, _size) =
    let v =
      match (tvar : var_subtype) with
      | Computed -> VarComputed idx1
      | Base -> VarBase idx1
      | _ -> VarInput idx1
    in
    let name =
      if alias then
        match alias_opt with
        | Some alias -> alias
        | None -> name
      else
        name
    in
    name, v
  in
  let vm =
    List.fold_left (fun vm vd ->
        let name, v = process_var ~alias:false vd in
        StringMap.add name v vm
    ) StringMap.empty vars
  in
  let vam =
    List.fold_left (fun vm vd ->
        let name, v = process_var ~alias:true vd in
        StringMap.add name v vm
    ) StringMap.empty vars
  in
  vm, vam

let open_file filename =
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  (oc, fmt)

(* Generate the auxiliary files AND return two maps:
   1 - map of variables names to TGV ids
   2 - map of variables aliases (or names when unavailable) to TGV ids *)
let generate_auxiliary_files prog : Dgfip_varid.var_id_map * Dgfip_varid.var_id_map =
  let flags =
    {
      default_flags with
      nom_application = "iliad";
      annee_revenu = 2020;
      flg_gcos = false;
      flg_iliad = true;
      flg_pro = false;
      flg_multithread = false;
      flg_debug = false;
      nb_debug_c = 4;
    }
  in

  let folder = Filename.dirname !Cli.output_file in

  let vars = get_vars prog in

  let oc, fmt = open_file (Filename.concat folder "restitue.c") in
  gen_table_output fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "contexte.c") in
  gen_table_context fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "famille.c") in
  gen_table_family fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "revenu.c") in
  gen_table_income fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "revcor.c") in
  gen_table_corrincome fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "variatio.c") in
  gen_table_variation fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "penalite.c") in
  gen_table_penality fmt flags vars;
  close_out oc;

  let vars_debug = get_vars_debug vars in
  let vars_debug_split = split_list vars_debug flags.nb_debug_c in
  let _ =
    if flags.nb_debug_c > 0 then
      List.fold_left
        (fun i vars ->
          let file = Printf.sprintf "tableg%02d.c" i in
          let oc, fmt = open_file (Filename.concat folder file) in
          if flags.flg_debug then gen_table_debug fmt flags vars i else gen_header fmt;
          close_out oc;
          i + 1)
        1 vars_debug_split
    else 0
  in

  let oc, fmt = open_file (Filename.concat folder "desc.h") in
  gen_desc fmt vars ~alias_only:true;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "desc_inv.h") in
  gen_desc fmt vars ~alias_only:false;
  close_out oc;

  let rules, verifs, errors, chainings = get_rules_verif_etc prog in

  let oc, fmt = open_file (Filename.concat folder "tableg.c") in
  gen_table_call fmt flags vars_debug rules chainings errors;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "tablev.c") in
  gen_table_verif fmt flags verifs;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "var.h") in
  gen_var_h fmt flags vars vars_debug_split rules verifs chainings errors;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "var.c") in
  gen_var_c fmt flags errors;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "annee.h") in
  gen_annee_h fmt flags;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "conf.h") in
  gen_conf_h fmt flags vars;
  close_out oc;

  let vm, vma = extract_var_ids vars in

  vm, vma
