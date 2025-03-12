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

(* Various flags used to control wicch data to put in each variable array *)
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

(* Used to compute array indices for each category of variable *)
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

type var_subtype =
  | Context
  | Family
  | Income
  | CorrIncome
  | Variation
  | Penality
  | Base
  | Computed

(* Used to specify the type of array to generate *)
type gen_type =
  | Computed of var_subtype option
  | Input of var_subtype option
  | Output (* can be of any subtype *)
  | Debug of int
(* can be of any subtype *)

let is_input st = match st with Base | Computed -> false | _ -> true

let is_computed st = match st with Base | Computed -> true | _ -> false

(* Used to generated the array names *)
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

(* Used to generated the array names *)
let req_type_name req_type =
  match req_type with
  | Computed (Some typ) -> subtype_name typ
  | Computed None -> Mast.computed_category
  | Input (Some typ) -> subtype_name typ
  | Input None -> Mast.input_category
  | Output -> Mast.givenback_category
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

(* Compute the variable indices in the different arrays according to its type *)
(* Returns 3 indices: 1 - Index in the Computed/Base/Input arrays of the TGV 2 -
   Index in the Context/Family/Income/... arrays 3 - Index in the Restituee
   array *)
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
  match StrMap.find_opt a attributes with
  | None -> if a = "primrest" then 1 else -1
  | Some al -> Pos.unmark al

let get_name name alias_opt =
  match alias_opt with Some alias -> alias | _ -> name

let sort_vars_by_alias is_ebcdic vars =
  let compare_name =
    if is_ebcdic then Strings.compare_ebcdic else Strings.compare_default
  in
  List.fast_sort
    (fun (_, _, _, _, name1, alias_opt1, _, _, _, _)
         (_, _, _, _, name2, alias_opt2, _, _, _, _) ->
      let var_name1 = get_name name1 alias_opt1 in
      let var_name2 = get_name name2 alias_opt2 in
      compare_name var_name1 var_name2)
    vars

let sort_vars_by_name is_ebcdic vars =
  let compare_name =
    if is_ebcdic then Strings.compare_ebcdic else Strings.compare_default
  in
  List.fast_sort
    (fun (_, _, _, _, name1, _, _, _, _, _) (_, _, _, _, name2, _, _, _, _, _) ->
      compare_name name1 name2)
    vars

(* Retrieve all the variables, sorted by alias, and compute their IDs *)
let get_vars (cprog : Mir.program) is_ebcdic =
  let idx = new_idx () in
  let get_cat_var (tgv : Com.Var.tgv) =
    match tgv.cat with
    | Computed { is_base } -> if is_base then Base else Computed
    | Input cat_var ->
        if StrSet.cardinal cat_var = 1 then
          match StrSet.min_elt cat_var with
          | "contexte" -> Context
          | "famille" -> Family
          | "revenu" -> Income
          | "variation" -> Variation
          | "penalite" -> Penality
          | _ -> CorrIncome
        else CorrIncome
  in
  let var_is_output (tgv : Com.Var.tgv) =
    tgv.is_given_back
    && ((not is_ebcdic)
       ||
       match StrMap.find_opt "primrest" tgv.attrs with
       | Some v -> Pos.unmark v <> 0
       | None -> false)
  in
  (* Retrieve the variables in file-order and compute their IDs *)
  let vars =
    StrMap.fold
      (fun name (var : Com.Var.t) vars ->
        match var.scope with
        | Tgv tgv ->
            let tvar = get_cat_var tgv in
            let size = Com.Var.size var in
            let idx1, idx2, idxo_opt =
              next_idx idx tvar (var_is_output tgv) size
            in
            let res =
              ( tvar,
                idx1,
                idx2,
                idxo_opt,
                name,
                Option.map Pos.unmark tgv.alias,
                Strings.sanitize_c_str (Pos.unmark tgv.descr),
                tgv.typ,
                tgv.attrs,
                size )
            in
            res :: vars
        | _ -> vars)
      cprog.program_vars []
  in

  let vars = sort_vars_by_name is_ebcdic vars in

  let idx = new_idx () in

  (* Recompute the indices of Restituee vars, as they are sorted by name (as
     opposed to file-order) *)
  let vars =
    List.map
      (fun ( tvar,
             idx1,
             idx2,
             idxo_opt,
             name,
             alias_opt,
             desc,
             typ_opt,
             attributes,
             size ) ->
        let _idx1, _idx2, idxo_opt =
          next_idx idx tvar (idxo_opt <> None) size
        in
        ( tvar,
          idx1,
          idx2,
          idxo_opt,
          name,
          alias_opt,
          desc,
          typ_opt,
          attributes,
          size ))
      vars
  in

  let vars = sort_vars_by_alias is_ebcdic vars in

  let idx = new_idx () in

  (* Recompute the indices of Context/Family/Income/... vars, as they are sorted
     by alias *)
  List.map
    (fun ( tvar,
           idx1,
           _idx2,
           idxo_opt,
           name,
           alias_opt,
           desc,
           typ_opt,
           attributes,
           size ) ->
      let _idx1, idx2, _idxo_opt = next_idx idx tvar (idxo_opt <> None) size in
      ( tvar,
        idx1,
        idx2,
        idxo_opt,
        name,
        alias_opt,
        desc,
        typ_opt,
        attributes,
        size ))
    vars

(* Retrieve the variables for the debug array; variables with aliases are
   duplicated *)
let get_vars_debug is_ebcdic vars =
  sort_vars_by_name is_ebcdic
    (List.fold_left
       (fun vars var ->
         let ( tvar,
               idx1,
               idx2,
               idxo_opt,
               _name,
               alias_opt,
               desc,
               typ_opt,
               attributes,
               size ) =
           var
         in
         match alias_opt with
         | Some alias ->
             ( tvar,
               idx1,
               idx2,
               idxo_opt,
               alias,
               alias_opt,
               desc,
               typ_opt,
               attributes,
               size )
             :: var :: vars
         | None -> var :: vars)
       [] vars)

(* Split a list in approximately equal chunks into a list of lists *)
let split_list lst cnt =
  let size = List.length lst in
  let rec aux l nl s sz nll c =
    let nl, s, sz, nll, c =
      if s < sz then (nl, s, sz, nll, c)
      else
        let sz = ((c + 2) * size / cnt) - ((c + 1) * size / cnt) in
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

(* Print a variable's description *)
let gen_var fmt req_type opt ~idx ~name ~tvar ~is_output ~typ_opt ~attributes
    ~desc ~alias_opt =
  let var_name = if opt.with_alias then get_name name alias_opt else name in

  let kind, is_input =
    match (tvar : var_subtype) with
    | Computed -> ("EST_CALCULEE", false)
    | Base -> ("EST_BASE", false)
    | _ -> ("EST_SAISIE", true)
  in

  let typ = match typ_opt with None -> Com.Real | Some ct -> ct in

  Format.fprintf fmt "    { \"%s\", %s | %d" var_name kind idx;
  if opt.with_type_donnee then
    Format.fprintf fmt ", %a" Com.format_value_typ typ;
  if opt.with_verif then
    if is_input && false then Format.fprintf fmt ", err_%s" name
      (* Note: no alias *)
    else Format.fprintf fmt ", no_error";
  (* does not seem to be used anymore... *)
  if opt.with_classe then
    Format.fprintf fmt ", %d" (get_attr "classe" attributes);
  if opt.with_priorite then
    Format.fprintf fmt ", %d" (get_attr "priorite" attributes);
  if opt.with_categorie_TL then
    Format.fprintf fmt ", %d" (get_attr "categorie_TL" attributes);
  if opt.with_nat_code then
    Format.fprintf fmt ", %d" (get_attr "nat_code" attributes);
  if opt.with_cotsoc then
    Format.fprintf fmt ", %d" (get_attr "cotsoc" attributes);
  if opt.with_ind_abat then
    Format.fprintf fmt ", %d" (get_attr "ind_abat" attributes);
  if opt.with_acompte then
    Format.fprintf fmt ", %d" (get_attr "acompte" attributes);
  if opt.with_avfisc then
    Format.fprintf fmt ", %d" (get_attr "avfisc" attributes);
  if opt.with_rapcat then
    Format.fprintf fmt ", %d" (get_attr "rapcat" attributes);
  if opt.with_sanction then
    Format.fprintf fmt ", %d" (get_attr "sanction" attributes);
  if opt.with_modcat then
    Format.fprintf fmt ", %d" (get_attr "modcat" attributes);
  if opt.with_liee then
    if true (* no linked var *) then Format.fprintf fmt ", (T_var_irdata)NULL"
    else Format.fprintf fmt ", desc_%s" (assert false);
  (* only REVENU vars may use this, but they don't... *)
  if opt.with_type && is_output then Format.fprintf fmt ", RESTITUEE";
  (* there also exist RESTITUEE_P and RESTITUEE_C, but they are unused *)
  if opt.with_primrest then
    Format.fprintf fmt ", %d" (get_attr "primrest" attributes);
  if opt.with_libelle then Format.fprintf fmt ", \"%s\"" desc
  else Format.fprintf fmt " /*\"%s\"*/" desc;
  begin
    match ((req_type : gen_type), tvar) with
    | Input _, Income -> Format.fprintf fmt ", \"%s\"" name
    | _ -> ()
  end;
  Format.fprintf fmt " },\n"

(* Check if a variable matches requested selection critaria *)
let var_matches req_type var_type is_output =
  match req_type with
  | Computed (Some rt) -> is_computed var_type && rt = var_type
  | Computed None -> is_computed var_type
  | Input (Some rt) -> is_input var_type && rt = var_type
  | Input None -> is_input var_type
  | Output -> is_output
  | Debug _i -> true

(* Print the specified variable table *)
let gen_table fmt is_ebcdic vars req_type opt =
  Format.fprintf fmt {|/****** LICENCE CECIL *****/

#include "compir.h"

|};
  (* if opt.with_verif then *)
  Format.fprintf fmt "extern T_discord *no_error(T_irdata *);\n";

  (* TODO there should be individual var verification functions here, but they
     do not seem to be used (for all kind of input vars as well as output vars
     and debug tables) *)
  let vars =
    if opt.with_alias then sort_vars_by_alias is_ebcdic vars
    else sort_vars_by_name is_ebcdic vars
  in
  let table_name = req_type_name req_type in
  let table_NAME = String.uppercase_ascii table_name in
  begin
    match req_type with
    | Debug _i ->
        Format.fprintf fmt "T_desc_debug desc_%s[NB_%s + 1] = {\n" table_name
          table_NAME
    | _ ->
        Format.fprintf fmt "T_desc_%s desc_%s[NB_%s + 1] = {\n" table_name
          table_name table_NAME
  end;

  let empty = ref true in
  List.iter
    (fun ( tvar,
           idx1,
           _idx2,
           idxo_opt,
           name,
           alias_opt,
           desc,
           typ_opt,
           attributes,
           _size ) ->
      let is_output = match idxo_opt with Some _ -> true | _ -> false in
      if var_matches req_type tvar is_output then begin
        empty := false;
        match req_type with
        | Debug _i ->
            (* Special case for debug *)
            let opt = { opt with with_alias = false } in
            gen_var fmt req_type opt ~idx:idx1 ~name ~tvar ~is_output ~typ_opt
              ~attributes ~desc ~alias_opt
        | _ ->
            (* General case*)
            gen_var fmt req_type opt ~idx:idx1 ~name ~tvar ~is_output ~typ_opt
              ~attributes ~desc ~alias_opt
      end)
    vars;
  (*
  if !empty then
    gen_var fmt req_type opt ~idx:0 ~name:"" ~tvar:Computed ~is_output:false
      ~typ_opt:None ~attributes:StrMap.empty ~desc:"" ~alias_opt:None;
*)
  Format.fprintf fmt "    NULL};\n"

let gen_desc fmt is_ebcdic vars ~alias_only =
  let vars = sort_vars_by_name is_ebcdic vars in

  Format.fprintf fmt {|/****** LICENCE CECIL *****/

#include "compir.h"

|};

  List.iter
    (fun ( tvar,
           _idx1,
           idx2,
           idxo_opt,
           name,
           alias_opt,
           _desc,
           _typ_opt,
           _attributes,
           _size ) ->
      if (not alias_only) || alias_opt <> None then
        let data_opt =
          match (tvar : var_subtype) with
          | Base | Computed -> begin
              (* computed var: only output *)
              match idxo_opt with
              | Some idx -> Some (Mast.givenback_category, idx)
              | None -> None
            end
          | _ -> Some (subtype_name tvar, idx2)
        in
        match data_opt with
        | Some (type_name, idx) ->
            let var_name =
              match (alias_only, alias_opt) with
              | true, Some alias -> alias
              | _ -> name
            in
            (* TODO special handling for debug vars (though it does not seem to
               happen) *)
            Format.fprintf fmt "#define desc_%s (T_var_irdata)(desc_%s + %d)\n"
              var_name type_name idx
        | None -> ())
    vars

(* TODO when flg_controle_immediat, add per variable verifications (add #define
   desc_verif) although it does not seem to be used anymore *)

let gen_table_output fmt flags vars =
  let is_ebcdic = flags.Dgfip_options.flg_tri_ebcdic in
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
      with_libelle = flags.Dgfip_options.flg_genere_libelle_restituee;
      with_alias = false;
      with_type = true;
      with_type_donnee = true;
      with_primrest = true;
    }
  in
  gen_table fmt is_ebcdic vars Output opt

let gen_table_context fmt flags vars =
  let is_ebcdic = flags.Dgfip_options.flg_tri_ebcdic in
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
  gen_table fmt is_ebcdic vars (Input (Some Context)) opt

let gen_table_family fmt flags vars =
  let is_ebcdic = flags.Dgfip_options.flg_tri_ebcdic in
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
      with_libelle = flags.Dgfip_options.flg_pro || flags.flg_iliad;
      with_alias = true;
      with_type = false;
      with_type_donnee = true;
      with_primrest = false;
    }
  in
  gen_table fmt is_ebcdic vars (Input (Some Family)) opt

let gen_table_income fmt flags vars =
  let is_ebcdic = flags.Dgfip_options.flg_tri_ebcdic in
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
      with_libelle = flags.Dgfip_options.flg_pro || not flags.flg_gcos;
      with_alias = true;
      with_type = false;
      with_type_donnee = true;
      with_primrest = false;
    }
  in
  gen_table fmt is_ebcdic vars (Input (Some Income)) opt

let gen_table_corrincome fmt flags vars =
  let is_ebcdic = flags.Dgfip_options.flg_tri_ebcdic in
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
  gen_table fmt is_ebcdic vars (Input (Some CorrIncome)) opt

let gen_table_variation fmt flags vars =
  let is_ebcdic = flags.Dgfip_options.flg_tri_ebcdic in
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
  gen_table fmt is_ebcdic vars (Input (Some Variation)) opt

let gen_table_penality fmt flags vars =
  let is_ebcdic = flags.Dgfip_options.flg_tri_ebcdic in
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
  gen_table fmt is_ebcdic vars (Input (Some Penality)) opt

let gen_table_debug fmt flags vars i =
  let is_ebcdic = flags.Dgfip_options.flg_tri_ebcdic in
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
  gen_table fmt is_ebcdic vars (Debug i) opt

(* Print the table of rule functions, and then the table of errors (tableg.c) *)
let gen_table_call fmt flags vars_debug (cprog : Mir.program) =
  Format.fprintf fmt {|/****** LICENCE CECIL *****/

#include "compir.h"

|};

  if flags.Dgfip_options.flg_debug then (
    if flags.nb_debug_c <= 0 then gen_table_debug fmt flags vars_debug 0;

    Format.fprintf fmt "T_desc_call desc_call[NB_CALL + 1] = {\n";
    IntMap.iter
      (fun id rn -> Format.fprintf fmt "    { %d, %s },\n" id rn)
      cprog.program_rules;
    Format.fprintf fmt "    0};\n\n";

    Format.fprintf fmt "T_desc_err desc_err[NB_ERR + 1] = {\n";
    StrMap.iter
      (fun _ (e : Com.Error.t) ->
        let en = Pos.unmark e.name in
        Format.fprintf fmt "    { \"%s\", &erreur_%s },\n" en en)
      cprog.program_errors;
    Format.fprintf fmt "    NULL};\n\n");

  StrMap.iter
    (fun _ tn -> Format.fprintf fmt "extern T_discord *%s(T_irdata *);\n" tn)
    cprog.program_chainings;

  Format.fprintf fmt "T_desc_ench desc_ench[NB_ENCH + 1] = {\n";
  StrMap.iter
    (fun cn tn -> Format.fprintf fmt "    { \"%s\", %s },\n" cn tn)
    cprog.program_chainings;
  Format.fprintf fmt "    NULL};\n"

(* Print the table of verification functions (tablev.c) *)
let gen_table_verif fmt flags (cprog : Mir.program) =
  Format.fprintf fmt {|/****** LICENCE CECIL *****/

#include "compir.h"

|};

  if flags.Dgfip_options.flg_debug || flags.flg_controle_immediat then (
    (* TODO: when control_immediat, don' put everything (but what ?) *)
    IntMap.iter
      (fun _ tn -> Format.fprintf fmt "extern T_discord *%s(T_irdata *);\n" tn)
      cprog.program_verifs;

    Format.fprintf fmt "T_desc_verif desc_verif[NB_VERIF + 1] = {\n";
    IntMap.iter
      (fun id tn -> Format.fprintf fmt "    { %d, %s },\n" id tn)
      cprog.program_verifs;
    Format.fprintf fmt "    0};\n\n")

(* Count variables in a specific category *)
let count vars req_type =
  List.fold_left
    (fun cpt (tvar, _, _, idxo_opt, _, _, _, _, _, size) ->
      let is_output = match idxo_opt with Some _ -> true | _ -> false in
      if var_matches req_type tvar is_output then cpt + size else cpt)
    0 vars

let gen_annee fmt flags =
  Format.fprintf fmt "#define ANNEE_REVENU %04d\n"
    flags.Dgfip_options.annee_revenu;
  Format.pp_print_flush fmt ()

let gen_compir_h fmt flags vars vars_debug =
  (* TODO paths may differ if dir_var_h is set *)
  Format.fprintf fmt
    {|/****** LICENCE CECIL *****/

#ifndef _COMPIR_H_
#define _COMPIR_H_

#include "mlang.h"

#define FALSE 0
#define TRUE 1

|};

  gen_annee fmt flags;
  let nb_contexte = count vars (Input (Some Context)) in
  let nb_famille = count vars (Input (Some Family)) in
  let nb_revenu = count vars (Input (Some Income)) in
  let nb_revenu_correc = count vars (Input (Some CorrIncome)) in
  let nb_variation = count vars (Input (Some Variation)) in
  let nb_penalite = count vars (Input (Some Penality)) in
  let nb_restituee = count vars Output in
  let nb_debug = List.map List.length vars_debug in

  Format.fprintf fmt
    {|#define NB_CONTEXTE %d
#define NB_FAMILLE %d
#define NB_REVENU %d
#define NB_REVENU_CORREC %d
#define NB_VARIATION %d
#define NB_PENALITE %d
#define NB_RESTITUEE %d
|}
    nb_contexte nb_famille nb_revenu nb_revenu_correc nb_variation nb_penalite
    nb_restituee;

  (if flags.Dgfip_options.flg_debug then
   if flags.nb_debug_c <= 0 then
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

  Format.fprintf fmt
    {|
typedef struct S_desc_contexte
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord *(*verif)(T_irdata *);
  int classe;
  int priorite;
  int categorie_TL;
  int modcat;
  char *libelle;
} T_desc_contexte;

typedef struct S_desc_famille
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord *(*verif)(T_irdata *);
  int classe;
  int priorite;
  int categorie_TL;
  int nat_code;
  int modcat;
  char *libelle;
} T_desc_famille;

typedef struct S_desc_revenu
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord *(*verif)(T_irdata *);
  int classe;
  int priorite;
  int categorie_TL;
  int nat_code;
  int cotsoc;
  int ind_abat;
  int acompte;
  int avfisc;
  int rapcat;
  int sanction;
  int modcat;
  T_var_irdata liee;
  char *libelle;
  char *code;
} T_desc_revenu;

typedef struct S_desc_revenu_correc
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord *(*verif)(T_irdata *);
  int classe;
  int priorite;
  int categorie_TL;
  int nat_code;
  int cotsoc;
  int ind_abat;
  int acompte;
  int avfisc;
  int rapcat;
  int sanction;
  int modcat;
  T_var_irdata liee;
  char *libelle;
} T_desc_revenu_correc;

typedef struct S_desc_variation
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord *(*verif)(T_irdata *);
  int classe;
} T_desc_variation;

typedef struct S_desc_penalite
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord *(*verif)(T_irdata *);
} T_desc_penalite;

typedef struct S_desc_restituee
{
  char *nom;
  int indice;
  long type_donnee;
  int type;
  int primrest;
#ifdef FLG_GENERE_LIBELLE_RESTITUEE
  char *libelle;
#endif /* FLG_GENERE_LIBELLE_RESTITUEE */
#ifdef FLG_EXTRACTION
  int est_extraite;
#endif /* FLG_EXTRACTION */
} T_desc_restituee;

typedef struct S_desc_debug
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord *(*verif)(T_irdata *);
  int classe;
  int priorite;
  int categorie_TL;
  int cotsoc;
  int ind_abat;
  int acompte;
  int avfisc;
  int rapcat;
  int sanction;
  int modcat;
  int nat_code;
  T_var_irdata liee;
#ifdef FLG_EXTRACTION
  int est_extraite;
#endif /* FLG_EXTRACTION */
} T_desc_debug;

typedef struct S_desc_err
{
  char *nom;
  T_erreur *erreur;
} T_desc_err;

typedef struct S_desc_call
{
  int num;
  T_discord *(*proc)(T_irdata *irdata);
} T_desc_call;

typedef struct S_desc_ench
{
  char *nom;
  T_discord *(*proc)(T_irdata *irdata);
} T_desc_ench;

typedef struct S_desc_verif
{
  int num;
  T_discord *(*proc)(T_irdata *irdata);
} T_desc_verif;

extern T_desc_contexte desc_contexte[];
extern T_desc_famille desc_famille[];
extern T_desc_penalite desc_penalite[];
extern T_desc_revenu desc_revenu[];
extern T_desc_revenu_correc desc_revenu_correc[];
extern T_desc_variation desc_variation[];
extern T_desc_restituee desc_restituee[];

extern T_desc_err desc_err[];
extern T_desc_verif desc_verif[];
extern T_desc_call desc_call[];
extern T_desc_ench desc_ench[];
extern T_desc_debug desc_debug01[];
extern T_desc_debug desc_debug02[];
extern T_desc_debug desc_debug03[];
extern T_desc_debug desc_debug04[];

extern struct S_erreur *tabErreurs[];

|};
  Format.fprintf fmt "#endif /* _COMPIR_H_ */\n"

(* Generate the auxiliary files AND return the map of variables names to TGV
   ids *)
let generate_compir_files flags (cprog : Mir.program) : unit =
  let vars = get_vars cprog Dgfip_options.(flags.flg_tri_ebcdic) in

  let oc, fmt = open_file "compir_restitue.c" in
  gen_table_output fmt flags vars;
  close_out oc;

  let oc, fmt = open_file "compir_contexte.c" in
  gen_table_context fmt flags vars;
  close_out oc;

  let oc, fmt = open_file "compir_famille.c" in
  gen_table_family fmt flags vars;
  close_out oc;

  let oc, fmt = open_file "compir_revenu.c" in
  gen_table_income fmt flags vars;
  close_out oc;

  let oc, fmt = open_file "compir_revcor.c" in
  gen_table_corrincome fmt flags vars;
  close_out oc;

  let oc, fmt = open_file "compir_variatio.c" in
  gen_table_variation fmt flags vars;
  close_out oc;

  let oc, fmt = open_file "compir_penalite.c" in
  gen_table_penality fmt flags vars;
  close_out oc;

  let vars_debug = get_vars_debug Dgfip_options.(flags.flg_tri_ebcdic) vars in
  let vars_debug_split = split_list vars_debug flags.nb_debug_c in
  let _ =
    if flags.nb_debug_c > 0 then
      List.fold_left
        (fun i vars ->
          let oc, fmt = open_file (Printf.sprintf "compir_tableg%02d.c" i) in
          if flags.flg_debug then gen_table_debug fmt flags vars i
          else
            Format.fprintf fmt
              "/****** LICENCE CECIL *****/\n\n#include \"compir.h\"\n\n";
          close_out oc;
          i + 1)
        1 vars_debug_split
    else 0
  in

  let oc, fmt = open_file "compir_desc.h" in
  gen_desc fmt Dgfip_options.(flags.flg_tri_ebcdic) vars ~alias_only:true;
  close_out oc;

  let oc, fmt = open_file "compir_desc_inv.h" in
  gen_desc fmt Dgfip_options.(flags.flg_tri_ebcdic) vars ~alias_only:false;
  close_out oc;

  let oc, fmt = open_file "compir_tableg.c" in
  gen_table_call fmt flags vars_debug cprog;
  close_out oc;

  let oc, fmt = open_file "compir_tablev.c" in
  gen_table_verif fmt flags cprog;
  close_out oc;

  let oc, fmt = open_file "compir.h" in
  gen_compir_h fmt flags vars vars_debug_split;
  close_out oc
