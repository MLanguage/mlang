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

let input_var_subtype iv : var_subtype =
  List.find_map
    (fun t ->
      match Pos.unmark t with
      | "contexte" -> Some Context
      | "famille" -> Some Family
      | "penalite" -> Some Penality
      | "revenu" -> Some Income
      | _ -> None)
    iv.Mast.input_category
  |> function
  | Some s -> s
  | None -> assert false
(* Missing CorrIncome and Variation (actually not used *)

let computed_var_subtype cv : var_subtype =
  let is_base =
    List.exists
      (fun ct -> String.equal (Pos.unmark ct) Mast.base_category)
      cv.Mast.comp_category
  in
  if is_base then Base else Computed

let computed_var_is_output cv = cv.Mast.comp_is_givenback

let input_var_is_output iv = iv.Mast.input_is_givenback

let consider_output is_ebcdic attribs =
  is_ebcdic = false
  || List.exists
       (fun (an, av) ->
         match (Pos.unmark an, Pos.unmark av) with
         | "primrest", v -> v <> 0
         | _ -> false)
       attribs

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
  let attr_opt =
    List.find_opt (fun (an, _al) -> Pos.unmark an = a) attributes
  in
  match attr_opt with
  | None -> if a = "primrest" then 1 else -1
  | Some (_an, al) -> Pos.unmark al

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
let get_vars prog is_ebcdic =
  let open Mast in
  let idx = new_idx () in

  (* Retrieve the variables in file-order and compute their IDs *)
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
                let size =
                  match cv.comp_table with
                  | Some (Mast.LiteralSize i, _) -> i
                  | None -> 1
                  | Some (Mast.SymbolSize _, _) -> assert false
                in
                let idx1, idx2, idxo_opt =
                  next_idx idx tvar
                    (computed_var_is_output cv
                    && consider_output is_ebcdic cv.Mast.comp_attributes)
                    size
                in
                let var =
                  ( tvar,
                    idx1,
                    idx2,
                    idxo_opt,
                    Pos.unmark cv.comp_name,
                    None,
                    Strings.sanitize_str cv.comp_description,
                    cv.comp_typ,
                    cv.comp_attributes,
                    size )
                in
                var :: vars
            | VariableDecl (InputVar iv) ->
                let iv = Pos.unmark iv in
                let tvar = input_var_subtype iv in
                let idx1, idx2, idxo_opt =
                  next_idx idx tvar
                    (input_var_is_output iv
                    && consider_output is_ebcdic iv.Mast.input_attributes)
                    1
                in
                let var =
                  ( tvar,
                    idx1,
                    idx2,
                    idxo_opt,
                    Pos.unmark iv.input_name,
                    Some (Pos.unmark iv.input_alias),
                    Strings.sanitize_str iv.input_description,
                    iv.input_typ,
                    iv.input_attributes,
                    1 )
                in
                var :: vars
            | _ -> vars)
          vars file)
      [] prog
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

  let typ = match typ_opt with None -> Com.Real | Some ct -> Pos.unmark ct in

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

  if !empty then
    gen_var fmt req_type opt ~idx:0 ~name:"" ~tvar:Computed ~is_output:false
      ~typ_opt:None ~attributes:[] ~desc:"" ~alias_opt:None;

  Format.fprintf fmt "};\n"

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

let gen_table_varinfo fmt var_dict cat
    Com.CatVar.{ id_int; id_str; attributs; _ } stats =
  Format.fprintf fmt "T_varinfo_%s varinfo_%s[NB_%s + 1] = {\n" id_str id_str
    id_str;
  let nb =
    StrMap.fold
      (fun _ (var, idx, size) nb ->
        if Com.CatVar.compare (Com.Var.cat var) cat = 0 then (
          let loc_cat =
            match (Com.Var.loc_tgv var).loc_cat with
            | Com.CatVar.LocComputed -> "EST_CALCULEE"
            | Com.CatVar.LocBase -> "EST_BASE"
            | Com.CatVar.LocInput -> "EST_SAISIE"
          in
          Format.fprintf fmt "  { \"%s\", \"%s\", %d, %d, %d, %s"
            (Com.Var.name_str var) (Com.Var.alias_str var) idx size id_int
            loc_cat;
          StrMap.iter
            (fun _ av -> Format.fprintf fmt ", %d" (Pos.unmark av))
            (Com.Var.attrs var);
          Format.fprintf fmt " },\n";
          nb + 1)
        else nb)
      var_dict 0
  in
  Format.fprintf fmt "  NULL\n};\n\n";
  let attr_set =
    StrMap.fold (fun an _ res -> StrSet.add an res) attributs StrSet.empty
  in
  Com.CatVar.Map.add cat (id_str, id_int, nb, attr_set) stats

let gen_table_varinfos fmt (cprog : Mir.program) vars =
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
      Format.fprintf fmt "char attribut_%s_def(T_varinfo *vi) {\n" attr;
      Format.fprintf fmt "  switch (vi->cat) {\n";
      Com.CatVar.Map.iter
        (fun _ Com.CatVar.{ id_str; attributs; _ } ->
          if StrMap.mem attr attributs then
            Format.fprintf fmt "    case ID_%s: return 1;\n" id_str)
        cprog.program_var_categories;
      Format.fprintf fmt "  }\n";
      Format.fprintf fmt "  return 0;\n";
      Format.fprintf fmt "}\n\n";
      Format.fprintf fmt "double attribut_%s(T_varinfo *vi) {\n" attr;
      Format.fprintf fmt "  switch (vi->cat) {\n";
      Com.CatVar.Map.iter
        (fun _ Com.CatVar.{ id_str; attributs; _ } ->
          if StrMap.mem attr attributs then (
            Format.fprintf fmt "    case ID_%s:\n" id_str;
            Format.fprintf fmt "      return ((T_varinfo_%s *)vi)->attr_%s;\n"
              id_str attr))
        cprog.program_var_categories;
      Format.fprintf fmt "  }\n";
      Format.fprintf fmt "  return 0.0;\n";
      Format.fprintf fmt "}\n\n")
    attrs;

  let var_dict =
    StrMap.fold
      (fun _ var dict ->
        StrMap.add (Pos.unmark var.Com.Var.name) (var, -1, -1) dict)
      cprog.program_vars StrMap.empty
  in
  let var_dict =
    List.fold_left
      (fun dict
           ( _tvar,
             idx1,
             _idx2,
             _idxo_opt,
             name,
             _alias_opt,
             _desc,
             _typ_opt,
             _attributes,
             size ) ->
        StrMap.update name
          (function Some (var, _, _) -> Some (var, idx1, size) | None -> None)
          dict)
      var_dict vars
  in
  Com.CatVar.Map.fold
    (gen_table_varinfo fmt var_dict)
    cprog.program_var_categories Com.CatVar.Map.empty

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
  Format.fprintf fmt "\n";
  Com.CatVar.Map.iter
    (fun _ (id_str, _, nb, _) ->
      Format.fprintf fmt "#define NB_%s %d\n" id_str nb)
    stats;
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

let is_valid_app apps = StrMap.mem "iliad" apps

(* Retrieve rules, verifications, errors and chainings from a program *)
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
                  if is_valid_app r.rule_apps then
                    ( Pos.unmark r.rule_number :: rules,
                      match r.rule_chaining with
                      | None -> chainings
                      | Some cn -> StrSet.add (Pos.unmark cn) chainings )
                  else (rules, chainings)
                in
                (rules, verifs, errors, chainings)
            | Verification v ->
                let verifs =
                  if is_valid_app v.verif_apps then
                    fst
                    @@ List.fold_left
                         (fun (verifs, vn) _vc -> (vn :: verifs, vn + 1))
                         (verifs, Pos.unmark v.verif_number)
                         v.verif_conditions
                  else verifs
                in
                (rules, verifs, errors, chainings)
            | Error e -> (rules, verifs, e :: errors, chainings)
            (* | Chaining (cn, _anl) -> rules, verifs, errors, cn ::
               chainings *)
            | _ -> (rules, verifs, errors, chainings))
          (rules, verifs, errors, chainings)
          file)
      ([], [], [], StrSet.empty) prog
  in

  let rules = List.fast_sort compare rules in
  let verifs = List.fast_sort compare verifs in
  let errors = List.fast_sort compare errors in

  (* let chainings = List.fast_sort compare chainings in *)
  (rules, verifs, errors, chainings)

(* Print the table of rule functions, and then the table of errors (tableg.c) *)
let gen_table_call fmt flags vars_debug prefix rules chainings errors =
  let open Mast in
  Format.fprintf fmt {|/****** LICENCE CECIL *****/

#include "compir.h"

|};

  if flags.Dgfip_options.flg_debug then begin
    if flags.nb_debug_c <= 0 then gen_table_debug fmt flags vars_debug 0;

    Format.fprintf fmt "T_desc_call desc_call[NB_CALL + 1] = {\n";
    List.iter
      (fun rn -> Format.fprintf fmt "    { %d, %s_regle_%d },\n" rn prefix rn)
      rules;
    Format.fprintf fmt "};\n\n";

    Format.fprintf fmt "T_desc_err desc_err[NB_ERR + 1] = {\n";
    List.iter
      (fun e ->
        let en = Pos.unmark e.error_name in
        Format.fprintf fmt "    { \"%s\", &erreur_%s },\n" en en)
      errors;
    Format.fprintf fmt "};\n\n"
  end;

  StrSet.iter
    (fun cn -> Format.fprintf fmt "extern T_discord *%s(T_irdata *);\n" cn)
    chainings;

  Format.fprintf fmt "T_desc_ench desc_ench[NB_ENCH + 1] = {\n";
  StrSet.iter
    (fun cn -> Format.fprintf fmt "    { \"%s\", %s },\n" cn cn)
    chainings;
  Format.fprintf fmt "};\n"

(* Print the table of verification functions (tablev.c) *)
let gen_table_verif fmt flags prefix verifs =
  Format.fprintf fmt {|/****** LICENCE CECIL *****/

#include "compir.h"

|};

  if flags.Dgfip_options.flg_debug || flags.flg_controle_immediat then begin
    (* TODO: when control_immediat, don' put everything (but what ?) *)
    List.iter
      (fun vn ->
        Format.fprintf fmt "extern T_discord *%s_verif_%d(T_irdata *);\n" prefix
          vn)
      verifs;

    Format.fprintf fmt "T_desc_verif desc_verif[NB_VERIF + 1] = {\n";
    List.iter
      (fun vn -> Format.fprintf fmt "    { %d, %s_verif_%d },\n" vn prefix vn)
      verifs;
    Format.fprintf fmt "};\n\n"
  end

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

let gen_erreurs_c fmt flags errors =
  let open Mast in
  Format.fprintf fmt {|/****** LICENCE CECIL *****/

#include "mlang.h"

|};

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
            if String.equal (Pos.unmark sous_code) "00" then ""
            else "-" ^ Pos.unmark sous_code
          in
          Format.fprintf fmt
            "T_erreur erreur_%s = { \"%s%s%s / %s\", \"%s\", \"%s\", \"%s\", \
             \"%s\", %d };\n"
            (Pos.unmark e.error_name) (Pos.unmark famille) (Pos.unmark code_bo)
            sous_code_suffix
            (Strings.sanitize_str libelle)
            (Pos.unmark code_bo) (Pos.unmark sous_code) (Pos.unmark is_isf)
            (Pos.unmark e.error_name) terr
      | _ -> failwith "Invalid error description")
    errors;

  if flags.Dgfip_options.flg_pro || flags.flg_iliad then begin
    Format.fprintf fmt "T_erreur *tabErreurs[] = {\n";

    List.iter
      (fun e ->
        Format.fprintf fmt "    &erreur_%s,\n" (Pos.unmark e.error_name))
      errors;

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
  if flags.flg_short then Format.fprintf fmt "#define FLG_SHORT\n";
  if flags.flg_register then Format.fprintf fmt "#define FLG_REGISTER\n";
  (* flag is not used *)
  if flags.flg_optim_min_max then
    Format.fprintf fmt "#define FLG_OPTIM_MIN_MAX\n";
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
    {|#ifdef FLG_COLORS
int change_couleur (int couleur,int typographie);
int get_couleur ( );
int get_typo ( );
#endif
    
#ifdef FLG_TRACE
extern int niv_trace;

extern void aff1(char *nom);

extern void aff_val(const char *nom, const T_irdata *irdata, int indice, int niv, const char *chaine, int is_tab, int expr, int maxi);

#define aff2(nom, irdata, indice) aff_val(nom, irdata, indice, 2, "<-", 0, 0, 1)

#define aff3(nom, irdata, indice) aff_val(nom, irdata, indice, 3, ":", 0, 0, 1)
#endif /* FLG_TRACE */
|}

let gen_const fmt =
  Format.fprintf fmt
    {|#define FALSE 0
#define TRUE 1

struct S_print_context {
  long indent;
  int is_newline;
};

typedef struct S_print_context T_print_context;

typedef void *T_var_irdata;

struct S_erreur
{
  char *message;
  char *codebo;
  char *souscode;
  char *isisf;
  char *nom;
  short type;
};

typedef struct S_erreur T_erreur;

struct S_discord
{
  struct S_discord *suivant;
  T_erreur *erreur;
};

typedef struct S_discord T_discord;

struct S_irdata
{
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
  T_discord *discords;
  T_discord *tas_discord;
  T_discord **p_discord;
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
  T_print_context ctx_pr_out;
  T_print_context ctx_pr_err;
};

typedef struct S_irdata T_irdata;

#define S_ irdata->saisie
#define C_ irdata->calculee
#define B_ irdata->base
/*#define T_ irdata->tmps*/
/*#define R_ irdata->ref*/
#define DS_ irdata->def_saisie
#define DC_ irdata->def_calculee
#define DB_ irdata->def_base
/*#define DT_ irdata->def_tmps*/
/*#define DR_ irdata->def_ref*/
/*#define IT_ irdata->info_tmps*/
/*#define IR_ irdata->info_ref*/

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
extern int multimax_def(int, char *);
extern double multimax(double, double *);
extern int modulo_def(int, int);
extern double modulo(double, double);
|}

let gen_lib fmt (cprog : Mir.program) flags rules verifs chainings errors =
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
  let nb_ench = StrSet.cardinal chainings in
  let nb_err = List.length errors in
  let nb_call = List.length rules in
  let nb_verif = List.length verifs in

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
  List.iter
    (fun e ->
      let en = Pos.unmark e.Mast.error_name in
      Format.fprintf fmt "extern T_erreur erreur_%s;\n" en)
    errors;

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

extern void env_sauvegarder(T_env_sauvegarde **liste, char *oDef, double *oVal, int sz);
extern void env_restaurer(T_env_sauvegarde **liste);
extern int nb_informatives(T_irdata *irdata);
extern int nb_discordances(T_irdata *irdata);
extern int nb_anomalies(T_irdata *irdata);
extern int nb_bloquantes(T_irdata *irdata);
extern void nettoie_erreur _PROTS((T_irdata *irdata ));
extern void finalise_erreur _PROTS((T_irdata *irdata ));
extern void exporte_erreur _PROTS((T_irdata *irdata ));
extern void init_erreur(T_irdata *irdata);
|}

let gen_decl_functions fmt (cprog : Mir.program) =
  let functions = Com.TargetMap.bindings cprog.program_functions in
  let pp_args fmt args =
    List.iteri
      (fun i _ -> Pp.fpr fmt ", char def_arg%d, double val_arg%d" i i)
      args
  in
  Format.fprintf fmt "@[<v 0>%a@]@,"
    (Format.pp_print_list (fun fmt (fn, fd) ->
         Format.fprintf fmt
           "extern int %s(T_irdata* irdata, char *def_res, double *val_res%a);"
           fn pp_args fd.Mir.target_args))
    functions

let gen_decl_targets fmt (cprog : Mir.program) =
  let targets = Com.TargetMap.bindings cprog.program_targets in
  Format.fprintf fmt "@[<v 0>%a@]@,"
    (Format.pp_print_list (fun fmt (name, _) ->
         Format.fprintf fmt "extern struct S_discord *%s(T_irdata* irdata);"
           name))
    targets

let gen_mlang_h fmt cprog flags stats_varinfos rules verifs chainings errors =
  let pr = Format.fprintf fmt in
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
  gen_annee fmt flags;
  pr "\n";
  gen_decl_varinfos fmt cprog stats_varinfos;
  pr "\n";
  gen_const fmt;
  pr "\n";
  (* The debug functions need T_irdata to be defined so we put them after *)
  gen_dbg fmt;
  pr "\n";
  gen_lib fmt cprog flags rules verifs chainings errors;
  pr "\n";
  gen_decl_functions fmt cprog;
  pr "\n";
  gen_decl_targets fmt cprog;
  pr "#endif /* _MLANG_H_ */\n\n"

let gen_mlang_c fmt =
  Format.fprintf fmt "%s"
    {|/****** LICENCE CECIL *****/

#include "mlang.h"

#ifdef FLG_COLORS 
int color = 37;
int typo = 0;

int change_couleur (int couleur,int typographie)
{
	 color = couleur;
	 typo = typographie;
	return 0 ;
}

int get_couleur ( )
{
	return color ;
}

int get_typo ( )
{
	return typo ;
}
#endif 

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

static void add_erreur_code(T_erreur *erreur, const char *code) {
  size_t len = 0;
  char *new_message = NULL;
  char *debut = NULL;

  if (code != NULL) {
    debut = strstr(erreur->message," ((");
    if (debut != NULL) {
      len = strlen(erreur->message) - strlen(debut);
    } else {
      len = strlen(erreur->message);
    }

    new_message = (char *)malloc((len + 10) * sizeof(char));
    memset(new_message, '\0', (len + 10) * sizeof(char));
    strncpy(new_message, erreur->message, len);
    strcat(new_message, " ((");
    strcat(new_message, code);
    strcat(new_message, "))\0");
    erreur->message = new_message;
  }
}

void init_erreur(T_irdata *irdata) {
/*  IRDATA_reset_erreur(irdata); */
  *irdata->p_discord = irdata->tas_discord;
  irdata->tas_discord = irdata->discords;
  irdata->discords = 0;
  irdata->p_discord = &irdata->discords;
  irdata->nb_anos = 0;
  irdata->nb_discos = 0;
  irdata->nb_infos = 0;
  irdata->nb_bloqs = 0;
  irdata->max_bloqs = 4;
}

void add_erreur(T_irdata *irdata, T_erreur *erreur, char *code) {
  T_discord *new_discord = NULL;

  if (irdata->tas_discord == 0) {
    new_discord = (T_discord *)malloc(sizeof(T_discord));
  } else {
    new_discord = irdata->tas_discord;
    irdata->tas_discord = new_discord->suivant;
  }

  add_erreur_code(erreur, code);

  new_discord->erreur = erreur;
  new_discord->suivant = 0;
  *irdata->p_discord = new_discord;
  irdata->p_discord = &new_discord->suivant;

  if (erreur->type == ANOMALIE) irdata->nb_anos++;
  if (erreur->type == DISCORDANCE) irdata->nb_discos++;
  if (erreur->type == INFORMATIVE) irdata->nb_infos++;

  if (strcmp(erreur->isisf, "N") == 0 && erreur->type == ANOMALIE) {
    irdata->nb_bloqs++;
    if (irdata->nb_bloqs >= irdata->max_bloqs) {
      longjmp(irdata->jmp_bloq, 1);
    }
  }
}

void free_erreur() {}

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

int niv_trace = 3;

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

T_discord * no_error(T_irdata *irdata) {
  return NULL;
}

int multimax_def(int nbopd, char *var) {
  int i = 0;
  for (i = 0; i < nbopd; i++) {
    if (var[i] == 1) return 1;
  }
  return 0;
}

double multimax(double nbopd, double *var) {
  int i = 0;
  double s = 0.0;
  for (i = 0; i < (int)nbopd; i++) {
    if (var[i] >= s) s = var[i];
  }
  return s;
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
    *liste = courant-> suite;
    *(courant->orig_def) = courant->sauv_def;
    *(courant->orig_val) = courant->sauv_val;
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
      fprintf(std, " ");
    }
    pr_ctx->is_newline = 0;
  }
}

void print_string(FILE *std, T_print_context *pr_ctx, char *str) {
  while (*str != 0) {
    if (*str == '\n') {
      pr_ctx->is_newline = 1;
    } else {
      print_indent(std, pr_ctx);
    }
    fprintf(std, "%c", *str);
    str++;
  }
}

void print_double(FILE *std, T_print_context *pr_ctx, double f, int pmin, int pmax) {
  print_indent(std, pr_ctx);
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
    fprintf(std, "incorrect");
  } else if (isinf(f)) {
    if (f >= 0.0) {
      fprintf(std, "+infini");
    } else {
      fprintf(std, "-infini");
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
    fprintf(std, "%s", buf);
/*    free(buf); */
  }
}

void nettoie_erreur(irdata)
T_irdata *irdata;
{
  init_erreur(irdata);
}
|}

let open_file filename =
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  (oc, fmt)

(* Generate the auxiliary files AND return the map of variables names to TGV
   ids *)
let generate_auxiliary_files flags prog (cprog : Mir.program) : unit =
  let folder = Filename.dirname !Cli.output_file in

  let vars = get_vars prog Dgfip_options.(flags.flg_tri_ebcdic) in

  let oc, fmt = open_file (Filename.concat folder "compir_restitue.c") in
  gen_table_output fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "compir_contexte.c") in
  gen_table_context fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "compir_famille.c") in
  gen_table_family fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "compir_revenu.c") in
  gen_table_income fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "compir_revcor.c") in
  gen_table_corrincome fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "compir_variatio.c") in
  gen_table_variation fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "compir_penalite.c") in
  gen_table_penality fmt flags vars;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "varinfos.c") in
  let stats_varinfos = gen_table_varinfos fmt cprog vars in
  close_out oc;

  let vars_debug = get_vars_debug Dgfip_options.(flags.flg_tri_ebcdic) vars in
  let vars_debug_split = split_list vars_debug flags.nb_debug_c in
  let _ =
    if flags.nb_debug_c > 0 then
      List.fold_left
        (fun i vars ->
          let file = Printf.sprintf "compir_tableg%02d.c" i in
          let oc, fmt = open_file (Filename.concat folder file) in
          if flags.flg_debug then gen_table_debug fmt flags vars i
          else
            Format.fprintf fmt
              "/****** LICENCE CECIL *****/\n\n#include \"compir.h\"\n\n";
          close_out oc;
          i + 1)
        1 vars_debug_split
    else 0
  in

  let oc, fmt = open_file (Filename.concat folder "compir_desc.h") in
  gen_desc fmt Dgfip_options.(flags.flg_tri_ebcdic) vars ~alias_only:true;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "compir_desc_inv.h") in
  gen_desc fmt Dgfip_options.(flags.flg_tri_ebcdic) vars ~alias_only:false;
  close_out oc;

  let rules, verifs, errors, chainings = get_rules_verif_etc prog in
  let prefix = cprog.program_safe_prefix in

  let oc, fmt = open_file (Filename.concat folder "compir_tableg.c") in
  gen_table_call fmt flags vars_debug prefix rules chainings errors;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "compir_tablev.c") in
  gen_table_verif fmt flags prefix verifs;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "compir.h") in
  gen_compir_h fmt flags vars vars_debug_split;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "erreurs.c") in
  gen_erreurs_c fmt flags errors;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "conf.h") in
  gen_conf_h fmt cprog flags;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "mlang.h") in
  gen_mlang_h fmt cprog flags stats_varinfos rules verifs chainings errors;
  close_out oc;

  let oc, fmt = open_file (Filename.concat folder "mlang.c") in
  gen_mlang_c fmt;
  close_out oc
