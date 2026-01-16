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

(* TGV variables accessors *)

let gen_tab = function
  | Com.CatVar.LocInput -> "saisie"
  | Com.CatVar.LocComputed -> "calculee"
  | Com.CatVar.LocBase -> "base"

let gen_tgv_def (m_sp_opt : Com.var_space) (l : Com.loc_tgv) vn =
  let tab = gen_tab l.loc_cat in
  match m_sp_opt with
  | Some (m_sp, _) ->
      let sp = Com.get_normal_var @@ Pos.unmark m_sp in
      Pp.spr "(irdata->def_%s_%s[%d/*%s*/])" tab sp l.loc_idx vn
  | None ->
      Pp.spr "(irdata->var_spaces[irdata->var_space].def_%s[%d/*%s*/])" tab
        l.loc_idx vn

let gen_tgv_val (m_sp_opt : Com.var_space) (l : Com.loc_tgv) vn =
  let tab = gen_tab l.loc_cat in
  match m_sp_opt with
  | Some (m_sp, _) ->
      let sp = Com.get_normal_var @@ Pos.unmark m_sp in
      Pp.spr "(irdata->%s_%s[%d/*%s*/])" tab sp l.loc_idx vn
  | None ->
      Pp.spr "(irdata->var_spaces[irdata->var_space].%s[%d/*%s*/])" tab
        l.loc_idx vn

let gen_tgv_def_ptr (m_sp_opt : Com.var_space) (l : Com.loc_tgv) vn =
  Pp.spr "&%s" (gen_tgv_def m_sp_opt l vn)

let gen_tgv_val_ptr (m_sp_opt : Com.var_space) (l : Com.loc_tgv) vn =
  Pp.spr "&%s" (gen_tgv_val m_sp_opt l vn)

let gen_tgv_info_ptr (l : Com.loc_tgv) vn =
  Pp.spr "I_(%s,%d/*%s*/)" l.loc_cat_str l.loc_cat_idx vn

(* temporary variables accessors *)

let gen_tmp_def (l : Com.loc_tmp) vn = Pp.spr "DT_((%d)/*%s*/)" l.loc_idx vn

let gen_tmp_val (l : Com.loc_tmp) vn = Pp.spr "T_((%d)/*%s*/)" l.loc_idx vn

let gen_tmp_def_ptr (l : Com.loc_tmp) vn = Pp.spr "&(%s)" (gen_tmp_def l vn)

let gen_tmp_val_ptr (l : Com.loc_tmp) vn = Pp.spr "&(%s)" (gen_tmp_val l vn)

let gen_tmp_info_ptr (l : Com.loc_tmp) vn =
  Pp.spr "IT_((%d)/*%s*/)" l.loc_idx vn

(* reference accessors *)

let gen_ref_def_ptr m_sp_opt i vn =
  match m_sp_opt with
  | None -> Printf.sprintf "DR_((%d)/*%s*/)" i vn
  | Some (_, i_sp) ->
      let info = Printf.sprintf "irdata->refs[irdata->refs_org + %d].info" i in
      Printf.sprintf "lis_varinfo_def_ptr(irdata, %d, %s)" i_sp info

let gen_ref_val_ptr m_sp_opt i vn =
  match m_sp_opt with
  | None -> Printf.sprintf "R_((%d)/*%s*/)" i vn
  | Some (_, i_sp) ->
      let info = Printf.sprintf "irdata->refs[irdata->refs_org + %d].info" i in
      Printf.sprintf "lis_varinfo_val_ptr(irdata, %d, %s)" i_sp info

let gen_ref_info_ptr i vn = Printf.sprintf "IR_((%d)/*%s*/)" i vn

let gen_ref_def m_sp_opt i vn = Pp.spr "*(%s)" (gen_ref_def_ptr m_sp_opt i vn)

let gen_ref_val m_sp_opt i vn = Pp.spr "*(%s)" (gen_ref_val_ptr m_sp_opt i vn)

(* generic accessors *)

let gen_def (m_sp_opt : Com.var_space) (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv_def m_sp_opt l vn
  | LocTmp (_, l) -> gen_tmp_def l vn
  | LocRef (_, i) -> gen_ref_def m_sp_opt i vn

let gen_val (m_sp_opt : Com.var_space) (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv_val m_sp_opt l vn
  | LocTmp (_, l) -> gen_tmp_val l vn
  | LocRef (_, i) -> gen_ref_val m_sp_opt i vn

let gen_info_ptr (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv_info_ptr l vn
  | LocTmp (_, l) -> gen_tmp_info_ptr l vn
  | LocRef (_, i) -> gen_ref_info_ptr i vn

let gen_def_ptr (m_sp_opt : Com.var_space) (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv_def_ptr m_sp_opt l vn
  | LocTmp (_, l) -> gen_tmp_def_ptr l vn
  | LocRef (_, i) -> gen_ref_def_ptr m_sp_opt i vn

let gen_val_ptr (m_sp_opt : Com.var_space) (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv_val_ptr m_sp_opt l vn
  | LocTmp (_, l) -> gen_tmp_val_ptr l vn
  | LocRef (_, i) -> gen_ref_val_ptr m_sp_opt i vn

let gen_ref_name_ptr (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocRef (_, i) -> Printf.sprintf "NR_((%d)/*%s*/)" i vn
  | _ -> assert false

let gen_ref_var_space_ptr (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocRef (_, i) -> Printf.sprintf "SR_((%d)/*%s*/)" i vn
  | _ -> assert false

let gen_pos_from_start (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) ->
      let loc_tab =
        match l.loc_cat with
        | Com.CatVar.LocComputed -> "EST_CALCULEE"
        | Com.CatVar.LocBase -> "EST_BASE"
        | Com.CatVar.LocInput -> "EST_SAISIE"
      in
      Printf.sprintf "%s | %d" loc_tab l.loc_idx
  | LocTmp (_, l) -> Pp.spr "EST_TEMPORAIRE | %d" l.loc_idx
  | LocRef (_, i) ->
      let info = gen_ref_info_ptr i vn in
      Printf.sprintf "%s->loc_cat | %s->idx" info info

let gen_size (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv _ | LocTmp _ -> Pp.spr "%d" (Com.Var.size v)
  | LocRef (_, i) -> Pp.spr "(%s->size)" (gen_ref_info_ptr i vn)

let gen_var_space_id_opt = function
  | None -> "(irdata->var_space)"
  | Some (_, i_sp) -> Pp.spr "%d" i_sp

let gen_var_space_id (m_sp_opt : Com.var_space) (v : Com.Var.t) =
  match v.loc with
  | LocTgv _ | LocTmp _ -> gen_var_space_id_opt m_sp_opt
  | LocRef (_, i) -> (
      match m_sp_opt with
      | None -> Pp.spr "(irdata->refs[irdata->refs_org + %d].var_space)" i
      | Some (_, i_sp) -> Pp.spr "%d" i_sp)
