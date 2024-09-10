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

let gen_tab = function
  | Com.CatVar.LocComputed -> "C_"
  | Com.CatVar.LocBase -> "B_"
  | Com.CatVar.LocInput -> "S_"

let gen_tgv pre (l : Com.loc_tgv) vn off =
  Printf.sprintf "%s%s[%d/*%s*/%s]" pre (gen_tab l.loc_cat) l.loc_idx vn off

let gen_tgv_ptr pre (l : Com.loc_tgv) vn =
  Printf.sprintf "(%s%s + (%d)/*%s*/)" pre (gen_tab l.loc_cat) l.loc_idx vn

let gen_tmp pre i vn off =
  Printf.sprintf "irdata->%stmps[irdata->tmps_org + (%d)/*%s*/%s]" pre i vn off

let gen_tmp_ptr pre i vn = Printf.sprintf "&(%s)" (gen_tmp pre i vn "")

let gen_ref_ptr pre i vn =
  Printf.sprintf "irdata->%sref[irdata->ref_org + (%d)/*%s*/]" pre i vn

let gen_ref pre i vn off = Printf.sprintf "*(%s%s)" (gen_ref_ptr pre i vn) off

let gen_def (v : Com.Var.t) offset =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv "D" l vn offset
  | LocTmp (_, i) -> gen_tmp "def_" i vn offset
  | LocRef (_, i) -> gen_ref "def_" i vn offset
  | LocArg (_, i) -> Pp.spr "def_arg%d" i
  | LocRes _ -> Pp.spr "(*def_res)"

let gen_val (v : Com.Var.t) offset =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv "" l vn offset
  | LocTmp (_, i) -> gen_tmp "" i vn offset
  | LocRef (_, i) -> gen_ref "" i vn offset
  | LocArg (_, i) -> Pp.spr "val_arg%d" i
  | LocRes _ -> Pp.spr "(*val_res)"

let gen_info_ptr (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) ->
      Printf.sprintf "((T_varinfo *)&(varinfo_%s[%d]/*%s*/))" l.loc_cat_str
        l.loc_cat_idx vn
  | LocTmp (_, i) -> gen_tmp_ptr "info_" i vn
  | LocRef (_, i) -> gen_ref_ptr "info_" i vn
  | LocArg _ | LocRes _ -> "NULL"

let gen_def_ptr (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv_ptr "D" l vn
  | LocTmp (_, i) -> gen_tmp_ptr "def_" i vn
  | LocRef (_, i) -> gen_ref_ptr "def_" i vn
  | LocArg (_, i) -> Pp.spr "(&def_arg%d)" i
  | LocRes _ -> Pp.spr "def_res"

let gen_val_ptr (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv_ptr "" l vn
  | LocTmp (_, i) -> gen_tmp_ptr "" i vn
  | LocRef (_, i) -> gen_ref_ptr "" i vn
  | LocArg (_, i) -> Pp.spr "(&val_arg%d)" i
  | LocRes _ -> Pp.spr "val_res"

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
  | LocTmp (_, i) -> Printf.sprintf "EST_TEMPORAIRE | %d" i
  | LocRef (_, i) ->
      let info = gen_ref_ptr "info_" i vn in
      Printf.sprintf "%s->loc_cat | %s->idx" info info
  | LocArg (_, i) -> Printf.sprintf "EST_ARGUMENT | %d" i
  | LocRes _ -> Printf.sprintf "EST_RESULTAT | 0"

let gen_size (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv _ | LocTmp _ -> Format.sprintf "%d" (Com.Var.size v)
  | LocRef (_, i) -> Format.sprintf "(%s->size)" (gen_ref_ptr "info_" i vn)
  | LocArg _ | LocRes _ -> "1"
