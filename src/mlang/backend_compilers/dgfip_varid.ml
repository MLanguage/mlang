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
  | Com.CatVar.LocComputed -> "C_"
  | Com.CatVar.LocBase -> "B_"
  | Com.CatVar.LocInput -> "S_"

let gen_tgv_def (l : Com.loc_tgv) vn off =
  Pp.spr "D%s[%d/*%s*/%s]" (gen_tab l.loc_cat) l.loc_idx vn off

let gen_tgv_val (l : Com.loc_tgv) vn off =
  Pp.spr "%s[%d/*%s*/%s]" (gen_tab l.loc_cat) l.loc_idx vn off

let gen_tgv_def_ptr (l : Com.loc_tgv) vn =
  Pp.spr "(D%s + (%d)/*%s*/)" (gen_tab l.loc_cat) l.loc_idx vn

let gen_tgv_val_ptr (l : Com.loc_tgv) vn =
  Pp.spr "(%s + (%d)/*%s*/)" (gen_tab l.loc_cat) l.loc_idx vn

let gen_tgv_info_ptr (l : Com.loc_tgv) vn =
  Pp.spr "I_(%s,%d/*%s*/)" l.loc_cat_str l.loc_cat_idx vn

(* temporary variables accessors *)

let gen_tmp_def i vn off = Pp.spr "DT_((%d)/*%s*/%s)" i vn off

let gen_tmp_val i vn off = Pp.spr "T_((%d)/*%s*/%s)" i vn off

let gen_tmp_def_ptr i vn = Pp.spr "&(%s)" (gen_tmp_def i vn "")

let gen_tmp_val_ptr i vn = Pp.spr "&(%s)" (gen_tmp_val i vn "")

let gen_tmp_info_ptr i vn = Pp.spr "IT_((%d)/*%s*/)" i vn

(* reference accessors *)

let gen_ref_def_ptr i vn = Printf.sprintf "DR_((%d)/*%s*/)" i vn

let gen_ref_val_ptr i vn = Printf.sprintf "R_((%d)/*%s*/)" i vn

let gen_ref_info_ptr i vn = Printf.sprintf "IR_((%d)/*%s*/)" i vn

let gen_ref_def i vn off = Pp.spr "*(%s%s)" (gen_ref_def_ptr i vn) off

let gen_ref_val i vn off = Pp.spr "*(%s%s)" (gen_ref_val_ptr i vn) off

(* arguments accessors *)

let gen_arg_def i = Pp.spr "arg_def%d" i

let gen_arg_val i = Pp.spr "arg_val%d" i

let gen_arg_def_ptr i = Pp.spr "(&arg_def%d)" i

let gen_arg_val_ptr i = Pp.spr "(&arg_val%d)" i

(* result accessors *)

let gen_res_def () = Pp.spr "(*res_def)"

let gen_res_val () = Pp.spr "(*res_val)"

let gen_res_def_ptr () = Pp.spr "res_def"

let gen_res_val_ptr () = Pp.spr "res_val"

(* generic accessors *)

let gen_def (v : Com.Var.t) offset =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv_def l vn offset
  | LocTmp (_, i) -> gen_tmp_def i vn offset
  | LocRef (_, i) -> gen_ref_def i vn offset
  | LocArg (_, i) -> gen_arg_def i
  | LocRes _ -> gen_res_def ()

let gen_val (v : Com.Var.t) offset =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv_val l vn offset
  | LocTmp (_, i) -> gen_tmp_val i vn offset
  | LocRef (_, i) -> gen_ref_val i vn offset
  | LocArg (_, i) -> gen_arg_val i
  | LocRes _ -> gen_res_val ()

let gen_info_ptr (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv_info_ptr l vn
  | LocTmp (_, i) -> gen_tmp_info_ptr i vn
  | LocRef (_, i) -> gen_ref_info_ptr i vn
  | LocArg _ | LocRes _ -> "NULL"

let gen_def_ptr (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv_def_ptr l vn
  | LocTmp (_, i) -> gen_tmp_def_ptr i vn
  | LocRef (_, i) -> gen_ref_def_ptr i vn
  | LocArg (_, i) -> gen_arg_def_ptr i
  | LocRes _ -> gen_res_def_ptr ()

let gen_val_ptr (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv (_, l) -> gen_tgv_val_ptr l vn
  | LocTmp (_, i) -> gen_tmp_val_ptr i vn
  | LocRef (_, i) -> gen_ref_val_ptr i vn
  | LocArg (_, i) -> gen_arg_val_ptr i
  | LocRes _ -> gen_res_val_ptr ()

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
  | LocTmp (_, i) -> Pp.spr "EST_TEMPORAIRE | %d" i
  | LocRef (_, i) ->
      let info = gen_ref_info_ptr i vn in
      Printf.sprintf "%s->loc_cat | %s->idx" info info
  | LocArg (_, i) -> Pp.spr "EST_ARGUMENT | %d" i
  | LocRes _ -> Pp.spr "EST_RESULTAT | 0"

let gen_size (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  match v.loc with
  | LocTgv _ | LocTmp _ -> Pp.spr "%d" (Com.Var.size v)
  | LocRef (_, i) -> Pp.spr "(%s->size)" (gen_ref_info_ptr i vn)
  | LocArg _ | LocRes _ -> "1"
