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

(* ID of a variable in its sub-array of the TGV *)
type var_id =
  | VarInput of int
  | VarBase of int
  | VarComputed of int
  | VarRef of int

(* Map from variables to their TGV ID *)
type var_id_map = var_id Mir.VariableMap.t

let gen_tab = function
  | Some Com.CatVar.LocComputed -> "C_"
  | Some Com.CatVar.LocBase -> "B_"
  | Some Com.CatVar.LocInput -> "S_"
  | None -> assert false

let gen_ref tab i = Printf.sprintf "irdata->%sref[irdata->ref_org + (%d)]" tab i

let gen_ref_info vm (v : Com.Var.t) =
  match Mir.VariableMap.find v vm with
  | VarRef i -> gen_ref "info_" i
  | _ -> assert false

let gen_ref_def vm (v : Com.Var.t) =
  match Mir.VariableMap.find v vm with
  | VarRef i -> gen_ref "def_" i
  | _ -> assert false

let gen_ref_val vm (v : Com.Var.t) =
  match Mir.VariableMap.find v vm with
  | VarRef i -> gen_ref "" i
  | _ -> assert false

let gen_access_def vm (v : Com.Var.t) offset =
  let vn = Pos.unmark v.name in
  if Com.Var.is_temp v then
    Printf.sprintf "irdata->def_tmps[irdata->tmps_org + (%d)/*%s*/%s]"
      (Com.Var.loc_int v) vn offset
  else
    match Mir.VariableMap.find v vm with
    | VarInput i -> Printf.sprintf "DS_[%d/*%s*/%s]" i vn offset
    | VarBase i -> Printf.sprintf "DB_[%d/*%s*/%s]" i vn offset
    | VarComputed i -> Printf.sprintf "DC_[%d/*%s*/%s]" i vn offset
    | VarRef i -> Printf.sprintf "*(%s/*%s*/%s)" (gen_ref "def_" i) vn offset

let gen_access_val vm (v : Com.Var.t) offset =
  let vn = Pos.unmark v.name in
  if Com.Var.is_temp v then
    Printf.sprintf "irdata->tmps[irdata->tmps_org + (%d)/*%s*/%s]"
      (Com.Var.loc_int v) vn offset
  else
    match Mir.VariableMap.find v vm with
    | VarInput i -> Printf.sprintf "S_[%d/*%s*/%s]" i vn offset
    | VarBase i -> Printf.sprintf "B_[%d/*%s*/%s]" i vn offset
    | VarComputed i -> Printf.sprintf "C_[%d/*%s*/%s]" i vn offset
    | VarRef i -> Printf.sprintf "*(%s/*%s*/%s)" (gen_ref "" i) vn offset

let gen_access_pointer vm (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  if Com.Var.is_temp v then
    Printf.sprintf "&(irdata->tmps[irdata->tmps_org + (%d)/*%s*/])"
      (Com.Var.loc_int v) vn
  else
    match Mir.VariableMap.find v vm with
    | VarInput i -> Printf.sprintf "(S_ + %d/*%s*/)" i vn
    | VarBase i -> Printf.sprintf "(B_ + %d/*%s*/)" i vn
    | VarComputed i -> Printf.sprintf "(C_ + %d/*%s*/)" i vn
    | VarRef i -> Printf.sprintf "(%s/*%s*/)" (gen_ref "" i) vn

let gen_access_def_pointer vm (v : Com.Var.t) =
  let vn = Pos.unmark v.name in
  if Com.Var.is_temp v then
    Printf.sprintf "&(irdata->def_tmps[irdata->tmps_org + (%d)/*%s*/])"
      (Com.Var.loc_int v) vn
  else
    match Mir.VariableMap.find v vm with
    | VarInput i -> Printf.sprintf "(DS_ + %d/*%s*/)" i vn
    | VarBase i -> Printf.sprintf "(DB_ + %d/*%s*/)" i vn
    | VarComputed i -> Printf.sprintf "(DC_ + %d/*%s*/)" i vn
    | VarRef i -> Printf.sprintf "(%s/*%s*/)" (gen_ref "def_" i) vn

let gen_access_pos_from_start vm (v : Com.Var.t) =
  if Com.Var.is_temp v then
    Printf.sprintf "EST_TEMPORAIRE | %d" (-Com.Var.loc_int v)
  else
    match Mir.VariableMap.find v vm with
    | VarInput i -> Printf.sprintf "EST_SAISIE | %d" i
    | VarBase i -> Printf.sprintf "EST_BASE | %d" i
    | VarComputed i -> Printf.sprintf "EST_CALCULEE | %d" i
    | VarRef i ->
        let info = gen_ref "info_" i in
        Printf.sprintf "%s->loc_cat | %s->idx" info info

let gen_size vm (v : Com.Var.t) =
  let get_size (v : Com.Var.t) = Format.sprintf "%d" (Com.Var.size v) in
  if Com.Var.is_temp v then get_size v
  else
    match Mir.VariableMap.find v vm with
    | VarInput _ | VarBase _ | VarComputed _ -> get_size v
    | VarRef i -> Format.sprintf "(%s->size)" (gen_ref "info_" i)
