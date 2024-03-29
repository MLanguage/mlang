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
  | VarIterate of string * Mir.cat_variable_loc * Mir.cat_variable_data

(* Map from variables to their TGV ID *)
type var_id_map = var_id Mir.VariableMap.t

let gen_tab = function
  | Mir.LocCalculated -> "C_"
  | Mir.LocBase -> "B_"
  | Mir.LocInput -> "S_"

let gen_loc_type = function
  | Mir.LocCalculated -> "EST_CALCULEE"
  | Mir.LocBase -> "EST_BASE"
  | Mir.LocInput -> "EST_SAISIE"

let gen_access_def vm v offset =
  let vn = Pos.unmark v.Mir.Variable.name in
  if v.Mir.Variable.is_temp then Printf.sprintf "%s_def[0%s]" vn offset
  else
    match Mir.VariableMap.find v vm with
    | VarInput i -> Printf.sprintf "DS_[%d/*%s*/%s]" i vn offset
    | VarBase i -> Printf.sprintf "DB_[%d/*%s*/%s]" i vn offset
    | VarComputed i -> Printf.sprintf "DC_[%d/*%s*/%s]" i vn offset
    | VarIterate (t, l, _) ->
        Printf.sprintf "D%s[%s->idx/*%s*/%s]" (gen_tab l) t vn offset

let gen_access_val vm v offset =
  let vn = Pos.unmark v.Mir.Variable.name in
  if v.Mir.Variable.is_temp then Printf.sprintf "%s_val[0%s]" vn offset
  else
    match Mir.VariableMap.find v vm with
    | VarInput i -> Printf.sprintf "S_[%d/*%s*/%s]" i vn offset
    | VarBase i -> Printf.sprintf "B_[%d/*%s*/%s]" i vn offset
    | VarComputed i -> Printf.sprintf "C_[%d/*%s*/%s]" i vn offset
    | VarIterate (t, l, _) ->
        Printf.sprintf "%s[%s->idx/*%s*/%s]" (gen_tab l) t vn offset

let gen_access_pointer vm v =
  let vn = Pos.unmark v.Mir.Variable.name in
  if v.Mir.Variable.is_temp then Printf.sprintf "(%s_val)" vn
  else
    match Mir.VariableMap.find v vm with
    | VarInput i -> Printf.sprintf "(S_ + %d/*%s*/)" i vn
    | VarBase i -> Printf.sprintf "(B_ + %d/*%s*/)" i vn
    | VarComputed i -> Printf.sprintf "(C_ + %d/*%s*/)" i vn
    | VarIterate (t, l, _) ->
        Printf.sprintf "(%s + %s->idx/*%s*/)" (gen_tab l) t vn

let gen_access_def_pointer vm v =
  let vn = Pos.unmark v.Mir.Variable.name in
  if v.Mir.Variable.is_temp then Printf.sprintf "(%s_def)" vn
  else
    match Mir.VariableMap.find v vm with
    | VarInput i -> Printf.sprintf "(DS_ + %d/*%s*/)" i vn
    | VarBase i -> Printf.sprintf "(DB_ + %d/*%s*/)" i vn
    | VarComputed i -> Printf.sprintf "(DC_ + %d/*%s*/)" i vn
    | VarIterate (t, l, _) ->
        Printf.sprintf "(D%s + %s->idx/*%s*/)" (gen_tab l) t vn

let gen_access_pos_from_start vm v =
  if v.Mir.Variable.is_temp then assert false
  else
    match Mir.VariableMap.find v vm with
    | VarInput i -> Printf.sprintf "EST_SAISIE | %d" i
    | VarBase i -> Printf.sprintf "EST_BASE | %d" i
    | VarComputed i -> Printf.sprintf "EST_CALCULEE | %d" i
    | VarIterate (t, l, _) -> Printf.sprintf "%s | %s->idx" (gen_loc_type l) t

let gen_size vm v =
  let get_size v =
    match v.Mir.Variable.is_table with
    | Some i -> Format.sprintf "%d" i
    | None -> "1"
  in
  if v.Mir.Variable.is_temp then get_size v
  else
    match Mir.VariableMap.find v vm with
    | VarInput _ | VarBase _ | VarComputed _ -> get_size v
    | VarIterate (t, _, _) -> Format.sprintf "(%s->size)" t
