(* Copyright (C) 2019 Inria, contributor: David Declerck <david.declerck@ocamlpro.com>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(* ID of a variable in its sub-array of the TGV *)
type var_id = VarInput of int | VarBase of int | VarComputed of int

(* Map from variables to their TGV ID *)
type var_id_map = var_id Mir.VariableMap.t

let gen_access_def vm v =
  let vn = Pos.unmark v.Mir.Variable.name in
  match Mir.VariableMap.find v vm with
  | VarInput i -> Printf.sprintf "DS_[%d /*%s*/]" i vn
  | VarBase i -> Printf.sprintf "DB_[%d /*%s*/]" i vn
  | VarComputed i -> Printf.sprintf "DC_[%d /*%s*/]" i vn

let gen_access_val vm v =
  let vn = Pos.unmark v.Mir.Variable.name in
  match Mir.VariableMap.find v vm with
  | VarInput i -> Printf.sprintf "S_[%d /*%s*/]" i vn
  | VarBase i -> Printf.sprintf "B_[%d /*%s*/]" i vn
  | VarComputed i -> Printf.sprintf "C_[%d /*%s*/]" i vn
