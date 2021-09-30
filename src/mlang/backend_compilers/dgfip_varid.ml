(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

module StringMap = Map.Make (String)

type var_id =
 | VarInput of int
 | VarBase of int
 | VarComputed of int

type var_id_map = var_id StringMap.t

let gen_access_def vm v =
  match StringMap.find v vm with
  | VarInput i -> Printf.sprintf "DS_[%d /*%s*/]" i v
  | VarBase i -> Printf.sprintf "DB_[%d /*%s*/]" i v
  | VarComputed i -> Printf.sprintf "DC_[%d /*%s*/]" i v

let gen_access_val vm v =
  match StringMap.find v vm with
  | VarInput i -> Printf.sprintf "S_[%d /*%s*/]" i v
  | VarBase i -> Printf.sprintf "B_[%d /*%s*/]" i v
  | VarComputed i -> Printf.sprintf "C_[%d /*%s*/]" i v
