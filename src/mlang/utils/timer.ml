(* Copyright (C) 2025 Contributor: Steven de Oliveira
   <steven.de-oliveira@ocamlpro.com>

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

type t = { mutable start : float option; mutable total : float }

let start ?(run = true) () : t =
  { start = (if run then Some (Sys.time ()) else None); total = 0. }

let restart (t : t) = t.start <- Some (Sys.time ())

let curr_time t =
  match t.start with
  | None -> t.total
  | Some s ->
      let new_tot = Sys.time () -. s in
      t.total +. new_tot

let stop t =
  t.total <- curr_time t;
  t.start <- None

let time_of f =
  let t = start () in
  let res = f () in
  let time = curr_time t in
  (time, res)
