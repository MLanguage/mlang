(* Copyright (C) 2023-2024 DGFiP, contributor: David Declerck, Mathieu Durero

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

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "This program requires a test file as argument\n";
    exit 1);
  let f = Sys.argv.(1) in
  let _tf =
    try Mlang.Irj_file.parse_file f with
    | Mlang.Irj_ast.TestParsingError (s, pos) ->
        let pos_1, pos_2 = pos.pos_loc in
        Printf.eprintf "%s" (Filename.basename f);
        Printf.eprintf ":(%d,%d)-(%d,%d)" pos_1.pos_lnum
          (pos_1.pos_cnum - pos_1.pos_bol)
          pos_2.pos_lnum
          (pos_2.pos_cnum - pos_2.pos_bol);
        Printf.eprintf " : %s\n" s;
        exit 1
    | _ ->
        Printf.eprintf "%s" (Filename.basename f);
        Printf.eprintf " : Unknown error\n";
        exit 1
  in
  ()
