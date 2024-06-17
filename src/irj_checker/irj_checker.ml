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

(** The Irj_checker Module is a simple entry point to use the Mlang IRJ file
    parser in order to perform syntactic checks on test files.

    Usage: irj_checker.exe <test_file.irj>*)

open Cmdliner
open Mlang

let irj_checker (f : string) : unit =
  try ignore (Mlang.Irj_file.parse_file f)
  with Errors.StructuredError (msg, pos, kont) ->
    Cli.error_print "%a" Errors.format_structured_error (msg, pos);
    (match kont with None -> () | Some kont -> kont ());
    exit 123

let file =
  let doc = "Test file (usually with the .irj extension)" in
  Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc)

let irj_checker_t = Term.(const irj_checker $ file)

let cmd =
  let doc = "parses, validates and transforms IRJ test files" in
  let man =
    [
      `S Manpage.s_bugs;
      `P "File bug reports at <https://github.com/MLanguage/mlang/issues>.";
    ]
  in
  Cmd.v
    (Cmd.info "irj_checker"
       ~version:
         (match Build_info.V1.version () with
         | None -> "n/a"
         | Some v -> Build_info.V1.Version.to_string v)
       ~doc ~man)
    irj_checker_t

let () = exit (Cmd.eval cmd)
