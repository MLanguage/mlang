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

type message_format_enum = Human | GNU

let irj_checker (f : string) (message_format : message_format_enum) : unit =
  try
    if not (Sys.file_exists f && not (Sys.is_directory f)) then
      Errors.raise_error
        (Format.asprintf "%s: this path is not a valid file in the filesystem" f);
    ignore (Mlang.Irj_file.parse_file f)
  with Errors.StructuredError (msg, pos, kont) ->
    (match message_format with
    | Human -> Cli.error_print "%a" Errors.format_structured_error
    | GNU -> Format.eprintf "%a" Errors.format_structured_error_gnu_format)
      (msg, pos);
    (match kont with None -> () | Some kont -> kont ());
    exit 123

let message_format_opt = [ ("human", Human); ("gnu", GNU) ]

let message_format =
  Arg.(
    value
    & opt (enum message_format_opt) Human
    & info [ "message-format" ]
        ~doc:
          "Selects the format of error and warning messages emitted by the \
           compiler. If set to $(i,human), the messages will be nicely \
           displayed and meant to be read by a human. If set to $(i, gnu), the \
           messages will be rendered according to the GNU coding standards.")

let file =
  let doc = "Test file (usually with the .irj extension)" in
  Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc)

let irj_checker_t = Term.(const irj_checker $ file $ message_format)

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
