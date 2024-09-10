(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

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

exception
  StructuredError of
    (string * (string option * Pos.t) list * (unit -> unit) option)

let format_structured_error fmt
    ((msg, pos) : string * (string option * Pos.t) list) =
  Format.fprintf fmt "%s%s%s%s" msg
    (if List.length pos = 0 then "" else "\n\n")
    (String.concat "\n\n"
       (List.map
          (fun (msg, pos) ->
            Printf.sprintf "%s%s"
              (match msg with None -> "" | Some msg -> msg ^ "\n")
              (Pos.retrieve_loc_text pos))
          pos))
    (if List.length pos = 0 then "" else "\n")

let format_structured_error_gnu_format fmt
    ((msg, pos) : string * (string option * Pos.t) list) =
  if pos = [] then Format.fprintf fmt "%s\n" msg
  else
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_newline fmt ())
      (fun fmt (pos_msg, pos) ->
        Format.fprintf fmt "%a: %s %a\n" Pos.format_position_gnu pos msg
          (fun fmt pos_msg ->
            match pos_msg with
            | None -> ()
            | Some pos_msg -> Format.fprintf fmt "[%s]" pos_msg)
          pos_msg)
      fmt pos

let raise_spanned_error (msg : string) ?(span_msg : string option)
    (span : Pos.t) : 'a =
  raise (StructuredError (msg, [ (span_msg, span) ], None))

let raise_multispanned_error (msg : string)
    (spans : (string option * Pos.t) list) =
  raise (StructuredError (msg, spans, None))

let raise_error (msg : string) : 'a = raise (StructuredError (msg, [], None))

let raise_spanned_error_with_continuation (msg : string)
    ?(span_msg : string option) (span : Pos.t) (kont : unit -> unit) : 'a =
  raise (StructuredError (msg, [ (span_msg, span) ], Some kont))

let print_spanned_warning (msg : string) ?(span_msg : string option)
    (span : Pos.t) : unit =
  Cli.warning_print "%a" format_structured_error (msg, [ (span_msg, span) ])
