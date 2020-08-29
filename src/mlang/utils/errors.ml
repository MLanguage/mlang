(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

exception StructuredError of (string * (string option * Pos.t) list)

let format_structured_error fmt ((msg, pos) : string * (string option * Pos.t) list) =
  Format.fprintf fmt "%s%s%s" msg
    (if List.length pos = 0 then "" else "\n\n")
    (String.concat "\n\n"
       (List.map
          (fun (msg, pos) ->
            Printf.sprintf "%s%s"
              (match msg with None -> "" | Some msg -> msg ^ "\n")
              (Pos.retrieve_loc_text pos))
          pos))

let raise_spanned_error (msg : string) ?(span_msg : string option) (span : Pos.t) : 'a =
  raise (StructuredError (msg, [ (span_msg, span) ]))

let raise_multispanned_error (msg : string) (spans : (string option * Pos.t) list) =
  raise (StructuredError (msg, spans))

let raise_error (msg : string) : 'a = raise (StructuredError (msg, []))

(** Error formatting and helper functions *)

(**{1 Typechecking}*)

exception Unimplemented of string

(**{1 Others}*)

exception ArgumentError of string

exception TestError of string
