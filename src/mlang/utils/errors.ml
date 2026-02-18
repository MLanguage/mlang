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

type raised_in = Validator

exception
  BlockingError of { raised_in : raised_in; error_message : Log.structured_msg }

exception StructuredError of (Log.structured_msg * (unit -> unit) option)

let raise_blocking_error ~raised_in ~msg =
  raise (BlockingError { raised_in; error_message = msg })

let raise_structured_error ?(kont : (unit -> unit) option)
    (msg : Log.structured_msg) =
  raise (StructuredError (msg, kont))

let raise_multispanned_error_with_continuation (msg : string)
    (spans : (string option * Pos.t) list) (kont : (unit -> unit) option) : 'a =
  raise_structured_error (Log.make msg ~spans) ?kont

let raise_multispanned_error (msg : string)
    (spans : (string option * Pos.t) list) =
  raise_multispanned_error_with_continuation msg spans None

let raise_spanned_error (msg : string) ?(span_msg : string option)
    (span : Pos.t) : 'a =
  raise_multispanned_error msg [ (span_msg, span) ]

let raise_error (msg : string) : 'a = raise_multispanned_error msg []

let raise_spanned_error_with_continuation (msg : string)
    ?(span_msg : string option) (span : Pos.t) (kont : unit -> unit) : 'a =
  raise_multispanned_error_with_continuation msg
    [ (span_msg, span) ]
    (Some kont)
