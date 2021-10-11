(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** {2 Structured error}*)

exception StructuredError of (string * (string option * Pos.t) list * (unit -> unit) option)

val format_structured_error : Format.formatter -> string * (string option * Pos.t) list -> unit

(** {2 Raising errors with useful error messages}*)

val raise_spanned_error : string -> ?span_msg:string -> Pos.t -> 'a

val raise_multispanned_error : string -> (string option * Pos.t) list -> 'a

val raise_error : string -> 'a

val raise_spanned_error_with_continuation :
  string -> ?span_msg:string -> Pos.t -> (unit -> unit) -> 'a
