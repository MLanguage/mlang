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

type t
(** A simple timer to keep track of system time through a user friendly
    interface. *)

val start : ?run:bool -> unit -> t
(** Starts a timer. If [run] is set to false (default: [true]), the timer does
    not start yet. *)

val restart : t -> unit
(** Restarts a timer. *)

val curr_time : t -> float
(** Returns the timer's total active time. *)

val stop : t -> unit
(** Stops a timer. *)

val time_of : (unit -> 'a) -> float * 'a
(** Calls the function in argument and returns the time it took to run. *)
