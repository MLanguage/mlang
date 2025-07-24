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

(** This module helps to keep track of the previous compilation metadata to
    prevent re-parsing and re-generation of C files. Warning: this module keeps
    a state for memoizing file digests. *)

type file_metadata = {
  m_file : string;
  (* The original M file name *)
  mast : Mast.source_file;
  (* The Mast of the M file *)
  uses_const : float StrMap.t option;
      (* The constants used and
         inlined before mirification. *)
}

type t
(** A map of file metadata, linking file digests to their metadata. *)

val empty : t
(** An empty map. *)

val read_metadata : output_dir:string -> t
(** Reads the metadata file, expected to be [output_dir] ^
    "project-data.internal". If it does not exist, returns an empty map. *)

val write_metadata : output_dir:string -> t -> unit
(** Writes the metadata in argument in [output_dir] ^ "project-data.internal".
*)

val get_file_data : string -> t -> file_metadata option
(** Returns the metadata of the m file in argument. *)

val add_file_to_metadata : file_metadata -> t -> t
(** Adds a file metadata to the metadata map. If it already exists for a given m
    file, overrides it. *)

val add_variable_map_to_metadata : Com.Var.t StrMap.t -> t -> t
(** Adds a variable map to the metadata, which is used to check if all files
    require a regeneration (see [all_files_need_regeneration]). *)

val file_uses_const : file:string -> const:string -> value:float -> t -> t
(** Adds a constant to the map of used constants for a given m file. *)

val file_needs_regeneration :
  file:string -> loaded_metadata:t -> new_metadata:t -> bool
(** Checks if a file needs to be regenerated given the loaded metadata and the
    newly computed metadata. *)

val all_files_need_regeneration : loaded_metadata:t -> new_metadata:t -> bool
(** Checks if all the files need regeneration, which happens if the order of
    variables have changed. *)

(* Generic helper *)

val target_file_from_m_file : string -> string
(** From a m file name, returns the basename prefix of the C file. *)
