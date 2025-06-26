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

type file_metadata = {
  m_file : string;
  mast : Mast.source_file;
  uses_const : float StrMap.t option;
}

let magic_header =
  lazy
    (match Build_info.V1.version () with
    | None -> "n/a"
    | Some v -> Build_info.V1.Version.to_string v)

let metadata_filename = "project-data.internal"

module FileDigest : sig
  type t

  val make : string -> t

  val compare : t -> t -> int
end = struct
  type t = string

  let memo = Hashtbl.create 13

  let make f =
    match Hashtbl.find_opt memo f with
    | Some v -> v
    | None ->
        let res = Digest.file f in
        Hashtbl.add memo f res;
        res

  let compare = String.compare
end

module FileMap = Map.Make (FileDigest)

type t = {
  filemap : file_metadata FileMap.t;
  variables : Com.Var.t StrMap.t option;
}

let to_lists t =
  ( FileMap.to_seq t.filemap |> Seq.fold_left (fun acc md -> md :: acc) [],
    Option.map
      (fun m -> StrMap.to_seq m |> Seq.fold_left (fun acc v -> v :: acc) [])
      t.variables )

let of_lists (l, lo) =
  {
    filemap =
      List.fold_left (fun acc (k, b) -> FileMap.add k b acc) FileMap.empty l;
    variables =
      Option.map
        (List.fold_left (fun acc (k, b) -> StrMap.add k b acc) StrMap.empty)
        lo;
  }

let empty = { filemap = FileMap.empty; variables = None }

(** Reads the metadata file assumed to be in output_dir. If it is absent,
    returns an empty map. If any error occurs, returns an empty map. *)
let read_metadata ~output_dir : t =
  let file = Filename.concat output_dir metadata_filename in
  match open_in_bin file with
  | exception Sys_error _ -> empty
  | chan ->
      let header_len = String.length (Lazy.force magic_header) in
      let header = really_input_string chan header_len in
      if header = Lazy.force magic_header then (
        match Marshal.from_channel chan with
        | exception End_of_file ->
            Format.eprintf "[File_versioning] End of file reached";
            close_in chan;
            empty
        | exception Failure s ->
            Format.eprintf
              "[File_versioning] Error while reading metadata file: %s" s;
            close_in chan;
            empty
        | msh ->
            let res = of_lists msh in
            close_in chan;
            res)
      else (
        close_in chan;
        empty)

(** (Over)writes the metadata file in output_dir. If any error occurs, returns
    an empty map. *)
let write_metadata ~output_dir t =
  let file = Filename.concat output_dir metadata_filename in
  match open_out_bin file with
  | exception Sys_error s ->
      Format.eprintf "Error while writing file %S: %s" file s
  | chan -> (
      output_string chan @@ Lazy.force magic_header;
      let d = to_lists t in
      match Marshal.to_channel chan d [ No_sharing; Compat_32 ] with
      | exception Failure s ->
          Format.eprintf
            "[File_versioning] Error while writing metadata file: %s" s;
          close_out chan
      | () -> close_out chan)

let get_file_data f (t : t) =
  try FileMap.find_opt (FileDigest.make f) t.filemap with Sys_error _ -> None

let add_file_to_metadata (fd : file_metadata) (t : t) : t =
  { t with filemap = FileMap.add (FileDigest.make fd.m_file) fd t.filemap }

let add_variable_map_to_metadata (m : Com.Var.t StrMap.t) (t : t) : t =
  { t with variables = Some m }

let file_uses_const ~file ~const ~value t : t =
  match get_file_data file t with
  | None ->
      Format.ksprintf invalid_arg
        "File_versioning.file_uses_metadata %S (file not found)" file
  | Some { uses_const = None; _ } ->
      Format.ksprintf invalid_arg
        "File_versioning.file_uses_metadata %S (uniitialized map)" file
  | Some ({ uses_const = Some e; _ } as mtdt) ->
      let uses_const = Some (StrMap.add const value e) in
      add_file_to_metadata { mtdt with uses_const } t

(* To check if a file needs regeneration, we compare its previous version
   to its new.
   1/ If the file does not have the same digest between the old and the new
   version (get_file_data file = None), then we must regenerate it.
   2/ If the previous version has not been expanded (expanded = None) or the
   new version has not been expanded (same condition, but it should not
   happen), then it is most likely the case the file does not exist at all.
   3/ Also, if the constants inlined differ (used_const <> used_const), then we
   must regerate them to take into account the difference.

   Otherwise, it means the file did not change (by 1/), all the constants it uses
   are given (by 2/) and they are strictly the same (by 3/). We conclude the
   generated files are equivalent. *)
let file_needs_regeneration ~file ~loaded_metadata ~new_metadata =
  match
    (get_file_data file loaded_metadata, get_file_data file new_metadata)
  with
  | None, _ | _, None -> true (* 1/ *)
  | Some load_mtdt, Some new_mtdt -> (
      match (load_mtdt.uses_const, new_mtdt.uses_const) with
      | None, _ | _, None -> true (* 2/ *)
      | Some load_uses, Some new_uses ->
          (* 3/ *)
          not @@ StrMap.equal Float.equal load_uses new_uses)

(* If the variable indexes are different between the two metadatas, then
   their index will *)
let all_files_need_regeneration ~(loaded_metadata : t) ~new_metadata =
  match (loaded_metadata.variables, new_metadata.variables) with
  | None, _ | _, None -> true
  | Some lmap, Some nmap ->
      StrMap.exists
        (fun k ({ loc; _ } : Com.Var.t) ->
          match StrMap.find k nmap with
          | exception Not_found -> true
          | { loc = loc'; _ } -> (
              match (loc, loc') with
              | LocTgv (_, l), LocTgv (_, l') -> l.loc_idx <> l'.loc_idx
              | _ -> loc <> loc'))
        lmap
(* Helper *)

let target_file_from_m_file file =
  let file =
    try Filename.(chop_extension @@ basename file)
    with Invalid_argument _ -> file
  in
  Format.sprintf "m_%s" file
