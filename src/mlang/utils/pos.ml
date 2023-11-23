(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** {1 Source code position} *)

type t = { pos_filename : string; pos_loc : Lexing.position * Lexing.position }
(** A position in the source code is a file, as well as begin and end location
    of the form col:line *)

let make_position (f : string) (loc : Lexing.position * Lexing.position) =
  { pos_filename = f; pos_loc = loc }

let format_position_short fmt pos =
  let s, e = pos.pos_loc in
  if s.Lexing.pos_lnum = e.Lexing.pos_lnum then
    Format.fprintf fmt "in file %s:%d:%d-%d"
      (Filename.basename pos.pos_filename)
      s.Lexing.pos_lnum
      (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
      (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)
  else
    Format.fprintf fmt "in file %s, from %d:%d to %d:%d"
      (Filename.basename pos.pos_filename)
      s.Lexing.pos_lnum
      (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
      e.Lexing.pos_lnum
      (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)

let format_position fmt (pos : t) =
  let s, e = pos.pos_loc in
  Format.fprintf fmt "in file %s, from %d:%d to %d:%d" pos.pos_filename
    s.Lexing.pos_lnum
    (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
    e.Lexing.pos_lnum
    (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)

type 'a marked = 'a * t
(** Everything related to the source code should keep its t stored, to improve
    error messages *)

(** Placeholder t *)
let no_pos : t =
  let zero_pos =
    {
      Lexing.pos_fname = "";
      Lexing.pos_lnum = 0;
      Lexing.pos_cnum = 0;
      Lexing.pos_bol = 0;
    }
  in
  { pos_filename = "unknown t"; pos_loc = (zero_pos, zero_pos) }

let unmark ((x, _) : 'a marked) : 'a = x

let get_position ((_, x) : 'a marked) : t = x

let map_under_mark (f : 'a -> 'b) ((x, y) : 'a marked) : 'b marked = (f x, y)

let same_pos_as (x : 'a) ((_, y) : 'b marked) : 'a marked = (x, y)

let unmark_option (x : 'a marked option) : 'a option =
  match x with Some x -> Some (unmark x) | None -> None

module VarNameToID = Map.Make (String)

let get_start_line (pos : t) : int =
  let s, _ = pos.pos_loc in
  s.Lexing.pos_lnum

let get_start_column (pos : t) : int =
  let s, _ = pos.pos_loc in
  s.Lexing.pos_cnum - s.Lexing.pos_bol + 1

let get_end_line (pos : t) : int =
  let _, e = pos.pos_loc in
  e.Lexing.pos_lnum

let get_end_column (pos : t) : int =
  let _, e = pos.pos_loc in
  e.Lexing.pos_cnum - e.Lexing.pos_bol + 1

let get_file (pos : t) : string = (fst pos.pos_loc).Lexing.pos_fname

let indent_number (s : string) : int =
  try
    let rec aux (i : int) = if s.[i] = ' ' then aux (i + 1) else i in
    aux 0
  with Invalid_argument _ -> String.length s

let retrieve_loc_text (pos : t) : string =
  let filename = get_file pos in
  let blue_style = [ ANSITerminal.Bold; ANSITerminal.blue ] in
  if filename = "" then "No position information"
  else
    let sline = get_start_line pos in
    let eline = get_end_line pos in
    let oc =
      try open_in filename
      with Sys_error _ ->
        Cli.error_print "File not found for displaying position : \"%s\""
          filename;
        exit (-1)
    in
    let input_line_opt () : string option =
      try Some (input_line oc) with End_of_file -> None
    in
    let print_matched_line (line : string) (line_no : int) : string =
      let line_indent = indent_number line in
      let error_indicator_style = [ ANSITerminal.red; ANSITerminal.Bold ] in
      line
      ^
      if line_no >= sline && line_no <= eline then
        "\n"
        ^
        if line_no = sline && line_no = eline then
          Cli.format_with_style error_indicator_style "%*s"
            (get_end_column pos - 1)
            (String.make (get_end_column pos - get_start_column pos) '^')
        else if line_no = sline && line_no <> eline then
          Cli.format_with_style error_indicator_style "%*s"
            (String.length line - 1)
            (String.make (String.length line - get_start_column pos) '^')
        else if line_no <> sline && line_no <> eline then
          Cli.format_with_style error_indicator_style "%*s%s" line_indent ""
            (String.make (String.length line - line_indent) '^')
        else if line_no <> sline && line_no = eline then
          Cli.format_with_style error_indicator_style "%*s%*s" line_indent ""
            (get_end_column pos - 1 - line_indent)
            (String.make (get_end_column pos - line_indent) '^')
        else assert false (* should not happen *)
      else ""
    in
    let include_extra_count = 0 in
    let rec get_lines (n : int) : string list =
      match input_line_opt () with
      | Some line ->
          if n < sline - include_extra_count then get_lines (n + 1)
          else if
            n >= sline - include_extra_count && n <= eline + include_extra_count
          then print_matched_line line n :: get_lines (n + 1)
          else []
      | None -> []
    in
    let pos_lines = get_lines 1 in
    let spaces = int_of_float (log10 (float_of_int eline)) + 1 in
    close_in oc;
    Cli.format_with_style blue_style "%*s--> %s\n%s" spaces "" filename
      (Cli.add_prefix_to_each_line
         (Printf.sprintf "\n%s" (String.concat "\n" pos_lines))
         (fun i ->
           let cur_line = sline - include_extra_count + i - 1 in
           if
             cur_line >= sline
             && cur_line <= sline + (2 * (eline - sline))
             && cur_line mod 2 = sline mod 2
           then
             Cli.format_with_style blue_style "%*d | " spaces
               (sline + ((cur_line - sline) / 2))
           else if cur_line >= sline - include_extra_count && cur_line < sline
           then Cli.format_with_style blue_style "%*d | " spaces cur_line
           else if
             cur_line <= sline + (2 * (eline - sline)) + 1 + include_extra_count
             && cur_line > sline + (2 * (eline - sline)) + 1
           then
             Cli.format_with_style blue_style "%*d | " spaces
               (cur_line - (eline - sline + 1))
           else Cli.format_with_style blue_style "%*s | " spaces ""))
