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

let make (f : string) (loc : Lexing.position * Lexing.position) =
  { pos_filename = f; pos_loc = loc }

let make_between (p1 : t) (p2 : t) : t =
  if p1.pos_filename <> p2.pos_filename then begin
    Cli.error_print "Conflicting position filenames: %s <> %s" p1.pos_filename
      p2.pos_filename;
    failwith "Pos error"
  end
  else
    let b1, e1 = p1.pos_loc in
    let b2, e2 = p2.pos_loc in
    let b = if b1.Lexing.pos_cnum < b2.Lexing.pos_cnum then b1 else b2 in
    let e = if e2.Lexing.pos_cnum < e1.Lexing.pos_cnum then e1 else e2 in
    let pos_loc = (b, e) in
    { p1 with pos_loc }

let format_gnu fmt pos =
  let s, e = pos.pos_loc in
  if s.Lexing.pos_lnum = e.Lexing.pos_lnum then
    Format.fprintf fmt "%s:%d.%d-%d"
      (Filename.basename pos.pos_filename)
      s.Lexing.pos_lnum
      (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
      (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)
  else
    Format.fprintf fmt "%s:%d.%d-%d.%d"
      (Filename.basename pos.pos_filename)
      s.Lexing.pos_lnum
      (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
      e.Lexing.pos_lnum
      (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)

let format_short fmt pos =
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

let format fmt (pos : t) =
  let s, e = pos.pos_loc in
  Format.fprintf fmt "in file %s, from %d:%d to %d:%d" pos.pos_filename
    s.Lexing.pos_lnum
    (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
    e.Lexing.pos_lnum
    (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)

type 'a marked =
  | Mark of 'a * t
      (** Everything related to the source code should keep its t stored, to
          improve error messages *)

(** Placeholder t *)
let none : t =
  let zero_pos =
    {
      Lexing.pos_fname = "";
      Lexing.pos_lnum = 0;
      Lexing.pos_cnum = 0;
      Lexing.pos_bol = 0;
    }
  in
  { pos_filename = "unknown t"; pos_loc = (zero_pos, zero_pos) }

let without (x : 'a) : 'a marked = Mark (x, none)

let mark value pos = Mark (value, pos)

let unmark (Mark (x, _) : 'a marked) : 'a = x

let get (Mark (_, x) : 'a marked) : t = x

let to_couple (Mark (x, p) : 'a marked) : 'a * t = (x, p)

let map (f : 'a -> 'b) (Mark (x, y) : 'a marked) : 'b marked = Mark (f x, y)

let same (x : 'a) (Mark (_, y) : 'b marked) : 'a marked = Mark (x, y)

let unmark_option (x : 'a marked option) : 'a option =
  match x with Some x -> Some (unmark x) | None -> None

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
    let oc, input_line_opt =
      try
        if filename == Dgfip_m.internal_m then
          let input_line_opt : unit -> string option =
            let curr = ref 0 in
            let src = Dgfip_m.declarations in
            let lng = String.length src in
            let rec new_curr () =
              if !curr < lng then
                if src.[!curr] = '\n' then (
                  let res = !curr in
                  incr curr;
                  Some res)
                else (
                  incr curr;
                  new_curr ())
              else None
            in
            function
            | () -> (
                let p0 = !curr in
                match new_curr () with
                | None -> None
                | Some p1 -> Some (String.sub Dgfip_m.declarations p0 (p1 - p0))
                )
          in
          (None, input_line_opt)
        else
          let ocf = open_in filename in
          let input_line_opt () : string option =
            try Some (input_line ocf) with End_of_file -> None
          in
          (Some ocf, input_line_opt)
      with Sys_error _ ->
        Cli.error_print "File not found for displaying position : \"%s\""
          filename;
        failwith "Pos error"
    in
    let print_matched_line (line : string) (line_no : int) : string =
      let line_indent = indent_number line in
      let error_indicator_style = [ ANSITerminal.red; ANSITerminal.Bold ] in
      let line_start_col =
        if line_no = sline then get_start_column pos else 1
      in
      let line_end_col =
        if line_no = eline then get_end_column pos else String.length line + 1
      in
      let line_length = String.length line + 1 in
      line
      ^
      if line_no >= sline && line_no <= eline then
        "\n"
        ^
        if line_no = sline && line_no = eline then
          Cli.format_with_style error_indicator_style "%*s" (line_end_col - 1)
            (String.make (line_end_col - line_start_col) '^')
        else if line_no = sline && line_no <> eline then
          Cli.format_with_style error_indicator_style "%*s" (line_length - 1)
            (String.make (line_length - line_start_col) '^')
        else if line_no <> sline && line_no <> eline then
          Cli.format_with_style error_indicator_style "%*s%s" line_indent ""
            (String.make (line_length - line_indent) '^')
        else if line_no <> sline && line_no = eline then
          Cli.format_with_style error_indicator_style "%*s%*s" line_indent ""
            (line_end_col - 1 - line_indent)
            (String.make (line_end_col - line_indent) '^')
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
    (match oc with Some ocf -> close_in ocf | _ -> ());
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
