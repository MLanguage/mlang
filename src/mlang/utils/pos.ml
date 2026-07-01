(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2019 - 2026 DGFiP - INRIA                               *)
(*                                                                            *)
(* Ce programme est distribué sous la licence CeCILL-C: vous pouvez le        *)
(* redistribuer et/ou le modifier sous les contraintes de celle-ci.           *)
(*                                                                            *)
(* L'accessibilité au code source et les droits de copie, de modification et  *)
(* de redistribution qui découlent de ce contrat ont pour contrepartie de     *)
(* n'offrir aux utilisateurs qu'une garantie limitée et de ne faire peser sur *)
(* l'auteur du logiciel, le titulaire des droits patrimoniaux et les          *)
(* concédants successifs qu'une responsabilité restreinte.                    *)
(*                                                                            *)
(******************************************************************************)

(** {1 Source code position} *)

exception ConflictingFilenames of string * string

type t = { pos_filename : string; pos_loc : Lexing.position * Lexing.position }
(** A position in the source code is a file, as well as begin and end location
    of the form col:line *)

let make (f : string) (loc : Lexing.position * Lexing.position) =
  { pos_filename = f; pos_loc = loc }

let make_between (p1 : t) (p2 : t) : t =
  if p1.pos_filename <> p2.pos_filename then begin
    raise @@ ConflictingFilenames (p1.pos_filename, p2.pos_filename)
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

let is_none p = p = none

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
