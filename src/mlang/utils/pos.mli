(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2021 - 2026 DGFiP - INRIA                               *)
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

(** {1 Source Code Position}

    This module provides utilities for handling and formatting source code
    positions. *)

(** {2 Types} *)

type t
(** The abstract type for a position in a source file. *)

(** A generic type to associate any value with a source code position. *)
type 'a marked =
  | Mark of 'a * t
      (** The constructor for a marked value, containing the value and its
          position [t]. *)

val make : string -> Lexing.position * Lexing.position -> t
(** [make filename (start_pos, end_pos)] creates a new position [t].
    @param filename The name of the source file.
    @param loc A tuple containing the start and end [Lexing.position]. *)

val make_between : t -> t -> t
(** [make_between p1 p2] creates a new position that starts at the beginning of
    [p1] and ends at the end of [p2].
    @raise Failure if [p1] and [p2] are from different files. *)

(** {2 Formatting} *)

val format_short : Format.formatter -> t -> unit
(** [format_short ppf pos] prints a concise, single-line representation of the
    position to the formatter [ppf]. Example: `in file foo.ml:10:5-15`. *)

val format_gnu : Format.formatter -> t -> unit
(** [format_gnu ppf pos] prints the position in a format compatible with GNU
    error message standards. (See:
    https://www.gnu.org/prep/standards/standards.html#Formatting-Error-Messages)
*)

val format : Format.formatter -> t -> unit
(** [format ppf pos] prints a detailed, human-readable representation of the
    position to the formatter [ppf]. Example: `in file foo.ml, from 10:5 to
    12:20`. *)

(** {2 Marked Value Manipulators} *)

val none : t
(** [none t] creates the null position. *)

val is_none : t -> bool
(** Returns [true] if the argument is equal to [none]. *)

val without : 'a -> 'a marked
(** [without x] marks the value [x] with the [none] position. *)

val mark : 'a -> t -> 'a marked
(** [mark value pos] creates a new marked value from a [value] and a position
    [pos]. *)

val unmark : 'a marked -> 'a
(** [unmark marked_value] extracts the original value from a marked item,
    discarding the position. *)

val get : 'a marked -> t
(** [get marked_value] extracts the position [t] from a marked item. *)

val to_couple : 'a marked -> 'a * t
(** [to_couple marked_value] converts a marked item into a tuple of
    [(value, position)]. *)

val map : ('a -> 'b) -> 'a marked -> 'b marked
(** [map f marked_value] applies the function [f] to the value inside
    [marked_value], returning a new marked value with the same position. *)

val same : 'a -> 'b marked -> 'a marked
(** [same new_value marked_value] creates a new marked value containing
    [new_value] but with the position of [marked_value]. *)

val unmark_option : 'a marked option -> 'a option
(** [unmark_option opt] unwraps and unmarks an optional marked value. If the
    input is [Some (Mark (x, p))], it returns [Some x]. Otherwise, [None]. *)

(** {2 Position Accessors} *)

val get_start_line : t -> int
(** [get_start_line pos] returns the starting line number of the position. *)

val get_start_column : t -> int
(** [get_start_column pos] returns the starting column number of the position.
*)

val get_end_line : t -> int
(** [get_end_line pos] returns the ending line number of the position. *)

val get_end_column : t -> int
(** [get_end_column pos] returns the ending column number of the position. *)

val get_file : t -> string
(** [get_file pos] returns the filename associated with the position. *)
