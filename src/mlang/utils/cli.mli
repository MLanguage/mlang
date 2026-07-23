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

(** Command-line interface helpers *)

(**{2 Command line arguments parsing}*)

val mlang_t :
  (string list ->
  string list ->
  bool ->
  bool ->
  string list ->
  bool ->
  bool ->
  string option ->
  string option ->
  string option ->
  bool ->
  string option ->
  string option ->
  bool ->
  string option ->
  string option ->
  float option ->
  int ->
  bool ->
  string list option ->
  bool ->
  bool ->
  bool ->
  string option ->
  Config.message_format ->
  Config.optim list ->
  (string * float option option) list ->
  'a) ->
  'a Cmdliner.Term.t
(** Mlang binary command-line arguments parsing function *)

val info : Cmdliner.Cmd.info
(** Command-line man page for --help *)

val add_prefix_to_each_line : string -> (int -> string) -> string
(** [add_prefix_to_each_line msg prefix] will print msg but each line with line
    number [i] starts with the string [prefix i]*)

val retrieve_loc_text : Pos.t -> string
(** [retrieve_loc_text pos] reads the source file associated with [pos] and
    returns a formatted string of the code at that location, with the exact
    columns highlighted. This is used to display code snippets in error
    messages. *)
