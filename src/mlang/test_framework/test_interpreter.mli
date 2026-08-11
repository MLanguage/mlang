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

type target_dbg_info = { target : string; dbg_info : M_interpreter.Dbg_info.t }

val check_all_tests :
  Mir.program ->
  string ->
  Config.value_sort ->
  Config.round_ops ->
  (string -> bool) ->
  unit
(** [check_all_tests p folder vs ro filter] Executes [p] with all tests in
    [folder] whose name satisfy [filter]. *)

val check_test :
  Mir.program ->
  Irj_file.input ->
  Config.value_sort ->
  Config.round_ops ->
  StrSet.t ->
  target_dbg_info list

val check_one_test :
  Mir.program -> string -> Config.value_sort -> Config.round_ops -> unit
(** Same as [check_all_tests], but for one test. *)

exception InterpError of int
