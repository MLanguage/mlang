(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2023 - 2026 DGFiP - INRIA                               *)
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

let mk_position sloc : Pos.t = Pos.make (fst sloc).Lexing.pos_fname sloc

type literal = I of int | F of float | U

type var_value = string Pos.marked * literal Pos.marked

(* type var_values = var_value list *)

type calc_error = string Pos.marked

(* type calc_errors = calc_error list *)

(* type rappel = string * string * var_value * string * string * string * string
 * string *)
type rappel = {
  event_nb : int;
  rappel_nb : int;
  variable_code : string;
  change_value : int;
  direction : string;
  (* R, C, M, P *)
  penalty_code : int option;
  (* 0 - 99 *)
  base_tolerance_legale : int option;
  month_year : int;
  (* MMYYYY *)
  decl_2042_rect : int option;
  (* 0 or 1 *)
  pos : Pos.t;
}

type prim_data_block = {
  entrees : var_value list;
  controles_attendus : calc_error list;
  resultats_attendus : var_value list;
}

type corr_data_block = {
  entrees_rappels : rappel list;
  controles_attendus : calc_error list;
  resultats_attendus : var_value list;
}

type irj_file = {
  nom : string;
  prim : prim_data_block;
  rapp : corr_data_block option;
      (* corr : prim_data_block option; *)
      (*corr is for old correctif form from primitif files, rapp is for the
        actual one in correctif files*)
}
