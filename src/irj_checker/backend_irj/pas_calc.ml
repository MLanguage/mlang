open Utils
open Irj_utils.Irj_ast

type avis_type = Texte | Gavlir

let open_file filename =
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  (oc, fmt)

let print_comma oc () = Format.fprintf oc ","

let format_value fmt (value : literal) =
  match value with
  | I i -> Format.fprintf fmt "%d" i
  | F f -> Format.fprintf fmt "%f" f
  | U -> Format.fprintf fmt "indefini"

let format_code_revenu fmt
    ((Pos.Mark (var, _), Pos.Mark (value, _)) : var_value) =
  Format.fprintf fmt
    {|@;<0 2>{@;<0 4>"code": "%s",@;<0 4>"valeur": "%a"@;<0 2>}|} var
    format_value value

let format_rappel fmt (rappel : rappel) =
  Format.fprintf fmt
    {|@;<0 2>{@;<0 4>"numEvt": "%d",@;<0 4>"numRappel": "%d",@;<0 4>"descripteur": "%s",@;<0 4>"montant": "%d",@;<0 4>"sens": "%s",@;<0 4>"penalite": "%a",@;<0 4>"baseTL": "%a",@;<0 4>"date": "%.6d",@;<0 4>"abatt": "%a"@;<0 2>}|}
    rappel.event_nb rappel.rappel_nb rappel.variable_code rappel.change_value
    rappel.direction
    (Format.pp_print_option Format.pp_print_int)
    rappel.penalty_code
    (Format.pp_print_option Format.pp_print_int)
    rappel.base_tolerance_legale rappel.month_year
    (Format.pp_print_option Format.pp_print_int)
    rappel.decl_2042_rect

let format_code_list fmt input_list =
  Format.pp_print_list ~pp_sep:print_comma format_code_revenu fmt input_list

let format_rappel_list fmt rappels =
  Format.pp_print_list ~pp_sep:print_comma format_rappel fmt rappels

let format_avis_element fmt avis_type =
  Format.fprintf fmt {|"formatAvis": "%s",@,|}
    (match avis_type with Texte -> "texte" | Gavlir -> "gavlir")

let gen_pas_calc_json_primitif fmt (prim_data : prim_data_block) mode =
  Format.fprintf fmt {|@[<%c 2>{@,%a"listeCodes": [%a@,]@]|} mode
    format_avis_element Texte format_code_list prim_data.entrees;
  Format.fprintf fmt "}"

let gen_pas_calc_json_correctif fmt (test_data : irj_file) mode =
  (*Pour l’instant on va se contenter de partir en dur sur du correctif avec avis.*)
  let rappels : rappel list option =
    match test_data.rapp with
    | None -> None
    | Some rappels -> Some rappels.entrees_rappels
  in
  Format.fprintf fmt
    {|@[<%c 2>{@,%a"codesRevenu": [%a@,],@,"lignesRappel": [%a@,]@]|} mode
    format_avis_element Texte format_code_list test_data.prim.entrees
    (Format.pp_print_option format_rappel_list)
    rappels;
  Format.fprintf fmt "}"
