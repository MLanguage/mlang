open Mlang.Test_ast

let open_file filename =
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  (oc, fmt)

let print_comma oc () = Format.fprintf oc ","

let format_value fmt (value : value) =
  match value with
  | Int i -> Format.fprintf fmt "%d" i
  | Float f -> Format.fprintf fmt "%f" f

let format_code_revenu fmt ((var, value, _) : input) =
  Format.fprintf fmt
    {|@;<0 2>{@;<0 4>"code": "%s",@;<0 4>"valeur": "%a"@;<0 2>}|} var
    format_value value

let format_rappel fmt rappel =
  Format.fprintf fmt
    {|@;<0 2>{@;<0 4>"numEvt": "%d",@;<0 4>"numRappel": "%d",@;<0 4>"descripteur": "%s",@;<0 4>"montant": "%d",@;<0 4>"sens": "%s",@;<0 4>"penalite": "%a",@;<0 4>"baseTL": "%a",@;<0 4>"date": "%.6d",@;<0 4>"abatt": "%a"@;<0 2>}|}
    rappel.num_evt rappel.num_rap rappel.variable rappel.value rappel.sens
    (Format.pp_print_option Format.pp_print_int)
    rappel.penalite
    (Format.pp_print_option Format.pp_print_int)
    rappel.base_tl rappel.date_inr
    (Format.pp_print_option Format.pp_print_int)
    rappel.ind20

let format_code_list fmt input_list =
  Format.pp_print_list ~pp_sep:print_comma format_code_revenu fmt input_list

let format_rappel_list fmt rappels =
  Format.pp_print_list ~pp_sep:print_comma format_rappel fmt rappels

let gen_pas_calc_json_primitif fmt test_data =
  (* let offset = 4 in *)
  Format.fprintf fmt
    {|@[<h 2>{@,"formatAvis": "texte",@,"listeCodes": [%a@,]@]|}
    format_code_list test_data.ep;
  Format.fprintf fmt "}\n";
  Format.pp_print_flush fmt ()

let gen_pas_calc_json_correctif fmt test_data =
  (*Pour l’instant on va se contenter de partir en dur sur du correctif avec avis.*)
  let rappels : rappel list option =
    match test_data.rapp with
    (* | Some (a, _, _) -> Some a *)
    (* pour remetter les rappels*)
    | Some _ -> None
    | None -> None
  in
  Format.fprintf fmt
    {|@[<h 2>{@,"formatAvis": "texte",@,"codesRevenu": [%a@,],@,"lignesRappel": [%a@,]@]|}
    format_code_list test_data.ep
    (Format.pp_print_option format_rappel_list)
    rappels;
  Format.fprintf fmt "}\n";
  Format.pp_print_flush fmt ()
