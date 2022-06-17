open Mvalue
  
  (* .Chargement d'un fichier FIP 
  .Sortir la structure revenue_code list 
  .Appeler Ir_xx.calculate_tax 
  .Récupérer les valeurs de sorties 
  Comparer les valeurs de sorties à celles attendues précisées dans le fichier FIP 
  .Écrire le résultat quelque part *)[@ocamlformat "disable"]

type test_block = { block_name : string; block_data : Mvalue.revenue_code list }

type test_data = { test_name : string; blocks : test_block list }

let load_file (input_file : string) : string list =
  let ic = open_in input_file in
  let rec build_list line_list =
    match input_line ic with
    | next_line -> build_list (next_line :: line_list)
    | exception End_of_file ->
        close_in ic;
        List.rev line_list
    | exception e ->
        close_in_noerr ic;
        raise e
  in
  build_list []

let line2revenue_code line : Mvalue.revenue_code =
  match String.split_on_char '/' line with
  | [ head; tail ] -> { alias = head; value = Float.of_string tail }
  | [] -> failwith "Empty line !"
  | _ :: tail -> failwith "Line with multiple separators /."
(*let build_entry_list (input_file : string) : Mvalue.revenue_code list =*)

let parse_codes line_list : Mvalue.revenue_code list =
  match line_list with [] -> [] | _ -> List.map line2revenue_code line_list

let parse_block line_list : test_block =
  match line_list with
  | head :: tail when String.starts_with ~prefix:"#" head ->
      { block_name = head; block_data = parse_codes tail }
  | _ -> failwith "Data block has not the expected structure."

let rec split_blocks (line_list : string list)
    (line_list_list : string list list) : string list list =
  match line_list with
  | [] -> List.rev (List.map List.rev line_list_list)
  | line :: tail ->
      if String.starts_with ~prefix:"#" line then
        split_blocks tail ([ [ line ] ] @ line_list_list)
      else
        split_blocks tail
          (([ line ] @ List.hd line_list_list) :: List.tl line_list_list)

let parse_FIP line_list : test_data =
  match line_list with
  | head :: name :: tail ->
      { test_name = name; blocks = List.map parse_block (split_blocks tail []) }
  | _ -> failwith "FIP file has not the expected structure."

let get_data_block (input_file : string) (block_name : string) :
    Mvalue.revenue_code list =
  (List.find
     (fun block -> block.block_name = block_name)
     (parse_FIP (load_file input_file)).blocks)
    .block_data

let entry_list (input_file : string) : Mvalue.revenue_code list =
  get_data_block input_file "#ENTREES-PRIMITIF"

let write_codes (_output_file : string) (result_list : Mvalue.revenue_code list)
    : unit =
  let _oc = open_out _output_file in
  let oc = Format.formatter_of_out_channel _oc in
  let print_result_line fmt (result : Mvalue.revenue_code) =
    Format.fprintf fmt "%s : %f" result.alias result.value
  in
  let print_results fmt (result_list : Mvalue.revenue_code list) =
    Format.pp_print_list print_result_line fmt result_list
  in
  Format.fprintf oc "@[<v 0>%a@]@." print_results result_list;
  close_out _oc

let () =
  Format.printf "Programme %s lancé.\nA lu dans %s\n Écrira x_%s\n" Sys.argv.(0)
    Sys.argv.(1) Sys.argv.(2);
  write_codes ("Aentprim_" ^ Sys.argv.(2)) (entry_list Sys.argv.(1));
  write_codes
    ("Aresprim_" ^ Sys.argv.(2))
    (get_data_block Sys.argv.(1) "#RESULTATS-PRIMITIF");
  write_codes
    ("Arescorr_" ^ Sys.argv.(2))
    (get_data_block Sys.argv.(1) "#RESULTATS-CORRECTIF");
  write_codes
    ("Acalfix_" ^ Sys.argv.(2))
    (Ir_tests_2020.calculate_tax
       [
         { alias = "1AJ"; value = 120000.0 };
         { alias = "1BJ"; value = 120000.0 };
         { alias = "0CF"; value = 0.0 };
       ]);
  write_codes
    ("Acalcul_" ^ Sys.argv.(2))
    (Ir_tests_2020.calculate_tax (entry_list Sys.argv.(1)))

(* (Ir_basic_case.calculate_tax entry_list)*)