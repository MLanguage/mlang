open Mvalue

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

let get_file_in_dir (dir_handle : Unix.dir_handle) : string list =
  let rec build_list file_list =
    match Unix.readdir dir_handle with
    | next_file when next_file = "." || next_file = ".." -> build_list file_list
    | next_file -> build_list (next_file :: file_list)
    | exception End_of_file ->
        Unix.closedir dir_handle;
        file_list
    | exception e ->
        Unix.closedir dir_handle;
        raise e
  in
  build_list []

let compare_rev_code code1 code2 : int = compare code1.alias code2.alias

let line2revenue_code line : Mvalue.revenue_code =
  match String.split_on_char '/' line with
  | [ head; tail ] -> { alias = head; value = Float.of_string tail }
  | [] -> failwith "Empty line !"
  | _ :: tail -> failwith "Line with multiple separators /."

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

let reference_list (input_file : string) : Mvalue.revenue_code list =
  get_data_block input_file "#RESULTATS-PRIMITIF"

let filter_rev_code_list initial_list ref_list : Mvalue.revenue_code list =
  let alias_eq code1 code2 = code1.alias = code2.alias in
  let ref_exists code : bool =
    List.exists (fun ref_code -> alias_eq code ref_code) ref_list
  in
  List.filter ref_exists initial_list

let list_discrepancies filtered_list ref_list :
    (Mvalue.revenue_code * Mvalue.revenue_code) list =
  let value_neq (code1, code2) = not (code1.value = code2.value) in
  List.filter value_neq
    (List.combine
       (List.sort compare_rev_code filtered_list)
       (List.sort compare_rev_code ref_list))

let print_rev_code (oc : Format.formatter) (rev_code : Mvalue.revenue_code) :
    unit =
  Format.fprintf oc "%s/%f" rev_code.alias rev_code.value

let compute_discrepancies_from_file_2020 (fip_file : string) :
    (Mvalue.revenue_code * Mvalue.revenue_code) list =
  let tax_result = Ir_tests_2020.calculate_tax (entry_list fip_file) in
  let ref_list = reference_list fip_file in
  Format.printf "Test case: %s@." fip_file;
  list_discrepancies (filter_rev_code_list tax_result ref_list) ref_list

let print_file_discrepancies (oc : Format.formatter)
    (file_discrepancies_list :
      string * (Mvalue.revenue_code * Mvalue.revenue_code) list) : unit =
  let print_line fmt (discrepancy : Mvalue.revenue_code * Mvalue.revenue_code) =
    let result, reference = discrepancy in
    Format.fprintf fmt "%a instead of %a" print_rev_code result print_rev_code
      reference
  in
  let print_results fmt
      (result_list : (Mvalue.revenue_code * Mvalue.revenue_code) list) =
    Format.pp_print_list print_line fmt result_list
  in
  let filename, discrepancy_list = file_discrepancies_list in
  Format.fprintf oc "@[<v 2>File: %s@,%a@]@." filename print_results
    discrepancy_list

let print_discrepancies (oc : Format.formatter)
    (discrepancy_list : (Mvalue.revenue_code * Mvalue.revenue_code) list)
    (fip_file : string) : unit =
  let print_if_empty fmt (file, _) =
    Format.fprintf fmt "No discrepancy for file %s" file
  in
  Format.fprintf oc
    "@[<v 0>OCaml computed value | Reference value from file@,%a@]@."
    (if List.length discrepancy_list <> 0 then print_file_discrepancies
    else print_if_empty)
    (fip_file, discrepancy_list)

let test_FIP_2020 (fip_file : string) (output_file_name : string) : unit =
  Format.printf "Simple test on file %s.@." fip_file;
  let _oc = open_out (output_file_name ^ "_disc.txt") in
  let oc = Format.formatter_of_out_channel _oc in
  let discrepancy_list = compute_discrepancies_from_file_2020 fip_file in
  print_discrepancies oc discrepancy_list fip_file;
  close_out _oc

let run_test_directory (directory : string) (output_file_name : string) : unit =
  let dir_handle = Unix.opendir directory in
  let file_list =
    List.map (String.cat directory) (get_file_in_dir dir_handle)
  in
  let files_discrepancies_list =
    List.combine file_list
      (List.map compute_discrepancies_from_file_2020 file_list)
  in
  let _oc = open_out (output_file_name ^ "_disc.txt") in
  let oc = Format.formatter_of_out_channel _oc in
  let print_discrepancy_report fmt files_discrepancies_list =
    Format.pp_print_list print_file_discrepancies fmt files_discrepancies_list
  in
  let result_to_print =
    List.filter
      (fun (filename, discrepancy_list) -> List.length discrepancy_list <> 0)
      files_discrepancies_list
  in
  let print_if_empty fmt _placeholder =
    Format.fprintf fmt "Nothing to see here…"
  in
  Format.fprintf oc "@[<v 0>Discrepancy report@,%a@]@."
    (if List.length result_to_print <> 0 then print_discrepancy_report
    else print_if_empty)
    result_to_print;
  close_out _oc

let () =
  Format.printf "Starting %s.@." Sys.argv.(0);
  match Sys.argv.(1) with
  | "mono" -> test_FIP_2020 Sys.argv.(2) Sys.argv.(3)
  | "multi" -> run_test_directory Sys.argv.(2) Sys.argv.(3)
  | other -> Format.printf "Unknown command: %s@." other
