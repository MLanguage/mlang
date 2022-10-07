open Mvalue

type test_block = { block_name : string; block_data : Mvalue.revenue_code list }

type test_data = { test_name : string; blocks : test_block list }

let fichier (input_file : string) : Types_module.test_file = Fip.parse_file input_file

let revenue_code_list_from_var_values (values : Types_module.var_values) : Mvalue.revenue_code list =
  List.map (fun (var, value, pos) -> match value with
  | Types_module.F value -> Mvalue.{alias=var; value=value }
  | Types_module.I _ -> assert false) values

let entry_list_parsed (fichier : Types_module.test_file) : Mvalue.revenue_code list = revenue_code_list_from_var_values fichier.ep

let reference_list_parsed (fichier : Types_module.test_file) : Mvalue.revenue_code list = revenue_code_list_from_var_values fichier.rp

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
  let tax_result_array, errors = Ir.calculate_tax (entry_list_parsed (fichier fip_file)) in
  let tax_result = Array.to_list tax_result_array in
  let ref_list = reference_list_parsed (fichier fip_file) in
  let filtered_ref_list = filter_rev_code_list ref_list tax_result in
  let was_erased ref_list code : bool = not (List.mem code ref_list) in
  let erased_codes : revenue_code list =
    List.filter (was_erased filtered_ref_list) ref_list
  in
  let print_list fmt code_list =
    Format.pp_print_list print_rev_code fmt code_list
  in
  let warning_string fmt () =
    if List.length erased_codes <> 0 then
      Format.fprintf fmt
        "@.Warning: following codes were expected results but are not part of \
         the output variable list@.%a"
        print_list erased_codes
    else Format.fprintf fmt ""
  in
  Format.printf "Test case: %s%a@." fip_file warning_string ();
  list_discrepancies
    (filter_rev_code_list tax_result filtered_ref_list)
    filtered_ref_list

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

let print_error oc (error : m_error) : unit =
  Format.fprintf oc
    "@[<v 0>Codename: %s@,\
     kind: %s@,\
     codes: major %s minor %s@,\
     description: %s@,\
     alias: %s@]" error.name error.kind error.major_code error.minor_code
    error.description error.alias

let print_errors oc error_list = Format.pp_print_list print_error oc error_list

let () =
  Printexc.register_printer (function
    | M_exn e_list ->
        Some (Format.asprintf "M_exn@;<0 4>M Exception: %a" print_errors e_list)
    | _ -> None)

let compute_on_FIP_2020 (fip_file : string) (output_file_name : string) : unit =
  Format.printf "Simple test on file %s.@." fip_file;
  let _oc = open_out (output_file_name ^ "_disc.txt") in
  let oc = Format.formatter_of_out_channel _oc in
  let tax_result_array, errors =
    try Ir.calculate_tax (entry_list_parsed (fichier fip_file))
    with M_exn e_list ->
      Format.fprintf oc "TEST CASE %s ends with M Exception:@,@,%a@." fip_file
        print_errors e_list;
      close_out _oc;
      raise (M_exn e_list)
  in
  let tax_result = Array.to_list tax_result_array in
  let ref_list = reference_list_parsed (fichier fip_file) in
  let print_list fmt code_list =
    Format.pp_print_list print_rev_code fmt code_list
  in
  Format.fprintf oc
    "@[<v 0>TEST CASE: %s@,\
     @,\
     ANOMALIES@,\
     @,\
     %a@,\
     @,\
     REFERENCE@,\
     @,\
     %a@,\
     @,\
     OUTPUT@,\
     @,\
     %a@,\
     @,\
     FILTERED OUTPUT@,\
     @,\
     %a@]@."
    fip_file print_errors errors print_list
    (List.sort compare_rev_code ref_list)
    print_list
    (List.sort compare_rev_code tax_result)
    print_list
    (List.sort compare_rev_code (filter_rev_code_list tax_result ref_list));
  close_out _oc

let run_test_directory (directory : string) (output_file_name : string) : unit =
  let dir_handle = Unix.opendir directory in
  let file_list =
    List.map (fun (file_name) -> directory ^ file_name) (get_file_in_dir dir_handle)
  in
  let files_discrepancies_list =
    List.combine file_list
      (List.map compute_discrepancies_from_file_2020 file_list)
  in
  let _oc =
    if String.length output_file_name = 0 then stdout
    else open_out (output_file_name ^ "_disc.txt")
  in
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
  | "raw" -> compute_on_FIP_2020 Sys.argv.(2) Sys.argv.(3)
  | "mono" -> test_FIP_2020 Sys.argv.(2) Sys.argv.(3)
  | "multi" -> run_test_directory Sys.argv.(2) Sys.argv.(3)
  | other -> Format.printf "Unknown command: %s@." other
