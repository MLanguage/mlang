
open Common

let read_test filename =
  let test = Read_test.read_test filename in
  let tgv = TGV.alloc_tgv () in
  let res_prim, ctl_prim =
    List.fold_left (fun (res_prim, ctl_prim) s ->
        match s with
        | `EntreesPrimitif pl ->
            List.iter (fun (code, montant) ->
              TGV.set tgv code montant
            ) pl;
            res_prim, ctl_prim
        | `ResultatsPrimitif pl ->
            let res_prim =
              List.fold_left (fun resultats (code, montant) ->
                  StrMap.add code montant resultats
                ) res_prim pl
            in
            res_prim, ctl_prim
        | `ControlesPrimitif el ->
            let ctl_prim =
              List.fold_left (fun erreurs e ->
                  StrSet.add e erreurs
                ) ctl_prim el
            in
            res_prim, ctl_prim
        | _ ->
            res_prim, ctl_prim
      ) (StrMap.empty, StrSet.empty) test
  in
  tgv, res_prim, ctl_prim

let check_result tgv err expected_tgv expected_err =
  let result = ref true in
  StrMap.iter (fun code montant ->
      (* Consider missing variables as 0.0 and dismiss NBPT (DGFiP does the same) *)
      (* NBPT variable doesn't bear any meaning due to DGFiP test generation method *)
      match code with
      | "NBPT" -> ()
      | _ ->
          let montant' = try TGV.get_def tgv code 0.0 with Not_found -> 0.0 in
          (* Comparaison des valeurs au centime près, conformément à la doctrine
             DGFiP actuellement en vigueur *)
          let comp =
            let m = Float.round (montant *. 100.) in
            let m' = Float.round (montant' *. 100.) in
            abs_float (m -. m') > 0.0
          in
          if comp then
            begin
              result := false;
              Printf.eprintf "KO | %s = %f au lieu de %f\n"
               code montant' montant
            end
    ) expected_tgv;
  let missing_errs = StrSet.diff expected_err err in
  let unexpected_errs = StrSet.diff err expected_err in
  if not (StrSet.is_empty missing_errs && StrSet.is_empty unexpected_errs) then (
    result := false;
    StrSet.iter (Printf.eprintf "KO | %s attendue non recue\n") missing_errs;
    StrSet.iter (Printf.eprintf "KO | %s recu en trop\n") unexpected_errs;
  );
  !result

let var_addr () =
  let vars = lazy
    (let vf = open_in "vars.txt" in
     let rec aux vars =
       try
         let line = input_line vf in
         match String.split_on_char ' ' line with
         | addr::var::_ -> aux ((int_of_string ("0x"^addr), var)::vars)
         | _ -> assert false
       with
       | End_of_file -> vars
     in
     aux [] |> List.rev)
  in
  Lazy.force vars

module StrSet = Set.Make(String)

let compare_dump out outexp =
  let out = open_in_bin out in
  let outexp = open_in_bin outexp in
  let read64 ic =
    let buf = Bytes.create 8 in
    really_input ic buf 0 8;
    buf
  in
  let rec read_val_diffs vars diffs =
    match vars with
    | [] -> diffs
    | (addr, var)::vars ->
      assert (pos_in out = addr);
      let res_val = read64 out in
      let expe_val = read64 outexp in
      if Bytes.equal res_val expe_val then
        read_val_diffs vars diffs
      else
        read_val_diffs vars ((var, res_val, expe_val)::diffs)
  in
  let rec read_strings ic strs =
    try
      let str = read64 ic in
      let strs = StrSet.add (Bytes.to_string str) strs in
      read_strings ic strs
    with
    | End_of_file -> strs
  in
  let diffs = read_val_diffs (var_addr ()) [] in
  let raised_disco = read_strings out StrSet.empty in
  let expected_disco = read_strings outexp StrSet.empty in
  StrSet.iter (fun ano ->
      Printf.eprintf "Raised unexpected discordance %s\n" ano
    ) (StrSet.diff raised_disco expected_disco);
  StrSet.iter (fun ano ->
      Printf.eprintf "Expected discordance %s not raised\n" ano
    ) (StrSet.diff expected_disco raised_disco);
  let undef = Int64.of_string "0xefbeaddeefbeadde" in
  let hex2floatstr bytes =
    let i64 = Bytes.get_int64_le bytes 0 in
    if Int64.equal i64 undef then "undef"
    else string_of_float(Int64.float_of_bits i64)
  in
  List.iter (fun (var, res, exp) ->
      Printf.eprintf "%s: %s found, expected %s\n"
        var (hex2floatstr res) (hex2floatstr exp)
    )
    diffs;
  flush stderr;
  close_in out;
  close_in outexp

let run_test test_file annee_exec flag_no_bin_compare =
  Printf.printf "Testing %s...\n%!" test_file;

  let annee_calc = M.annee_calc () in

  let out = Printf.sprintf "%d.output/%s.tgv" annee_calc (Filename.basename test_file) in
  let out_exp = Printf.sprintf "%d.expected/%s.tgv" annee_calc (Filename.basename test_file) in

  let tgv, res_prim, ctl_prim = read_test test_file in

  let annee_revenu = TGV.get_int_def tgv "ANREV" annee_calc in
  if annee_revenu <> annee_calc then (
    Printf.eprintf
      "Attention, année calculette (%d) <> année revenu (%d)\n%!"
      annee_calc
      annee_revenu
  );

  TGV.set_int tgv "IND_TRAIT" 4 (* = primitif *);
  TGV.set_int tgv "ANCSDED" annee_exec; (* instead of execution date *)
  init_errs ();
  let err = M.enchainement_primitif tgv in
  M.export_errs tgv;
  M.dump_raw_tgv_in out tgv err;

  let res_ok = check_result tgv (get_errs ()) res_prim ctl_prim in

  match flag_no_bin_compare with
  | true -> if res_ok then 0 else 1
  | false -> 
    if not (Sys.file_exists out_exp)
      then begin
      Printf.eprintf "Expected results file %s not found." out_exp;
      exit (if res_ok then 10 else 11)
    end;

    begin
      match Unix.system (Printf.sprintf "diff -q %s %s 1>/dev/null 2>&1" out out_exp) with
      | WEXITED 0 ->
          if res_ok then 0 else 1
      | WEXITED 1 -> (* Differences *)
          compare_dump out out_exp;
          if res_ok then 20 else 21
      | WEXITED 2 -> (* Other error *)
          if res_ok then 30 else 31
      | WEXITED _ ->
          32
      | WSIGNALED _ | WSTOPPED _ ->
          33
      end

let main () =
  if Array.length Sys.argv < 2 then
    begin
      Printf.printf "Syntaxe :\n%s fichier_test\n" Sys.argv.(0);
      exit 31
    end;

  (try Unix.mkdir ((string_of_int (M.annee_calc ()))^".output")
         0o755 with _ -> ());

  let args = List.tl (Array.to_list Sys.argv) in
  let annee_exec, test_files =
    match args with
    | "--annee" :: ann :: files ->
        let annee =
          try int_of_string ann with
          | _ ->
              Printf.eprintf "--annee accepte un entier comme argument (%s)\n" ann;
              exit 31
        in
        annee, files
    | "--annee" :: []->
        Printf.eprintf "argument manquant pour --annee\n";
        exit 31
    | files ->
        let annee = M.annee_calc () + 1 in
        annee, files
  in
  let rec loop = function
  | [] -> 0
  | test_file :: files ->
      let res = run_test test_file annee_exec true in
      (* Gc.minor (); (* sinon out of memory *)*)
      if res <> 0 then res else loop files
  in
  loop test_files

let () =
  Printexc.record_backtrace true;
  try
    let res = main () in
    free_errs ();
    exit res
  with e ->
    Printf.eprintf "%s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    free_errs ();
    exit 30
