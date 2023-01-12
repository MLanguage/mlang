
open Common

let read_test filename =

  let test = Read_test.read_test filename in
  let tgv = TGV.alloc_tgv () in
  let res_prim =
    List.fold_left (fun res_prim s ->
        match s with
        | `EntreesPrimitif pl ->
          List.iter (fun (code, montant) ->
              TGV.set tgv code montant)
            pl;
          res_prim
        | `ResultatsPrimitif pl ->
          let res_prim =
            List.fold_left (fun resultats (code, montant) ->
                StrMap.add code montant resultats
              ) res_prim pl
          in
          res_prim
        | _ ->
          res_prim
      ) StrMap.empty test
  in
  tgv, res_prim

let check_result tgv err expected_tgv expected_err =
  let result = ref true in
  StrMap.iter (fun code montant ->
      (* Consider missing variables as 0.0 and dismiss NBPT (DGFiP does the same) *)
      (* NBPT variable doesn't bear any meaning due to DGFiP test generation method (default value for resultats primitif) *)
      match code with
      | "NBPT" -> ()
      | _ -> let montant' = try TGV.get_def tgv code 0.0 with Not_found -> 0.0 in
       if montant <> montant' then
        (result := false;
         Printf.eprintf "KO | %s attendu: %f - calculé: %f\n"
           code montant montant')
    ) expected_tgv;
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
      StrSet.add (Bytes.to_string str) strs
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
      Printf.eprintf "%s: %s found, expected %s\n%!"
        var (hex2floatstr res) (hex2floatstr exp)
    )
    diffs;
  close_in out;
  close_in outexp

let run_test test_file =
  Printf.printf "Testing %s...\n%!" test_file;

  let annee_calc = M.annee_calc () in

  let out = Printf.sprintf "%d.output/%s.tgv" annee_calc (Filename.basename test_file) in
  let out_exp = Printf.sprintf "%d.expected/%s.tgv" annee_calc (Filename.basename test_file) in

  let tgv, res_prim  = read_test test_file in

  let annee_revenu = TGV.get_int_def tgv "ANREV" annee_calc in
  if annee_revenu <> annee_calc then
    Printf.eprintf "Attention, année calculette (%d) <> année revenu (%d)\n%!"
      annee_calc annee_revenu;

  let annee_courante = annee_calc + 1 in

  TGV.set_int tgv "IND_TRAIT" 4 (* = primitif *);
  TGV.set_int tgv "ANCSDED" annee_courante;

  let err1 = M.verif_saisie_cohe_primitive tgv in
  let err2 =
    if List.exists (fun e -> e.[0] = 'A') err1 then
      begin
        Printf.eprintf
          "Anomalies dans les données saisies, pas de calcul primitif\n%!";
        []
      end
    else
      begin
        let _err = M.calcul_primitif_isf tgv in
        let _err = M.verif_calcul_primitive_isf tgv in
        TGV.reset_base tgv;
        let err = M.traite_double_liquidation_2 tgv M.Primitif in
        let _err = M.sauve_base_initial tgv in
        let _err = M.sauve_base_1728 tgv in
        let _err = M.sauve_base_anterieure tgv in
        let _err = M.sauve_base_anterieure_cor tgv in
        let _err = M.sauve_base_inr_inter22 tgv in
        err
      end
  in

  let err = err1 @ err2 in
  M.dump_raw_tgv_in out tgv err;

  let res_ok = check_result tgv [] res_prim [] in

  if not (Sys.file_exists out_exp)
  then begin
    Printf.eprintf "Expected results file %s not found." out_exp;
    exit (if res_ok then 10 else 11)
  end;

  begin
    match Unix.system (Printf.sprintf "diff -q %s %s 1>/dev/null 2>&1" out out_exp) with
    | WEXITED 0 ->
        if res_ok then 0 else 1
    | WEXITED 1 ->
      compare_dump out out_exp;
      if res_ok then 20 else 21
    | WEXITED 2 ->
        if res_ok then 10 else 11
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
  let rec loop =
    function
    | [] -> 0
    | test_file::args ->
      let res = run_test test_file in
      if res <> 0 then res else loop args
  in
  loop args

let () =
  Printexc.record_backtrace true;
  try
    let res = main () in
    exit res
  with e ->
    Printf.eprintf "%s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 30
