
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

let run_test test_file =
  Printf.printf "Testing %s...\n%!" test_file;

  let out = Printf.sprintf "output/%s.tgv" (Filename.basename test_file) in
  let out_exp = Printf.sprintf "output.expected/%s.tgv" (Filename.basename test_file) in

  let tgv, res_prim  = read_test test_file in

  let annee_calc = M.annee_calc () in
  let annee_revenu = TGV.get_int_def tgv "ANREV" annee_calc in
  if annee_revenu <> annee_calc then
    Printf.eprintf "Attention, année calculette (%d) <> année revenu (%d)\n%!"
      annee_calc annee_revenu;

  let annee_courante = 1900 + (Unix.localtime (Unix.time ())).tm_year in

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

  begin
    match Unix.system (Printf.sprintf "diff -q %s %s 1>/dev/null 2>&1" out out_exp) with
    | WEXITED 0 ->
        if res_ok then 0 else 1
    | WEXITED 1 ->
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

  (try Unix.mkdir "output" 0o755 with _ -> ());

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
