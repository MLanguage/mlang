
open Common

let read_test filename =

  let test = Read_test.read_test filename in

  List.fold_left (fun (tgv, res_prim) s ->
      match s with
      | `EntreesPrimitif pl ->
        let tgv =
          List.fold_left (fun tgv (code, montant) ->
              try TGV.set tgv code montant
              with Not_found ->
                Printf.eprintf "Variable %s inexistante, test invalide\n" code;
                exit 2
            ) tgv pl
        in
        tgv, res_prim
      | `ResultatsPrimitif pl ->
        let res_prim =
          List.fold_left (fun resultats (code, montant) ->
              StrMap.add code montant resultats
            ) res_prim pl
        in
        tgv, res_prim
      | _ ->
        tgv, res_prim
    ) (TGV.empty, StrMap.empty) test

let check_result tgv err expected_tgv expected_err =
  let result = ref 0 in
  StrMap.iter (fun code montant ->
      (* Consider missing variables as 0.0 (DGFiP does the same) *)
      let montant' = try TGV.get_def tgv code 0.0 with Not_found -> 0.0 in
      if montant <> montant' then
        (result := 1;
         Printf.eprintf "KO | %s attendu: %f - calculé: %f\n"
           code montant montant')
    ) expected_tgv;
  !result

let run_test test_file =
  Printf.printf "Testing %s...\n%!" test_file;

  let tgv, res_prim  = read_test test_file in

  let annee_calc = M.annee_calc () in
  let annee_revenu = TGV.get_int_def tgv "ANREV" annee_calc in
  if annee_revenu <> annee_calc then
    Printf.eprintf "Attention, année calculette (%d) <> année revenu (%d)\n%!"
      annee_calc annee_revenu;

  let annee_courante = 1900 + (Unix.localtime (Unix.time ())).tm_year in

  let tgv = TGV.set_int tgv "IND_TRAIT" 4 (* = primitif *) in
  let tgv = TGV.set_int tgv "ANCSDED" annee_courante in

  let tgv = M.traite_double_liquidation_2 tgv M.Primitif in

  check_result tgv [] res_prim []

let main () =

  if Array.length Sys.argv < 2 then
    begin
      Printf.printf "Syntaxe :\n%s fichier_test\n" Sys.argv.(0);
      exit 3
    end;

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
    Printf.printf "%s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout;
    exit 4
