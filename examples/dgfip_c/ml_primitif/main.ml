
open Common

let read_test filename =

  Printf.printf "Lecture du test\n";
  let test = Read_test.read_test filename in

  Printf.printf "Traduction du test\n";
  List.fold_left (fun (tgv, res_prim) s ->
      match s with
      | `EntreesPrimitif pl ->
        let tgv =
          List.fold_left (fun tgv (code, montant) ->
              try TGV.set tgv code montant
              with Not_found ->
                Printf.printf "Variable %s inexistante, test invalide\n" code;
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

let main () =

  if Array.length Sys.argv < 2 then
    begin
      Printf.printf "Syntaxe :\n%s fichier_test\n" Sys.argv.(0);
      exit 3
    end;

  let tgv, res_prim  = read_test Sys.argv.(1) in

  (* Normalement fait par règle 1 *)
  let tgv = TGV.set_int tgv "APPLI_BATCH" 0 in
  let tgv = TGV.set_int tgv "APPLI_OCEANS" 0 in
  let tgv = TGV.set_int tgv "APPLI_ILIAD" 1 in

  let annee_calc = M.annee_calc () in
  let annee_revenu = TGV.get_int_def tgv "ANREV" annee_calc in
  if annee_revenu <> annee_calc then
    Printf.printf "Attention, année calculette (%d) <> année revenu (%d)\n"
      annee_calc annee_revenu;

  let annee_courante = 1900 + (Unix.localtime (Unix.time ())).tm_year in

  let tgv = TGV.set_int tgv "IND_TRAIT" 4 (* = primitif *) in
  let tgv = TGV.set_int tgv "ANCSDED" annee_courante in

  Printf.printf "Lancement du calcul\n%!";
  let tgv = M.traite_double_liquidation_2 tgv M.Primitif in

  Printf.printf "Vérification du résultat\n";
  check_result tgv [] res_prim []

let () =
  Printexc.record_backtrace true;
  try
    let res = main () in
    exit res
  with e ->
    Printf.printf "%s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout;
    exit 4
