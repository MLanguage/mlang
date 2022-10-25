
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

  TGV.set_int tgv "IND_TRAIT" 4 (* = primitif *);
  TGV.set_int tgv "ANCSDED" annee_courante;

  M.traite_double_liquidation_2 tgv M.Primitif;

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
