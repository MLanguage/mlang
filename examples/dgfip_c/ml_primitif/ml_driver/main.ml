module StrSet = Set.Make (String)
module StrMap = Map.Make (String)

let pp fmt = Format.printf fmt
let ppe fmt = Format.eprintf fmt

type mode = Primitif | Correctif

type options = {
  files: string list;
  recursive: bool;
  year: int;
  mode : mode;
}

let read_test tgv filename =
  let test = Read_test.read_test filename in
  List.fold_left
    (fun (nom, est_corr, res_prim, ctl_prim, rappels, res_corr, ctl_rapp) s ->
      match s with
      | `Nom noms ->
           let nom = String.concat " " noms in
           nom, est_corr, res_prim, ctl_prim, rappels, res_corr, ctl_rapp
      | `EntreesPrimitif pl ->
          List.iter
            (fun (code, montant) ->
              M.set_var_opt tgv (M.cherche_var tgv code) (Some montant))
            pl;
          nom, est_corr, res_prim, ctl_prim, rappels, res_corr, ctl_rapp
      | `ControlesPrimitif el ->
          let ctl_prim =
            let fold err e = StrSet.add e err in
            List.fold_left fold ctl_prim el
          in
          nom, est_corr, res_prim, ctl_prim, rappels, res_corr, ctl_rapp
      | `ResultatsPrimitif pl ->
          let res_prim =
            List.fold_left
              (fun resultats (code, montant) ->
                StrMap.add code montant resultats
              )
              res_prim
              pl
          in
          nom, est_corr, res_prim, ctl_prim, rappels, res_corr, ctl_rapp
      | `EntreesRappels rappels ->
          nom, true, res_prim, ctl_prim, rappels, res_corr, ctl_rapp
      | `ControlesRappels el ->
          let ctl_rapp =
            let fold err e = StrSet.add e err in
            List.fold_left fold ctl_rapp el
          in
          nom, true, res_prim, ctl_prim, rappels, res_corr, ctl_rapp
      | `ResultatsRappels pl ->
          let res_corr =
            List.fold_left (fun resultats (code, montant) ->
                StrMap.add code montant resultats
              ) res_corr pl
          in
          nom, est_corr, res_prim, ctl_prim, rappels, res_corr, ctl_rapp
      | _ -> nom, true, res_prim, ctl_prim, rappels, res_corr, ctl_rapp
    )
    (
      "defaut",
      false,
      StrMap.empty,
      StrSet.empty,
      [],
      StrMap.empty,
      StrSet.empty
    )
    test

let check_result tgv err expected_tgv expected_err =
  let missing_errs = StrSet.diff expected_err err in
  let unexpected_errs = StrSet.diff err expected_err in
  let res = StrSet.is_empty missing_errs && StrSet.is_empty unexpected_errs in
  if not res then (
    StrSet.iter (pp "KO | %s attendue non recue@.") missing_errs;
    StrSet.iter (pp "KO | %s recu en trop@.") unexpected_errs;
  );
  StrMap.fold
    (fun code montant res ->
      match code with
      | "NBPT" -> res
      | _ ->
          let montant' =
            match M.cherche_var_opt tgv code with
            | Some var -> Option.value ~default:0.0 (M.get_var_opt tgv var)
            | None -> 0.0
          in
          let err =
            let m = Float.round (montant *. 100.) in
            let m' = Float.round (montant' *. 100.) in
            abs_float (m -. m') > 0.0
          in
          if err then (
            pp "KO | %s = %f au lieu de %f@." code montant' montant
          );
          res && not err
    )
    expected_tgv
    res

let list_vers_set l =
  let add res e = StrSet.add e res in
  List.fold_left add StrSet.empty l

let run_test_instance options test_file =
  let tgv = M.tgv_alloc () in
  let _, _, res_prim, ctl_prim, rappels, res_corr, ctl_rapp =
    read_test tgv test_file
  in
  M.set_evt_list tgv rappels;
  let annee_calc = M.annee_calc () in
  let annee_revenu =
    match M.get tgv "ANREV" with
    | Some anrev -> int_of_float anrev
    | None -> annee_calc
  in
  if annee_revenu <> annee_calc then (
    pp "DLDC02 | année calculette (%d) <> année revenu (%d)@."
      annee_calc
      annee_revenu
  );
  M.set tgv "ANCSDED" (float options.year);
  let resultat =
    match options.mode with
    | Primitif ->
        M.enchainement_primitif_interpreteur tgv;
        let err_set = list_vers_set (M.get_erreurs tgv) in
        check_result tgv err_set res_prim ctl_prim
    | Correctif ->
        (* Note: AIF fait le calcul correctif même si le primitif a échoué *)
        (* Note: techniquement il faut aussi qu'une section rappels soit
           présente, même si elle est vide *)
        M.set tgv "MODE_CORR" 1.0;
        M.enchaineur_primitif tgv;
        M.enchaineur_correctif tgv;
        let err_set = list_vers_set (M.get_erreurs tgv) in
        check_result tgv err_set res_corr ctl_rapp
  in
  Gc.minor (); (* évite une erreur de segmentation mystérieuse *)
  resultat

let run_tests options =
  let rec loop dir ok ko = function
  | [] -> ok, ko
  | test_file :: test_files ->
      let fname = Filename.basename test_file in
      let fdir = Filename.dirname test_file in
      if Sys.is_directory test_file then (
        if options.recursive then (
          let test_files' =
            let content =
              Sys.readdir test_file
              |> Array.to_list
              |> List.map (Filename.concat test_file)
            in
            content @ test_files
          in
          loop dir ok ko test_files'
        ) else (
          let dir =
            if dir <> fdir then (
              pp "ILDC01 | repertoire \"%s\"@." fdir;
              fdir
            ) else dir
          in
          pp "ILDC02 | repertoire \"%s\" ignore@." fname;
          loop dir ok ko test_files
        )
      ) else (
        let dir =
          if dir <> fdir then (
            pp "ILDC01 | repertoire \"%s\"@." fdir;
            fdir
          ) else dir
        in
        if Read_test.is_empty test_file then (
            pp "DLDC01 | le fichier \"%s\" est vide@." fname;
            loop dir ok ko test_files
        ) else (
          pp "ILDC03 | teste \"%s\"...@." fname;
          if run_test_instance options test_file then (
            pp "ILDC04 | teste \"%s\" OK@." fname;
            loop dir (ok + 1) ko test_files
          ) else (
            pp "ILDC05 | teste \"%s\" KO@." fname;
            loop dir ok (ko + 1) test_files
          )
        )
      )
  in
  loop "" 0 0 options.files

let aide oc =
  let pr fmt = Format.fprintf oc fmt in
  pr "@[<h 2>Syntaxe :@\n";
  pr "%s@\n" Sys.argv.(0);
  pr "-mode [primitif|correctif] (-m [p|c])@\n";
  pr "-annee [annee] (-a [annee])@\n";
  pr "-recursif (-r)@\n";
  pr "-- [fichiers IRJ] ([fichiers IRJ ne commencant pas par '-'])@]@."

let lis_args () =
  let rec loop fin acc = function
    | [] -> {acc with files = List.rev acc.files}
    (* Année *)
    | ("-annee" | "-a") :: ann :: tl -> (
        match int_of_string ann with
        | year -> loop fin {acc with year} tl
        | exception (Failure _) -> 
            ppe "ALDC01 | argument incorrect pour -annee (%s)@." ann;
            exit 1
    )
    | ["-annee" | "-a"] -> 
        ppe "ALDC02 | argument manquant pour -annee@.";
        exit 1
    (* Recursive *)
    | ("-recursif" | "-r") :: tl -> loop fin {acc with recursive = true} tl
    (* Mode *)
    | ("-mode" | "-m") :: mode :: tl -> (
        match mode with
        | "primitif" | "p" -> loop fin {acc with mode = Primitif} tl
        | "correctif" | "c" -> loop fin {acc with mode = Correctif} tl
        | m -> 
            ppe "ALDC03 | argument incorrect pour -mode (%s)@." m;
            exit 1
    )
    | ["-mode" | "-m"] -> 
         ppe "ALDC04 | argument manquant pour -mode@.";
         exit 1
    | ("-aide" | "-?") :: _ ->
        aide Format.std_formatter;
        exit 0
    | "--" :: tl -> loop true acc tl
    | file :: tl ->
        if not fin && String.length file > 0 && file.[0] = '-' then (
          ppe "ALDC05 | argument incorrect (%s)@." file;
          aide Format.err_formatter;
          exit 1
        ) else (
          loop fin {acc with files = file :: acc.files} tl
        )
  in
  let options = {
    files = [];
    recursive = false;
    year = M.annee_calc () + 1;
    mode = Primitif;
  } in
  loop false options (List.tl (Array.to_list Sys.argv))

let main () =
  let options = lis_args () in 
  let ok, ko = run_tests options in
  pp "ILDC06 | bilan: %d/%d tests reussis@." ok (ok + ko);
  if ko = 0 then 0 else 1

let () =
  Printexc.record_backtrace true;
  try
    let res = main () in
    exit res
  with e ->
    ppe "%s@." (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 1

