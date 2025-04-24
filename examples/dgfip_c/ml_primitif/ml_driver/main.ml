open Common

type instance = {
  nom : string;
  label : string;
  vars : float StrMap.t;
  events : rappel list;
  expectedVars : float StrMap.t;
  expectedAnos : StrSet.t;
}

let new_instance nom = {
  nom;
  label = "";
  vars = StrMap.empty;
  events = [];
  expectedVars = StrMap.empty;
  expectedAnos = StrSet.empty;
}

let read_test filename =
  let test = Read_test.read_test filename in
  let fold_prim (nom, inst, insts) s =
    match s with
    | `Nom noms ->
         let nom = String.concat " " noms in
         let inst = {inst with nom} in
         let insts = List.map (fun i -> {i with nom}) insts in
         (nom, inst, insts)
    | `EntreesPrimitif pl ->
        let vars =
          let fold res (code, montant) = StrMap.add code montant res in
          List.fold_left fold StrMap.empty pl
        in
        (nom, {inst with vars}, insts)
    | `ControlesPrimitif el ->
        let expectedAnos =
          let fold err e = StrSet.add e err in
          List.fold_left fold StrSet.empty el
        in
        (nom, {inst with expectedAnos}, insts)
    | `ResultatsPrimitif pl ->
        let expectedVars =
          let fold res (code, montant) = StrMap.add code montant res in
          List.fold_left fold StrMap.empty pl
        in
        let inst = {inst with label = "primitif"; expectedVars} in
        (nom, new_instance nom, inst :: insts)
    | `EntreesCorrectif _
    | `ControlesCorrectif _
    | `ResultatsCorrectif _ -> (nom, inst, insts)
    | `EntreesRappels events -> (nom, {inst with events}, insts)
    | `ControlesRappels el ->
        let expectedAnos =
          let fold err e = StrSet.add e err in
          List.fold_left fold StrSet.empty el
        in
        (nom, {inst with expectedAnos}, insts)
    | `ResultatsRappels pl ->
        let expectedVars =
          let fold res (code, montant) = StrMap.add code montant res in
          List.fold_left fold StrMap.empty pl
        in
        let inst = {inst with label = "correctif"; expectedVars} in
        (nom, new_instance nom, inst :: insts)
    | `Skip -> (nom, inst, insts)
  in
  let _, _, insts = List.fold_left fold_prim ("", new_instance "", []) test in
  insts

let check_result tgv err expected_tgv expected_err =
  let result = ref true in
  StrMap.iter (fun code montant ->
      (* Consider missing variables as 0.0 and dismiss NBPT (DGFiP does the same) *)
      (* NBPT variable doesn't bear any meaning due to DGFiP test generation method *)
      match code with
      | "NBPT" -> ()
      | _ ->
          let montant' = try M.TGV.get_def tgv code 0.0 with Not_found -> 0.0 in
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

let run_test test_file annee_exec =
  Printf.printf "Testing %s...\n%!" test_file;
  let annee_calc = M.annee_calc () in
  let insts = read_test test_file in
  let rec run_insts res = function
  | [] -> res
  | inst :: insts ->
      Printf.printf "  Running %s:%s...\n%!" inst.nom inst.label;
      let tgv = M.TGV.alloc_tgv () in
      StrMap.iter (M.TGV.set tgv) inst.vars;
      M.set_evt_list tgv inst.events;
      let annee_revenu = M.TGV.get_int_def tgv "ANREV" annee_calc in
      if annee_revenu <> annee_calc then (
        Printf.eprintf
          "Attention, année calculette (%d) <> année revenu (%d)\n%!"
          annee_calc
          annee_revenu
      );
      (match inst.label with
      | "primitif" -> M.TGV.set_int tgv "IND_TRAIT" 4
      | "correctif" -> M.TGV.set_int tgv "IND_TRAIT" 5
      | _ -> M.TGV.set_int tgv "IND_TRAIT" 0);
      M.TGV.set_int tgv "ANCSDED" annee_exec;
      M.init_errs tgv;
      let _err = M.enchainement_primitif tgv in
      M.export_errs tgv;
      let err_set =
        let add res e = StrSet.add e res in
        List.fold_left add StrSet.empty (M.get_err_list tgv)
      in
      res && check_result tgv err_set inst.expectedVars inst.expectedAnos
  in
  run_insts true insts

let main () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Syntaxe :\n%s fichier_test\n" Sys.argv.(0);
    exit 31
  );
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
  | [] -> true
  | test_file :: files ->
      run_test test_file annee_exec && (Gc.minor (); loop files)
  in
  match loop test_files with
  | true -> exit 0
  | false -> exit 1

let _ =
  Printexc.record_backtrace true;
  try main ()
  with e ->
    Printf.eprintf "%s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 30

