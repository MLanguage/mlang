
type file = {
  c: in_channel;
  mutable lines: string list;
}

let open_file filename =
  let c = open_in filename in
  { c; lines = [] }

let close_file file =
  close_in file.c

let read_line file =
  match file.lines with
  | line :: lines -> file.lines <- lines; line
  | [] -> String.trim (input_line file.c) (* some test have a trailing \r *)

let put_back_line file line =
  file.lines <- line :: file.lines

let convert_int s =
  try int_of_string s
  with _ -> 0

let convert_float s =
  try Float.of_string s
  (* with _ -> 0.0 *)
  with _ -> (* to cope with badly formatted tests *)
    try Float.of_string
          (String.sub s 0
             (String.index_from s
                ((String.index s '.') + 1) '.'))
    with _ -> 0.0

let parse_generic s =
  let sl = String.split_on_char '/' s in
  match sl with
  | [ code; montant ] -> (code, convert_float montant)
  | [ code ] -> (code, 0.0) (* to cope with badly formatted tests *)
  | _ -> failwith (Printf.sprintf "Ligne generique invalide: '%s'" s)

let parse_controle s =
  let sl = String.split_on_char '/' s in
  match sl with
  | [ err ] -> (String.trim err)
  | _ -> failwith (Printf.sprintf "Ligne controle invalide: '%s'" s)

let parse_entree_corr s =
  let sl = String.split_on_char '/' s in
  match sl with
  | [ code; montant; code_2042; sens; montant_rappel;
      date_evt; penalite; base_tl; _cle_oc1; _cle_oc2 ] ->
      let date_evt = convert_int date_evt in
      (convert_int code, convert_float montant, convert_int code_2042,
       sens.[0], convert_float montant_rappel,
       (date_evt mod 10000, date_evt / 10000),
       convert_int penalite, convert_float base_tl)
  | _ -> failwith (Printf.sprintf "Ligne entree correctif invalide: '%s'" s)

let parse_entree_rap s =
  let sl = String.split_on_char '/' s in
  match sl with
  | [ num_evt; num_rappel; code; montant; sens;
      penalite; base_tl; date_evt; ind20 ] ->
      let date_evt = convert_int date_evt in
      (convert_int num_evt, convert_int num_rappel,
       code, convert_float montant, sens.[0],
       convert_int penalite, convert_float base_tl,
       (date_evt mod 10000, date_evt / 10000), String.equal ind20 "1") (* TODO: improve *)
  | _ -> failwith (Printf.sprintf "Ligne entree rappel invalide: '%s'" s)

let read_section_contents f parsefun =
  let rec aux contents =
    let s = read_line f in
    if String.length s < 1 then
      aux contents
    (* else if s.[0] = '#' then *)
    else if s.[0] = '#' ||
            s = "CONTROLES-PRIMITIF" then (* to cope with badly formatted tests *)
      (put_back_line f s; List.rev contents)
    else
      aux (parsefun s :: contents)
  in
  aux []

let read_file f =
  let rec aux sections =
    let s = read_line f in
    if String.length s < 1 then
      aux sections
    else if String.equal s "##" then
      List.rev sections
    else
      let s =
        match s with
        | "#NOM" -> `Nom (read_section_contents f (fun x -> x))
        | "#ENTREES-PRIMITIF" ->
            `EntreesPrimitif (read_section_contents f parse_generic)
        (* | "#CONTROLES-PRIMITIF" -> *)
        | "#CONTROLES-PRIMITIF"
        | "CONTROLES-PRIMITIF" -> (* to cope with badly formatted tests *)
            `ControlesPrimitif (read_section_contents f parse_controle)
        | "#RESULTATS-PRIMITIF" ->
            `ResultatsPrimitif (read_section_contents f parse_generic)
        | "#ENTREES-CORRECTIF" ->
            `EntreesCorrectif (read_section_contents f parse_entree_corr)
        | "#CONTROLES-CORRECTIF" ->
            `ControlesCorrectif (read_section_contents f parse_controle)
        | "#RESULTATS-CORRECTIF" ->
            `ResultatsCorrectif (read_section_contents f parse_generic)
        | "#ENTREES-RAPPELS" ->
            `EntreesRappels (read_section_contents f parse_entree_rap)
        | "#CONTROLES-RAPPELS" ->
            `ControlesRappels (read_section_contents f parse_controle)
        | "#RESULTATS-RAPPELS" ->
            `ResultatsRappels (read_section_contents f parse_generic)
        | _ when s.[0] = '#' ->
            `Skip
        | _ ->
            failwith (Printf.sprintf "Ligne invalide: '%s'" s)
      in
      aux (s :: sections)
  in
  aux []

let read_test filename =
  try
    let f = open_file filename in
    let sections = read_file f in
    close_file f;
    sections
  with
  | Failure s -> failwith (Printf.sprintf "%s: %s" filename s)
  | e -> raise e
