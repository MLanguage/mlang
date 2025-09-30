
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

let convert_float s =
  try Float.of_string s
  (* with _ -> 0.0 *)
  with _ -> (* to cope with badly formatted tests *)
    try Float.of_string
          (String.sub s 0
             (String.index_from s
                ((String.index s '.') + 1) '.'))
    with _ -> 0.0

let convert_sens s =
  if String.length s = 0 then (
    failwith "Sens invalide"
  ) else (
    match s.[0] with
   | 'R' | 'r' -> 0.0
   | 'M' | 'm' -> 1.0
   | 'P' | 'p' -> 2.0
   | 'C' | 'c' -> 3.0
   | _ -> failwith "Sens invalide"
  )

let parse_generic s =
  let sl = String.split_on_char '/' s in
  match sl with
  | [ code; montant ] -> (code, convert_float montant)
  | [ code ] -> (code, 0.0) (* to cope with badly formatted tests *)
  | _ -> failwith (Printf.sprintf "Ligne generique invalide: '%s'" s)

let parse_controle s =
  let sl = String.split_on_char '/' s in
  match sl with
  | [ err ] -> (err)
  | _ -> failwith (Printf.sprintf "Ligne controle invalide: '%s'" s)

let parse_entree_corr s =
  let sl = String.split_on_char '/' s in
  match sl with
  | [
      code; montant; code_2042; sens; montant_rappel;
      date; penalite; base_tl; _cle_oc1; _cle_oc2
    ] -> (
      convert_float code, convert_float montant, convert_float code_2042,
      convert_sens sens, convert_float montant_rappel, convert_float date,
      convert_float penalite, convert_float base_tl
    )
  | _ -> failwith (Printf.sprintf "Ligne entree correctif invalide: '%s'" s)

let parse_entree_rap s =
  let sl = String.split_on_char '/' s in
  match sl with
  | [
      numero; rappel; code; montant; sens;
      penalite; base_tl; date; _2042_rect
    ] -> (
      convert_float numero, convert_float rappel, code, convert_float montant,
      convert_sens sens, convert_float penalite, convert_float base_tl,
      convert_float date, convert_float _2042_rect
    )
  | _ -> failwith (Printf.sprintf "Ligne entree rappel invalide: '%s'" s)

let read_section_contents f parsefun =
  let rec aux contents =
    let s = read_line f in
    if String.length s < 1 then (
      aux contents
    ) else if
      s.[0] = '#'
      || s = "CONTROLES-PRIMITIF" (* to cope with badly formatted tests *)
    then (
      put_back_line f s; List.rev contents
    ) else (
      aux (parsefun s :: contents)
    )
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

let is_empty filename =
  let c = open_in filename in
  try
    let _ : string = input_line c in
    close_in c;
    false
  with
  | End_of_file -> close_in c; true

