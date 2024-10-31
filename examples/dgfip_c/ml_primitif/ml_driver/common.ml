
let ( => ) x l = List.mem x l

let ( =: ) x (l, u) = x >= l && x <= u

module StrSet = Set.Make(String)
module StrMap = Map.Make(String)

type nature = Indefinie | Revenu | Charge

type genre = Saisie | Calculee | Base

type type_ = Reel | Booleen | Date

type domaine = Indefini | Contexte | Famille | Revenu |
               RevenuCorr | Variation | Penalite

module Var = struct

  type t = {
    code: string;
    alias: string option;
    genre: genre;
    domaine: domaine;
    type_: type_;
    nature: nature;
    classe: int;
    cat_tl: int;
    cot_soc: int;
    ind_abat: bool;
    rap_cat: int;
    sanction: int;
    acompte: bool;
    avfisc: int;
    restituee: bool;
  }

  let compare v1 v2 = String.compare v1.code v2.code

  let has_alias var =
    match var.alias with
    | None -> false
    | Some _ -> true

end

module VarDict = struct

  type t = (string, Var.t) Hashtbl.t

  external charge_vars :
    unit -> (string * string option * int * int * int * int * int * int *
             int * bool * int * int * int * bool * int * bool) list
    = "ml_charge_vars"

  let vars : t =
    let vars = Hashtbl.create 70000 in
    List.iter (fun (code, alias, genre, domaine, type_, nature,
                    classe, cat_tl, cot_soc, ind_abat, rap_cat,
                    sanction, indice_tab, acompte, avfisc,
                    restituee) ->
        let genre =
          match genre with
          | 1 -> Saisie
          | 2 -> Calculee
          | 3 -> Base
          | _ -> assert false
        in
        let domaine =
          match domaine with
          | -1 -> Indefini
          | 1 -> Contexte
          | 2 -> Famille
          | 3 -> Revenu
          | 4 -> RevenuCorr
          | 5 -> Variation
          | 6 -> Penalite
          | _ -> assert false
        in
        let type_ =
          match type_ with
          | 1 -> Reel
          | 2 -> Booleen
          | 3 -> Date
          | _ -> assert false
        in
        let nature =
          match nature with
          | -1 -> Indefinie
          | 1 -> Revenu
          | 2 -> Charge
          | _ -> assert false
        in
        let var = Var.{ code; alias; genre; domaine; type_; nature;
                        classe; cat_tl; cot_soc; ind_abat; rap_cat;
                        sanction; acompte; avfisc; restituee } in
        Hashtbl.add vars code var;
        match alias with
        | None -> ()
        | Some alias -> Hashtbl.add vars alias var
      ) (charge_vars ());
    vars

  let is_alias code =
    try
      let var = Hashtbl.find vars code in
      match var.Var.alias with
      | None -> false
      | Some alias -> code = alias
    with e ->
      Printf.printf "Variable non trouvee: %s\n" code;
      raise e

  let alias code =
    try
      let var = Hashtbl.find vars code in
      match var.Var.alias with
      | None -> var.code
      | Some alias -> alias
    with e ->
      Printf.printf "Variable non trouvee: %s\n" code;
      raise e

  let unalias code =
    try
      let var = Hashtbl.find vars code in
      var.Var.code
    with e ->
      Printf.printf "Variable non trouvee: %s\n" code;
      raise e

  let mem code =
    Hashtbl.mem vars code

  let find code =
    try
      Hashtbl.find vars code
    with e ->
      Printf.printf "Variable non trouvee: %s\n" code;
      raise e

  let filter pred =
    Hashtbl.fold
      (fun a b map -> if pred a b then StrMap.add a b map else map)
      vars StrMap.empty

  let fold f acc =
    Hashtbl.fold f vars acc

end

module TGV = struct

  type t

  external alloc_tgv : unit -> t = "ml_tgv_alloc"
  external udefined : t -> string -> bool = "ml_tgv_defined"
  external ureset : t -> string -> unit = "ml_tgv_reset"
  external uget : t -> string -> float option = "ml_tgv_get"
  external uget_array : t -> string -> int -> float option = "ml_tgv_get_array"
  external uset : t -> string -> float -> unit = "ml_tgv_set"
  external copy_all : t -> t -> unit = "ml_tgv_copy"

  let defined tgv var = udefined tgv (VarDict.unalias var)

  let reset tgv var = ureset tgv (VarDict.unalias var)

  let reset_list (tgv : t) var_list =
    List.iter (fun var -> reset tgv var) var_list

  let get_opt (tgv : t) var = uget tgv (VarDict.unalias var)

  let get_bool_opt (tgv : t) var =
    match get_opt tgv var with
    | None -> None
    | Some v -> Some (v <> 0.0)

  let get_int_opt (tgv : t) var =
    match get_opt tgv var with
    | None -> None
    | Some v -> Some (int_of_float v)

  let get_def (tgv : t) var def =
    match get_opt tgv var with
    | None -> def
    | Some v -> v

  let get_bool_def (tgv : t) var def =
    match get_opt tgv var with
    | None -> def
    | Some v -> v <> 0.0

  let get_int_def (tgv : t) var def =
    match get_opt tgv var with
    | None -> def
    | Some v -> int_of_float v

  let get_map_opt (tgv : t) var_list =
    List.fold_left (fun map var ->
        match get_opt tgv var with
        | None -> map
        | Some v -> StrMap.add (VarDict.unalias var) v map)
      StrMap.empty var_list

  let get_map_def (tgv : t) var_list def =
    List.fold_left (fun map var ->
        StrMap.add (VarDict.unalias var) (get_def tgv var def) map)
      StrMap.empty var_list

  let get_array_opt (tgv : t) var idx =
    uget_array tgv (VarDict.unalias var) idx

  let get_array_def (tgv : t) var idx def =
    match get_array_opt tgv var idx with
    | None -> def
    | Some v -> v

  let set ?(ignore_negative=false) (tgv : t) var v =
    if ignore_negative && v < 0.0 then ()
    else uset tgv (VarDict.unalias var) v

  let set_bool (tgv : t) var v =
    set tgv var (if v then 1.0 else 0.0)

  let set_int (tgv : t) var v =
    set tgv var (float_of_int v)

  let set_list0 set (tgv : t) var_v_list =
    List.iter (fun (var, v) -> set tgv var v) var_v_list

  let set_list (tgv : t) var_v_list =
    set_list0 set tgv var_v_list

  let set_bool_list (tgv : t) var_v_list =
    set_list0 set_bool tgv var_v_list

  let set_int_list (tgv : t) var_v_list =
    set_list0 set_int tgv var_v_list

  let set_map ?(ignore_negative=false) (tgv : t) var_v_map =
    StrMap.iter (fun var montant ->
        if ignore_negative && montant < 0.0 then ()
        else set tgv var montant
      ) var_v_map

  let update (tgv : t) var v_opt =
    match v_opt with
    | None -> reset tgv var
    | Some v -> set tgv var v

  let update_bool (tgv : t) var v_opt =
    match v_opt with
    | None -> reset tgv var
    | Some v -> set_bool tgv var v

  let update_int (tgv : t) var v_opt =
    match v_opt with
    | None ->  reset tgv var
    | Some v -> set_int tgv var v

  let copy_abs (tgv : t) svar dvar signvar =
    match get_opt tgv svar with
    | None -> ()
    | Some v ->
        set tgv svar (Float.abs v);
        set_bool tgv signvar (v < 0.0);
        set tgv dvar (Float.abs v)

  type undef_action =
    | UDIgnore
    | UDZero
    | UDCopy

  let internal_copy ~undef (tgv : t) var_list =
    List.iter (fun (svar, dvar) ->
        match get_opt tgv svar with
        | Some v -> set tgv dvar v
        | None ->
            match undef with
            | UDIgnore -> ()
            | UDZero -> set tgv dvar 0.0
            | UDCopy -> reset tgv dvar
      ) var_list

  let copy_list ~undef (stgv : t) (dtgv : t) var_list =
    List.iter (fun (svar, dvar) ->
        match get_opt stgv svar with
        | Some v -> set dtgv dvar v
        | None ->
            match undef with
            | UDIgnore -> ()
            | UDZero -> set dtgv dvar 0.0
            | UDCopy -> reset dtgv dvar
      ) var_list

end

external init_errs : TGV.t -> unit = "ml_init_errs"
external get_err_list : TGV.t -> string list = "ml_get_err_list"
(*external free_errs : TGV.t -> unit = "ml_free_errs"*)

let get_errs tgv =
  List.fold_left (fun res e -> StrSet.add e res) StrSet.empty (get_err_list tgv)

