
let ( => ) x l = List.mem x l

let ( =: ) x (l, u) = x >= l && x <= u

module StrMap = Map.Make (String)

module IntMap = Map.Make (Int)

module IndexedVarMap = struct

  type value =
    | Single of float
    | Indexed of float IntMap.t

  type t = value StrMap.t

  let empty =
    StrMap.empty

  let mem x i m =
    match StrMap.find_opt x m, i with
    | Some (Single _), None -> true
    | Some (Indexed _), None -> true
    | Some (Indexed m'), Some i' -> IntMap.mem i' m'
    | _ -> false

  let find_opt x i m =
    match StrMap.find_opt x m, i with
    | Some (Single v), None -> Some (v)
    | Some (Indexed _), None -> None
    | Some (Indexed m'), Some i' -> IntMap.find_opt i' m'
    | _ -> None

  let add x i v m =
    match StrMap.find_opt x m, i with
    | Some (Single _), None ->
        StrMap.add x (Single v) m
    | Some (Indexed _), None ->
        failwith "IndexedVarMap.add single overwrites indexed"
    | Some (Indexed m'), Some i ->
        StrMap.add x (Indexed (IntMap.add i v m')) m
    | Some (Single _), Some _ ->
        failwith "IndexedVarMap.add indexed overwrites single"
    | None, None ->
        StrMap.add x (Single v) m
    | None, Some i ->
        StrMap.add x (Indexed (IntMap.singleton i v)) m

  let remove x i m =
    match StrMap.find_opt x m, i with
    | Some (Single _), None ->
        StrMap.remove x m
    | Some (Indexed _), None ->
        failwith "IndexedVarMap.remove single overwrites indexed"
    | Some (Indexed m'), Some i ->
        let m' = IntMap.remove i m' in
        if IntMap.cardinal m' = 0 then StrMap.remove x m
        else StrMap.add x (Indexed (m')) m
    | Some (Single _), Some _ ->
        failwith "IndexedVarMap.remove indexed overwrites single"
    | None, _ ->
        m

  let iter f m =
    StrMap.iter (fun x v ->
        match v with
        | Single v' -> f x None v'
        | Indexed m' -> IntMap.iter (fun i v' -> f x (Some i) v') m'
      ) m

  let map f m =
    StrMap.map (fun x v ->
        match v with
        | Single v' -> Single (f x v')
        | Indexed m' -> Indexed (IntMap.map (fun v' -> f x v') m')
      ) m

  let mapi f m =
    StrMap.map (fun x v ->
        match v with
        | Single v' -> Single (f x None v')
        | Indexed m' -> Indexed (IntMap.mapi (fun i v' -> f x (Some i) v') m')
      ) m

  let fold f m acc =
    StrMap.fold (fun x v acc ->
        match v with
        | Single v' -> f x None v' acc
        | Indexed m' -> IntMap.fold (fun i v' acc -> f x (Some i) v' acc) m' acc
      ) m acc

  let filter f m =
    StrMap.fold (fun x v acc ->
        match v with
        | Single v' ->
            if f x None v' then StrMap.add x (Single v') acc else acc
        | Indexed m' ->
            let m' = IntMap.filter (fun i v' -> f x (Some i) v') m' in
            if IntMap.cardinal m' <> 0 then StrMap.add x (Indexed m') acc else acc
      ) m StrMap.empty

end



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

  external charge_vars :
    unit -> (string * string option * int * int * int * int * int * int *
             int * bool * int * int * int * bool * int * bool) list
    = "ml_charge_vars"

  let vars =
    List.fold_left (fun vars (code, alias, genre, domaine, type_, nature,
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
        let vars = StrMap.add code var vars in
        match alias with
        | None -> vars
        | Some alias -> StrMap.add alias var vars
      ) StrMap.empty (charge_vars ())

  let is_alias code =
    try
      let var = StrMap.find code vars in
      match var.Var.alias with
      | None -> false
      | Some alias -> code = alias
    with e ->
      Printf.printf "Variable non trouvee: %s\n" code;
      raise e

  let alias code =
    try
      let var = StrMap.find code vars in
      match var.Var.alias with
      | None -> var.code
      | Some alias -> alias
    with e ->
      Printf.printf "Variable non trouvee: %s\n" code;
      raise e

  let unalias code =
    try
      let var = StrMap.find code vars in
      var.Var.code
    with e ->
      Printf.printf "Variable non trouvee: %s\n" code;
      raise e

  let mem code =
    StrMap.mem code vars

  let find code =
    try
      StrMap.find code vars
    with e ->
      Printf.printf "Variable non trouvee: %s\n" code;
      raise e

  let exists pred =
    StrMap.exists pred vars

  let filter pred =
    StrMap.filter pred vars

  let fold pred acc =
    StrMap.fold pred vars acc

end



module TGV = struct

  type t = IndexedVarMap.t

  let empty =
    IndexedVarMap.empty

  let defined tgv var =
    IndexedVarMap.mem (VarDict.unalias var) None tgv

  let reset tgv var =
    IndexedVarMap.remove (VarDict.unalias var) None tgv

  let reset_list tgv var_list =
    List.fold_left (fun tgv var ->
        IndexedVarMap.remove (VarDict.unalias var) None tgv) tgv var_list

  let get_opt tgv var =
    IndexedVarMap.find_opt (VarDict.unalias var) None tgv

  let get_bool_opt tgv var =
    match get_opt tgv var with
    | None -> None
    | Some v -> Some (v <> 0.0)

  let get_int_opt tgv var =
    match get_opt tgv var with
    | None -> None
    | Some v -> Some (int_of_float v)

  let get_def tgv var def =
    match get_opt tgv var with
    | None -> def
    | Some v -> v

  let get_bool_def tgv var def =
    match get_opt tgv var with
    | None -> def
    | Some v -> v <> 0.0

  let get_int_def tgv var def =
    match get_opt tgv var with
    | None -> def
    | Some v -> int_of_float v

  let get_map_opt tgv var_list =
    List.fold_left (fun map var ->
        match get_opt tgv var with
        | None -> map
        | Some v -> StrMap.add (VarDict.unalias var) v map
      ) StrMap.empty var_list

  let get_map_def tgv var_list def =
    List.fold_left (fun map var ->
        StrMap.add (VarDict.unalias var) (get_def tgv var def) map
      ) StrMap.empty var_list

  let get_array_opt tgv var idx =
    IndexedVarMap.find_opt (VarDict.unalias var) (Some idx) tgv

  let get_array_def tgv var idx def =
    match get_array_opt tgv var idx with
    | None -> def
    | Some v -> v

  let set tgv var v =
    IndexedVarMap.add (VarDict.unalias var) None v tgv

  let set_bool tgv var v =
    set tgv var (if v then 1.0 else 0.0)

  let set_int tgv var v =
    set tgv var (float_of_int v)

  let set_list tgv var_v_list =
    List.fold_left (fun tgv (var, v) -> set tgv var v) tgv var_v_list

  let set_bool_list tgv var_v_list =
    List.fold_left (fun tgv (var, v) -> set_bool tgv var v) tgv var_v_list

  let set_int_list tgv var_v_list =
    List.fold_left (fun tgv (var, v) -> set_int tgv var v) tgv var_v_list

  let set_map ?(ignore_negative=false) tgv var_v_map =
    StrMap.fold (fun var montant tgv ->
        if ignore_negative && montant < 0.0 then tgv
        else set tgv (VarDict.unalias var) montant
      ) var_v_map tgv

  let set_array tgv var idx v =
    IndexedVarMap.add (VarDict.unalias var) (Some idx) v tgv

  let update tgv var v_opt =
    match v_opt with
    | None -> reset tgv var
    | Some v -> set tgv var v

  let update_bool tgv var v_opt =
    match v_opt with
    | None -> reset tgv var
    | Some v -> set_bool tgv var v

  let update_int tgv var v_opt =
    match v_opt with
    | None ->  reset tgv var
    | Some v -> set_int tgv var v

  let copy_abs tgv svar dvar signvar =
    match get_opt tgv svar with
    | None -> tgv
    | Some v ->
        let tgv = set_bool tgv signvar (v < 0.0) in
        set tgv dvar (Float.abs v)

  let internal_copy ~ignore_undefined tgv var_list =
    List.fold_left (fun tgv (svar, dvar) ->
        match get_opt tgv svar with
        | None when ignore_undefined -> tgv
        | None -> reset tgv dvar
        | Some v -> set tgv dvar v
      ) tgv var_list

  let copy ~ignore_undefined stgv dtgv var_list =
    List.fold_left (fun dtgv (svar, dvar) ->
        match IndexedVarMap.find_opt (VarDict.unalias svar) None stgv with
        | None when ignore_undefined -> dtgv
        | None -> IndexedVarMap.remove (VarDict.unalias dvar) None dtgv
        | Some v -> IndexedVarMap.add (VarDict.unalias dvar) None v dtgv
      ) dtgv var_list

  let reset_matching ~except f tgv =
    let keep_vars =
      VarDict.filter (fun code var ->
          if code <> var.code then false
          else if List.exists (fun code ->
              VarDict.unalias code = var.code
            ) except then true
          else f var
        ) in
    IndexedVarMap.filter (fun code _id_opt _montant ->
        StrMap.mem code keep_vars) tgv

  let reset_saisie_calc ~except tgv =
    reset_matching ~except (fun var ->
        match var.Var.genre with
        | Saisie | Calculee -> false
        | Base -> true) tgv

  let reset_calculee tgv =
    reset_matching ~except:[] (fun var ->
        match var.Var.genre with
        | Calculee -> false
        | Saisie | Base -> true) tgv

  let reset_base tgv =
    reset_matching ~except:[] (fun var ->
        match var.Var.genre with
        | Base -> false
        | Saisie | Calculee -> true) tgv

  let iter f tgv =
    IndexedVarMap.iter f tgv

  let map f tgv =
    IndexedVarMap.map f tgv

  let fold f tgv acc =
    IndexedVarMap.fold f tgv acc

end
