open Common

module TGV = struct
  type t

  external alloc_tgv : unit -> t = "ml_tgv_alloc"
  external unalias : t -> string -> string = "ml_unalias"
  external udefined : t -> string -> bool = "ml_tgv_defined"
  external ureset : t -> string -> unit = "ml_tgv_reset"
  external uget : t -> string -> float option = "ml_tgv_get"
  external uget_array : t -> string -> int -> float option = "ml_tgv_get_array"
  external uset : t -> string -> float -> unit = "ml_tgv_set"

  let defined tgv var = udefined tgv var

  let reset tgv var = ureset tgv var

  let reset_list (tgv : t) var_list =
    List.iter (fun var -> reset tgv var) var_list

  let get_opt (tgv : t) var = uget tgv var

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
        | Some v -> StrMap.add (unalias tgv var) v map)
      StrMap.empty var_list

  let get_map_def (tgv : t) var_list def =
    List.fold_left (fun map var ->
        StrMap.add (unalias tgv var) (get_def tgv var def) map)
      StrMap.empty var_list

  let get_array_opt (tgv : t) var idx = uget_array tgv var idx

  let get_array_def (tgv : t) var idx def =
    match get_array_opt tgv var idx with
    | None -> def
    | Some v -> v

  let set ?(ignore_negative=false) (tgv : t) var v =
    if ignore_negative && v < 0.0 then ()
    else uset tgv var v

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

  type undef_action = UDIgnore | UDZero | UDCopy

  let internal_copy ~undef (tgv : t) var_list =
    List.iter (fun (svar, dvar) ->
        match get_opt tgv svar with
        | Some v -> set tgv dvar v
        | None -> (
            match undef with
            | UDIgnore -> ()
            | UDZero -> set tgv dvar 0.0
            | UDCopy -> reset tgv dvar
          )
      ) var_list

  let copy_list ~undef (stgv : t) (dtgv : t) var_list =
    List.iter (fun (svar, dvar) ->
        match get_opt stgv svar with
        | Some v -> set dtgv dvar v
        | None -> (
            match undef with
            | UDIgnore -> ()
            | UDZero -> set dtgv dvar 0.0
            | UDCopy -> reset dtgv dvar
          )
      ) var_list
end

external init_errs : TGV.t -> unit = "ml_init_errs"
external get_err_list : TGV.t -> string list = "ml_get_err_list"
external annee_calc : unit -> int = "ml_annee_calc"
external export_errs : TGV.t -> unit = "ml_export_errs"
external enchainement_primitif : TGV.t -> unit = "ml_enchainement_primitif"
external set_evt_list :
  TGV.t
  -> (
      float
      * float
      * string
      * float
      * float
      * float option
      * float option
      * float
      * float option
    ) list
  -> unit
= "ml_set_evt_list" 

