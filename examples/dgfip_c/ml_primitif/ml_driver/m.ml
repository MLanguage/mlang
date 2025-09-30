type tgv
type var

external tgv_alloc : unit -> tgv = "ml_tgv_alloc"
external cherche_var_opt : tgv -> string -> var option = "ml_cherche_var_opt"
external nom_var : var -> string = "ml_nom_var"
external alias_var : var -> string option = "ml_alias_var"
external get_var_opt : tgv -> var -> float option = "ml_get_var_opt"
external set_var_opt : tgv -> var -> float option -> unit = "ml_set_var_opt"
external init_erreurs : tgv -> unit = "ml_init_erreurs"
external get_discords : tgv -> string list = "ml_get_discords"
external exporte_erreurs : tgv -> unit = "ml_exporte_erreurs"
external get_erreurs : tgv -> string list = "ml_get_erreurs"
external set_evt_list :
  tgv
  -> (float * float * string * float * float * float * float * float * float) list
  -> unit
= "ml_set_evt_list"

let cherche_var tgv code =
  match cherche_var_opt tgv code with
  | Some var -> var
  | None ->
      Printf.eprintf "variable non trouvee: %s\n" code;
      raise Not_found

let alias_var_str var =
  match alias_var var with
  | None -> nom_var var
  | Some a -> a

let get tgv name = get_var_opt tgv (cherche_var tgv name)

let get0 tgv name value = 
  Option.value ~default:0.0 (get_var_opt tgv (cherche_var tgv name))

let set tgv name value = set_var_opt tgv (cherche_var tgv name) (Some value)

external set_evt_list :
  tgv
  -> (float * float * string * float * float * float * float * float * float) list
  -> unit
= "ml_set_evt_list"

external annee_calc : unit -> int = "ml_annee_calc"
external enchainement_primitif_interpreteur : tgv -> unit = "ml_enchainement_primitif_interpreteur"
external enchaineur_primitif : tgv -> unit = "ml_enchaineur_primitif"
external enchaineur_correctif : tgv -> unit = "ml_enchaineur_correctif"

