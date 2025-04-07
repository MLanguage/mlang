module CatVar = struct
  type t = Input of StrSet.t | Computed of { is_base : bool }

  let pp fmt = function
    | Input id ->
        let pp fmt set = StrSet.iter (Format.fprintf fmt " %s") set in
        Format.fprintf fmt "saisie%a" pp id
    | Computed id ->
        Format.fprintf fmt "calculee%s" (if id.is_base then " base" else "")

  let compare a b =
    match (a, b) with
    | Input _, Computed _ -> 1
    | Computed _, Input _ -> -1
    | Input id0, Input id1 -> StrSet.compare id0 id1
    | Computed c0, Computed c1 -> compare c0.is_base c1.is_base

  type cat_var_t = t

  let cat_var_pp = pp

  let cat_var_compare = compare

  module Set = struct
    include SetExt.Make (struct
      type t = cat_var_t

      let compare = cat_var_compare
    end)

    let pp ?(sep = ", ") ?(pp_elt = cat_var_pp) (_ : unit)
        (fmt : Format.formatter) (set : t) : unit =
      pp ~sep ~pp_elt () fmt set
  end

  module Map = struct
    include MapExt.Make (struct
      type t = cat_var_t

      let compare = cat_var_compare
    end)

    let pp ?(sep = "; ") ?(pp_key = cat_var_pp) ?(assoc = " => ")
        (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
        (map : 'a t) : unit =
      pp ~sep ~pp_key ~assoc pp_val fmt map

    let from_string_list = function
      | [ ("*", _) ], id_pos ->
          one (Input (StrSet.one "*")) id_pos
          |> add (Computed { is_base = false }) id_pos
          |> add (Computed { is_base = true }) id_pos
      | [ ("saisie", _); ("*", _) ], id_pos ->
          one (Input (StrSet.one "*")) id_pos
      | ("saisie", _) :: id, id_pos ->
          one (Input (StrSet.from_marked_list id)) id_pos
      | ("calculee", _) :: id, id_pos -> (
          match id with
          | [] -> one (Computed { is_base = false }) id_pos
          | [ ("base", _) ] -> one (Computed { is_base = true }) id_pos
          | [ ("*", _) ] ->
              one (Computed { is_base = false }) id_pos
              |> add (Computed { is_base = true }) id_pos
          | _ -> Errors.raise_spanned_error "invalid variable category" id_pos)
      | _, id_pos ->
          Errors.raise_spanned_error "invalid variable category" id_pos
  end

  type loc = LocComputed | LocBase | LocInput

  type data = {
    id : t;
    id_str : string;
    id_int : int;
    loc : loc;
    pos : Pos.t;
    attributs : Pos.t StrMap.t;
  }
end

(** Here are all the types a value can have. Date types don't seem to be used at
    all though. *)
type value_typ =
  | Boolean
  | DateYear
  | DateDayMonthYear
  | DateMonth
  | Integer
  | Real

type loc_tgv = {
  loc_id : string;
  loc_cat : CatVar.loc;
  loc_idx : int;
  loc_cat_id : CatVar.t;
  loc_cat_str : string;
  loc_cat_idx : int;
  loc_int : int;
}

type loc =
  | LocTgv of string * loc_tgv
  | LocTmp of string * int
  | LocRef of string * int
  | LocArg of string * int
  | LocRes of string

module Var = struct
  type id = int

  let id_cpt = ref 0

  let new_id () =
    let id = !id_cpt in
    incr id_cpt;
    id

  type tgv = {
    is_table : int option;
    alias : string Pos.marked option;  (** Input variable have an alias *)
    descr : string Pos.marked;
        (** Description taken from the variable declaration *)
    attrs : int Pos.marked StrMap.t;
    cat : CatVar.t;
    is_given_back : bool;
    typ : value_typ option;
  }

  type scope = Tgv of tgv | Temp of int option | Ref | Arg | Res

  type t = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    id : id;
    loc : loc;
    scope : scope;
  }

  let tgv v =
    match v.scope with
    | Tgv s -> s
    | _ ->
        Errors.raise_error
          (Format.sprintf "%s is not a TGV variable" (Pos.unmark v.name))

  let name v = v.name

  let name_str v = Pos.unmark v.name

  let is_table v =
    match v.scope with
    | Tgv tgv -> tgv.is_table
    | Temp is_table -> is_table
    | Ref | Arg | Res -> None

  let cat_var_loc v =
    match v.scope with
    | Tgv tgv -> (
        match tgv.cat with
        | CatVar.Input _ -> Some CatVar.LocInput
        | Computed { is_base } when is_base -> Some CatVar.LocBase
        | Computed _ -> Some CatVar.LocComputed)
    | Temp _ | Ref | Arg | Res -> None

  let size v = match is_table v with None -> 1 | Some sz -> sz

  let alias v = (tgv v).alias

  let alias_str v = Option.fold ~none:"" ~some:Pos.unmark (tgv v).alias

  let descr v = (tgv v).descr

  let descr_str v = Pos.unmark (tgv v).descr

  let attrs v = (tgv v).attrs

  let cat v = (tgv v).cat

  let is_given_back v = (tgv v).is_given_back

  let loc_tgv v =
    match v.loc with
    | LocTgv (_, l) -> l
    | _ ->
        Errors.raise_error
          (Format.sprintf "%s is not a TGV variable" (Pos.unmark v.name))

  let loc_int v =
    match v.loc with
    | LocTgv (_, tgv) -> tgv.loc_int
    | LocTmp (_, li) | LocRef (_, li) | LocArg (_, li) -> li
    | LocRes id ->
        Errors.raise_error
          (Format.sprintf "variable %s doesn't have an index" id)

  let is_temp v = match v.scope with Temp _ -> true | _ -> false

  let is_ref v = v.scope = Ref

  let is_arg v = v.scope = Arg

  let is_res v = v.scope = Res

  let init_loc loc_cat_id =
    {
      loc_id = "";
      loc_cat = CatVar.LocInput;
      loc_idx = 0;
      loc_cat_id;
      loc_cat_str = "";
      loc_cat_idx = 0;
      loc_int = 0;
    }

  let new_tgv ~(name : string Pos.marked) ~(is_table : int option)
      ~(is_given_back : bool) ~(alias : string Pos.marked option)
      ~(descr : string Pos.marked) ~(attrs : int Pos.marked StrMap.t)
      ~(cat : CatVar.t) ~(typ : value_typ option) : t =
    {
      name;
      id = new_id ();
      loc = LocTgv (Pos.unmark name, init_loc cat);
      scope = Tgv { is_table; alias; descr; attrs; cat; is_given_back; typ };
    }

  let new_temp ~(name : string Pos.marked) ~(is_table : int option)
      ~(loc_int : int) : t =
    let loc = LocTmp (Pos.unmark name, loc_int) in
    { name; id = new_id (); loc; scope = Temp is_table }

  let new_ref ~(name : string Pos.marked) ~(loc_int : int) : t =
    let loc = LocRef (Pos.unmark name, loc_int) in
    { name; id = new_id (); loc; scope = Ref }

  let new_arg ~(name : string Pos.marked) ~(loc_int : int) : t =
    let loc = LocArg (Pos.unmark name, loc_int) in
    { name; id = new_id (); loc; scope = Arg }

  let new_res ~(name : string Pos.marked) : t =
    let loc = LocRes (Pos.unmark name) in
    { name; id = new_id (); loc; scope = Res }

  let int_of_scope = function
    | Tgv _ -> 0
    | Temp _ -> 1
    | Ref -> 2
    | Arg -> 3
    | Res -> 4

  let compare (var1 : t) (var2 : t) =
    let c = compare (int_of_scope var1.scope) (int_of_scope var2.scope) in
    if c <> 0 then c
    else
      let c = compare (Pos.unmark var1.name) (Pos.unmark var2.name) in
      if c <> 0 then c else compare var1.id var2.id

  let pp fmt (v : t) = Format.fprintf fmt "(%d)%s" v.id (Pos.unmark v.name)

  type t_var = t

  let pp_var = pp

  let compare_var v0 v1 = Int.compare v0.id v1.id

  module Set = struct
    include SetExt.Make (struct
      type t = t_var

      let compare = compare_var
    end)

    let pp ?(sep = ", ") ?(pp_elt = pp_var) (_ : unit) (fmt : Format.formatter)
        (set : t) : unit =
      pp ~sep ~pp_elt () fmt set
  end

  module Map = struct
    include MapExt.Make (struct
      type t = t_var

      let compare = compare_var
    end)

    let pp ?(sep = "; ") ?(pp_key = pp_var) ?(assoc = " => ")
        (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
        (map : 'a t) : unit =
      pp ~sep ~pp_key ~assoc pp_val fmt map
  end

  (* let compare_name_ref = ref (fun _ _ -> assert false)

     let compare_name n0 n1 = !compare_name_ref n0 n1*)
end

module DomainId = StrSet

module DomainIdSet = struct
  include SetSetExt.Make (DomainId)

  module type T =
    SetSetExt.T with type base_elt = string and type elt = DomainId.t

  let pp ?(sep1 = ", ") ?(sep2 = " ") ?(pp_elt = Format.pp_print_string)
      (_ : unit) (fmt : Format.formatter) (setSet : t) : unit =
    pp ~sep1 ~sep2 ~pp_elt () fmt setSet
end

module DomainIdMap = struct
  include MapExt.Make (DomainId)

  module type T = MapExt.T with type key = DomainId.t

  let pp ?(sep = ", ") ?(pp_key = DomainId.pp ()) ?(assoc = " => ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

type 'a domain = {
  dom_id : DomainId.t Pos.marked;
  dom_names : Pos.t DomainIdMap.t;
  dom_by_default : bool;
  dom_min : DomainIdSet.t;
  dom_max : DomainIdSet.t;
  dom_rov : IntSet.t;
  dom_data : 'a;
  dom_used : int Pos.marked option;
}

type rule_domain_data = { rdom_computable : bool }

type rule_domain = rule_domain_data domain

type verif_domain_data = {
  vdom_auth : Pos.t CatVar.Map.t;
  vdom_verifiable : bool;
}

type verif_domain = verif_domain_data domain

module TargetMap = StrMap

type literal = Float of float | Undefined

type 'v atom = AtomVar of 'v | AtomLiteral of literal

type 'v set_value_loop =
  | Single of 'v atom Pos.marked
  | Range of 'v atom Pos.marked * 'v atom Pos.marked
  | Interval of 'v atom Pos.marked * 'v atom Pos.marked

type 'v loop_variable = char Pos.marked * 'v set_value_loop list

type 'v loop_variables =
  | ValueSets of 'v loop_variable list
  | Ranges of 'v loop_variable list

(** Unary operators *)
type unop = Not | Minus

(** Binary operators *)
type binop = And | Or | Add | Sub | Mul | Div

(** Comparison operators *)
type comp_op = Gt | Gte | Lt | Lte | Eq | Neq

type 'v set_value =
  | FloatValue of float Pos.marked
  | VarValue of 'v Pos.marked
  | Interval of int Pos.marked * int Pos.marked

type func =
  | SumFunc  (** Sums the arguments *)
  | AbsFunc  (** Absolute value *)
  | MinFunc  (** Minimum of a list of values *)
  | MaxFunc  (** Maximum of a list of values *)
  | GtzFunc  (** Greater than zero (strict) ? *)
  | GtezFunc  (** Greater or equal than zero ? *)
  | NullFunc  (** Equal to zero ? *)
  | ArrFunc  (** Round to nearest integer *)
  | InfFunc  (** Truncate to integer *)
  | PresentFunc  (** Different than zero ? *)
  | Multimax  (** ??? *)
  | Supzero  (** ??? *)
  | VerifNumber
  | ComplNumber
  | Func of string

type 'v expression =
  | TestInSet of bool * 'v m_expression * 'v set_value list
      (** Test if an expression is in a set of value (or not in the set if the
          flag is set to [false]) *)
  | Unop of unop * 'v m_expression
  | Comparison of comp_op Pos.marked * 'v m_expression * 'v m_expression
  | Binop of binop Pos.marked * 'v m_expression * 'v m_expression
  | Index of 'v Pos.marked * 'v m_expression
  | Conditional of 'v m_expression * 'v m_expression * 'v m_expression option
  | FuncCall of func Pos.marked * 'v m_expression list
  | FuncCallLoop of
      func Pos.marked * 'v loop_variables Pos.marked * 'v m_expression
  | Literal of literal
  | Var of 'v
  | Loop of 'v loop_variables Pos.marked * 'v m_expression
      (** The loop is prefixed with the loop variables declarations *)
  | NbCategory of Pos.t CatVar.Map.t
  | Attribut of 'v Pos.marked * string Pos.marked
  | Size of 'v Pos.marked
  | NbAnomalies
  | NbDiscordances
  | NbInformatives
  | NbBloquantes

and 'v m_expression = 'v expression Pos.marked

let get_used_variables (e : 'v expression) : 'v list =
  let rec get_used_variables_ (e : 'v expression) (acc : 'v list) =
    match e with
    | TestInSet (_, (e, _), _) | Unop (_, (e, _)) ->
        let acc = get_used_variables_ e acc in
        acc
    | Comparison (_, (e1, _), (e2, _)) | Binop (_, (e1, _), (e2, _)) ->
        let acc = get_used_variables_ e1 acc in
        let acc = get_used_variables_ e2 acc in
        acc
    | Index ((var, _), (e, _)) ->
        let acc = var :: acc in
        let acc = get_used_variables_ e acc in
        acc
    | Conditional ((e1, _), (e2, _), e3) -> (
        let acc = get_used_variables_ e1 acc in
        let acc = get_used_variables_ e2 acc in
        match e3 with None -> acc | Some (e3, _) -> get_used_variables_ e3 acc)
    | FuncCall (_, args) ->
        List.fold_left
          (fun acc (arg, _) -> get_used_variables_ arg acc)
          acc args
    | FuncCallLoop _ | Loop _ -> assert false
    | Var var | Size (var, _) | Attribut ((var, _), _) -> var :: acc
    | Literal _ | NbCategory _ | NbAnomalies | NbDiscordances | NbInformatives
    | NbBloquantes ->
        acc
  in
  get_used_variables_ e []

module Error = struct
  type typ = Anomaly | Discordance | Information

  let compare_typ e1 e2 =
    match (e1, e2) with
    | Anomaly, (Discordance | Information) -> -1
    | (Discordance | Information), Anomaly -> 1
    | Information, Discordance -> -1
    | Discordance, Information -> 1
    | _ -> 0

  type t = {
    name : string Pos.marked;
    famille : string Pos.marked;
    code_bo : string Pos.marked;
    sous_code : string Pos.marked;
    libelle : string Pos.marked;
    is_isf : string Pos.marked;
    typ : typ;
  }

  let pp_descr fmt err =
    Format.fprintf fmt "%s:%s:%s:%s:%s" (Pos.unmark err.famille)
      (Pos.unmark err.code_bo) (Pos.unmark err.sous_code)
      (Pos.unmark err.libelle) (Pos.unmark err.is_isf)

  let compare (var1 : t) (var2 : t) = compare var1.name var2.name
end

type print_std = StdOut | StdErr

type 'v print_arg =
  | PrintString of string
  | PrintName of 'v Pos.marked
  | PrintAlias of 'v Pos.marked
  | PrintIndent of 'v m_expression
  | PrintExpr of 'v m_expression * int * int

type 'v formula_loop = 'v loop_variables Pos.marked

type 'v formula_decl = 'v Pos.marked * 'v m_expression option * 'v m_expression

type 'v formula =
  | SingleFormula of 'v formula_decl
  | MultipleFormulaes of 'v formula_loop * 'v formula_decl

type ('v, 'e) instruction =
  | Affectation of 'v formula Pos.marked
  | IfThenElse of
      'v m_expression
      * ('v, 'e) m_instruction list
      * ('v, 'e) m_instruction list
  | WhenDoElse of
      ('v m_expression * ('v, 'e) m_instruction list * Pos.t) list
      * ('v, 'e) m_instruction list Pos.marked
  | ComputeDomain of string Pos.marked list Pos.marked
  | ComputeChaining of string Pos.marked
  | ComputeVerifs of string Pos.marked list Pos.marked * 'v m_expression
  | ComputeTarget of string Pos.marked * 'v Pos.marked list
  | VerifBlock of ('v, 'e) m_instruction list
  | Print of print_std * 'v print_arg Pos.marked list
  | Iterate of
      'v Pos.marked
      * 'v Pos.marked list
      * (Pos.t CatVar.Map.t * 'v m_expression) list
      * ('v, 'e) m_instruction list
  | Restore of
      'v Pos.marked list
      * ('v Pos.marked * Pos.t CatVar.Map.t * 'v m_expression) list
      * ('v, 'e) m_instruction list
  | RaiseError of 'e Pos.marked * string Pos.marked option
  | CleanErrors
  | ExportErrors
  | FinalizeErrors

and ('v, 'e) m_instruction = ('v, 'e) instruction Pos.marked

let set_loc_int loc loc_int =
  match loc with
  | LocTgv (id, tgv) -> LocTgv (id, { tgv with loc_int })
  | LocTmp (id, _) -> LocTmp (id, loc_int)
  | LocRef (id, _) -> LocRef (id, loc_int)
  | LocArg (id, _) -> LocArg (id, loc_int)
  | LocRes id ->
      Errors.raise_error (Format.sprintf "variable %s doesn't have an index" id)

let set_loc_tgv_cat loc loc_cat loc_cat_str loc_cat_idx =
  match loc with
  | LocTgv (id, tgv) ->
      LocTgv (id, { tgv with loc_cat; loc_cat_str; loc_cat_idx })
  | LocTmp (id, _) | LocRef (id, _) | LocArg (id, _) | LocRes id ->
      Errors.raise_error (Format.sprintf "%s has not a TGV location" id)

let set_loc_tgv_idx loc loc_idx =
  match loc with
  | LocTgv (id, tgv) -> LocTgv (id, { tgv with loc_idx })
  | LocTmp (id, _) | LocRef (id, _) | LocArg (id, _) | LocRes id ->
      Errors.raise_error (Format.sprintf "%s has not a TGV location" id)

let format_value_typ fmt t =
  Pp.string fmt
    (match t with
    | Boolean -> "BOOLEEN"
    | DateYear -> "DATE_AAAA"
    | DateDayMonthYear -> "DATE_JJMMAAAA"
    | DateMonth -> "DATE_MM"
    | Integer -> "ENTIER"
    | Real -> "REEL")

let format_literal fmt l =
  Format.pp_print_string fmt
    (match l with Float f -> string_of_float f | Undefined -> "indefini")

let format_atom form_var fmt vl =
  match vl with
  | AtomVar v -> form_var fmt v
  | AtomLiteral l -> format_literal fmt l

let format_set_value_loop form_var fmt sv =
  let form_atom = format_atom form_var in
  match sv with
  | Single l -> Format.fprintf fmt "%a" form_atom (Pos.unmark l)
  | Range (i1, i2) ->
      Format.fprintf fmt "%a..%a" form_atom (Pos.unmark i1) form_atom
        (Pos.unmark i2)
  | Interval (i1, i2) ->
      Format.fprintf fmt "%a-%a" form_atom (Pos.unmark i1) form_atom
        (Pos.unmark i2)

let format_loop_variable_ranges form_var fmt (v, vs) =
  Format.fprintf fmt "un %c dans %a" (Pos.unmark v)
    (Pp.list_comma (format_set_value_loop form_var))
    vs

let format_loop_variable_value_set form_var fmt (v, vs) =
  Format.fprintf fmt "%c=%a" (Pos.unmark v)
    (Pp.list_comma (format_set_value_loop form_var))
    vs

let format_loop_variables form_var fmt lvs =
  match lvs with
  | ValueSets vvs ->
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
        (format_loop_variable_value_set form_var)
        fmt vvs
  | Ranges vvs ->
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " et ")
        (format_loop_variable_ranges form_var)
        fmt vvs

let format_unop fmt op =
  Format.pp_print_string fmt (match op with Not -> "non" | Minus -> "-")

let format_binop fmt op =
  Format.pp_print_string fmt
    (match op with
    | And -> "et"
    | Or -> "ou"
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/")

let format_comp_op fmt op =
  Format.pp_print_string fmt
    (match op with
    | Gt -> ">"
    | Gte -> ">="
    | Lt -> "<"
    | Lte -> "<="
    | Eq -> "="
    | Neq -> "!=")

let format_set_value format_variable fmt sv =
  let open Format in
  match sv with
  | VarValue v -> format_variable fmt (Pos.unmark v)
  | Interval (i1, i2) -> fprintf fmt "%d..%d" (Pos.unmark i1) (Pos.unmark i2)
  | FloatValue i -> fprintf fmt "%f" (Pos.unmark i)

let format_func fmt f =
  Format.pp_print_string fmt
    (match f with
    | SumFunc -> "somme"
    | AbsFunc -> "abs"
    | MinFunc -> "min"
    | MaxFunc -> "max"
    | GtzFunc -> "positif"
    | GtezFunc -> "positif_ou_nul"
    | NullFunc -> "null"
    | ArrFunc -> "arr"
    | InfFunc -> "inf"
    | PresentFunc -> "present"
    | Multimax -> "multimax"
    | Supzero -> "supzero"
    | VerifNumber -> "numero_verif"
    | ComplNumber -> "numero_compl"
    | Func fn -> fn)

let rec format_expression form_var fmt =
  let form_expr = format_expression form_var in
  function
  | TestInSet (belong, e, values) ->
      Format.fprintf fmt "(%a %sdans %a)" form_expr (Pos.unmark e)
        (if belong then "" else "non ")
        (Pp.list_comma (format_set_value form_var))
        values
  | Comparison (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" form_expr (Pos.unmark e1) format_comp_op
        (Pos.unmark op) form_expr (Pos.unmark e2)
  | Binop (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" form_expr (Pos.unmark e1) format_binop
        (Pos.unmark op) form_expr (Pos.unmark e2)
  | Unop (op, e) ->
      Format.fprintf fmt "%a %a" format_unop op form_expr (Pos.unmark e)
  | Index (v, i) ->
      Format.fprintf fmt "%a[%a]" form_var (Pos.unmark v) form_expr
        (Pos.unmark i)
  | Conditional (e1, e2, e3) ->
      let pp_sinon fmt e = Format.fprintf fmt " sinon %a" form_expr e in
      Format.fprintf fmt "(si %a alors %a%a finsi)" form_expr (Pos.unmark e1)
        form_expr (Pos.unmark e2)
        (Pp.option (Pp.unmark pp_sinon))
        e3
  | FuncCall (f, args) ->
      Format.fprintf fmt "%a(%a)" format_func (Pos.unmark f)
        (Pp.list_space (Pp.unmark form_expr))
        args
  | FuncCallLoop (f, lvs, e) ->
      Format.fprintf fmt "%a(%a%a)" format_func (Pos.unmark f)
        (format_loop_variables form_var)
        (Pos.unmark lvs) form_expr (Pos.unmark e)
  | Literal l -> format_literal fmt l
  | Var v -> form_var fmt v
  | Loop (lvs, e) ->
      Format.fprintf fmt "pour %a%a"
        (format_loop_variables form_var)
        (Pos.unmark lvs) form_expr (Pos.unmark e)
  | NbCategory cs ->
      Format.fprintf fmt "nb_categorie(%a)" (CatVar.Map.pp_keys ()) cs
  | Attribut (v, a) ->
      Format.fprintf fmt "attribut(%a, %s)" form_var (Pos.unmark v)
        (Pos.unmark a)
  | Size v -> Format.fprintf fmt "taille(%a)" form_var (Pos.unmark v)
  | NbAnomalies -> Format.fprintf fmt "nb_anomalies()"
  | NbDiscordances -> Format.fprintf fmt "nb_discordances()"
  | NbInformatives -> Format.fprintf fmt "nb_informatives()"
  | NbBloquantes -> Format.fprintf fmt "nb_bloquantes()"

let format_print_arg form_var fmt = function
  | PrintString s -> Format.fprintf fmt "\"%s\"" s
  | PrintName v -> Format.fprintf fmt "nom(%a)" (Pp.unmark form_var) v
  | PrintAlias v -> Format.fprintf fmt "alias(%a)" (Pp.unmark form_var) v
  | PrintIndent e ->
      Format.fprintf fmt "indenter(%a)"
        (Pp.unmark (format_expression form_var))
        e
  | PrintExpr (e, min, max) ->
      if min = max_int then
        Format.fprintf fmt "(%a)" (Pp.unmark (format_expression form_var)) e
      else if max = max_int then
        Format.fprintf fmt "(%a):%d"
          (Pp.unmark (format_expression form_var))
          e min
      else
        Format.fprintf fmt "(%a):%d..%d"
          (Pp.unmark (format_expression form_var))
          e min max

let format_formula_decl form_var fmt (v, idx, e) =
  Format.fprintf fmt "%a" form_var (Pos.unmark v);
  (match idx with
  | Some vi ->
      Format.fprintf fmt "[%a]" (format_expression form_var) (Pos.unmark vi)
  | None -> ());
  Format.fprintf fmt " = %a" (format_expression form_var) (Pos.unmark e)

let format_formula form_var fmt f =
  match f with
  | SingleFormula f -> format_formula_decl form_var fmt f
  | MultipleFormulaes (lvs, f) ->
      Format.fprintf fmt "pour %a\n%a"
        (format_loop_variables form_var)
        (Pos.unmark lvs)
        (format_formula_decl form_var)
        f

let rec format_instruction form_var form_err =
  let form_expr = format_expression form_var in
  let form_instrs = format_instructions form_var form_err in
  fun fmt instr ->
    match instr with
    | Affectation f -> Pp.unmark (format_formula form_var) fmt f
    | IfThenElse (cond, t, []) ->
        Format.fprintf fmt "if(%a):@\n@[<h 2>  %a@]@\n" form_expr
          (Pos.unmark cond) form_instrs t
    | IfThenElse (cond, t, f) ->
        Format.fprintf fmt "if(%a):@\n@[<h 2>  %a@]else:@\n@[<h 2>  %a@]@\n"
          form_expr (Pos.unmark cond) form_instrs t form_instrs f
    | WhenDoElse (wdl, ed) ->
        let pp_wd th fmt (expr, dl, _) =
          Format.fprintf fmt "@[<v 2>%swhen (%a) do@\n%a@;@]" th form_expr
            (Pos.unmark expr) form_instrs dl
        in
        let pp_wdl fmt wdl =
          let rec aux th = function
            | wd :: l ->
                pp_wd th fmt wd;
                aux "then_" l
            | [] -> ()
          in
          aux "" wdl
        in
        let pp_ed fmt (dl, _) =
          Format.fprintf fmt "@[<v 2>else_do@\n%a@;@]endwhen@;" form_instrs dl
        in
        Format.fprintf fmt "%a%a@\n" pp_wdl wdl pp_ed ed
    | VerifBlock vb ->
        Format.fprintf fmt
          "@[<v 2># debut verif block@\n%a@]@\n# fin verif block@\n" form_instrs
          vb
    | ComputeDomain l ->
        Format.fprintf fmt "calculer domaine %a;"
          (Pp.list_space (Pp.unmark Pp.string))
          (Pos.unmark l)
    | ComputeChaining ch ->
        Format.fprintf fmt "calculer enchaineur %s;" (Pos.unmark ch)
    | ComputeVerifs (l, expr) ->
        Format.fprintf fmt "verifier %a : avec %a;"
          (Pp.list_space (Pp.unmark Pp.string))
          (Pos.unmark l) (Pp.unmark form_expr) expr
    | ComputeTarget (tname, targs) ->
        Format.fprintf fmt "calculer cible %s : avec %a@," (Pos.unmark tname)
          (Pp.list_comma (Pp.unmark form_var))
          targs
    | Print (std, args) ->
        let print_cmd =
          match std with StdOut -> "afficher" | StdErr -> "afficher_erreur"
        in
        Format.fprintf fmt "%s %a;" print_cmd
          (Pp.list_space (Pp.unmark (format_print_arg form_var)))
          args
    | Iterate (var, vars, var_params, itb) ->
        let format_var_param fmt (vcs, expr) =
          Format.fprintf fmt ": categorie %a : avec %a@\n"
            (CatVar.Map.pp_keys ()) vcs form_expr (Pos.unmark expr)
        in
        Format.fprintf fmt "iterate variable %a@;: %a@;: %a@;: dans (" form_var
          (Pos.unmark var)
          (Pp.list_comma (Pp.unmark form_var))
          vars
          (Pp.list_space format_var_param)
          var_params;
        Format.fprintf fmt "@[<h 2>  %a@]@\n)@\n" form_instrs itb
    | Restore (vars, var_params, rb) ->
        let format_var_param fmt (var, vcs, expr) =
          Format.fprintf fmt ": variable %a : categorie %a : avec %a@\n"
            (Pp.unmark form_var) var (CatVar.Map.pp_keys ()) vcs form_expr
            (Pos.unmark expr)
        in
        Format.fprintf fmt "restaure@;: %a@;: %a@;: apres ("
          (Pp.list_comma (Pp.unmark form_var))
          vars
          (Pp.list_space format_var_param)
          var_params;
        Format.fprintf fmt "@[<h 2>  %a@]@\n)@\n" form_instrs rb
    | RaiseError (err, var_opt) ->
        Format.fprintf fmt "leve_erreur %a %s\n" form_err (Pos.unmark err)
          (match var_opt with Some var -> " " ^ Pos.unmark var | None -> "")
    | CleanErrors -> Format.fprintf fmt "nettoie_erreurs\n"
    | ExportErrors -> Format.fprintf fmt "exporte_erreurs\n"
    | FinalizeErrors -> Format.fprintf fmt "finalise_erreurs\n"

and format_instructions form_var form_err fmt instrs =
  Pp.list "" (Pp.unmark (format_instruction form_var form_err)) fmt instrs
