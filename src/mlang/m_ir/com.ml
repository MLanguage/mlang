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
      | Pos.Mark ([ Pos.Mark ("*", _) ], id_pos) ->
          one (Input (StrSet.one "*")) id_pos
          |> add (Computed { is_base = false }) id_pos
          |> add (Computed { is_base = true }) id_pos
      | Pos.Mark ([ Pos.Mark ("saisie", _); Pos.Mark ("*", _) ], id_pos) ->
          one (Input (StrSet.one "*")) id_pos
      | Pos.Mark (Pos.Mark ("saisie", _) :: id, id_pos) ->
          one (Input (StrSet.from_marked_list id)) id_pos
      | Pos.Mark (Pos.Mark ("calculee", _) :: id, id_pos) -> (
          match id with
          | [] -> one (Computed { is_base = false }) id_pos
          | [ Pos.Mark ("base", _) ] -> one (Computed { is_base = true }) id_pos
          | [ Pos.Mark ("*", _) ] ->
              one (Computed { is_base = false }) id_pos
              |> add (Computed { is_base = true }) id_pos
          | _ -> Errors.raise_spanned_error "invalid variable category" id_pos)
      | Pos.Mark (_, id_pos) ->
          Errors.raise_spanned_error "invalid variable category" id_pos
  end

  type loc = LocComputed | LocBase | LocInput

  let pp_loc oc = function
    | LocInput -> Pp.fpr oc "input"
    | LocComputed -> Pp.fpr oc "computed"
    | LocBase -> Pp.fpr oc "base"

  module LocSet = struct
    include SetExt.Make (struct
      type t = loc

      let compare = Stdlib.compare
    end)

    let pp ?(sep = ", ") ?(pp_elt = pp_loc) (_ : unit) (fmt : Format.formatter)
        (set : t) : unit =
      pp ~sep ~pp_elt () fmt set
  end

  module LocMap = struct
    include MapExt.Make (struct
      type t = loc

      let compare = Stdlib.compare
    end)

    let pp ?(sep = "; ") ?(pp_key = pp_loc) ?(assoc = " => ")
        (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
        (map : 'a t) : unit =
      pp ~sep ~pp_key ~assoc pp_val fmt map
  end

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
  loc_cat : CatVar.loc;
  loc_idx : int;
  loc_tab_idx : int;
  loc_cat_id : CatVar.t;
  loc_cat_str : string;
  loc_cat_idx : int;
}

type loc_tmp = { loc_idx : int; loc_tab_idx : int; loc_cat_idx : int }

type loc =
  | LocTgv of string * loc_tgv
  | LocTmp of string * loc_tmp
  | LocRef of string * int

module Var = struct
  type id = int

  let id_cpt = ref 0

  let new_id () =
    let id = !id_cpt in
    incr id_cpt;
    id

  type tgv = {
    table : t Array.t option;
    alias : string Pos.marked option;  (** Input variable have an alias *)
    descr : string Pos.marked;
        (** Description taken from the variable declaration *)
    attrs : int Pos.marked StrMap.t;
    cat : CatVar.t;
    is_given_back : bool;
    typ : value_typ option;
  }

  and scope = Tgv of tgv | Temp of t Array.t option | Ref

  and t = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    id : id;
    loc : loc;
    scope : scope;
  }

  let tgv v =
    match v.scope with
    | Tgv s -> s
    | _ ->
        let msg = Pp.spr "%s is not a TGV variable" (Pos.unmark v.name) in
        Errors.raise_error msg

  let name v = v.name

  let name_str v = Pos.unmark v.name

  let get_table v =
    match v.scope with
    | Tgv tgv -> tgv.table
    | Temp table -> table
    | Ref -> None

  let is_table v = get_table v <> None

  let set_table v table =
    match v.scope with
    | Tgv tgv -> { v with scope = Tgv { tgv with table } }
    | Temp _ -> { v with scope = Temp table }
    | Ref -> v

  let cat_var_loc v =
    match v.scope with
    | Tgv tgv -> (
        match tgv.cat with
        | CatVar.Input _ -> CatVar.LocInput
        | Computed { is_base } when is_base -> CatVar.LocBase
        | Computed _ -> CatVar.LocComputed)
    | Temp _ | Ref -> failwith "not a TGV variable"

  let size v = match get_table v with None -> 1 | Some tab -> Array.length tab

  let alias v = match v.scope with Tgv s -> s.alias | _ -> None

  let alias_str v =
    match v.scope with
    | Tgv s -> Option.fold ~none:"" ~some:Pos.unmark s.alias
    | _ -> ""

  let descr v = (tgv v).descr

  let descr_str v = Pos.unmark (tgv v).descr

  let attrs v = (tgv v).attrs

  let cat v = (tgv v).cat

  let typ v = (tgv v).typ

  let is_given_back v = (tgv v).is_given_back

  let loc_tgv v =
    match v.loc with
    | LocTgv (_, l) -> l
    | _ ->
        let msg = Pp.spr "%s is not a TGV variable" (Pos.unmark v.name) in
        Errors.raise_error msg

  let loc_cat_idx v =
    match v.loc with
    | LocTgv (_, tgv) -> tgv.loc_cat_idx
    | LocTmp (_, tmp) -> tmp.loc_cat_idx
    | LocRef (_, li) -> li

  let set_loc_tgv_idx v (cv : CatVar.data) i =
    match v.loc with
    | LocTgv (id, tgv) ->
        let loc_cat = cv.loc in
        let loc_cat_str = cv.id_str in
        let tgv = { tgv with loc_cat; loc_cat_str; loc_cat_idx = i } in
        { v with loc = LocTgv (id, tgv) }
    | LocTmp (id, _) | LocRef (id, _) ->
        Errors.raise_error (Pp.spr "%s has not a TGV location" id)

  let set_loc_tmp_idx v i =
    match v.loc with
    | LocTmp (id, tmp) ->
        let tmp = { tmp with loc_cat_idx = i } in
        { v with loc = LocTmp (id, tmp) }
    | LocTgv (id, _) | LocRef (id, _) ->
        Errors.raise_error (Pp.spr "%s has not a TGV location" id)

  let loc_idx v =
    match v.loc with
    | LocTgv (_, tgv) -> tgv.loc_idx
    | LocTmp (_, tmp) -> tmp.loc_idx
    | LocRef (_, li) -> li

  let set_loc_idx v loc_idx =
    let loc =
      match v.loc with
      | LocTgv (id, tgv) -> LocTgv (id, { tgv with loc_idx })
      | LocTmp (id, tmp) -> LocTmp (id, { tmp with loc_idx })
      | LocRef (id, _) -> LocRef (id, loc_idx)
    in
    { v with loc }

  let loc_tab_idx v =
    match v.loc with
    | LocTgv (_, tgv) -> tgv.loc_tab_idx
    | LocTmp (_, tmp) -> tmp.loc_tab_idx
    | LocRef (id, _) ->
        let msg = Pp.spr "variable %s cannot be a table" id in
        Errors.raise_error msg

  let set_loc_tab_idx v loc_tab_idx =
    let loc =
      match v.loc with
      | LocTgv (id, tgv) -> LocTgv (id, { tgv with loc_tab_idx })
      | LocTmp (id, tmp) -> LocTmp (id, { tmp with loc_tab_idx })
      | LocRef (id, _) ->
          let msg = Pp.spr "variable %s cannot be a table" id in
          Errors.raise_error msg
    in
    { v with loc }

  let is_tgv v = match v.scope with Tgv _ -> true | _ -> false

  let is_temp v = match v.scope with Temp _ -> true | _ -> false

  let is_ref v = v.scope = Ref

  let init_loc loc_cat_id =
    {
      loc_cat = CatVar.LocInput;
      loc_idx = 0;
      loc_tab_idx = -1;
      loc_cat_id;
      loc_cat_str = "";
      loc_cat_idx = 0;
    }

  let new_tgv ~(name : string Pos.marked) ~(table : t Array.t option)
      ~(is_given_back : bool) ~(alias : string Pos.marked option)
      ~(descr : string Pos.marked) ~(attrs : int Pos.marked StrMap.t)
      ~(cat : CatVar.t) ~(typ : value_typ option) : t =
    {
      name;
      id = new_id ();
      loc = LocTgv (Pos.unmark name, init_loc cat);
      scope = Tgv { table; alias; descr; attrs; cat; is_given_back; typ };
    }

  let new_temp ~(name : string Pos.marked) ~(table : t Array.t option) : t =
    let loc =
      LocTmp
        (Pos.unmark name, { loc_idx = -1; loc_tab_idx = -1; loc_cat_idx = -1 })
    in
    { name; id = new_id (); loc; scope = Temp table }

  let new_ref ~(name : string Pos.marked) : t =
    let loc = LocRef (Pos.unmark name, -1) in
    { name; id = new_id (); loc; scope = Ref }

  let new_arg ~(name : string Pos.marked) : t = new_temp ~name ~table:None

  let new_res ~(name : string Pos.marked) : t = new_temp ~name ~table:None

  let int_of_scope = function Tgv _ -> 0 | Temp _ -> 1 | Ref -> 2

  let compare (var1 : t) (var2 : t) =
    let c = compare (int_of_scope var1.scope) (int_of_scope var2.scope) in
    if c <> 0 then c
    else
      let c = compare (Pos.unmark var1.name) (Pos.unmark var2.name) in
      if c <> 0 then c else compare var1.id var2.id

  let pp fmt (v : t) = Format.fprintf fmt "(%d)%s" v.id (Pos.unmark v.name)

  type t_var = t

  let pp_var = pp

  let compare_var v0 v1 = compare v0 v1

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

type event_field = { name : string Pos.marked; index : int; is_var : bool }

type ('n, 'v) event_value = Numeric of 'n | RefVar of 'v

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

type variable_space = {
  vs_id : int;
  vs_name : string Pos.marked;
  vs_cats : CatVar.loc Pos.marked CatVar.LocMap.t;
  vs_by_default : bool;
}

type literal = Float of float | Undefined

type origin = string Pos.marked option

type literal_with_orig = { lit : literal; origin : origin }

(** Unary operators *)
type unop = Not | Minus

(** Binary operators *)
type binop = And | Or | Add | Sub | Mul | Div | Mod

(** Comparison operators *)
type comp_op = Gt | Gte | Lt | Lte | Eq | Neq

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
  | NbEvents
  | Func of string

type var_name_generic = { base : string; parameters : char list }
(** For generic variables, we record the list of their lowercase parameters *)

(** A variable is either generic (with loop parameters) or normal *)
type var_name = Normal of string | Generic of var_name_generic

type m_var_name = var_name Pos.marked

type var_space = (m_var_name * int) option

type 'v access =
  | VarAccess of var_space * 'v
  | TabAccess of var_space * 'v * 'v m_expression
  | FieldAccess of var_space * 'v m_expression * string Pos.marked * int

and 'v m_access = 'v access Pos.marked

and 'v atom = AtomVar of 'v | AtomLiteral of literal_with_orig

and 'v set_value_loop =
  | Single of 'v atom Pos.marked
  | Range of 'v atom Pos.marked * 'v atom Pos.marked
  | Interval of 'v atom Pos.marked * 'v atom Pos.marked

and 'v loop_variable = char Pos.marked * 'v set_value_loop list

and 'v loop_variables =
  | ValueSets of 'v loop_variable list
  | Ranges of 'v loop_variable list

and 'v set_value =
  | FloatValue of float Pos.marked
  | VarValue of 'v m_access
  | IntervalValue of int Pos.marked * int Pos.marked

and 'v expression =
  | TestInSet of bool * 'v m_expression * 'v set_value list
      (** Test if an expression is in a set of value (or not in the set if the
          flag is set to [false]) *)
  | Unop of unop * 'v m_expression
  | Comparison of comp_op Pos.marked * 'v m_expression * 'v m_expression
  | Binop of binop Pos.marked * 'v m_expression * 'v m_expression
  | Conditional of 'v m_expression * 'v m_expression * 'v m_expression option
  | FuncCall of func Pos.marked * 'v m_expression list
  | FuncCallLoop of
      func Pos.marked * 'v loop_variables Pos.marked * 'v m_expression
  | Literal of literal_with_orig
  | Var of 'v access
  | Loop of 'v loop_variables Pos.marked * 'v m_expression
      (** The loop is prefixed with the loop variables declarations *)
  | NbCategory of Pos.t CatVar.Map.t
  | Attribut of 'v m_access * string Pos.marked
  | Size of 'v m_access
  | IsVariable of 'v m_access * string Pos.marked
  | NbAnomalies
  | NbDiscordances
  | NbInformatives
  | NbBloquantes

and 'v m_expression = 'v expression Pos.marked

type const = { id : string; value : literal; pos : Pos.t }

type 'v dep =
  | Tab of 'v * 'v m_expression
  | V of 'v
  | LiteralDep of literal
  | Const of const

(* This code was taken from Noe and adapted to the 2025 var architecture *)
let get_used_variables (e : 'v expression) :
    ('v dep * 'v expression option) list =
  let rec get_used_variables_ (e : 'v expression)
      (acc : ('v dep * 'v expression option) list) =
    match e with
    | TestInSet (_, Mark (e, _), _) | Unop (_, Mark (e, _)) ->
        let acc = get_used_variables_ e acc in
        acc
    | Comparison (_, Mark (e1, _), Mark (e2, _))
    | Binop (_, Mark (e1, _), Mark (e2, _)) ->
        let acc = get_used_variables_ e1 acc in
        let acc = get_used_variables_ e2 acc in
        acc
    | Conditional (Mark (e1, _), Mark (e2, _), e3) -> (
        let acc = get_used_variables_ e1 acc in
        let acc = get_used_variables_ e2 acc in
        match e3 with
        | None -> acc
        | Some (Mark (e3, _)) -> get_used_variables_ e3 acc)
    | FuncCall (_, args) ->
        List.fold_left
          (fun acc arg -> get_used_variables_ (Pos.unmark arg) acc)
          acc args
    | FuncCallLoop _ | Loop _ -> assert false
    | Var var
    | Size (Mark (var, _))
    | Attribut (Mark (var, _), _)
    | IsVariable (Mark (var, _), _) -> (
        match var with
        | TabAccess (_, v, m_i) -> (Tab (v, m_i), None) :: acc
        | VarAccess (_, v) -> (V v, None) :: acc
        | FieldAccess (_, Mark (v, _), _, _) -> get_used_variables_ v acc)
    | Literal { lit; origin = Some (Mark (id, pos)) } ->
        (Const { id; value = lit; pos }, None) :: acc
    | Literal { lit; origin = None } -> (LiteralDep lit, None) :: acc
    | NbCategory _ | NbAnomalies | NbDiscordances | NbInformatives
    | NbBloquantes ->
        acc
  in
  get_used_variables_ e []

let mk_lit_with_orig lit origin = { lit; origin }

let mk_lit lit = Literal (mk_lit_with_orig lit None)

let mk_lit_from_const lit constname =
  Literal (mk_lit_with_orig lit (Some constname))

let mk_atomlit lit = AtomLiteral (mk_lit_with_orig lit None)

let mk_atomlit_from_const lit constname =
  AtomLiteral (mk_lit_with_orig lit (Some constname))

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
    Pp.fpr fmt "%s:%s:%s:%s:%s" (Pos.unmark err.famille)
      (Pos.unmark err.code_bo) (Pos.unmark err.sous_code)
      (Pos.unmark err.libelle) (Pos.unmark err.is_isf)

  let pp fmt err = Pp.fpr fmt "%s:%a" (Pos.unmark err.name) pp_descr err

  let compare (err1 : t) (err2 : t) = compare err1.name err2.name

  type error_t = t

  let error_pp = pp

  let error_compare = compare

  module Set = struct
    include SetExt.Make (struct
      type t = error_t

      let compare = error_compare
    end)

    let pp ?(sep = ", ") ?(pp_elt = error_pp) (_ : unit)
        (fmt : Format.formatter) (set : t) : unit =
      pp ~sep ~pp_elt () fmt set
  end

  module Map = struct
    include MapExt.Make (struct
      type t = error_t

      let compare = error_compare
    end)

    let pp ?(sep = "; ") ?(pp_key = error_pp) ?(assoc = " => ")
        (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
        (map : 'a t) : unit =
      pp ~sep ~pp_key ~assoc pp_val fmt map
  end
end

type print_std = StdOut | StdErr

type print_info = Name | Alias

type 'v print_arg =
  | PrintString of string
  | PrintAccess of print_info * 'v m_access
  | PrintIndent of 'v m_expression
  | PrintExpr of 'v m_expression * int * int

type 'v formula_loop = 'v loop_variables Pos.marked

type 'v formula_decl =
  | VarDecl of 'v access Pos.marked * 'v m_expression
  | EventFieldRef of 'v m_expression * string Pos.marked * int * 'v

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
  | ComputeDomain of string Pos.marked list Pos.marked * var_space
  | ComputeChaining of string Pos.marked * var_space
  | ComputeVerifs of
      string Pos.marked list Pos.marked * 'v m_expression * var_space
  | ComputeTarget of string Pos.marked * 'v m_access list * var_space
  | VerifBlock of ('v, 'e) m_instruction list
  | Print of print_std * 'v print_arg Pos.marked list
  | Iterate of
      'v
      * 'v m_access list
      * (Pos.t CatVar.Map.t * 'v m_expression * var_space) list
      * ('v, 'e) m_instruction list
  | Iterate_values of
      'v
      * ('v m_expression * 'v m_expression * 'v m_expression) list
      * ('v, 'e) m_instruction list
  | Restore of
      'v m_access list
      * ('v * Pos.t CatVar.Map.t * 'v m_expression * var_space) list
      * 'v m_expression list
      * ('v * 'v m_expression) list
      * ('v, 'e) m_instruction list
  | ArrangeEvents of
      ('v * 'v * 'v m_expression) option
      * ('v * 'v m_expression) option
      * 'v m_expression option
      * ('v, 'e) m_instruction list
  | RaiseError of 'e Pos.marked * string Pos.marked option
  | CleanErrors
  | ExportErrors
  | FinalizeErrors

and ('v, 'e) m_instruction = ('v, 'e) instruction Pos.marked

type ('v, 'e) target = {
  target_name : string Pos.marked;
  target_file : string option;
  target_apps : string Pos.marked StrMap.t;
  target_args : 'v list;
  target_result : 'v option;
  target_tmp_vars : 'v StrMap.t;
  target_nb_tmps : int;
  target_sz_tmps : int;
  target_nb_refs : int;
  target_prog : ('v, 'e) m_instruction list;
}

let target_is_function t = t.target_result <> None

let rec access_map_var f = function
  | VarAccess (m_sp_opt, v) -> VarAccess (m_sp_opt, f v)
  | TabAccess (m_sp_opt, v, m_i) ->
      let v' = f v in
      let m_i' = m_expr_map_var f m_i in
      TabAccess (m_sp_opt, v', m_i')
  | FieldAccess (m_sp_opt, m_i, field, id) ->
      let m_i' = m_expr_map_var f m_i in
      FieldAccess (m_sp_opt, m_i', field, id)

and m_access_map_var f m_access = Pos.map (access_map_var f) m_access

and set_value_map_var f = function
  | FloatValue value -> FloatValue value
  | VarValue m_access ->
      let m_access' = m_access_map_var f m_access in
      VarValue m_access'
  | IntervalValue (i0, i1) -> IntervalValue (i0, i1)

and atom_map_var f = function
  | AtomVar v -> AtomVar (f v)
  | AtomLiteral l -> AtomLiteral l

and m_atom_map_var f m_a = Pos.map (atom_map_var f) m_a

and set_value_loop_map_var f = function
  | Single m_a0 -> Single (m_atom_map_var f m_a0)
  | Range (m_a0, m_a1) ->
      let m_a0' = m_atom_map_var f m_a0 in
      let m_a1' = m_atom_map_var f m_a1 in
      Range (m_a0', m_a1')
  | Interval (m_a0, m_a1) ->
      let m_a0' = m_atom_map_var f m_a0 in
      let m_a1' = m_atom_map_var f m_a1 in
      Interval (m_a0', m_a1')

and loop_variable_map_var f (m_ch, svl) =
  let svl' = List.map (set_value_loop_map_var f) svl in
  (m_ch, svl')

and loop_variables_map_var f = function
  | ValueSets lvl -> ValueSets (List.map (loop_variable_map_var f) lvl)
  | Ranges lvl -> Ranges (List.map (loop_variable_map_var f) lvl)

and expr_map_var f = function
  | TestInSet (positive, m_e0, values) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let values' = List.map (set_value_map_var f) values in
      TestInSet (positive, m_e0', values')
  | Unop (op, m_e0) -> Unop (op, m_expr_map_var f m_e0)
  | Comparison (op, m_e0, m_e1) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let m_e1' = m_expr_map_var f m_e1 in
      Comparison (op, m_e0', m_e1')
  | Binop (op, m_e0, m_e1) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let m_e1' = m_expr_map_var f m_e1 in
      Binop (op, m_e0', m_e1')
  | Conditional (m_e0, m_e1, m_e2_opt) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let m_e1' = m_expr_map_var f m_e1 in
      let m_e2_opt' = Option.map (m_expr_map_var f) m_e2_opt in
      Conditional (m_e0', m_e1', m_e2_opt')
  | FuncCall (fn, m_el) ->
      let m_el' = List.map (m_expr_map_var f) m_el in
      FuncCall (fn, m_el')
  | FuncCallLoop (fn, m_loop, m_e0) ->
      let m_loop' = Pos.map (loop_variables_map_var f) m_loop in
      let m_e0' = m_expr_map_var f m_e0 in
      FuncCallLoop (fn, m_loop', m_e0')
  | Literal l -> Literal l
  | Var access -> Var (access_map_var f access)
  | Loop (m_loop, m_e0) ->
      let m_loop' = Pos.map (loop_variables_map_var f) m_loop in
      let m_e0' = m_expr_map_var f m_e0 in
      Loop (m_loop', m_e0')
  | NbCategory cvm -> NbCategory cvm
  | Attribut (m_access, attr) ->
      let m_access' = m_access_map_var f m_access in
      Attribut (m_access', attr)
  | Size m_access -> Size (m_access_map_var f m_access)
  | IsVariable (m_access, name) ->
      let m_access' = m_access_map_var f m_access in
      IsVariable (m_access', name)
  | NbAnomalies -> NbAnomalies
  | NbDiscordances -> NbDiscordances
  | NbInformatives -> NbInformatives
  | NbBloquantes -> NbBloquantes

and m_expr_map_var f e = Pos.map (expr_map_var f) e

let rec print_arg_map_var f = function
  | PrintString s -> PrintString s
  | PrintAccess (info, m_a) -> PrintAccess (info, m_access_map_var f m_a)
  | PrintIndent m_e0 -> PrintIndent (m_expr_map_var f m_e0)
  | PrintExpr (m_e0, i0, i1) -> PrintExpr (m_expr_map_var f m_e0, i0, i1)

and formula_loop_map_var f m_lvs = Pos.map (loop_variables_map_var f) m_lvs

and formula_decl_map_var f = function
  | VarDecl (m_access, m_e1) ->
      let m_access' = m_access_map_var f m_access in
      let m_e1' = m_expr_map_var f m_e1 in
      VarDecl (m_access', m_e1')
  | EventFieldRef (m_e0, m_if, id, v) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let v' = f v in
      EventFieldRef (m_e0', m_if, id, v')

and formula_map_var f = function
  | SingleFormula fd -> SingleFormula (formula_decl_map_var f fd)
  | MultipleFormulaes (fl, fd) ->
      let fl' = formula_loop_map_var f fl in
      let fd' = formula_decl_map_var f fd in
      MultipleFormulaes (fl', fd')

and instr_map_var f g = function
  | Affectation m_f -> Affectation (Pos.map (formula_map_var f) m_f)
  | IfThenElse (m_e0, m_il0, m_il1) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let m_il0' = List.map (m_instr_map_var f g) m_il0 in
      let m_il1' = List.map (m_instr_map_var f g) m_il1 in
      IfThenElse (m_e0', m_il0', m_il1')
  | WhenDoElse (m_eil, m_il) ->
      let map (m_e0, m_il0, pos) =
        let m_e0' = m_expr_map_var f m_e0 in
        let m_il0' = List.map (m_instr_map_var f g) m_il0 in
        (m_e0', m_il0', pos)
      in
      let m_eil' = List.map map m_eil in
      let m_il' = Pos.map (List.map (m_instr_map_var f g)) m_il in
      WhenDoElse (m_eil', m_il')
  | ComputeDomain (dom, m_sp_opt) -> ComputeDomain (dom, m_sp_opt)
  | ComputeChaining (ch, m_sp_opt) -> ComputeChaining (ch, m_sp_opt)
  | ComputeVerifs (m_sl, m_e0, m_sp_opt) ->
      let m_e0' = m_expr_map_var f m_e0 in
      ComputeVerifs (m_sl, m_e0', m_sp_opt)
  | ComputeTarget (tn, args, m_sp_opt) ->
      let args' = List.map (m_access_map_var f) args in
      ComputeTarget (tn, args', m_sp_opt)
  | VerifBlock m_il0 -> VerifBlock (List.map (m_instr_map_var f g) m_il0)
  | Print (pr_std, pr_args) ->
      let pr_args' = List.map (Pos.map (print_arg_map_var f)) pr_args in
      Print (pr_std, pr_args')
  | Iterate (v, al, cvml, m_il) ->
      let v' = f v in
      let al' = List.map (m_access_map_var f) al in
      let cvml' =
        let map (cvm, m_e, m_sp_opt) = (cvm, m_expr_map_var f m_e, m_sp_opt) in
        List.map map cvml
      in
      let m_il' = List.map (m_instr_map_var f g) m_il in
      Iterate (v', al', cvml', m_il')
  | Iterate_values (v, e3l, m_il) ->
      let v' = f v in
      let e3l' =
        let map (m_e0, m_e1, m_e2) =
          let m_e0' = m_expr_map_var f m_e0 in
          let m_e1' = m_expr_map_var f m_e1 in
          let m_e2' = m_expr_map_var f m_e2 in
          (m_e0', m_e1', m_e2')
        in
        List.map map e3l
      in
      let m_il' = List.map (m_instr_map_var f g) m_il in
      Iterate_values (v', e3l', m_il')
  | Restore (al, cvml, el, vel, m_il) ->
      let al' = List.map (m_access_map_var f) al in
      let cvml' =
        let map (v, cvm, m_e0, m_sp_opt) =
          let v' = f v in
          let m_e0' = m_expr_map_var f m_e0 in
          (v', cvm, m_e0', m_sp_opt)
        in
        List.map map cvml
      in
      let el' = List.map (m_expr_map_var f) el in
      let vel' =
        let map (v, m_e0) =
          let v' = f v in
          let m_e0' = m_expr_map_var f m_e0 in
          (v', m_e0')
        in
        List.map map vel
      in
      let m_il' = List.map (m_instr_map_var f g) m_il in
      Restore (al', cvml', el', vel', m_il')
  | ArrangeEvents (vve_opt, ve_opt, e_opt, m_il) ->
      let vve_opt' =
        let map (v0, v1, m_e0) =
          let v0' = f v0 in
          let v1' = f v1 in
          let m_e0' = m_expr_map_var f m_e0 in
          (v0', v1', m_e0')
        in
        Option.map map vve_opt
      in
      let ve_opt' =
        let map (v, m_e0) =
          let v' = f v in
          let m_e0' = m_expr_map_var f m_e0 in
          (v', m_e0')
        in
        Option.map map ve_opt
      in
      let e_opt' = Option.map (m_expr_map_var f) e_opt in
      let m_il' = List.map (m_instr_map_var f g) m_il in
      ArrangeEvents (vve_opt', ve_opt', e_opt', m_il')
  | RaiseError (m_err, m_s_opt) ->
      let m_err' = Pos.map g m_err in
      RaiseError (m_err', m_s_opt)
  | CleanErrors -> CleanErrors
  | ExportErrors -> ExportErrors
  | FinalizeErrors -> FinalizeErrors

and m_instr_map_var f g m_i = Pos.map (instr_map_var f g) m_i

type var_usage = Read | Write | Info | DeclRef | ArgRef | DeclLocal | Macro

let fold_list fold l acc = List.fold_left (fun a e -> fold e a) acc l

let fold_opt fold opt acc = match opt with Some e -> fold e acc | None -> acc

let rec access_fold_var usage f a acc =
  match a with
  | VarAccess (m_sp_opt, v) -> acc |> f usage m_sp_opt (Some v)
  | TabAccess (m_sp_opt, v, m_i) ->
      acc |> f usage m_sp_opt (Some v) |> m_expr_fold_var f m_i
  | FieldAccess (m_sp_opt, m_i, _, _) ->
      acc |> f usage m_sp_opt None |> m_expr_fold_var f m_i

and m_access_fold_var usage f m_access acc =
  acc |> access_fold_var usage f (Pos.unmark m_access)

and set_value_fold_var f sv acc =
  match sv with
  | FloatValue _ -> acc
  | VarValue m_access -> acc |> m_access_fold_var Read f m_access
  | IntervalValue _ -> acc

and atom_fold_var f a acc =
  match a with
  | AtomVar v -> acc |> f Macro None (Some v)
  | AtomLiteral _ -> acc

and m_atom_fold_var f m_a acc = acc |> atom_fold_var f (Pos.unmark m_a)

and set_value_loop_fold_var f svl acc =
  match svl with
  | Single m_a0 -> acc |> m_atom_fold_var f m_a0
  | Range (m_a0, m_a1) ->
      acc |> m_atom_fold_var f m_a0 |> m_atom_fold_var f m_a1
  | Interval (m_a0, m_a1) ->
      acc |> m_atom_fold_var f m_a0 |> m_atom_fold_var f m_a1

and loop_variable_fold_var f (_, svl) acc =
  fold_list (set_value_loop_fold_var f) svl acc

and loop_variables_fold_var f lv acc =
  match lv with
  | ValueSets lvl -> fold_list (loop_variable_fold_var f) lvl acc
  | Ranges lvl -> fold_list (loop_variable_fold_var f) lvl acc

and expr_fold_var f e acc =
  match e with
  | TestInSet (_, m_e0, values) ->
      acc |> m_expr_fold_var f m_e0 |> fold_list (set_value_fold_var f) values
  | Unop (_, m_e0) -> m_expr_fold_var f m_e0 acc
  | Comparison (_, m_e0, m_e1) ->
      acc |> m_expr_fold_var f m_e0 |> m_expr_fold_var f m_e1
  | Binop (_, m_e0, m_e1) ->
      acc |> m_expr_fold_var f m_e0 |> m_expr_fold_var f m_e1
  | Conditional (m_e0, m_e1, m_e2_opt) ->
      acc |> m_expr_fold_var f m_e0 |> m_expr_fold_var f m_e1
      |> fold_opt (m_expr_fold_var f) m_e2_opt
  | FuncCall (_, m_el) -> fold_list (m_expr_fold_var f) m_el acc
  | FuncCallLoop (_, m_loop, m_e0) ->
      acc
      |> loop_variables_fold_var f (Pos.unmark m_loop)
      |> m_expr_fold_var f m_e0
  | Literal _ -> acc
  | Var access -> access_fold_var Read f access acc
  | Loop (m_loop, m_e0) ->
      acc
      |> loop_variables_fold_var f (Pos.unmark m_loop)
      |> m_expr_fold_var f m_e0
  | NbCategory _ -> acc
  | Attribut (m_access, _) -> m_access_fold_var Info f m_access acc
  | Size m_access -> m_access_fold_var Info f m_access acc
  | IsVariable (m_access, _) -> m_access_fold_var Info f m_access acc
  | NbAnomalies -> acc
  | NbDiscordances -> acc
  | NbInformatives -> acc
  | NbBloquantes -> acc

and m_expr_fold_var f e acc = expr_fold_var f (Pos.unmark e) acc

let rec print_arg_fold_var f pa acc =
  match pa with
  | PrintString _ -> acc
  | PrintAccess (_, m_a) -> m_access_fold_var Info f m_a acc
  | PrintIndent m_e0 -> m_expr_fold_var f m_e0 acc
  | PrintExpr (m_e0, _, _) -> m_expr_fold_var f m_e0 acc

and m_print_arg_fold_var f m_pa acc = print_arg_fold_var f (Pos.unmark m_pa) acc

and formula_loop_fold_var f m_lvs acc =
  loop_variables_fold_var f (Pos.unmark m_lvs) acc

and formula_decl_fold_var f fd acc =
  match fd with
  | VarDecl (m_access, m_e1) ->
      acc |> m_access_fold_var Write f m_access |> m_expr_fold_var f m_e1
  | EventFieldRef (m_e0, _, _, v) ->
      acc |> m_expr_fold_var f m_e0 |> f ArgRef None (Some v)

and formula_fold_var f fm acc =
  match fm with
  | SingleFormula fd -> formula_decl_fold_var f fd acc
  | MultipleFormulaes (fl, fd) ->
      acc |> formula_loop_fold_var f fl |> formula_decl_fold_var f fd

and instr_fold_var f instr acc =
  match instr with
  | Affectation m_f -> formula_fold_var f (Pos.unmark m_f) acc
  | IfThenElse (m_e0, m_il0, m_il1) ->
      acc |> m_expr_fold_var f m_e0
      |> fold_list (m_instr_fold_var f) m_il0
      |> fold_list (m_instr_fold_var f) m_il1
  | WhenDoElse (m_eil, m_il) ->
      let fold (m_e0, m_il0, _) accu =
        accu |> m_expr_fold_var f m_e0 |> fold_list (m_instr_fold_var f) m_il0
      in
      acc |> fold_list fold m_eil
      |> fold_list (m_instr_fold_var f) (Pos.unmark m_il)
  | ComputeDomain _ -> acc
  | ComputeChaining _ -> acc
  | ComputeVerifs (_, m_e0, _) -> m_expr_fold_var f m_e0 acc
  | ComputeTarget (_, args, _) ->
      fold_list (m_access_fold_var ArgRef f) args acc
  | VerifBlock m_il0 -> fold_list (m_instr_fold_var f) m_il0 acc
  | Print (_, pr_args) -> fold_list (m_print_arg_fold_var f) pr_args acc
  | Iterate (v, al, cvml, m_il) ->
      acc |> f DeclRef None (Some v)
      |> fold_list (m_access_fold_var ArgRef f) al
      |> (let fold (_, m_e, m_sp_opt) accu =
            accu |> f DeclRef m_sp_opt (Some v) |> m_expr_fold_var f m_e
          in
          fold_list fold cvml)
      |> fold_list (m_instr_fold_var f) m_il
  | Iterate_values (v, e3l, m_il) ->
      acc |> f DeclLocal None (Some v)
      |> (let fold (m_e0, m_e1, m_e2) accu =
            accu |> m_expr_fold_var f m_e0 |> m_expr_fold_var f m_e1
            |> m_expr_fold_var f m_e2
          in
          fold_list fold e3l)
      |> fold_list (m_instr_fold_var f) m_il
  | Restore (al, cvml, el, vel, m_il) ->
      acc
      |> fold_list (m_access_fold_var ArgRef f) al
      |> (let fold (v, _, m_e0, m_sp_opt) accu =
            accu |> f DeclRef m_sp_opt (Some v) |> m_expr_fold_var f m_e0
          in
          fold_list fold cvml)
      |> fold_list (m_expr_fold_var f) el
      |> (let fold (v, m_e0) accu =
            accu |> f DeclLocal None (Some v) |> m_expr_fold_var f m_e0
          in
          fold_list fold vel)
      |> fold_list (m_instr_fold_var f) m_il
  | ArrangeEvents (vve_opt, ve_opt, e_opt, m_il) ->
      acc
      |> (let fold (v0, v1, m_e0) accu =
            accu |> f DeclLocal None (Some v0) |> f DeclLocal None (Some v1)
            |> m_expr_fold_var f m_e0
          in
          fold_opt fold vve_opt)
      |> (let fold (v, m_e0) accu =
            accu |> f DeclLocal None (Some v) |> m_expr_fold_var f m_e0
          in
          fold_opt fold ve_opt)
      |> fold_opt (m_expr_fold_var f) e_opt
      |> fold_list (m_instr_fold_var f) m_il
  | RaiseError _ -> acc
  | CleanErrors -> acc
  | ExportErrors -> acc
  | FinalizeErrors -> acc

and m_instr_fold_var f m_i acc = instr_fold_var f (Pos.unmark m_i) acc

let get_var_name v = match v with Normal s -> s | Generic s -> s.base

let get_normal_var = function Normal name -> name | Generic _ -> assert false

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
  | AtomLiteral l -> format_literal fmt l.lit

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
    | Div -> "/"
    | Mod -> "%")

let format_comp_op fmt op =
  Format.pp_print_string fmt
    (match op with
    | Gt -> ">"
    | Gte -> ">="
    | Lt -> "<"
    | Lte -> "<="
    | Eq -> "="
    | Neq -> "!=")

let format_access form_var form_expr fmt = function
  | VarAccess (m_sp_opt, v) ->
      let sp_str =
        match m_sp_opt with
        | None -> ""
        | Some (m_sp, _) -> get_var_name (Pos.unmark m_sp) ^ "."
      in
      Pp.fpr fmt "%s%a" sp_str form_var v
  | TabAccess (m_sp_opt, v, m_i) ->
      let sp_str =
        match m_sp_opt with
        | None -> ""
        | Some (m_sp, _) -> get_var_name (Pos.unmark m_sp) ^ "."
      in
      Pp.fpr fmt "%s%a[%a]" sp_str form_var v form_expr (Pos.unmark m_i)
  | FieldAccess (m_sp_opt, e, f, _) ->
      let sp_str =
        match m_sp_opt with
        | None -> ""
        | Some (m_sp, _) -> get_var_name (Pos.unmark m_sp) ^ "."
      in
      Pp.fpr fmt "%schamp_evenement(%a, %s)" sp_str form_expr (Pos.unmark e)
        (Pos.unmark f)

let format_set_value form_var form_expr fmt sv =
  match sv with
  | FloatValue i -> Pp.fpr fmt "%f" (Pos.unmark i)
  | VarValue m_acc -> format_access form_var form_expr fmt (Pos.unmark m_acc)
  | IntervalValue (i1, i2) ->
      Pp.fpr fmt "%d..%d" (Pos.unmark i1) (Pos.unmark i2)

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
    | NbEvents -> "nb_evenements"
    | Func fn -> fn)

let rec format_expression form_var fmt =
  let form_expr = format_expression form_var in
  function
  | TestInSet (belong, e, values) ->
      Format.fprintf fmt "(%a %sdans %a)" form_expr (Pos.unmark e)
        (if belong then "" else "non ")
        (Pp.list_comma (format_set_value form_var form_expr))
        values
  | Comparison (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" form_expr (Pos.unmark e1) format_comp_op
        (Pos.unmark op) form_expr (Pos.unmark e2)
  | Binop (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" form_expr (Pos.unmark e1) format_binop
        (Pos.unmark op) form_expr (Pos.unmark e2)
  | Unop (op, e) ->
      Format.fprintf fmt "%a %a" format_unop op form_expr (Pos.unmark e)
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
  | Literal { lit; _ } -> format_literal fmt lit
  | Var acc -> format_access form_var form_expr fmt acc
  | Loop (lvs, e) ->
      Format.fprintf fmt "pour %a%a"
        (format_loop_variables form_var)
        (Pos.unmark lvs) form_expr (Pos.unmark e)
  | NbCategory cs ->
      Format.fprintf fmt "nb_categorie(%a)" (CatVar.Map.pp_keys ()) cs
  | Attribut (m_acc, a) ->
      Format.fprintf fmt "attribut(%a, %s)"
        (format_access form_var form_expr)
        (Pos.unmark m_acc) (Pos.unmark a)
  | Size m_acc ->
      Format.fprintf fmt "taille(%a)"
        (format_access form_var form_expr)
        (Pos.unmark m_acc)
  | IsVariable (m_acc, name) ->
      Format.fprintf fmt "est_variable(%a, %s)"
        (format_access form_var form_expr)
        (Pos.unmark m_acc) (Pos.unmark name)
  | NbAnomalies -> Format.fprintf fmt "nb_anomalies()"
  | NbDiscordances -> Format.fprintf fmt "nb_discordances()"
  | NbInformatives -> Format.fprintf fmt "nb_informatives()"
  | NbBloquantes -> Format.fprintf fmt "nb_bloquantes()"

let format_print_arg form_var fmt =
  let form_expr = format_expression form_var in
  function
  | PrintString s -> Format.fprintf fmt "\"%s\"" s
  | PrintAccess (info, m_a) ->
      let infoStr = match info with Name -> "nom" | Alias -> "alias" in
      Format.fprintf fmt "%s(%a)" infoStr
        (format_access form_var form_expr)
        (Pos.unmark m_a)
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

let format_formula_decl form_var fmt = function
  | VarDecl (m_access, e) ->
      format_access form_var
        (format_expression form_var)
        fmt (Pos.unmark m_access);
      Format.fprintf fmt " = %a" (format_expression form_var) (Pos.unmark e)
  | EventFieldRef (idx, f, _, v) ->
      Format.fprintf fmt "champ_evenement(%a,%s) reference %a"
        (format_expression form_var)
        (Pos.unmark idx) (Pos.unmark f) form_var v

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
  let form_access = format_access form_var form_expr in
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
        let pp_ed fmt (Pos.Mark (dl, _)) =
          Format.fprintf fmt "@[<v 2>else_do@\n%a@;@]endwhen@;" form_instrs dl
        in
        Format.fprintf fmt "%a%a@\n" pp_wdl wdl pp_ed ed
    | VerifBlock vb ->
        Format.fprintf fmt
          "@[<v 2># debut verif block@\n%a@]@\n# fin verif block@\n" form_instrs
          vb
    | ComputeDomain (l, m_sp_opt) ->
        let pp_sp fmt m_sp_opt =
          match m_sp_opt with
          | None -> ()
          | Some (m_sp, _) ->
              Pp.fpr fmt " : espace %s" (get_var_name (Pos.unmark m_sp))
        in
        Format.fprintf fmt "calculer domaine %a%a;"
          (Pp.list_space (Pp.unmark Pp.string))
          (Pos.unmark l) pp_sp m_sp_opt
    | ComputeChaining (ch, m_sp_opt) ->
        let pp_sp fmt m_sp_opt =
          match m_sp_opt with
          | None -> ()
          | Some (m_sp, _) ->
              Pp.fpr fmt " : espace %s" (get_var_name (Pos.unmark m_sp))
        in
        Format.fprintf fmt "calculer enchaineur %s%a;" (Pos.unmark ch) pp_sp
          m_sp_opt
    | ComputeVerifs (l, expr, m_sp_opt) ->
        let pp_sp fmt m_sp_opt =
          match m_sp_opt with
          | None -> ()
          | Some (m_sp, _) ->
              Pp.fpr fmt " : espace %s" (get_var_name (Pos.unmark m_sp))
        in
        Format.fprintf fmt "verifier %a%a : avec %a;"
          (Pp.list_space (Pp.unmark Pp.string))
          (Pos.unmark l) (Pp.unmark form_expr) expr pp_sp m_sp_opt
    | ComputeTarget (tname, targs, m_sp_opt) ->
        let pp_sp fmt m_sp_opt =
          match m_sp_opt with
          | None -> ()
          | Some (m_sp, _) ->
              Pp.fpr fmt " : espace %s" (get_var_name (Pos.unmark m_sp))
        in
        let pp_args fmt = function
          | [] -> ()
          | args ->
              let pp_m_access fmt m_a =
                format_access form_var form_expr fmt (Pos.unmark m_a)
              in
              Pp.list_comma pp_m_access fmt args
        in
        Format.fprintf fmt "calculer cible %s%a%a@," (Pos.unmark tname) pp_args
          targs pp_sp m_sp_opt
    | Print (std, args) ->
        let print_cmd =
          match std with StdOut -> "afficher" | StdErr -> "afficher_erreur"
        in
        Format.fprintf fmt "%s %a;" print_cmd
          (Pp.list_space (Pp.unmark (format_print_arg form_var)))
          args
    | Iterate (var, al, var_params, itb) ->
        let form_alist fmt = function
          | [] -> ()
          | al ->
              let form = Pp.list_comma @@ Pp.unmark form_access in
              Format.fprintf fmt "@;: %a" form al
        in
        let format_var_param fmt (vcs, expr, m_sp_opt) =
          let sp_str =
            match m_sp_opt with
            | None -> ""
            | Some (m_sp, _) -> " : espace " ^ get_var_name (Pos.unmark m_sp)
          in
          Format.fprintf fmt ": categorie %a : avec %a%s@\n"
            (CatVar.Map.pp_keys ()) vcs form_expr (Pos.unmark expr) sp_str
        in
        Format.fprintf fmt "iterate variable %a@;: %a@;: %a@;: dans (" form_var
          var form_alist al
          (Pp.list_space format_var_param)
          var_params;
        Format.fprintf fmt "@[<h 2>  %a@]@\n)@\n" form_instrs itb
    | Iterate_values (var, var_intervals, itb) ->
        let format_var_intervals fmt (e0, e1, step) =
          Format.fprintf fmt ": entre %a .. %a increment %a@\n" form_expr
            (Pos.unmark e0) form_expr (Pos.unmark e1) form_expr
            (Pos.unmark step)
        in
        Format.fprintf fmt "iterate variable %a@;: %a@;: dans (" form_var var
          (Pp.list_space format_var_intervals)
          var_intervals;
        Format.fprintf fmt "@[<h 2>  %a@]@\n)@\n" form_instrs itb
    | Restore (al, var_params, evts, evtfs, rb) ->
        let form_alist fmt = function
          | [] -> ()
          | al ->
              let form = Pp.list_comma @@ Pp.unmark form_access in
              Format.fprintf fmt "@;: variables %a" form al
        in
        let format_var_param fmt (var, vcs, expr, m_sp_opt) =
          let sp_str =
            match m_sp_opt with
            | None -> ""
            | Some (m_sp, _) -> " : espace " ^ get_var_name (Pos.unmark m_sp)
          in
          Format.fprintf fmt "@;: variable %a : categorie %a : avec %a%s"
            form_var var (CatVar.Map.pp_keys ()) vcs form_expr (Pos.unmark expr)
            sp_str
        in
        let format_var_params fmt = function
          | [] -> ()
          | var_params -> Pp.list "" format_var_param fmt var_params
        in
        let format_evts fmt = function
          | [] -> ()
          | evts ->
              Format.fprintf fmt "@;: evenements %a"
                (Pp.list_comma (Pp.unmark form_expr))
                evts
        in
        let format_evtfs fmt = function
          | [] -> ()
          | evtfs ->
              List.iter
                (fun (v, e) ->
                  Format.fprintf fmt "@;: evenement %a : avec %a" form_var v
                    (Pp.unmark form_expr) e)
                evtfs
        in
        Format.fprintf fmt "restaure%a%a%a%a@;: apres (" form_alist al
          format_var_params var_params format_evts evts format_evtfs evtfs;
        Format.fprintf fmt "@[<h 2>  %a@]@;)@;" form_instrs rb
    | ArrangeEvents (s, f, a, itb) ->
        Format.fprintf fmt "arrange_evenements@;:";
        (match s with
        | Some (v0, v1, e) ->
            Format.fprintf fmt "trier %a,%a : avec %a@;" form_var v0 form_var v1
              form_expr (Pos.unmark e)
        | None -> ());
        (match f with
        | Some (v, e) ->
            Format.fprintf fmt "filter %a : avec %a@;" form_var v form_expr
              (Pos.unmark e)
        | None -> ());
        (match a with
        | Some e -> Format.fprintf fmt "ajouter %a@;" form_expr (Pos.unmark e)
        | None -> ());
        Format.fprintf fmt ": dans (@[<h 2>  %a@]@\n)@\n" form_instrs itb
    | RaiseError (err, var_opt) ->
        Format.fprintf fmt "leve_erreur %a %s\n" form_err (Pos.unmark err)
          (match var_opt with Some var -> " " ^ Pos.unmark var | None -> "")
    | CleanErrors -> Format.fprintf fmt "nettoie_erreurs\n"
    | ExportErrors -> Format.fprintf fmt "exporte_erreurs\n"
    | FinalizeErrors -> Format.fprintf fmt "finalise_erreurs\n"

and format_instructions form_var form_err fmt instrs =
  Pp.list "" (Pp.unmark (format_instruction form_var form_err)) fmt instrs
