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
    is_table : t Array.t option;
    alias : string Pos.marked option;  (** Input variable have an alias *)
    descr : string Pos.marked;
        (** Description taken from the variable declaration *)
    attrs : int Pos.marked StrMap.t;
    cat : CatVar.t;
    is_given_back : bool;
    typ : value_typ option;
  }

  and scope = Tgv of tgv | Temp of t Array.t option | Ref | Arg | Res

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

  let is_table v =
    match v.scope with
    | Tgv tgv -> tgv.is_table
    | Temp is_table -> is_table
    | Ref | Arg | Res -> None

  let set_is_table v is_table =
    match v.scope with
    | Tgv tgv -> { v with scope = Tgv { tgv with is_table } }
    | Temp _ -> { v with scope = Temp is_table }
    | Ref | Arg | Res -> v

  let cat_var_loc v =
    match v.scope with
    | Tgv tgv -> (
        match tgv.cat with
        | CatVar.Input _ -> Some CatVar.LocInput
        | Computed { is_base } when is_base -> Some CatVar.LocBase
        | Computed _ -> Some CatVar.LocComputed)
    | Temp _ | Ref | Arg | Res -> None

  let size v = match is_table v with None -> 1 | Some tab -> Array.length tab

  let alias v = match v.scope with Tgv s -> s.alias | _ -> None

  let alias_str v =
    match v.scope with
    | Tgv s -> Option.fold ~none:"" ~some:Pos.unmark s.alias
    | _ -> ""

  let descr v = (tgv v).descr

  let descr_str v = Pos.unmark (tgv v).descr

  let attrs v = (tgv v).attrs

  let cat v = (tgv v).cat

  let is_given_back v = (tgv v).is_given_back

  let loc_tgv v =
    match v.loc with
    | LocTgv (_, l) -> l
    | _ ->
        let msg = Pp.spr "%s is not a TGV variable" (Pos.unmark v.name) in
        Errors.raise_error msg

  let set_loc_tgv_cat v (cv : CatVar.data) i =
    match v.loc with
    | LocTgv (id, tgv) ->
        let loc_cat = cv.loc in
        let loc_cat_str = cv.id_str in
        let tgv = { tgv with loc_cat; loc_cat_str; loc_cat_idx = i } in
        { v with loc = LocTgv (id, tgv) }
    | LocTmp (id, _) | LocRef (id, _) | LocArg (id, _) | LocRes id ->
        Errors.raise_error (Pp.spr "%s has not a TGV location" id)

  let set_loc_tgv_idx v i =
    match v.loc with
    | LocTgv (id, tgv) -> { v with loc = LocTgv (id, { tgv with loc_idx = i }) }
    | LocTmp (id, _) | LocRef (id, _) | LocArg (id, _) | LocRes id ->
        Errors.raise_error (Pp.spr "%s has not a TGV location" id)

  let loc_int v =
    match v.loc with
    | LocTgv (_, tgv) -> tgv.loc_int
    | LocTmp (_, li) | LocRef (_, li) | LocArg (_, li) -> li
    | LocRes id ->
        let msg = Pp.spr "variable %s doesn't have an index" id in
        Errors.raise_error msg

  let set_loc_int v loc_int =
    let loc =
      match v.loc with
      | LocTgv (id, tgv) -> LocTgv (id, { tgv with loc_int })
      | LocTmp (id, _) -> LocTmp (id, loc_int)
      | LocRef (id, _) -> LocRef (id, loc_int)
      | LocArg (id, _) -> LocArg (id, loc_int)
      | LocRes id ->
          let msg = Pp.spr "variable %s doesn't have an index" id in
          Errors.raise_error msg
    in
    { v with loc }

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

  let new_tgv ~(name : string Pos.marked) ~(is_table : t Array.t option)
      ~(is_given_back : bool) ~(alias : string Pos.marked option)
      ~(descr : string Pos.marked) ~(attrs : int Pos.marked StrMap.t)
      ~(cat : CatVar.t) ~(typ : value_typ option) : t =
    {
      name;
      id = new_id ();
      loc = LocTgv (Pos.unmark name, init_loc cat);
      scope = Tgv { is_table; alias; descr; attrs; cat; is_given_back; typ };
    }

  let new_temp ~(name : string Pos.marked) ~(is_table : t Array.t option)
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

type literal = Float of float | Undefined

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

type 'v access =
  | VarAccess of 'v
  | TabAccess of 'v Pos.marked * 'v m_expression
  | ConcAccess of m_var_name * string Pos.marked * 'v m_expression
  | FieldAccess of 'v m_expression * string Pos.marked * int

and 'v m_access = 'v access Pos.marked

and 'v atom = AtomVar of 'v | AtomLiteral of literal

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
  | Index of 'v m_access * 'v m_expression
  | Conditional of 'v m_expression * 'v m_expression * 'v m_expression option
  | FuncCall of func Pos.marked * 'v m_expression list
  | FuncCallLoop of
      func Pos.marked * 'v loop_variables Pos.marked * 'v m_expression
  | Literal of literal
  | Var of 'v access
  | Loop of 'v loop_variables Pos.marked * 'v m_expression
      (** The loop is prefixed with the loop variables declarations *)
  | NbCategory of Pos.t CatVar.Map.t
  | Attribut of 'v m_access * string Pos.marked
  | Size of 'v m_access
  | NbAnomalies
  | NbDiscordances
  | NbInformatives
  | NbBloquantes

and 'v m_expression = 'v expression Pos.marked

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

type 'v print_arg =
  | PrintString of string
  | PrintName of 'v Pos.marked
  | PrintAlias of 'v Pos.marked
  | PrintConcName of m_var_name * string Pos.marked * 'v m_expression
  | PrintConcAlias of m_var_name * string Pos.marked * 'v m_expression
  | PrintEventName of 'v m_expression * string Pos.marked * int
  | PrintEventAlias of 'v m_expression * string Pos.marked * int
  | PrintIndent of 'v m_expression
  | PrintExpr of 'v m_expression * int * int

type 'v formula_loop = 'v loop_variables Pos.marked

type 'v formula_decl =
  | VarDecl of 'v access Pos.marked * 'v m_expression option * 'v m_expression
  | EventFieldRef of 'v m_expression * string Pos.marked * int * 'v Pos.marked

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
  | Iterate_values of
      'v Pos.marked
      * ('v m_expression * 'v m_expression * 'v m_expression) list
      * ('v, 'e) m_instruction list
  | Restore of
      'v Pos.marked list
      * ('v Pos.marked * Pos.t CatVar.Map.t * 'v m_expression) list
      * 'v m_expression list
      * ('v Pos.marked * 'v m_expression) list
      * ('v, 'e) m_instruction list
  | ArrangeEvents of
      ('v Pos.marked * 'v Pos.marked * 'v m_expression) option
      * ('v Pos.marked * 'v m_expression) option
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
  target_args : Var.t list;
  target_result : Var.t option;
  target_tmp_vars : Var.t StrMap.t;
  target_nb_tmps : int;
  target_sz_tmps : int;
  target_nb_refs : int;
  target_prog : ('v, 'e) m_instruction list;
}

let rec access_map_var f = function
  | VarAccess v -> VarAccess (f v)
  | TabAccess (m_v, m_i) ->
      let m_v' = Pos.map_under_mark f m_v in
      let m_i' = m_expr_map_var f m_i in
      TabAccess (m_v', m_i')
  | ConcAccess (vname, m_ifmt, m_i) ->
      let m_i' = m_expr_map_var f m_i in
      ConcAccess (vname, m_ifmt, m_i')
  | FieldAccess (m_i, field, id) ->
      let m_i' = m_expr_map_var f m_i in
      FieldAccess (m_i', field, id)

and m_access_map_var f m_access = Pos.map_under_mark (access_map_var f) m_access

and set_value_map_var f = function
  | FloatValue value -> FloatValue value
  | VarValue m_access ->
      let m_access' = m_access_map_var f m_access in
      VarValue m_access'
  | IntervalValue (i0, i1) -> IntervalValue (i0, i1)

and atom_map_var f = function
  | AtomVar v -> AtomVar (f v)
  | AtomLiteral l -> AtomLiteral l

and m_atom_map_var f m_a = Pos.map_under_mark (atom_map_var f) m_a

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
  | Index (m_access, m_idx) ->
      let m_access' = m_access_map_var f m_access in
      let m_idx' = m_expr_map_var f m_idx in
      Index (m_access', m_idx')
  | Conditional (m_e0, m_e1, m_e2_opt) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let m_e1' = m_expr_map_var f m_e1 in
      let m_e2_opt' = Option.map (m_expr_map_var f) m_e2_opt in
      Conditional (m_e0', m_e1', m_e2_opt')
  | FuncCall (fn, m_el) ->
      let m_el' = List.map (m_expr_map_var f) m_el in
      FuncCall (fn, m_el')
  | FuncCallLoop (fn, m_loop, m_e0) ->
      let m_loop' = Pos.map_under_mark (loop_variables_map_var f) m_loop in
      let m_e0' = m_expr_map_var f m_e0 in
      FuncCallLoop (fn, m_loop', m_e0')
  | Literal l -> Literal l
  | Var access -> Var (access_map_var f access)
  | Loop (m_loop, m_e0) ->
      let m_loop' = Pos.map_under_mark (loop_variables_map_var f) m_loop in
      let m_e0' = m_expr_map_var f m_e0 in
      Loop (m_loop', m_e0')
  | NbCategory cvm -> NbCategory cvm
  | Attribut (m_access, attr) ->
      let m_access' = Pos.map_under_mark (access_map_var f) m_access in
      Attribut (m_access', attr)
  | Size m_access -> Size (Pos.map_under_mark (access_map_var f) m_access)
  | NbAnomalies -> NbAnomalies
  | NbDiscordances -> NbDiscordances
  | NbInformatives -> NbInformatives
  | NbBloquantes -> NbBloquantes

and m_expr_map_var f e = Pos.map_under_mark (expr_map_var f) e

let rec print_arg_map_var f = function
  | PrintString s -> PrintString s
  | PrintName m_v -> PrintName (Pos.map_under_mark f m_v)
  | PrintAlias m_v -> PrintAlias (Pos.map_under_mark f m_v)
  | PrintConcName (m_vn, m_if, m_e0) ->
      let m_e0' = m_expr_map_var f m_e0 in
      PrintConcName (m_vn, m_if, m_e0')
  | PrintConcAlias (m_vn, m_if, m_e0) ->
      let m_e0' = m_expr_map_var f m_e0 in
      PrintConcAlias (m_vn, m_if, m_e0')
  | PrintEventName (m_idx, m_field, id) ->
      let m_idx' = m_expr_map_var f m_idx in
      PrintEventName (m_idx', m_field, id)
  | PrintEventAlias (m_idx, m_field, id) ->
      let m_idx' = m_expr_map_var f m_idx in
      PrintEventAlias (m_idx', m_field, id)
  | PrintIndent m_e0 -> PrintIndent (m_expr_map_var f m_e0)
  | PrintExpr (m_e0, i0, i1) -> PrintExpr (m_expr_map_var f m_e0, i0, i1)

and formula_loop_map_var f m_lvs =
  Pos.map_under_mark (loop_variables_map_var f) m_lvs

and formula_decl_map_var f = function
  | VarDecl (m_access, m_e0_opt, m_e1) ->
      let m_access' = m_access_map_var f m_access in
      let m_e0_opt' = Option.map (m_expr_map_var f) m_e0_opt in
      let m_e1' = m_expr_map_var f m_e1 in
      VarDecl (m_access', m_e0_opt', m_e1')
  | EventFieldRef (m_e0, m_if, id, m_v) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let m_v' = Pos.map_under_mark f m_v in
      EventFieldRef (m_e0', m_if, id, m_v')

and formula_map_var f = function
  | SingleFormula fd -> SingleFormula (formula_decl_map_var f fd)
  | MultipleFormulaes (fl, fd) ->
      let fl' = formula_loop_map_var f fl in
      let fd' = formula_decl_map_var f fd in
      MultipleFormulaes (fl', fd')

and instr_map_var f g = function
  | Affectation m_f -> Affectation (Pos.map_under_mark (formula_map_var f) m_f)
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
      let m_il' = Pos.map_under_mark (List.map (m_instr_map_var f g)) m_il in
      WhenDoElse (m_eil', m_il')
  | ComputeDomain dom -> ComputeDomain dom
  | ComputeChaining ch -> ComputeChaining ch
  | ComputeVerifs (m_sl, m_e0) ->
      let m_e0' = m_expr_map_var f m_e0 in
      ComputeVerifs (m_sl, m_e0')
  | ComputeTarget (tn, args) ->
      let args' = List.map (Pos.map_under_mark f) args in
      ComputeTarget (tn, args')
  | VerifBlock m_il0 -> VerifBlock (List.map (m_instr_map_var f g) m_il0)
  | Print (pr_std, pr_args) ->
      let pr_args' =
        List.map (Pos.map_under_mark (print_arg_map_var f)) pr_args
      in
      Print (pr_std, pr_args')
  | Iterate (m_v, m_vl, cvml, m_il) ->
      let m_v' = Pos.map_under_mark f m_v in
      let m_vl' = List.map (Pos.map_under_mark f) m_vl in
      let cvml' =
        let map (cvm, m_e) = (cvm, m_expr_map_var f m_e) in
        List.map map cvml
      in
      let m_il' = List.map (m_instr_map_var f g) m_il in
      Iterate (m_v', m_vl', cvml', m_il')
  | Iterate_values (m_v, e3l, m_il) ->
      let m_v' = Pos.map_under_mark f m_v in
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
      Iterate_values (m_v', e3l', m_il')
  | Restore (m_vl, cvml, el, vel, m_il) ->
      let m_vl' = List.map (Pos.map_under_mark f) m_vl in
      let cvml' =
        let map (m_v, cvm, m_e0) =
          let m_v' = Pos.map_under_mark f m_v in
          let m_e0' = m_expr_map_var f m_e0 in
          (m_v', cvm, m_e0')
        in
        List.map map cvml
      in
      let el' = List.map (m_expr_map_var f) el in
      let vel' =
        let map (m_v, m_e0) =
          let m_v' = Pos.map_under_mark f m_v in
          let m_e0' = m_expr_map_var f m_e0 in
          (m_v', m_e0')
        in
        List.map map vel
      in
      let m_il' = List.map (m_instr_map_var f g) m_il in
      Restore (m_vl', cvml', el', vel', m_il')
  | ArrangeEvents (vve_opt, ve_opt, e_opt, m_il) ->
      let vve_opt' =
        let map (m_v0, m_v1, m_e0) =
          let m_v0' = Pos.map_under_mark f m_v0 in
          let m_v1' = Pos.map_under_mark f m_v1 in
          let m_e0' = m_expr_map_var f m_e0 in
          (m_v0', m_v1', m_e0')
        in
        Option.map map vve_opt
      in
      let ve_opt' =
        let map (m_v, m_e0) =
          let m_v' = Pos.map_under_mark f m_v in
          let m_e0' = m_expr_map_var f m_e0 in
          (m_v', m_e0')
        in
        Option.map map ve_opt
      in
      let e_opt' = Option.map (m_expr_map_var f) e_opt in
      let m_il' = List.map (m_instr_map_var f g) m_il in
      ArrangeEvents (vve_opt', ve_opt', e_opt', m_il')
  | RaiseError (m_err, m_s_opt) ->
      let m_err' = Pos.map_under_mark g m_err in
      RaiseError (m_err', m_s_opt)
  | CleanErrors -> CleanErrors
  | ExportErrors -> ExportErrors
  | FinalizeErrors -> FinalizeErrors

and m_instr_map_var f g m_i = Pos.map_under_mark (instr_map_var f g) m_i

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
  | VarAccess v -> form_var fmt v
  | TabAccess (m_v, m_i) ->
      Format.fprintf fmt "%a[%a]" form_var (Pos.unmark m_v) form_expr
        (Pos.unmark m_i)
  | ConcAccess (m_vn, m_idxf, idx) ->
      Format.fprintf fmt "%s{%s, %a}"
        (get_var_name (Pos.unmark m_vn))
        (Pos.unmark m_idxf) form_expr (Pos.unmark idx)
  | FieldAccess (e, f, _) ->
      Format.fprintf fmt "champ_evenement(%a, %s)" form_expr (Pos.unmark e)
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
  | Index (m_acc, i) ->
      Format.fprintf fmt "%a[%a]"
        (format_access form_var form_expr)
        (Pos.unmark m_acc) form_expr (Pos.unmark i)
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
  | NbAnomalies -> Format.fprintf fmt "nb_anomalies()"
  | NbDiscordances -> Format.fprintf fmt "nb_discordances()"
  | NbInformatives -> Format.fprintf fmt "nb_informatives()"
  | NbBloquantes -> Format.fprintf fmt "nb_bloquantes()"

let format_print_arg form_var fmt =
  let form_expr = format_expression form_var in
  function
  | PrintString s -> Format.fprintf fmt "\"%s\"" s
  | PrintName v -> Format.fprintf fmt "nom(%a)" (Pp.unmark form_var) v
  | PrintAlias v -> Format.fprintf fmt "alias(%a)" (Pp.unmark form_var) v
  | PrintConcName (m_vn, m_idxf, idx) ->
      Format.fprintf fmt "nom(%s{%s, %a})"
        (get_var_name (Pos.unmark m_vn))
        (Pos.unmark m_idxf) (Pp.unmark form_expr) idx
  | PrintConcAlias (m_vn, m_idxf, idx) ->
      Format.fprintf fmt "alias(%s{%s, %a})"
        (get_var_name (Pos.unmark m_vn))
        (Pos.unmark m_idxf) (Pp.unmark form_expr) idx
  | PrintEventName (e, f, _) ->
      Format.fprintf fmt "nom(champ_evenement(%a, %s))" form_expr (Pos.unmark e)
        (Pos.unmark f)
  | PrintEventAlias (e, f, _) ->
      Format.fprintf fmt "alias(champt_evenement(%a, %s))" form_expr
        (Pos.unmark e) (Pos.unmark f)
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
  | VarDecl (m_access, idx, e) ->
      format_access form_var
        (format_expression form_var)
        fmt (Pos.unmark m_access);
      (match idx with
      | Some vi ->
          Format.fprintf fmt "[%a]" (format_expression form_var) (Pos.unmark vi)
      | None -> ());
      Format.fprintf fmt " = %a" (format_expression form_var) (Pos.unmark e)
  | EventFieldRef (idx, f, _, v) ->
      Format.fprintf fmt "champ_evenement(%a,%s) reference %a"
        (format_expression form_var)
        (Pos.unmark idx) (Pos.unmark f) form_var (Pos.unmark v)

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
    | Iterate_values (var, var_intervals, itb) ->
        let format_var_intervals fmt (e0, e1, step) =
          Format.fprintf fmt ": %a .. %a increment %a@\n" form_expr
            (Pos.unmark e0) form_expr (Pos.unmark e1) form_expr
            (Pos.unmark step)
        in
        Format.fprintf fmt "iterate variable %a@;: %a@;: dans (" form_var
          (Pos.unmark var)
          (Pp.list_space format_var_intervals)
          var_intervals;
        Format.fprintf fmt "@[<h 2>  %a@]@\n)@\n" form_instrs itb
    | Restore (vars, var_params, evts, evtfs, rb) ->
        let format_vars fmt = function
          | [] -> ()
          | vars ->
              Format.fprintf fmt "@;: variables %a"
                (Pp.list_comma (Pp.unmark form_var))
                vars
        in
        let format_var_param fmt (var, vcs, expr) =
          Format.fprintf fmt "@;: variable %a : categorie %a : avec %a"
            (Pp.unmark form_var) var (CatVar.Map.pp_keys ()) vcs form_expr
            (Pos.unmark expr)
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
                  Format.fprintf fmt "@;: evenement %a : avec %a"
                    (Pp.unmark form_var) v (Pp.unmark form_expr) e)
                evtfs
        in
        Format.fprintf fmt "restaure%a%a%a%a@;: apres (" format_vars vars
          format_var_params var_params format_evts evts format_evtfs evtfs;
        Format.fprintf fmt "@[<h 2>  %a@]@;)@;" form_instrs rb
    | ArrangeEvents (s, f, a, itb) ->
        Format.fprintf fmt "arrange_evenements@;:";
        (match s with
        | Some (v0, v1, e) ->
            Format.fprintf fmt "trier %a,%a : avec %a@;" form_var
              (Pos.unmark v0) form_var (Pos.unmark v1) form_expr (Pos.unmark e)
        | None -> ());
        (match f with
        | Some (v, e) ->
            Format.fprintf fmt "filter %a : avec %a@;" form_var (Pos.unmark v)
              form_expr (Pos.unmark e)
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
