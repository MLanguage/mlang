(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

type rule_or_verif = Rule | Verif

type rdom_or_chain = RuleDomain of Mast.DomainId.t | Chaining of string

module Err = struct
  let rov_to_str rov = match rov with Rule -> "rule" | Verif -> "verif"

  let application_already_defined name old_pos pos =
    let msg =
      Format.asprintf "application %s already defined %a" name
        Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let chaining_already_defined name old_pos pos =
    let msg =
      Format.asprintf "chaining %s already defined %a" name Pos.format_position
        old_pos
    in
    Errors.raise_spanned_error msg pos

  let unknown_application pos =
    Errors.raise_spanned_error "unknown application" pos

  let application_already_specified old_pos pos =
    let msg =
      Format.asprintf "application already specified %a" Pos.format_position
        old_pos
    in
    Errors.raise_spanned_error msg pos

  let attribute_already_declared attr old_pos pos =
    let msg =
      Format.asprintf
        "attribute \"%s\" declared more than once: already declared %a" attr
        Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let var_category_already_definied cat old_pos pos =
    let msg =
      Format.asprintf
        "Category \"%a\" defined more than once: already defined %a"
        Mir.pp_cat_variable cat Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let attribute_already_defined attr old_pos pos =
    let msg =
      Format.asprintf
        "attribute \"%s\" defined more than once: already defined %a" attr
        Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let variable_of_unknown_category cat name_pos =
    let msg =
      Format.asprintf "variable with unknown category %a" Mir.pp_cat_variable
        cat
    in
    Errors.raise_spanned_error msg name_pos

  let attribute_is_not_defined name attr pos =
    let msg =
      Format.asprintf "variable \"%s\" has no attribute \"%s\"" name attr
    in
    Errors.raise_spanned_error msg pos

  let alias_already_declared alias old_pos pos =
    let msg =
      Format.asprintf
        "alias \"%s\" declared more than once: already declared %a" alias
        Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let variable_already_declared name old_pos pos =
    let msg =
      Format.asprintf
        "variable \"%s\" declared more than once: already declared %a" name
        Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let error_already_declared name old_pos pos =
    let msg =
      Format.asprintf
        "error \"%s\" declared more than once: already declared %a" name
        Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let domain_already_declared rov old_pos pos =
    let msg =
      Format.asprintf "%s domain declared more than once: already declared %a"
        (rov_to_str rov) Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let default_domain_already_declared rov old_pos pos =
    let msg =
      Format.asprintf
        "default %s domain declared more than once: already declared %a"
        (rov_to_str rov) Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let no_default_domain rov =
    let msg =
      Format.asprintf "there are no default %s domain" (rov_to_str rov)
    in
    Errors.raise_error msg

  let loop_in_domains rov cycle =
    let pp_cycle fmt cycle =
      let foldCycle first id =
        if first then Format.fprintf fmt "%a@;" (Mast.DomainId.pp ()) id
        else Format.fprintf fmt "-> %a@;" (Mast.DomainId.pp ()) id;
        false
      in
      ignore (List.fold_left foldCycle true cycle)
    in
    let msg =
      Format.asprintf "there is a loop in the %s domain hierarchy@;@[<v 2>%a@]"
        (rov_to_str rov) pp_cycle cycle
    in
    Errors.raise_error msg

  let domain_specialize_itself rov dom_id pos =
    let msg =
      Format.asprintf "%s domain \"%a\" specialize itself" (rov_to_str rov)
        (Mast.DomainId.pp ()) dom_id
    in
    Errors.raise_spanned_error msg pos

  let target_already_declared name old_pos pos =
    let msg =
      Format.asprintf
        "target \"%s\" declared more than once: already declared %a" name
        Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let unknown_variable pos = Errors.raise_spanned_error "unknown variable" pos

  let variable_used_as_table decl_pos pos =
    let msg =
      Format.asprintf "variable used as a table, declared %a"
        Pos.format_position decl_pos
    in
    Errors.raise_spanned_error msg pos

  let table_used_as_variable decl_pos pos =
    let msg =
      Format.asprintf "table used as a variable, declared %a"
        Pos.format_position decl_pos
    in
    Errors.raise_spanned_error msg pos

  let unknown_attribut_for_var cat pos =
    let msg =
      Format.asprintf "unknown attribute for a variable of category \"%a\""
        Mir.pp_cat_variable cat
    in
    Errors.raise_spanned_error msg pos

  let tmp_vars_have_no_attrs pos =
    Errors.raise_spanned_error "temporary variables have no attributes" pos

  let unknown_variable_category pos =
    Errors.raise_spanned_error "unknown_variable_category" pos

  let insruction_forbidden_in_rules pos =
    Errors.raise_spanned_error "instruction forbidden in rules" pos

  let unknown_domain rov pos =
    let msg = Format.asprintf "unknown %s domain" (rov_to_str rov) in
    Errors.raise_spanned_error msg pos

  let unknown_chaining pos = Errors.raise_spanned_error "unknown chaining" pos

  let rule_domain_not_computable pos =
    Errors.raise_spanned_error "rule domain not computable" pos

  let verif_domain_not_verifiable pos =
    Errors.raise_spanned_error "verif domain not verifiable" pos

  let chaining_without_app pos =
    Errors.raise_spanned_error "chaining without compatible application" pos

  let rule_already_defined rule_id old_pos pos =
    let msg =
      Format.asprintf "rule %d defined more than once: already defined %a"
        rule_id Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let multimax_require_two_args pos =
    Errors.raise_spanned_error "function multimax require two arguments" pos

  let second_arg_of_multimax pos =
    Errors.raise_spanned_error
      "second argument of functionn multimax must be a variable name" pos

  let loop_in_rules rdom_chain cycle =
    let rdom_chain_str =
      match rdom_chain with
      | RuleDomain rdom_id ->
          Format.asprintf "rule domain \"%a\"" (Mast.DomainId.pp ()) rdom_id
      | Chaining ch -> Format.sprintf "chaining \"%s\"" ch
    in
    let pp_cycle fmt cycle =
      let rec aux first = function
        | [] -> ()
        | (v, Some e) :: tl ->
            if first then Format.fprintf fmt "rule %d\n" v
            else Format.fprintf fmt " -(%s)-> rule %d\n" e v;
            aux false tl
        | (v, None) :: tl ->
            if first then Format.fprintf fmt "rule %d\n" v
            else Format.fprintf fmt " -()-> rule %d\n" v;
            aux false tl
      in
      aux true cycle
    in
    let msg =
      Format.asprintf "there is a loop in rules of %s:\n%a" rdom_chain_str
        pp_cycle cycle
    in
    Errors.raise_error msg

  let rule_domain_incompatible_with_chaining ch_name pos =
    let msg =
      Format.asprintf "rule domain incompatible with chaining \"%s\"" ch_name
    in
    Errors.raise_spanned_error msg pos
end

type global_variable = {
  global_name : string Pos.marked;
  global_category : Mir.cat_variable;
  global_attrs : int Pos.marked StrMap.t;
  global_alias : string Pos.marked option;
  global_table : int option;
  global_description : string Pos.marked;
  global_typ : Mast.value_typ option;
}

type variable = GlobalVar of global_variable

type error = {
  name : string Pos.marked;
  typ : Mast.error_typ;
  kind : string Pos.marked;
  major_code : string Pos.marked;
  minor_code : string Pos.marked;
  isisf : string Pos.marked;
  description : string;
}

type syms = Mast.DomainId.t Pos.marked Mast.DomainIdMap.t

type 'a doms = 'a Mir.domain Mast.DomainIdMap.t

type chaining = {
  chain_name : string Pos.marked;
  chain_apps : Pos.t StrMap.t;
  chain_rules : Mir.rule_domain Pos.marked IntMap.t;
}

type target = {
  target_name : string Pos.marked;
  target_apps : Pos.t StrMap.t;
  target_tmp_vars : int option Pos.marked StrMap.t;
  target_prog : Mast.instruction Pos.marked list;
}

type rule = {
  rule_id : int Pos.marked;
  rule_apps : Pos.t StrMap.t;
  rule_domain : Mir.rule_domain;
  rule_chain : string option;
  rule_instrs : Mast.instruction Pos.marked list;
  rule_in_vars : StrSet.t;
  rule_out_vars : StrSet.t;
}

type program = {
  prog_prefix : string;
  prog_app : string;
  prog_apps : Pos.t StrMap.t;
  prog_chainings : chaining StrMap.t;
  prog_var_cats : Mir.cat_variable_data Mir.CatVarMap.t;
  prog_vars : global_variable StrMap.t;
  prog_alias : global_variable StrMap.t;
  prog_errors : error StrMap.t;
  prog_rdoms : Mir.rule_domain_data doms;
  prog_rdom_syms : syms;
  prog_vdoms : Mir.verif_domain_data doms;
  prog_vdom_syms : syms;
  prog_rules : rule IntMap.t;
  prog_verifs : unit IntMap.t;
  prog_targets : target StrMap.t;
}

let safe_prefix (p : Mast.program) : string =
  let target_names =
    List.fold_left
      (fun names source_file ->
        List.fold_left
          (fun names (item, _pos) ->
            match item with
            | Mast.Target t -> Pos.unmark t.Mast.target_name :: names
            | _ -> names)
          names source_file)
      [] p
  in
  let sorted_names =
    List.sort
      (fun x0 x1 ->
        let cmp = compare (String.length x1) (String.length x0) in
        if cmp = 0 then compare x0 x1 else cmp)
      target_names
  in
  let buf = Buffer.create 16 in
  let starts_with p s =
    let lp = String.length p in
    let ls = String.length s in
    let rec aux i = i = lp || (p.[i] = s.[i] && aux (i + 1)) in
    lp <= ls && aux 0
  in
  let rec make_prefix = function
    | name :: tl ->
        let i = Buffer.length buf in
        if i >= String.length name then make_prefix []
        else (
          (if starts_with (Buffer.contents buf) name then
           let c = match name.[i] with 'a' -> 'b' | _ -> 'a' in
           Buffer.add_char buf c);
          make_prefix tl)
    | [] -> Buffer.contents buf
  in
  make_prefix sorted_names

let empty_program (p : Mast.program) prog_app =
  {
    prog_prefix = safe_prefix p;
    prog_app;
    prog_apps = StrMap.empty;
    prog_chainings = StrMap.empty;
    prog_var_cats = Mir.CatVarMap.empty;
    prog_vars = StrMap.empty;
    prog_alias = StrMap.empty;
    prog_errors = StrMap.empty;
    prog_rdoms = Mast.DomainIdMap.empty;
    prog_rdom_syms = Mast.DomainIdMap.empty;
    prog_vdoms = Mast.DomainIdMap.empty;
    prog_vdom_syms = Mast.DomainIdMap.empty;
    prog_rules = IntMap.empty;
    prog_verifs = IntMap.empty;
    prog_targets = StrMap.empty;
  }

let check_application (name : string) (pos : Pos.t) (prog : program) : program =
  match StrMap.find_opt name prog.prog_apps with
  | Some old_pos -> Err.application_already_defined name old_pos pos
  | None ->
      let prog_apps = StrMap.add name pos prog.prog_apps in
      { prog with prog_apps }

let check_chaining (name : string) (pos : Pos.t)
    (m_apps : string Pos.marked list) (prog : program) : program =
  (match StrMap.find_opt name prog.prog_chainings with
  | Some { chain_name = _, old_pos; _ } ->
      Err.chaining_already_defined name old_pos pos
  | None -> ());
  let chain_name = (name, pos) in
  let chain_apps =
    List.fold_left
      (fun apps (app, app_pos) ->
        (match StrMap.find_opt app prog.prog_apps with
        | None -> Err.unknown_application app_pos
        | Some _ -> ());
        (match StrMap.find_opt app apps with
        | Some old_pos -> Err.application_already_specified old_pos app_pos
        | None -> ());
        StrMap.add app app_pos apps)
      StrMap.empty m_apps
  in
  let chain_rules = IntMap.empty in
  let chaining = { chain_name; chain_apps; chain_rules } in
  let prog_chainings = StrMap.add name chaining prog.prog_chainings in
  { prog with prog_chainings }

let get_var_cat_id_str (var_cat : Mir.cat_variable) : string =
  let buf = Buffer.create 100 in
  (match var_cat with
  | Mir.CatComputed ccs ->
      Buffer.add_string buf "calculee";
      Mir.CatCompSet.iter
        (function
          | Mir.Base -> Buffer.add_string buf "_base"
          | Mir.GivenBack -> Buffer.add_string buf "_restituee")
        ccs
  | Mir.CatInput ss ->
      Buffer.add_string buf "saisie";
      let add buf s =
        String.iter
          (function
            | '_' -> Buffer.add_string buf "__" | c -> Buffer.add_char buf c)
          s
      in
      StrSet.iter
        (fun s ->
          Buffer.add_char buf '_';
          add buf s)
        ss);
  Buffer.contents buf

let get_var_cat_loc (var_cat : Mir.cat_variable) : Mir.cat_variable_loc =
  match var_cat with
  | Mir.CatComputed ccs ->
      if Mir.CatCompSet.mem Mir.Base ccs then Mir.LocBase else Mir.LocCalculated
  | Mir.CatInput _ -> Mir.LocInput

let get_var_cats (cat_decl : Mast.var_category_decl) : Mir.cat_variable list =
  match cat_decl.Mast.var_type with
  | Mast.Input ->
      let id = StrSet.from_marked_list cat_decl.Mast.var_category in
      [ Mir.CatInput id ]
  | Mast.Computed ->
      let base = Mir.CatCompSet.singleton Base in
      let givenBack = Mir.CatCompSet.singleton GivenBack in
      let baseAndGivenBack = base |> Mir.CatCompSet.add GivenBack in
      [
        Mir.CatComputed Mir.CatCompSet.empty;
        Mir.CatComputed base;
        Mir.CatComputed givenBack;
        Mir.CatComputed baseAndGivenBack;
      ]

let check_var_category (cat_decl : Mast.var_category_decl) (decl_pos : Pos.t)
    (prog : program) : program =
  let attributs =
    List.fold_left
      (fun attributs (attr, pos) ->
        match StrMap.find_opt attr attributs with
        | None -> StrMap.add attr pos attributs
        | Some old_pos -> Err.attribute_already_declared attr old_pos pos)
      StrMap.empty cat_decl.Mast.var_attributes
  in
  let add_cat cats cat =
    match Mir.CatVarMap.find_opt cat cats with
    | Some Mir.{ pos; _ } -> Err.var_category_already_definied cat pos decl_pos
    | None ->
        let data =
          Mir.
            {
              id = cat;
              id_str = get_var_cat_id_str cat;
              id_int = Mir.CatVarMap.cardinal cats;
              loc = get_var_cat_loc cat;
              attributs;
              pos = decl_pos;
            }
        in
        Mir.CatVarMap.add cat data cats
  in
  let prog_var_cats =
    List.fold_left add_cat prog.prog_var_cats (get_var_cats cat_decl)
  in
  { prog with prog_var_cats }

let get_attributes (attr_list : Mast.variable_attribute list) :
    int Pos.marked StrMap.t =
  List.fold_left
    (fun attributes (m_attr, m_value) ->
      let attr, attr_pos = m_attr in
      let value, _ = m_value in
      match StrMap.find_opt attr attributes with
      | Some (_, old_pos) -> Err.attribute_already_defined attr old_pos attr_pos
      | None -> StrMap.add attr (value, attr_pos) attributes)
    StrMap.empty attr_list

let check_global_var (var : global_variable) (prog : program) : program =
  let name, name_pos = var.global_name in
  let cat =
    match Mir.CatVarMap.find_opt var.global_category prog.prog_var_cats with
    | None ->
        Format.eprintf "XXX %a\n" Mir.pp_cat_variable var.global_category;
        Mir.CatVarMap.iter
          (fun cat _ -> Format.eprintf "YYY %a\n" Mir.pp_cat_variable cat)
          prog.prog_var_cats;
        Err.variable_of_unknown_category var.global_category name_pos
    | Some cat -> cat
  in
  StrMap.iter
    (fun attr _ ->
      if not (StrMap.mem attr var.global_attrs) then
        Err.attribute_is_not_defined name attr name_pos)
    cat.Mir.attributs;
  let prog_vars =
    match StrMap.find_opt name prog.prog_vars with
    | Some gvar ->
        let old_pos = Pos.get_position gvar.global_name in
        Err.variable_already_declared name old_pos name_pos
    | None -> StrMap.add name var prog.prog_vars
  in
  let prog_alias =
    match var.global_alias with
    | Some (alias, alias_pos) -> (
        match StrMap.find_opt alias prog.prog_alias with
        | Some gvar ->
            let old_pos = Pos.get_position (Option.get gvar.global_alias) in
            Err.alias_already_declared alias old_pos alias_pos
        | None -> StrMap.add alias var prog.prog_alias)
    | None -> prog.prog_alias
  in
  { prog with prog_vars; prog_alias }

let check_var_decl (var_decl : Mast.variable_decl) (prog : program) : program =
  match var_decl with
  | Mast.ConstVar _ -> assert false
  | Mast.InputVar (input_var, _decl_pos) ->
      let global_category =
        let input_set =
          List.fold_left
            (fun res (str, _pos) -> StrSet.add str res)
            StrSet.empty input_var.input_category
        in
        Mir.CatInput input_set
      in
      let var =
        {
          global_name = input_var.Mast.input_name;
          global_category;
          global_attrs = get_attributes input_var.Mast.input_attributes;
          global_alias = Some input_var.Mast.input_alias;
          global_table = None;
          global_description = input_var.Mast.input_description;
          global_typ = Option.map Pos.unmark input_var.Mast.input_typ;
        }
      in
      check_global_var var prog
  | Mast.ComputedVar (comp_var, _decl_pos) ->
      let global_category =
        let comp_set =
          List.fold_left
            (fun res (str, _pos) ->
              let elt =
                match str with
                | "base" -> Mir.Base
                | "restituee" -> Mir.GivenBack
                | _ -> assert false
              in
              Mir.CatCompSet.add elt res)
            Mir.CatCompSet.empty comp_var.comp_category
        in
        Mir.CatComputed comp_set
      in
      let global_table =
        match comp_var.Mast.comp_table with
        | Some (Mast.LiteralSize sz, _pos) -> Some sz
        | Some _ -> assert false
        | None -> None
      in
      let var =
        {
          global_name = comp_var.Mast.comp_name;
          global_category;
          global_attrs = get_attributes comp_var.Mast.comp_attributes;
          global_alias = None;
          global_table;
          global_description = comp_var.Mast.comp_description;
          global_typ = Option.map Pos.unmark comp_var.Mast.comp_typ;
        }
      in
      check_global_var var prog

let check_error (error : Mast.error_) (prog : program) : program =
  let kind = List.nth error.error_descr 0 in
  let major_code = List.nth error.error_descr 1 in
  let minor_code = List.nth error.error_descr 2 in
  let descr = List.nth error.error_descr 3 in
  let isisf =
    match List.nth_opt error.error_descr 4 with
    | Some s -> s
    | None -> ("", Pos.no_pos)
  in
  let description =
    let params = [ kind; major_code; minor_code; descr; isisf ] in
    String.concat ":" (List.map Pos.unmark params)
  in
  let err =
    {
      name = error.Mast.error_name;
      typ = Pos.unmark error.Mast.error_typ;
      kind;
      major_code;
      minor_code;
      isisf;
      description;
    }
  in
  let name, name_pos = err.name in
  match StrMap.find_opt name prog.prog_errors with
  | Some old_err ->
      let old_pos = Pos.get_position old_err.name in
      Err.error_already_declared name old_pos name_pos
  | None ->
      let prog_errors = StrMap.add name err prog.prog_errors in
      { prog with prog_errors }

let check_domain (rov : rule_or_verif) (decl : 'a Mast.domain_decl)
    (dom_data : 'b) ((doms, syms) : 'b doms * syms) : 'b doms * syms =
  let dom_names =
    List.fold_left
      (fun dom_names (sl, sl_pos) ->
        let id = Mast.DomainId.from_marked_list sl in
        Mast.DomainIdMap.add id sl_pos dom_names)
      Mast.DomainIdMap.empty decl.dom_names
  in
  let dom_id = Mast.DomainIdMap.min_binding dom_names in
  let domain =
    Mir.
      {
        dom_id;
        dom_names;
        dom_by_default = decl.dom_by_default;
        dom_min = Mast.DomainIdSet.from_marked_list_list decl.dom_parents;
        dom_max = Mast.DomainIdSet.empty;
        dom_rov = IntSet.empty;
        dom_data;
      }
  in
  let dom_id_name, dom_id_pos = dom_id in
  let syms =
    Mast.DomainIdMap.fold
      (fun name name_pos syms ->
        match Mast.DomainIdMap.find_opt name syms with
        | Some (_, old_pos) -> Err.domain_already_declared rov old_pos name_pos
        | None ->
            let value = (dom_id_name, name_pos) in
            Mast.DomainIdMap.add name value syms)
      dom_names syms
  in
  let syms =
    if decl.dom_by_default then
      match Mast.DomainIdMap.find_opt Mast.DomainId.empty syms with
      | Some (_, old_pos) ->
          Err.default_domain_already_declared rov old_pos dom_id_pos
      | None ->
          let value = (dom_id_name, Pos.no_pos) in
          Mast.DomainIdMap.add Mast.DomainId.empty value syms
    else syms
  in
  let doms = Mast.DomainIdMap.add dom_id_name domain doms in
  (doms, syms)

let check_rule_dom_decl (decl : Mast.rule_domain_decl) (prog : program) :
    program =
  let dom_data = Mir.{ rdom_computable = decl.Mast.dom_data.rdom_computable } in
  let doms_syms = (prog.prog_rdoms, prog.prog_rdom_syms) in
  let doms, syms = check_domain Rule decl dom_data doms_syms in
  { prog with prog_rdoms = doms; prog_rdom_syms = syms }

let mast_to_catvars (l : Mast.var_category_id) (cats : 'a Mir.CatVarMap.t) :
    Mir.CatVarSet.t =
  let filter_cats pred =
    Mir.CatVarMap.fold
      (fun cv _ res -> if pred cv then Mir.CatVarSet.add cv res else res)
      cats Mir.CatVarSet.empty
  in
  match l with
  | [ ("*", _) ], _ -> filter_cats (fun _ -> true)
  | [ ("saisie", _); ("*", _) ], _ ->
      filter_cats (fun cv ->
          match cv with Mir.CatInput _ -> true | _ -> false)
  | ("saisie", _) :: id, pos ->
      let vcat = Mir.CatInput (StrSet.from_marked_list id) in
      if Mir.CatVarMap.mem vcat cats then Mir.CatVarSet.singleton vcat
      else Err.unknown_variable_category pos
  | ("calculee", _) :: id, id_pos -> (
      match id with
      | [] -> Mir.CatVarSet.singleton (Mir.CatComputed Mir.CatCompSet.empty)
      | [ ("base", _) ] ->
          let base = Mir.CatCompSet.singleton Mir.Base in
          Mir.CatVarSet.singleton (Mir.CatComputed base)
      | [ ("base", _); ("*", _) ] ->
          let base = Mir.CatCompSet.singleton Mir.Base in
          let baseAndGivenBack = base |> Mir.CatCompSet.add Mir.GivenBack in
          Mir.CatVarSet.singleton (Mir.CatComputed base)
          |> Mir.CatVarSet.add (Mir.CatComputed baseAndGivenBack)
      | [ ("restituee", _) ] ->
          let givenBack = Mir.CatCompSet.singleton Mir.GivenBack in
          Mir.CatVarSet.singleton (Mir.CatComputed givenBack)
      | [ ("restituee", _); ("*", _) ] ->
          let givenBack = Mir.CatCompSet.singleton Mir.GivenBack in
          let baseAndGivenBack = givenBack |> Mir.CatCompSet.add Mir.Base in
          Mir.CatVarSet.singleton (Mir.CatComputed givenBack)
          |> Mir.CatVarSet.add (Mir.CatComputed baseAndGivenBack)
      | [ ("base", _); ("restituee", _) ] | [ ("restituee", _); ("base", _) ] ->
          let baseAndGivenBack =
            Mir.CatCompSet.singleton Mir.Base
            |> Mir.CatCompSet.add Mir.GivenBack
          in
          Mir.CatVarSet.singleton (Mir.CatComputed baseAndGivenBack)
      | [ ("*", _) ] ->
          let base = Mir.CatCompSet.singleton Mir.Base in
          let givenBack = Mir.CatCompSet.singleton Mir.GivenBack in
          let baseAndGivenBack = base |> Mir.CatCompSet.add Mir.GivenBack in
          Mir.CatVarSet.singleton (Mir.CatComputed Mir.CatCompSet.empty)
          |> Mir.CatVarSet.add (Mir.CatComputed base)
          |> Mir.CatVarSet.add (Mir.CatComputed givenBack)
          |> Mir.CatVarSet.add (Mir.CatComputed baseAndGivenBack)
      | _ -> Err.unknown_variable_category id_pos)
  | _ -> assert false

let check_verif_dom_decl (decl : Mast.verif_domain_decl) (prog : program) :
    program =
  let vdom_auth =
    let rec aux vdom_auth = function
      | [] -> vdom_auth
      | l :: t ->
          let vcats = mast_to_catvars l prog.prog_var_cats in
          aux (Mir.CatVarSet.union vcats vdom_auth) t
    in
    aux Mir.CatVarSet.empty decl.Mast.dom_data.vdom_auth
  in
  let vdom_verifiable = decl.Mast.dom_data.vdom_verifiable in
  let dom_data = Mir.{ vdom_auth; vdom_verifiable } in
  let doms_syms = (prog.prog_vdoms, prog.prog_vdom_syms) in
  let doms, syms = check_domain Verif decl dom_data doms_syms in
  { prog with prog_vdoms = doms; prog_vdom_syms = syms }

let complete_dom_decls (rov : rule_or_verif) ((doms, syms) : 'a doms * syms) :
    'a doms =
  let get_id id = Pos.unmark (Mast.DomainIdMap.find id syms) in
  let get_dom id doms = Mast.DomainIdMap.find (get_id id) doms in
  let module DomGraph :
    TopologicalSorting.GRAPH
      with type 'a t = 'a doms
       and type vertex = Mast.DomainId.t
       and type edge = unit = struct
    type 'a t = 'a doms

    type vertex = Mast.DomainId.t

    type edge = unit

    type 'a vertexMap = 'a Mast.DomainIdMap.t

    let vertexMapEmpty = Mast.DomainIdMap.empty

    let vertexMapAdd id value map = Mast.DomainIdMap.add (get_id id) value map

    let vertexMapRemove id map = Mast.DomainIdMap.remove (get_id id) map

    let vertexMapFindOpt id map = Mast.DomainIdMap.find_opt (get_id id) map

    let vertexMapFold fold map res =
      Mast.DomainIdMap.fold
        (fun id edge res -> fold (get_id id) edge res)
        map res

    let vertices doms =
      let get_vertex id _ nds = Mast.DomainIdMap.add id None nds in
      Mast.DomainIdMap.fold get_vertex doms Mast.DomainIdMap.empty

    let edges doms id =
      Mast.DomainIdSet.fold
        (fun id res -> Mast.DomainIdMap.add id None res)
        (get_dom id doms).Mir.dom_min Mast.DomainIdMap.empty
  end in
  let module DomSorting = TopologicalSorting.Make (DomGraph) in
  let sorted_doms =
    try DomSorting.sort doms with
    | DomSorting.Cycle cycle -> Err.loop_in_domains rov (List.map fst cycle)
    | DomSorting.AutoCycle (id, _) ->
        let dom = get_dom id doms in
        let dom_id, dom_id_pos = dom.Mir.dom_id in
        Err.domain_specialize_itself rov dom_id dom_id_pos
  in
  let doms =
    let set_min doms id =
      let dom = get_dom id doms in
      let dom_min =
        let fold parent_id res =
          let parent_dom = get_dom parent_id doms in
          let parent_id = Pos.unmark parent_dom.Mir.dom_id in
          let dom_min = Mast.DomainIdSet.map get_id parent_dom.Mir.dom_min in
          Mast.DomainIdSet.singleton parent_id
          |> Mast.DomainIdSet.union dom_min
          |> Mast.DomainIdSet.union res
        in
        Mast.DomainIdSet.fold fold dom.Mir.dom_min Mast.DomainIdSet.empty
      in
      let dom = Mir.{ dom with dom_min } in
      Mast.DomainIdMap.add id dom doms
    in
    List.fold_left set_min doms sorted_doms
  in
  let doms =
    let set_max id dom doms =
      let fold min_id doms =
        let min_dom = Mast.DomainIdMap.find min_id doms in
        let dom_max = Mast.DomainIdSet.add id min_dom.Mir.dom_max in
        let min_dom = Mir.{ min_dom with dom_max } in
        Mast.DomainIdMap.add min_id min_dom doms
      in
      Mast.DomainIdSet.fold fold dom.Mir.dom_min doms
    in
    Mast.DomainIdMap.fold set_max doms doms
  in
  let doms =
    let add_sym name (id, _) doms =
      Mast.DomainIdMap.add name (get_dom id doms) doms
    in
    Mast.DomainIdMap.fold add_sym syms doms
  in
  match Mast.DomainIdMap.find_opt Mast.DomainId.empty doms with
  | None -> Err.no_default_domain rov
  | Some _ -> doms

type 'a var_mem_type = Both | OneOf of 'a option

let check_variable (var : Mast.variable Pos.marked)
    (idx_mem : Mast.table_index Pos.marked var_mem_type)
    (tmp_vars : int option Pos.marked StrMap.t) (it_vars : Pos.t StrMap.t)
    (prog : program) : string =
  let var_data, var_pos = var in
  let name, decl_mem, decl_pos =
    match var_data with
    | Normal vn -> (
        match StrMap.find_opt vn prog.prog_vars with
        | Some { global_name = _, decl_pos; global_table; _ } ->
            (vn, OneOf global_table, decl_pos)
        | None -> (
            match StrMap.find_opt vn tmp_vars with
            | Some (decl_size, decl_pos) -> (vn, OneOf decl_size, decl_pos)
            | None -> (
                match StrMap.find_opt vn it_vars with
                | Some decl_pos -> (vn, Both, decl_pos)
                | None -> Err.unknown_variable var_pos)))
    | Generic _ -> assert false
  in
  match (idx_mem, decl_mem) with
  | Both, _ | _, Both -> name
  | OneOf idx, OneOf decl_size -> (
      match (idx, decl_size) with
      | None, None -> name
      | None, Some _ -> Err.variable_used_as_table decl_pos var_pos
      | Some _, Some _ -> name
      | Some _, None -> Err.table_used_as_variable decl_pos var_pos)

let rec check_expression (m_expr : Mast.expression Pos.marked)
    (tmp_vars : int option Pos.marked StrMap.t) (it_vars : Pos.t StrMap.t)
    (prog : program) : StrSet.t =
  ignore (tmp_vars, prog);
  let expr, expr_pos = m_expr in
  match expr with
  | Mast.TestInSet (_positive, e, values) ->
      let in_vars = check_expression e tmp_vars it_vars prog in
      List.fold_left
        (fun in_vars set_value ->
          match set_value with
          | Mast.VarValue m_var ->
              let name =
                check_variable m_var (OneOf None) tmp_vars it_vars prog
              in
              StrSet.add name in_vars
          | Mast.FloatValue _ | Mast.Interval _ -> in_vars)
        in_vars values
  | Mast.Comparison (_op, e1, e2) ->
      check_expression e1 tmp_vars it_vars prog
      |> StrSet.union (check_expression e2 tmp_vars it_vars prog)
  | Mast.Binop (_op, e1, e2) ->
      check_expression e1 tmp_vars it_vars prog
      |> StrSet.union (check_expression e2 tmp_vars it_vars prog)
  | Mast.Unop (_op, e) -> check_expression e tmp_vars it_vars prog
  | Mast.Index (t, i) ->
      let name = check_variable t (OneOf (Some i)) tmp_vars it_vars prog in
      StrSet.singleton name
  | Mast.Conditional (e1, e2, e3_opt) -> (
      let in_vars =
        check_expression e1 tmp_vars it_vars prog
        |> StrSet.union (check_expression e2 tmp_vars it_vars prog)
      in
      match e3_opt with
      | Some e3 ->
          StrSet.union in_vars (check_expression e3 tmp_vars it_vars prog)
      | None -> in_vars)
  | Mast.FunctionCall ((func_name, _), args) -> (
      match func_name with
      | "multimax" -> (
          match args with
          | Mast.ArgList [ expr; var_expr ] -> (
              match var_expr with
              | Mast.Literal (Mast.Variable var), var_pos ->
                  let var_name =
                    check_variable (var, var_pos) Both tmp_vars it_vars prog
                  in
                  StrSet.singleton var_name
                  |> StrSet.union (check_expression expr tmp_vars it_vars prog)
              | _ -> Err.second_arg_of_multimax (Pos.get_position var_expr))
          | Mast.ArgList _ -> Err.multimax_require_two_args expr_pos
          | Mast.LoopList _ -> assert false)
      | _ -> (
          match args with
          | Mast.ArgList args ->
              List.fold_left
                (fun in_vars e ->
                  check_expression e tmp_vars it_vars prog
                  |> StrSet.union in_vars)
                StrSet.empty args
          | Mast.LoopList _ -> assert false))
  | Mast.Literal l -> (
      match l with
      | Mast.Variable var ->
          let name =
            check_variable (var, expr_pos) (OneOf None) tmp_vars it_vars prog
          in
          StrSet.singleton name
      | Mast.Float _ | Mast.Undefined -> StrSet.empty)
  | Mast.NbCategory _ -> StrSet.empty
  | Mast.Attribut (v, a) ->
      let name = check_variable v Both tmp_vars it_vars prog in
      (match StrMap.find_opt name prog.prog_vars with
      | Some { global_attrs; global_category; _ } ->
          if not (StrMap.mem (Pos.unmark a) global_attrs) then
            Err.unknown_attribut_for_var global_category (Pos.get_position a)
      | None -> (
          match StrMap.find_opt name tmp_vars with
          | Some _ -> Err.tmp_vars_have_no_attrs (Pos.get_position v)
          | None -> ()));
      StrSet.singleton name
  | Mast.Size v ->
      let name = check_variable v Both tmp_vars it_vars prog in
      StrSet.singleton name
  | Mast.NbError -> StrSet.empty
  | Mast.Loop _ -> assert false

let get_compute_id_str (instr : Mast.instruction) (prog : program) : string =
  let buf = Buffer.create 100 in
  Buffer.add_string buf prog.prog_prefix;
  let add_sml buf sml =
    let id = Mast.DomainId.from_marked_list (Pos.unmark sml) in
    let add s =
      String.iter
        (function
          | '_' -> Buffer.add_string buf "__" | c -> Buffer.add_char buf c)
        s
    in
    Mast.DomainId.iter
      (fun s ->
        Buffer.add_char buf '_';
        add s)
      id;
    id
  in
  (match instr with
  | Mast.ComputeDomain l -> (
      Buffer.add_string buf "_rules";
      let id = add_sml buf l in
      match Mast.DomainIdMap.find_opt id prog.prog_rdom_syms with
      | Some (dom_id, _) ->
          let rdom = Mast.DomainIdMap.find dom_id prog.prog_rdoms in
          if not rdom.Mir.dom_data.rdom_computable then
            Err.rule_domain_not_computable (Pos.get_position l)
      | None -> Err.unknown_domain Rule (Pos.get_position l))
  | Mast.ComputeChaining (ch_name, ch_pos) -> (
      Buffer.add_string buf "_chaining_";
      Buffer.add_string buf ch_name;
      match StrMap.find_opt ch_name prog.prog_chainings with
      | Some _ -> ()
      | None -> Err.unknown_chaining ch_pos)
  | Mast.ComputeVerifs (l, _) -> (
      Buffer.add_string buf "_verifs";
      let id = add_sml buf l in
      Buffer.add_char buf '_';
      let cpt = IntMap.cardinal prog.prog_verifs in
      Buffer.add_string buf (Format.sprintf "%d" cpt);
      match Mast.DomainIdMap.find_opt id prog.prog_vdom_syms with
      | Some (dom_id, _) ->
          let vdom = Mast.DomainIdMap.find dom_id prog.prog_vdoms in
          if not vdom.Mir.dom_data.vdom_verifiable then
            Err.verif_domain_not_verifiable (Pos.get_position l)
      | None -> Err.unknown_domain Verif (Pos.get_position l))
  | _ -> assert false);
  Buffer.contents buf

let cats_variable_from_decl_list (l : Mast.var_category_id list)
    (cats : 'a Mir.CatVarMap.t) : Mir.CatVarSet.t =
  let rec aux res = function
    | [] -> res
    | l :: t ->
        let vcats = mast_to_catvars l cats in
        aux (Mir.CatVarSet.union vcats res) t
  in
  aux Mir.CatVarSet.empty l

let rec check_instructions (instrs : Mast.instruction Pos.marked list)
    (is_rule : bool) (tmp_vars : int option Pos.marked StrMap.t)
    (it_vars : Pos.t StrMap.t) (prog : program) :
    Mast.instruction Pos.marked list * StrSet.t * StrSet.t =
  let rec aux (res, in_vars, out_vars) = function
    | [] -> (List.rev res, in_vars, out_vars)
    | m_instr :: il -> (
        let instr, instr_pos = m_instr in
        match instr with
        | Mast.Formula (f, _) -> (
            match f with
            | Mast.SingleFormula sf ->
                let lval = Pos.unmark sf.lvalue in
                let out_var =
                  check_variable lval.var (OneOf lval.index) tmp_vars it_vars
                    prog
                in
                let in_vars_index =
                  match lval.index with
                  | Some (Mast.SymbolIndex vn, vpos) ->
                      let var = (vn, vpos) in
                      let name =
                        check_variable var (OneOf None) tmp_vars it_vars prog
                      in
                      StrSet.singleton name
                  | Some (Mast.LiteralIndex _, _) | None -> StrSet.empty
                in
                let in_vars_expr =
                  check_expression sf.formula tmp_vars it_vars prog
                in
                if is_rule then
                  let in_vars_aff = StrSet.union in_vars_index in_vars_expr in
                  let in_vars =
                    StrSet.union in_vars (StrSet.diff in_vars_aff out_vars)
                  in
                  let out_vars = StrSet.add out_var out_vars in
                  aux (m_instr :: res, in_vars, out_vars) il
                else aux (m_instr :: res, in_vars, out_vars) il
            | Mast.MultipleFormulaes _ -> assert false)
        | Mast.IfThenElse (expr, i_then, i_else) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let _ = check_expression expr tmp_vars it_vars prog in
            let res_then, _, _ =
              check_instructions i_then is_rule tmp_vars it_vars prog
            in
            let res_else, _, _ =
              check_instructions i_else is_rule tmp_vars it_vars prog
            in
            let res_instr = Mast.IfThenElse (expr, res_then, res_else) in
            aux ((res_instr, instr_pos) :: res, in_vars, out_vars) il
        | Mast.ComputeDomain _ | Mast.ComputeChaining _ | Mast.ComputeVerifs _
          ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let tname = get_compute_id_str instr prog in
            let res_instr = Mast.ComputeTarget (tname, Pos.no_pos) in
            aux ((res_instr, instr_pos) :: res, in_vars, out_vars) il
        | Mast.ComputeTarget _tn ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            aux (m_instr :: res, in_vars, out_vars) il
        | Mast.Print (_std, args) ->
            List.iter
              (fun arg ->
                match Pos.unmark arg with
                | Mast.PrintString _ -> ()
                | Mast.PrintName v | Mast.PrintAlias v ->
                    ignore (check_variable v Both tmp_vars it_vars prog)
                | Mast.PrintExpr (e, _min, _max) ->
                    ignore (check_expression e tmp_vars it_vars prog))
              args;
            aux (m_instr :: res, in_vars, out_vars) il
        | Mast.Iterate (var, vcats, expr, instrs) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let var_name, var_pos = var in
            (match StrMap.find_opt var_name prog.prog_vars with
            | Some { global_name = _, old_pos; _ } ->
                Err.variable_already_declared var_name old_pos var_pos
            | None -> ());
            (match StrMap.find_opt var_name tmp_vars with
            | Some (_, old_pos) ->
                Err.variable_already_declared var_name old_pos var_pos
            | None -> ());
            (match StrMap.find_opt var_name it_vars with
            | Some old_pos ->
                Err.variable_already_declared var_name old_pos var_pos
            | None -> ());
            ignore (cats_variable_from_decl_list vcats prog.prog_var_cats);
            let it_vars = StrMap.add var_name var_pos it_vars in
            ignore (check_expression expr tmp_vars it_vars prog);
            let res_instrs, _, _ =
              check_instructions instrs is_rule tmp_vars it_vars prog
            in
            let res_instr = Mast.Iterate (var, vcats, expr, res_instrs) in
            aux ((res_instr, instr_pos) :: res, in_vars, out_vars) il
        | Mast.Restore (rest_params, instrs) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            List.iter
              (fun rest_param ->
                match Pos.unmark rest_param with
                | Mast.VarList vl ->
                    List.iter
                      (fun (vn, vpos) ->
                        let var = (Mast.Normal vn, vpos) in
                        ignore (check_variable var Both tmp_vars it_vars prog))
                      vl
                | Mast.VarCats (vn, vcats, expr) ->
                    let var_name, var_pos = vn in
                    (match StrMap.find_opt var_name prog.prog_vars with
                    | Some { global_name = _, old_pos; _ } ->
                        Err.variable_already_declared var_name old_pos var_pos
                    | None -> ());
                    (match StrMap.find_opt var_name tmp_vars with
                    | Some (_, old_pos) ->
                        Err.variable_already_declared var_name old_pos var_pos
                    | None -> ());
                    (match StrMap.find_opt var_name it_vars with
                    | Some old_pos ->
                        Err.variable_already_declared var_name old_pos var_pos
                    | None -> ());
                    ignore
                      (cats_variable_from_decl_list vcats prog.prog_var_cats);
                    let it_vars = StrMap.add var_name var_pos it_vars in
                    ignore (check_expression expr tmp_vars it_vars prog))
              rest_params;

            let res_instrs, _, _ =
              check_instructions instrs is_rule tmp_vars it_vars prog
            in
            let res_instr = Mast.Restore (rest_params, res_instrs) in
            aux ((res_instr, instr_pos) :: res, in_vars, out_vars) il)
  in
  aux ([], StrSet.empty, StrSet.empty) instrs

let check_target (t : Mast.target) (prog : program) : program =
  let tname, tpos = t.Mast.target_name in
  let target_name =
    (match StrMap.find_opt tname prog.prog_targets with
    | Some { target_name = _, old_pos; _ } ->
        Err.target_already_declared tname old_pos tpos
    | None -> ());
    (tname, tpos)
  in
  let target_apps =
    List.fold_left
      (fun target_apps (app, app_pos) ->
        (match StrMap.find_opt app prog.prog_apps with
        | None -> Err.unknown_application app_pos
        | Some _ -> ());
        (match StrMap.find_opt app target_apps with
        | Some old_pos -> Err.application_already_specified old_pos app_pos
        | None -> ());
        StrMap.add app app_pos target_apps)
      StrMap.empty t.Mast.target_applications
  in
  if StrMap.mem prog.prog_app target_apps then
    let target_tmp_vars =
      let check_tmp_var (vn, vpos) tmp_vars =
        (match StrMap.find_opt vn prog.prog_vars with
        | Some { global_name = _, old_pos; _ } ->
            Err.variable_already_declared vn old_pos vpos
        | None -> ());
        match StrMap.find_opt vn tmp_vars with
        | Some (_, old_pos) -> Err.variable_already_declared vn old_pos vpos
        | None -> ()
      in
      List.fold_left
        (fun target_tmp_vars (var, size) ->
          check_tmp_var var target_tmp_vars;
          let vn, vpos = var in
          let sz =
            match size with
            | None -> None
            | Some (Mast.LiteralSize i, _) -> Some i
            | Some (Mast.SymbolSize _, _) -> assert false
          in
          StrMap.add vn (sz, vpos) target_tmp_vars)
        StrMap.empty t.Mast.target_tmp_vars
    in
    let target_prog, _, _ =
      check_instructions t.Mast.target_prog false target_tmp_vars StrMap.empty
        prog
    in
    let target = { target_name; target_apps; target_tmp_vars; target_prog } in
    let prog_targets = StrMap.add tname target prog.prog_targets in
    { prog with prog_targets }
  else
    let target_tmp_vars = StrMap.empty in
    let target_prog = [] in
    let target = { target_name; target_apps; target_tmp_vars; target_prog } in
    let prog_targets = StrMap.add tname target prog.prog_targets in
    { prog with prog_targets }

let check_rule (r : Mast.rule) (prog : program) : program =
  let id, id_pos = r.Mast.rule_number in
  let rule_id = (id, id_pos) in
  let rule_apps =
    List.fold_left
      (fun rule_apps (app, app_pos) ->
        match StrMap.find_opt app prog.prog_apps with
        | None -> Err.unknown_application app_pos
        | Some _ -> StrMap.add app app_pos rule_apps)
      StrMap.empty r.Mast.rule_applications
  in
  if StrMap.mem prog.prog_app rule_apps then (
    let rdom_id =
      Mast.DomainId.from_marked_list (Pos.unmark r.Mast.rule_tag_names)
    in
    let rule_domain, rule_domain_pos =
      let rid, rid_pos =
        match Mast.DomainIdMap.find_opt rdom_id prog.prog_rdom_syms with
        | Some m_rid -> m_rid
        | None ->
            Err.unknown_domain Rule (Pos.get_position r.Mast.rule_tag_names)
      in
      (Mast.DomainIdMap.find rid prog.prog_rdoms, rid_pos)
    in
    let rule_app_set =
      StrMap.fold (fun a _ set -> StrSet.add a set) rule_apps StrSet.empty
    in
    let rule_chain, prog_chainings =
      match r.Mast.rule_chaining with
      | None -> (None, prog.prog_chainings)
      | Some (ch_name, ch_pos) -> (
          match StrMap.find_opt ch_name prog.prog_chainings with
          | None -> Err.unknown_chaining ch_pos
          | Some chain ->
              let app_set =
                StrMap.fold
                  (fun a _ set -> StrSet.add a set)
                  chain.chain_apps StrSet.empty
              in
              if StrSet.cardinal (StrSet.inter app_set rule_app_set) = 0 then
                Err.chaining_without_app ch_pos
              else if StrSet.mem prog.prog_app app_set then
                let chain_rules =
                  IntMap.add id (rule_domain, rule_domain_pos) chain.chain_rules
                in
                let chain = { chain with chain_rules } in
                let prog_chainings =
                  StrMap.add ch_name chain prog.prog_chainings
                in
                (Some ch_name, prog_chainings)
              else (None, prog.prog_chainings))
    in
    let rule_instrs =
      List.map
        (fun f -> Pos.same_pos_as (Mast.Formula f) f)
        r.Mast.rule_formulaes
    in
    let rule_instrs, rule_in_vars, rule_out_vars =
      check_instructions rule_instrs true StrMap.empty StrMap.empty prog
    in
    let rule =
      {
        rule_id;
        rule_apps;
        rule_domain;
        rule_chain;
        rule_instrs;
        rule_in_vars;
        rule_out_vars;
      }
    in
    (match IntMap.find_opt id prog.prog_rules with
    | Some r -> Err.rule_already_defined id (Pos.get_position r.rule_id) id_pos
    | None -> ());
    let prog_rules = IntMap.add id rule prog.prog_rules in
    { prog with prog_rules; prog_chainings })
  else prog

let convert_rules (prog : program) : program =
  let prog_targets =
    IntMap.fold
      (fun id rule prog_targets ->
        let tname = Format.sprintf "%s_rule_%d" prog.prog_prefix id in
        let target =
          {
            target_name = (tname, Pos.no_pos);
            target_apps = StrMap.singleton prog.prog_app Pos.no_pos;
            target_tmp_vars = StrMap.empty;
            target_prog = rule.rule_instrs;
          }
        in
        StrMap.add tname target prog_targets)
      prog.prog_rules prog.prog_targets
  in
  { prog with prog_targets }

let is_vartmp (var : string) =
  String.length var >= 6 && String.sub var 0 6 = "VARTMP"

let create_rule_graph (in_vars_from : rule -> StrSet.t)
    (out_vars_from : rule -> StrSet.t) (rules : 'a IntMap.t) :
    string IntMap.t option IntMap.t =
  let in_vars_of_rules =
    IntMap.fold
      (fun id rule var_map ->
        StrSet.fold
          (fun var var_map ->
            if is_vartmp var then var_map
            else
              StrMap.update var
                (function
                  | None -> Some (IntSet.singleton id)
                  | Some set -> Some (IntSet.add id set))
                var_map)
          (in_vars_from rule) var_map)
      rules StrMap.empty
  in
  IntMap.map
    (fun rule ->
      let edges =
        StrSet.fold
          (fun out_var edges ->
            if is_vartmp out_var then edges
            else
              match StrMap.find_opt out_var in_vars_of_rules with
              | Some out_rules ->
                  IntSet.fold
                    (fun out_id edges -> IntMap.add out_id out_var edges)
                    out_rules edges
              | None -> edges)
          (out_vars_from rule) IntMap.empty
      in
      Some edges)
    rules

let rule_graph_to_instrs (rdom_chain : rdom_or_chain) (prog : program)
    (rule_graph : string IntMap.t option IntMap.t) :
    Mast.instruction Pos.marked list =
  let module RuleGraph :
    TopologicalSorting.GRAPH
      with type 'a t = string IntMap.t option IntMap.t
       and type vertex = int
       and type edge = string = struct
    type 'a t = string IntMap.t option IntMap.t

    type vertex = int

    type edge = string

    type 'a vertexMap = 'a IntMap.t

    let vertexMapEmpty = IntMap.empty

    let vertexMapAdd id value map = IntMap.add id value map

    let vertexMapRemove id map = IntMap.remove id map

    let vertexMapFindOpt id map = IntMap.find_opt id map

    let vertexMapFold fold map res = IntMap.fold fold map res

    let vertices rules =
      IntMap.fold (fun id _ res -> IntMap.add id None res) rules IntMap.empty

    let edges rules id =
      let es = Option.get (IntMap.find id rules) in
      IntMap.map (fun var -> Some var) es
  end in
  let module RulesSorting = TopologicalSorting.Make (RuleGraph) in
  let auto_cycle =
    Some
      (function
      | id, var ->
          Cli.debug_print "warning: auto-cycle in rule %d with variable %s" id
            var)
  in
  let sorted_rules =
    try RulesSorting.sort ~auto_cycle rule_graph with
    | RulesSorting.Cycle cycle -> Err.loop_in_rules rdom_chain cycle
    | RulesSorting.AutoCycle _ -> assert false
  in
  List.map
    (fun id ->
      let name = Format.sprintf "%s_rule_%d" prog.prog_prefix id in
      (Mast.ComputeTarget (name, Pos.no_pos), Pos.no_pos))
    sorted_rules

let rdom_rule_filter (rdom : Mir.rule_domain_data Mir.domain) (rule : rule) :
    bool =
  let rdom_id = Pos.unmark rdom.dom_id in
  let rule_rdom_id = Pos.unmark rule.rule_domain.dom_id in
  Mast.DomainId.equal rdom_id rule_rdom_id
  || Mast.DomainIdSet.mem rule_rdom_id rdom.Mir.dom_min

let complete_rule_domains (prog : program) : program =
  let prog_targets =
    Mast.DomainIdMap.fold
      (fun rdom_id rdom prog_targets ->
        if rdom.Mir.dom_data.Mir.rdom_computable then
          let rdom_rules =
            IntMap.filter
              (fun _ rule -> rdom_rule_filter rdom rule)
              prog.prog_rules
          in
          let rule_graph =
            create_rule_graph
              (fun r -> r.rule_in_vars)
              (fun r -> r.rule_out_vars)
              rdom_rules
          in
          let target_prog =
            rule_graph_to_instrs (RuleDomain rdom_id) prog rule_graph
          in
          let tname =
            let spl =
              Mast.DomainId.fold (fun s l -> (s, Pos.no_pos) :: l) rdom_id []
            in
            get_compute_id_str (Mast.ComputeDomain (spl, Pos.no_pos)) prog
          in
          let target =
            {
              target_name = (tname, Pos.no_pos);
              target_apps = StrMap.singleton prog.prog_app Pos.no_pos;
              target_tmp_vars = StrMap.empty;
              target_prog;
            }
          in
          StrMap.add tname target prog_targets
        else prog_targets)
      prog.prog_rdoms prog.prog_targets
  in
  { prog with prog_targets }

let rdom_id_rule_filter (prog : program) (rdom_id : Mast.DomainId.t)
    (rule : rule) : bool =
  let rdom = Mast.DomainIdMap.find rdom_id prog.prog_rdoms in
  rdom_rule_filter rdom rule

let rdom_ids_rule_filter (prog : program) (rdom_ids : Mast.DomainIdSet.t)
    (rule : rule) : bool =
  Mast.DomainIdSet.exists
    (fun rdom_id -> rdom_id_rule_filter prog rdom_id rule)
    rdom_ids

let complete_chainings (prog : program) : program =
  let prog_targets =
    StrMap.fold
      (fun ch_name chain prog_targets ->
        let all_ids =
          Mast.DomainIdMap.fold
            (fun _ rdom ids ->
              let uid = Pos.unmark rdom.Mir.dom_id in
              Mast.DomainIdSet.add uid ids)
            prog.prog_rdoms Mast.DomainIdSet.empty
        in
        let sup_ids =
          IntMap.fold
            (fun _ (rdom, id_pos) sup_ids ->
              let uid = Pos.unmark rdom.Mir.dom_id in
              let rdom_supeq = Mast.DomainIdSet.add uid rdom.Mir.dom_max in
              let sup_ids = Mast.DomainIdSet.inter sup_ids rdom_supeq in
              if Mast.DomainIdSet.cardinal sup_ids = 0 then
                Err.rule_domain_incompatible_with_chaining ch_name id_pos
              else sup_ids)
            chain.chain_rules all_ids
        in
        let min_ids =
          Mast.DomainIdSet.filter
            (fun id ->
              let rdom = Mast.DomainIdMap.find id prog.prog_rdoms in
              let min_sups = Mast.DomainIdSet.inter sup_ids rdom.Mir.dom_min in
              Mast.DomainIdSet.is_empty min_sups)
            sup_ids
        in
        let rdom_rules =
          IntMap.filter
            (fun _ rule -> rdom_ids_rule_filter prog min_ids rule)
            prog.prog_rules
        in
        let inverted_rule_graph =
          create_rule_graph
            (fun r -> r.rule_out_vars)
            (fun r -> r.rule_in_vars)
            rdom_rules
        in
        let rules =
          let rec add_connected_rules rid rules =
            if IntMap.mem rid rules then rules
            else
              let edges = Option.get (IntMap.find rid inverted_rule_graph) in
              let rules = IntMap.add rid (IntMap.find rid rdom_rules) rules in
              IntMap.fold
                (fun rid _ rules -> add_connected_rules rid rules)
                edges rules
          in
          IntMap.fold
            (fun rid _ rules -> add_connected_rules rid rules)
            chain.chain_rules IntMap.empty
        in
        let rule_graph =
          create_rule_graph
            (fun r -> r.rule_in_vars)
            (fun r -> r.rule_out_vars)
            rules
        in
        let target_prog =
          rule_graph_to_instrs (Chaining ch_name) prog rule_graph
        in
        let tname =
          get_compute_id_str (Mast.ComputeChaining (ch_name, Pos.no_pos)) prog
        in
        let target =
          {
            target_name = (tname, Pos.no_pos);
            target_apps = StrMap.singleton prog.prog_app Pos.no_pos;
            target_tmp_vars = StrMap.empty;
            target_prog;
          }
        in
        StrMap.add tname target prog_targets)
      prog.prog_chainings prog.prog_targets
  in
  { prog with prog_targets }

let proceed (p : Mast.program) : program =
  let app = "iliad" in
  (* à paramétrer *)
  let prog =
    List.fold_left
      (fun prog source_file ->
        List.fold_left
          (fun prog (item, _pos_item) ->
            match item with
            | Mast.Application (name, pos) -> check_application name pos prog
            | Mast.Chaining ((name, pos), m_apps) ->
                check_chaining name pos m_apps prog
            | Mast.VarCatDecl (decl, pos) -> check_var_category decl pos prog
            | Mast.VariableDecl var_decl -> check_var_decl var_decl prog
            | Mast.Error error -> check_error error prog
            | Mast.Function -> prog (* unused *)
            | Mast.Output _ -> prog (* unused *)
            | Mast.RuleDomDecl decl -> check_rule_dom_decl decl prog
            | Mast.VerifDomDecl decl -> check_verif_dom_decl decl prog
            | Mast.Target t -> check_target t prog
            | Mast.Rule r -> check_rule r prog
            | _ -> prog)
          prog source_file)
      (empty_program p app) p
  in
  let doms_syms = (prog.prog_rdoms, prog.prog_rdom_syms) in
  let prog_rdoms = complete_dom_decls Rule doms_syms in
  let doms_syms = (prog.prog_vdoms, prog.prog_vdom_syms) in
  let prog_vdoms = complete_dom_decls Verif doms_syms in
  let prog = convert_rules { prog with prog_rdoms; prog_vdoms } in
  let prog = complete_rule_domains prog in
  let prog = complete_chainings prog in
  prog
