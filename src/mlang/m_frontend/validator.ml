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

type rdom_or_chain = RuleDomain of Com.DomainId.t | Chaining of string

module Err = struct
  let rov_to_str rov = match rov with Rule -> "rule" | Verif -> "verif"

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
        Com.CatVar.pp cat Pos.format_position old_pos
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
      Format.asprintf "variable with unknown category %a" Com.CatVar.pp cat
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

  let temporary_variable_already_declared name old_pos pos =
    let msg =
      Format.asprintf
        "temporary variable \"%s\" declared more than once: already declared %a"
        name Pos.format_position old_pos
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
        if first then Format.fprintf fmt "%a@;" (Com.DomainId.pp ()) id
        else Format.fprintf fmt "-> %a@;" (Com.DomainId.pp ()) id;
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
        (Com.DomainId.pp ()) dom_id
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
        Com.CatVar.pp cat
    in
    Errors.raise_spanned_error msg pos

  let unknown_attribut attr pos =
    let msg = Format.sprintf "unknown attribute \"%s\"" attr in
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

  let rov_already_defined rov rov_id old_pos pos =
    let msg =
      Format.asprintf "%s %d defined more than once: already defined %a"
        (rov_to_str rov) rov_id Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let multimax_require_two_args pos =
    Errors.raise_spanned_error "function multimax require two arguments" pos

  let second_arg_of_multimax pos =
    Errors.raise_spanned_error
      "second argument of function multimax must be a variable name" pos

  let loop_in_rules rdom_chain cycle =
    let rdom_chain_str =
      match rdom_chain with
      | RuleDomain rdom_id ->
          Format.asprintf "rule domain \"%a\"" (Com.DomainId.pp ()) rdom_id
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

  let domain_already_used rov dom_pos pos =
    let msg =
      Format.asprintf "domain of this %s already used %a" (rov_to_str rov)
        Pos.format_position dom_pos
    in
    Errors.raise_spanned_error msg pos

  let unknown_error pos = Errors.raise_spanned_error "unknown error" pos

  let variable_forbidden_in_filter pos =
    Errors.raise_spanned_error "variables are forbidden in verif filters" pos

  let forbidden_expresion_in_filter pos =
    Errors.raise_spanned_error "forbidden expression in verif filter" pos

  let expression_only_in_filter pos =
    Errors.raise_spanned_error "expression authorized only in verif filters" pos

  let wrong_interval_bounds pos =
    Errors.raise_spanned_error "wrong interval bounds" pos

  let wrong_arity_of_function func_name arity pos =
    let msg =
      Format.asprintf "wrong arity: function \"%a\" expect %d argument%s"
        Com.format_func func_name arity
        (if arity = 1 then "" else "s")
    in
    Errors.raise_spanned_error msg pos

  let variable_with_forbidden_category pos =
    let msg = Format.sprintf "variable with forbidden category in verif" in
    Errors.raise_spanned_error msg pos

  let variable_already_specified name old_pos pos =
    let msg =
      Format.asprintf
        "variable \"%s\" specified more than once: already specified %a" name
        Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let main_target_not_found main_target =
    Errors.raise_error
      (Format.sprintf "main target \"%s\" not found" main_target)

  let unknown_target name pos =
    let msg = Format.asprintf "unknown target %s" name in
    Errors.raise_spanned_error msg pos

  let wrong_number_of_args nb_args pos =
    let msg =
      Format.asprintf "wrong number of arguments, %d required" nb_args
    in
    Errors.raise_spanned_error msg pos

  let target_must_not_have_a_result tn pos =
    let msg = Format.sprintf "target %s must not have a result" tn in
    Errors.raise_spanned_error msg pos

  let function_result_missing fn pos =
    let msg = Format.sprintf "result missing in function %s" fn in
    Errors.raise_spanned_error msg pos

  let forbidden_in_var_in_function vn fn pos =
    let msg =
      Format.sprintf "variable %s cannot be read in function %s" vn fn
    in
    Errors.raise_spanned_error msg pos

  let forbidden_out_var_in_function vn fn pos =
    let msg =
      Format.sprintf "variable %s cannot be written in function %s" vn fn
    in
    Errors.raise_spanned_error msg pos

  let function_does_not_exist fn pos =
    let msg = Format.sprintf "function %s does not exist" fn in
    Errors.raise_spanned_error msg pos

  let is_base_function fn pos =
    let msg = Format.sprintf "function %s already exist as base function" fn in
    Errors.raise_spanned_error msg pos

  let event_already_declared old_pos pos =
    let msg =
      Format.asprintf "event fields are already declared at %a"
        Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let event_field_already_declared name old_pos pos =
    let msg =
      Format.asprintf "event field \"%s\" is already declared at %a" name
        Pos.format_position old_pos
    in
    Errors.raise_spanned_error msg pos

  let unknown_event_field name pos =
    let msg = Format.asprintf "unknown event field \"%s\"" name in
    Errors.raise_spanned_error msg pos

  let event_field_need_a_variable name pos =
    let msg = Format.asprintf "event field \"%s\" require a variable" name in
    Errors.raise_spanned_error msg pos

  let event_field_is_not_a_reference name pos =
    let msg =
      Format.asprintf "event field \"%s\" is not a variable reference" name
    in
    Errors.raise_spanned_error msg pos

  let has_no_target () = Errors.raise_error "this program has no target"
end

type syms = Com.DomainId.t Pos.marked Com.DomainIdMap.t

type 'a doms = 'a Com.domain Com.DomainIdMap.t

type chaining = {
  chain_name : string Pos.marked;
  chain_apps : Pos.t StrMap.t;
  chain_rules : Com.rule_domain Pos.marked IntMap.t;
}

type rule = {
  rule_id : int Pos.marked;
  rule_apps : Pos.t StrMap.t;
  rule_domain : Com.rule_domain;
  rule_chains : Pos.t StrMap.t;
  rule_tmp_vars : Com.Var.t StrMap.t;
  rule_instrs : Mast.instruction Pos.marked list;
  rule_in_vars : StrSet.t;
  rule_out_vars : Pos.t StrMap.t;
  rule_seq : int;
}

type verif = {
  verif_id : int Pos.marked;
  verif_apps : Pos.t StrMap.t;
  verif_domain : Com.verif_domain;
  verif_expr : Mast.expression Pos.marked;
  verif_error : Mast.error_name Pos.marked;
  verif_var : string Pos.marked option;
  verif_is_blocking : bool;
  verif_cat_var_stats : int Com.CatVar.Map.t;
  verif_var_stats : int StrMap.t;
  verif_seq : int;
}

type target = (string Pos.marked, Mast.error_name) Com.target

type program = {
  prog_prefix : string;
  prog_seq : int;
  prog_app : Pos.t StrMap.t;
  prog_apps : Pos.t StrMap.t;
  prog_chainings : chaining StrMap.t;
  prog_var_cats : Com.CatVar.data Com.CatVar.Map.t;
  prog_dict : Com.Var.t IntMap.t;
  prog_vars : int StrMap.t;
  prog_alias : int StrMap.t;
  prog_event_fields : Com.event_field StrMap.t;
  prog_event_field_idxs : string IntMap.t;
  prog_event_pos : Pos.t;
  prog_errors : Com.Error.t StrMap.t;
  prog_rdoms : Com.rule_domain_data doms;
  prog_rdom_syms : syms;
  prog_vdoms : Com.verif_domain_data doms;
  prog_vdom_syms : syms;
  prog_functions : target StrMap.t;
  prog_rules : rule IntMap.t;
  prog_rdom_calls : (int Pos.marked * Com.DomainId.t) StrMap.t;
  prog_verifs : verif IntMap.t;
  prog_vdom_calls :
    (int Pos.marked * Com.DomainId.t * Mast.expression Pos.marked) StrMap.t;
  prog_targets : target StrMap.t;
  prog_main_target : string;
}

let is_vartmp (var : string) =
  String.length var >= 6 && String.sub var 0 6 = "VARTMP"

let check_name_in_tgv prog m_name =
  let vn, vpos = m_name in
  let err old_pos = Err.variable_already_declared vn old_pos vpos in
  match StrMap.find_opt vn prog.prog_vars with
  | Some id ->
      let var = IntMap.find id prog.prog_dict in
      let old_pos = Pos.get @@ Com.Var.name var in
      err old_pos
  | None -> ()

let check_alias_in_tgv prog m_alias =
  let an, apos = m_alias in
  let err old_pos = Err.alias_already_declared an old_pos apos in
  match StrMap.find_opt an prog.prog_alias with
  | Some id ->
      let var = IntMap.find id prog.prog_dict in
      let old_pos = Pos.get @@ Option.get @@ Com.Var.alias var in
      err old_pos
  | None -> ()

let check_name_in_tmp tmps m_name =
  let vn, vpos = m_name in
  let err old_pos = Err.variable_already_declared vn old_pos vpos in
  match StrMap.find_opt vn tmps with
  | Some var -> err (Pos.get (Com.Var.name var))
  | None -> ()

let check_name_in_args args m_name =
  let vn, vpos = m_name in
  let err old_pos = Err.variable_already_declared vn old_pos vpos in
  match List.find_opt (fun (va, _) -> vn = va) args with
  | Some (_, old_pos) -> err old_pos
  | None -> ()

let get_target_file (pos : Pos.t) : string =
  let file = Pos.get_file pos |> Filename.basename in
  let file =
    try Filename.chop_extension file with Invalid_argument _ -> file
  in
  Format.sprintf "m_%s" file

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

let empty_program (p : Mast.program) main_target =
  let prog_app =
    let fold s a = StrMap.add a Pos.no_pos s in
    List.fold_left fold StrMap.empty !Cli.application_names
  in
  {
    prog_prefix = safe_prefix p;
    prog_seq = 0;
    prog_app;
    prog_apps = StrMap.empty;
    prog_chainings = StrMap.empty;
    prog_var_cats = Com.CatVar.Map.empty;
    prog_dict = IntMap.empty;
    prog_vars = StrMap.empty;
    prog_event_fields = StrMap.empty;
    prog_event_field_idxs = IntMap.empty;
    prog_event_pos = Pos.no_pos;
    prog_alias = StrMap.empty;
    prog_errors = StrMap.empty;
    prog_rdoms = Com.DomainIdMap.empty;
    prog_rdom_syms = Com.DomainIdMap.empty;
    prog_vdoms = Com.DomainIdMap.empty;
    prog_vdom_syms = Com.DomainIdMap.empty;
    prog_functions = StrMap.empty;
    prog_rules = IntMap.empty;
    prog_rdom_calls = StrMap.empty;
    prog_verifs = IntMap.empty;
    prog_vdom_calls = StrMap.empty;
    prog_targets = StrMap.empty;
    prog_main_target = main_target;
  }

let get_seq (prog : program) : int * program =
  let seq = prog.prog_seq in
  let prog = { prog with prog_seq = seq + 1 } in
  (seq, prog)

let check_application (name : string) (pos : Pos.t) (prog : program) : program =
  (* Already checked during preprocessing *)
  let prog_apps = StrMap.add name pos prog.prog_apps in
  { prog with prog_apps }

let check_chaining (name : string) (pos : Pos.t)
    (m_apps : string Pos.marked list) (prog : program) : program =
  (* Already checked during preprocessing *)
  let chain_name = (name, pos) in
  let chain_apps =
    List.fold_left
      (fun apps (app, app_pos) -> StrMap.add app app_pos apps)
      StrMap.empty m_apps
  in
  let chain_rules = IntMap.empty in
  let chaining = { chain_name; chain_apps; chain_rules } in
  let prog_chainings = StrMap.add name chaining prog.prog_chainings in
  { prog with prog_chainings }

let get_var_cat_id_str (var_cat : Com.CatVar.t) : string =
  let buf = Buffer.create 100 in
  (match var_cat with
  | Com.CatVar.Computed { is_base } ->
      Buffer.add_string buf "calculee";
      if is_base then Buffer.add_string buf "_base"
  | Com.CatVar.Input ss ->
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

let get_var_cat_loc (var_cat : Com.CatVar.t) : Com.CatVar.loc =
  match var_cat with
  | Com.CatVar.Computed { is_base } ->
      if is_base then Com.CatVar.LocBase else Com.CatVar.LocComputed
  | Com.CatVar.Input _ -> Com.CatVar.LocInput

let get_var_cats (cat_decl : Mast.var_category_decl) : Com.CatVar.t list =
  match cat_decl.Mast.var_type with
  | Mast.Input ->
      let id = StrSet.from_marked_list cat_decl.Mast.var_category in
      [ Com.CatVar.Input id ]
  | Mast.Computed ->
      [
        Com.CatVar.Computed { is_base = false };
        Com.CatVar.Computed { is_base = true };
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
    match Com.CatVar.Map.find_opt cat cats with
    | Some Com.CatVar.{ pos; _ } ->
        Err.var_category_already_definied cat pos decl_pos
    | None ->
        let data =
          Com.CatVar.
            {
              id = cat;
              id_str = get_var_cat_id_str cat;
              id_int = Com.CatVar.Map.cardinal cats;
              loc = get_var_cat_loc cat;
              attributs;
              pos = decl_pos;
            }
        in
        Com.CatVar.Map.add cat data cats
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

let check_global_var (var : Com.Var.t) (prog : program) : program =
  let name, name_pos = var.name in
  let cat =
    let cat = Com.Var.cat var in
    match Com.CatVar.Map.find_opt cat prog.prog_var_cats with
    | None -> Err.variable_of_unknown_category cat name_pos
    | Some cat -> cat
  in
  StrMap.iter
    (fun attr _ ->
      if not (StrMap.mem attr (Com.Var.attrs var)) then
        Err.attribute_is_not_defined name attr name_pos)
    cat.attributs;
  check_name_in_tgv prog var.name;
  let prog_dict = IntMap.add var.id var prog.prog_dict in
  let prog_vars = StrMap.add name var.id prog.prog_vars in
  let prog_alias =
    match Com.Var.alias var with
    | Some m_alias ->
        check_alias_in_tgv prog m_alias;
        StrMap.add (Pos.unmark m_alias) var.id prog.prog_alias
    | None -> prog.prog_alias
  in
  { prog with prog_dict; prog_vars; prog_alias }

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
        Com.CatVar.Input input_set
      in
      let var =
        Com.Var.new_tgv ~name:input_var.Mast.input_name ~is_table:None
          ~is_given_back:input_var.input_is_givenback
          ~alias:(Some input_var.Mast.input_alias)
          ~descr:input_var.Mast.input_description
          ~attrs:(get_attributes input_var.Mast.input_attributes)
          ~cat:global_category
          ~typ:(Option.map Pos.unmark input_var.Mast.input_typ)
      in
      check_global_var var prog
  | Mast.ComputedVar (comp_var, _decl_pos) ->
      let global_category =
        let is_base =
          List.fold_left
            (fun res (str, _pos) -> match str with "base" -> true | _ -> res)
            false comp_var.comp_category
        in
        Com.CatVar.Computed { is_base }
      in
      let m_name = comp_var.Mast.comp_name in
      let is_given_back = comp_var.comp_is_givenback in
      let alias = None in
      let descr = comp_var.Mast.comp_description in
      let attrs = get_attributes comp_var.Mast.comp_attributes in
      let cat = global_category in
      let typ = Option.map Pos.unmark comp_var.Mast.comp_typ in
      let var =
        Com.Var.new_tgv ~name:m_name ~is_table:None ~is_given_back ~alias ~descr
          ~attrs ~cat ~typ
      in
      let is_table =
        match comp_var.Mast.comp_table with
        | Some (Mast.LiteralSize sz, _pos) ->
            let name, name_pos = m_name in
            let iFmt = String.map (fun _ -> '0') (Pp.spr "%d" sz) in
            let init i =
              let m_iName = (Strings.concat_int name iFmt i, name_pos) in
              Com.Var.new_tgv ~name:m_iName ~is_table:None ~is_given_back ~alias
                ~descr ~attrs ~cat ~typ
            in
            Some (Array.init sz init)
        | Some _ -> assert false
        | None -> None
      in
      let prog =
        match is_table with
        | Some tab -> Array.fold_left (fun p v -> check_global_var v p) prog tab
        | None -> prog
      in
      let var = Com.Var.set_is_table var is_table in
      check_global_var var prog

let check_event_decl (evt_decl : Com.event_field list) (decl_pos : Pos.t)
    (prog : program) : program =
  if not (StrMap.is_empty prog.prog_event_fields) then
    Err.event_already_declared prog.prog_event_pos decl_pos;
  let prog_event_fields =
    let fold (map, index) (ef : Com.event_field) =
      let name = Pos.unmark ef.name in
      match StrMap.find_opt name map with
      | None ->
          let map = StrMap.add name { ef with index } map in
          let index = index + 1 in
          (map, index)
      | Some old_ef ->
          let old_pos = Pos.get old_ef.name in
          let name_pos = Pos.get ef.name in
          Err.event_field_already_declared name old_pos name_pos
    in
    fst (List.fold_left fold (StrMap.empty, 0) evt_decl)
  in
  let prog_event_field_idxs =
    let fold name (ef : Com.event_field) map = IntMap.add ef.index name map in
    StrMap.fold fold prog_event_fields IntMap.empty
  in
  let prog_event_pos = decl_pos in
  { prog with prog_event_fields; prog_event_field_idxs; prog_event_pos }

let check_error (error : Mast.error_) (prog : program) : program =
  let famille = List.nth error.error_descr 0 in
  let code_bo = List.nth error.error_descr 1 in
  let sous_code = List.nth error.error_descr 2 in
  let libelle = List.nth error.error_descr 3 in
  let is_isf =
    match List.nth_opt error.error_descr 4 with
    | Some s -> s
    | None -> ("", Pos.no_pos)
  in
  let err =
    Com.Error.
      {
        name = error.Mast.error_name;
        typ = Pos.unmark error.Mast.error_typ;
        famille;
        code_bo;
        sous_code;
        is_isf;
        libelle;
      }
  in
  let name, name_pos = err.name in
  match StrMap.find_opt name prog.prog_errors with
  | Some old_err ->
      let old_pos = Pos.get old_err.name in
      Err.error_already_declared name old_pos name_pos
  | None ->
      let prog_errors = StrMap.add name err prog.prog_errors in
      { prog with prog_errors }

let check_domain (rov : rule_or_verif) (decl : 'a Mast.domain_decl)
    (dom_data : 'b) ((doms, syms) : 'b doms * syms) : 'b doms * syms =
  let dom_names =
    List.fold_left
      (fun dom_names (sl, sl_pos) ->
        let id = Com.DomainId.from_marked_list sl in
        Com.DomainIdMap.add id sl_pos dom_names)
      Com.DomainIdMap.empty decl.dom_names
  in
  let dom_id = Com.DomainIdMap.min_binding dom_names in
  let domain =
    Com.
      {
        dom_id;
        dom_names;
        dom_by_default = decl.dom_by_default;
        dom_min = DomainIdSet.from_marked_list_list decl.dom_parents;
        dom_max = DomainIdSet.empty;
        dom_rov = IntSet.empty;
        dom_data;
        dom_used = None;
      }
  in
  let dom_id_name, dom_id_pos = dom_id in
  let syms =
    Com.DomainIdMap.fold
      (fun name name_pos syms ->
        match Com.DomainIdMap.find_opt name syms with
        | Some (_, old_pos) -> Err.domain_already_declared rov old_pos name_pos
        | None ->
            let value = (dom_id_name, name_pos) in
            Com.DomainIdMap.add name value syms)
      dom_names syms
  in
  let syms =
    if decl.dom_by_default then
      match Com.DomainIdMap.find_opt Com.DomainId.empty syms with
      | Some (_, old_pos) ->
          Err.default_domain_already_declared rov old_pos dom_id_pos
      | None ->
          let value = (dom_id_name, Pos.no_pos) in
          Com.DomainIdMap.add Com.DomainId.empty value syms
    else syms
  in
  let doms = Com.DomainIdMap.add dom_id_name domain doms in
  (doms, syms)

let check_rule_dom_decl (decl : Mast.rule_domain_decl) (prog : program) :
    program =
  let dom_data = Com.{ rdom_computable = decl.Mast.dom_data.rdom_computable } in
  let doms_syms = (prog.prog_rdoms, prog.prog_rdom_syms) in
  let doms, syms = check_domain Rule decl dom_data doms_syms in
  { prog with prog_rdoms = doms; prog_rdom_syms = syms }

let mast_to_catvars (cs : Pos.t Com.CatVar.Map.t)
    (cats : Com.CatVar.data Com.CatVar.Map.t) : Pos.t Com.CatVar.Map.t =
  let filter_cats pred =
    Com.CatVar.Map.fold
      (fun cv (cvd : Com.CatVar.data) res ->
        if pred cv then Com.CatVar.Map.add cv cvd.pos res else res)
      cats Com.CatVar.Map.empty
  in
  let fold cv pos res =
    match cv with
    | Com.CatVar.Input set when StrSet.mem "*" set ->
        filter_cats (function Com.CatVar.Input _ -> true | _ -> false)
        |> Com.CatVar.Map.union (fun _ p _ -> Some p) res
    | Com.CatVar.Input _ ->
        if Com.CatVar.Map.mem cv cats then Com.CatVar.Map.add cv pos res
        else Err.unknown_variable_category pos
    | _ -> Com.CatVar.Map.add cv pos res
  in
  Com.CatVar.Map.fold fold cs Com.CatVar.Map.empty

let check_verif_dom_decl (decl : Mast.verif_domain_decl) (prog : program) :
    program =
  let vdom_auth =
    let rec aux vdom_auth = function
      | [] -> vdom_auth
      | l :: t ->
          let vcats =
            mast_to_catvars
              (Com.CatVar.Map.from_string_list l)
              prog.prog_var_cats
          in
          aux (Com.CatVar.Map.union (fun _ p _ -> Some p) vcats vdom_auth) t
    in
    aux Com.CatVar.Map.empty decl.Mast.dom_data.vdom_auth
  in
  let vdom_verifiable = decl.Mast.dom_data.vdom_verifiable in
  let dom_data = Com.{ vdom_auth; vdom_verifiable } in
  let doms_syms = (prog.prog_vdoms, prog.prog_vdom_syms) in
  let doms, syms = check_domain Verif decl dom_data doms_syms in
  { prog with prog_vdoms = doms; prog_vdom_syms = syms }

let complete_dom_decls (rov : rule_or_verif) ((doms, syms) : 'a doms * syms) :
    'a doms =
  let get_id id = Pos.unmark (Com.DomainIdMap.find id syms) in
  let get_dom id doms = Com.DomainIdMap.find (get_id id) doms in
  let module DomGraph :
    TopologicalSorting.GRAPH
      with type 'a t = 'a doms
       and type vertex = Com.DomainId.t
       and type edge = unit = struct
    type 'a t = 'a doms

    type vertex = Com.DomainId.t

    type edge = unit

    type 'a vertexMap = 'a Com.DomainIdMap.t

    let vertexMapEmpty = Com.DomainIdMap.empty

    let vertexMapAdd id value map = Com.DomainIdMap.add (get_id id) value map

    let vertexMapRemove id map = Com.DomainIdMap.remove (get_id id) map

    let vertexMapFindOpt id map = Com.DomainIdMap.find_opt (get_id id) map

    let vertexMapFold fold map res =
      Com.DomainIdMap.fold
        (fun id edge res -> fold (get_id id) edge res)
        map res

    let vertices doms =
      let get_vertex id _ nds = Com.DomainIdMap.add id None nds in
      Com.DomainIdMap.fold get_vertex doms Com.DomainIdMap.empty

    let edges doms id =
      Com.DomainIdSet.fold
        (fun id res -> Com.DomainIdMap.add id None res)
        (get_dom id doms).Com.dom_min Com.DomainIdMap.empty
  end in
  let module DomSorting = TopologicalSorting.Make (DomGraph) in
  let sorted_doms =
    try DomSorting.sort doms with
    | DomSorting.Cycle cycle -> Err.loop_in_domains rov (List.map fst cycle)
    | DomSorting.AutoCycle (id, _) ->
        let dom = get_dom id doms in
        let dom_id, dom_id_pos = dom.Com.dom_id in
        Err.domain_specialize_itself rov dom_id dom_id_pos
  in
  let doms =
    let set_min doms id =
      let dom = get_dom id doms in
      let dom_min =
        let fold parent_id res =
          let parent_dom = get_dom parent_id doms in
          let parent_id = Pos.unmark parent_dom.Com.dom_id in
          let dom_min = Com.DomainIdSet.map get_id parent_dom.Com.dom_min in
          Com.DomainIdSet.one parent_id
          |> Com.DomainIdSet.union dom_min
          |> Com.DomainIdSet.union res
        in
        Com.DomainIdSet.fold fold dom.Com.dom_min Com.DomainIdSet.empty
      in
      let dom = Com.{ dom with dom_min } in
      Com.DomainIdMap.add id dom doms
    in
    List.fold_left set_min doms sorted_doms
  in
  let doms =
    let set_max id dom doms =
      let fold min_id doms =
        let min_dom = Com.DomainIdMap.find min_id doms in
        let dom_max = Com.DomainIdSet.add id min_dom.Com.dom_max in
        let min_dom = Com.{ min_dom with dom_max } in
        Com.DomainIdMap.add min_id min_dom doms
      in
      Com.DomainIdSet.fold fold dom.Com.dom_min doms
    in
    Com.DomainIdMap.fold set_max doms doms
  in
  let doms =
    let add_sym name (id, _) doms =
      Com.DomainIdMap.add name (get_dom id doms) doms
    in
    Com.DomainIdMap.fold add_sym syms doms
  in
  match Com.DomainIdMap.find_opt Com.DomainId.empty doms with
  | None -> Err.no_default_domain rov
  | Some _ -> doms

let complete_rdom_decls (prog : program) : program =
  let prog_rdoms =
    let doms_syms = (prog.prog_rdoms, prog.prog_rdom_syms) in
    let prog_rdoms = complete_dom_decls Rule doms_syms in
    StrMap.fold
      (fun _ (m_seq, rdom_id) prog_rdoms ->
        let rdom = Com.DomainIdMap.find rdom_id prog_rdoms in
        Com.DomainIdSet.fold
          (fun rid prog_rdoms ->
            let rd = Com.DomainIdMap.find rid prog_rdoms in
            let rd =
              match rd.Com.dom_used with
              | Some _ -> rd
              | None -> { rd with Com.dom_used = Some m_seq }
            in
            Com.DomainIdMap.add rid rd prog_rdoms)
          (Com.DomainIdSet.add rdom_id rdom.Com.dom_min)
          prog_rdoms)
      prog.prog_rdom_calls prog_rdoms
  in
  { prog with prog_rdoms }

let complete_vdom_decls (prog : program) : program =
  let prog_vdoms =
    let doms_syms = (prog.prog_vdoms, prog.prog_vdom_syms) in
    let prog_vdoms = complete_dom_decls Verif doms_syms in
    StrMap.fold
      (fun _ (m_seq, vdom_id, _) prog_vdoms ->
        let vdom = Com.DomainIdMap.find vdom_id prog_vdoms in
        Com.DomainIdSet.fold
          (fun vid prog_vdoms ->
            let vd = Com.DomainIdMap.find vid prog_vdoms in
            let vd =
              match vd.Com.dom_used with
              | Some _ -> vd
              | None -> { vd with Com.dom_used = Some m_seq }
            in
            Com.DomainIdMap.add vid vd prog_vdoms)
          (Com.DomainIdSet.add vdom_id vdom.Com.dom_min)
          prog_vdoms)
      prog.prog_vdom_calls prog_vdoms
  in
  { prog with prog_vdoms }

type var_mem_type = Num | Table | Both

type var_env = {
  prog : program;
  tmp_vars : int option Pos.marked StrMap.t;
  (* ajouter tmp_tabs !!! *)
  ref_vars : Pos.t StrMap.t;
  res_var : string Pos.marked option;
}

let check_name_in_env env m_name =
  let name, pos = m_name in
  check_name_in_tgv env.prog m_name;
  let err old_pos = Err.variable_already_declared name old_pos pos in
  (match StrMap.find_opt name env.tmp_vars with
  | Some (_, old_pos) -> err old_pos
  | None -> ());
  (match StrMap.find_opt name env.ref_vars with
  | Some old_pos -> err old_pos
  | None -> ());
  match env.res_var with
  | Some (res_name, old_pos) when res_name = name -> err old_pos
  | Some _ | None -> ()

let rec fold_var_expr
    (fold_var : Com.m_var_name -> var_mem_type -> var_env -> 'a -> 'a)
    (is_filter : bool) (acc : 'a) (m_expr : Mast.m_expression) (env : var_env) :
    'a =
  let expr, expr_pos = m_expr in
  match expr with
  | TestInSet (_positive, e, values) ->
      let acc = fold_var_expr fold_var is_filter acc e env in
      List.fold_left
        (fun acc set_value ->
          match set_value with
          | Com.VarValue (a, a_pos) -> (
              if is_filter then Err.forbidden_expresion_in_filter a_pos;
              match a with
              | VarAccess m_v -> fold_var m_v Num env acc
              | TabAccess (m_v, m_i) ->
                  let acc = fold_var_expr fold_var is_filter acc m_i env in
                  fold_var m_v Table env acc
              | ConcAccess (_m_v, _m_if, i) ->
                  fold_var_expr fold_var is_filter acc i env
              | FieldAccess (ie, f, _) ->
                  let f_name, f_pos = f in
                  (match StrMap.find_opt f_name env.prog.prog_event_fields with
                  | Some ef when ef.is_var -> ()
                  | Some _ -> Err.event_field_is_not_a_reference f_name f_pos
                  | None -> Err.unknown_event_field f_name f_pos);
                  fold_var_expr fold_var is_filter acc ie env)
          | Com.FloatValue _ -> acc
          | Com.IntervalValue (bn, en) ->
              if Pos.unmark bn > Pos.unmark en then
                Err.wrong_interval_bounds (Pos.get bn);
              acc)
        acc values
  | Comparison (_op, e1, e2) ->
      let acc = fold_var_expr fold_var is_filter acc e1 env in
      fold_var_expr fold_var is_filter acc e2 env
  | Binop (_op, e1, e2) ->
      let acc = fold_var_expr fold_var is_filter acc e1 env in
      fold_var_expr fold_var is_filter acc e2 env
  | Unop (_op, e) -> fold_var_expr fold_var is_filter acc e env
  | Index ((access, _pos), e) -> (
      if is_filter then Err.forbidden_expresion_in_filter expr_pos;
      match access with
      | VarAccess m_v ->
          let acc = fold_var_expr fold_var is_filter acc e env in
          fold_var m_v Table env acc
      | TabAccess _ -> assert false
      | ConcAccess (_, _, i) -> fold_var_expr fold_var is_filter acc i env
      | FieldAccess (ie, f, _) ->
          let f_name, f_pos = f in
          (match StrMap.find_opt f_name env.prog.prog_event_fields with
          | Some ef when ef.is_var -> ()
          | Some _ -> Err.event_field_is_not_a_reference f_name f_pos
          | None -> Err.unknown_event_field f_name f_pos);
          let acc = fold_var_expr fold_var is_filter acc ie env in
          fold_var_expr fold_var is_filter acc e env)
  | Conditional (e1, e2, e3_opt) -> (
      let acc = fold_var_expr fold_var is_filter acc e1 env in
      let acc = fold_var_expr fold_var is_filter acc e2 env in
      match e3_opt with
      | Some e3 -> fold_var_expr fold_var is_filter acc e3 env
      | None -> acc)
  | FuncCall ((func_name, fpos), args) -> (
      let check_func arity =
        if arity > -1 && List.length args <> arity then
          Err.wrong_arity_of_function func_name arity expr_pos;
        List.fold_left
          (fun acc e -> fold_var_expr fold_var is_filter acc e env)
          acc args
      in
      match func_name with
      | Com.Multimax -> (
          if is_filter then Err.forbidden_expresion_in_filter expr_pos;
          match args with
          | [ expr; var_expr ] -> (
              let acc = fold_var_expr fold_var is_filter acc expr env in
              match var_expr with
              | Var (VarAccess m_v), _ -> fold_var m_v Table env acc
              | _ -> Err.second_arg_of_multimax (Pos.get var_expr))
          | _ -> Err.multimax_require_two_args expr_pos)
      | Com.SumFunc -> check_func (-1)
      | Com.VerifNumber -> check_func 0
      | Com.ComplNumber -> check_func 0
      | Com.AbsFunc -> check_func 1
      | Com.MinFunc -> check_func 2
      | Com.MaxFunc -> check_func 2
      | Com.GtzFunc -> check_func 1
      | Com.GtezFunc -> check_func 1
      | Com.NullFunc -> check_func 1
      | Com.ArrFunc -> check_func 1
      | Com.InfFunc -> check_func 1
      | Com.Supzero -> check_func 1
      | Com.PresentFunc ->
          if is_filter then Err.forbidden_expresion_in_filter expr_pos;
          check_func 1
      | Com.NbEvents ->
          if is_filter then Err.forbidden_expresion_in_filter expr_pos;
          check_func 0
      | Com.Func fn ->
          if is_filter then Err.forbidden_expresion_in_filter expr_pos;
          let fd =
            match StrMap.find_opt fn env.prog.prog_functions with
            | Some fd -> fd
            | None -> Err.function_does_not_exist fn fpos
          in
          check_func (List.length fd.target_args))
  | Literal _ -> acc
  | Var access -> (
      if is_filter then Err.variable_forbidden_in_filter expr_pos;
      match access with
      | VarAccess m_v -> fold_var m_v Num env acc
      | TabAccess (m_v, m_i) ->
          let acc = fold_var_expr fold_var is_filter acc m_i env in
          fold_var m_v Table env acc
      | ConcAccess (_, _, i) -> fold_var_expr fold_var is_filter acc i env
      | FieldAccess (e, f, _) -> (
          match StrMap.find_opt (Pos.unmark f) env.prog.prog_event_fields with
          | Some _ -> fold_var_expr fold_var is_filter acc e env
          | None -> Err.unknown_event_field (Pos.unmark f) (Pos.get f)))
  | NbCategory cs ->
      if not is_filter then Err.expression_only_in_filter expr_pos;
      let cats = mast_to_catvars cs env.prog.prog_var_cats in
      Com.CatVar.Map.iter
        (fun cat pos ->
          if not (Com.CatVar.Map.mem cat env.prog.prog_var_cats) then
            Err.unknown_domain Verif pos)
        cats;
      acc
  | Attribut ((access, _pos), a) -> (
      match access with
      | VarAccess m_v ->
          let name, var_pos = Pos.map_under_mark Com.get_normal_var m_v in
          (match StrMap.find_opt name env.prog.prog_vars with
          | Some id ->
              let var = IntMap.find id env.prog.prog_dict in
              let cat = Com.Var.cat var in
              if not (StrMap.mem (Pos.unmark a) (Com.Var.attrs var)) then
                Err.unknown_attribut_for_var cat (Pos.get a)
          | None -> (
              match StrMap.find_opt name env.tmp_vars with
              | Some _ -> Err.tmp_vars_have_no_attrs var_pos
              | None -> ()));
          fold_var m_v Both env acc
      | TabAccess (m_v, m_i) ->
          let name, var_pos = Pos.map_under_mark Com.get_normal_var m_v in
          (match StrMap.find_opt name env.prog.prog_vars with
          | Some id ->
              let var = IntMap.find id env.prog.prog_dict in
              let cat = Com.Var.cat var in
              if not (StrMap.mem (Pos.unmark a) (Com.Var.attrs var)) then
                Err.unknown_attribut_for_var cat (Pos.get a)
          | None -> (
              match StrMap.find_opt name env.tmp_vars with
              | Some _ -> Err.tmp_vars_have_no_attrs var_pos
              | None -> ()));
          let acc = fold_var_expr fold_var is_filter acc m_i env in
          fold_var m_v Table env acc
      | ConcAccess (_, _, i) ->
          if is_filter then Err.forbidden_expresion_in_filter expr_pos;
          fold_var_expr fold_var is_filter acc i env
      | FieldAccess (e, f, _) ->
          if is_filter then Err.forbidden_expresion_in_filter expr_pos;
          let f_name, f_pos = f in
          (match StrMap.find_opt f_name env.prog.prog_event_fields with
          | Some ef when ef.is_var ->
              let attr = Pos.unmark a in
              let fold _ (cvd : Com.CatVar.data) res =
                res || StrMap.mem attr cvd.attributs
              in
              if not (Com.CatVar.Map.fold fold env.prog.prog_var_cats false)
              then Err.unknown_attribut attr (Pos.get a)
          | Some _ -> Err.event_field_is_not_a_reference f_name f_pos
          | None -> Err.unknown_event_field f_name f_pos);
          fold_var_expr fold_var is_filter acc e env)
  | Size (access, _pos) -> (
      match access with
      | VarAccess m_v -> fold_var m_v Both env acc
      | TabAccess (m_v, m_i) ->
          let acc = fold_var_expr fold_var is_filter acc m_i env in
          fold_var m_v Table env acc
      | ConcAccess (_, _, i) ->
          if is_filter then Err.forbidden_expresion_in_filter expr_pos;
          fold_var_expr fold_var is_filter acc i env
      | FieldAccess (e, f, _) ->
          if is_filter then Err.forbidden_expresion_in_filter expr_pos;
          let f_name, f_pos = f in
          (match StrMap.find_opt f_name env.prog.prog_event_fields with
          | Some ef when ef.is_var -> ()
          | Some _ -> Err.event_field_is_not_a_reference f_name f_pos
          | None -> Err.unknown_event_field f_name f_pos);
          fold_var_expr fold_var is_filter acc e env)
  | NbAnomalies | NbDiscordances | NbInformatives | NbBloquantes ->
      if is_filter then Err.forbidden_expresion_in_filter expr_pos;
      acc
  | FuncCallLoop _ | Loop _ -> assert false

let get_var_mem_type (var : Com.m_var_name) (env : var_env) :
    var_mem_type Pos.marked =
  let var_data, var_pos = var in
  let vn = Com.get_normal_var var_data in
  let to_mem is_t = match is_t with Some _ -> Table | None -> Num in
  let find_tgv_var next () =
    match StrMap.find_opt vn env.prog.prog_vars with
    | Some id ->
        let v = IntMap.find id env.prog.prog_dict in
        Pos.same_pos_as (to_mem (Com.Var.is_table v)) (Com.Var.name v)
    | None -> next ()
  in
  let find_tmp_var next () =
    match StrMap.find_opt vn env.tmp_vars with
    | Some decl -> Pos.map_under_mark to_mem decl
    | None -> next ()
  in
  let find_ref next () =
    match StrMap.find_opt vn env.ref_vars with
    | Some decl_pos -> Pos.mark Both decl_pos
    | None -> next ()
  in
  let find_res () =
    match env.res_var with
    | Some (vr, decl_pos) when vr = vn -> Pos.mark Num decl_pos
    | Some _ | None -> Err.unknown_variable var_pos
  in
  () |> find_tgv_var @@ find_tmp_var @@ find_ref @@ find_res

let check_variable (var : Com.m_var_name) (idx_mem : var_mem_type)
    (env : var_env) : unit =
  let decl_mem, decl_pos = get_var_mem_type var env in
  match (decl_mem, idx_mem) with
  | Both, _ | _, Both | Num, Num | Table, Table -> ()
  | Num, Table -> Err.variable_used_as_table decl_pos (Pos.get var)
  | Table, Num -> Err.table_used_as_variable decl_pos (Pos.get var)

let check_expression (is_filter : bool) (m_expr : Mast.expression Pos.marked)
    (env : var_env) : unit =
  let fold_var var idx_mem env _acc = check_variable var idx_mem env in
  fold_var_expr fold_var is_filter () m_expr env

let get_compute_id_str (instr : Mast.instruction) (prog : program) : string =
  let buf = Buffer.create 100 in
  Buffer.add_string buf prog.prog_prefix;
  let add_sml buf sml =
    let id = Com.DomainId.from_marked_list (Pos.unmark sml) in
    let add s =
      String.iter
        (function
          | '_' -> Buffer.add_string buf "__" | c -> Buffer.add_char buf c)
        s
    in
    Com.DomainId.iter
      (fun s ->
        Buffer.add_char buf '_';
        add s)
      id;
    id
  in
  (match instr with
  | Com.ComputeDomain l -> (
      Buffer.add_string buf "_rules";
      let id = add_sml buf l in
      match Com.DomainIdMap.find_opt id prog.prog_rdom_syms with
      | Some (dom_id, _) ->
          let rdom = Com.DomainIdMap.find dom_id prog.prog_rdoms in
          if not rdom.Com.dom_data.rdom_computable then
            Err.rule_domain_not_computable (Pos.get l)
      | None -> Err.unknown_domain Rule (Pos.get l))
  | Com.ComputeChaining (ch_name, ch_pos) -> (
      Buffer.add_string buf "_chaining_";
      Buffer.add_string buf ch_name;
      match StrMap.find_opt ch_name prog.prog_chainings with
      | Some _ -> ()
      | None -> Err.unknown_chaining ch_pos)
  | Com.ComputeVerifs (l, _) -> (
      Buffer.add_string buf "_verifs";
      let id = add_sml buf l in
      Buffer.add_char buf '_';
      let cpt = StrMap.cardinal prog.prog_vdom_calls in
      Buffer.add_string buf (Format.sprintf "%d" cpt);
      match Com.DomainIdMap.find_opt id prog.prog_vdom_syms with
      | Some (dom_id, _) ->
          let vdom = Com.DomainIdMap.find dom_id prog.prog_vdoms in
          if not vdom.Com.dom_data.vdom_verifiable then
            Err.verif_domain_not_verifiable (Pos.get l)
      | None -> Err.unknown_domain Verif (Pos.get l))
  | _ -> assert false);
  Buffer.contents buf

let cats_variable_from_decl_list (l : Mast.var_category_id list)
    (cats : Com.CatVar.data Com.CatVar.Map.t) : Pos.t Com.CatVar.Map.t =
  let rec aux res = function
    | [] -> res
    | l :: t ->
        let vcats = mast_to_catvars (Com.CatVar.Map.from_string_list l) cats in
        aux (Com.CatVar.Map.union (fun _ p _ -> Some p) vcats res) t
  in
  aux Com.CatVar.Map.empty l

let rec check_instructions (instrs : Mast.instruction Pos.marked list)
    (is_rule : bool) (env : var_env) :
    program * Mast.instruction Pos.marked list =
  let check_it_var env var =
    let m_name = Pos.same_pos_as (Com.get_normal_var (Pos.unmark var)) var in
    check_name_in_env env m_name;
    m_name
  in
  let rec aux (env, res) = function
    | [] -> (env, List.rev res)
    | m_instr :: il -> (
        let instr, instr_pos = m_instr in
        match instr with
        | Com.Affectation (f, _) -> (
            match f with
            | Com.SingleFormula (VarDecl (m_access, idx, e)) -> (
                let access = Pos.unmark m_access in
                (match idx with
                | Some ei -> check_expression false ei env
                | None -> ());
                check_expression false e env;
                match access with
                | VarAccess m_v ->
                    let idx_mem = match idx with None -> Num | _ -> Table in
                    check_variable m_v idx_mem env;
                    aux (env, m_instr :: res) il
                | TabAccess (m_v, m_i) ->
                    check_variable m_v Table env;
                    check_expression false m_i env;
                    aux (env, m_instr :: res) il
                | ConcAccess (_, _, i) ->
                    if is_rule then Err.insruction_forbidden_in_rules instr_pos;
                    check_expression false i env;
                    aux (env, m_instr :: res) il
                | FieldAccess (i, f, _) ->
                    if is_rule then Err.insruction_forbidden_in_rules instr_pos;
                    let f_name, f_pos = f in
                    (match
                       StrMap.find_opt f_name env.prog.prog_event_fields
                     with
                    | Some ef when (not ef.is_var) && idx <> None ->
                        Err.event_field_is_not_a_reference f_name f_pos
                    | Some _ -> ()
                    | None -> Err.unknown_event_field f_name f_pos);
                    check_expression false i env;
                    aux (env, m_instr :: res) il)
            | Com.SingleFormula (EventFieldRef (i, f, _, v)) ->
                if is_rule then Err.insruction_forbidden_in_rules instr_pos;
                let f_name, f_pos = f in
                (match StrMap.find_opt f_name env.prog.prog_event_fields with
                | Some ef when ef.is_var -> ()
                | Some _ -> Err.event_field_is_not_a_reference f_name f_pos
                | None -> Err.unknown_event_field f_name f_pos);
                check_expression false i env;
                check_variable v Both env;
                aux (env, m_instr :: res) il
            | Com.MultipleFormulaes _ -> assert false)
        | Com.IfThenElse (expr, i_then, i_else) ->
            check_expression false expr env;
            let prog, res_then = check_instructions i_then is_rule env in
            let env = { env with prog } in
            let prog, res_else = check_instructions i_else is_rule env in
            let env = { env with prog } in
            let res_instr = Com.IfThenElse (expr, res_then, res_else) in
            aux (env, (res_instr, instr_pos) :: res) il
        | Com.WhenDoElse (wdl, ed) ->
            let rec wde (env, res) = function
              | (expr, dl, pos) :: l ->
                  check_expression false expr env;
                  let prog, res_do = check_instructions dl is_rule env in
                  let env = { env with prog } in
                  let res = (expr, res_do, pos) :: res in
                  wde (env, res) l
              | [] ->
                  let prog, res_ed =
                    check_instructions (Pos.unmark ed) is_rule env
                  in
                  let env = { env with prog } in
                  let ed' = Pos.same_pos_as res_ed ed in
                  let res = Com.WhenDoElse (List.rev res, ed') in
                  (env, res)
            in
            let env, wde_res = wde (env, []) wdl in
            aux (env, (wde_res, instr_pos) :: res) il
        | Com.ComputeDomain (rdom_list, rdom_pos) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let tname = get_compute_id_str instr env.prog in
            let rdom_id =
              let id = Com.DomainId.from_marked_list rdom_list in
              Pos.unmark (Com.DomainIdMap.find id env.prog.prog_rdom_syms)
            in
            let seq, prog = get_seq env.prog in
            let prog_rdom_calls =
              let used_data = ((seq, rdom_pos), rdom_id) in
              StrMap.add tname used_data prog.prog_rdom_calls
            in
            let prog = { prog with prog_rdom_calls } in
            let env = { env with prog } in
            let res_instr = Com.ComputeTarget ((tname, Pos.no_pos), []) in
            aux (env, (res_instr, instr_pos) :: res) il
        | Com.ComputeChaining _ ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let tname = get_compute_id_str instr env.prog in
            let res_instr = Com.ComputeTarget ((tname, Pos.no_pos), []) in
            aux (env, (res_instr, instr_pos) :: res) il
        | Com.ComputeVerifs ((vdom_list, vdom_pos), expr) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let tname = get_compute_id_str instr env.prog in
            let vdom_id =
              let id = Com.DomainId.from_marked_list vdom_list in
              Pos.unmark (Com.DomainIdMap.find id env.prog.prog_vdom_syms)
            in
            let seq, prog = get_seq env.prog in
            check_expression true expr env;
            let prog_vdom_calls =
              let used_data = ((seq, vdom_pos), vdom_id, expr) in
              StrMap.add tname used_data prog.prog_vdom_calls
            in
            let prog = { prog with prog_vdom_calls } in
            let env = { env with prog } in
            let res_instr = Com.ComputeTarget ((tname, Pos.no_pos), []) in
            aux (env, (res_instr, instr_pos) :: res) il
        | Com.VerifBlock instrs ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let prog, res_instrs = check_instructions instrs is_rule env in
            let env = { env with prog } in
            let res_instr = Com.VerifBlock res_instrs in
            aux (env, (res_instr, instr_pos) :: res) il
        | Com.ComputeTarget ((tn, tpos), targs) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            (match StrMap.find_opt tn env.prog.prog_targets with
            | None -> Err.unknown_target tn tpos
            | Some target ->
                let nb_args = List.length target.target_args in
                if List.length targs <> nb_args then
                  Err.wrong_number_of_args nb_args tpos);
            List.iter (fun var -> check_variable var Both env) targs;
            aux (env, m_instr :: res) il
        | Com.Print (_std, args) ->
            List.iter
              (fun arg ->
                match Pos.unmark arg with
                | Com.PrintString _ -> ()
                | Com.PrintName v | Com.PrintAlias v ->
                    check_variable v Both env
                | Com.PrintConcName (_, _, i) | Com.PrintConcAlias (_, _, i) ->
                    check_expression false i env
                | Com.PrintEventName (e, f, _) | Com.PrintEventAlias (e, f, _)
                  -> (
                    let f_name, f_pos = f in
                    match StrMap.find_opt f_name env.prog.prog_event_fields with
                    | Some ef when ef.is_var -> check_expression false e env
                    | Some _ -> Err.event_field_is_not_a_reference f_name f_pos
                    | None -> Err.unknown_event_field f_name f_pos)
                | Com.PrintIndent e -> check_expression false e env
                | Com.PrintExpr (e, _min, _max) -> check_expression false e env)
              args;
            aux (env, m_instr :: res) il
        | Com.Iterate (var, vars, var_params, instrs) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let var_name, var_pos = check_it_var env var in
            let env' =
              { env with ref_vars = StrMap.add var_name var_pos env.ref_vars }
            in
            ignore
              (List.fold_left
                 (fun seen var ->
                   let var_pos = Pos.get var in
                   let var_name = Com.get_normal_var (Pos.unmark var) in
                   check_variable var Both env;
                   match StrMap.find_opt var_name seen with
                   | None -> StrMap.add var_name var_pos seen
                   | Some old_pos ->
                       Err.variable_already_specified var_name old_pos var_pos)
                 StrMap.empty vars);
            List.iter
              (fun (vcats, expr) ->
                ignore (mast_to_catvars vcats env.prog.prog_var_cats);
                check_expression false expr env')
              var_params;
            let prog, res_instrs = check_instructions instrs is_rule env' in
            let env = { env with prog } in
            let res_instr = Com.Iterate (var, vars, var_params, res_instrs) in
            aux (env, (res_instr, instr_pos) :: res) il
        | Com.Iterate_values (var, var_intervals, instrs) ->
            let var_name, var_pos = check_it_var env var in
            let tmp_vars = StrMap.add var_name (None, var_pos) env.tmp_vars in
            let env' = { env with tmp_vars } in
            List.iter
              (fun (e0, e1, step) ->
                check_expression false e0 env;
                check_expression false e1 env;
                check_expression false step env)
              var_intervals;
            let prog, res_instrs = check_instructions instrs is_rule env' in
            let env = { env with prog } in
            let res_instr =
              Com.Iterate_values (var, var_intervals, res_instrs)
            in
            aux (env, (res_instr, instr_pos) :: res) il
        | Com.Restore (vars, var_params, evts, evtfs, instrs) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            ignore
              (List.fold_left
                 (fun seen var ->
                   let var_pos = Pos.get var in
                   let var_name = Com.get_normal_var (Pos.unmark var) in
                   check_variable var Both env;
                   match StrMap.find_opt var_name seen with
                   | None -> StrMap.add var_name var_pos seen
                   | Some old_pos ->
                       Err.variable_already_specified var_name old_pos var_pos)
                 StrMap.empty vars);
            List.iter
              (fun (var, vcats, expr) ->
                let var_name, var_pos = check_it_var env var in
                ignore (mast_to_catvars vcats env.prog.prog_var_cats);
                let ref_vars = StrMap.add var_name var_pos env.ref_vars in
                let env = { env with ref_vars } in
                check_expression false expr env)
              var_params;
            List.iter (fun expr -> check_expression false expr env) evts;
            List.iter
              (fun (var, expr) ->
                let vname, vpos = check_it_var env var in
                let tmp_vars = StrMap.add vname (None, vpos) env.tmp_vars in
                let env = { env with tmp_vars } in
                check_expression false expr env)
              evtfs;
            let prog, res_instrs = check_instructions instrs is_rule env in
            let env = { env with prog } in
            let res_instr =
              Com.Restore (vars, var_params, evts, evtfs, res_instrs)
            in
            aux (env, (res_instr, instr_pos) :: res) il
        | Com.ArrangeEvents (sort, filter, add, instrs) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            (match sort with
            | Some (var0, var1, expr) ->
                let var0_name, var0_pos = check_it_var env var0 in
                let var1_name, var1_pos = check_it_var env var1 in
                let tmp_vars =
                  env.tmp_vars
                  |> StrMap.add var0_name (None, var0_pos)
                  |> StrMap.add var1_name (None, var1_pos)
                in
                let env = { env with tmp_vars } in
                check_expression false expr env
            | None -> ());
            (match filter with
            | Some (var, expr) ->
                let vname, vpos = check_it_var env var in
                let tmp_vars = StrMap.add vname (None, vpos) env.tmp_vars in
                let env = { env with tmp_vars } in
                check_expression false expr env
            | None -> ());
            (match add with
            | Some expr -> check_expression false expr env
            | None -> ());
            let prog, res_instrs = check_instructions instrs is_rule env in
            let env = { env with prog } in
            let res_instr = Com.ArrangeEvents (sort, filter, add, res_instrs) in
            aux (env, (res_instr, instr_pos) :: res) il
        | Com.RaiseError (m_err, m_var_opt) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let err_name, err_pos = m_err in
            (match StrMap.find_opt err_name env.prog.prog_errors with
            | None -> Err.unknown_error err_pos
            | Some _ -> ());
            (match m_var_opt with
            | Some (var_name, var_pos) -> (
                if
                  (not (StrMap.mem var_name env.tmp_vars))
                  && not (StrMap.mem var_name env.ref_vars)
                then
                  match StrMap.find_opt var_name env.prog.prog_vars with
                  | None -> Err.unknown_variable var_pos
                  | Some _ -> ())
            | None -> ());
            aux (env, m_instr :: res) il
        | Com.CleanErrors | Com.ExportErrors | Com.FinalizeErrors ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            aux (env, m_instr :: res) il)
  in
  let env, res = aux (env, []) instrs in
  (env.prog, res)

let inout_variable (var : Com.m_var_name) : Pos.t StrMap.t =
  let vn = Com.get_normal_var (Pos.unmark var) in
  StrMap.one vn (Pos.get var)

let inout_expression (m_expr : Mast.expression Pos.marked) (env : var_env) :
    Pos.t StrMap.t =
  let fold_var var _idx_mem _env acc =
    StrMap.union_snd (inout_variable var) acc
  in
  fold_var_expr fold_var false StrMap.empty m_expr env

let rec inout_instructions (instrs : Mast.m_instruction list) (env : var_env) :
    Pos.t StrMap.t * Pos.t StrMap.t * Pos.t list StrMap.t =
  let check_it_var env var =
    let m_name = Pos.same_pos_as (Com.get_normal_var (Pos.unmark var)) var in
    check_name_in_env env m_name;
    m_name
  in
  let diff_map m0 m1 =
    let filter vn _ = not (StrMap.mem vn m1) in
    StrMap.filter filter m0
  in
  let merge_seq_defs map0 map1 =
    let merge _vn lo0 lo1 =
      match (lo0, lo1) with
      | None, None -> None
      | None, Some l | Some l, None -> Some l
      | Some l0, Some l1 -> Some (l1 @ l0)
    in
    StrMap.merge merge map0 map1
  in
  let merge_par_defs map0 map1 =
    let merge _vn lo0 lo1 =
      match (lo0, lo1) with
      | None, None -> None
      | None, Some l | Some l, None -> Some l
      | Some l0, Some _l1 -> Some l0
    in
    StrMap.merge merge map0 map1
  in
  let rec aux (env, in_vars, out_vars, def_vars) = function
    | [] -> (env, in_vars, out_vars, def_vars)
    | m_instr :: il -> (
        let instr, instr_pos = m_instr in
        match instr with
        | Com.Affectation (f, _) -> (
            match f with
            | Com.SingleFormula (VarDecl (m_access, idx, e)) -> (
                let access, access_pos = m_access in
                let in_vars_index =
                  match idx with
                  | Some ei -> inout_expression ei env
                  | None -> StrMap.empty
                in
                let in_vars_expr = inout_expression e env in
                let in_vars_aff = StrMap.union_fst in_vars_index in_vars_expr in
                match access with
                | VarAccess m_v ->
                    let out_vars_lvalue = inout_variable m_v in
                    let in_vars =
                      StrMap.union_fst in_vars (diff_map in_vars_aff out_vars)
                    in
                    let out_vars = StrMap.union_snd out_vars_lvalue out_vars in
                    let def_vars =
                      let vn = Com.get_normal_var @@ Pos.unmark m_v in
                      let def_list =
                        match StrMap.find_opt vn def_vars with
                        | None -> [ access_pos ]
                        | Some l -> access_pos :: l
                      in
                      StrMap.add vn def_list def_vars
                    in
                    aux (env, in_vars, out_vars, def_vars) il
                | TabAccess (m_v, m_i) ->
                    let out_vars_lvalue = inout_variable m_v in
                    let in_vars_i = inout_expression m_i env in
                    let in_vars_aff = StrMap.union_fst in_vars_i in_vars_aff in
                    let in_vars =
                      StrMap.union_fst in_vars (diff_map in_vars_aff out_vars)
                    in
                    let out_vars = StrMap.union_fst out_vars_lvalue out_vars in
                    let def_vars =
                      let vn = Com.get_normal_var (Pos.unmark m_v) in
                      let def_list =
                        match StrMap.find_opt vn def_vars with
                        | None -> [ access_pos ]
                        | Some l -> access_pos :: l
                      in
                      StrMap.add vn def_list def_vars
                    in
                    aux (env, in_vars, out_vars, def_vars) il
                | ConcAccess _ | FieldAccess _ ->
                    Err.insruction_forbidden_in_rules instr_pos)
            | Com.SingleFormula (EventFieldRef _) ->
                Err.insruction_forbidden_in_rules instr_pos
            | Com.MultipleFormulaes _ -> assert false)
        | Com.IfThenElse (expr, i_then, i_else) ->
            let in_expr = inout_expression expr env in
            let in_then, out_then, def_then = inout_instructions i_then env in
            let in_else, out_else, def_else = inout_instructions i_else env in
            let in_vars =
              in_vars |> StrMap.union_snd in_expr |> StrMap.union_snd in_then
              |> StrMap.union_snd in_else
            in
            let out_vars =
              out_vars |> StrMap.union_snd out_then |> StrMap.union_snd out_else
            in
            let def_vars =
              merge_seq_defs def_vars (merge_par_defs def_then def_else)
            in
            aux (env, in_vars, out_vars, def_vars) il
        | Com.WhenDoElse (wdl, ed) ->
            let rec wde (env, in_vars, out_vars, def_vars) = function
              | (expr, dl, _pos) :: l ->
                  let in_expr = inout_expression expr env in
                  let in_do, out_do, def_do = inout_instructions dl env in
                  let in_vars =
                    in_vars |> StrMap.union_snd in_expr
                    |> StrMap.union_snd in_do
                  in
                  let out_vars = out_vars |> StrMap.union_snd out_do in
                  let def_vars = merge_par_defs def_vars def_do in
                  wde (env, in_vars, out_vars, def_vars) l
              | [] ->
                  let in_ed, out_ed, def_ed =
                    inout_instructions (Pos.unmark ed) env
                  in
                  let in_vars = in_vars |> StrMap.union_snd in_ed in
                  let out_vars = out_vars |> StrMap.union_snd out_ed in
                  let def_vars = merge_par_defs def_vars def_ed in
                  (env, in_vars, out_vars, def_vars)
            in
            let env, in_vars, out_vars, def_vars_wde =
              wde (env, in_vars, out_vars, StrMap.empty) wdl
            in
            let def_vars = merge_seq_defs def_vars def_vars_wde in
            aux (env, in_vars, out_vars, def_vars) il
        | Com.ComputeDomain _ | Com.ComputeChaining _ | Com.ComputeVerifs _
        | Com.VerifBlock _ | Com.ComputeTarget _ ->
            Err.insruction_forbidden_in_rules instr_pos
        | Com.Print _ -> aux (env, in_vars, out_vars, def_vars) il
        | Com.Iterate _ -> Err.insruction_forbidden_in_rules instr_pos
        | Com.Iterate_values (var, var_intervals, instrs) ->
            let var_name, var_pos = check_it_var env var in
            let env' =
              {
                env with
                tmp_vars = StrMap.add var_name (None, var_pos) env.tmp_vars;
              }
            in
            let in_exprs =
              List.fold_left
                (fun in_exprs (e0, e1, step) ->
                  in_exprs
                  |> StrMap.union_snd (inout_expression e0 env)
                  |> StrMap.union_snd (inout_expression e1 env)
                  |> StrMap.union_snd (inout_expression step env))
                StrMap.empty var_intervals
            in
            let in_instrs, out_instrs, def_instrs =
              inout_instructions instrs env'
            in
            let in_vars =
              in_vars
              |> StrMap.union_snd
                   (in_exprs |> StrMap.union_snd in_instrs
                  |> StrMap.remove var_name)
            in
            let out_vars =
              out_vars |> StrMap.union_snd (out_instrs |> StrMap.remove var_name)
            in
            let def_vars = merge_seq_defs def_vars def_instrs in
            aux (env, in_vars, out_vars, def_vars) il
        | Com.Restore _ -> Err.insruction_forbidden_in_rules instr_pos
        | Com.ArrangeEvents _ -> Err.insruction_forbidden_in_rules instr_pos
        | Com.RaiseError _ -> Err.insruction_forbidden_in_rules instr_pos
        | Com.CleanErrors | Com.ExportErrors | Com.FinalizeErrors ->
            Err.insruction_forbidden_in_rules instr_pos)
  in
  let env, in_vars, out_vars, def_vars =
    aux (env, StrMap.empty, StrMap.empty, StrMap.empty) instrs
  in
  StrMap.iter
    (fun vn l ->
      if List.length l > 1 && not (is_vartmp vn) then
        Errors.print_multispanned_warning
          (Format.asprintf
             "Variable %s is defined more than once in the same rule" vn)
          (List.map (fun pos -> (None, pos)) (List.rev l)))
    (* List.rev for purely cosmetic reasons *)
    def_vars;
  let tmp_vars = StrMap.map snd env.tmp_vars in
  let in_vars = diff_map in_vars tmp_vars in
  let out_vars = diff_map out_vars tmp_vars in
  let def_vars = diff_map def_vars tmp_vars in
  (in_vars, out_vars, def_vars)

let check_target (is_function : bool) (t : Mast.target) (prog : program) :
    program =
  let target_name = t.target_name in
  let tname, tpos = target_name in
  if Com.Func tname <> Pos.unmark (Parse_utils.parse_function_name target_name)
  then Err.is_base_function tname tpos;
  (match StrMap.find_opt tname prog.prog_targets with
  | Some { target_name = _, old_pos; _ } ->
      Err.target_already_declared tname old_pos tpos
  | None -> ());
  let target_file = Some (get_target_file tpos) in
  let target_apps =
    (* Already checked during preprocessing *)
    t.target_apps
  in
  let target, prog =
    let target_tmp_vars =
      let vars =
        List.fold_left
          (fun vars (((vn, vpos) as m_v), sz) ->
            let check_tmp vars (vn, vpos) =
              let err old_pos =
                Err.temporary_variable_already_declared vn old_pos vpos
              in
              match StrMap.find_opt vn vars with
              | Some var -> err (Pos.get (Com.Var.name var))
              | None -> ()
            in
            check_name_in_tgv prog m_v;
            check_tmp vars m_v;
            let size = Option.map Pos.unmark (Mast.get_table_size_opt sz) in
            match size with
            | None ->
                let var =
                  Com.Var.new_temp ~name:m_v ~is_table:None ~loc_int:0
                in
                StrMap.add vn var vars
            | Some sz_int ->
                let iFmt = String.map (fun _ -> '0') (Pp.spr "%d" sz_int) in
                let rec loop vars i =
                  if i >= sz_int then vars
                  else
                    let iName = Strings.concat_int vn iFmt i in
                    let m_iName = Pos.mark iName vpos in
                    check_tmp vars m_iName;
                    let var =
                      Com.Var.new_temp ~name:m_iName ~is_table:None ~loc_int:0
                    in
                    let vars = StrMap.add iName var vars in
                    loop vars (i + 1)
                in
                loop vars 0)
          StrMap.empty t.target_tmp_vars
      in
      List.fold_left
        (fun vars (((vn, _vpos) as m_v), sz) ->
          let size = Option.map Pos.unmark (Mast.get_table_size_opt sz) in
          match size with
          | None -> vars
          | Some sz_int ->
              let is_table =
                let iFmt = String.map (fun _ -> '0') (Pp.spr "%d" sz_int) in
                let init i =
                  let iName = Strings.concat_int vn iFmt i in
                  StrMap.find iName vars
                in
                Some (Array.init sz_int init)
              in
              let var = Com.Var.new_temp ~name:m_v ~is_table ~loc_int:0 in
              StrMap.add vn var vars)
        vars t.target_tmp_vars
    in
    let target_args =
      let fold target_args m_v =
        check_name_in_tgv prog m_v;
        check_name_in_tmp target_tmp_vars m_v;
        let var =
          if is_function then Com.Var.new_arg ~name:m_v ~loc_int:0
          else Com.Var.new_ref ~name:m_v ~loc_int:0
        in
        var :: target_args
      in
      List.rev @@ List.fold_left fold [] t.target_args
    in
    let target_result =
      match t.target_result with
      | Some m_name ->
          if not is_function then Err.target_must_not_have_a_result tname tpos;
          check_name_in_tgv prog m_name;
          check_name_in_tmp target_tmp_vars m_name;
          check_name_in_args t.target_args m_name;
          Some (Com.Var.new_res ~name:m_name)
      | None ->
          if is_function then Err.function_result_missing tname tpos;
          None
    in
    let tmp_vars =
      let map var =
        let size = Option.map Array.length (Com.Var.is_table var) in
        Pos.same_pos_as size (Com.Var.name var)
      in
      StrMap.map map target_tmp_vars
    in
    let ref_vars =
      List.fold_left
        (fun res (vn, vpos) -> StrMap.add vn vpos res)
        StrMap.empty t.target_args
    in
    let res_var = t.target_result in
    let prog, target_prog =
      let env = { prog; tmp_vars; ref_vars; res_var } in
      let prog, target_prog =
        check_instructions t.target_prog is_function env
      in
      if is_function then (
        let in_vars, out_vars, _ = inout_instructions target_prog env in
        let vr = Pos.unmark (Option.get t.target_result) in
        let bad_in_vars =
          List.fold_left
            (fun res (vn, _) -> StrMap.remove vn res)
            in_vars t.target_args
          |> StrMap.remove vr
        in
        let bad_out_vars = StrMap.remove vr out_vars in
        (if StrMap.card bad_in_vars > 0 then
         let vn, vpos = StrMap.min_binding bad_in_vars in
         Err.forbidden_in_var_in_function vn tname vpos);
        if StrMap.card bad_out_vars > 0 then
          let vn, vpos = StrMap.min_binding bad_out_vars in
          Err.forbidden_out_var_in_function vn tname vpos);
      let target_prog =
        let map =
          Com.m_instr_map_var (Pos.map_under_mark Com.get_normal_var) Fun.id
        in
        List.map map target_prog
      in
      (prog, target_prog)
    in
    let target =
      Com.
        {
          target_name;
          target_file;
          target_apps;
          target_args;
          target_result;
          target_tmp_vars;
          target_nb_tmps = 0;
          target_sz_tmps = 0;
          target_nb_refs = 0;
          target_prog;
        }
    in
    (target, prog)
  in
  if is_function then
    let prog_functions = StrMap.add tname target prog.prog_functions in
    { prog with prog_functions }
  else
    let prog_targets = StrMap.add tname target prog.prog_targets in
    { prog with prog_targets }

let check_rule (r : Mast.rule) (prog : program) : program =
  let id, id_pos = r.Mast.rule_number in
  let rule_id = (id, id_pos) in
  let rule_apps =
    (* Already checked during preprocessing *)
    StrMap.map (function _, pos -> pos) r.Mast.rule_apps
  in
  let rdom_id =
    Com.DomainId.from_marked_list (Pos.unmark r.Mast.rule_tag_names)
  in
  let rule_domain, rule_domain_pos =
    let rid, rid_pos =
      match Com.DomainIdMap.find_opt rdom_id prog.prog_rdom_syms with
      | Some m_rid -> m_rid
      | None -> Err.unknown_domain Rule (Pos.get r.Mast.rule_tag_names)
    in
    let rule_domain = Com.DomainIdMap.find rid prog.prog_rdoms in
    (rule_domain, rid_pos)
  in
  let rule_chains, prog_chainings =
    let fold _ (ch, chpos) (rule_chains, prog_chainings) =
      (* Already checked during preprocessing *)
      let chain = StrMap.find ch prog.prog_chainings in
      let chain_rules =
        IntMap.add id (rule_domain, rule_domain_pos) chain.chain_rules
      in
      let chain = { chain with chain_rules } in
      let rule_chains = StrMap.add ch chpos rule_chains in
      let prog_chainings = StrMap.add ch chain prog_chainings in
      (rule_chains, prog_chainings)
    in
    StrMap.fold fold r.rule_chainings (StrMap.empty, prog.prog_chainings)
  in
  let rule_tmp_vars =
    let vars =
      List.fold_left
        (fun vars (((vn, vpos) as m_v), sz) ->
          let check_tmp vars (vn, vpos) =
            let err old_pos =
              Err.temporary_variable_already_declared vn old_pos vpos
            in
            match StrMap.find_opt vn vars with
            | Some var -> err (Pos.get (Com.Var.name var))
            | None -> ()
          in
          check_name_in_tgv prog m_v;
          check_tmp vars m_v;
          let size = Option.map Pos.unmark (Mast.get_table_size_opt sz) in
          match size with
          | None ->
              let var = Com.Var.new_temp ~name:m_v ~is_table:None ~loc_int:0 in
              StrMap.add vn var vars
          | Some sz_int ->
              let iFmt = String.map (fun _ -> '0') (Pp.spr "%d" sz_int) in
              let rec loop vars i =
                if i >= sz_int then vars
                else
                  let iName = Strings.concat_int vn iFmt i in
                  let m_iName = Pos.mark iName vpos in
                  check_tmp vars m_iName;
                  let var =
                    Com.Var.new_temp ~name:m_iName ~is_table:None ~loc_int:0
                  in
                  let vars = StrMap.add iName var vars in
                  loop vars (i + 1)
              in
              loop vars 0)
        StrMap.empty r.rule_tmp_vars
    in
    List.fold_left
      (fun vars (((vn, _vpos) as m_v), sz) ->
        let size = Option.map Pos.unmark (Mast.get_table_size_opt sz) in
        match size with
        | None -> vars
        | Some sz_int ->
            let is_table =
              let iFmt = String.map (fun _ -> '0') (Pp.spr "%d" sz_int) in
              let init i =
                let iName = Strings.concat_int vn iFmt i in
                StrMap.find iName vars
              in
              Some (Array.init sz_int init)
            in
            let var = Com.Var.new_temp ~name:m_v ~is_table ~loc_int:0 in
            StrMap.add vn var vars)
      vars r.rule_tmp_vars
  in
  let tmp_vars =
    let map var =
      let size = Option.map Array.length (Com.Var.is_table var) in
      Pos.same_pos_as size (Com.Var.name var)
    in
    StrMap.map map rule_tmp_vars
  in
  let rule_instrs = r.Mast.rule_formulaes in
  let env = { prog; tmp_vars; ref_vars = StrMap.empty; res_var = None } in
  let prog, rule_instrs = check_instructions rule_instrs true env in
  let rule_in_vars, rule_out_vars =
    let in_vars, out_vars, _ = inout_instructions rule_instrs env in
    (StrMap.keySet in_vars, out_vars)
  in
  let rule_seq, prog = get_seq prog in
  let rule =
    {
      rule_id;
      rule_apps;
      rule_domain;
      rule_chains;
      rule_tmp_vars;
      rule_instrs;
      rule_in_vars;
      rule_out_vars;
      rule_seq;
    }
  in
  (match IntMap.find_opt id prog.prog_rules with
  | Some r ->
      let rule_pos = Pos.get r.rule_id in
      Err.rov_already_defined Rule id rule_pos id_pos
  | None -> ());
  let prog_rules = IntMap.add id rule prog.prog_rules in
  { prog with prog_rules; prog_chainings }

let convert_rules (prog : program) : program =
  let prog_targets =
    IntMap.fold
      (fun id rule prog_targets ->
        let tname = Format.sprintf "%s_regle_%d" prog.prog_prefix id in
        let target_file = Some (get_target_file (Pos.get rule.rule_id)) in
        let target_prog =
          let map =
            Com.m_instr_map_var (Pos.map_under_mark Com.get_normal_var) Fun.id
          in
          List.map map rule.rule_instrs
        in
        let target =
          Com.
            {
              target_name = (tname, Pos.no_pos);
              target_file;
              target_apps = StrMap.mapi (fun a p -> (a, p)) prog.prog_app;
              target_args = [];
              target_result = None;
              target_tmp_vars = rule.rule_tmp_vars;
              target_prog;
              target_nb_tmps = 0;
              target_sz_tmps = 0;
              target_nb_refs = 0;
            }
        in
        StrMap.add tname target prog_targets)
      prog.prog_rules prog.prog_targets
  in
  { prog with prog_targets }

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
                  | None -> Some (IntSet.one id)
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
      let name = Format.sprintf "%s_regle_%d" prog.prog_prefix id in
      (Com.ComputeTarget ((name, Pos.no_pos), []), Pos.no_pos))
    sorted_rules

let rdom_rule_filter (rdom : Com.rule_domain_data Com.domain) (rule : rule) :
    bool =
  (match rdom.Com.dom_used with
  | Some (rdom_seq, seq_pos) ->
      if rdom_seq <= rule.rule_seq then
        Err.domain_already_used Rule seq_pos (Pos.get rule.rule_id)
  | None -> ());
  let rdom_id = Pos.unmark rdom.dom_id in
  let rule_rdom_id = Pos.unmark rule.rule_domain.dom_id in
  Com.DomainId.equal rdom_id rule_rdom_id
  || Com.DomainIdSet.mem rule_rdom_id rdom.Com.dom_min

let check_no_variable_duplicates (rdom_rules : rule IntMap.t)
    (rdom_id : Com.DomainId.t) : unit =
  (* checks whether a variable is defined in two different rules given a rule "set".
     We cannot do it over all the rules of a single program because some are defined in different chainings *)
  let rule_defined =
    IntMap.fold
      (fun _ r rule_defined ->
        let out = r.rule_out_vars in
        StrMap.fold
          (fun var var_pos rule_defined ->
            let tail =
              match StrMap.find_opt var rule_defined with
              | Some tl -> tl
              | None -> []
            in
            StrMap.add var (var_pos :: tail) rule_defined)
          out rule_defined)
      rdom_rules StrMap.empty
  in
  StrMap.iter
    (fun var_name pos_list ->
      if (not (is_vartmp var_name)) && List.length pos_list > 1 then
        let msg =
          Format.asprintf
            "Variable %s is defined in %d different rules in rule domain %a"
            var_name (List.length pos_list) (Com.DomainId.pp ()) rdom_id
        in
        Errors.raise_multispanned_error msg
          (List.map (fun pos -> (None, pos)) (List.rev pos_list)))
    (* List.rev for cosmetic reasons *)
    rule_defined

let complete_rule_domains (prog : program) : program =
  let prog_targets =
    Com.DomainIdMap.fold
      (fun rdom_id rdom prog_targets ->
        if rdom.Com.dom_data.Com.rdom_computable then (
          let rdom_rules =
            IntMap.filter
              (fun _ rule -> rdom_rule_filter rdom rule)
              prog.prog_rules
          in
          check_no_variable_duplicates rdom_rules rdom_id;
          let rule_graph =
            create_rule_graph
              (fun r -> r.rule_in_vars)
              (fun r -> StrMap.keySet r.rule_out_vars)
              rdom_rules
          in
          let target_prog =
            let map =
              Com.m_instr_map_var (Pos.map_under_mark Com.get_normal_var) Fun.id
            in
            rule_graph_to_instrs (RuleDomain rdom_id) prog rule_graph
            |> List.map map
          in
          let tname =
            let spl =
              Com.DomainId.fold (fun s l -> (s, Pos.no_pos) :: l) rdom_id []
            in
            get_compute_id_str (Com.ComputeDomain (spl, Pos.no_pos)) prog
          in
          let target =
            Com.
              {
                target_name = (tname, Pos.no_pos);
                target_file = None;
                target_apps = StrMap.mapi (fun a p -> (a, p)) prog.prog_app;
                target_args = [];
                target_result = None;
                target_tmp_vars = StrMap.empty;
                target_prog;
                target_nb_tmps = 0;
                target_sz_tmps = 0;
                target_nb_refs = 0;
              }
          in
          StrMap.add tname target prog_targets)
        else prog_targets)
      prog.prog_rdoms prog.prog_targets
  in
  { prog with prog_targets }

let rdom_id_rule_filter (prog : program) (rdom_id : Com.DomainId.t)
    (rule : rule) : bool =
  let rdom = Com.DomainIdMap.find rdom_id prog.prog_rdoms in
  rdom_rule_filter rdom rule

let rdom_ids_rule_filter (prog : program) (rdom_ids : Com.DomainIdSet.t)
    (rule : rule) : bool =
  Com.DomainIdSet.exists
    (fun rdom_id -> rdom_id_rule_filter prog rdom_id rule)
    rdom_ids

let complete_chainings (prog : program) : program =
  let prog_targets =
    StrMap.fold
      (fun ch_name chain prog_targets ->
        let all_ids =
          Com.DomainIdMap.fold
            (fun _ rdom ids ->
              let uid = Pos.unmark rdom.Com.dom_id in
              Com.DomainIdSet.add uid ids)
            prog.prog_rdoms Com.DomainIdSet.empty
        in
        let sup_ids =
          IntMap.fold
            (fun _ (rdom, id_pos) sup_ids ->
              let uid = Pos.unmark rdom.Com.dom_id in
              let rdom_supeq = Com.DomainIdSet.add uid rdom.Com.dom_max in
              let sup_ids = Com.DomainIdSet.inter sup_ids rdom_supeq in
              if Com.DomainIdSet.cardinal sup_ids = 0 then
                Err.rule_domain_incompatible_with_chaining ch_name id_pos
              else sup_ids)
            chain.chain_rules all_ids
        in
        let min_ids =
          Com.DomainIdSet.filter
            (fun id ->
              let rdom = Com.DomainIdMap.find id prog.prog_rdoms in
              let min_sups = Com.DomainIdSet.inter sup_ids rdom.Com.dom_min in
              Com.DomainIdSet.is_empty min_sups)
            sup_ids
        in
        let rdom_rules =
          IntMap.filter
            (fun _ rule -> rdom_ids_rule_filter prog min_ids rule)
            prog.prog_rules
        in
        let inverted_rule_graph =
          create_rule_graph
            (fun r -> StrMap.keySet r.rule_out_vars)
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
            (fun r -> StrMap.keySet r.rule_out_vars)
            rules
        in
        let target_prog =
          let map =
            Com.m_instr_map_var (Pos.map_under_mark Com.get_normal_var) Fun.id
          in
          rule_graph_to_instrs (Chaining ch_name) prog rule_graph
          |> List.map map
        in
        let tname =
          get_compute_id_str (Com.ComputeChaining (ch_name, Pos.no_pos)) prog
        in
        let target =
          Com.
            {
              target_name = (tname, Pos.no_pos);
              target_file = None;
              target_apps = StrMap.mapi (fun a p -> (a, p)) prog.prog_app;
              target_args = [];
              target_result = None;
              target_tmp_vars = StrMap.empty;
              target_prog;
              target_nb_tmps = 0;
              target_sz_tmps = 0;
              target_nb_refs = 0;
            }
        in
        StrMap.add tname target prog_targets)
      prog.prog_chainings prog.prog_targets
  in
  { prog with prog_targets }

let check_verif (v : Mast.verification) (prog : program) : program =
  let verif_apps =
    (* Already checked during preprocessing *)
    StrMap.map (function _, pos -> pos) v.Mast.verif_apps
  in
  let vdom_id =
    Com.DomainId.from_marked_list (Pos.unmark v.Mast.verif_tag_names)
  in
  let verif_domain =
    let vid =
      match Com.DomainIdMap.find_opt vdom_id prog.prog_vdom_syms with
      | Some (vid, _) -> vid
      | None -> Err.unknown_domain Verif (Pos.get v.Mast.verif_tag_names)
    in
    Com.DomainIdMap.find vid prog.prog_vdoms
  in
  let prog_verifs, prog, _ =
    List.fold_left
      (fun (prog_verifs, prog, num) (cond, cond_pos) ->
        let id, id_pos = v.Mast.verif_number in
        let id = id + num in
        let verif_id = (id, id_pos) in
        let verif_expr = cond.Mast.verif_cond_expr in
        let verif_error, verif_var = cond.Mast.verif_cond_error in
        let err_name, err_pos = verif_error in
        let verif_is_blocking =
          match StrMap.find_opt err_name prog.prog_errors with
          | None -> Err.unknown_error err_pos
          | Some err -> (
              match err.typ with Com.Error.Anomaly -> true | _ -> false)
        in
        (match verif_var with
        | Some (var_name, var_pos) -> (
            match StrMap.find_opt var_name prog.prog_vars with
            | None -> Err.unknown_variable var_pos
            | Some _ -> ())
        | None -> ());
        let verif_cat_var_stats, verif_var_stats =
          let fold_var var idx_mem env (vdom_sts, var_sts) =
            check_variable var idx_mem env;
            let name = Com.get_normal_var (Pos.unmark var) in
            let id = StrMap.find name env.prog.prog_vars in
            let var_data = IntMap.find id env.prog.prog_dict in
            let cat = Com.Var.cat var_data in
            if not (Com.CatVar.Map.mem cat verif_domain.dom_data.vdom_auth) then
              Err.variable_with_forbidden_category (Pos.get var);
            let incr = function None -> Some 1 | Some i -> Some (i + 1) in
            let vdom_sts = Com.CatVar.Map.update cat incr vdom_sts in
            let var_sts = StrMap.update name incr var_sts in
            (vdom_sts, var_sts)
          in
          let init = (Com.CatVar.Map.empty, StrMap.empty) in
          let env =
            {
              prog;
              tmp_vars = StrMap.empty;
              ref_vars = StrMap.empty;
              res_var = None;
            }
          in
          fold_var_expr fold_var false init verif_expr env
        in
        let verif_seq, prog = get_seq prog in
        let verif =
          {
            verif_id;
            verif_apps;
            verif_domain;
            verif_expr;
            verif_error;
            verif_var;
            verif_is_blocking;
            verif_cat_var_stats;
            verif_var_stats;
            verif_seq;
          }
        in
        (match IntMap.find_opt id prog.prog_verifs with
        | Some v ->
            let verif_pos = Pos.get v.verif_id in
            Err.rov_already_defined Verif id verif_pos cond_pos
        | None -> ());
        let prog_verifs = IntMap.add id verif prog_verifs in
        (prog_verifs, prog, num + 1))
      (prog.prog_verifs, prog, 0)
      v.Mast.verif_conditions
  in
  { prog with prog_verifs }

let convert_verifs (prog : program) : program =
  let prog_targets =
    IntMap.fold
      (fun id verif prog_targets ->
        let tname = Format.sprintf "%s_verif_%d" prog.prog_prefix id in
        let target_file = Some (get_target_file (Pos.get verif.verif_id)) in
        let target_prog =
          List.map
            (Com.m_instr_map_var (Pos.map_under_mark Com.get_normal_var) Fun.id)
            [
              ( Com.IfThenElse
                  ( verif.verif_expr,
                    [
                      ( Com.RaiseError (verif.verif_error, verif.verif_var),
                        Pos.no_pos );
                    ],
                    [] ),
                Pos.no_pos );
            ]
        in
        let target =
          Com.
            {
              target_name = (tname, Pos.no_pos);
              target_file;
              target_apps = StrMap.mapi (fun a p -> (a, p)) prog.prog_app;
              target_args = [];
              target_result = None;
              target_tmp_vars = StrMap.empty;
              target_prog;
              target_nb_tmps = 0;
              target_sz_tmps = 0;
              target_nb_refs = 0;
            }
        in
        StrMap.add tname target prog_targets)
      prog.prog_verifs prog.prog_targets
  in
  { prog with prog_targets }

let eval_expr_verif (prog : program) (verif : verif)
    (expr : Mast.expression Pos.marked) : float option =
  let my_floor a = floor (a +. 0.000001) in
  let _my_ceil a = ceil (a -. 0.000001) in
  let my_arr a =
    let my_var1 = floor a in
    let my_var2 = ((a -. my_var1) *. 100000.0) +. 0.5 in
    let my_var2 = floor my_var2 /. 100000.0 in
    let my_var2 = my_var1 +. my_var2 +. 0.5 in
    floor my_var2
  in
  let rec aux expr =
    match Pos.unmark expr with
    | Com.Literal (Com.Float f) -> Some f
    | Literal Com.Undefined -> None
    | Var _ -> Err.variable_forbidden_in_filter (Pos.get expr)
    | Attribut ((VarAccess m_v, _), m_attr) ->
        let var_name = Com.get_normal_var @@ Pos.unmark m_v in
        let id = StrMap.find var_name prog.prog_vars in
        let var = IntMap.find id prog.prog_dict in
        let attrs = Com.Var.attrs var in
        let m_val = StrMap.find (Pos.unmark m_attr) attrs in
        Some (float (Pos.unmark m_val))
    | Size (VarAccess m_v, _) ->
        let var_name = Com.get_normal_var @@ Pos.unmark m_v in
        let id = StrMap.find var_name prog.prog_vars in
        let var = IntMap.find id prog.prog_dict in
        Some (float @@ Com.Var.size @@ var)
    | NbCategory cs ->
        let cats = mast_to_catvars cs prog.prog_var_cats in
        let sum =
          Com.CatVar.Map.fold
            (fun cat _ sum ->
              match Com.CatVar.Map.find_opt cat verif.verif_cat_var_stats with
              | Some i -> sum + i
              | None -> sum)
            cats 0
        in
        Some (float sum)
    | Unop (op, e0) -> (
        match aux e0 with
        | None -> None
        | Some f -> (
            match op with Com.Not -> Some (1.0 -. f) | Com.Minus -> Some ~-.f))
    | FuncCall (func, args) -> (
        let rl = List.map aux args in
        let unFunc f =
          match rl with
          | [ None ] -> None
          | [ Some x ] -> Some (f x)
          | _ -> assert false
        in
        let biFunc f =
          match rl with
          | [ None; None ] -> None
          | [ None; r ] | [ r; None ] -> r
          | [ Some x0; Some x1 ] -> Some (f x0 x1)
          | _ -> assert false
        in
        match Pos.unmark func with
        | Com.VerifNumber -> Some (float (Pos.unmark verif.verif_id))
        | Com.ComplNumber -> assert false
        | Com.SumFunc ->
            List.fold_left
              (fun res r ->
                match r with
                | None -> res
                | Some f -> (
                    match res with None -> r | Some fr -> Some (f +. fr)))
              None rl
        | Com.AbsFunc -> unFunc abs_float
        | Com.MinFunc -> biFunc min
        | Com.MaxFunc -> biFunc max
        | Com.GtzFunc -> unFunc (fun x -> if x > 0.0 then 1.0 else 0.0)
        | Com.GtezFunc -> unFunc (fun x -> if x >= 0.0 then 1.0 else 0.0)
        | Com.NullFunc -> unFunc (fun x -> if x = 0.0 then 1.0 else 0.0)
        | Com.ArrFunc -> unFunc my_arr
        | Com.InfFunc -> unFunc my_floor
        | Com.Supzero -> (
            match rl with
            | [ None ] -> None
            | [ Some f ] when f = 0.0 -> None
            | [ r ] -> r
            | _ -> assert false)
        | Com.PresentFunc | Com.Multimax | Com.NbEvents | Com.Func _ ->
            assert false)
    | Comparison (op, e0, e1) -> (
        match (aux e0, aux e1) with
        | None, _ | _, None -> None
        | Some f0, Some f1 -> (
            let open Com in
            match Pos.unmark op with
            | Gt -> Some (if f0 > f1 then 1.0 else 0.0)
            | Gte -> Some (if f0 >= f1 then 1.0 else 0.0)
            | Lt -> Some (if f0 < f1 then 1.0 else 0.0)
            | Lte -> Some (if f0 <= f1 then 1.0 else 0.0)
            | Eq -> Some (if f0 = f1 then 1.0 else 0.0)
            | Neq -> Some (if f0 <> f1 then 1.0 else 0.0)))
    | Binop (op, e0, e1) -> (
        let r0 = aux e0 in
        let r1 = aux e1 in
        match Pos.unmark op with
        | Com.And -> (
            match r0 with
            | None -> None
            | Some f0 -> if f0 = 0.0 then r0 else r1)
        | Com.Or -> (
            match r0 with None -> r1 | Some f0 -> if f0 = 0.0 then r1 else r0)
        | Com.Add -> (
            match (r0, r1) with
            | None, None -> None
            | None, Some _ -> r1
            | Some _, None -> r0
            | Some f0, Some f1 -> Some (f0 +. f1))
        | Com.Sub -> (
            match (r0, r1) with
            | None, None -> None
            | None, Some _ -> r1
            | Some _, None -> r0
            | Some f0, Some f1 -> Some (f0 +. f1))
        | Com.Mul -> (
            match (r0, r1) with
            | None, _ | _, None -> None
            | Some f0, Some f1 -> Some (f0 *. f1))
        | Com.Div -> (
            match (r0, r1) with
            | None, _ | _, None -> None
            | Some f0, Some f1 -> if f1 = 0.0 then r1 else Some (f0 /. f1))
        | Com.Mod -> (
            match (r0, r1) with
            | None, _ | _, None -> None
            | Some f0, Some f1 ->
                if f1 = 0.0 then r1 else Some (mod_float f0 f1)))
    | Conditional (e0, e1, e2) -> (
        let r0 = aux e0 in
        let r1 = aux e1 in
        let r2 = match e2 with Some e -> aux e | None -> None in
        match r0 with None -> None | Some f -> if f = 1.0 then r1 else r2)
    | TestInSet (positive, e, values) -> (
        match aux e with
        | None -> None
        | Some v ->
            let res =
              List.fold_left
                (fun res set_value ->
                  match set_value with
                  | Com.VarValue _ -> assert false
                  | Com.FloatValue (f, _) -> res || f = v
                  | Com.IntervalValue ((bn, _), (en, _)) ->
                      res || (float bn <= v && v <= float en))
                false values
            in
            Some (if res = positive then 1.0 else 0.0))
    | NbAnomalies | NbDiscordances | NbInformatives | NbBloquantes | Index _
    | FuncCallLoop _ | Loop _ | Attribut _ | Size _ ->
        assert false
  in
  aux expr

let vdom_rule_filter (prog : program) (vdom : Com.verif_domain_data Com.domain)
    (expr : Mast.expression Pos.marked) (verif : verif) : bool =
  (match vdom.Com.dom_used with
  | Some (vdom_seq, seq_pos) ->
      if vdom_seq <= verif.verif_seq then
        Err.domain_already_used Verif seq_pos (Pos.get verif.verif_id)
  | None -> ());
  let filter_expr =
    match eval_expr_verif prog verif expr with Some 1.0 -> true | _ -> false
  in
  let vdom_id = Pos.unmark vdom.dom_id in
  let verif_vdom_id = Pos.unmark verif.verif_domain.dom_id in
  filter_expr
  && (Com.DomainId.equal vdom_id verif_vdom_id
     || Com.DomainIdSet.mem verif_vdom_id vdom.Com.dom_min)

module OrdVerif = struct
  type t = int * int * int

  let make v =
    let iBlock = if v.verif_is_blocking then 0 else 1 in
    (iBlock, -v.verif_seq, Pos.unmark v.verif_id)

  let get_id (_, _, id) = id

  let compare x y = compare x y
end

module OrdVerifSet = struct
  include SetExt.Make (OrdVerif)

  let _pp ?(sep = " ")
      ?(pp_elt =
        fun fmt (i, j, k) -> Format.fprintf fmt "(%d, %d, %d)" i (-j) k)
      (_ : unit) (fmt : Format.formatter) (set : t) : unit =
    pp ~sep ~pp_elt () fmt set
end

module OrdVerifSetMap = struct
  include MapExt.Make (OrdVerifSet)

  let _pp ?(sep = ", ") ?(pp_key = OrdVerifSet.pp ()) ?(assoc = " => ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

let complete_verif_calls (prog : program) : program =
  let prog_targets, _ =
    StrMap.fold
      (fun tname (_, vdom_id, expr) (prog_targets, verif_calls) ->
        let verif_set =
          IntMap.fold
            (fun _verif_id verif verif_set ->
              let vdom = Com.DomainIdMap.find vdom_id prog.prog_vdoms in
              if vdom_rule_filter prog vdom expr verif then
                OrdVerifSet.add (OrdVerif.make verif) verif_set
              else verif_set)
            prog.prog_verifs OrdVerifSet.empty
        in
        match OrdVerifSetMap.find_opt verif_set verif_calls with
        | Some tn ->
            let target_prog =
              [ (Com.ComputeTarget ((tn, Pos.no_pos), []), Pos.no_pos) ]
            in
            let target =
              Com.
                {
                  target_name = (tname, Pos.no_pos);
                  target_file = None;
                  target_apps = StrMap.mapi (fun a p -> (a, p)) prog.prog_app;
                  target_args = [];
                  target_result = None;
                  target_tmp_vars = StrMap.empty;
                  target_prog;
                  target_nb_tmps = 0;
                  target_sz_tmps = 0;
                  target_nb_refs = 0;
                }
            in
            let prog_targets = StrMap.add tname target prog_targets in
            (prog_targets, verif_calls)
        | None ->
            let instrs =
              let instrs =
                OrdVerifSet.fold
                  (fun ord_verif target_prog ->
                    let verif_id = OrdVerif.get_id ord_verif in
                    let verif_tn =
                      Format.sprintf "%s_verif_%d" prog.prog_prefix verif_id
                    in
                    (Com.ComputeTarget ((verif_tn, Pos.no_pos), []), Pos.no_pos)
                    :: target_prog)
                  verif_set []
              in
              List.rev instrs
            in
            let target_prog = [ (Com.VerifBlock instrs, Pos.no_pos) ] in
            let target =
              Com.
                {
                  target_name = (tname, Pos.no_pos);
                  target_file = None;
                  target_apps = StrMap.mapi (fun a p -> (a, p)) prog.prog_app;
                  target_args = [];
                  target_result = None;
                  target_tmp_vars = StrMap.empty;
                  target_prog;
                  target_nb_tmps = 0;
                  target_sz_tmps = 0;
                  target_nb_refs = 0;
                }
            in
            let prog_targets = StrMap.add tname target prog_targets in
            let verif_calls = OrdVerifSetMap.add verif_set tname verif_calls in
            (prog_targets, verif_calls))
      prog.prog_vdom_calls
      (prog.prog_targets, OrdVerifSetMap.empty)
  in
  { prog with prog_targets }

let proceed (main_target : string) (p : Mast.program) : program =
  (* à paramétrer *)
  let prog =
    List.fold_left
      (fun prog source_file ->
        List.fold_left
          (fun prog (item, pos_item) ->
            match item with
            | Mast.Application (name, pos) -> check_application name pos prog
            | Mast.Chaining ((name, pos), m_apps) ->
                check_chaining name pos m_apps prog
            | Mast.VarCatDecl (decl, pos) -> check_var_category decl pos prog
            | Mast.VariableDecl var_decl -> check_var_decl var_decl prog
            | Mast.EventDecl evt_decl -> check_event_decl evt_decl pos_item prog
            | Mast.Error error -> check_error error prog
            | Mast.Func -> prog (* unused *)
            | Mast.Output _ -> prog (* unused *)
            | Mast.RuleDomDecl decl -> check_rule_dom_decl decl prog
            | Mast.VerifDomDecl decl -> check_verif_dom_decl decl prog
            | Mast.Function f -> check_target true f prog
            | Mast.Target t -> check_target false t prog
            | Mast.Rule r -> check_rule r prog
            | Mast.Verification v -> check_verif v prog)
          prog source_file)
      (empty_program p main_target)
      p
  in
  StrMap.iter
    (fun name (ef : Com.event_field) ->
      if ef.is_var && StrMap.cardinal prog.prog_vars = 0 then
        Err.event_field_need_a_variable name (Pos.get ef.name))
    prog.prog_event_fields;
  if StrMap.is_empty prog.prog_targets then Err.has_no_target ();
  (match StrMap.find_opt prog.prog_main_target prog.prog_targets with
  | None -> Err.main_target_not_found prog.prog_main_target
  | Some _ -> ());
  prog |> complete_rdom_decls |> complete_vdom_decls |> convert_rules
  |> complete_rule_domains |> complete_chainings |> convert_verifs
  |> complete_verif_calls
