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
      "second argument of functionn multimax must be a variable name" pos

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
  rule_tmp_vars :
    (string Pos.marked * Mast.table_size Pos.marked option) StrMap.t;
  rule_instrs : Mast.instruction Pos.marked list;
  rule_in_vars : StrSet.t;
  rule_out_vars : StrSet.t;
  rule_seq : int;
}

type verif = {
  verif_id : int Pos.marked;
  verif_apps : Pos.t StrMap.t;
  verif_domain : Com.verif_domain;
  verif_expr : Mast.expression Pos.marked;
  verif_error : Mast.error_name Pos.marked;
  verif_var : Mast.variable_name Pos.marked option;
  verif_is_blocking : bool;
  verif_cat_var_stats : int Com.CatVar.Map.t;
  verif_var_stats : int StrMap.t;
  verif_seq : int;
}

type program = {
  prog_prefix : string;
  prog_seq : int;
  prog_app : Pos.t StrMap.t;
  prog_apps : Pos.t StrMap.t;
  prog_chainings : chaining StrMap.t;
  prog_var_cats : Com.CatVar.data Com.CatVar.Map.t;
  prog_vars : Com.Var.t StrMap.t;
  prog_alias : Com.Var.t StrMap.t;
  prog_event_fields : Com.event_field StrMap.t;
  prog_event_field_idxs : string IntMap.t;
  prog_event_pos : Pos.t;
  prog_errors : Com.Error.t StrMap.t;
  prog_rdoms : Com.rule_domain_data doms;
  prog_rdom_syms : syms;
  prog_vdoms : Com.verif_domain_data doms;
  prog_vdom_syms : syms;
  prog_functions : Mast.target StrMap.t;
  prog_rules : rule IntMap.t;
  prog_rdom_calls : (int Pos.marked * Com.DomainId.t) StrMap.t;
  prog_verifs : verif IntMap.t;
  prog_vdom_calls :
    (int Pos.marked * Com.DomainId.t * Mast.expression Pos.marked) StrMap.t;
  prog_targets : Mast.target StrMap.t;
  prog_main_target : string;
  prog_stats : Mir.stats;
}

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
    prog_stats =
      {
        nb_calculated = 0;
        nb_base = 0;
        nb_input = 0;
        nb_vars = 0;
        nb_all_tmps = 0;
        nb_all_refs = 0;
        sz_calculated = 0;
        sz_base = 0;
        sz_input = 0;
        sz_vars = 0;
        sz_all_tmps = 0;
      };
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
  let prog_vars =
    match StrMap.find_opt name prog.prog_vars with
    | Some (gvar : Com.Var.t) ->
        let old_pos = Pos.get_position gvar.name in
        Err.variable_already_declared name old_pos name_pos
    | None -> StrMap.add name var prog.prog_vars
  in
  let prog_alias =
    match Com.Var.alias var with
    | Some (alias, alias_pos) -> (
        match StrMap.find_opt alias prog.prog_alias with
        | Some (gvar : Com.Var.t) ->
            let old_pos = Pos.get_position (Option.get (Com.Var.alias gvar)) in
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
      let global_table =
        match comp_var.Mast.comp_table with
        | Some (Mast.LiteralSize sz, _pos) -> Some sz
        | Some _ -> assert false
        | None -> None
      in
      let var =
        Com.Var.new_tgv ~name:comp_var.Mast.comp_name ~is_table:global_table
          ~is_given_back:comp_var.comp_is_givenback ~alias:None
          ~descr:comp_var.Mast.comp_description
          ~attrs:(get_attributes comp_var.Mast.comp_attributes)
          ~cat:global_category
          ~typ:(Option.map Pos.unmark comp_var.Mast.comp_typ)
      in
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
          let old_pos = Pos.get_position old_ef.name in
          let name_pos = Pos.get_position ef.name in
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

type 'a var_mem_type = Both | OneOf of 'a option

type var_env = {
  prog : program;
  tmp_vars : int option Pos.marked StrMap.t;
  ref_vars : Pos.t StrMap.t;
  res_var : string Pos.marked option;
}

let rec fold_var_expr
    (fold_var :
      Mast.variable Pos.marked -> unit var_mem_type -> var_env -> 'a -> 'a)
    (is_filter : bool) (acc : 'a) (m_expr : Mast.expression Pos.marked)
    (env : var_env) : 'a =
  let expr, expr_pos = m_expr in
  match expr with
  | TestInSet (_positive, e, values) ->
      let res = fold_var_expr fold_var is_filter acc e env in
      List.fold_left
        (fun res set_value ->
          match set_value with
          | Com.VarValue v ->
              if is_filter then
                Err.forbidden_expresion_in_filter (Pos.get_position v);
              fold_var v (OneOf None) env res
          | Com.FloatValue _ -> res
          | Com.Interval (bn, en) ->
              if Pos.unmark bn > Pos.unmark en then
                Err.wrong_interval_bounds (Pos.get_position bn);
              res)
        res values
  | Comparison (_op, e1, e2) ->
      let acc = fold_var_expr fold_var is_filter acc e1 env in
      fold_var_expr fold_var is_filter acc e2 env
  | Binop (_op, e1, e2) ->
      let acc = fold_var_expr fold_var is_filter acc e1 env in
      fold_var_expr fold_var is_filter acc e2 env
  | Unop (_op, e) -> fold_var_expr fold_var is_filter acc e env
  | Index (t, e) ->
      if is_filter then Err.forbidden_expresion_in_filter expr_pos;
      let acc = fold_var_expr fold_var is_filter acc e env in
      fold_var t (OneOf (Some ())) env acc
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
              match var_expr with
              | Var var, var_pos ->
                  let acc = fold_var_expr fold_var is_filter acc expr env in
                  fold_var (var, var_pos) Both env acc
              | _ -> Err.second_arg_of_multimax (Pos.get_position var_expr))
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
      | Com.Func fn ->
          if is_filter then Err.forbidden_expresion_in_filter expr_pos;
          let fd =
            match StrMap.find_opt fn env.prog.prog_functions with
            | Some fd -> fd
            | None -> Err.function_does_not_exist fn fpos
          in
          check_func (List.length fd.target_args))
  | Literal _ -> acc
  | Var var ->
      if is_filter then Err.variable_forbidden_in_filter expr_pos;
      fold_var (var, expr_pos) (OneOf None) env acc
  | NbCategory cs ->
      if not is_filter then Err.expression_only_in_filter expr_pos;
      let cats = mast_to_catvars cs env.prog.prog_var_cats in
      Com.CatVar.Map.iter
        (fun cat pos ->
          if not (Com.CatVar.Map.mem cat env.prog.prog_var_cats) then
            Err.unknown_domain Verif pos)
        cats;
      acc
  | Attribut (v, a) ->
      let name, var_pos =
        match v with
        | Mast.Normal name, var_pos -> (name, var_pos)
        | Mast.Generic _, _ -> assert false
      in
      (match StrMap.find_opt name env.prog.prog_vars with
      | Some var ->
          let cat = Com.Var.cat var in
          if not (StrMap.mem (Pos.unmark a) (Com.Var.attrs var)) then
            Err.unknown_attribut_for_var cat (Pos.get_position a)
      | None -> (
          match StrMap.find_opt name env.tmp_vars with
          | Some _ -> Err.tmp_vars_have_no_attrs var_pos
          | None -> ()));
      fold_var v Both env acc
  | Size v -> fold_var v Both env acc
  | NbAnomalies | NbDiscordances | NbInformatives | NbBloquantes ->
      if is_filter then Err.forbidden_expresion_in_filter expr_pos;
      acc
  | FuncCallLoop _ | Loop _ -> assert false

let check_variable (var : Mast.variable Pos.marked)
    (idx_mem : unit var_mem_type) (env : var_env) : string =
  let var_data, var_pos = var in
  let name, decl_mem, decl_pos =
    match var_data with
    | Normal vn -> (
        match StrMap.find_opt vn env.prog.prog_vars with
        | Some v ->
            (vn, OneOf (Com.Var.is_table v), Pos.get_position (Com.Var.name v))
        | None -> (
            match StrMap.find_opt vn env.tmp_vars with
            | Some (decl_size, decl_pos) -> (vn, OneOf decl_size, decl_pos)
            | None -> (
                match StrMap.find_opt vn env.ref_vars with
                | Some decl_pos -> (vn, Both, decl_pos)
                | None -> (
                    match env.res_var with
                    | Some (vr, decl_pos) when vr = vn ->
                        (vn, OneOf None, decl_pos)
                    | Some _ | None -> Err.unknown_variable var_pos))))
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

let check_expression (is_filter : bool) (m_expr : Mast.expression Pos.marked)
    (env : var_env) : StrSet.t =
  let fold_var var idx_mem env acc =
    let name = check_variable var idx_mem env in
    StrSet.add name acc
  in
  fold_var_expr fold_var is_filter StrSet.empty m_expr env

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
            Err.rule_domain_not_computable (Pos.get_position l)
      | None -> Err.unknown_domain Rule (Pos.get_position l))
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
            Err.verif_domain_not_verifiable (Pos.get_position l)
      | None -> Err.unknown_domain Verif (Pos.get_position l))
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
    program * Mast.instruction Pos.marked list * StrSet.t * StrSet.t =
  let rec aux (env, res, in_vars, out_vars) = function
    | [] -> (env, List.rev res, in_vars, out_vars)
    | m_instr :: il -> (
        let instr, instr_pos = m_instr in
        match instr with
        | Com.Affectation (f, _) -> (
            match f with
            | Com.SingleFormula (v, idx, e) ->
                let out_var =
                  let idx_mem = OneOf (Option.map (fun _ -> ()) idx) in
                  check_variable v idx_mem env
                in
                let in_vars_index =
                  match idx with
                  | Some ei -> check_expression false ei env
                  | None -> StrSet.empty
                in
                let in_vars_expr = check_expression false e env in
                if is_rule then
                  let in_vars_aff = StrSet.union in_vars_index in_vars_expr in
                  let in_vars =
                    StrSet.union in_vars (StrSet.diff in_vars_aff out_vars)
                  in
                  let out_vars = StrSet.add out_var out_vars in
                  aux (env, m_instr :: res, in_vars, out_vars) il
                else aux (env, m_instr :: res, in_vars, out_vars) il
            | Com.MultipleFormulaes _ -> assert false)
        | Com.IfThenElse (expr, i_then, i_else) ->
            (* if is_rule then Err.insruction_forbidden_in_rules instr_pos; *)
            let in_expr = check_expression false expr env in
            let prog, res_then, in_then, out_then =
              check_instructions i_then is_rule env
            in
            let env = { env with prog } in
            let prog, res_else, in_else, out_else =
              check_instructions i_else is_rule env
            in
            let env = { env with prog } in
            let res_instr = Com.IfThenElse (expr, res_then, res_else) in
            let in_vars =
              in_vars |> StrSet.union in_expr |> StrSet.union in_then
              |> StrSet.union in_else
            in
            let out_vars =
              out_vars |> StrSet.union out_then |> StrSet.union out_else
            in
            aux (env, (res_instr, instr_pos) :: res, in_vars, out_vars) il
        | Com.WhenDoElse (wdl, ed) ->
            let rec wde (env, res, in_vars, out_vars) = function
              | (expr, dl, pos) :: l ->
                  let in_expr = check_expression false expr env in
                  let prog, res_do, in_do, out_do =
                    check_instructions dl is_rule env
                  in
                  let env = { env with prog } in
                  let in_vars =
                    in_vars |> StrSet.union in_expr |> StrSet.union in_do
                  in
                  let out_vars = out_vars |> StrSet.union out_do in
                  wde (env, (expr, res_do, pos) :: res, in_vars, out_vars) l
              | [] ->
                  let prog, res_ed, in_ed, out_ed =
                    check_instructions (Pos.unmark ed) is_rule env
                  in
                  let env = { env with prog } in
                  let ed' = Pos.same_pos_as res_ed ed in
                  let in_vars = in_vars |> StrSet.union in_ed in
                  let out_vars = out_vars |> StrSet.union out_ed in
                  (env, Com.WhenDoElse (List.rev res, ed'), in_vars, out_vars)
            in
            let env, wde_res, in_vars, out_vars =
              wde (env, [], in_vars, out_vars) wdl
            in
            aux (env, (wde_res, instr_pos) :: res, in_vars, out_vars) il
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
            aux (env, (res_instr, instr_pos) :: res, in_vars, out_vars) il
        | Com.ComputeChaining _ ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let tname = get_compute_id_str instr env.prog in
            let res_instr = Com.ComputeTarget ((tname, Pos.no_pos), []) in
            aux (env, (res_instr, instr_pos) :: res, in_vars, out_vars) il
        | Com.ComputeVerifs ((vdom_list, vdom_pos), expr) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let tname = get_compute_id_str instr env.prog in
            let vdom_id =
              let id = Com.DomainId.from_marked_list vdom_list in
              Pos.unmark (Com.DomainIdMap.find id env.prog.prog_vdom_syms)
            in
            let seq, prog = get_seq env.prog in
            ignore (check_expression true expr env);
            let prog_vdom_calls =
              let used_data = ((seq, vdom_pos), vdom_id, expr) in
              StrMap.add tname used_data prog.prog_vdom_calls
            in
            let prog = { prog with prog_vdom_calls } in
            let env = { env with prog } in
            let res_instr = Com.ComputeTarget ((tname, Pos.no_pos), []) in
            aux (env, (res_instr, instr_pos) :: res, in_vars, out_vars) il
        | Com.VerifBlock instrs ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let prog, res_instrs, _, _ =
              check_instructions instrs is_rule env
            in
            let env = { env with prog } in
            let res_instr = Com.VerifBlock res_instrs in
            aux (env, (res_instr, instr_pos) :: res, in_vars, out_vars) il
        | Com.ComputeTarget ((tn, tpos), targs) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            (match StrMap.find_opt tn env.prog.prog_targets with
            | None -> Err.unknown_target tn tpos
            | Some target ->
                let nb_args = List.length target.target_args in
                if List.length targs <> nb_args then
                  Err.wrong_number_of_args nb_args tpos);
            List.iter (fun var -> ignore (check_variable var Both env)) targs;
            aux (env, m_instr :: res, in_vars, out_vars) il
        | Com.Print (_std, args) ->
            List.iter
              (fun arg ->
                match Pos.unmark arg with
                | Com.PrintString _ -> ()
                | Com.PrintName v | Com.PrintAlias v ->
                    ignore (check_variable v Both env)
                | Com.PrintIndent e -> ignore (check_expression false e env)
                | Com.PrintExpr (e, _min, _max) ->
                    ignore (check_expression false e env))
              args;
            aux (env, m_instr :: res, in_vars, out_vars) il
        | Com.Iterate (var, vars, var_params, instrs) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let var_pos = Pos.get_position var in
            let var_name =
              match Pos.unmark var with
              | Mast.Normal var -> var
              | Mast.Generic _ -> assert false
            in
            (match StrMap.find_opt var_name env.prog.prog_vars with
            | Some Com.Var.{ name = _, old_pos; _ } ->
                Err.variable_already_declared var_name old_pos var_pos
            | None -> ());
            (match StrMap.find_opt var_name env.tmp_vars with
            | Some (_, old_pos) ->
                Err.variable_already_declared var_name old_pos var_pos
            | None -> ());
            (match StrMap.find_opt var_name env.ref_vars with
            | Some old_pos ->
                Err.variable_already_declared var_name old_pos var_pos
            | None -> ());
            let env' =
              { env with ref_vars = StrMap.add var_name var_pos env.ref_vars }
            in
            ignore
              (List.fold_left
                 (fun seen var ->
                   let var_pos = Pos.get_position var in
                   let var_name = Mast.get_normal_var (Pos.unmark var) in
                   ignore (check_variable var Both env);
                   match StrMap.find_opt var_name seen with
                   | None -> StrMap.add var_name var_pos seen
                   | Some old_pos ->
                       Err.variable_already_specified var_name old_pos var_pos)
                 StrMap.empty vars);
            List.iter
              (fun (vcats, expr) ->
                ignore (mast_to_catvars vcats env.prog.prog_var_cats);
                ignore (check_expression false expr env'))
              var_params;
            let prog, res_instrs, _, _ =
              check_instructions instrs is_rule env'
            in
            let env = { env with prog } in
            let res_instr = Com.Iterate (var, vars, var_params, res_instrs) in
            aux (env, (res_instr, instr_pos) :: res, in_vars, out_vars) il
        | Com.Iterate_values (var, var_intervals, instrs) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            let var_pos = Pos.get_position var in
            let var_name =
              match Pos.unmark var with
              | Mast.Normal var -> var
              | Mast.Generic _ -> assert false
            in
            (match StrMap.find_opt var_name env.prog.prog_vars with
            | Some Com.Var.{ name = _, old_pos; _ } ->
                Err.variable_already_declared var_name old_pos var_pos
            | None -> ());
            (match StrMap.find_opt var_name env.tmp_vars with
            | Some (_, old_pos) ->
                Err.variable_already_declared var_name old_pos var_pos
            | None -> ());
            (match StrMap.find_opt var_name env.ref_vars with
            | Some old_pos ->
                Err.variable_already_declared var_name old_pos var_pos
            | None -> ());
            let env' =
              {
                env with
                tmp_vars = StrMap.add var_name (None, var_pos) env.tmp_vars;
              }
            in
            List.iter
              (fun (e0, e1) ->
                ignore (check_expression false e0 env);
                ignore (check_expression false e1 env))
              var_intervals;
            let prog, res_instrs, _, _ =
              check_instructions instrs is_rule env'
            in
            let env = { env with prog } in
            let res_instr =
              Com.Iterate_values (var, var_intervals, res_instrs)
            in
            aux (env, (res_instr, instr_pos) :: res, in_vars, out_vars) il
        | Com.Restore (vars, var_params, instrs) ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            ignore
              (List.fold_left
                 (fun seen var ->
                   let var_pos = Pos.get_position var in
                   let var_name = Mast.get_normal_var (Pos.unmark var) in
                   ignore (check_variable var Both env);
                   match StrMap.find_opt var_name seen with
                   | None -> StrMap.add var_name var_pos seen
                   | Some old_pos ->
                       Err.variable_already_specified var_name old_pos var_pos)
                 StrMap.empty vars);
            List.iter
              (fun (var, vcats, expr) ->
                let var_pos = Pos.get_position var in
                let var_name = Mast.get_normal_var (Pos.unmark var) in
                (match StrMap.find_opt var_name env.prog.prog_vars with
                | Some Com.Var.{ name = _, old_pos; _ } ->
                    Err.variable_already_declared var_name old_pos var_pos
                | None -> ());
                (match StrMap.find_opt var_name env.tmp_vars with
                | Some (_, old_pos) ->
                    Err.variable_already_declared var_name old_pos var_pos
                | None -> ());
                (match StrMap.find_opt var_name env.ref_vars with
                | Some old_pos ->
                    Err.variable_already_declared var_name old_pos var_pos
                | None -> ());
                ignore (mast_to_catvars vcats env.prog.prog_var_cats);
                let env =
                  {
                    env with
                    ref_vars = StrMap.add var_name var_pos env.ref_vars;
                  }
                in
                ignore (check_expression false expr env))
              var_params;
            let prog, res_instrs, _, _ =
              check_instructions instrs is_rule env
            in
            let env = { env with prog } in
            let res_instr = Com.Restore (vars, var_params, res_instrs) in
            aux (env, (res_instr, instr_pos) :: res, in_vars, out_vars) il
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
            aux (env, m_instr :: res, in_vars, out_vars) il
        | Com.CleanErrors | Com.ExportErrors | Com.FinalizeErrors ->
            if is_rule then Err.insruction_forbidden_in_rules instr_pos;
            aux (env, m_instr :: res, in_vars, out_vars) il)
  in
  let env, res, in_vars, out_vars =
    aux (env, [], StrSet.empty, StrSet.empty) instrs
  in
  let tmp_vars =
    StrMap.fold (fun vn _ s -> StrSet.add vn s) env.tmp_vars StrSet.empty
  in
  let in_vars = StrSet.diff in_vars tmp_vars in
  let out_vars = StrSet.diff out_vars tmp_vars in
  (env.prog, res, in_vars, out_vars)

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
    let target_args = t.target_args in
    List.iter
      (fun (vn, vpos) ->
        match StrMap.find_opt vn prog.prog_vars with
        | Some Com.Var.{ name = _, old_pos; _ } ->
            Err.variable_already_declared vn old_pos vpos
        | None -> ())
      target_args;
    let target_tmp_vars = t.target_tmp_vars in
    StrMap.iter
      (fun _ ((vn, vpos), _) ->
        match StrMap.find_opt vn prog.prog_vars with
        | Some Com.Var.{ name = _, old_pos; _ } ->
            Err.variable_already_declared vn old_pos vpos
        | None -> ())
      target_tmp_vars;
    List.iter
      (fun (vn, vpos) ->
        match StrMap.find_opt vn target_tmp_vars with
        | Some ((_, old_pos), _) ->
            Err.variable_already_declared vn old_pos vpos
        | None -> ())
      target_args;
    let target_result = t.target_result in
    (match target_result with
    | Some (vn, vpos) -> (
        if not is_function then Err.target_must_not_have_a_result tname tpos;
        (match StrMap.find_opt vn prog.prog_vars with
        | Some { name = _, old_pos; _ } ->
            Err.variable_already_declared vn old_pos vpos
        | None -> ());
        (match List.find_opt (fun (va, _) -> vn = va) target_args with
        | Some (_, old_pos) -> Err.variable_already_declared vn old_pos vpos
        | None -> ());
        match StrMap.find_opt vn target_tmp_vars with
        | Some ((_, old_pos), _) ->
            Err.variable_already_declared vn old_pos vpos
        | None -> ())
    | None -> if is_function then Err.function_result_missing tname tpos);
    let tmp_vars =
      StrMap.map
        (fun (var, size) ->
          let vpos = Pos.get_position var in
          let sz =
            match size with
            | None -> None
            | Some (Mast.LiteralSize i, _) -> Some i
            | Some (Mast.SymbolSize _, _) -> assert false
          in
          (sz, vpos))
        target_tmp_vars
    in
    let ref_vars =
      List.fold_left
        (fun res (vn, vpos) -> StrMap.add vn vpos res)
        StrMap.empty target_args
    in
    let res_var = target_result in
    let prog, target_prog =
      let env = { prog; tmp_vars; ref_vars; res_var } in
      let prog, target_prog, _in_vars, out_vars =
        check_instructions t.target_prog is_function env
      in
      (if is_function then
       let vr = Pos.unmark (Option.get target_result) in
       let bad_out_vars = StrSet.remove vr out_vars in
       if StrSet.card bad_out_vars > 0 then
         let vn = StrSet.min_elt bad_out_vars in
         Err.forbidden_out_var_in_function vn tname tpos);
      (prog, target_prog)
    in
    let target =
      {
        t with
        target_name;
        target_file;
        target_apps;
        target_args;
        target_result;
        target_tmp_vars;
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
      | None -> Err.unknown_domain Rule (Pos.get_position r.Mast.rule_tag_names)
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
  let rule_tmp_vars = r.Mast.rule_tmp_vars in
  StrMap.iter
    (fun _ ((vn, vpos), _) ->
      match StrMap.find_opt vn prog.prog_vars with
      | Some Com.Var.{ name = _, old_pos; _ } ->
          Err.variable_already_declared vn old_pos vpos
      | None -> ())
    rule_tmp_vars;
  let tmp_vars =
    StrMap.map
      (fun (var, size) ->
        let vpos = Pos.get_position var in
        let sz =
          match size with
          | None -> None
          | Some (Mast.LiteralSize i, _) -> Some i
          | Some (Mast.SymbolSize _, _) -> assert false
        in
        (sz, vpos))
      rule_tmp_vars
  in
  let rule_instrs = r.Mast.rule_formulaes in
  let prog, rule_instrs, rule_in_vars, rule_out_vars =
    let env = { prog; tmp_vars; ref_vars = StrMap.empty; res_var = None } in
    check_instructions rule_instrs true env
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
      let rule_pos = Pos.get_position r.rule_id in
      Err.rov_already_defined Rule id rule_pos id_pos
  | None -> ());
  let prog_rules = IntMap.add id rule prog.prog_rules in
  { prog with prog_rules; prog_chainings }

let convert_rules (prog : program) : program =
  let prog_targets =
    IntMap.fold
      (fun id rule prog_targets ->
        let tname = Format.sprintf "%s_regle_%d" prog.prog_prefix id in
        let target_file =
          Some (get_target_file (Pos.get_position rule.rule_id))
        in
        let target =
          Mast.
            {
              target_name = (tname, Pos.no_pos);
              target_file;
              target_apps = StrMap.mapi (fun a p -> (a, p)) prog.prog_app;
              target_args = [];
              target_result = None;
              target_tmp_vars = rule.rule_tmp_vars;
              target_prog = rule.rule_instrs;
              target_nb_tmps = 0;
              target_sz_tmps = 0;
              target_nb_refs = 0;
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
        Err.domain_already_used Rule seq_pos (Pos.get_position rule.rule_id)
  | None -> ());
  let rdom_id = Pos.unmark rdom.dom_id in
  let rule_rdom_id = Pos.unmark rule.rule_domain.dom_id in
  Com.DomainId.equal rdom_id rule_rdom_id
  || Com.DomainIdSet.mem rule_rdom_id rdom.Com.dom_min

let complete_rule_domains (prog : program) : program =
  let prog_targets =
    Com.DomainIdMap.fold
      (fun rdom_id rdom prog_targets ->
        if rdom.Com.dom_data.Com.rdom_computable then
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
              Com.DomainId.fold (fun s l -> (s, Pos.no_pos) :: l) rdom_id []
            in
            get_compute_id_str (Com.ComputeDomain (spl, Pos.no_pos)) prog
          in
          let target =
            Mast.
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
          StrMap.add tname target prog_targets
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
          get_compute_id_str (Com.ComputeChaining (ch_name, Pos.no_pos)) prog
        in
        let target =
          Mast.
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
      | None ->
          Err.unknown_domain Verif (Pos.get_position v.Mast.verif_tag_names)
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
            let name = check_variable var idx_mem env in
            let var_data = StrMap.find name env.prog.prog_vars in
            let cat = Com.Var.cat var_data in
            if not (Com.CatVar.Map.mem cat verif_domain.dom_data.vdom_auth) then
              Err.variable_with_forbidden_category (Pos.get_position var);
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
            let verif_pos = Pos.get_position v.verif_id in
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
        let target_file =
          Some (get_target_file (Pos.get_position verif.verif_id))
        in
        let target_prog =
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
          Mast.
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
    | Var _ -> Err.variable_forbidden_in_filter (Pos.get_position expr)
    | Attribut (m_var, m_attr) ->
        let var =
          match Pos.unmark m_var with
          | Mast.Normal var -> var
          | _ -> assert false
        in
        let attrs = Com.Var.attrs (StrMap.find var prog.prog_vars) in
        let m_val = StrMap.find (Pos.unmark m_attr) attrs in
        Some (float (Pos.unmark m_val))
    | Size m_var -> (
        let var =
          match Pos.unmark m_var with
          | Mast.Normal var -> var
          | _ -> assert false
        in
        match Com.Var.is_table (StrMap.find var prog.prog_vars) with
        | Some sz -> Some (float sz)
        | None -> Some 1.0)
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
        | Com.PresentFunc | Com.Multimax | Com.Func _ -> assert false)
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
            | Some f0, Some f1 -> if f1 = 0.0 then r1 else Some (f0 /. f1)))
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
                  | Com.Interval ((bn, _), (en, _)) ->
                      res || (float bn <= v && v <= float en))
                false values
            in
            Some (if res = positive then 1.0 else 0.0))
    | NbAnomalies | NbDiscordances | NbInformatives | NbBloquantes | Index _
    | FuncCallLoop _ | Loop _ ->
        assert false
  in
  aux expr

let vdom_rule_filter (prog : program) (vdom : Com.verif_domain_data Com.domain)
    (expr : Mast.expression Pos.marked) (verif : verif) : bool =
  (match vdom.Com.dom_used with
  | Some (vdom_seq, seq_pos) ->
      if vdom_seq <= verif.verif_seq then
        Err.domain_already_used Verif seq_pos (Pos.get_position verif.verif_id)
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
              Mast.
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
              Mast.
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

let complete_vars (prog : program) : program =
  let prog_vars = prog.prog_vars in
  let prog_vars =
    let incr_cpt cat cpt =
      let i = Com.CatVar.Map.find cat cpt in
      let cpt = Com.CatVar.Map.add cat (i + 1) cpt in
      (cpt, i)
    in
    let cat_cpt = Com.CatVar.Map.map (fun _ -> 0) prog.prog_var_cats in
    let prog_vars, _ =
      StrMap.fold
        (fun vn (var : Com.Var.t) (res, cpt) ->
          let tgv = Com.Var.tgv var in
          let dcat = Com.CatVar.Map.find tgv.cat prog.prog_var_cats in
          let cpt, i = incr_cpt tgv.cat cpt in
          let loc = Com.set_loc_tgv_cat var.loc dcat.loc dcat.id_str i in
          let var = Com.Var.{ var with loc } in
          let res = StrMap.add vn var res in
          (res, cpt))
        prog_vars (StrMap.empty, cat_cpt)
    in
    prog_vars
  in
  let module CatLoc = struct
    type t = Com.CatVar.loc

    let pp fmt (loc : t) =
      match loc with
      | Com.CatVar.LocComputed -> Format.fprintf fmt "calculee"
      | Com.CatVar.LocBase -> Format.fprintf fmt "base"
      | Com.CatVar.LocInput -> Format.fprintf fmt "saisie"

    let compare x y = compare x y
  end in
  let module CatLocMap = struct
    include MapExt.Make (CatLoc)

    let _pp ?(sep = ", ") ?(pp_key = CatLoc.pp) ?(assoc = " => ")
        (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
        (map : 'a t) : unit =
      pp ~sep ~pp_key ~assoc pp_val fmt map
  end in
  let loc_vars, sz_loc_vars, sz_vars =
    let fold _ (var : Com.Var.t) (loc_vars, sz_loc_vars, n) =
      let var = Com.Var.{ var with loc = Com.set_loc_int var.loc n } in
      let loc_cat =
        (Com.CatVar.Map.find (Com.Var.cat var) prog.prog_var_cats).loc
      in
      let loc_vars =
        let upd = function
          | None -> Some (Com.Var.Set.one var)
          | Some set -> Some (Com.Var.Set.add var set)
        in
        CatLocMap.update loc_cat upd loc_vars
      in
      let sz = Com.Var.size var in
      let sz_loc_vars =
        let upd = function
          | None -> Some sz
          | Some n_loc -> Some (n_loc + sz)
        in
        CatLocMap.update loc_cat upd sz_loc_vars
      in
      (loc_vars, sz_loc_vars, n + sz)
    in
    StrMap.fold fold prog_vars (CatLocMap.empty, CatLocMap.empty, 0)
  in
  let update_loc (var : Com.Var.t) (vars, n) =
    let loc = Com.set_loc_tgv_idx var.loc n in
    let vars =
      StrMap.add (Com.Var.name_str var) Com.Var.{ var with loc } vars
    in
    (vars, n + Com.Var.size var)
  in
  let prog_vars =
    CatLocMap.fold
      (fun _loc_cat vars prog_vars ->
        (prog_vars, 0) |> Com.Var.Set.fold update_loc vars |> fst)
      loc_vars StrMap.empty
  in
  let nb_loc loc_cat =
    match CatLocMap.find_opt loc_cat loc_vars with
    | Some set -> Com.Var.Set.cardinal set
    | None -> 0
  in
  let sz_loc loc_cat =
    match CatLocMap.find_opt loc_cat sz_loc_vars with
    | Some sz -> sz
    | None -> 0
  in
  let prog_stats =
    Mir.
      {
        prog.prog_stats with
        nb_calculated = nb_loc Com.CatVar.LocComputed;
        nb_input = nb_loc Com.CatVar.LocInput;
        nb_base = nb_loc Com.CatVar.LocBase;
        nb_vars = StrMap.cardinal prog_vars;
        sz_calculated = sz_loc Com.CatVar.LocComputed;
        sz_input = sz_loc Com.CatVar.LocInput;
        sz_base = sz_loc Com.CatVar.LocBase;
        sz_vars;
      }
  in
  { prog with prog_vars; prog_stats }

let complete_vars_stack (prog : program) : program =
  let prog_functions, prog_targets =
    let rec aux_instrs mil =
      let fold (nbRef, nbIt) mi =
        let nbRef', nbIt' = aux_instr mi in
        (max nbRef nbRef', max nbIt nbIt')
      in
      List.fold_left fold (0, 0) mil
    and aux_instr (instr, _pos) =
      match instr with
      | Com.IfThenElse (_, ilThen, ilElse) ->
          let nbRefThen, nbItThen = aux_instrs ilThen in
          let nbRefElse, nbItElse = aux_instrs ilElse in
          (max nbRefThen nbRefElse, max nbItThen nbItElse)
      | Com.WhenDoElse (wdl, ed) ->
          let rec wde (nbRef, nbIt) = function
            | (_, dl, _) :: wdl' ->
                let nbRefD, nbItD = aux_instrs dl in
                wde (max nbRef nbRefD, max nbIt nbItD) wdl'
            | [] ->
                let nbRefD, nbItD = aux_instrs (Pos.unmark ed) in
                (max nbRef nbRefD, max nbIt nbItD)
          in
          wde (0, 0) wdl
      | Com.VerifBlock instrs -> aux_instrs instrs
      | Com.Iterate (_, _, _, instrs) ->
          let nbRef, nbIt = aux_instrs instrs in
          (nbRef + 1, nbIt)
      | Com.Iterate_values (_, _, instrs) ->
          let nbRef, nbIt = aux_instrs instrs in
          (nbRef, nbIt + 1)
      | Com.Restore (_, _, instrs) ->
          let nbRef, nbIt = aux_instrs instrs in
          (max nbRef 1, nbIt)
      | Com.Affectation _ | Com.Print _ | Com.ComputeTarget _ | Com.RaiseError _
      | Com.CleanErrors | Com.ExportErrors | Com.FinalizeErrors ->
          (0, 0)
      | Com.ComputeDomain _ | Com.ComputeChaining _ | Com.ComputeVerifs _ ->
          assert false
    in
    let map (t : Mast.target) =
      let nbRef, nbIt = aux_instrs t.target_prog in
      let target_nb_tmps = StrMap.cardinal t.target_tmp_vars + nbIt in
      let target_sz_tmps =
        let fold _ (_, tsz_opt) sz =
          match tsz_opt with
          | None -> sz + 1
          | Some (tsz, _) -> sz + Mast.get_table_size tsz
        in
        StrMap.fold fold t.target_tmp_vars nbIt
      in
      let target_nb_refs = List.length t.target_args + nbRef in
      { t with target_nb_tmps; target_sz_tmps; target_nb_refs }
    in
    (StrMap.map map prog.prog_functions, StrMap.map map prog.prog_targets)
  in
  let nb_all_tmps, sz_all_tmps, nb_all_refs =
    let rec aux_instrs tdata mil =
      let fold (nb, sz, nbRef, tdata) mi =
        let nb', sz', nbRef', tdata = aux_instr tdata mi in
        (max nb nb', max sz sz', max nbRef nbRef', tdata)
      in
      List.fold_left fold (0, 0, 0, tdata) mil
    and aux_call tdata name =
      match StrMap.find_opt name tdata with
      | Some (nb, sz, nbRef) -> (nb, sz, nbRef, tdata)
      | None -> (
          let eval_call (t : Mast.target) =
            let nb, sz, nbRef =
              ( t.target_nb_tmps,
                t.target_sz_tmps,
                List.length t.target_args + t.target_nb_refs )
            in
            let nb', sz', nbRef', tdata = aux_instrs tdata t.target_prog in
            let nb = nb + nb' in
            let sz = sz + sz' in
            let nbRef = nbRef + nbRef' in
            let tdata = StrMap.add name (nb, sz, nbRef) tdata in
            (nb, sz, nbRef, tdata)
          in
          match StrMap.find_opt name prog_functions with
          | Some t -> eval_call t
          | None -> eval_call (StrMap.find name prog_targets))
    and aux_instr tdata (instr, _pos) =
      match instr with
      | Com.Affectation mf -> (
          match Pos.unmark mf with
          | SingleFormula (_, mei_opt, mev) ->
              let nbI, szI, nbRefI, tdata =
                match mei_opt with
                | None -> (0, 0, 0, tdata)
                | Some mei -> aux_expr tdata mei
              in
              let nbV, szV, nbRefV, tdata = aux_expr tdata mev in
              (max nbI nbV, max szI szV, max nbRefI nbRefV, tdata)
          | MultipleFormulaes _ -> assert false)
      | Com.ComputeTarget (tn, _args) -> aux_call tdata (Pos.unmark tn)
      | Com.IfThenElse (meI, ilT, ilE) ->
          let nbI, szI, nbRefI, tdata = aux_expr tdata meI in
          let nbT, szT, nbRefT, tdata = aux_instrs tdata ilT in
          let nbE, szE, nbRefE, tdata = aux_instrs tdata ilE in
          let nb = max nbI (max nbT nbE) in
          let sz = max szI (max szT szE) in
          let nbRef = max nbRefI (max nbRefT nbRefE) in
          (nb, sz, nbRef, tdata)
      | Com.WhenDoElse (wdl, ed) ->
          let rec wde (nb, sz, nbRef, tdata) = function
            | (me, dl, _) :: wdl' ->
                let nbE, szE, nbRefE, tdata = aux_expr tdata me in
                let nbD, szD, nbRefD, tdata = aux_instrs tdata dl in
                let nb = max nb (max nbE nbD) in
                let sz = max sz (max szE szD) in
                let nbRef = max nbRef (max nbRefE nbRefD) in
                wde (nb, sz, nbRef, tdata) wdl'
            | [] ->
                let nbD, szD, nbRefD, tdata =
                  aux_instrs tdata (Pos.unmark ed)
                in
                let nb = max nb nbD in
                let sz = max sz szD in
                let nbRef = max nbRef nbRefD in
                (nb, sz, nbRef, tdata)
          in
          wde (0, 0, 0, tdata) wdl
      | Com.VerifBlock instrs -> aux_instrs tdata instrs
      | Com.Print (_, pal) ->
          let fold (nb, sz, nbRef, tdata) (a, _pos) =
            match a with
            | Com.PrintString _ | Com.PrintName _ | Com.PrintAlias _ ->
                (nb, sz, nbRef, tdata)
            | Com.PrintIndent me | Com.PrintExpr (me, _, _) ->
                let nb', sz', nbRef', tdata = aux_expr tdata me in
                (max nb nb', max sz sz', max nbRef nbRef', tdata)
          in
          List.fold_left fold (0, 0, 0, tdata) pal
      | Com.Iterate (_, _, mel, instrs) ->
          let fold (nb, sz, nbRef, tdata) (_, me) =
            let nb', sz', nbRef', tdata = aux_expr tdata me in
            (max nb nb', max sz sz', max nbRef nbRef', tdata)
          in
          let nb', sz', nbRef', tdata =
            List.fold_left fold (0, 0, 0, tdata) mel
          in
          let nb, sz, nbRef, tdata = aux_instrs tdata instrs in
          let nb = max nb nb' in
          let sz = max sz sz' in
          let nbRef = 1 + max nbRef nbRef' in
          (nb, sz, nbRef, tdata)
      | Com.Iterate_values (_, me2l, instrs) ->
          let fold (nb, sz, nbRef, tdata) (me0, me1) =
            let nb', sz', nbRef', tdata = aux_expr tdata me0 in
            let nb'', sz'', nbRef'', tdata = aux_expr tdata me1 in
            let nb = max nb (max nb' nb'') in
            let sz = max sz (max sz' sz'') in
            let nbRef = max nbRef (max nbRef' nbRef'') in
            (nb, sz, nbRef, tdata)
          in
          let nb', sz', nbRef', tdata =
            List.fold_left fold (0, 0, 0, tdata) me2l
          in
          let nb, sz, nbRef, tdata = aux_instrs tdata instrs in
          let nb = 1 + max nb nb' in
          let sz = 1 + max sz sz' in
          let nbRef = max nbRef nbRef' in
          (nb, sz, nbRef, tdata)
      | Com.Restore (_, mel, instrs) ->
          let fold (nb, sz, nbRef, tdata) (_, _, me) =
            let nb', sz', nbRef', tdata = aux_expr tdata me in
            (max nb nb', max sz sz', max nbRef nbRef', tdata)
          in
          let nb', sz', nbRef', tdata =
            List.fold_left fold (0, 0, 0, tdata) mel
          in
          let nb, sz, nbRef, tdata = aux_instrs tdata instrs in
          let nb = max nb nb' in
          let sz = max sz sz' in
          let nbRef = 1 + max nbRef nbRef' in
          (nb, sz, nbRef, tdata)
      | Com.RaiseError _ | Com.CleanErrors | Com.ExportErrors
      | Com.FinalizeErrors ->
          (0, 0, 0, tdata)
      | Com.ComputeDomain _ | Com.ComputeChaining _ | Com.ComputeVerifs _ ->
          assert false
    and aux_expr tdata (expr, _pos) =
      match expr with
      | Com.TestInSet (_, me, _) | Com.Unop (_, me) | Com.Index (_, me) ->
          aux_expr tdata me
      | Com.Comparison (_, me0, me1) | Com.Binop (_, me0, me1) ->
          let nb0, sz0, nbRef0, tdata = aux_expr tdata me0 in
          let nb1, sz1, nbRef1, tdata = aux_expr tdata me1 in
          (max nb0 nb1, max sz0 sz1, max nbRef0 nbRef1, tdata)
      | Com.Conditional (meI, meT, meEOpt) ->
          let nbI, szI, nbRefI, tdata = aux_expr tdata meI in
          let nbT, szT, nbRefT, tdata = aux_expr tdata meT in
          let nbE, szE, nbRefE, tdata =
            match meEOpt with
            | None -> (0, 0, 0, tdata)
            | Some meE -> aux_expr tdata meE
          in
          let nb = max nbI (max nbT nbE) in
          let sz = max szI (max szT szE) in
          let nbRef = max nbRefI (max nbRefT nbRefE) in
          (nb, sz, nbRef, tdata)
      | Com.FuncCall (func, mel) ->
          let fold (nb, sz, nbRef, tdata) me =
            let nb', sz', nbRef', tdata = aux_expr tdata me in
            (max nb nb', max sz sz', max nbRef nbRef', tdata)
          in
          let nb', sz', nbRef', tdata =
            List.fold_left fold (0, 0, 0, tdata) mel
          in
          let nb, sz, nbRef, tdata =
            match Pos.unmark func with
            | Func name -> aux_call tdata name
            | _ -> (0, 0, 0, tdata)
          in
          (max nb nb', max sz sz', max nbRef nbRef', tdata)
      | Com.Literal _ | Com.Var _ | Com.NbCategory _ | Com.Attribut _
      | Com.Size _ | Com.NbAnomalies | Com.NbDiscordances | Com.NbInformatives
      | Com.NbBloquantes ->
          (0, 0, 0, tdata)
      | Com.FuncCallLoop _ | Com.Loop _ -> assert false
    in
    let nb, sz, nbRef, _ =
      let fold tn _ (nb, sz, nbRef, tdata) =
        let nb', sz', nbRef', tdata = aux_call tdata tn in
        (max nb nb', max sz sz', max nbRef nbRef', tdata)
      in
      (0, 0, 0, StrMap.empty)
      |> StrMap.fold fold prog_functions
      |> StrMap.fold fold prog_targets
    in
    (nb, sz, nbRef)
  in
  (match StrMap.find_opt prog.prog_main_target prog_targets with
  | None -> Err.main_target_not_found prog.prog_main_target
  | Some _ -> ());
  let prog_stats =
    Mir.{ prog.prog_stats with nb_all_tmps; sz_all_tmps; nb_all_refs }
  in
  { prog with prog_functions; prog_targets; prog_stats }

let proceed (p : Mast.program) (main_target : string) : program =
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
  prog |> complete_rdom_decls |> complete_vdom_decls |> convert_rules
  |> complete_rule_domains |> complete_chainings |> convert_verifs
  |> complete_verif_calls |> complete_vars |> complete_vars_stack
