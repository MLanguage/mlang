(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

module Err = struct
  type rov = Rule | Verif

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

  let loop_in_domains rov =
    let msg =
      Format.sprintf "there is a loop in the %s domain hierarchy"
        (rov_to_str rov)
    in
    Errors.raise_error msg
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

type program = {
  prog_apps : Pos.t StrMap.t;
  prog_chainings : Pos.t StrMap.t Pos.marked StrMap.t;
  prog_var_cats : Mir.cat_variable_data Mir.CatVarMap.t;
  prog_vars : global_variable StrMap.t;
  prog_alias : global_variable StrMap.t;
  prog_errors : error StrMap.t;
  prog_rdoms : Mir.rule_domain Mast.DomainIdMap.t;
  prog_rdom_syms : Mast.DomainId.t Pos.marked Mast.DomainIdMap.t;
  prog_vdoms : Mir.verif_domain Mast.DomainIdMap.t;
  prog_vdom_syms : Mast.DomainId.t Pos.marked Mast.DomainIdMap.t;
}

let empty_program =
  {
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
    | [] ->
        Buffer.add_char buf '_';
        Buffer.contents buf
  in
  make_prefix sorted_names

let check_application (name : string) (pos : Pos.t) (prog : program) : program =
  match StrMap.find_opt name prog.prog_apps with
  | Some old_pos -> Err.application_already_defined name old_pos pos
  | None ->
      let prog_apps = StrMap.add name pos prog.prog_apps in
      { prog with prog_apps }

let check_chaining (name : string) (pos : Pos.t)
    (m_apps : string Pos.marked list) (prog : program) : program =
  (match StrMap.find_opt name prog.prog_chainings with
  | Some (_, old_pos) -> Err.chaining_already_defined name old_pos pos
  | None -> ());
  let apps =
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
  let prog_chainings = StrMap.add name (apps, pos) prog.prog_chainings in
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

let check_domain (rov : Err.rov) (decl : 'a Mast.domain_decl) (dom_data : 'b)
    ((doms, syms) :
      'c Mir.domain Mast.DomainIdMap.t
      * Mast.DomainId.t Pos.marked Mast.DomainIdMap.t) :
    'c Mir.domain Mast.DomainIdMap.t
    * Mast.DomainId.t Pos.marked Mast.DomainIdMap.t =
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
  let doms, syms = check_domain Err.Rule decl dom_data doms_syms in
  { prog with prog_rdoms = doms; prog_rdom_syms = syms }

let check_verif_dom_decl (decl : Mast.verif_domain_decl) (prog : program) :
    program =
  let vdom_auth =
    let rec aux vdom_auth = function
      | [] -> vdom_auth
      | l :: t ->
          let vcats = Mir.mast_to_catvars prog.prog_var_cats l in
          aux (Mir.CatVarSet.union vcats vdom_auth) t
    in
    aux Mir.CatVarSet.empty decl.Mast.dom_data.vdom_auth
  in
  let dom_data = Mir.{ vdom_auth } in
  let doms_syms = (prog.prog_vdoms, prog.prog_vdom_syms) in
  let doms, syms = check_domain Err.Verif decl dom_data doms_syms in
  { prog with prog_vdoms = doms; prog_vdom_syms = syms }

let complete_dom_decls (rov : Err.rov)
    ((doms, syms) :
      'c Mir.domain Mast.DomainIdMap.t
      * Mast.DomainId.t Pos.marked Mast.DomainIdMap.t) :
    'c Mir.domain Mast.DomainIdMap.t =
  let get_dom id dom =
    Mast.DomainIdMap.find (Pos.unmark (Mast.DomainIdMap.find id syms)) dom
  in
  let doms =
    let rec set_min id dom (visiting, visited, doms) =
      if Mast.DomainIdSet.mem id visited then (visiting, visited, doms)
      else if Mast.DomainIdSet.mem id visiting then Err.loop_in_domains rov
      else
        let visiting = Mast.DomainIdSet.add id visiting in
        let visiting, visited, doms =
          let parentMap =
            let fold parentId map =
              let parentDom = get_dom parentId doms in
              let parentId = Pos.unmark parentDom.Mir.dom_id in
              Mast.DomainIdMap.add parentId parentDom map
            in
            Mast.DomainIdSet.fold fold dom.Mir.dom_min Mast.DomainIdMap.empty
          in
          Mast.DomainIdMap.fold set_min parentMap (visiting, visited, doms)
        in
        let dom_min =
          let fold parentId res =
            let parentDom = get_dom parentId doms in
            let parentId = Pos.unmark parentDom.Mir.dom_id in
            Mast.DomainIdSet.singleton parentId
            |> Mast.DomainIdSet.union parentDom.Mir.dom_min
            |> Mast.DomainIdSet.union res
          in
          Mast.DomainIdSet.fold fold dom.Mir.dom_min Mast.DomainIdSet.empty
        in
        let dom = Mir.{ dom with dom_min } in
        let doms = Mast.DomainIdMap.add id dom doms in
        let visiting = Mast.DomainIdSet.remove id visiting in
        let visited = Mast.DomainIdSet.add id visited in
        (visiting, visited, doms)
    in
    let init = (Mast.DomainIdSet.empty, Mast.DomainIdSet.empty, doms) in
    let _, _, doms = Mast.DomainIdMap.fold set_min doms init in
    doms
  in
  let doms =
    let set_max id dom doms =
      let fold minId doms =
        let minDom = Mast.DomainIdMap.find minId doms in
        let dom_max = Mast.DomainIdSet.add id minDom.Mir.dom_max in
        let minDom = Mir.{ minDom with dom_max } in
        Mast.DomainIdMap.add minId minDom doms
      in
      Mast.DomainIdSet.fold fold dom.Mir.dom_min doms
    in
    Mast.DomainIdMap.fold set_max doms doms
  in
  match Mast.DomainIdMap.find_opt Mast.DomainId.empty syms with
  | None -> Err.no_default_domain rov
  | Some _ ->
      Mast.DomainIdMap.fold
        (fun name (id, _) doms ->
          Mast.DomainIdMap.add name (get_dom id doms) doms)
        syms doms

let proceed (p : Mast.program) : program =
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
            | _ -> prog)
          prog source_file)
      empty_program p
  in
  let doms_syms = (prog.prog_rdoms, prog.prog_rdom_syms) in
  let prog_rdoms = complete_dom_decls Err.Rule doms_syms in
  let doms_syms = (prog.prog_vdoms, prog.prog_vdom_syms) in
  let prog_vdoms = complete_dom_decls Err.Verif doms_syms in
  { prog with prog_rdoms; prog_vdoms }
