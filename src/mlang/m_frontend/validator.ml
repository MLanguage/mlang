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
        Pos.format old_pos
    in
    Errors.raise_spanned_error msg pos

  let var_category_already_definied cat old_pos pos =
    let msg =
      Format.asprintf
        "Category \"%a\" defined more than once: already defined %a"
        Com.CatVar.pp cat Pos.format old_pos
    in
    Errors.raise_spanned_error msg pos

  let attribute_already_defined attr old_pos pos =
    let msg =
      Format.asprintf
        "attribute \"%s\" defined more than once: already defined %a" attr
        Pos.format old_pos
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
        Pos.format old_pos
    in
    Errors.raise_spanned_error msg pos

  let variable_already_declared name old_pos pos =
    let msg =
      Format.asprintf
        "variable \"%s\" declared more than once: already declared %a" name
        Pos.format old_pos
    in
    Errors.raise_spanned_error msg pos

  let temporary_variable_already_declared name old_pos pos =
    let msg =
      Format.asprintf
        "temporary variable \"%s\" declared more than once: already declared %a"
        name Pos.format old_pos
    in
    Errors.raise_spanned_error msg pos

  let error_already_declared name old_pos pos =
    let msg =
      Format.asprintf
        "error \"%s\" declared more than once: already declared %a" name
        Pos.format old_pos
    in
    Errors.raise_spanned_error msg pos

  let domain_already_declared rov old_pos pos =
    let msg =
      Format.asprintf "%s domain declared more than once: already declared %a"
        (rov_to_str rov) Pos.format old_pos
    in
    Errors.raise_spanned_error msg pos

  let default_domain_already_declared rov old_pos pos =
    let msg =
      Format.asprintf
        "default %s domain declared more than once: already declared %a"
        (rov_to_str rov) Pos.format old_pos
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

  let variable_space_already_declared old_pos pos =
    let msg =
      Pp.spr "variable space declared more than once: already declared %a"
        Pos.format old_pos
    in
    Errors.raise_spanned_error msg pos

  let default_variable_space_already_declared old_pos pos =
    let msg =
      Pp.spr
        "default variable space declared more than once: already declared %a"
        Pos.format old_pos
    in
    Errors.raise_spanned_error msg pos

  let no_default_variable_space () =
    let msg = Pp.spr "there are no default variable space" in
    Errors.raise_error msg

  let target_already_declared name old_pos pos =
    let msg =
      Format.asprintf
        "target \"%s\" declared more than once: already declared %a" name
        Pos.format old_pos
    in
    Errors.raise_spanned_error msg pos

  let unknown_variable pos = Errors.raise_spanned_error "unknown variable" pos

  let variable_used_as_table decl_pos pos =
    let msg =
      Format.asprintf "variable used as a table, declared %a" Pos.format
        decl_pos
    in
    Errors.raise_spanned_error msg pos

  let table_used_as_variable decl_pos pos =
    let msg =
      Format.asprintf "table used as a variable, declared %a" Pos.format
        decl_pos
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

  let var_have_no_attrs var pos =
    let msg = Pp.spr "variable %s have no attributes" var in
    Errors.raise_spanned_error msg pos

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
        (rov_to_str rov) rov_id Pos.format old_pos
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
        Pos.format dom_pos
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
      Format.asprintf "event fields are already declared at %a" Pos.format
        old_pos
    in
    Errors.raise_spanned_error msg pos

  let event_field_already_declared name old_pos pos =
    let msg =
      Format.asprintf "event field \"%s\" is already declared at %a" name
        Pos.format old_pos
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

  let forbidden_variable_in_raise pos =
    let msg = "forbidden variable in leve_erreur" in
    Errors.raise_spanned_error msg pos

  let unknown_var_space name pos =
    let msg = Format.asprintf "unknown variable space \"%s\"" name in
    Errors.raise_spanned_error msg pos

  let var_spaces_forbidden_in_this this pos =
    let msg = Pp.spr "variable spaces are forbidden in %s" this in
    Errors.raise_spanned_error msg pos

  let variable_not_in_var_space var_name sp_name pos =
    let msg =
      Pp.spr "variable \"%s\" does not belong to space \"%s\"" var_name sp_name
    in
    Errors.raise_spanned_error msg pos

  let category_forbidden_with_space cat_pos sp_name =
    let msg =
      Pp.spr "variable category forbidden with variable space \"%s\"" sp_name
    in
    Errors.raise_spanned_error msg cat_pos
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
  rule_tmp_vars : int Pos.marked StrMap.t;
  rule_instrs : (int Pos.marked, Mast.error_name) Com.m_instruction list;
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

type target = (int Pos.marked, Mast.error_name) Com.target

type call_compute =
  | CallDomain of string * Com.DomainId.t * string option
  | CallVerifs of string * Com.DomainId.t * string option
  | CallChaining of string * string * string option
  | CallTarget of string * string option

let compare_call_compute cc0 cc1 =
  let cons_to_int = function
    | CallDomain _ -> 0
    | CallVerifs _ -> 1
    | CallChaining _ -> 2
    | CallTarget _ -> 3
  in
  let c = Int.compare (cons_to_int cc0) (cons_to_int cc1) in
  if c <> 0 then c
  else
    match (cc0, cc1) with
    | CallDomain (tn0, _, sp_opt0), CallDomain (tn1, _, sp_opt1)
    | CallVerifs (tn0, _, sp_opt0), CallVerifs (tn1, _, sp_opt1)
    | CallChaining (tn0, _, sp_opt0), CallChaining (tn1, _, sp_opt1)
    | CallTarget (tn0, sp_opt0), CallTarget (tn1, sp_opt1) ->
        let c = String.compare tn0 tn1 in
        if c <> 0 then c else compare sp_opt0 sp_opt1
    | _, _ -> assert false

let pp_call_compute fmt = function
  | CallDomain (_, dom, sp_opt) ->
      Pp.fpr fmt "CallDomain(%a, %a)" (Com.DomainId.pp ()) dom
        (Pp.option Pp.string) sp_opt
  | CallVerifs (_, dom, sp_opt) ->
      Pp.fpr fmt "CallVerifs(%a, %a)" (Com.DomainId.pp ()) dom
        (Pp.option Pp.string) sp_opt
  | CallChaining (_, chain, sp_opt) ->
      Pp.fpr fmt "CallChaining(%s, %a)" chain (Pp.option Pp.string) sp_opt
  | CallTarget (name, sp_opt) ->
      Pp.fpr fmt "CallTarget(%s, %a)" name (Pp.option Pp.string) sp_opt

module CallMap = struct
  include MapExt.Make (struct
    type t = call_compute

    let compare = compare_call_compute
  end)

  let pp ?(sep = "; ") ?(pp_key = pp_call_compute) ?(assoc = " => ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

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
  prog_var_spaces : int StrMap.t;
  prog_var_spaces_idx : Com.variable_space IntMap.t;
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
  prog_call_map : (Pos.t CallMap.t * Pos.t) CallMap.t;
}

let is_vartmp (var : string) =
  String.length var >= 6 && String.sub var 0 6 = "VARTMP"

let check_name_in_tgv prog m_name =
  let vn, vpos = Pos.to_couple m_name in
  let err old_pos = Err.variable_already_declared vn old_pos vpos in
  match StrMap.find_opt vn prog.prog_vars with
  | Some id ->
      let var = IntMap.find id prog.prog_dict in
      let old_pos = Pos.get @@ Com.Var.name var in
      err old_pos
  | None -> ()

let check_alias_in_tgv prog m_alias =
  let an, apos = Pos.to_couple m_alias in
  let err old_pos = Err.alias_already_declared an old_pos apos in
  match StrMap.find_opt an prog.prog_alias with
  | Some id ->
      let var = IntMap.find id prog.prog_dict in
      let old_pos = Pos.get @@ Option.get @@ Com.Var.alias var in
      err old_pos
  | None -> ()

let check_name_in_tmp tmps m_name =
  let vn, vpos = Pos.to_couple m_name in
  let err old_pos = Err.variable_already_declared vn old_pos vpos in
  match StrMap.find_opt vn tmps with
  | Some (Pos.Mark (_, old_pos)) -> err old_pos
  | None -> ()

let check_name_in_args dict args m_name =
  let vn, vpos = Pos.to_couple m_name in
  let err old_pos = Err.variable_already_declared vn old_pos vpos in
  let find (Pos.Mark (id, _)) =
    let var = IntMap.find id dict in
    vn = Com.Var.name_str var
  in
  match List.find_opt find args with
  | Some (Pos.Mark (_, old_pos)) -> err old_pos
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
          (fun names (Pos.Mark (item, _pos)) ->
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
  let rec make_prefix = function
    | name :: tl ->
        let i = Buffer.length buf in
        if i >= String.length name then make_prefix []
        else (
          (if Strings.starts_with ~prefix:(Buffer.contents buf) name then
           let c = match name.[i] with 'a' -> 'b' | _ -> 'a' in
           Buffer.add_char buf c);
          make_prefix tl)
    | [] -> Buffer.contents buf
  in
  make_prefix sorted_names

let empty_program (p : Mast.program) main_target =
  let prog_app =
    let fold s a = StrMap.add a Pos.none s in
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
    prog_var_spaces = StrMap.empty;
    prog_var_spaces_idx = IntMap.empty;
    prog_event_fields = StrMap.empty;
    prog_event_field_idxs = IntMap.empty;
    prog_event_pos = Pos.none;
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
    prog_call_map = CallMap.empty;
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
  let chain_name = Pos.mark name pos in
  let chain_apps =
    List.fold_left
      (fun apps (Pos.Mark (app, app_pos)) -> StrMap.add app app_pos apps)
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
      (fun attributs (Pos.Mark (attr, pos)) ->
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
      let attr, attr_pos = Pos.to_couple m_attr in
      let value = Pos.unmark m_value in
      match StrMap.find_opt attr attributes with
      | Some (Pos.Mark (_, old_pos)) ->
          Err.attribute_already_defined attr old_pos attr_pos
      | None -> StrMap.add attr (Pos.mark value attr_pos) attributes)
    StrMap.empty attr_list

let check_global_var (var : Com.Var.t) (prog : program) : program =
  let name, name_pos = Pos.to_couple var.name in
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
  | Mast.InputVar (Pos.Mark (input_var, _decl_pos)) ->
      let global_category =
        let input_set =
          List.fold_left
            (fun res (Pos.Mark (str, _pos)) -> StrSet.add str res)
            StrSet.empty input_var.input_category
        in
        Com.CatVar.Input input_set
      in
      let var =
        Com.Var.new_tgv ~name:input_var.Mast.input_name ~table:None
          ~is_given_back:input_var.input_is_givenback
          ~alias:(Some input_var.Mast.input_alias)
          ~descr:input_var.Mast.input_description
          ~attrs:(get_attributes input_var.Mast.input_attributes)
          ~cat:global_category
          ~typ:(Option.map Pos.unmark input_var.Mast.input_typ)
      in
      check_global_var var prog
  | Mast.ComputedVar (Pos.Mark (comp_var, _decl_pos)) ->
      let global_category =
        let is_base =
          List.fold_left
            (fun res (Pos.Mark (str, _pos)) ->
              match str with "base" -> true | _ -> res)
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
        Com.Var.new_tgv ~name:m_name ~table:None ~is_given_back ~alias ~descr
          ~attrs ~cat ~typ
      in
      let table =
        match comp_var.Mast.comp_table with
        | Some (Pos.Mark (Mast.LiteralSize sz, _pos)) ->
            let name, name_pos = Pos.to_couple m_name in
            let iFmt = String.map (fun _ -> '0') (Pp.spr "%d" sz) in
            let init i =
              let m_iName =
                Pos.mark (Strings.concat_int name iFmt i) name_pos
              in
              Com.Var.new_tgv ~name:m_iName ~table:None ~is_given_back ~alias
                ~descr ~attrs ~cat ~typ
            in
            Some (Array.init sz init)
        | Some _ -> assert false
        | None -> None
      in
      let prog =
        match table with
        | Some tab -> Array.fold_left (fun p v -> check_global_var v p) prog tab
        | None -> prog
      in
      let var = Com.Var.set_table var table in
      check_global_var var prog

let check_variable_space_decl (vsd : Com.variable_space) (prog : program) :
    program =
  let name, pos = Pos.to_couple vsd.vs_name in
  let vsd, prog_var_spaces_idx =
    match StrMap.find_opt name prog.prog_var_spaces with
    | Some old_id ->
        let old_vsd = IntMap.find old_id prog.prog_var_spaces_idx in
        Err.variable_space_already_declared (Pos.get old_vsd.vs_name) pos
    | None ->
        let vs_id = IntMap.cardinal prog.prog_var_spaces_idx in
        let vsd = { vsd with vs_id } in
        (vsd, IntMap.add vs_id vsd prog.prog_var_spaces_idx)
  in
  let prog_var_spaces =
    if vsd.vs_by_default then
      match StrMap.find_opt "" prog.prog_var_spaces with
      | Some old_id ->
          let old_vsd = IntMap.find old_id prog.prog_var_spaces_idx in
          Err.default_variable_space_already_declared (Pos.get old_vsd.vs_name)
            pos
      | None ->
          prog.prog_var_spaces |> StrMap.add "" vsd.vs_id
          |> StrMap.add name vsd.vs_id
    else prog.prog_var_spaces |> StrMap.add name vsd.vs_id
  in
  { prog with prog_var_spaces; prog_var_spaces_idx }

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
    | None -> Pos.without ""
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
  let name, name_pos = Pos.to_couple err.name in
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
      (fun dom_names (Pos.Mark (sl, sl_pos)) ->
        let id = Com.DomainId.from_marked_list sl in
        Com.DomainIdMap.add id sl_pos dom_names)
      Com.DomainIdMap.empty decl.dom_names
  in
  let dom_id =
    let n, p = Com.DomainIdMap.min_binding dom_names in
    Pos.mark n p
  in
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
  let dom_id_name, dom_id_pos = Pos.to_couple dom_id in
  let syms =
    Com.DomainIdMap.fold
      (fun name name_pos syms ->
        match Com.DomainIdMap.find_opt name syms with
        | Some (Pos.Mark (_, old_pos)) ->
            Err.domain_already_declared rov old_pos name_pos
        | None ->
            let value = Pos.mark dom_id_name name_pos in
            Com.DomainIdMap.add name value syms)
      dom_names syms
  in
  let syms =
    if decl.dom_by_default then
      match Com.DomainIdMap.find_opt Com.DomainId.empty syms with
      | Some (Pos.Mark (_, old_pos)) ->
          Err.default_domain_already_declared rov old_pos dom_id_pos
      | None ->
          let value = Pos.without dom_id_name in
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
        let dom_id, dom_id_pos = Pos.to_couple dom.Com.dom_id in
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
    let add_sym name (Pos.Mark (id, _)) doms =
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

type proc_type = Target of call_compute * Pos.t | Rule | Verif | Func | Filter

type var_env = { prog : program; proc_type : proc_type; vars : int StrMap.t }

let new_var_env ?(vars = StrMap.empty) prog proc_type =
  { prog; proc_type; vars }

let mod_var_env env proc_type = { env with proc_type }

let add_var_env (var : Com.Var.t) env =
  let prog_dict = IntMap.add var.id var env.prog.prog_dict in
  let prog = { env.prog with prog_dict } in
  let vars = StrMap.add (Com.Var.name_str var) var.id env.vars in
  { env with prog; vars }

let check_name_in_env env m_name =
  let name, pos = Pos.to_couple m_name in
  match StrMap.find_opt name env.vars with
  | Some id ->
      let var = IntMap.find id env.prog.prog_dict in
      let old_pos = Pos.get @@ Com.Var.name var in
      Err.variable_already_declared name old_pos pos
  | None -> ()

let rec fold_var_expr (get_var : 'v -> string Pos.marked)
    (fold_sp : Com.var_space -> var_env -> 'a -> 'a)
    (fold_var : Com.var_space -> 'v -> var_mem_type -> var_env -> 'a -> 'a)
    (acc : 'a) (m_expr : 'v Com.m_expression) (env : var_env) : 'a =
  let fold_aux = fold_var_expr get_var fold_sp fold_var in
  let expr, expr_pos = Pos.to_couple m_expr in
  match expr with
  | TestInSet (_positive, e, values) ->
      let acc = fold_aux acc e env in
      List.fold_left
        (fun acc set_value ->
          match set_value with
          | Com.VarValue (Pos.Mark (a, a_pos)) -> (
              if env.proc_type = Filter then
                Err.forbidden_expresion_in_filter a_pos;
              match a with
              | VarAccess (m_sp_opt, m_v) ->
                  let acc = fold_sp m_sp_opt env acc in
                  fold_var m_sp_opt m_v Num env acc
              | TabAccess (m_sp_opt, m_v, m_i) ->
                  let acc = fold_sp m_sp_opt env acc in
                  let acc = fold_var m_sp_opt m_v Table env acc in
                  fold_aux acc m_i env
              | FieldAccess (m_sp_opt, ie, f, _) ->
                  let acc = fold_sp m_sp_opt env acc in
                  let f_name, f_pos = Pos.to_couple f in
                  (match StrMap.find_opt f_name env.prog.prog_event_fields with
                  | Some ef when ef.is_var -> ()
                  | Some _ -> Err.event_field_is_not_a_reference f_name f_pos
                  | None -> Err.unknown_event_field f_name f_pos);
                  fold_aux acc ie env)
          | Com.FloatValue _ -> acc
          | Com.IntervalValue (bn, en) ->
              if Pos.unmark bn > Pos.unmark en then
                Err.wrong_interval_bounds (Pos.get bn);
              acc)
        acc values
  | Comparison (_op, e1, e2) ->
      let acc = fold_aux acc e1 env in
      fold_aux acc e2 env
  | Binop (_op, e1, e2) ->
      let acc = fold_aux acc e1 env in
      fold_aux acc e2 env
  | Unop (_op, e) -> fold_aux acc e env
  | Conditional (e1, e2, e3_opt) -> (
      let acc = fold_aux acc e1 env in
      let acc = fold_aux acc e2 env in
      match e3_opt with Some e3 -> fold_aux acc e3 env | None -> acc)
  | FuncCall (Pos.Mark (func_name, fpos), args) -> (
      let check_func arity =
        if arity > -1 && List.length args <> arity then
          Err.wrong_arity_of_function func_name arity expr_pos;
        List.fold_left (fun acc e -> fold_aux acc e env) acc args
      in
      match func_name with
      | Com.Multimax -> (
          if env.proc_type = Filter then
            Err.forbidden_expresion_in_filter expr_pos;
          match args with
          | [ expr; var_expr ] -> (
              let acc = fold_aux acc expr env in
              match var_expr with
              | Pos.Mark (Var (VarAccess (m_sp_opt, m_v)), _) ->
                  let acc = fold_sp m_sp_opt env acc in
                  fold_var m_sp_opt m_v Table env acc
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
          if env.proc_type = Filter then
            Err.forbidden_expresion_in_filter expr_pos;
          check_func 1
      | Com.NbEvents ->
          if env.proc_type = Filter then
            Err.forbidden_expresion_in_filter expr_pos;
          check_func 0
      | Com.Func fn ->
          if env.proc_type = Filter then
            Err.forbidden_expresion_in_filter expr_pos;
          let fd =
            match StrMap.find_opt fn env.prog.prog_functions with
            | Some fd -> fd
            | None -> Err.function_does_not_exist fn fpos
          in
          check_func (List.length fd.target_args))
  | Literal _ -> acc
  | Var access -> (
      if env.proc_type = Filter then Err.variable_forbidden_in_filter expr_pos;
      match access with
      | VarAccess (m_sp_opt, m_v) ->
          let acc = fold_sp m_sp_opt env acc in
          fold_var m_sp_opt m_v Num env acc
      | TabAccess (m_sp_opt, m_v, m_i) ->
          let acc = fold_sp m_sp_opt env acc in
          let acc = fold_var m_sp_opt m_v Table env acc in
          fold_aux acc m_i env
      | FieldAccess (m_sp_opt, e, f, _) -> (
          match StrMap.find_opt (Pos.unmark f) env.prog.prog_event_fields with
          | Some _ ->
              let acc = fold_sp m_sp_opt env acc in
              fold_aux acc e env
          | None -> Err.unknown_event_field (Pos.unmark f) (Pos.get f)))
  | NbCategory cs ->
      if not (env.proc_type = Filter) then
        Err.expression_only_in_filter expr_pos;
      let cats = mast_to_catvars cs env.prog.prog_var_cats in
      Com.CatVar.Map.iter
        (fun cat pos ->
          if not (Com.CatVar.Map.mem cat env.prog.prog_var_cats) then
            Err.unknown_domain Verif pos)
        cats;
      acc
  | Attribut (Pos.Mark (access, _pos), a) -> (
      match access with
      | VarAccess (m_sp_opt, m_v) ->
          let name, var_pos = Pos.to_couple @@ get_var m_v in
          (match StrMap.find_opt name env.vars with
          | Some id ->
              let var = IntMap.find id env.prog.prog_dict in
              if Com.Var.is_tgv var then (
                let cat = Com.Var.cat var in
                if not (StrMap.mem (Pos.unmark a) (Com.Var.attrs var)) then
                  Err.unknown_attribut_for_var cat (Pos.get a))
              else if Com.Var.is_temp var then
                Err.var_have_no_attrs (Com.Var.name_str var) var_pos
          | None -> Err.unknown_variable var_pos);
          let acc = fold_sp m_sp_opt env acc in
          fold_var m_sp_opt m_v Both env acc
      | TabAccess (m_sp_opt, m_v, m_i) ->
          let name, var_pos = Pos.to_couple @@ get_var m_v in
          (match StrMap.find_opt name env.vars with
          | Some id ->
              let var = IntMap.find id env.prog.prog_dict in
              if Com.Var.is_tgv var then (
                let cat = Com.Var.cat var in
                if not (StrMap.mem (Pos.unmark a) (Com.Var.attrs var)) then
                  Err.unknown_attribut_for_var cat (Pos.get a))
              else if Com.Var.is_temp var then
                Err.var_have_no_attrs (Com.Var.name_str var) var_pos
              else if Com.Var.is_ref var then
                Err.variable_used_as_table (Pos.get @@ Com.Var.name var) var_pos
          | None -> Err.unknown_variable var_pos);
          let acc = fold_sp m_sp_opt env acc in
          let acc = fold_var m_sp_opt m_v Table env acc in
          fold_aux acc m_i env
      | FieldAccess (m_sp_opt, e, f, _) ->
          if env.proc_type = Filter then
            Err.forbidden_expresion_in_filter expr_pos;
          let acc = fold_sp m_sp_opt env acc in
          let f_name, f_pos = Pos.to_couple f in
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
          fold_aux acc e env)
  | Size (Pos.Mark (access, _)) | IsVariable (Pos.Mark (access, _), _) -> (
      match access with
      | VarAccess (m_sp_opt, m_v) ->
          let acc = fold_sp m_sp_opt env acc in
          fold_var m_sp_opt m_v Both env acc
      | TabAccess (m_sp_opt, m_v, m_i) ->
          let acc = fold_sp m_sp_opt env acc in
          let acc = fold_var m_sp_opt m_v Table env acc in
          fold_aux acc m_i env
      | FieldAccess (m_sp_opt, e, f, _) ->
          if env.proc_type = Filter then
            Err.forbidden_expresion_in_filter expr_pos;
          let acc = fold_sp m_sp_opt env acc in
          let f_name, f_pos = Pos.to_couple f in
          (match StrMap.find_opt f_name env.prog.prog_event_fields with
          | Some ef when ef.is_var -> ()
          | Some _ -> Err.event_field_is_not_a_reference f_name f_pos
          | None -> Err.unknown_event_field f_name f_pos);
          fold_aux acc e env)
  | NbAnomalies | NbDiscordances | NbInformatives | NbBloquantes ->
      if env.proc_type = Filter then Err.forbidden_expresion_in_filter expr_pos;
      acc
  | FuncCallLoop _ | Loop _ -> assert false

let get_var_mem_type (var : Com.m_var_name) (env : var_env) :
    var_mem_type Pos.marked =
  let var_data, var_pos = Pos.to_couple var in
  let vn = Com.get_normal_var var_data in
  let to_mem is_t = match is_t with Some _ -> Table | None -> Num in
  match StrMap.find_opt vn env.vars with
  | Some id ->
      let var = IntMap.find id env.prog.prog_dict in
      let mem =
        if Com.Var.is_ref var then Num else to_mem (Com.Var.get_table var)
      in
      Pos.same mem (Com.Var.name var)
  | None -> Err.unknown_variable var_pos

let check_var_space (m_sp_opt : Com.var_space) (env : var_env) : unit =
  match m_sp_opt with
  | None -> ()
  | Some (m_sp, _) -> (
      match env.proc_type with
      | Rule -> Err.var_spaces_forbidden_in_this "rules" (Pos.get m_sp)
      | Verif -> Err.var_spaces_forbidden_in_this "verifs" (Pos.get m_sp)
      | Func -> Err.var_spaces_forbidden_in_this "functions" (Pos.get m_sp)
      | Filter -> Err.var_spaces_forbidden_in_this "filters" (Pos.get m_sp)
      | Target _ -> (
          let sp_name = Com.get_normal_var @@ Pos.unmark m_sp in
          match StrMap.find_opt sp_name env.prog.prog_var_spaces with
          | Some _ -> ()
          | None -> Err.unknown_var_space sp_name (Pos.get m_sp)))

let check_variable (m_sp_opt : Com.var_space) (m_vn : Com.m_var_name)
    (idx_mem : var_mem_type) (env : var_env) : unit =
  let decl_mem, decl_pos = Pos.to_couple @@ get_var_mem_type m_vn env in
  (match (decl_mem, idx_mem) with
  | _, Both | Num, Num | Table, Table -> ()
  | Both, _ -> assert false
  (* | Both, Num -> Err.mixed_variable_used_as_num decl_pos (Pos.get m_vn)
     | Both, Table -> Err.mixed_variable_used_as_table decl_pos (Pos.get m_vn)*)
  | Num, Table -> Err.variable_used_as_table decl_pos (Pos.get m_vn)
  | Table, Num -> Err.table_used_as_variable decl_pos (Pos.get m_vn));
  match m_sp_opt with
  | None -> ()
  | Some (m_sp, _) ->
      let sp_name = Com.get_normal_var @@ Pos.unmark m_sp in
      let sp_id = StrMap.find sp_name env.prog.prog_var_spaces in
      let vsd = IntMap.find sp_id env.prog.prog_var_spaces_idx in
      let v_name = Com.get_normal_var @@ Pos.unmark m_vn in
      let var =
        let id = StrMap.find v_name env.vars in
        IntMap.find id env.prog.prog_dict
      in
      let var_loc = Com.Var.cat_var_loc var in
      if not (Com.CatVar.LocMap.mem var_loc vsd.vs_cats) then
        Err.variable_not_in_var_space v_name sp_name (Pos.get m_vn)

let check_expression (env : var_env) (m_expr : Mast.m_expression) : unit =
  let get_var m_v = Pos.same (Com.get_normal_var @@ Pos.unmark m_v) m_v in
  let fold_sp m_sp_opt env _acc = check_var_space m_sp_opt env in
  let fold_var m_sp_opt var idx_mem env _acc =
    check_variable m_sp_opt var idx_mem env
  in
  fold_var_expr get_var fold_sp fold_var () m_expr env

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

let get_compute_domain_id_str (l : string Pos.marked list Pos.marked)
    (prog : program) : string =
  let buf = Buffer.create 100 in
  Buffer.add_string buf prog.prog_prefix;
  Buffer.add_string buf "_rules";
  let id = add_sml buf l in
  (match Com.DomainIdMap.find_opt id prog.prog_rdom_syms with
  | Some (Pos.Mark (dom_id, _)) ->
      let rdom = Com.DomainIdMap.find dom_id prog.prog_rdoms in
      if not rdom.Com.dom_data.rdom_computable then
        Err.rule_domain_not_computable (Pos.get l)
  | None -> Err.unknown_domain Rule (Pos.get l));
  Buffer.contents buf

let get_compute_chaining_id_str (ch : string Pos.marked) (prog : program) :
    string =
  let buf = Buffer.create 100 in
  Buffer.add_string buf prog.prog_prefix;
  let ch_name, ch_pos = Pos.to_couple ch in
  Buffer.add_string buf "_chaining_";
  Buffer.add_string buf ch_name;
  (match StrMap.find_opt ch_name prog.prog_chainings with
  | Some _ -> ()
  | None -> Err.unknown_chaining ch_pos);
  Buffer.contents buf

let get_compute_verifs_id_str (l : string Pos.marked list Pos.marked)
    (prog : program) : string =
  let buf = Buffer.create 100 in
  Buffer.add_string buf prog.prog_prefix;
  Buffer.add_string buf "_verifs";
  let id = add_sml buf l in
  Buffer.add_char buf '_';
  let cpt = StrMap.cardinal prog.prog_vdom_calls in
  Buffer.add_string buf (Format.sprintf "%d" cpt);
  (match Com.DomainIdMap.find_opt id prog.prog_vdom_syms with
  | Some (Pos.Mark (dom_id, _)) ->
      let vdom = Com.DomainIdMap.find dom_id prog.prog_vdoms in
      if not vdom.Com.dom_data.vdom_verifiable then
        Err.verif_domain_not_verifiable (Pos.get l)
  | None -> Err.unknown_domain Verif (Pos.get l));
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

(*  changer "= Rule" en "<> Target" !!! *)
let rec check_instructions (env : var_env)
    (instrs : Mast.instruction Pos.marked list) :
    program * (int Pos.marked, Mast.error_name) Com.m_instruction list =
  let map_var env m_v =
    let name = Com.get_normal_var (Pos.unmark m_v) in
    let id = StrMap.find name env.vars in
    Pos.same id m_v
  in
  let map_expr env m_expr =
    check_expression env m_expr;
    Com.m_expr_map_var (map_var env) m_expr
  in
  let check_it_var env var =
    let m_name = Pos.same (Com.get_normal_var (Pos.unmark var)) var in
    check_name_in_env env m_name;
    m_name
  in
  let get_sp_opt m_sp_opt =
    match m_sp_opt with
    | None -> None
    | Some (Pos.Mark (sp_name, sp_opt), _) -> (
        let sp = Com.get_normal_var sp_name in
        match StrMap.find_opt sp env.prog.prog_var_spaces with
        | Some _ -> Some sp
        | None -> Err.unknown_var_space sp sp_opt)
  in
  let update_call_map env called instr_pos =
    match env.proc_type with
    | Target (cc, cc_pos) -> (
        match CallMap.find_opt cc env.prog.prog_call_map with
        | Some (called_map, _) -> (
            match CallMap.find_opt called called_map with
            | Some _ -> env.prog.prog_call_map
            | None ->
                let called_map = CallMap.add called instr_pos called_map in
                CallMap.add cc (called_map, cc_pos) env.prog.prog_call_map)
        | None ->
            let called_map = CallMap.one called instr_pos in
            CallMap.add cc (called_map, cc_pos) env.prog.prog_call_map)
    | _ -> env.prog.prog_call_map
  in
  let check_m_access ~onlyVar mem_var env m_a =
    let access, apos = Pos.to_couple m_a in
    match access with
    | Com.VarAccess (m_sp_opt, m_v) ->
        check_var_space m_sp_opt env;
        check_variable m_sp_opt m_v mem_var env;
        let m_v' = map_var env m_v in
        Pos.mark (Com.VarAccess (m_sp_opt, m_v')) apos
    | Com.TabAccess (m_sp_opt, m_v, m_i) ->
        check_var_space m_sp_opt env;
        check_variable m_sp_opt m_v Table env;
        let m_v' = map_var env m_v in
        let m_i' = map_expr env m_i in
        Pos.mark (Com.TabAccess (m_sp_opt, m_v', m_i')) apos
    | Com.FieldAccess (m_sp_opt, m_i, f, id) ->
        if env.proc_type = Rule then
          Err.insruction_forbidden_in_rules (Pos.get m_a);
        check_var_space m_sp_opt env;
        let f_name, f_pos = Pos.to_couple f in
        (match StrMap.find_opt f_name env.prog.prog_event_fields with
        | Some ef ->
            if onlyVar && not ef.is_var then
              Err.event_field_is_not_a_reference f_name f_pos
        | None -> Err.unknown_event_field f_name f_pos);
        let m_i' = map_expr env m_i in
        let a' = Com.FieldAccess (m_sp_opt, m_i', f, id) in
        Pos.mark a' apos
  in
  let rec aux
      ((env, res) :
        var_env * (int Pos.marked, Mast.error_name) Com.m_instruction list)
      (m_instr_list : Mast.instruction Pos.marked list) :
      var_env * (int Pos.marked, Mast.error_name) Com.m_instruction list =
    match m_instr_list with
    | [] -> (env, List.rev res)
    | m_instr :: il -> (
        let instr, instr_pos = Pos.to_couple m_instr in
        match instr with
        | Com.Affectation (Pos.Mark (f, fpos)) -> (
            match f with
            | Com.SingleFormula (VarDecl (m_a, e)) ->
                let m_a' = check_m_access ~onlyVar:false Num env m_a in
                let e' = map_expr env e in
                let f' = Com.SingleFormula (VarDecl (m_a', e')) in
                let instr' = Com.Affectation (Pos.mark f' fpos) in
                aux (env, Pos.mark instr' instr_pos :: res) il
            | Com.SingleFormula (EventFieldRef (m_i, f, iFmt, m_v)) ->
                if env.proc_type = Rule then
                  Err.insruction_forbidden_in_rules instr_pos;
                let f_name, f_pos = Pos.to_couple f in
                (match StrMap.find_opt f_name env.prog.prog_event_fields with
                | Some ef when ef.is_var -> ()
                | Some _ -> Err.event_field_is_not_a_reference f_name f_pos
                | None -> Err.unknown_event_field f_name f_pos);
                let m_i' = map_expr env m_i in
                check_variable None m_v Num env;
                let m_v' = map_var env m_v in
                let f' =
                  Com.SingleFormula (EventFieldRef (m_i', f, iFmt, m_v'))
                in
                let instr' = Com.Affectation (Pos.mark f' fpos) in
                aux (env, Pos.mark instr' instr_pos :: res) il
            | Com.MultipleFormulaes _ -> assert false)
        | Com.IfThenElse (expr, i_then, i_else) ->
            let expr' = map_expr env expr in
            let prog, res_then = check_instructions env i_then in
            let env = { env with prog } in
            let prog, res_else = check_instructions env i_else in
            let env = { env with prog } in
            let res_instr = Com.IfThenElse (expr', res_then, res_else) in
            aux (env, Pos.mark res_instr instr_pos :: res) il
        | Com.WhenDoElse (wdl, ed) ->
            let rec wde (env, res) = function
              | (expr, dl, pos) :: l ->
                  let expr' = map_expr env expr in
                  let prog, res_do = check_instructions env dl in
                  let env = { env with prog } in
                  let res = (expr', res_do, pos) :: res in
                  wde (env, res) l
              | [] ->
                  let prog, res_ed = check_instructions env (Pos.unmark ed) in
                  let env = { env with prog } in
                  let ed' = Pos.same res_ed ed in
                  let res = Com.WhenDoElse (List.rev res, ed') in
                  (env, res)
            in
            let env, wde_res = wde (env, []) wdl in
            aux (env, Pos.mark wde_res instr_pos :: res) il
        | Com.ComputeDomain (rdom, m_sp_opt) ->
            if env.proc_type = Rule then
              Err.insruction_forbidden_in_rules instr_pos;
            let tname = get_compute_domain_id_str rdom env.prog in
            let rdom_list, rdom_pos = Pos.to_couple rdom in
            let id = Com.DomainId.from_marked_list rdom_list in
            let rdom_id =
              Pos.unmark (Com.DomainIdMap.find id env.prog.prog_rdom_syms)
            in
            let seq, prog = get_seq env.prog in
            let prog_rdom_calls =
              let used_data = (Pos.mark seq rdom_pos, rdom_id) in
              StrMap.add tname used_data prog.prog_rdom_calls
            in
            let prog_call_map =
              let sp_opt = get_sp_opt m_sp_opt in
              update_call_map env (CallDomain (tname, id, sp_opt)) instr_pos
            in
            let prog = { prog with prog_rdom_calls; prog_call_map } in
            let env = { env with prog } in
            let res_instr =
              Com.ComputeTarget (Pos.without tname, [], m_sp_opt)
            in
            aux (env, Pos.mark res_instr instr_pos :: res) il
        | Com.ComputeChaining (chain, m_sp_opt) ->
            if env.proc_type = Rule then
              Err.insruction_forbidden_in_rules instr_pos;
            let tname = get_compute_chaining_id_str chain env.prog in
            let prog_call_map =
              let sp_opt = get_sp_opt m_sp_opt in
              let cc = CallChaining (tname, Pos.unmark chain, sp_opt) in
              update_call_map env cc instr_pos
            in
            let prog = { env.prog with prog_call_map } in
            let env = { env with prog } in
            let res_instr =
              Com.ComputeTarget (Pos.without tname, [], m_sp_opt)
            in
            aux (env, Pos.mark res_instr instr_pos :: res) il
        | Com.ComputeVerifs (vdom, expr, m_sp_opt) ->
            if env.proc_type = Rule then
              Err.insruction_forbidden_in_rules instr_pos;
            let vdom_list, vdom_pos = Pos.to_couple vdom in
            let tname = get_compute_verifs_id_str vdom env.prog in
            let id = Com.DomainId.from_marked_list vdom_list in
            let vdom_id =
              Pos.unmark (Com.DomainIdMap.find id env.prog.prog_vdom_syms)
            in
            let seq, prog = get_seq env.prog in
            check_expression (mod_var_env env Filter) expr;
            let prog_vdom_calls =
              let used_data = (Pos.mark seq vdom_pos, vdom_id, expr) in
              StrMap.add tname used_data prog.prog_vdom_calls
            in
            let prog_call_map =
              let sp_opt = get_sp_opt m_sp_opt in
              update_call_map env (CallVerifs (tname, id, sp_opt)) instr_pos
            in
            let prog = { prog with prog_vdom_calls; prog_call_map } in
            let env = { env with prog } in
            let res_instr =
              Com.ComputeTarget (Pos.without tname, [], m_sp_opt)
            in
            aux (env, Pos.mark res_instr instr_pos :: res) il
        | Com.VerifBlock instrs ->
            if env.proc_type = Rule then
              Err.insruction_forbidden_in_rules instr_pos;
            let prog, res_instrs = check_instructions env instrs in
            let env = { env with prog } in
            let res_instr = Com.VerifBlock res_instrs in
            aux (env, Pos.mark res_instr instr_pos :: res) il
        | Com.ComputeTarget (Pos.Mark (tn, tpos), targs, m_sp_opt) ->
            if env.proc_type = Rule then
              Err.insruction_forbidden_in_rules instr_pos;
            (match StrMap.find_opt tn env.prog.prog_targets with
            | None -> Err.unknown_target tn tpos
            | Some target ->
                let nb_args = List.length target.target_args in
                if List.length targs <> nb_args then
                  Err.wrong_number_of_args nb_args tpos);
            let prog_call_map =
              let sp_opt = get_sp_opt m_sp_opt in
              update_call_map env (CallTarget (tn, sp_opt)) instr_pos
            in
            let prog = { env.prog with prog_call_map } in
            let env = { env with prog } in
            let targs' =
              List.map (check_m_access ~onlyVar:true Num env) targs
            in
            let instr' =
              Com.ComputeTarget (Pos.mark tn tpos, targs', m_sp_opt)
            in
            aux (env, Pos.mark instr' instr_pos :: res) il
        | Com.Print (std, args) ->
            let args' =
              List.map
                (fun m_arg ->
                  let arg, arg_pos = Pos.to_couple m_arg in
                  let arg' =
                    match arg with
                    | Com.PrintString s -> Com.PrintString s
                    | Com.PrintAccess (info, m_a) ->
                        let a' =
                          match Pos.unmark m_a with
                          | Com.VarAccess (m_sp_opt, v) ->
                              check_var_space m_sp_opt env;
                              check_variable m_sp_opt v Both env;
                              Com.VarAccess (m_sp_opt, map_var env v)
                          | Com.TabAccess (m_sp_opt, m_v, m_i) ->
                              check_var_space m_sp_opt env;
                              check_variable m_sp_opt m_v Table env;
                              let m_v' = map_var env m_v in
                              let m_i' = map_expr env m_i in
                              Com.TabAccess (m_sp_opt, m_v', m_i')
                          | Com.FieldAccess (m_sp_opt, e, f, id) -> (
                              let f_name, f_pos = Pos.to_couple f in
                              check_var_space m_sp_opt env;
                              match
                                StrMap.find_opt f_name
                                  env.prog.prog_event_fields
                              with
                              | Some ef when ef.is_var ->
                                  let e' = map_expr env e in
                                  Com.FieldAccess (m_sp_opt, e', f, id)
                              | Some _ ->
                                  Err.event_field_is_not_a_reference f_name
                                    f_pos
                              | None -> Err.unknown_event_field f_name f_pos)
                        in
                        Com.PrintAccess (info, Pos.same a' m_a)
                    | Com.PrintIndent e ->
                        let e' = map_expr env e in
                        Com.PrintIndent e'
                    | Com.PrintExpr (e, min, max) ->
                        let e' = map_expr env e in
                        Com.PrintExpr (e', min, max)
                  in
                  Pos.mark arg' arg_pos)
                args
            in
            let instr' = Com.Print (std, args') in
            aux (env, Pos.mark instr' instr_pos :: res) il
        | Com.Iterate (var, al, var_params, instrs) ->
            if env.proc_type = Rule then
              Err.insruction_forbidden_in_rules instr_pos;
            let m_name = check_it_var env var in
            let env' =
              let v = Com.Var.new_ref ~name:m_name in
              add_var_env v env
            in
            let var' = map_var env' var in
            let al' = List.map (check_m_access ~onlyVar:true Num env) al in
            let var_params' =
              List.map
                (fun (vcats, expr, m_sp_opt) ->
                  let cats = mast_to_catvars vcats env.prog.prog_var_cats in
                  (match get_sp_opt m_sp_opt with
                  | None -> ()
                  | Some sp ->
                      let vsd_id = StrMap.find sp env.prog.prog_var_spaces in
                      let vsd =
                        IntMap.find vsd_id env.prog.prog_var_spaces_idx
                      in
                      let iter cat cat_pos =
                        let loc =
                          (Com.CatVar.Map.find cat env.prog.prog_var_cats).loc
                        in
                        match Com.CatVar.LocMap.find_opt loc vsd.vs_cats with
                        | Some _ -> ()
                        | None ->
                            Err.category_forbidden_with_space cat_pos
                              (Pos.unmark vsd.vs_name)
                      in
                      Com.CatVar.Map.iter iter cats);
                  (vcats, map_expr env' expr, m_sp_opt))
                var_params
            in
            let prog, instrs' = check_instructions env' instrs in
            let env = { env with prog } in
            let instr' = Com.Iterate (var', al', var_params', instrs') in
            aux (env, Pos.mark instr' instr_pos :: res) il
        | Com.Iterate_values (var, var_intervals, instrs) ->
            let m_name = check_it_var env var in
            let env' =
              let v = Com.Var.new_temp ~name:m_name ~table:None in
              add_var_env v env
            in
            let var' = map_var env' var in
            let var_intervals' =
              List.map
                (fun (e0, e1, step) ->
                  let e0' = map_expr env e0 in
                  let e1' = map_expr env e1 in
                  let step' = map_expr env step in
                  (e0', e1', step'))
                var_intervals
            in
            let prog, instrs' = check_instructions env' instrs in
            let env = { env with prog } in
            let instr' = Com.Iterate_values (var', var_intervals', instrs') in
            aux (env, Pos.mark instr' instr_pos :: res) il
        | Com.Restore (al, var_params, evts, evtfs, instrs) ->
            if env.proc_type = Rule then
              Err.insruction_forbidden_in_rules instr_pos;
            let al' = List.map (check_m_access ~onlyVar:true Both env) al in
            let env, var_params' =
              let fold (env, var_params') (var, vcats, expr, m_sp_opt) =
                let m_name = check_it_var env var in
                let cats = mast_to_catvars vcats env.prog.prog_var_cats in
                (match get_sp_opt m_sp_opt with
                | None -> ()
                | Some sp ->
                    let vsd_id = StrMap.find sp env.prog.prog_var_spaces in
                    let vsd = IntMap.find vsd_id env.prog.prog_var_spaces_idx in
                    let iter cat cat_pos =
                      let loc =
                        (Com.CatVar.Map.find cat env.prog.prog_var_cats).loc
                      in
                      match Com.CatVar.LocMap.find_opt loc vsd.vs_cats with
                      | Some _ -> ()
                      | None ->
                          Err.category_forbidden_with_space cat_pos
                            (Pos.unmark vsd.vs_name)
                    in
                    Com.CatVar.Map.iter iter cats);
                let env' =
                  let v = Com.Var.new_ref ~name:m_name in
                  add_var_env v env
                in
                let var' = map_var env' var in
                let expr' = map_expr env' expr in
                let env = { env with prog = env'.prog } in
                let var_params' =
                  (var', vcats, expr', m_sp_opt) :: var_params'
                in
                (env, var_params')
              in
              let env, var_params' = List.fold_left fold (env, []) var_params in
              (env, List.rev var_params')
            in
            let evts' = List.map (map_expr env) evts in
            let env, evtfs' =
              let fold (env, evtfs') (var, expr) =
                let m_name = check_it_var env var in
                let env' =
                  let v = Com.Var.new_temp ~name:m_name ~table:None in
                  add_var_env v env
                in
                let var' = map_var env' var in
                let expr' = map_expr env' expr in
                let env = { env with prog = env'.prog } in
                let evtfs' = (var', expr') :: evtfs' in
                (env, evtfs')
              in
              let env, evtfs' = List.fold_left fold (env, []) evtfs in
              (env, List.rev evtfs')
            in
            let prog, instrs' = check_instructions env instrs in
            let env = { env with prog } in
            let instr' =
              Com.Restore (al', var_params', evts', evtfs', instrs')
            in
            aux (env, Pos.mark instr' instr_pos :: res) il
        | Com.ArrangeEvents (sort, filter, add, instrs) ->
            if env.proc_type = Rule then
              Err.insruction_forbidden_in_rules instr_pos;
            let env, sort' =
              match sort with
              | Some (var0, var1, expr) ->
                  let m_name0 = check_it_var env var0 in
                  let m_name1 = check_it_var env var1 in

                  let env' =
                    let v0 = Com.Var.new_temp ~name:m_name0 ~table:None in
                    let v1 = Com.Var.new_temp ~name:m_name1 ~table:None in
                    env |> add_var_env v0 |> add_var_env v1
                  in
                  let var0' = map_var env' var0 in
                  let var1' = map_var env' var1 in
                  let expr' = map_expr env' expr in
                  let env = { env with prog = env'.prog } in
                  (env, Some (var0', var1', expr'))
              | None -> (env, None)
            in
            let env, filter' =
              match filter with
              | Some (var, expr) ->
                  let m_name = check_it_var env var in
                  let env' =
                    let v = Com.Var.new_temp ~name:m_name ~table:None in
                    add_var_env v env
                  in
                  let var' = map_var env' var in
                  let expr' = map_expr env' expr in
                  let env = { env with prog = env'.prog } in
                  (env, Some (var', expr'))
              | None -> (env, None)
            in
            let add' = Option.map (map_expr env) add in
            let prog, instrs' = check_instructions env instrs in
            let env = { env with prog } in
            let instr' = Com.ArrangeEvents (sort', filter', add', instrs') in
            aux (env, Pos.mark instr' instr_pos :: res) il
        | Com.RaiseError (m_err, m_var_opt) ->
            if env.proc_type = Rule then
              Err.insruction_forbidden_in_rules instr_pos;
            let err_name, err_pos = Pos.to_couple m_err in
            (match StrMap.find_opt err_name env.prog.prog_errors with
            | None -> Err.unknown_error err_pos
            | Some _ -> ());
            (match m_var_opt with
            | Some m_var -> (
                let var_name, var_pos = Pos.to_couple m_var in
                match StrMap.find_opt var_name env.vars with
                | Some id ->
                    let var = IntMap.find id env.prog.prog_dict in
                    if not (Com.Var.is_tgv var || Com.Var.is_ref var) then
                      Err.forbidden_variable_in_raise var_pos
                | None -> Err.unknown_variable var_pos)
            | None -> ());
            let instr' = Com.RaiseError (m_err, m_var_opt) in
            aux (env, Pos.mark instr' instr_pos :: res) il
        | Com.CleanErrors ->
            if env.proc_type = Rule then
              Err.insruction_forbidden_in_rules instr_pos;
            aux (env, Pos.mark Com.CleanErrors instr_pos :: res) il
        | Com.ExportErrors ->
            if env.proc_type = Rule then
              Err.insruction_forbidden_in_rules instr_pos;
            aux (env, Pos.mark Com.ExportErrors instr_pos :: res) il
        | Com.FinalizeErrors ->
            if env.proc_type = Rule then
              Err.insruction_forbidden_in_rules instr_pos;
            aux (env, Pos.mark Com.FinalizeErrors instr_pos :: res) il)
  in
  let env, res = aux (env, []) instrs in
  (env.prog, res)

let inout_expression (env : var_env) (m_expr : int Pos.marked Com.m_expression)
    : Pos.t StrMap.t =
  let get_var m_id =
    let var = IntMap.find (Pos.unmark m_id) env.prog.prog_dict in
    Pos.same (Com.Var.name_str var) m_id
  in
  let fold_sp _m_sp_opt _env acc = acc in
  let fold_var _m_sp_opt m_id _idx_mem _env acc =
    let name, pos = Pos.to_couple @@ get_var m_id in
    StrMap.union_snd (StrMap.one name pos) acc
  in
  fold_var_expr get_var fold_sp fold_var StrMap.empty m_expr env

let rec inout_instrs (env : var_env) (tmps : Pos.t StrMap.t)
    (instrs : (int Pos.marked, Mast.error_name) Com.m_instruction list) :
    Pos.t StrMap.t * Pos.t StrMap.t * Pos.t list StrMap.t =
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
  let rec aux (tmps, in_vars, out_vars, def_vars) = function
    | [] -> (tmps, in_vars, out_vars, def_vars)
    | m_instr :: il -> (
        let instr, instr_pos = Pos.to_couple m_instr in
        match instr with
        | Com.Affectation (Pos.Mark (f, _)) -> (
            match f with
            | Com.SingleFormula (VarDecl (m_access, e)) -> (
                let access, access_pos = Pos.to_couple m_access in
                let in_vars_aff = inout_expression env e in
                match access with
                | VarAccess (_, m_id) ->
                    let m_v =
                      let var =
                        IntMap.find (Pos.unmark m_id) env.prog.prog_dict
                      in
                      Pos.same (Com.Var.name_str var) m_id
                    in
                    let out_vars_lvalue =
                      StrMap.one (Pos.unmark m_v) (Pos.get m_v)
                    in
                    let in_vars =
                      StrMap.union_fst in_vars (diff_map in_vars_aff out_vars)
                    in
                    let out_vars = StrMap.union_snd out_vars_lvalue out_vars in
                    let def_vars =
                      let vn = Pos.unmark m_v in
                      let def_list =
                        match StrMap.find_opt vn def_vars with
                        | None -> [ access_pos ]
                        | Some l -> access_pos :: l
                      in
                      StrMap.add vn def_list def_vars
                    in
                    aux (tmps, in_vars, out_vars, def_vars) il
                | TabAccess (_, m_id, m_i) ->
                    let m_v =
                      let var =
                        IntMap.find (Pos.unmark m_id) env.prog.prog_dict
                      in
                      Pos.same (Com.Var.name_str var) m_id
                    in
                    let out_vars_lvalue =
                      StrMap.one (Pos.unmark m_v) (Pos.get m_v)
                    in
                    let in_vars_i = inout_expression env m_i in
                    let in_vars_aff = StrMap.union_fst in_vars_i in_vars_aff in
                    let in_vars =
                      StrMap.union_fst in_vars (diff_map in_vars_aff out_vars)
                    in
                    let out_vars = StrMap.union_fst out_vars_lvalue out_vars in
                    let def_vars =
                      let vn = Pos.unmark m_v in
                      let def_list =
                        match StrMap.find_opt vn def_vars with
                        | None -> [ access_pos ]
                        | Some l -> access_pos :: l
                      in
                      StrMap.add vn def_list def_vars
                    in
                    aux (tmps, in_vars, out_vars, def_vars) il
                | FieldAccess _ -> Err.insruction_forbidden_in_rules instr_pos)
            | Com.SingleFormula (EventFieldRef _) ->
                Err.insruction_forbidden_in_rules instr_pos
            | Com.MultipleFormulaes _ -> assert false)
        | Com.IfThenElse (expr, i_then, i_else) ->
            let in_expr = inout_expression env expr in
            let in_then, out_then, def_then = inout_instrs env tmps i_then in
            let in_else, out_else, def_else = inout_instrs env tmps i_else in
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
            aux (tmps, in_vars, out_vars, def_vars) il
        | Com.WhenDoElse (wdl, ed) ->
            let rec wde (in_vars, out_vars, def_vars) = function
              | (expr, dl, _pos) :: l ->
                  let in_expr = inout_expression env expr in
                  let in_do, out_do, def_do = inout_instrs env tmps dl in
                  let in_vars =
                    in_vars |> StrMap.union_snd in_expr
                    |> StrMap.union_snd in_do
                  in
                  let out_vars = out_vars |> StrMap.union_snd out_do in
                  let def_vars = merge_par_defs def_vars def_do in
                  wde (in_vars, out_vars, def_vars) l
              | [] ->
                  let in_ed, out_ed, def_ed =
                    inout_instrs env tmps (Pos.unmark ed)
                  in
                  let in_vars = in_vars |> StrMap.union_snd in_ed in
                  let out_vars = out_vars |> StrMap.union_snd out_ed in
                  let def_vars = merge_par_defs def_vars def_ed in
                  (in_vars, out_vars, def_vars)
            in
            let in_vars, out_vars, def_vars_wde =
              wde (in_vars, out_vars, StrMap.empty) wdl
            in
            let def_vars = merge_seq_defs def_vars def_vars_wde in
            aux (tmps, in_vars, out_vars, def_vars) il
        | Com.ComputeDomain _ | Com.ComputeChaining _ | Com.ComputeVerifs _
        | Com.VerifBlock _ | Com.ComputeTarget _ ->
            Err.insruction_forbidden_in_rules instr_pos
        | Com.Print _ -> aux (tmps, in_vars, out_vars, def_vars) il
        | Com.Iterate _ -> Err.insruction_forbidden_in_rules instr_pos
        | Com.Iterate_values (m_id, var_intervals, instrs) ->
            let var_name, var_pos =
              let var = IntMap.find (Pos.unmark m_id) env.prog.prog_dict in
              (Com.Var.name_str var, Pos.get m_id)
            in
            let tmps' = StrMap.add var_name var_pos tmps in
            let in_exprs =
              List.fold_left
                (fun in_exprs (e0, e1, step) ->
                  in_exprs
                  |> StrMap.union_snd (inout_expression env e0)
                  |> StrMap.union_snd (inout_expression env e1)
                  |> StrMap.union_snd (inout_expression env step))
                StrMap.empty var_intervals
            in
            let in_instrs, out_instrs, def_instrs =
              inout_instrs env tmps' instrs
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
            aux (tmps', in_vars, out_vars, def_vars) il
        | Com.Restore _ -> Err.insruction_forbidden_in_rules instr_pos
        | Com.ArrangeEvents _ -> Err.insruction_forbidden_in_rules instr_pos
        | Com.RaiseError _ -> Err.insruction_forbidden_in_rules instr_pos
        | Com.CleanErrors | Com.ExportErrors | Com.FinalizeErrors ->
            Err.insruction_forbidden_in_rules instr_pos)
  in
  let tmps', in_vars, out_vars, def_vars =
    aux (tmps, StrMap.empty, StrMap.empty, StrMap.empty) instrs
  in
  if env.proc_type = Rule then
    StrMap.iter
      (fun vn l ->
        if List.length l > 1 && not (is_vartmp vn) then
          Errors.print_multispanned_warning
            (Format.asprintf
               "Variable %s is defined more than once in the same rule" vn)
            (List.map (fun pos -> (None, pos)) (List.rev l)))
      (* List.rev for purely cosmetic reasons *)
      def_vars;
  let in_vars = diff_map in_vars tmps' in
  let out_vars = diff_map out_vars tmps' in
  let def_vars = diff_map def_vars tmps' in
  (in_vars, out_vars, def_vars)

let check_code (env : var_env) (m_tname : string Pos.marked) tmp_vars args
    result instrs =
  let tname, tpos = Pos.to_couple m_tname in
  let tmp_vars', env =
    let vars, env =
      List.fold_left
        (fun (vars, env) ((Pos.Mark (vn, vpos) as m_v), sz) ->
          let check_tmp vars (Pos.Mark (vn, vpos)) =
            let err old_pos =
              Err.temporary_variable_already_declared vn old_pos vpos
            in
            match StrMap.find_opt vn vars with
            | Some (Pos.Mark (_, old_pos)) -> err old_pos
            | None -> ()
          in
          check_name_in_tgv env.prog m_v;
          check_tmp vars m_v;
          let size = Option.map Pos.unmark (Mast.get_table_size_opt sz) in
          match size with
          | None ->
              let var = Com.Var.new_temp ~name:m_v ~table:None in
              (StrMap.add vn (Pos.mark var.id vpos) vars, add_var_env var env)
          | Some sz_int ->
              let iFmt = String.map (fun _ -> '0') (Pp.spr "%d" sz_int) in
              let rec loop (vars, env) i =
                if i >= sz_int then (vars, env)
                else
                  let iName = Strings.concat_int vn iFmt i in
                  let m_iName = Pos.mark iName vpos in
                  check_tmp vars m_iName;
                  let var = Com.Var.new_temp ~name:m_iName ~table:None in
                  let vars = StrMap.add iName (Pos.mark var.id vpos) vars in
                  let env = add_var_env var env in
                  loop (vars, env) (i + 1)
              in
              loop (vars, env) 0)
        (StrMap.empty, env) tmp_vars
    in
    List.fold_left
      (fun (vars, env) ((Pos.Mark (vn, vpos) as m_v), sz) ->
        let size = Option.map Pos.unmark (Mast.get_table_size_opt sz) in
        match size with
        | None -> (vars, env)
        | Some sz_int ->
            let table =
              let iFmt = String.map (fun _ -> '0') (Pp.spr "%d" sz_int) in
              let init i =
                let iName = Strings.concat_int vn iFmt i in
                let iId = Pos.unmark @@ StrMap.find iName vars in
                IntMap.find iId env.prog.prog_dict
              in
              Some (Array.init sz_int init)
            in
            let var = Com.Var.new_temp ~name:m_v ~table in
            (StrMap.add vn (Pos.mark var.id vpos) vars, add_var_env var env))
      (vars, env) tmp_vars
  in
  let args', env =
    if env.proc_type = Rule then ([], env)
    else
      let fold (args, env) m_v =
        check_name_in_tgv env.prog m_v;
        check_name_in_tmp tmp_vars' m_v;
        let var =
          if env.proc_type = Func then Com.Var.new_arg ~name:m_v
          else Com.Var.new_ref ~name:m_v
        in
        (Pos.same var.id m_v :: args, add_var_env var env)
      in
      let args', env = List.fold_left fold ([], env) args in
      (List.rev args', env)
  in
  let result', env =
    if env.proc_type = Rule then (None, env)
    else
      match result with
      | Some m_name ->
          if not (env.proc_type = Func) then
            Err.target_must_not_have_a_result tname tpos;
          check_name_in_tgv env.prog m_name;
          check_name_in_tmp tmp_vars' m_name;
          check_name_in_args env.prog.prog_dict args' m_name;
          let var = Com.Var.new_res ~name:m_name in
          (Some (Pos.same var.id m_name), add_var_env var env)
      | None ->
          if env.proc_type = Func then Err.function_result_missing tname tpos;
          (None, env)
  in
  let prog', instrs' = check_instructions env instrs in
  let env' = { env with prog = prog' } in
  if env.proc_type = Func then (
    let tmps = StrMap.map Pos.get tmp_vars' in
    let in_vars, out_vars, _ = inout_instrs env' tmps instrs' in
    let vr = Pos.unmark (Option.get result) in
    let bad_in_vars =
      List.fold_left
        (fun res (Pos.Mark (vn, _)) -> StrMap.remove vn res)
        in_vars args
      |> StrMap.remove vr
    in
    let bad_out_vars = StrMap.remove vr out_vars in
    (if StrMap.card bad_in_vars > 0 then
     let vn, vpos = StrMap.min_binding bad_in_vars in
     Err.forbidden_in_var_in_function vn tname vpos);
    if StrMap.card bad_out_vars > 0 then
      let vn, vpos = StrMap.min_binding bad_out_vars in
      Err.forbidden_out_var_in_function vn tname vpos);
  (env', args', result', tmp_vars', instrs')

let check_target (proc_type : proc_type) (t : Mast.target) (prog : program) :
    program =
  let target_name = t.target_name in
  let tname, tpos = Pos.to_couple target_name in
  if Com.Func tname <> Pos.unmark (Parse_utils.parse_function_name target_name)
  then Err.is_base_function tname tpos;
  (match StrMap.find_opt tname prog.prog_targets with
  | Some { target_name = Pos.Mark (_, old_pos); _ } ->
      Err.target_already_declared tname old_pos tpos
  | None -> ());
  let target_file = Some (get_target_file tpos) in
  let target_apps =
    (* Already checked during preprocessing *)
    t.target_apps
  in
  let env = new_var_env ~vars:prog.prog_vars prog proc_type in
  let env, target_args, target_result, target_tmp_vars, target_prog =
    check_code env target_name t.target_tmp_vars t.target_args t.target_result
      t.target_prog
  in
  let prog = env.prog in
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
  if proc_type = Func then
    let prog_functions = StrMap.add tname target prog.prog_functions in
    { prog with prog_functions }
  else
    let prog_targets = StrMap.add tname target prog.prog_targets in
    { prog with prog_targets }

let check_rule (r : Mast.rule) (prog : program) : program =
  let id, id_pos = Pos.to_couple r.Mast.rule_number in
  let rule_id = Pos.mark id id_pos in
  let rule_apps =
    (* Already checked during preprocessing *)
    StrMap.map Pos.get r.Mast.rule_apps
  in
  let rdom_id =
    Com.DomainId.from_marked_list (Pos.unmark r.Mast.rule_tag_names)
  in
  let rule_domain, rule_domain_pos =
    let rid, rid_pos =
      match Com.DomainIdMap.find_opt rdom_id prog.prog_rdom_syms with
      | Some m_rid -> Pos.to_couple m_rid
      | None -> Err.unknown_domain Rule (Pos.get r.Mast.rule_tag_names)
    in
    let rule_domain = Com.DomainIdMap.find rid prog.prog_rdoms in
    (rule_domain, rid_pos)
  in
  let rule_chains, prog_chainings =
    let fold _ (Pos.Mark (ch, chpos)) (rule_chains, prog_chainings) =
      (* Already checked during preprocessing *)
      let chain = StrMap.find ch prog.prog_chainings in
      let chain_rules =
        IntMap.add id (Pos.mark rule_domain rule_domain_pos) chain.chain_rules
      in
      let chain = { chain with chain_rules } in
      let rule_chains = StrMap.add ch chpos rule_chains in
      let prog_chainings = StrMap.add ch chain prog_chainings in
      (rule_chains, prog_chainings)
    in
    StrMap.fold fold r.rule_chainings (StrMap.empty, prog.prog_chainings)
  in
  let env = new_var_env ~vars:prog.prog_vars prog Rule in
  let env, _, _, rule_tmp_vars, rule_instrs =
    check_code env (Pos.without "") r.rule_tmp_vars [] None r.rule_formulaes
  in
  let prog = env.prog in
  let rule_in_vars, rule_out_vars =
    let tmps = StrMap.map Pos.get rule_tmp_vars in
    let in_vars, out_vars, _ = inout_instrs env tmps rule_instrs in
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
        let tpos = Pos.get rule.rule_id in
        let tname = Format.sprintf "%s_regle_%d" prog.prog_prefix id in
        let target_file = Some (get_target_file tpos) in
        let target_prog = rule.rule_instrs in
        let target =
          Com.
            {
              target_name = Pos.mark tname tpos;
              target_file;
              target_apps = StrMap.mapi Pos.mark prog.prog_app;
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

let rule_graph_to_target_names (rdom_chain : rdom_or_chain) (prog : program)
    (rule_graph : string IntMap.t option IntMap.t) : string Pos.marked list =
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
      let rule = IntMap.find id prog.prog_rules in
      Pos.same name rule.rule_id)
    sorted_rules

let rdom_rule_filter (rdom : Com.rule_domain_data Com.domain) (rule : rule) :
    bool =
  (match rdom.Com.dom_used with
  | Some (Pos.Mark (rdom_seq, seq_pos)) ->
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
  let prog_targets, prog_call_map =
    Com.DomainIdMap.fold
      (fun rdom_id rdom (prog_targets, prog_call_map) ->
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
          let target_names =
            rule_graph_to_target_names (RuleDomain rdom_id) prog rule_graph
          in
          let target_prog =
            List.map
              (fun m_name ->
                Pos.same (Com.ComputeTarget (m_name, [], None)) m_name)
              target_names
          in
          let tpos = Pos.get rdom.Com.dom_id in
          let tname =
            let spl =
              Com.DomainId.fold (fun s l -> Pos.without s :: l) rdom_id []
            in
            get_compute_domain_id_str (Pos.mark spl tpos) prog
          in
          let prog_call_map =
            let cc_dom = CallDomain (tname, rdom_id, None) in
            let ccm =
              List.fold_left
                (fun ccm m_name ->
                  let cc = CallTarget (Pos.unmark m_name, None) in
                  CallMap.add cc (Pos.get m_name) ccm)
                CallMap.empty target_names
            in
            CallMap.add cc_dom (ccm, tpos) prog_call_map
          in
          let target =
            Com.
              {
                target_name = Pos.mark tname tpos;
                target_file = None;
                target_apps = StrMap.mapi Pos.mark prog.prog_app;
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
          (prog_targets, prog_call_map))
        else (prog_targets, prog_call_map))
      prog.prog_rdoms
      (prog.prog_targets, prog.prog_call_map)
  in
  { prog with prog_targets; prog_call_map }

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
  let prog_targets, prog_call_map =
    StrMap.fold
      (fun ch_name chain (prog_targets, prog_call_map) ->
        let all_ids =
          Com.DomainIdMap.fold
            (fun _ rdom ids ->
              let uid = Pos.unmark rdom.Com.dom_id in
              Com.DomainIdSet.add uid ids)
            prog.prog_rdoms Com.DomainIdSet.empty
        in
        let sup_ids =
          IntMap.fold
            (fun _ (Pos.Mark (rdom, id_pos)) sup_ids ->
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
        let target_names =
          rule_graph_to_target_names (Chaining ch_name) prog rule_graph
        in
        let target_prog =
          List.map
            (fun m_name ->
              Pos.same (Com.ComputeTarget (m_name, [], None)) m_name)
            target_names
        in
        let tpos = Pos.get chain.chain_name in
        let tname = get_compute_chaining_id_str chain.chain_name prog in
        let prog_call_map =
          let cc_dom =
            CallChaining (tname, Pos.unmark chain.chain_name, None)
          in
          let ccm =
            List.fold_left
              (fun ccm m_name ->
                let cc = CallTarget (Pos.unmark m_name, None) in
                CallMap.add cc (Pos.get m_name) ccm)
              CallMap.empty target_names
          in
          CallMap.add cc_dom (ccm, tpos) prog_call_map
        in
        let target =
          Com.
            {
              target_name = Pos.mark tname tpos;
              target_file = None;
              target_apps = StrMap.mapi Pos.mark prog.prog_app;
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
        (prog_targets, prog_call_map))
      prog.prog_chainings
      (prog.prog_targets, prog.prog_call_map)
  in
  { prog with prog_targets; prog_call_map }

let check_verif (v : Mast.verification) (prog : program) : program =
  let verif_apps =
    (* Already checked during preprocessing *)
    StrMap.map Pos.get v.Mast.verif_apps
  in
  let vdom_id =
    Com.DomainId.from_marked_list (Pos.unmark v.Mast.verif_tag_names)
  in
  let verif_domain =
    let vid =
      match Com.DomainIdMap.find_opt vdom_id prog.prog_vdom_syms with
      | Some (Pos.Mark (vid, _)) -> vid
      | None -> Err.unknown_domain Verif (Pos.get v.Mast.verif_tag_names)
    in
    Com.DomainIdMap.find vid prog.prog_vdoms
  in
  let prog_verifs, prog, _ =
    List.fold_left
      (fun (prog_verifs, prog, num) (Pos.Mark (cond, cond_pos)) ->
        let id, id_pos = Pos.to_couple v.Mast.verif_number in
        let id = id + num in
        let verif_id = Pos.mark id id_pos in
        let verif_expr = cond.Mast.verif_cond_expr in
        let verif_error, verif_var = cond.Mast.verif_cond_error in
        let err_name, err_pos = Pos.to_couple verif_error in
        let verif_is_blocking =
          match StrMap.find_opt err_name prog.prog_errors with
          | None -> Err.unknown_error err_pos
          | Some err -> (
              match err.typ with Com.Error.Anomaly -> true | _ -> false)
        in
        (match verif_var with
        | Some (Pos.Mark (var_name, var_pos)) -> (
            match StrMap.find_opt var_name prog.prog_vars with
            | None -> Err.unknown_variable var_pos
            | Some _ -> ())
        | None -> ());
        let verif_cat_var_stats, verif_var_stats =
          let get_var m_v =
            Pos.same (Com.get_normal_var @@ Pos.unmark m_v) m_v
          in
          let fold_sp _m_sp_opt _env (vdom_sts, var_sts) =
            (vdom_sts, var_sts)
          in
          let fold_var _m_sp_opt m_v idx_mem env (vdom_sts, var_sts) =
            check_variable None m_v idx_mem env;
            let name = Com.get_normal_var (Pos.unmark m_v) in
            let id = StrMap.find name env.vars in
            let var = IntMap.find id env.prog.prog_dict in
            let cat = Com.Var.cat var in
            if not (Com.CatVar.Map.mem cat verif_domain.dom_data.vdom_auth) then
              Err.variable_with_forbidden_category (Pos.get m_v);
            let incr = function None -> Some 1 | Some i -> Some (i + 1) in
            let vdom_sts = Com.CatVar.Map.update cat incr vdom_sts in
            let var_sts = StrMap.update name incr var_sts in
            (vdom_sts, var_sts)
          in
          let init = (Com.CatVar.Map.empty, StrMap.empty) in
          let env = new_var_env ~vars:prog.prog_vars prog Verif in
          fold_var_expr get_var fold_sp fold_var init verif_expr env
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
        let tpos = Pos.get verif.verif_id in
        let tname = Format.sprintf "%s_verif_%d" prog.prog_prefix id in
        let target_file = Some (get_target_file tpos) in
        let target_prog =
          let map_var m_v =
            let name = Com.get_normal_var (Pos.unmark m_v) in
            let id = StrMap.find name prog.prog_vars in
            Pos.same id m_v
          in
          List.map
            (Com.m_instr_map_var map_var Fun.id)
            [
              Pos.without
                (Com.IfThenElse
                   ( verif.verif_expr,
                     [
                       Pos.without
                         (Com.RaiseError (verif.verif_error, verif.verif_var));
                     ],
                     [] ));
            ]
        in
        let target =
          Com.
            {
              target_name = Pos.mark tname tpos;
              target_file;
              target_apps = StrMap.mapi Pos.mark prog.prog_app;
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
    | Attribut (Pos.Mark (VarAccess (_, m_v), _), m_attr) ->
        let var_name = Com.get_normal_var @@ Pos.unmark m_v in
        let id = StrMap.find var_name prog.prog_vars in
        let var = IntMap.find id prog.prog_dict in
        let attrs = Com.Var.attrs var in
        let m_val = StrMap.find (Pos.unmark m_attr) attrs in
        Some (float (Pos.unmark m_val))
    | Size (Pos.Mark (VarAccess (_, m_v), _)) ->
        let var_name = Com.get_normal_var @@ Pos.unmark m_v in
        let id = StrMap.find var_name prog.prog_vars in
        let var = IntMap.find id prog.prog_dict in
        Some (float @@ Com.Var.size @@ var)
    | IsVariable (Pos.Mark (VarAccess (_, m_v), _), m_name) -> (
        let var_name = Com.get_normal_var @@ Pos.unmark m_v in
        let id = StrMap.find var_name prog.prog_vars in
        let var = IntMap.find id prog.prog_dict in
        if Pos.unmark m_name = Com.Var.name_str var then Some 1.0
        else
          match Com.Var.alias var with
          | Some m_alias when Pos.unmark m_alias = Pos.unmark m_name -> Some 1.0
          | _ -> Some 0.0)
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
                  | Com.FloatValue (Pos.Mark (f, _)) -> res || f = v
                  | Com.IntervalValue (Pos.Mark (bn, _), Pos.Mark (en, _)) ->
                      res || (float bn <= v && v <= float en))
                false values
            in
            Some (if res = positive then 1.0 else 0.0))
    | NbAnomalies | NbDiscordances | NbInformatives | NbBloquantes
    | FuncCallLoop _ | Loop _ | Attribut _ | Size _ | IsVariable _ ->
        assert false
  in
  aux expr

let vdom_rule_filter (prog : program) (vdom : Com.verif_domain_data Com.domain)
    (expr : Mast.expression Pos.marked) (verif : verif) : bool =
  (match vdom.Com.dom_used with
  | Some (Pos.Mark (vdom_seq, seq_pos)) ->
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
  let prog_targets, prog_call_map, _ =
    StrMap.fold
      (fun tname (_, vdom_id, expr) (prog_targets, prog_call_map, verif_calls) ->
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
              [ Pos.without (Com.ComputeTarget (Pos.without tn, [], None)) ]
            in
            let target =
              Com.
                {
                  target_name = Pos.without tname;
                  target_file = None;
                  target_apps = StrMap.mapi Pos.mark prog.prog_app;
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
            let ccm = CallMap.one (CallTarget (tn, None)) Pos.none in
            let cc = CallTarget (tname, None) in
            let prog_call_map = CallMap.add cc (ccm, Pos.none) prog_call_map in
            (prog_targets, prog_call_map, verif_calls)
        | None ->
            let instrs, ccm =
              let instrs, ccm =
                OrdVerifSet.fold
                  (fun ord_verif (target_prog, ccm) ->
                    let verif_id = OrdVerif.get_id ord_verif in
                    let verif_tn =
                      Format.sprintf "%s_verif_%d" prog.prog_prefix verif_id
                    in
                    let instr =
                      Com.ComputeTarget (Pos.without verif_tn, [], None)
                    in
                    let target_prog = Pos.without instr :: target_prog in
                    let cc = CallTarget (verif_tn, None) in
                    let ccm = CallMap.add cc Pos.none ccm in
                    (target_prog, ccm))
                  verif_set ([], CallMap.empty)
              in
              (List.rev instrs, ccm)
            in
            let target_prog = [ Pos.without (Com.VerifBlock instrs) ] in
            let target =
              Com.
                {
                  target_name = Pos.without tname;
                  target_file = None;
                  target_apps = StrMap.mapi Pos.mark prog.prog_app;
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
            let cc = CallTarget (tname, None) in
            let prog_call_map = CallMap.add cc (ccm, Pos.none) prog_call_map in
            let verif_calls = OrdVerifSetMap.add verif_set tname verif_calls in
            (prog_targets, prog_call_map, verif_calls))
      prog.prog_vdom_calls
      (prog.prog_targets, prog.prog_call_map, OrdVerifSetMap.empty)
  in
  { prog with prog_targets; prog_call_map }

let check_called_spaces (prog : program) : program =
  (* let pp_call_trace fmt call_map =
       let iter cc (ccm, cc_pos) =
         Pp.fpr fmt "@;@[<v 2>call %a %a:" pp_call_compute cc Pos.format_short
           cc_pos;
         let iter' cc' cc_pos' =
           Pp.fpr fmt "@;%a %a" pp_call_compute cc' Pos.format_short cc_pos'
         in
         CallMap.iter iter' ccm;
         Pp.fpr fmt "@]@;"
       in
       CallMap.iter iter call_map
     in
     Pp.epr "%a@." pp_call_trace prog.prog_call_map; *)
  let get_cc_tname = function
    | CallDomain (tname, _, _)
    | CallVerifs (tname, _, _)
    | CallChaining (tname, _, _)
    | CallTarget (tname, _) ->
        tname
  in
  let get_sp_opt = function
    | CallDomain (_, _, sp_opt)
    | CallVerifs (_, _, sp_opt)
    | CallChaining (_, _, sp_opt)
    | CallTarget (_, sp_opt) ->
        sp_opt
  in
  let check_cc trace checked cc cc_pos =
    if CallMap.mem cc checked then checked
    else
      let tname = get_cc_tname cc in
      match StrMap.find_opt tname prog.prog_targets with
      | None -> CallMap.add cc () checked
      | Some target ->
          let sp_opt = get_sp_opt cc in
          let vs_name = match sp_opt with None -> "" | Some vsn -> vsn in
          let vsd_def =
            let vs_id = StrMap.find vs_name prog.prog_var_spaces in
            IntMap.find vs_id prog.prog_var_spaces_idx
          in
          let check_var usage m_sp_opt v_opt () =
            match (m_sp_opt, v_opt, usage) with
            | None, Some m_id, Com.(Read | Write | ArgRef) ->
                let v = IntMap.find (Pos.unmark m_id) prog.prog_dict in
                if Com.Var.is_tgv v then
                  (* Pp.epr "check_var <%s> %s@." vs_name (Com.Var.name_str v); *)
                  let loc = Com.Var.cat_var_loc v in
                  if not (Com.CatVar.LocMap.mem loc vsd_def.vs_cats) then (
                    Pp.epr "@.trace:@.";
                    List.iter
                      (fun (cc', cc_pos') ->
                        Pp.epr "  %a %a@." pp_call_compute cc' Pos.format_short
                          cc_pos')
                      ((cc, cc_pos) :: trace);
                    Err.variable_not_in_var_space (Com.Var.name_str v)
                      (Pos.unmark vsd_def.vs_name)
                      (Pos.get m_id))
            | _ -> ()
          in
          let iter m_i = Com.m_instr_fold_var check_var m_i () in
          List.iter iter target.target_prog;
          CallMap.add cc () checked
  in
  let cc_sp sp_opt = function
    | CallDomain (tn, dom, _) -> CallDomain (tn, dom, sp_opt)
    | CallVerifs (tn, dom, _) -> CallVerifs (tn, dom, sp_opt)
    | CallChaining (tn, name, _) -> CallChaining (tn, name, sp_opt)
    | CallTarget (name, _) -> CallTarget (name, sp_opt)
  in
  let rec foldCcm sp_opt cc pos (trace, checked) =
    let cc_sp_opt = get_sp_opt cc in
    let sp_opt' = match cc_sp_opt with None -> sp_opt | _ -> cc_sp_opt in
    let cc' = cc_sp sp_opt' cc in
    let checked = check_cc trace checked cc' pos in
    match CallMap.find_opt (cc_sp None cc) prog.prog_call_map with
    | Some (ccm, _) ->
        let trace' = (cc, pos) :: trace in
        let checked =
          snd @@ CallMap.fold (foldCcm sp_opt') ccm (trace', checked)
        in
        (trace, checked)
    | None -> (trace, checked)
  in
  let call_map = CallMap.map snd prog.prog_call_map in
  ignore @@ CallMap.fold (foldCcm None) call_map ([], CallMap.empty);
  prog

let proceed (main_target : string) (p : Mast.program) : program =
  (* à paramétrer *)
  let prog =
    List.fold_left
      (fun prog source_file ->
        List.fold_left
          (fun prog (Pos.Mark (item, pos_item)) ->
            match item with
            | Mast.Application (Pos.Mark (name, pos)) ->
                check_application name pos prog
            | Mast.Chaining (Pos.Mark (name, pos), m_apps) ->
                check_chaining name pos m_apps prog
            | Mast.VarCatDecl (Pos.Mark (decl, pos)) ->
                check_var_category decl pos prog
            | Mast.VariableDecl var_decl -> check_var_decl var_decl prog
            | Mast.VariableSpaceDecl vsd -> check_variable_space_decl vsd prog
            | Mast.EventDecl evt_decl -> check_event_decl evt_decl pos_item prog
            | Mast.Error error -> check_error error prog
            | Mast.Func -> prog (* unused *)
            | Mast.Output _ -> prog (* unused *)
            | Mast.RuleDomDecl decl -> check_rule_dom_decl decl prog
            | Mast.VerifDomDecl decl -> check_verif_dom_decl decl prog
            | Mast.Function f -> check_target Func f prog
            | Mast.Target t ->
                let cc = CallTarget (Pos.unmark t.target_name, None) in
                check_target (Target (cc, pos_item)) t prog
            | Mast.Rule r -> check_rule r prog
            | Mast.Verification v -> check_verif v prog)
          prog source_file)
      (empty_program p main_target)
      p
  in
  (match StrMap.find_opt "" prog.prog_var_spaces with
  | None -> Err.no_default_variable_space ()
  | Some _ -> ());
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
  |> complete_verif_calls |> check_called_spaces
