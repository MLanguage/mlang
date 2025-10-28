(*
Copyright (C) 2019-2021 Inria, contributor:
    Denis Merigoux <denis.merigoux@inria.fr>
    Raphaël Monat <raphael.monat@lip6.fr>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

%{
 open Mast
 open Parse_utils

 type comp_subtyp_or_attr =
 | CompSubTyp of string Pos.marked
 | Attr of variable_attribute

 (** Module generated automaticcaly by Menhir, the parser generator *)
%}

%token<string> SYMBOL STRING

%token PLUS MINUS TIMES DIV MOD
%token GTE LTE GT LT NEQ EQUALS
%token SEMICOLON COLON COMMA
%token AND OR NOT UNDEFINED

%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token RANGE DOT

%token BOOLEAN DATE_YEAR DATE_DAY_MONTH_YEAR DATE_MONTH INTEGER REAL
%token ONE IN APPLICATION CHAINING TYPE TABLE
%token COMPUTED CONST ALIAS INPUT FOR
%token RULE VERIFICATION TARGET INPUT_ARGS TEMP_VARS SIZE RESULT
%token IF THEN ELSEIF ELSE ENDIF PRINT PRINT_ERR NAME INDENT
%token WHEN DO THEN_WHEN ELSE_DO ENDWHEN NOTHING
%token COMPUTE VERIFY WITH VERIF_NUMBER COMPL_NUMBER NB_CATEGORY
%token NB_ANOMALIES NB_DISCORDANCES NB_INFORMATIVES NB_BLOCKING
%token RAISE_ERROR EXPORT_ERRORS CLEAN_ERRORS FINALIZE_ERRORS
%token ITERATE CATEGORY RESTORE AFTER BETWEEN
%token ERROR ANOMALY DISCORDANCE
%token INFORMATIVE OUTPUT FONCTION VARIABLE ATTRIBUT
%token BASE GIVEN_BACK COMPUTABLE BY_DEFAULT
%token DOMAIN SPECIALIZE AUTHORIZE VERIFIABLE EVENT EVENTS VALUE STEP
%token EVENT_FIELD ARRANGE_EVENTS SORT FILTER ADD REFERENCE
%token SAME_VARIABLE VARIABLE_SPACE SPACE IN_DOMAIN CLEAN_FINALIZED_ERRORS
%token STOP MATCH CASE DEFAULT

%token EOF

%type<Mast.source_file> source_file
%type<Mast.m_instruction list> instruction_list_rev

%nonassoc SEMICOLON
%left OR
%left AND
%nonassoc NOT
(* %nonassoc SYMBOL *)

%start source_file

%%

%inline with_pos(X):
| x = X { Pos.mark x (mk_position $sloc) }

symbol_with_pos:
| s = with_pos(SYMBOL) { s }

symbol_list_with_pos:
| sl = with_pos(symbol_with_pos+) { sl }

variable_name:
| s = SYMBOL { parse_variable_name $sloc s }

source_file:
| vl = with_pos(symbol_colon_etc)* is = source_file_rev EOF {
    List.flatten (vl :: List.rev is)
  }

symbol_colon_etc:
| v = variable_decl { v }
| e = error_ { e }
| fonction { Func }

source_file_rev:
| is = source_file_rev i = source_file_item { i :: is }
| { [] }

source_file_item:
| al = application_etc { al }
| cl = chaining_etc { cl }
| cl = var_category_decl_etc { cl }
| el = event_decl_etc { el }
| crl = rule_domain_decl_etc { crl }
| cvl = verif_domain_decl_etc { cvl }
| vsl = variable_space_decl_etc { vsl }
| ol = output_etc { ol }
| rl = rule_etc { rl }
| vl = verification_etc { vl }
| tl = target_etc { tl }
| fl = function_etc { fl }

var_typ:
| INPUT { Input }
| COMPUTED { Computed }

var_category_decl_etc:
| c = with_pos(var_category_decl) l = with_pos(symbol_colon_etc)* {
    Pos.same (VarCatDecl c) c :: l
  }

var_category_decl:
| VARIABLE var_type = var_typ var_category = symbol_with_pos* COLON
  ATTRIBUT var_attributes = separated_nonempty_list(COMMA, symbol_with_pos)
  SEMICOLON {
    { var_type; var_category; var_attributes }
  }

event_decl_etc:
| e = with_pos(event_decl) l = with_pos(symbol_colon_etc)* {
    Pos.same (EventDecl (Pos.unmark e)) e :: l
  }

event_field:
| VARIABLE name = symbol_with_pos { Com.{name; is_var = true; index = 0} }
| VALUE name = symbol_with_pos { Com.{name; is_var = false; index = 0} }

event_decl:
| EVENT COLON el = separated_nonempty_list(COLON, event_field) SEMICOLON { el }

rule_domain_decl_etc:
| cr =with_pos(rule_domain_decl) l = with_pos(symbol_colon_etc)* { cr :: l }

rule_domain_decl:
| DOMAIN RULE rdom_params = separated_nonempty_list(COLON, with_pos(rdom_param))
  SEMICOLON {
    let err msg pos = Errors.raise_spanned_error msg pos in
    let fold (dno, dso, dco, dpdo) = function
    | Pos.Mark ((Some dn, _, _, _), pos) ->
        if dno = None then Some dn, dso, dco, dpdo
        else err "rule domain names are already defined" pos
    | Pos.Mark ((_, Some ds, _, _), pos) ->
        if dso = None then dno, Some ds, dco, dpdo
        else err "rule domain specialization is already specified" pos
    | Pos.Mark ((_, _, Some dc, _), pos) ->
        if dco = None then dno, dso, Some dc, dpdo
        else err "rule domain is already calculated" pos
    | Pos.Mark ((_, _, _, Some dpd), pos) ->
        if dpdo = None then dno, dso, dco, Some dpd
        else err "rule domain is already defined by defaut" pos
    | Pos.Mark ((_, _, _, _), _) -> assert false
    in
    let init = None, None, None, None in
    let dno, dso, dco, dpdo = List.fold_left fold init rdom_params in
    let dom_names =
      match dno with
      | None -> err "rule domain names must be defined" (mk_position $sloc)
      | Some dn -> dn
    in
    let decl = {
      dom_names;
      dom_parents = (match dso with None -> [] | Some ds -> ds);
      dom_by_default = (match dpdo with None -> false | _ -> true);
      dom_data = {rdom_computable = (match dco with None -> false | _ -> true)};
    } in
    RuleDomDecl decl
  }

rdom_param:
| rdom_names = separated_nonempty_list(COMMA, symbol_list_with_pos)
  { (Some rdom_names, None, None, None) }
| SPECIALIZE rdom_parents = separated_nonempty_list(COMMA, symbol_list_with_pos)
  { (None, Some rdom_parents, None, None) }
| COMPUTABLE
  { (None, None, Some (), None) }
| BY_DEFAULT
  { (None, None, None, Some ()) }

verif_domain_decl_etc:
| cv = with_pos(verif_domain_decl) l = with_pos(symbol_colon_etc)* { cv :: l }

verif_domain_decl:
| DOMAIN VERIFICATION vdom_params = separated_nonempty_list(COLON, with_pos(vdom_param))
  SEMICOLON {
    let err msg pos = Errors.raise_spanned_error msg pos in
    let fold (dno, dso, dvo, dpdo, dco) = function
    | Pos.Mark ((Some dn, _, _, _, _), pos) ->
        if dno = None then Some dn, dso, dvo, dpdo, dco
        else err "verif domain names are already defined" pos
    | Pos.Mark ((_, Some ds, _, _, _), pos) ->
        if dso = None then dno, Some ds, dvo, dpdo, dco
        else err "verif domain specialization is already specified" pos
    | Pos.Mark ((_, _, Some dv, _, _), pos) ->
        if dvo = None then dno, dso, Some dv, dpdo, dco
        else err "verif domain authorization is already specified" pos
    | Pos.Mark ((_, _, _, Some dpd, _), pos) ->
        if dpdo = None then dno, dso, dvo, Some dpd, dco
        else err "verif domain is already defined by defaut" pos
    | Pos.Mark ((_, _, _, _, Some dcd), pos) ->
        if dco = None then dno, dso, dvo, dpdo, Some dcd
        else err "verif domain is already verifiable" pos
    | Pos.Mark ((_, _, _, _, _), _) -> assert false
    in
    let init = None, None, None, None, None in
    let dno, dso, dvo, dpdo, dco = List.fold_left fold init vdom_params in
    let dom_names =
      match dno with
      | None -> err "rule domain names must be defined" (mk_position $sloc)
      | Some dn -> dn
    in
    let dom_data = {
      vdom_auth = (match dvo with None -> [] | Some dv -> dv);
      vdom_verifiable = (match dco with None -> false | _ -> true); 
    } in
    let decl = {
      dom_names;
      dom_parents = (match dso with None -> [] | Some ds -> ds);
      dom_by_default = (match dpdo with None -> false | _ -> true);
      dom_data;
    } in
    VerifDomDecl decl
  }

var_category_id:
| INPUT TIMES { [Pos.without "saisie"; Pos.without "*"] }
| INPUT l = symbol_with_pos+ { (Pos.without "saisie") :: l }
| COMPUTED TIMES { [Pos.without "calculee"; Pos.without "*"] }
| COMPUTED BASE { [Pos.without "calculee"; Pos.without "*"] }
| COMPUTED { [Pos.without "calculee"] }
| TIMES { [Pos.without "*"] }

vdom_param:
| vdom_names = separated_nonempty_list(COMMA, symbol_list_with_pos)
  { (Some vdom_names, None, None, None, None) }
| SPECIALIZE vdom_parents = separated_nonempty_list(COMMA, symbol_list_with_pos)
  { (None, Some vdom_parents, None, None, None) }
| AUTHORIZE vcats = separated_nonempty_list(COMMA, with_pos(var_category_id))
  { (None, None, Some vcats, None, None) }
| BY_DEFAULT
  { (None, None, None, Some (), None) }
| VERIFIABLE
  { (None, None, None, None, Some ()) }

fonction:
| SYMBOL COLON FONCTION SYMBOL SEMICOLON { () }

application_etc:
| a = with_pos(application) l = with_pos(symbol_colon_etc)* { a :: l }

application:
| APPLICATION s = with_pos(SYMBOL) SEMICOLON { Application s }

application_reference:
| APPLICATION COLON ss = symbol_enumeration { ss }

chaining_etc:
| c = with_pos(chaining) l = with_pos(symbol_colon_etc)* { c :: l }

chaining:
| CHAINING s = symbol_with_pos aps = application_reference SEMICOLON {
    Chaining (s, aps)
  }

variable_decl:
| v = with_pos(comp_variable) { VariableDecl (ComputedVar v) }
| cv = const_variable { let n, v = cv in VariableDecl (ConstVar (n, v)) }
| v = with_pos(input_variable) { VariableDecl (InputVar v) }

const_variable_name:
| name = SYMBOL COLON CONST { parse_variable_name $sloc name }

const_value:
| value = SYMBOL { parse_atom $sloc value }

const_variable:
| name = with_pos(const_variable_name) EQUALS value = with_pos(const_value)
  SEMICOLON { (name, value) }

comp_variable_name:
| name = SYMBOL COLON { parse_variable_name $sloc name }

comp_variable_table:
| TABLE LBRACKET size = SYMBOL RBRACKET { parse_table_size size }

comp_variable_descr:
| descr = STRING { parse_string descr }

comp_attr:
| BASE { "base" }
| GIVEN_BACK { "restituee" }

comp_attr_or_subtyp:
| attr = variable_attribute { let (x, y) = attr in Attr (x, y) }
| attr = with_pos(comp_attr) { CompSubTyp attr }

comp_variable:
| comp_name = with_pos(comp_variable_name) comp_table = with_pos(comp_variable_table)?
  COMPUTED subtyp = comp_attr_or_subtyp* COLON
  comp_description = with_pos(comp_variable_descr) comp_typ = value_type?
  SEMICOLON {
    let comp_attributes =
      subtyp
      |> List.filter (function Attr _ -> true | _ -> false)
      |> List.map (function Attr (x, y) -> (x, y) | _ -> assert false)
    in
    let comp_category =
      subtyp
      |> List.filter (function CompSubTyp (Pos.Mark ("base", _)) -> true | _ -> false) 
      |> List.map (function CompSubTyp x -> x | _ -> assert false)
    in
    let comp_is_givenback =
      subtyp
      |> List.exists (function CompSubTyp (Pos.Mark ("restituee", _)) -> true | _ -> false)
    in
    {
      comp_name;
      comp_table;
      comp_attributes;
      comp_category;
      comp_is_givenback;
      comp_description;
      comp_typ;
    }
  }

input_variable_name:
| name = SYMBOL COLON { parse_variable_name $sloc name }

input_descr:
descr = STRING { parse_string descr }

input_attr_or_category:
| attr = variable_attribute { (None, Some attr, false) }
| cat = symbol_with_pos { (Some cat, None, false) }
| GIVEN_BACK { None, None, true }

input_variable_alias:
| ALIAS alias = SYMBOL { parse_variable_name $sloc alias }

variable_attribute_value:
| value = SYMBOL { parse_int $sloc value }

variable_attribute:
| attr = symbol_with_pos EQUALS
  lit = with_pos(variable_attribute_value)
{ (attr, lit) }

value_type_prim:
| BOOLEAN { Com.Boolean }
| DATE_YEAR { Com.DateYear }
| DATE_DAY_MONTH_YEAR { Com.DateDayMonthYear }
| DATE_MONTH { Com.DateMonth }
| INTEGER { Com.Integer }
| REAL { Com.Real }

value_type:
| TYPE typ = with_pos(value_type_prim) { typ }

input_variable:
| name = with_pos(input_variable_name) INPUT
  category_attrs = input_attr_or_category* alias = with_pos(input_variable_alias)
  COLON descr = with_pos(input_descr) typ = value_type?
  SEMICOLON {
    let (category, attrs, givenback) =
      List.fold_left
        (fun (category, attrs, givenback) (c, a, r) -> 
          match c, a, r with
          | Some x, _, _ -> x :: category, attrs, givenback
          | _, Some x, _ -> category, x :: attrs, givenback
          | _, _, true -> category, attrs, true
          | _, _, _ -> category, attrs, givenback)
        ([], [], false)
        category_attrs
    in
    {
      input_name = name;
      input_category = category;
      input_attributes = attrs;
      input_alias = alias;
      input_is_givenback = givenback;
      input_typ = typ;
      input_description = descr;
    }
  }

variable_space_decl_etc:
| vs = with_pos(variable_space_decl) l = with_pos(symbol_colon_etc)* { vs :: l }

variable_space_decl:
| VARIABLE_SPACE m_name = symbol_with_pos COLON
  vs_params = separated_nonempty_list(COLON, with_pos(vs_param)) SEMICOLON {
    let err msg pos = Errors.raise_spanned_error msg pos in
    let fold (co, pdo) = function
    | Pos.Mark ((Some cats, _), pos) -> (
        match co, pdo with
        | _, Some _ ->
            err "variable space categories are implicit with by_default" pos
        | Some _, _ -> err "variable space categories are already specified" pos
        | None, _  -> Some cats, pdo
      )
    | Pos.Mark ((_, Some ()), pos) -> (
        match co, pdo with
        | Some _, _ ->
            err "variable space categories are implicit with by_default" pos
        | _, Some _ -> err "by_default is already specified" pos
        | _, None  -> co, Some ()
      )
    | Pos.Mark ((_, _), _) -> assert false
    in
    let init = None, None in
    let co, pdo = List.fold_left fold init vs_params in
    let decl = Com.{
      vs_id = -1;
      vs_name = m_name;
      vs_cats = (
        match co with
        | None ->
            Com.CatVar.(
              LocMap.empty
              |> LocMap.add LocInput (Pos.without LocInput)
              |> LocMap.add LocComputed (Pos.without LocComputed)
              |> LocMap.add LocBase (Pos.without LocBase)
            )
        | Some cats -> cats
      );
      vs_by_default = (match pdo with None -> false | _ -> true);
    } in
    VariableSpaceDecl decl
  }

vs_cat:
| INPUT { Com.CatVar.LocInput }
| COMPUTED { Com.CatVar.LocComputed }
| BASE { Com.CatVar.LocBase }

vs_param:
| CATEGORY vs_cats = separated_nonempty_list(COMMA, with_pos(vs_cat)) {
    let rec check map = function
    | (Pos.Mark (lct, pos) as m_lct) :: l ->
        if Com.CatVar.LocMap.mem lct map then (
          let msg =
            Pp.spr "category \"%a\" already specified" Com.CatVar.pp_loc lct
          in
          Errors.raise_spanned_error msg pos
        );
        check (Com.CatVar.LocMap.add lct m_lct map) l
    | [] -> map
    in
    let cats = check Com.CatVar.LocMap.empty vs_cats in
    Some cats, None
  }
| BY_DEFAULT { None, Some () }

rule_etc:
| RULE name = symbol_list_with_pos COLON
  header = nonempty_list(with_pos(rule_header_elt))
  formulaes_etc = instruction_list_etc
  {
    let num, rule_tag_names =
      let uname = Pos.unmark name in
      let begPos =
        match uname with
        | h :: _ -> Pos.get h
        | [] -> assert false
      in
      let rec aux tags endPos = function
      | [num] ->
           let pos = Pos.make_between begPos endPos in
           num, (Pos.mark tags pos)
      | h :: t -> aux (h :: tags) (Pos.get h) t
      | [] -> assert false
      in
      aux [] begPos uname
    in
    let rule_number =
      try Pos.map int_of_string num
      with _ ->
        Errors.raise_spanned_error
          "this rule doesn't have an execution number"
          (Pos.get num)
    in
    let rule_apps, rule_chainings, rule_tmp_vars =
      let rec aux apps_opt chs_opt vars_opt = function
      | Pos.Mark (`Applications apps', pos) :: h ->
          let apps_opt' =
            match apps_opt with
            | None -> Some (apps', pos)
            | Some (_, old_pos) ->
                Errors.raise_spanned_error
                  (Format.asprintf
                    "application list already declared %a"
                    Pos.format old_pos)
                  pos
          in
          aux apps_opt' chs_opt vars_opt h
      | Pos.Mark (`Chainings chs', pos) :: h ->
          let chs_opt' =
            match chs_opt with
            | None -> Some (chs', pos)
            | Some (_, old_pos) ->
                Errors.raise_spanned_error
                  (Format.asprintf
                    "chaining list already declared %a"
                    Pos.format old_pos)
                  pos
          in
          aux apps_opt chs_opt' vars_opt h
      | Pos.Mark (`TmpVars vars', pos) :: h ->
          let vars_opt' =
            match vars_opt with
            | None -> Some (vars', pos)
            | Some (_, old_pos) ->
                Errors.raise_spanned_error
                  (Format.asprintf
                    "temporary variables already declared %a"
                    Pos.format old_pos)
                  pos
          in
          aux apps_opt chs_opt vars_opt' h
      | [] ->
          let apps =
            match apps_opt with
            | Some (apps, _) ->
                List.fold_left
                  (fun res (Pos.Mark (app, pos)) ->
                    match StrMap.find_opt app res with
                    | Some (Pos.Mark (_, old_pos)) ->
                        let msg =
                          Format.asprintf "application %s already declared %a"
                            app
                            Pos.format old_pos
                        in
                        Errors.raise_spanned_error msg pos
                    | None -> StrMap.add app (Pos.mark app pos) res)
                  StrMap.empty
                  apps
            | None ->
                Errors.raise_spanned_error
                "this rule doesn't belong to an application"
                (Pos.get num)
          in
          let chs =
            match chs_opt with
            | Some (chs, _) ->
                List.fold_left
                  (fun res (Pos.Mark (ch, pos)) ->
                    match StrMap.find_opt ch res with
                    | Some (Pos.Mark (_, old_pos)) ->
                        let msg =
                          Format.asprintf "chaining %s already declared %a"
                            ch
                            Pos.format old_pos
                        in
                        Errors.raise_spanned_error msg pos
                    | None -> StrMap.add ch (Pos.mark ch pos) res)
                  StrMap.empty
                  chs
            | None -> StrMap.empty
          in
          let vars = match vars_opt with None -> [] | Some (l, _) -> l in
          apps, chs, vars
      in
      aux None None None header
    in
    let rule_formulaes, l = formulaes_etc in 
    let rule = {
      rule_number;
      rule_tag_names;
      rule_apps;
      rule_chainings;
      rule_tmp_vars;
      rule_formulaes;
    } in
    Pos.same (Rule rule) name :: l
  }

rule_header_elt:
| APPLICATION COLON apps = symbol_enumeration SEMICOLON { `Applications apps }
| CHAINING COLON chs = symbol_enumeration SEMICOLON { `Chainings chs }
| TEMP_VARS COLON
  tmp_vars = separated_nonempty_list(COMMA, temporary_variable_name) SEMICOLON
  { `TmpVars tmp_vars }

target_etc:
| TARGET name = symbol_with_pos COLON
  header = nonempty_list(with_pos(target_header_elt))
  prog_etc = instruction_list_etc
  {
    let target_prog, l = prog_etc in
    let target_apps, target_args, target_tmp_vars, _ =
      parse_target_or_function_header name false header
    in
    let target = {
      target_name = name;
      target_file = None;
      target_apps;
      target_args;
      target_result = None;
      target_tmp_vars;
      target_prog;
    } in
    Pos.same (Target target) name :: l
  }

target_header_elt:
| APPLICATION COLON apps = symbol_enumeration SEMICOLON { Target_apps apps }
| INPUT_ARGS COLON
  inputs = separated_nonempty_list(COMMA, with_pos(variable_name)) SEMICOLON {
    Target_input_arg inputs
  }
| TEMP_VARS COLON
  tmp_vars = separated_nonempty_list(COMMA, temporary_variable_name) SEMICOLON {
    Target_tmp_vars tmp_vars
  }

function_etc:
| FONCTION name = symbol_with_pos COLON
  header = nonempty_list(with_pos(function_header_elt))
  prog_etc = instruction_list_etc
  {
    let target_prog, l = prog_etc in
    let target_apps, target_args, target_tmp_vars, target_result =
      parse_target_or_function_header name true header
    in
    let target = {
      target_name = name;
      target_file = None;
      target_apps;
      target_args;
      target_result;
      target_tmp_vars;
      target_prog;
    } in
    Pos.same (Function target) name :: l
  }

function_header_elt:
| APPLICATION COLON apps = symbol_enumeration SEMICOLON { Target_apps apps }
| INPUT_ARGS COLON
  inputs = separated_nonempty_list(COMMA, with_pos(variable_name)) SEMICOLON {
    Target_input_arg inputs
  }
| TEMP_VARS COLON
  tmp_vars = separated_nonempty_list(COMMA, temporary_variable_name) SEMICOLON {
    Target_tmp_vars tmp_vars
  }
| RESULT COLON res = with_pos(variable_name) SEMICOLON {
    Function_result res
  }

temporary_variable_name:
| name = symbol_with_pos size = with_pos(comp_variable_table)? {
    let name_str, name_pos = Pos.to_couple name in
    (Pos.mark (parse_variable_name $sloc name_str) name_pos), size
  }

compute_space:
| COLON SPACE sp = symbol_with_pos {
    Pos.same (parse_variable $sloc (Pos.unmark sp)) sp
  }

instruction_list_etc:
| i_opt = with_pos(instruction) l = with_pos(symbol_colon_etc)* {
    match Pos.unmark i_opt with
    | None -> [], l
    | Some i -> [Pos.same i i_opt], l
  }
| i_opt = with_pos(instruction) il_etc = instruction_list_etc {
    match Pos.unmark i_opt with
    | None -> il_etc
    | Some i ->
        let il, l = il_etc in
        (Pos.same i i_opt) :: il, l
  }

instruction_list_rev:
| i_opt = with_pos(instruction) {
    match Pos.unmark i_opt with
    | None -> []
    | Some i -> [Pos.same i i_opt]
  }
| il = instruction_list_rev i_opt = with_pos(instruction) {
    match Pos.unmark i_opt with
    | None -> il
    | Some i -> (Pos.same i i_opt) :: il  
  }

instruction:
| NOTHING SEMICOLON { None }
| f = with_pos(formula_kind) SEMICOLON { Some (Affectation f) }
| IF e = with_pos(expression)
  THEN ilt = instruction_list_rev
  ilel = instruction_else_branch {
    let ilite = (Some e, List.rev ilt, mk_position $sloc) :: ilel in
    Some (parse_if_then_etc ilite)
  }
| WHEN e = with_pos(expression)
  DO ild = instruction_list_rev
  iltwe = instruction_then_when_branch {
    let iltwl, ed = iltwe in
    Some (parse_when_do_etc ((e, List.rev ild, mk_position $sloc) :: iltwl, ed))
  }
| COMPUTE DOMAIN dom = symbol_list_with_pos sp_opt = compute_space? SEMICOLON {
    match sp_opt with
    | None -> Some (ComputeDomain (dom, None))
    | Some m_sp -> Some (ComputeDomain (dom, Some (m_sp, -1)))
  }
| COMPUTE CHAINING chain = symbol_with_pos sp_opt = compute_space? SEMICOLON {
    match sp_opt with
    | None -> Some (ComputeChaining (chain, None))
    | Some m_sp -> Some (ComputeChaining (chain, Some (m_sp, -1)))
  }
| COMPUTE TARGET
  target = symbol_with_pos
  params = list(with_pos(target_param))
  SEMICOLON {
    let err msg pos = Errors.raise_spanned_error msg pos in
    let fold (spo, alo) = function
    | Pos.Mark ((Some sp, _), pos) ->
        if spo = None then Some sp, alo
        else err "variable space is already defined" pos
    | Pos.Mark ((_, Some al), pos) ->
        if alo = None then spo, Some al
        else err "arguments are already specified" pos
    | Pos.Mark ((_, _), _) -> assert false
    in
    let init = None, None in
    let spo, alo = List.fold_left fold init params in
    let m_sp_opt = match spo with Some m_sp -> Some (m_sp, -1) | None -> None in
    let args =
      match alo with
      | Some al -> al
      | None -> []
    in
    Some (ComputeTarget (target, args, m_sp_opt))
  }
| VERIFY DOMAIN
  dom = symbol_list_with_pos
  params = list(with_pos(verify_param))
  SEMICOLON {
    let err msg pos = Errors.raise_spanned_error msg pos in
    let fold (spo, eo) = function
    | Pos.Mark ((Some sp, _), pos) ->
        if spo = None then Some sp, eo
        else err "variable space is already defined" pos
    | Pos.Mark ((_, Some e), pos) ->
        if eo = None then spo, Some e
        else err "filter is already specified" pos
    | Pos.Mark ((_, _), _) -> assert false
    in
    let init = None, None in
    let spo, eo = List.fold_left fold init params in
    let m_sp_opt = match spo with Some m_sp -> Some (m_sp, -1) | None -> None in
    let expr =
      match eo with
      | Some expr -> expr
      | None -> Pos.without (Com.Literal (Com.Float 1.0))
    in
    Some (ComputeVerifs (dom, expr, m_sp_opt))
  }
| PRINT args = with_pos(print_argument)* SEMICOLON {
    Some (Print (StdOut, args))
  }
| PRINT_ERR args = with_pos(print_argument)* SEMICOLON {
    Some (Print (StdErr, args))
  }
| ITERATE COLON
  VARIABLE vn = symbol_with_pos COLON
  it_params = nonempty_list(with_pos(it_param))
  IN LPAREN instrs = instruction_list_rev RPAREN {
    let var = Pos.same (Com.Normal (Pos.unmark vn)) vn in
    match it_params with
    | Pos.Mark (`VarInterval _, _) :: _ ->
        let var_intervals =
          let fold var_intervals = function
          | Pos.Mark (`VarInterval (e0, e1, step), _) -> (e0, e1, step) :: var_intervals
          | Pos.Mark (`VarList _, pos) | Pos.Mark (`VarCatsIt _, pos) ->
              Errors.raise_spanned_error "variable descriptors forbidden in values iteration" pos
          in
          List.fold_left fold [] it_params
        in
        Some (Iterate_values (var, List.rev var_intervals, List.rev instrs))
    | _ ->
        let var_list, var_cats =
          let fold (var_list, var_cats) = function
          | Pos.Mark (`VarList al, _) -> (List.rev al) @ var_list, var_cats
          | Pos.Mark (`VarCatsIt vc, _) -> var_list, vc :: var_cats
          | Pos.Mark (`VarInterval _, pos) ->
              Errors.raise_spanned_error "interval forbidden in variable iteration" pos
          in
          List.fold_left fold ([], []) it_params
        in
        Some (Iterate (var, List.rev var_list, List.rev var_cats, List.rev instrs))
  }
| RESTORE COLON rest_params = nonempty_list(rest_param)
  AFTER LPAREN instrs = instruction_list_rev RPAREN {
    let a_list, var_cats, event_list, event_filter =
      let fold (a_list, var_cats, event_list, event_filter) = function
      | `AccessList al -> (List.rev al) @ a_list, var_cats, event_list, event_filter
      | `VarCatsRest vc -> a_list, vc :: var_cats, event_list, event_filter
      | `EventList el -> a_list, var_cats, el @ event_list, event_filter
      | `EventFilter ef -> a_list, var_cats, event_list, ef :: event_filter
      in
      List.fold_left fold ([], [], [], []) rest_params
    in
    Some (
      Restore (
        List.rev a_list,
        List.rev var_cats,
        List.rev event_list,
        List.rev event_filter,
        List.rev instrs
      )
    )
  }
| ARRANGE_EVENTS COLON
  arr_params = nonempty_list(with_pos(arrange_events_param))
  IN LPAREN instrs = instruction_list_rev RPAREN {
    let sort, filter, add =
      let fold (sort, sort_pos, filter, filter_pos, add, add_pos) = function
      | Pos.Mark (`ArrangeEventsSort (v0, v1, e), pos) when sort = None ->
          (Some (v0, v1, e), pos, filter, filter_pos, add, add_pos)
      | Pos.Mark (`ArrangeEventsFilter (v, e), pos) when filter = None ->
          (sort, sort_pos, Some (v, e), pos, add, add_pos)
      | Pos.Mark (`ArrangeEventsAdd e, pos) when add = None ->
          (sort, sort_pos, filter, filter_pos, Some e, pos)
      | Pos.Mark (`ArrangeEventsSort _, pos) ->
          let msg =
            Format.asprintf
              "event sorting already specified at %a"
              Pos.format sort_pos
          in
          Errors.raise_spanned_error msg pos
      | Pos.Mark (`ArrangeEventsFilter _, pos) ->
          let msg =
            Format.asprintf
              "event filter already specified at %a"
              Pos.format sort_pos
          in
          Errors.raise_spanned_error msg pos
      | Pos.Mark (`ArrangeEventsAdd _, pos) ->
          let msg =
            Format.asprintf
              "event creation already specified at %a"
              Pos.format add_pos
          in
          Errors.raise_spanned_error msg pos
      in
      let sort, _, filter, _, add, _ =
        List.fold_left fold
          (None, Pos.none, None, Pos.none, None, Pos.none)
          arr_params
      in
      match sort, filter, add with
      | None, None, None ->
          let msg = "event organizer needs a sort, a filter or a creation specification" in
          Errors.raise_spanned_error msg (mk_position $sloc)
      | _, _, _ -> sort, filter, add
    in
    Some (ArrangeEvents (sort, filter, add, List.rev instrs))
  }
| RAISE_ERROR e_name = symbol_with_pos var = with_pos(variable_name)? SEMICOLON {
    Some (RaiseError (e_name, var))
  }
| CLEAN_ERRORS SEMICOLON { Some CleanErrors }
| CLEAN_FINALIZED_ERRORS SEMICOLON { Some CleanFinalizedErrors }
| EXPORT_ERRORS SEMICOLON { Some ExportErrors }
| FINALIZE_ERRORS SEMICOLON { Some FinalizeErrors }
| STOP APPLICATION SEMICOLON { Some (Stop SKApplication) }
| STOP FONCTION SEMICOLON { Some (Stop SKFun) }
| STOP TARGET SEMICOLON { Some (Stop SKTarget) } 
| STOP s = SYMBOL SEMICOLON { Some (Stop (SKId (Some s))) }
| STOP SEMICOLON { Some (Stop (SKId None)) }
| MATCH LPAREN e = with_pos(expression) RPAREN COLON LPAREN l = nonempty_list(switch_case) RPAREN SEMICOLON
  { Some (Switch (e, l)) }

switch_case_value:
| CASE s = SYMBOL { Some (Com.Float (float_of_string s)) }
| CASE UNDEFINED { Some Com.Undefined }
| DEFAULT { None }

switch_case:
  | c = switch_case_value COLON LPAREN ilt = instruction_list_rev RPAREN
    { c, List.rev ilt }

target_param:
| COLON SPACE sp = symbol_with_pos {
    let m_sp = Pos.same (parse_variable $sloc (Pos.unmark sp)) sp in
    Some m_sp, None
  }
| COLON WITH args = separated_nonempty_list(COMMA, with_pos(var_access)) {
    None, Some args
  }

verify_param:
| COLON SPACE sp = symbol_with_pos {
    let m_sp = Pos.same (parse_variable $sloc (Pos.unmark sp)) sp in
    Some m_sp, None
  }
| COLON WITH expr = with_pos(expression) {
  None, Some expr
}

instruction_else_branch:
| ELSEIF e = with_pos(expression)
  THEN ilt = instruction_list_rev
  ilel = instruction_else_branch {
    (Some e, List.rev ilt, mk_position $sloc) :: ilel
  }
| ELSE il = instruction_list_rev ENDIF {
    [None, List.rev il, mk_position $sloc]
  }
| ENDIF { [] }

instruction_then_when_branch:
| THEN_WHEN e = with_pos(expression)
  DO ild = instruction_list_rev
  iltwe = instruction_then_when_branch {
    let iltwl, ed = iltwe in
    ((e, List.rev ild, mk_position $sloc) :: iltwl, ed)
  }
| ELSE_DO il = instruction_list_rev ENDWHEN {
    ([], (Pos.mark (List.rev il) (mk_position $sloc)))
  }
| ENDWHEN { ([], (Pos.without [])) }

print_argument:
| s = STRING { Com.PrintString (parse_string s) }
| f = with_pos(print_function) LPAREN m_a = with_pos(var_access) RPAREN {
    match Pos.unmark f with
    | "nom" -> Com.PrintAccess (Com.Name, m_a)
    | "alias" -> Com.PrintAccess (Com.Alias, m_a)
    | _ -> assert false
  }
| INDENT LPAREN e = with_pos(expression) RPAREN { Com.PrintIndent e }
| LPAREN e = with_pos(expression) RPAREN prec = print_precision? {
    match prec with
    | Some (min, max) -> Com.PrintExpr (e, min, max)
    | None -> Com.PrintExpr (e, 0, 20)
  }

print_function:
| NAME { "nom" }
| ALIAS { "alias" }

print_precision:
| COLON min = symbol_with_pos
    {
      let min_str, min_pos = Pos.to_couple min in
      let min_val =
        try int_of_string min_str with
        | Failure _ -> Errors.raise_spanned_error "should be an integer" min_pos
      in
      (if min_val < 0 then
        Errors.raise_spanned_error "precision must be positive" min_pos);
      (min_val, min_val)
    }
| COLON min = symbol_with_pos RANGE max = symbol_with_pos
    {
      let min_str, min_pos = Pos.to_couple min in
      let min_val =
        try int_of_string min_str with
        | Failure _ -> Errors.raise_spanned_error "should be an integer" min_pos
      in
      (if min_val < 0 then
        Errors.raise_spanned_error "precision must be positive" min_pos);
      let max_str, max_pos = Pos.to_couple max in
      let max_val =
        try int_of_string max_str with
        | Failure _ -> Errors.raise_spanned_error "should be an integer" max_pos
      in
      (if max_val < 0 then
        Errors.raise_spanned_error "precision must be positive" max_pos);
      (if max_val < min_val then
        Errors.raise_spanned_error
          "maximum precision must be smaller than minimum precision"
          max_pos);
      (min_val, max_val)
    }

it_param:
| al = separated_nonempty_list(COMMA, with_pos(var_access)) COLON {
    `VarList al
  }
| CATEGORY vcat_list = separated_nonempty_list(COMMA, with_pos(var_category_id))
  COLON params = list(with_pos(it_param_category)) {
    let vcats =
      let fold res vc =
        let vcm = Com.CatVar.Map.from_string_list vc in
        Com.CatVar.Map.union (fun _ p _ -> Some p) vcm res
      in
      List.fold_left fold Com.CatVar.Map.empty vcat_list
    in
    let err msg pos = Errors.raise_spanned_error msg pos in
    let fold (eo, spo) = function
    | Pos.Mark (`Expr m_e, pos) -> (
        match eo with
        | None -> Some m_e, spo
        | _ -> err "expression is already specified" pos
      )
    | Pos.Mark (`Space m_sp, pos) -> (
        match spo with
        | None -> eo, Some m_sp
        | _ -> err "variable space is already specified" pos
      )
    in
    let init = None, None in
    let eo, spo = List.fold_left fold init params in
    let expr =
      match eo with
      | Some expr -> expr
      | None -> Pos.without (Com.Literal (Com.Float 1.0))
    in
    let m_sp_opt = match spo with Some m_sp -> Some (m_sp, -1) | None -> None in
    `VarCatsIt (vcats, expr, m_sp_opt)
  }
| BETWEEN expr0 = with_pos(expression) RANGE expr1 = with_pos(expression)
  STEP step = with_pos(expression) COLON {
    `VarInterval (expr0, expr1, step)
  }

it_param_category:
| WITH expr = with_pos(expression) COLON { `Expr expr }
| SPACE sp = symbol_with_pos COLON {
    let sp_name = Pos.same (parse_variable $sloc (Pos.unmark sp)) sp in
    `Space sp_name
  }

rest_param:
| al = separated_nonempty_list(COMMA, with_pos(var_access)) COLON {
    `AccessList al
  }
| VARIABLE vn = symbol_with_pos
  COLON params = nonempty_list(with_pos(rest_param_category)) {
    let var = Pos.same (Com.Normal (Pos.unmark vn)) vn in
    let err msg pos = Errors.raise_spanned_error msg pos in
    let fold (co, eo, spo) = function
    | Pos.Mark (`Vcats vcats, pos) -> (
        match co with
        | None -> Some vcats, eo, spo
        | _ -> err "variable categories are already specified" pos
      )
    | Pos.Mark (`Expr m_e, pos) -> (
        match eo with
        | None -> co, Some m_e, spo
        | _ -> err "expression is already specified" pos
      )
    | Pos.Mark (`Space m_sp, pos) -> (
        match spo with
        | None -> co, eo, Some m_sp
        | _ -> err "variable space is already specified" pos
      )
    in
    let init = None, None, None in
    let co, eo, spo = List.fold_left fold init params in
    let vcats =
      match co with
      | Some vcats -> vcats
      | None -> err "categories must be specified for this variable" (Pos.get vn)
    in
    let expr =
      match eo with
      | Some expr -> expr
      | None -> Pos.without (Com.Literal (Com.Float 1.0))
    in
    let m_sp_opt = match spo with Some m_sp -> Some (m_sp, -1) | None -> None in
    `VarCatsRest (var, vcats, expr, m_sp_opt)
  }
| EVENTS expr_list = separated_nonempty_list(COMMA, with_pos(expression)) COLON {
    `EventList expr_list
  }
| EVENT vn = symbol_with_pos COLON WITH expr = with_pos(expression) COLON {
     let var = Pos.same (Com.Normal (Pos.unmark vn)) vn in
    `EventFilter (var, expr)
  }

rest_param_category:
| CATEGORY vcat_list = separated_nonempty_list(COMMA, with_pos(var_category_id)) COLON {
    let vcats =
      let fold res vc =
        let vcm = Com.CatVar.Map.from_string_list vc in
        Com.CatVar.Map.union (fun _ p _ -> Some p) vcm res
      in
      List.fold_left fold Com.CatVar.Map.empty vcat_list
    in
    `Vcats vcats
  }
| WITH expr = with_pos(expression) COLON { `Expr expr }
| SPACE sp = symbol_with_pos COLON {
    let sp_name = Pos.same (parse_variable $sloc (Pos.unmark sp)) sp in
    `Space sp_name
  }

arrange_events_param:
| SORT v0 = symbol_with_pos COMMA v1 = symbol_with_pos
  COLON WITH expr = with_pos(expression) COLON {
    let var0 = Pos.same (Com.Normal (Pos.unmark v0)) v0 in
    let var1 = Pos.same (Com.Normal (Pos.unmark v1)) v1 in  
    `ArrangeEventsSort (var0, var1, expr)
  }
| FILTER v = symbol_with_pos COLON WITH expr = with_pos(expression) COLON {
    let var = Pos.same (Com.Normal (Pos.unmark v)) v in
    `ArrangeEventsFilter (var, expr)
  }
| ADD expr = with_pos(expression) COLON {
    `ArrangeEventsAdd (expr)
  }

formula_kind:
| f = formula { SingleFormula f }
| fs = for_formula { let (lv, ft) = fs in MultipleFormulaes (lv, ft) }

for_formula:
| FOR lv = with_pos(loop_variables) COLON ft = formula { (lv, ft) }

var_access:
| sp = symbol_with_pos DOT v = symbol_with_pos m_i_opt = with_pos(brackets)? {
    let m_sp = Pos.same (parse_variable $sloc (Pos.unmark sp)) sp in
    let m_v = Pos.same (parse_variable $sloc (Pos.unmark v)) v in
    match m_i_opt with
    | None -> Com.VarAccess (Some (m_sp, -1), m_v)
    | Some m_i -> Com.TabAccess (Some (m_sp, -1), m_v, m_i)
  }
| v = symbol_with_pos m_i_opt = with_pos(brackets)? {
    let m_v = Pos.same (parse_variable $sloc (Pos.unmark v)) v in
    match m_i_opt with
    | None -> Com.VarAccess (None, m_v)
    | Some m_i -> Com.TabAccess (None, m_v, m_i)
  }
| sp = symbol_with_pos DOT EVENT_FIELD LPAREN idx = with_pos(expression)
  COMMA f = symbol_with_pos RPAREN {
    let m_sp = Pos.same (parse_variable $sloc (Pos.unmark sp)) sp in
    Com.FieldAccess (Some (m_sp, -1), idx, f, -1)
  }
| EVENT_FIELD LPAREN idx = with_pos(expression)
  COMMA f = symbol_with_pos RPAREN {
    Com.FieldAccess (None, idx, f, -1)
  }

formula:
| access = with_pos(var_access) EQUALS e = with_pos(expression) {
    VarDecl (access, e)
  }
| EVENT_FIELD LPAREN idx = with_pos(expression)
  COMMA f = symbol_with_pos RPAREN REFERENCE v = symbol_with_pos {
    let var = Pos.same (parse_variable $sloc (Pos.unmark v)) v in
    EventFieldRef (idx, f, -1, var)
  }

verification_etc:
| v = with_pos(verification) l = with_pos(symbol_colon_etc)* { v :: l }

verification:
| VERIFICATION name = symbol_list_with_pos COLON
  APPLICATION COLON apps = symbol_enumeration SEMICOLON
  verif_conditions = with_pos(verification_condition)+ {
    let num, verif_tag_names =
      let uname = Pos.unmark name in
      let begPos =
        match uname with
        | h :: _ -> Pos.get h
        | [] -> assert false
      in
      let rec aux tags endPos = function
      | [num] ->
           let pos = Pos.make_between begPos endPos in
           num, (Pos.mark tags pos)
      | h :: t -> aux (h :: tags) (Pos.get h) t
      | [] -> assert false
      in
      aux [] begPos uname
    in
    let verif_number =
      try Pos.map int_of_string num
      with _ ->
        Errors.raise_spanned_error
          "this verification doesn't have an execution number"
          (Pos.get num)
    in
    let verif_apps =
      match apps with
      | [] ->
          Errors.raise_spanned_error
            "this verification doesn't belong to an application"
            (Pos.get verif_number)
      | _ ->
        List.fold_left
          (fun res (Pos.Mark (app, pos)) ->
            match StrMap.find_opt app res with
            | Some (Pos.Mark (_, old_pos)) ->
                let msg =
                  Format.asprintf "application %s already declared %a"
                    app
                    Pos.format old_pos
                in
                Errors.raise_spanned_error msg pos
            | None -> StrMap.add app (Pos.mark app pos) res)
          StrMap.empty
          apps
    in
    let verif = {
      verif_number;
      verif_tag_names;
      verif_apps;
      verif_conditions
    } in
    Verification verif
  }

verification_condition:
| IF e = with_pos(expression) THEN
  ERROR e_name = symbol_with_pos var = with_pos(variable_name)? SEMICOLON {
    {
      verif_cond_expr = e;
      verif_cond_error = e_name, var;
    }
  }

error_name:
n = SYMBOL COLON { n }

error_descr:
s = STRING { parse_string s }

error_message:
| COLON  s = with_pos(error_descr) { s }

error_:
| n = with_pos(error_name) t = with_pos(type_error)
  COLON s1 = with_pos(error_descr)
  COLON s2 = with_pos(error_descr)
  COLON s3 = with_pos(error_descr)
  COLON s4 = with_pos(error_descr)
  s5 = error_message? SEMICOLON {
    let err = {
      error_name = n;
      error_typ = t;
      error_descr =
        let s5l = match s5 with None -> [] | Some s -> [s] in
        s1 :: s2 :: s3 :: s4 :: s5l;
    } in
    Error err
  }

type_error:
| ANOMALY { Com.Error.Anomaly }
| DISCORDANCE { Com.Error.Discordance }
| INFORMATIVE { Com.Error.Information }


output_etc:
| o = with_pos(output) l = with_pos(symbol_colon_etc)* { o :: l }

output:
| OUTPUT LPAREN s = with_pos(variable_name) RPAREN SEMICOLON { Output s }

brackets:
| LBRACKET i = expression RBRACKET { i }

loop_variables:
| lrs = loop_variables_ranges { Com.Ranges lrs }
| lvs = loop_variables_values { Com.ValueSets lvs }

loop_variables_values:
| lvs = separated_nonempty_list(SEMICOLON, loop_variables_value) { lvs }

loop_variable_value_name:
| s = SYMBOL { parse_parameter $sloc s }

loop_variables_value:
| s = with_pos(loop_variable_value_name) EQUALS e = enumeration_loop { s, e }

loop_variables_ranges:
| r = loop_variables_range { [r] }
| r = loop_variables_range AND rs = loop_variables_ranges { r::rs }

loop_variables_range:
| ONE s = with_pos(loop_variable_value_name) IN e = enumeration_loop { s, e }

enumeration_loop:
| i = enumeration_loop_item { [i] }
| i = enumeration_loop_item COMMA is = enumeration_loop { i::is }

enumeration_loop_item:
| bounds = interval_loop { bounds  }
| s = SYMBOL {
    let pos = mk_position $sloc in
    Com.Single (Pos.mark (parse_to_atom (parse_variable_or_int $sloc s) pos) pos)
  }

range_or_minus:
| RANGE { `Range }
| MINUS { `Minus }

interval_loop:
| i1 = SYMBOL rm = range_or_minus i2 = SYMBOL {
    let pos = mk_position $sloc in
    let l1 = Pos.mark (parse_to_atom (parse_variable_or_int $sloc i1) pos) pos in
    let l2 = Pos.mark (parse_to_atom (parse_variable_or_int $sloc i2) pos) pos in
    match rm with
    | `Range -> Com.Range (l1, l2)
    | `Minus -> Com.Interval (l1, l2)
  }

enumeration:
| i = enumeration_item { [i] }
| i = enumeration_item COMMA is = enumeration { i::is }

enumeration_item:
| bounds = interval { bounds }
| sp = symbol_with_pos DOT EVENT_FIELD LPAREN idx = with_pos(expression)
  COMMA field = symbol_with_pos RPAREN {
    let m_sp = Pos.same (parse_variable $sloc (Pos.unmark sp)) sp in
    let pos = mk_position $sloc in
    let access = Com.FieldAccess (Some (m_sp, -1), idx, field, -1) in
    Com.VarValue (Pos.mark access pos)
  }
| EVENT_FIELD LPAREN idx = with_pos(expression)
  COMMA field = symbol_with_pos RPAREN {
    let pos = mk_position $sloc in
    let access = Com.FieldAccess (None, idx, field, -1) in
    Com.VarValue (Pos.mark access pos)
  }
| sp = symbol_with_pos DOT v = symbol_with_pos m_i_opt = with_pos(brackets)? {
    let m_sp = Pos.same (parse_variable $sloc (Pos.unmark sp)) sp in
    let m_v =  Pos.same (parse_variable $sloc (Pos.unmark v)) v in
    let a =
      match m_i_opt with
      | None -> Com.VarAccess (Some (m_sp, -1), m_v)
      | Some m_i -> Com.TabAccess (Some (m_sp, -1), m_v, m_i)
    in
    Com.VarValue (Pos.mark a (mk_position $sloc))
  }
| v = symbol_with_pos LBRACKET m_i = with_pos(expression) RBRACKET {
    let m_v =  Pos.same (parse_variable $sloc (Pos.unmark v)) v in
    let a = Com.TabAccess (None, m_v, m_i) in
    Com.VarValue (Pos.mark a (mk_position $sloc))
  }
| v = SYMBOL {
    let pos = mk_position $sloc in
    match parse_variable_or_int $sloc v with
    | ParseVar v' ->
        Com.VarValue (Pos.mark (Com.VarAccess (None, Pos.mark v' pos)) pos)
    | ParseInt i -> Com.FloatValue (Pos.mark (float_of_int i) pos)
  }

interval:
| i1 = SYMBOL RANGE i2 = SYMBOL {
    let pos = mk_position $sloc in
    let ir1 = Pos.mark (parse_int $sloc i1) pos in
    let ir2 = Pos.mark (parse_int $sloc i2) pos in
    Com.IntervalValue (ir1, ir2) : set_value
  }
 (* Some intervals are "03..06" so we must keep the prefix "0" *)

expression:
| e = with_pos(sum_expression) NOT IN LPAREN s = enumeration RPAREN {
    TestInSet (false, e, s)
  }
| e = with_pos(sum_expression) IN LPAREN s = enumeration RPAREN {
    TestInSet (true, e, s)
  }
| e1 = with_pos(sum_expression)
  op = with_pos(comparison_op)
  e2 = with_pos(sum_expression) {
    Comparison (op, e1, e2)
  }
| e = sum_expression { e }
| e1 = with_pos(expression)
  op = with_pos(logical_binop)
  e2 = with_pos(expression) {
    Binop (op, e1, e2)
  }
| FOR le = loop_expression { let l1, l2 = le in Loop (l1, l2) }
| NOT e = with_pos(expression) { Unop (Not, e) }

%inline logical_binop:
| AND { Com.And }
| OR { Com.Or }

sum_expression:
| e = product_expression { e }
| e1 = with_pos(sum_expression)
  op = with_pos(sum_operator)
  e2 = with_pos(product_expression) {
    Com.Binop (op, e1, e2)
  }

%inline sum_operator:
| PLUS { Com.Add }
| MINUS { Com.Sub }

product_expression:
| e = factor { e }
| e1 = with_pos(product_expression)
  op = with_pos(product_operator)
  e2 = with_pos(factor) {
    Com.Binop (op, e1, e2)
  }

%inline product_operator:
| TIMES { Com.Mul }
| DIV { Com.Div }
| MOD { Com.Mod }

factor:
| MINUS e = with_pos(factor) { Com.Unop (Minus, e) }
| e = ternary_operator { e }
| e = function_call { e }
| sp = symbol_with_pos DOT EVENT_FIELD LPAREN m_idx = with_pos(expression)
  COMMA field = symbol_with_pos RPAREN {
    let m_sp =  Pos.same (parse_variable $sloc (Pos.unmark sp)) sp in
    Var (FieldAccess (Some (m_sp, -1), m_idx, field, -1))
  }
| EVENT_FIELD LPAREN m_idx = with_pos(expression)
  COMMA field = symbol_with_pos RPAREN {
    Var (FieldAccess (None, m_idx, field, -1))
  }
| sp = symbol_with_pos DOT v = symbol_with_pos
  LBRACKET m_i = with_pos(sum_expression) RBRACKET {
    let m_sp =  Pos.same (parse_variable $sloc (Pos.unmark sp)) sp in
    let m_v = Pos.same (parse_variable $sloc (Pos.unmark v)) v in
    Var (TabAccess (Some (m_sp, -1), m_v, m_i))
  }
| v = symbol_with_pos LBRACKET m_i = with_pos(sum_expression) RBRACKET {
    let m_v = Pos.same (parse_variable $sloc (Pos.unmark v)) v in
    Var (TabAccess (None, m_v, m_i))
  }
| sp = symbol_with_pos DOT v = symbol_with_pos {
    let m_sp =  Pos.same (parse_variable $sloc (Pos.unmark sp)) sp in
    let m_v = Pos.same (parse_variable $sloc (Pos.unmark v)) v in
    Var (VarAccess (Some (m_sp, -1), m_v))
  }
| a = SYMBOL {
    match parse_atom $sloc a with
    | Com.AtomVar v -> Com.Var (VarAccess (None, v))
    | Com.AtomLiteral l -> Com.Literal l
  }
| UNDEFINED { Com.Literal Undefined }
| LPAREN e = expression RPAREN { e }

loop_expression:
| lvs = with_pos(loop_variables) COLON e = with_pos(expression) {
    lvs, e
  } %prec SEMICOLON

ternary_operator:
| IF e1 = with_pos(expression)
  THEN e2 = with_pos(expression)
  e3 = else_branch?
  ENDIF {
    Com.Conditional (e1, e2, e3)
  }

else_branch:
| ELSE e = with_pos(expression) { e }

function_name:
| VERIF_NUMBER { "numero_verif" }
| COMPL_NUMBER { "numero_compl" }
| s = SYMBOL { parse_func_name $sloc s }

function_call:
| NB_CATEGORY LPAREN cats = with_pos(var_category_id) RPAREN {
    NbCategory (Com.CatVar.Map.from_string_list cats)
  }
| ATTRIBUT LPAREN access = with_pos(var_access)
  COMMA attr = symbol_with_pos RPAREN {
    Attribut (access, attr)
  }
| SIZE LPAREN access = with_pos(var_access) RPAREN { Size access }
| TYPE LPAREN access = with_pos(var_access)
  COMMA typ = with_pos(value_type_prim) RPAREN { Type (access, typ) }
| NB_ANOMALIES LPAREN RPAREN { NbAnomalies }
| NB_DISCORDANCES LPAREN RPAREN { NbDiscordances }
| NB_INFORMATIVES LPAREN RPAREN { NbInformatives }
| NB_BLOCKING LPAREN RPAREN { NbBloquantes }
| SAME_VARIABLE LPAREN access0 = with_pos(var_access)
  COMMA access1 = with_pos(var_access) RPAREN {
    SameVariable (access0, access1)
  }
| IN_DOMAIN LPAREN access = with_pos(var_access)
  COMMA vcat = with_pos(var_category_id) RPAREN {
    let vc = Com.CatVar.Map.from_string_list vcat in
    InDomain (access, vc)
  }
| s = with_pos(function_name) LPAREN RPAREN {
    FuncCall (parse_function_name s, [])
  }
| s = with_pos(function_name) LPAREN call_args = function_call_args RPAREN {
    let f_name = parse_function_name s in
    match call_args with
    | `CallArgs args -> Com.FuncCall (f_name, args)
    | `CallLoop (l1, l2) -> Com.FuncCallLoop (f_name, l1, l2)
  }

function_call_args:
| l = loop_expression { let l1, l2 = l in `CallLoop (l1, l2) }
| args = function_arguments { `CallArgs args }

function_arguments:
| e = with_pos(sum_expression) { [e] }
| e = with_pos(sum_expression) COMMA es = function_arguments { e :: es }

%inline comparison_op:
| GTE  { Com.Gte }
| LTE  { Com.Lte }
| LT { Com.Lt }
| GT { Com.Gt }
| NEQ  { Com.Neq }
| EQUALS { Com.Eq }

symbol_enumeration:
| ss = separated_nonempty_list(COMMA, symbol_with_pos) { ss }

