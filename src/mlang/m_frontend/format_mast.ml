(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

(** AST pretty printer *)

open Mast

let format_application fmt (app : application) = Format.fprintf fmt "%s" app

let format_chaining fmt (c : chaining) = Format.fprintf fmt "%s" c

let format_variable_name fmt (v : variable_name) = Format.fprintf fmt "%s" v

let format_variable_generic_name fmt (v : variable_generic_name) =
  Format.fprintf fmt "%s" v.base

let format_variable fmt (v : variable) =
  match v with
  | Normal v -> format_variable_name fmt v
  | Generic v -> format_variable_generic_name fmt v

let format_error_name fmt (e : error_name) = Format.fprintf fmt "%s" e

let format_expression = Com.format_expression format_variable

let format_var_category_id fmt (vd : var_category_id) =
  match Pos.unmark vd with
  | ("saisie", _) :: l ->
      Format.fprintf fmt "saisie %a" (Pp.list_space (Pp.unmark Pp.string)) l
  | ("calculee", _) :: l ->
      Format.fprintf fmt "calculee %a" (Pp.list_space (Pp.unmark Pp.string)) l
  | [ ("*", _) ] -> Format.fprintf fmt "*"
  | _ -> assert false

let format_event_decl fmt el =
  let pp_field fmt (ef : Com.event_field) =
    let ef_type = if ef.is_var then "variable" else "valeur" in
    Format.fprintf fmt "%s %s" ef_type (Pos.unmark ef.name)
  in
  Format.fprintf fmt "evenement : %a;" (Pp.list " : " pp_field) el

let format_instruction fmt i =
  Com.format_instruction format_variable Pp.string fmt i

let format_instruction_list fmt (il : instruction Pos.marked list) =
  (Pp.list "" (Pp.unmark format_instruction)) fmt il

let format_rule fmt (r : rule) =
  Format.fprintf fmt "regle %d:\napplication %a;\n%a;\n"
    (Pos.unmark r.rule_number)
    (StrMap.pp ~pp_key:Pp.nil ~sep:"," (Pp.unmark Pp.string))
    r.rule_apps format_instruction_list r.rule_formulaes

let format_table_size fmt = function
  | Some (Mast.LiteralSize i, _) -> Format.fprintf fmt "[%d]" i
  | Some (Mast.SymbolSize s, _) -> Format.fprintf fmt "[%s]" s
  | None -> ()

let format_target fmt (t : target) =
  let format_tmp_var fmt (name, size) =
    let name = Pos.unmark name in
    Format.fprintf fmt "%s%a" name format_table_size size
  in
  Format.fprintf fmt
    "cible %s:\napplication %a\n: variables temporaires %a;\n%a;\n"
    (Pos.unmark t.target_name)
    (StrMap.pp ~pp_key:Pp.nil ~sep:"," (Pp.unmark Pp.string))
    t.target_apps
    (StrMap.pp ~pp_key:Pp.nil ~sep:"," format_tmp_var)
    t.target_tmp_vars format_instruction_list t.target_prog

let format_input_attribute fmt ((n, v) : variable_attribute) =
  Format.fprintf fmt "%s = %d" (Pos.unmark n) (Pos.unmark v)

let format_input_variable fmt (v : input_variable) =
  Format.fprintf fmt "%a %s %a %a %a : %s%a;" format_variable_name
    (Pos.unmark v.input_name) Mast.input_category (Pp.list_space Pp.string)
    (List.map Pos.unmark v.input_category)
    (Pp.list_space format_input_attribute)
    v.input_attributes format_variable_name (Pos.unmark v.input_alias)
    (Pos.unmark v.input_description)
    (Pp.option (Pp.unmark Com.format_value_typ))
    v.input_typ

let format_computed_variable fmt (v : computed_variable) =
  Format.fprintf fmt "%s%a %s %a : %a%s;" (Pos.unmark v.comp_name)
    format_table_size v.comp_table computed_category
    (Pp.list_space (Pp.unmark Pp.string))
    v.comp_category
    (Pp.option (Pp.unmark Com.format_value_typ))
    v.comp_typ
    (Pos.unmark v.comp_description)

let format_atom = Com.format_atom format_variable

let format_variable_decl fmt (v : variable_decl) =
  match v with
  | ComputedVar v -> format_computed_variable fmt (Pos.unmark v)
  | ConstVar (name, value) ->
      Format.fprintf fmt "%a : const = %a" format_variable_name
        (Pos.unmark name) format_atom (Pos.unmark value)
  | InputVar v -> format_input_variable fmt (Pos.unmark v)

let format_verification_condition fmt (vc : verification_condition) =
  Format.fprintf fmt "si %a\n alors erreur %a %a;" format_expression
    (Pos.unmark vc.verif_cond_expr)
    (Pp.unmark format_error_name)
    (fst vc.verif_cond_error)
    (Pp.option (Pp.unmark format_variable_name))
    (snd vc.verif_cond_error)

let format_verification fmt (v : verification) =
  Format.fprintf fmt "verif %d : %a;\n%a"
    (Pos.unmark v.verif_number)
    (StrMap.pp ~pp_key:Pp.nil ~sep:"," (Pp.unmark Pp.string))
    v.verif_apps
    (Pp.list_space (Pp.unmark format_verification_condition))
    v.verif_conditions

let format_error_typ fmt (e : Com.Error.typ) =
  Pp.string fmt
    (match e with
    | Com.Error.Anomaly -> "anomalie"
    | Com.Error.Discordance -> "discordance"
    | Com.Error.Information -> "information")

let format_error_ fmt (e : error_) =
  Format.fprintf fmt "%a : %a : %a;" format_error_name (Pos.unmark e.error_name)
    format_error_typ (Pos.unmark e.error_typ)
    (Pp.list " : " (Pp.unmark Pp.string))
    e.error_descr

let format_var_type (t : var_type) =
  match t with Input -> input_category | Computed -> computed_category

let format_var_category fmt (c : var_category_decl) =
  Format.fprintf fmt "%s %a :@ attributs %a"
    (format_var_type c.var_type)
    (Pp.list_space (Pp.unmark Pp.string))
    c.var_category
    (Pp.list_comma (Pp.unmark Pp.string))
    c.var_attributes

let format_specialize_domain fmt (dl : string Pos.marked list Pos.marked list) =
  match dl with
  | [] -> ()
  | _ ->
      Format.fprintf fmt " :@ specialise %a"
        (Pp.list_comma (Pp.unmark (Pp.list_space (Pp.unmark Pp.string))))
        dl

let format_domain_attribute attr fmt b =
  if b then Format.fprintf fmt " :@ %s" attr

let format_domain (pp_data : Format.formatter -> 'a -> unit) fmt
    (d : 'a domain_decl) =
  Format.fprintf fmt "%a%a%a%a"
    (Pp.list_comma (Pp.unmark (Pp.list_space (Pp.unmark Pp.string))))
    d.dom_names format_specialize_domain d.dom_parents
    (format_domain_attribute "par_defaut")
    d.dom_by_default pp_data d.dom_data

let format_rule_domain fmt (rd : rule_domain_decl) =
  let pp_data fmt data =
    Format.fprintf fmt "%a"
      (format_domain_attribute "calculable")
      data.rdom_computable
  in
  format_domain pp_data fmt rd

let format_verif_domain fmt (vd : verif_domain_decl) =
  let pp_data fmt data =
    Format.fprintf fmt "%a"
      (Pp.list_comma format_var_category_id)
      data.vdom_auth
  in
  format_domain pp_data fmt vd

let format_source_file_item fmt (i : source_file_item) =
  match i with
  | Application app ->
      Format.fprintf fmt "application %a;" format_application (Pos.unmark app)
  | Chaining (c, apps) ->
      Format.fprintf fmt "enchaineur %a %a;" format_chaining (Pos.unmark c)
        (Pp.list_space (Pp.unmark format_application))
        apps
  | VariableDecl vd -> format_variable_decl fmt vd
  | EventDecl el -> format_event_decl fmt el
  | Function t -> format_target fmt t
  | Rule r -> format_rule fmt r
  | Target t -> format_target fmt t
  | Verification v -> format_verification fmt v
  | Func -> ()
  | Error e -> format_error_ fmt e
  | Output o ->
      Format.fprintf fmt "sortie(%a);" format_variable_name (Pos.unmark o)
  | VarCatDecl c ->
      Format.fprintf fmt "variable category %a;" format_var_category
        (Pos.unmark c)
  | RuleDomDecl rd -> Format.fprintf fmt "rule domain %a;" format_rule_domain rd
  | VerifDomDecl vd ->
      Format.fprintf fmt "verif domain %a;" format_verif_domain vd

let format_source_file fmt (f : source_file) =
  Pp.list_endline (Pp.unmark format_source_file_item) fmt f
