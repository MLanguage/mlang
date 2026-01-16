(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

(** The validator performs several verifications on the consistency of the M
    program. The list of errors is defined in the internal module [Err] inside
    this module. Verification requires to translate the program into an
    intermediate representation, {!Validator.program}. In this representation,
    rules are sorted by dependencies over the variables of the selected
    applications and domains (using {!Utils.TopologicalSorting}). The output
    type is very close to {!M_ir.Mir.program} and will be translated later in
    this type, by {!Mast_to_mir.translate}.

    NB: the output of {!Validator.proceed} is temporary and will be modified in
    {!Mast_to_mir.translate}. *)

type syms = Com.DomainId.t Pos.marked Com.DomainIdMap.t

type 'a doms = 'a Com.domain Com.DomainIdMap.t
(** Domains can either be rule domains or verification domains. *)

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

module CallMap : MapExt.T with type key = call_compute

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
  prog_varalias : int StrMap.t;
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

val mast_to_catvars :
  Pos.t Com.CatVar.Map.t ->
  Com.CatVar.data Com.CatVar.Map.t ->
  Pos.t Com.CatVar.Map.t

val cats_variable_from_decl_list :
  Mast.var_category_id list ->
  Com.CatVar.data Com.CatVar.Map.t ->
  Pos.t Com.CatVar.Map.t

val proceed : string -> Mast.program -> program
