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

type syms = Mast.DomainId.t Pos.marked Mast.DomainIdMap.t

type 'a doms = 'a Mir.domain Mast.DomainIdMap.t

type chaining = {
  chain_name : string Pos.marked;
  chain_apps : Pos.t StrMap.t;
  chain_rules : Mir.rule_domain Pos.marked IntMap.t;
}

type rule = {
  rule_id : int Pos.marked;
  rule_apps : Pos.t StrMap.t;
  rule_domain : Mir.rule_domain;
  rule_chain : string option;
  rule_instrs : Mast.instruction Pos.marked list;
  rule_in_vars : StrSet.t;
  rule_out_vars : StrSet.t;
  rule_seq : int;
}

type verif = {
  verif_id : int Pos.marked;
  verif_apps : Pos.t StrMap.t;
  verif_domain : Mir.verif_domain;
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
  prog_app : string;
  prog_apps : Pos.t StrMap.t;
  prog_chainings : chaining StrMap.t;
  prog_var_cats : Com.CatVar.data Com.CatVar.Map.t;
  prog_vars : Mir.Var.t StrMap.t;
  prog_alias : Mir.Var.t StrMap.t;
  prog_errors : Com.Error.t StrMap.t;
  prog_rdoms : Mir.rule_domain_data doms;
  prog_rdom_syms : syms;
  prog_vdoms : Mir.verif_domain_data doms;
  prog_vdom_syms : syms;
  prog_rules : rule IntMap.t;
  prog_rdom_calls : (int Pos.marked * Mast.DomainId.t) StrMap.t;
  prog_verifs : verif IntMap.t;
  prog_vdom_calls :
    (int Pos.marked * Mast.DomainId.t * Mast.expression Pos.marked) StrMap.t;
  prog_targets : Mast.target StrMap.t;
  prog_main_target : string;
  prog_stats : Mir.stats;
}

val mast_to_catvars :
  Pos.t Com.CatVar.Map.t ->
  Com.CatVar.data Com.CatVar.Map.t ->
  Pos.t Com.CatVar.Map.t

val cats_variable_from_decl_list :
  Mast.var_category_id list ->
  Com.CatVar.data Com.CatVar.Map.t ->
  Pos.t Com.CatVar.Map.t

val check_domain :
  rule_or_verif -> 'a Mast.domain_decl -> 'b -> 'b doms * syms -> 'b doms * syms

val proceed : Mast.program -> string -> program
