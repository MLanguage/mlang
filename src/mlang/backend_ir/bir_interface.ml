(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr> Raphaël Monat <raphael.monat@lip6.fr>

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

type bir_function = {
  func_variable_inputs : unit Bir.VariableMap.t;
  func_constant_inputs : Bir.expression Pos.marked Bir.VariableMap.t;
  func_outputs : unit Bir.VariableMap.t;
  func_conds : Bir.condition_data Mir.RuleMap.t;
  func_errors : string list;
}

let translate_external_conditions var_cats idmap
    (conds : Mast.expression Pos.marked list) : Bir.condition_data Mir.RuleMap.t
    =
  let check_boolean (mexpr : Mast.expression Pos.marked) =
    match Pos.unmark mexpr with
    | Binop (((And | Or), _), _, _) -> true
    | Comparison (_, _, _) -> true
    | Unop (Not, _) -> true
    | TestInSet _ -> true
    (* TODO: check Literal Variable ? *)
    | _ -> false
  in
  let mk_neg (mexpr : Mast.expression Pos.marked) =
    Pos.same_pos_as (Mast.Unop (Mast.Not, mexpr)) mexpr
  in
  let dummy_entry = ("", Pos.no_pos) in
  let test_error =
    Mir.Error.new_error ("-1", Pos.no_pos)
      {
        error_name = ("", Pos.no_pos);
        error_typ = (Mast.Anomaly, Pos.no_pos);
        error_descr =
          [
            ("Condition error in tests", Pos.no_pos);
            dummy_entry;
            dummy_entry;
            dummy_entry;
          ];
      }
      Mast.Anomaly
  in
  let verif_conds =
    List.fold_left
      (fun acc cond ->
        if not (check_boolean cond) then
          Errors.raise_spanned_error "condition should have type bool"
            (Pos.get_position cond)
        else
          Pos.same_pos_as
            {
              Mast.verif_cond_expr = mk_neg cond;
              verif_cond_error = (("-1", Pos.no_pos), None);
            }
            cond
          :: acc)
      [] conds
  in
  let verif_dom_decl =
    Mast.
      {
        dom_names = [ ([ ("toto", Pos.no_pos) ], Pos.no_pos) ];
        dom_parents = [];
        dom_by_default = true;
        dom_data =
          {
            vdom_auth = [ ([ ("*", Pos.no_pos) ], Pos.no_pos) ];
            vdom_verifiable = true;
          };
      }
  in
  let program =
    List.map
      (fun item -> (item, Pos.no_pos))
      [
        Mast.VerifDomDecl verif_dom_decl;
        Mast.Verification
          {
            verif_number = (0, Pos.no_pos);
            verif_tag_names = ([ ("toto", Pos.no_pos) ], Pos.no_pos);
            verif_applications = [ ("iliad", Pos.no_pos) ];
            verif_conditions = verif_conds;
          };
      ]
  in
  let vdoms =
    let vdom_auth =
      let base = Mir.CatCompSet.singleton Base in
      let givenBack = Mir.CatCompSet.singleton GivenBack in
      let baseAndGivenBack = base |> Mir.CatCompSet.add GivenBack in
      Mir.CatVarSet.empty
      |> Mir.CatVarSet.add (Mir.CatComputed Mir.CatCompSet.empty)
      |> Mir.CatVarSet.add (Mir.CatComputed base)
      |> Mir.CatVarSet.add (Mir.CatComputed givenBack)
      |> Mir.CatVarSet.add (Mir.CatComputed baseAndGivenBack)
      |> Mir.CatVarSet.add (Mir.CatInput (StrSet.singleton "revenu"))
    in
    let dom_data = Mir.{ vdom_auth; vdom_verifiable = true } in
    let doms_syms = (Mast.DomainIdMap.empty, Mast.DomainIdMap.empty) in
    let doms, _ =
      Check_validity.check_domain Check_validity.Verif verif_dom_decl dom_data
        doms_syms
    in
    doms
  in
  let conds =
    (* Leave a constant map empty is risky, it will fail if we allow tests to
       refer to M constants in their expressions *)
    Mast_to_mir.get_conds vdoms var_cats [ test_error ] idmap [ program ]
  in
  Mir.RuleMap.map
    (fun data -> Mir.map_cond_data_var Bir.(var_from_mir default_tgv) data)
    conds

let generate_function_all_vars (_p : Bir.program) : bir_function =
  {
    func_variable_inputs = Bir.VariableMap.empty;
    func_constant_inputs = Bir.VariableMap.empty;
    func_outputs = Bir.VariableMap.empty;
    func_conds = Mir.RuleMap.empty;
    func_errors = [];
  }

(** Add variables, constants, conditions and outputs from [f] to [p] *)
let adapt_program_to_function (p : Bir.program) (f : bir_function) :
    Bir.program * int =
  let const_input_stmts =
    Bir.VariableMap.fold
      (fun var e acc ->
        Pos.same_pos_as (Bir.SAssign (var, Mir.SimpleVar e)) e :: acc)
      f.func_constant_inputs []
  in
  let unused_input_stmts = [] in
  let conds_stmts =
    Mir.RuleMap.fold
      (fun _ cond acc ->
        Pos.same_pos_as (Bir.SVerif cond) cond.cond_expr :: acc)
      f.func_conds []
  in
  ( {
      p with
      context =
        Some
          Bir.
            {
              constant_inputs_init_stmts = const_input_stmts;
              adhoc_specs_conds_stmts = conds_stmts;
              unused_inputs_init_stmts = unused_input_stmts;
            };
      outputs = f.func_outputs;
    },
    List.length unused_input_stmts + List.length const_input_stmts )
