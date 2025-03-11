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

(** Main data structure for M analysis *)

(**{1 Variables} *)

(** Variables are first-class objects *)

type set_value = Com.Var.t Com.set_value

type expression = Com.Var.t Com.expression

type m_expression = expression Pos.marked

(** The definitions here are modeled closely to the source M language. One could
    also adopt a more lambda-calculus-compatible model with functions used to
    model tables. *)

type instruction = (Com.Var.t, Com.Error.t) Com.instruction

type m_instruction = instruction Pos.marked

type target = (Com.Var.t, Com.Error.t) Com.target

type stats = {
  nb_calculated : int;
  nb_base : int;
  nb_input : int;
  nb_vars : int;
  nb_all_tmps : int;
  nb_all_refs : int;
  sz_calculated : int;
  sz_base : int;
  sz_input : int;
  sz_vars : int;
  sz_all_tmps : int;
}

type program = {
  program_safe_prefix : string;
  program_applications : Pos.t StrMap.t;
  program_var_categories : Com.CatVar.data Com.CatVar.Map.t;
  program_rule_domains : Com.rule_domain Com.DomainIdMap.t;
  program_verif_domains : Com.verif_domain Com.DomainIdMap.t;
  program_vars : Com.Var.t StrMap.t;
  program_tabs : Com.Tab.t StrMap.t;
  program_alias : string Pos.marked StrMap.t;
  program_event_fields : Com.event_field StrMap.t;
  program_event_field_idxs : string IntMap.t;
  program_rules : string IntMap.t;
  program_verifs : string IntMap.t;
  program_chainings : string StrMap.t;
  program_errors : Com.Error.t StrMap.t;
  program_functions : target StrMap.t;
  program_targets : target StrMap.t;
  program_main_target : string;
  program_stats : stats;
}

(** {1 Helpers}*)

(** Throws an error in case of alias not found *)
let find_var_name_by_alias (p : program) (alias : string Pos.marked) : string =
  let v =
    StrMap.fold
      (fun _ v acc ->
        match (acc, Com.Var.alias v) with
        | Some _, _ | None, None -> acc
        | None, Some v_alias ->
            if Pos.unmark v_alias = Pos.unmark alias then
              Some (Pos.unmark v.name)
            else None)
      p.program_vars None
  in
  match v with
  | Some v -> v
  | None ->
      Errors.raise_spanned_error
        (Format.asprintf "alias not found: %s" (Pos.unmark alias))
        (Pos.get alias)

let find_var_by_name (p : program) (name : string Pos.marked) : Com.Var.t =
  try StrMap.find (Pos.unmark name) p.program_vars
  with Not_found -> (
    try
      let name = find_var_name_by_alias p name in
      StrMap.find name p.program_vars
    with Not_found ->
      Errors.raise_spanned_error "unknown variable" (Pos.get name))

let rec expand_functions_expr (e : 'var Com.expression Pos.marked) :
    'var Com.expression Pos.marked =
  let open Com in
  match Pos.unmark e with
  | TestInSet (positive, e0, values) ->
      let new_e0 = expand_functions_expr e0 in
      let new_values =
        let map = function
          | Com.VarValue (access, pos) -> (
              match access with
              | VarAccess v -> Com.VarValue (VarAccess v, pos)
              | TabAccess (m_v, i) ->
                  let new_i = expand_functions_expr i in
                  Com.VarValue (TabAccess (m_v, new_i), pos)
              | ConcAccess (m_vn, m_if, i) ->
                  let new_i = expand_functions_expr i in
                  Com.VarValue (ConcAccess (m_vn, m_if, new_i), pos)
              | FieldAccess (mei, f, i_f) ->
                  let new_mei = expand_functions_expr mei in
                  Com.VarValue (FieldAccess (new_mei, f, i_f), pos))
          | value -> value
        in
        List.map map values
      in
      Pos.same_pos_as (TestInSet (positive, new_e0, new_values)) e
  | Comparison (op, e1, e2) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      Pos.same_pos_as (Comparison (op, new_e1, new_e2)) e
  | Binop (op, e1, e2) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      Pos.same_pos_as (Binop (op, new_e1, new_e2)) e
  | Unop (op, e1) ->
      let new_e1 = expand_functions_expr e1 in
      Pos.same_pos_as (Unop (op, new_e1)) e
  | Conditional (e1, e2, e3) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      let new_e3 = Option.map expand_functions_expr e3 in
      Pos.same_pos_as (Conditional (new_e1, new_e2, new_e3)) e
  | Index ((VarAccess v, pos), e1) ->
      let new_e1 = expand_functions_expr e1 in
      Pos.same_pos_as (Index ((VarAccess v, pos), new_e1)) e
  | Index ((TabAccess _, _), _) -> assert false
  | Index ((ConcAccess (m_v, m_if, i), pos), e1) ->
      let new_i = expand_functions_expr i in
      let new_e1 = expand_functions_expr e1 in
      Pos.same_pos_as (Index ((ConcAccess (m_v, m_if, new_i), pos), new_e1)) e
  | Index ((FieldAccess (ie, f, i_f), pos), e1) ->
      let new_ie = expand_functions_expr ie in
      let new_e1 = expand_functions_expr e1 in
      Pos.same_pos_as (Index ((FieldAccess (new_ie, f, i_f), pos), new_e1)) e
  | Literal _ -> e
  | Var _ -> e
  | FuncCall ((SumFunc, _), args) ->
      let expr_opt =
        List.fold_left
          (fun acc_opt arg ->
            match acc_opt with
            | None -> Some (Pos.unmark (expand_functions_expr arg))
            | Some acc ->
                Some
                  (Binop
                     ( Pos.same_pos_as Com.Add e,
                       Pos.same_pos_as acc e,
                       expand_functions_expr arg )))
          None args
      in
      let expr =
        match expr_opt with None -> Literal (Float 0.0) | Some expr -> expr
      in
      Pos.same_pos_as expr e
  | FuncCall ((GtzFunc, _), [ arg ]) ->
      Pos.same_pos_as
        (Comparison
           ( Pos.same_pos_as Com.Gt e,
             expand_functions_expr arg,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FuncCall ((GtezFunc, _), [ arg ]) ->
      Pos.same_pos_as
        (Comparison
           ( Pos.same_pos_as Com.Gte e,
             expand_functions_expr arg,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FuncCall ((((MinFunc | MaxFunc) as f), pos), [ arg1; arg2 ]) ->
      let earg1 = expand_functions_expr arg1 in
      let earg2 = expand_functions_expr arg2 in
      Pos.same_pos_as (FuncCall ((f, pos), [ earg1; earg2 ])) e
  | FuncCall ((AbsFunc, pos), [ arg ]) ->
      Pos.same_pos_as
        (FuncCall ((AbsFunc, pos), [ expand_functions_expr arg ]))
        e
  | FuncCall ((NullFunc, _), [ arg ]) ->
      Pos.same_pos_as
        (Comparison
           ( Pos.same_pos_as Com.Eq e,
             expand_functions_expr arg,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FuncCall (fn, args) ->
      Pos.same_pos_as (FuncCall (fn, List.map expand_functions_expr args)) e
  | Attribut ((VarAccess _, _), _) -> e
  | Attribut ((TabAccess (m_v, i), pos), a) ->
      let new_i = expand_functions_expr i in
      Pos.same_pos_as (Attribut ((TabAccess (m_v, new_i), pos), a)) e
  | Attribut ((ConcAccess (m_v, m_if, i), pos), a) ->
      let new_i = expand_functions_expr i in
      Pos.same_pos_as (Attribut ((ConcAccess (m_v, m_if, new_i), pos), a)) e
  | Attribut ((FieldAccess (ie, f, i_f), pos), a) ->
      let new_ie = expand_functions_expr ie in
      Pos.same_pos_as (Attribut ((FieldAccess (new_ie, f, i_f), pos), a)) e
  | Size (VarAccess _, _) -> e
  | Size (TabAccess (m_v, i), pos) ->
      let new_i = expand_functions_expr i in
      Pos.same_pos_as (Size (TabAccess (m_v, new_i), pos)) e
  | Size (ConcAccess (m_v, m_if, i), pos) ->
      let new_i = expand_functions_expr i in
      Pos.same_pos_as (Size (ConcAccess (m_v, m_if, new_i), pos)) e
  | Size (FieldAccess (ie, f, i_f), pos) ->
      let new_ie = expand_functions_expr ie in
      Pos.same_pos_as (Size (FieldAccess (new_ie, f, i_f), pos)) e
  | NbAnomalies | NbDiscordances | NbInformatives | NbBloquantes
  | FuncCallLoop _ | Loop _ | NbCategory _ ->
      e

let expand_functions (p : program) : program =
  let open Com in
  let update_instrs p_procs =
    let rec map_instr m_instr =
      let instr, instr_pos = m_instr in
      match instr with
      | Affectation (SingleFormula (VarDecl (v_acc, v_idx_opt, v_expr)), pos) ->
          let m_idx_opt =
            match v_idx_opt with
            | Some v_idx -> Some (expand_functions_expr v_idx)
            | None -> None
          in
          let m_expr = expand_functions_expr v_expr in
          let m_acc =
            match Pos.unmark v_acc with
            | VarAccess _ -> v_acc
            | TabAccess (m_v, i) ->
                let i' = expand_functions_expr i in
                Pos.same_pos_as (TabAccess (m_v, i')) v_acc
            | ConcAccess (m_v, m_if, i) ->
                let i' = expand_functions_expr i in
                Pos.same_pos_as (ConcAccess (m_v, m_if, i')) v_acc
            | FieldAccess (v_i, f, i_f) ->
                let m_i = expand_functions_expr v_i in
                Pos.same_pos_as (FieldAccess (m_i, f, i_f)) v_acc
          in
          ( Affectation (SingleFormula (VarDecl (m_acc, m_idx_opt, m_expr)), pos),
            instr_pos )
      | Affectation (SingleFormula (EventFieldRef (v_idx, f, i, v_id)), pos) ->
          let m_idx = expand_functions_expr v_idx in
          ( Affectation (SingleFormula (EventFieldRef (m_idx, f, i, v_id)), pos),
            instr_pos )
      | Affectation (MultipleFormulaes _, _) -> assert false
      | IfThenElse (i, t, e) ->
          let i' = expand_functions_expr i in
          let t' = List.map map_instr t in
          let e' = List.map map_instr e in
          (IfThenElse (i', t', e'), instr_pos)
      | WhenDoElse (wdl, ed) ->
          let map_wdl (expr, dl, pos) =
            let expr' = expand_functions_expr expr in
            let dl' = List.map map_instr dl in
            (expr', dl', pos)
          in
          let wdl' = List.map map_wdl wdl in
          let ed' = Pos.map_under_mark (List.map map_instr) ed in
          (WhenDoElse (wdl', ed'), instr_pos)
      | ComputeTarget _ -> m_instr
      | VerifBlock instrs ->
          let instrs' = List.map map_instr instrs in
          (VerifBlock instrs', instr_pos)
      | Print (out, pr_args) ->
          let pr_args' =
            List.map
              (fun m_arg ->
                let arg, arg_pos = m_arg in
                match arg with
                | Com.PrintConcName (m_v, m_if, i) ->
                    let i' = expand_functions_expr i in
                    (Com.PrintConcName (m_v, m_if, i'), arg_pos)
                | Com.PrintConcAlias (m_v, m_if, i) ->
                    let i' = expand_functions_expr i in
                    (Com.PrintConcAlias (m_v, m_if, i'), arg_pos)
                | Com.PrintEventName (e, f, i) ->
                    let e' = expand_functions_expr e in
                    (Com.PrintEventName (e', f, i), arg_pos)
                | Com.PrintEventAlias (e, f, i) ->
                    let e' = expand_functions_expr e in
                    (Com.PrintEventAlias (e', f, i), arg_pos)
                | Com.PrintIndent e ->
                    let e' = expand_functions_expr e in
                    (Com.PrintIndent e', arg_pos)
                | Com.PrintExpr (e, mi, ma) ->
                    let e' = expand_functions_expr e in
                    (Com.PrintExpr (e', mi, ma), arg_pos)
                | Com.PrintString _ | Com.PrintName _ | Com.PrintAlias _ ->
                    m_arg)
              pr_args
          in
          (Print (out, pr_args'), instr_pos)
      | Iterate (v_id, vars, var_params, instrs) ->
          let var_params' =
            List.map
              (fun (cats, e) ->
                let e' = expand_functions_expr e in
                (cats, e'))
              var_params
          in
          let instrs' = List.map map_instr instrs in
          (Iterate (v_id, vars, var_params', instrs'), instr_pos)
      | Iterate_values (v_id, var_intervals, instrs) ->
          let var_intervals' =
            List.map
              (fun (e0, e1, step) ->
                let e0' = expand_functions_expr e0 in
                let e1' = expand_functions_expr e1 in
                let step' = expand_functions_expr step in
                (e0', e1', step'))
              var_intervals
          in
          let instrs' = List.map map_instr instrs in
          (Iterate_values (v_id, var_intervals', instrs'), instr_pos)
      | Restore (vars, var_params, evts, evtfs, instrs) ->
          let var_params' =
            List.map
              (fun (v, cs, e) -> (v, cs, expand_functions_expr e))
              var_params
          in
          let evts' = List.map expand_functions_expr evts in
          let evtfs' =
            List.map (fun (v, e) -> (v, expand_functions_expr e)) evtfs
          in
          let instrs' = List.map map_instr instrs in
          (Restore (vars, var_params', evts', evtfs', instrs'), instr_pos)
      | ArrangeEvents (sort, filter, add, instrs) ->
          let sort' =
            match sort with
            | Some (var0, var1, expr) ->
                let expr' = expand_functions_expr expr in
                Some (var0, var1, expr')
            | None -> None
          in
          let filter' =
            match filter with
            | Some (var, expr) ->
                let expr' = expand_functions_expr expr in
                Some (var, expr')
            | None -> None
          in
          let add' = Option.map expand_functions_expr add in
          let instrs' = List.map map_instr instrs in
          (ArrangeEvents (sort', filter', add', instrs'), instr_pos)
      | RaiseError _ | CleanErrors | ExportErrors | FinalizeErrors -> m_instr
      | ComputeDomain _ | ComputeChaining _ | ComputeVerifs _ -> assert false
    in
    StrMap.map
      (fun t ->
        let target_prog = List.map map_instr t.target_prog in
        { t with target_prog })
      p_procs
  in
  let program_functions = update_instrs p.program_functions in
  let program_targets = update_instrs p.program_targets in
  { p with program_functions; program_targets }
