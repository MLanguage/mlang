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
  nb_computed : int;
  nb_base : int;
  nb_input : int;
  nb_vars : int;
  nb_all_tmps : int;
  nb_all_refs : int;
  sz_computed : int;
  sz_base : int;
  sz_input : int;
  sz_vars : int;
  sz_all_tmps : int;
  nb_all_tables : int;
  sz_all_tables : int;
  max_nb_args : int;
  table_map : Com.Var.t IntMap.t;
}

type program = {
  program_safe_prefix : string;
  program_applications : Pos.t StrMap.t;
  program_var_categories : Com.CatVar.data Com.CatVar.Map.t;
  program_rule_domains : Com.rule_domain Com.DomainIdMap.t;
  program_verif_domains : Com.verif_domain Com.DomainIdMap.t;
  program_dict : Com.Var.t IntMap.t;
  program_vars : Com.Var.t StrMap.t;
  program_alias : Com.Var.t StrMap.t;
  program_var_spaces : int StrMap.t;
  program_var_spaces_idx : Com.variable_space IntMap.t;
  program_var_space_def : Com.variable_space;
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
          | Com.VarValue m_a ->
              let a' = expand_functions_access (Pos.unmark m_a) in
              Com.VarValue (Pos.same a' m_a)
          | value -> value
        in
        List.map map values
      in
      Pos.same (TestInSet (positive, new_e0, new_values)) e
  | Comparison (op, e1, e2) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      Pos.same (Comparison (op, new_e1, new_e2)) e
  | Binop (op, e1, e2) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      Pos.same (Binop (op, new_e1, new_e2)) e
  | Unop (op, e1) ->
      let new_e1 = expand_functions_expr e1 in
      Pos.same (Unop (op, new_e1)) e
  | Conditional (e1, e2, e3) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      let new_e3 = Option.map expand_functions_expr e3 in
      Pos.same (Conditional (new_e1, new_e2, new_e3)) e
  | Var access ->
      let e' = Var (expand_functions_access access) in
      Pos.same e' e
  | Literal _ -> e
  | FuncCall (Pos.Mark (SumFunc, _), args) ->
      let expr_opt =
        List.fold_left
          (fun acc_opt arg ->
            match acc_opt with
            | None -> Some (Pos.unmark (expand_functions_expr arg))
            | Some acc ->
                Some
                  (Binop
                     ( Pos.same Com.Add e,
                       Pos.same acc e,
                       expand_functions_expr arg )))
          None args
      in
      let expr =
        match expr_opt with None -> Literal (Float 0.0) | Some expr -> expr
      in
      Pos.same expr e
  | FuncCall (Pos.Mark (GtzFunc, _), [ arg ]) ->
      Pos.same
        (Comparison
           ( Pos.same Com.Gt e,
             expand_functions_expr arg,
             Pos.same (Literal (Float 0.0)) e ))
        e
  | FuncCall (Pos.Mark (GtezFunc, _), [ arg ]) ->
      Pos.same
        (Comparison
           ( Pos.same Com.Gte e,
             expand_functions_expr arg,
             Pos.same (Literal (Float 0.0)) e ))
        e
  | FuncCall ((Pos.Mark ((MinFunc | MaxFunc), _) as fn), [ arg1; arg2 ]) ->
      let earg1 = expand_functions_expr arg1 in
      let earg2 = expand_functions_expr arg2 in
      Pos.same (FuncCall (fn, [ earg1; earg2 ])) e
  | FuncCall ((Pos.Mark (AbsFunc, _) as fn), [ arg ]) ->
      Pos.same (FuncCall (fn, [ expand_functions_expr arg ])) e
  | FuncCall (Pos.Mark (NullFunc, _), [ arg ]) ->
      Pos.same
        (Comparison
           ( Pos.same Com.Eq e,
             expand_functions_expr arg,
             Pos.same (Literal (Float 0.0)) e ))
        e
  | FuncCall (fn, args) ->
      Pos.same (FuncCall (fn, List.map expand_functions_expr args)) e
  | Attribut (m_a, attr) ->
      let a' = expand_functions_access (Pos.unmark m_a) in
      let e' = Attribut (Pos.same a' m_a, attr) in
      Pos.same e' e
  | Size m_a ->
      let a' = expand_functions_access (Pos.unmark m_a) in
      let e' = Size (Pos.same a' m_a) in
      Pos.same e' e
  | IsVariable (m_a, name) ->
      let a' = expand_functions_access (Pos.unmark m_a) in
      let e' = IsVariable (Pos.same a' m_a, name) in
      Pos.same e' e
  | NbAnomalies | NbDiscordances | NbInformatives | NbBloquantes
  | FuncCallLoop _ | Loop _ | NbCategory _ ->
      e

and expand_functions_access (access : 'var Com.access) : 'var Com.access =
  match access with
  | VarAccess _ -> access
  | TabAccess (m_sp, i_sp, m_v, i) ->
      let i' = expand_functions_expr i in
      TabAccess (m_sp, i_sp, m_v, i')
  | FieldAccess (m_sp_opt, i_sp, v_i, f, i_f) ->
      let m_i = expand_functions_expr v_i in
      FieldAccess (m_sp_opt, i_sp, m_i, f, i_f)

let expand_functions (p : program) : program =
  let open Com in
  let update_instrs p_procs =
    let rec map_instr m_instr =
      match Pos.unmark m_instr with
      | Affectation (Pos.Mark (SingleFormula (VarDecl (m_a, v_expr)), pos)) ->
          let m_expr = expand_functions_expr v_expr in
          let m_a' =
            let a' = expand_functions_access (Pos.unmark m_a) in
            Pos.same a' m_a
          in
          let form = SingleFormula (VarDecl (m_a', m_expr)) in
          Pos.same (Affectation (Pos.mark form pos)) m_instr
      | Affectation
          (Pos.Mark (SingleFormula (EventFieldRef (v_idx, f, i, v_id)), pos)) ->
          let m_idx = expand_functions_expr v_idx in
          let form = SingleFormula (EventFieldRef (m_idx, f, i, v_id)) in
          Pos.same (Affectation (Pos.mark form pos)) m_instr
      | Affectation (Pos.Mark (MultipleFormulaes _, _)) -> assert false
      | IfThenElse (i, t, e) ->
          let i' = expand_functions_expr i in
          let t' = List.map map_instr t in
          let e' = List.map map_instr e in
          Pos.same (IfThenElse (i', t', e')) m_instr
      | WhenDoElse (wdl, ed) ->
          let map_wdl (expr, dl, pos) =
            let expr' = expand_functions_expr expr in
            let dl' = List.map map_instr dl in
            (expr', dl', pos)
          in
          let wdl' = List.map map_wdl wdl in
          let ed' = Pos.map (List.map map_instr) ed in
          Pos.same (WhenDoElse (wdl', ed')) m_instr
      | ComputeTarget _ -> m_instr
      | VerifBlock instrs ->
          let instrs' = List.map map_instr instrs in
          Pos.same (VerifBlock instrs') m_instr
      | Print (out, pr_args) ->
          let pr_args' =
            List.map
              (fun m_arg ->
                match Pos.unmark m_arg with
                | Com.PrintAccess (info, m_a) ->
                    let a' = expand_functions_access (Pos.unmark m_a) in
                    let m_a' = Pos.same a' m_a in
                    Pos.same (Com.PrintAccess (info, m_a')) m_arg
                | Com.PrintIndent e ->
                    let e' = expand_functions_expr e in
                    Pos.same (Com.PrintIndent e') m_arg
                | Com.PrintExpr (e, mi, ma) ->
                    let e' = expand_functions_expr e in
                    Pos.same (Com.PrintExpr (e', mi, ma)) m_arg
                | Com.PrintString _ -> m_arg)
              pr_args
          in
          Pos.same (Print (out, pr_args')) m_instr
      | Iterate (v_id, vars, var_params, instrs) ->
          let var_params' =
            List.map
              (fun (cats, e) ->
                let e' = expand_functions_expr e in
                (cats, e'))
              var_params
          in
          let instrs' = List.map map_instr instrs in
          Pos.same (Iterate (v_id, vars, var_params', instrs')) m_instr
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
          Pos.same (Iterate_values (v_id, var_intervals', instrs')) m_instr
      | Restore (vars, var_params, evts, evtfs, instrs) ->
          let var_params' =
            let map (v, cs, e) = (v, cs, expand_functions_expr e) in
            List.map map var_params
          in
          let evts' = List.map expand_functions_expr evts in
          let evtfs' =
            List.map (fun (v, e) -> (v, expand_functions_expr e)) evtfs
          in
          let instrs' = List.map map_instr instrs in
          let instr' = Restore (vars, var_params', evts', evtfs', instrs') in
          Pos.same instr' m_instr
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
          Pos.same (ArrangeEvents (sort', filter', add', instrs')) m_instr
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
