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

type translation_ctx = {
  new_variables : Bir.variable StrMap.t;
  variables_used_as_inputs : Mir.VariableDict.t;
  used_rule_domains : Mast.DomainIdSet.t;
  used_chainings : Mast.ChainingSet.t;
  verif_seen : bool;
}

let empty_translation_ctx : translation_ctx =
  {
    new_variables = StrMap.empty;
    variables_used_as_inputs = Mir.VariableDict.empty;
    used_rule_domains = Mast.DomainIdSet.empty;
    used_chainings = Mast.ChainingSet.empty;
    verif_seen = false;
  }

let wrap_m_code_call (m_program : Mir_interface.full_program)
    (order : Mir_interface.chain_order) (ctx : translation_ctx) :
    translation_ctx * Bir.stmt list =
  let m_program =
    {
      m_program with
      program =
        Bir_interpreter.FloatDefInterp.replace_undefined_with_input_variables
          m_program.program ctx.variables_used_as_inputs;
    }
  in
  let program_stmts =
    List.fold_left
      (fun stmts rov_id ->
        let rule = Mir.RuleMap.find rov_id m_program.program.program_rules in
        Pos.same_pos_as (Bir.SRovCall rov_id) rule.Mir.rule_number :: stmts)
      [] order.execution_order
  in
  let program_stmts = List.rev program_stmts in
  (ctx, program_stmts)

let generate_verif_cond (cond : Mir.condition_data) : Bir.stmt =
  let data = Mir.map_cond_data_var Bir.(var_from_mir default_tgv) cond in
  (Bir.SVerif data, Pos.get_position data.cond_expr)

let generate_verifs_prog (m_program : Mir_interface.full_program)
    (dom : Mast.DomainId.t) (expr : Mir.expression Pos.marked) =
  Mir_typechecker.typecheck_top_down ~in_generic_table:false expr;
  let my_floor a = floor (a +. 0.000001) in
  let _my_ceil a = ceil (a -. 0.000001) in
  let my_arr a =
    let my_var1 = floor a in
    let my_var2 = ((a -. my_var1) *. 100000.0) +. 0.5 in
    let my_var2 = floor my_var2 /. 100000.0 in
    let my_var2 = my_var1 +. my_var2 +. 0.5 in
    floor my_var2
  in
  let to_filter (expr : Mir.expression Pos.marked) cond =
    let rec aux env (expr : Mir.expression Pos.marked) =
      match Pos.unmark expr with
      | Mir.Literal l -> l
      | Mir.Unop (op, e0) -> begin
          match aux env e0 with
          | Mir.Undefined -> Mir.Undefined
          | Mir.Float f -> begin
              match op with
              | Mast.Not -> Mir.Float (1.0 -. f)
              | Mast.Minus -> Mir.Float ~-.f
            end
        end
      | Mir.FunctionCall (func, args) -> begin
          let rl = List.map (aux env) args in
          match func with
          | Mir.VerifNumber -> begin
              match Pos.unmark cond.Mir.cond_number with
              | Mir.VerifID id -> Mir.Float (float_of_int id)
              | _ -> assert false
            end
          | Mir.ComplNumber -> assert false
          | Mir.SumFunc ->
              List.fold_left
                (fun res r ->
                  match r with
                  | Mir.Undefined -> res
                  | Mir.Float f -> begin
                      match res with
                      | Mir.Undefined -> r
                      | Mir.Float fr -> Mir.Float (f +. fr)
                    end)
                Mir.Undefined rl
          | Mir.AbsFunc -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] -> Mir.Float (abs_float f)
              | _ -> assert false
            end
          | Mir.MinFunc -> begin
              match rl with
              | [ Mir.Undefined; Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Undefined; r ] | [ r; Mir.Undefined ] -> r
              | [ Mir.Float f0; Mir.Float f1 ] -> Mir.Float (min f0 f1)
              | _ -> assert false
            end
          | Mir.MaxFunc -> begin
              match rl with
              | [ Mir.Undefined; Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Undefined; r ] | [ r; Mir.Undefined ] -> r
              | [ Mir.Float f0; Mir.Float f1 ] -> Mir.Float (max f0 f1)
              | _ -> assert false
            end
          | Mir.GtzFunc -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] -> Mir.Float (if f > 0.0 then 1.0 else 0.0)
              | _ -> assert false
            end
          | Mir.GtezFunc -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] -> Mir.Float (if f >= 0.0 then 1.0 else 0.0)
              | _ -> assert false
            end
          | Mir.NullFunc -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] -> Mir.Float (if f = 0.0 then 1.0 else 0.0)
              | _ -> assert false
            end
          | Mir.ArrFunc -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] -> Mir.Float (my_arr f)
              | _ -> assert false
            end
          | Mir.InfFunc -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] -> Mir.Float (my_floor f)
              | _ -> assert false
            end
          | Mir.PresentFunc ->
              Errors.raise_spanned_error
                "function present is forbidden in verification filter"
                (Pos.get_position expr)
          | Mir.Multimax ->
              Errors.raise_spanned_error
                "function multimax is forbidden in verification filter"
                (Pos.get_position expr)
          | Mir.Supzero -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] when f = 0.0 -> Mir.Undefined
              | [ r ] -> r
              | _ -> assert false
            end
        end
      | Mir.Comparison (op, e0, e1) -> begin
          match (aux env e0, aux env e1) with
          | Mir.Undefined, _ | _, Mir.Undefined -> Mir.Undefined
          | Mir.Float f0, Mir.Float f1 -> begin
              match Pos.unmark op with
              | Mast.Gt -> Mir.Float (if f0 > f1 then 1.0 else 0.0)
              | Mast.Gte -> Mir.Float (if f0 >= f1 then 1.0 else 0.0)
              | Mast.Lt -> Mir.Float (if f0 < f1 then 1.0 else 0.0)
              | Mast.Lte -> Mir.Float (if f0 <= f1 then 1.0 else 0.0)
              | Mast.Eq -> Mir.Float (if f0 = f1 then 1.0 else 0.0)
              | Mast.Neq -> Mir.Float (if f0 <> f1 then 1.0 else 0.0)
            end
        end
      | Mir.Binop (op, e0, e1) -> begin
          let r0 = aux env e0 in
          let r1 = aux env e1 in
          match Pos.unmark op with
          | And -> begin
              match r0 with
              | Mir.Undefined -> Mir.Undefined
              | Mir.Float f0 -> if f0 = 0.0 then r0 else r1
            end
          | Or -> begin
              match r0 with
              | Mir.Undefined -> r1
              | Mir.Float f0 -> if f0 = 0.0 then r1 else r0
            end
          | Add -> begin
              match (r0, r1) with
              | Mir.Undefined, Mir.Undefined -> Mir.Undefined
              | Mir.Undefined, Mir.Float _ -> r1
              | Mir.Float _, Mir.Undefined -> r0
              | Mir.Float f0, Mir.Float f1 -> Mir.Float (f0 +. f1)
            end
          | Sub -> begin
              match (r0, r1) with
              | Mir.Undefined, Mir.Undefined -> Mir.Undefined
              | Mir.Undefined, Mir.Float _ -> r1
              | Mir.Float _, Mir.Undefined -> r0
              | Mir.Float f0, Mir.Float f1 -> Mir.Float (f0 +. f1)
            end
          | Mul -> begin
              match (r0, r1) with
              | Mir.Undefined, _ | _, Mir.Undefined -> Mir.Undefined
              | Mir.Float f0, Mir.Float f1 -> Mir.Float (f0 *. f1)
            end
          | Div -> begin
              match (r0, r1) with
              | Mir.Undefined, _ | _, Mir.Undefined -> Mir.Undefined
              | Mir.Float f0, Mir.Float f1 ->
                  if f1 = 0.0 then r1 else Mir.Float (f0 /. f1)
            end
        end
      | Mir.Conditional (e0, e1, e2) -> begin
          let r0 = aux env e0 in
          let r1 = aux env e1 in
          let r2 = aux env e2 in
          match r0 with
          | Mir.Undefined -> Mir.Undefined
          | Mir.Float f -> if f = 1.0 then r1 else r2
        end
      | Mir.Var v | Mir.Index ((v, _), _) ->
          Errors.raise_spanned_error
            "variables are forbidden in verification filter"
            (Pos.get_position v.Mir.name)
      | Mir.LocalVar lv -> begin
          match IntMap.find_opt lv.Mir.id env with
          | Some r -> r
          | None -> assert false
        end
      | Mir.LocalLet (lv, e0, e1) ->
          let r0 = aux env e0 in
          let env0 = IntMap.add lv.Mir.id r0 env in
          aux env0 e1
      | Mir.Error ->
          Errors.raise_spanned_error
            "errors are forbidden in verification filter"
            (Pos.get_position expr)
      | Mir.NbCategory cats ->
          let nb =
            Mir.fold_expr_var
              (fun res v ->
                match v.Mir.cats with
                | Some c when Mir.CatVarSet.mem c cats -> res +. 1.0
                | _ -> res)
              0.0
              (Pos.unmark cond.Mir.cond_expr)
          in
          Mir.Float nb
      | Mir.NbError ->
          Errors.raise_spanned_error
            "nb_erreur is forbidden in verification filter"
            (Pos.get_position expr)
      | Mir.Attribut _ | Mir.Size _ -> assert false
    in
    aux IntMap.empty expr
  in
  let is_verif_relevant rov_id cond =
    let is_verif = match rov_id with Mir.VerifID _ -> true | _ -> false in
    let verif_domain = cond.Mir.cond_domain in
    let is_max = Mast.DomainIdSet.mem dom verif_domain.dom_max in
    let is_eq = Pos.unmark verif_domain.dom_id = dom in
    let is_var_compatible =
      (* !!! à valider en amont *)
      Mir.CatVarSet.subset
        (Mir.cond_cats_to_set cond.Mir.cond_cats)
        verif_domain.dom_data.vdom_auth
    in
    let is_kept = to_filter expr cond = Mir.Float 1.0 in
    is_verif && (is_max || is_eq) && is_var_compatible && is_kept
  in
  m_program.program.program_conds
  |> Mir.RuleMap.filter is_verif_relevant
  |> Mir.RuleMap.bindings
  |> List.sort (fun (_, cond1) (_, cond2) ->
         let res =
           Mast.compare_error_type (fst cond1.Mir.cond_error).typ
             (fst cond2.Mir.cond_error).typ
         in
         if res <> 0 then res
         else Stdlib.compare cond1.Mir.cond_seq_id cond2.Mir.cond_seq_id)
  |> List.map (fun (rov_id, cond) ->
         (Bir.SRovCall rov_id, Pos.get_position cond.Mir.cond_number))

let rec translate_m_code (m_program : Mir_interface.full_program)
    (ctx : translation_ctx) (instrs : Mir.instruction Pos.marked list) =
  let rec aux ctx res = function
    | [] -> (ctx, List.rev res)
    | (Mir.Affectation (vid, vdef), pos) :: instrs -> (
        try
          let var = Mir.VariableDict.find vid m_program.program.program_vars in
          let var_definition =
            Mir.map_var_def_var
              Bir.(var_from_mir default_tgv)
              vdef.Mir.var_definition
          in
          match var_definition with
          | InputVar -> aux ctx res instrs
          | TableVar _ | SimpleVar _ ->
              aux ctx
                (( Bir.SAssign
                     (Bir.(var_from_mir default_tgv) var, var_definition),
                   var.Mir.Variable.execution_number.pos )
                :: res)
                instrs
        with Not_found ->
          Errors.raise_spanned_error
            (Format.sprintf "unknown variable id %d" vid)
            pos)
    | (Mir.IfThenElse (e, ilt, ile), pos) :: instrs ->
        let expr = Mir.map_expr_var Bir.(var_from_mir default_tgv) e in
        let ctx, stmts_then = translate_m_code m_program ctx ilt in
        let ctx, stmts_else = translate_m_code m_program ctx ile in
        aux ctx
          ((Bir.SConditional (expr, stmts_then, stmts_else), pos) :: res)
          instrs
    | (Mir.ComputeDomain l, _pos) :: instrs ->
        let dom = Mast.DomainId.from_marked_list (Pos.unmark l) in
        let order =
          match Mast.DomainIdMap.find_opt dom m_program.domains_orders with
          | Some order -> order
          | None ->
              Errors.raise_spanned_error
                (Format.asprintf "Unknown rule domain: %a" (Mast.DomainId.pp ())
                   dom)
                (Pos.get_position l)
        in
        let ctx =
          {
            ctx with
            used_rule_domains = Mast.DomainIdSet.add dom ctx.used_rule_domains;
          }
        in
        let ctx, stmts = wrap_m_code_call m_program order ctx in
        aux ctx (List.rev stmts @ res) instrs
    | (Mir.ComputeChaining ch, _pos) :: instrs ->
        let chain = Pos.unmark ch in
        let order =
          match Mast.ChainingMap.find_opt chain m_program.chainings_orders with
          | Some order -> order
          | None ->
              Errors.raise_spanned_error
                (Format.sprintf "Unknown chaining: %s" chain)
                (Pos.get_position ch)
        in
        let ctx =
          {
            ctx with
            used_chainings = Mast.ChainingSet.add chain ctx.used_chainings;
          }
        in
        let ctx, stmts = wrap_m_code_call m_program order ctx in
        aux ctx (List.rev stmts @ res) instrs
    | (Mir.ComputeTarget tn, pos) :: instrs ->
        let name = Pos.unmark tn in
        let ctx, stmt =
          match
            Mir.TargetMap.find_opt name m_program.program.program_targets
          with
          | Some _ -> (ctx, (Bir.SFunctionCall (name, []), pos))
          | None ->
              Errors.raise_spanned_error
                (Format.asprintf "Unknown target: %s" name)
                (Pos.get_position tn)
        in
        aux ctx (stmt :: res) instrs
    | (Mir.ComputeVerifs (l, expr), pos) :: instrs ->
        let dom = Mast.DomainId.from_marked_list (Pos.unmark l) in
        let ctx = { ctx with verif_seen = true } in
        let stmts = generate_verifs_prog m_program dom expr in
        let stmt = (Bir.SVerifBlock stmts, pos) in
        aux ctx (stmt :: res) instrs
    | (Mir.VerifBlock stmts, pos) :: instrs ->
        let ctx, stmts' = translate_m_code m_program ctx stmts in
        aux ctx ((Bir.SVerifBlock stmts', pos) :: res) instrs
    | (Mir.Print (std, args), pos) :: instrs ->
        let bir_args =
          List.rev
            (List.fold_left
               (fun res arg ->
                 let bir_arg =
                   match Pos.unmark arg with
                   | Mir.PrintString s -> Mir.PrintString s
                   | Mir.PrintName (v, vid) -> Mir.PrintName (v, vid)
                   | Mir.PrintAlias (v, vid) -> Mir.PrintAlias (v, vid)
                   | Mir.PrintExpr (e, min, max) ->
                       Mir.PrintExpr
                         ( Pos.same_pos_as
                             (Mir.map_expr_var
                                Bir.(var_from_mir default_tgv)
                                (Pos.unmark e))
                             e,
                           min,
                           max )
                 in
                 bir_arg :: res)
               [] args)
        in
        aux ctx ((Bir.SPrint (std, bir_args), pos) :: res) instrs
    | (Mir.Iterate (v, vcs, e, iit), pos) :: instrs ->
        let var =
          Bir.(var_from_mir default_tgv)
            (Mir.VariableDict.find v m_program.program.program_vars)
        in
        let expr =
          Mir.map_expr_var Bir.(var_from_mir default_tgv) (Pos.unmark e)
        in
        let ctx, stmts = translate_m_code m_program ctx iit in
        aux ctx ((Bir.SIterate (var, vcs, expr, stmts), pos) :: res) instrs
    | (Mir.Restore (vars, var_params, irest), pos) :: instrs ->
        let vars =
          Mir.VariableMap.fold
            (fun v _ vars ->
              let var =
                Bir.(var_from_mir default_tgv)
                  (Mir.VariableDict.find v.Mir.id m_program.program.program_vars)
              in
              Bir.VariableSet.add var vars)
            vars Bir.VariableSet.empty
        in
        let var_params =
          List.fold_left
            (fun var_params ((v : Mir.variable), vcs, expr) ->
              let var =
                Bir.(var_from_mir default_tgv)
                  (Mir.VariableDict.find v.Mir.id m_program.program.program_vars)
              in
              let expr =
                Mir.map_expr_var
                  Bir.(var_from_mir default_tgv)
                  (Pos.unmark expr)
              in
              (var, vcs, expr) :: var_params)
            [] var_params
        in
        let ctx, stmts = translate_m_code m_program ctx irest in
        aux ctx ((Bir.SRestore (vars, var_params, stmts), pos) :: res) instrs
    | (Mir.RaiseError (err, var_opt), pos) :: instrs ->
        aux ctx ((Bir.SRaiseError (err, var_opt), pos) :: res) instrs
    | (Mir.CleanErrors, pos) :: instrs ->
        aux ctx ((Bir.SCleanErrors, pos) :: res) instrs
  in
  aux ctx [] instrs

let create_combined_program (m_program : Mir_interface.full_program)
    (mpp_function_to_extract : string) : Bir.program =
  try
    let ctx = empty_translation_ctx in
    let ctx, targets =
      Mir.TargetMap.fold
        (fun n t (ctx, targets) ->
          let ctx, code = translate_m_code m_program ctx t.Mir.target_prog in
          ( ctx,
            Mir.TargetMap.add n
              Bir.
                {
                  file = t.Mir.target_file;
                  tmp_vars =
                    StrMap.map
                      (fun (var, pos, size) ->
                        (Bir.(var_from_mir default_tgv) var, pos, size))
                      t.Mir.target_tmp_vars;
                  stmts = code;
                  is_verif = true || ctx.verif_seen;
                }
              targets ))
        m_program.program.program_targets (ctx, Mir.TargetMap.empty)
    in
    let rules =
      Mir.RuleMap.fold
        (fun rov_id rule_data rules ->
          if
            let rule_domain = rule_data.Mir.rule_domain in
            let has_max =
              not
                (Mast.DomainIdSet.disjoint ctx.used_rule_domains
                   rule_domain.dom_max)
            in
            let has_used_domain =
              Mast.DomainIdSet.mem
                (Pos.unmark rule_domain.dom_id)
                ctx.used_rule_domains
            in
            let has_used_chaining =
              match rule_data.Mir.rule_chain with
              | None -> false
              | Some (ch, _) -> Mast.ChainingSet.mem ch ctx.used_chainings
            in
            let is_not_rule_0 =
              Pos.unmark rule_data.Mir.rule_number <> RuleID 0
            in
            is_not_rule_0 && (has_max || has_used_domain || has_used_chaining)
          then
            let rov_name =
              Pos.map_under_mark
                (fun n -> string_of_int (Mir.num_of_rule_or_verif_id n))
                rule_data.Mir.rule_number
            in
            let rov_code =
              Bir.Rule
                (snd (translate_m_code m_program ctx rule_data.Mir.rule_vars))
            in
            Mir.RuleMap.add rov_id Bir.{ rov_id; rov_name; rov_code } rules
          else rules)
        m_program.program.program_rules Mir.RuleMap.empty
    in
    let rules_and_verifs =
      Mir.RuleMap.fold
        (fun _ cond_data rules ->
          let rov_id = Pos.unmark cond_data.Mir.cond_number in
          let rov_name =
            Pos.same_pos_as
              (string_of_int (Mir.num_of_rule_or_verif_id rov_id))
              cond_data.Mir.cond_number
          in
          let rov_code = Bir.Verif (generate_verif_cond cond_data) in
          Mir.RuleMap.add rov_id Bir.{ rov_id; rov_name; rov_code } rules)
        m_program.program.program_conds rules
    in
    if not (Mir.TargetMap.mem mpp_function_to_extract targets) then
      Errors.raise_error
        (Format.asprintf "M++ function %s not found in M file!"
           mpp_function_to_extract);
    {
      targets;
      rules_and_verifs;
      mpp_functions = Bir.FunctionMap.empty;
      main_function = mpp_function_to_extract;
      context = None;
      idmap = m_program.program.program_idmap;
      mir_program = m_program.program;
      outputs = Bir.VariableMap.empty;
    }
  with Bir_interpreter.FloatDefInterp.RuntimeError (r, ctx) ->
    Bir_interpreter.FloatDefInterp.raise_runtime_as_structured r ctx
      m_program.program
