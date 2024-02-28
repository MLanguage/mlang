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
    | (Mir.ComputeVerifs (_l, _expr), _pos) :: instrs -> aux ctx res instrs
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
                   | Mir.PrintIndent e ->
                       Mir.PrintIndent
                         (Pos.same_pos_as
                            (Mir.map_expr_var
                               Bir.(var_from_mir default_tgv)
                               (Pos.unmark e))
                            e)
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
    | (Mir.ExportErrors, pos) :: instrs ->
        aux ctx ((Bir.SExportErrors, pos) :: res) instrs
    | (Mir.FinalizeErrors, pos) :: instrs ->
        aux ctx ((Bir.SFinalizeErrors, pos) :: res) instrs
  in
  aux ctx [] instrs

let create_combined_program (m_program : Mir_interface.full_program)
    (mpp_function_to_extract : string) : Bir.program =
  try
    let ctx = empty_translation_ctx in
    let _ctx, targets =
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
    if not (Mir.TargetMap.mem mpp_function_to_extract targets) then
      Errors.raise_error
        (Format.asprintf "M++ function %s not found in M file!"
           mpp_function_to_extract);
    {
      targets;
      rules_and_verifs = Bir.ROVMap.empty;
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
