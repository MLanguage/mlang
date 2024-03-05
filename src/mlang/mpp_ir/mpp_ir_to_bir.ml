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

let rec translate_m_code (m_program : Mir.program)
    (instrs : Mir.instruction Pos.marked list) =
  let rec aux res = function
    | [] -> List.rev res
    | (Mir.Affectation (vid, vdef), pos) :: instrs -> (
        try
          let var = Mir.VariableDict.find vid m_program.program_vars in
          let var_definition =
            Mir.map_var_def_var
              Bir.(var_from_mir default_tgv)
              vdef.Mir.var_definition
          in
          match var_definition with
          | InputVar -> aux res instrs
          | TableVar _ | SimpleVar _ ->
              aux
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
        let stmts_then = translate_m_code m_program ilt in
        let stmts_else = translate_m_code m_program ile in
        aux
          ((Bir.SConditional (expr, stmts_then, stmts_else), pos) :: res)
          instrs
    | (Mir.ComputeTarget tn, pos) :: instrs ->
        let name = Pos.unmark tn in
        let stmt =
          match Mir.TargetMap.find_opt name m_program.program_targets with
          | Some _ -> (Bir.SFunctionCall (name, []), pos)
          | None ->
              Errors.raise_spanned_error
                (Format.asprintf "Unknown target: %s" name)
                (Pos.get_position tn)
        in
        aux (stmt :: res) instrs
    | (Mir.VerifBlock stmts, pos) :: instrs ->
        let stmts' = translate_m_code m_program stmts in
        aux ((Bir.SVerifBlock stmts', pos) :: res) instrs
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
        aux ((Bir.SPrint (std, bir_args), pos) :: res) instrs
    | (Mir.Iterate (v, vcs, e, iit), pos) :: instrs ->
        let var =
          Bir.(var_from_mir default_tgv)
            (Mir.VariableDict.find v m_program.program_vars)
        in
        let expr =
          Mir.map_expr_var Bir.(var_from_mir default_tgv) (Pos.unmark e)
        in
        let stmts = translate_m_code m_program iit in
        aux ((Bir.SIterate (var, vcs, expr, stmts), pos) :: res) instrs
    | (Mir.Restore (vars, var_params, irest), pos) :: instrs ->
        let vars =
          Mir.VariableMap.fold
            (fun v _ vars ->
              let var =
                Bir.(var_from_mir default_tgv)
                  (Mir.VariableDict.find v.Mir.id m_program.program_vars)
              in
              Bir.VariableSet.add var vars)
            vars Bir.VariableSet.empty
        in
        let var_params =
          List.fold_left
            (fun var_params ((v : Mir.variable), vcs, expr) ->
              let var =
                Bir.(var_from_mir default_tgv)
                  (Mir.VariableDict.find v.Mir.id m_program.program_vars)
              in
              let expr =
                Mir.map_expr_var
                  Bir.(var_from_mir default_tgv)
                  (Pos.unmark expr)
              in
              (var, vcs, expr) :: var_params)
            [] var_params
        in
        let stmts = translate_m_code m_program irest in
        aux ((Bir.SRestore (vars, var_params, stmts), pos) :: res) instrs
    | (Mir.RaiseError (err, var_opt), pos) :: instrs ->
        aux ((Bir.SRaiseError (err, var_opt), pos) :: res) instrs
    | (Mir.CleanErrors, pos) :: instrs ->
        aux ((Bir.SCleanErrors, pos) :: res) instrs
    | (Mir.ExportErrors, pos) :: instrs ->
        aux ((Bir.SExportErrors, pos) :: res) instrs
    | (Mir.FinalizeErrors, pos) :: instrs ->
        aux ((Bir.SFinalizeErrors, pos) :: res) instrs
  in
  aux [] instrs

let create_combined_program (m_program : Mir.program)
    (mpp_function_to_extract : string) : Bir.program =
  try
    let targets =
      Mir.TargetMap.fold
        (fun n t targets ->
          let code = translate_m_code m_program t.Mir.target_prog in
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
                is_verif = true;
              }
            targets)
        m_program.program_targets Mir.TargetMap.empty
    in
    if not (Mir.TargetMap.mem mpp_function_to_extract targets) then
      Errors.raise_error
        (Format.asprintf "M target %s not found in M file!"
           mpp_function_to_extract);
    {
      targets;
      rules_and_verifs = Bir.ROVMap.empty;
      mpp_functions = Bir.FunctionMap.empty;
      main_function = mpp_function_to_extract;
      idmap = m_program.program_idmap;
      mir_program = m_program;
    }
  with Bir_interpreter.FloatDefInterp.RuntimeError (r, _ctx) ->
    Bir_interpreter.FloatDefInterp.raise_runtime_as_structured r
