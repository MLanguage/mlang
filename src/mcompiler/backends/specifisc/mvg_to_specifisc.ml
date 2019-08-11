(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

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

open Verifisc_utils

type translated_var =
  | Bool of Specifisc.Ast.BoolVariable.t
  | Int of Specifisc.Ast.IntVariable.t

type ctx = {
  ctx_typ_info: Typechecker.typ_info;
  ctx_var_mapping: translated_var Mvg.VariableMap.t;
  ctx_local_var_mapping: translated_var Mvg.LocalVariableMap.t;
}

let empty_ctx (typing : Typechecker.typ_info): ctx = {
  ctx_typ_info = typing;
  ctx_var_mapping = Mvg.VariableMap.empty;
  ctx_local_var_mapping = Mvg.LocalVariableMap.empty;
}

let switch_div_mul (op: Ast.binop) : Specifisc.Ast.arithmetic_binop = match op with
  | Ast.Div -> Specifisc.Ast.Mul
  | Ast.Mul -> Specifisc.Ast.Div
  | _ -> assert false (* should not happen *)


let rec translate_logical_expression
    (e: Mvg.expression Pos.marked)
    (ctx: ctx)
  : Specifisc.Ast.logical_expression Pos.marked * Specifisc.Ast.command list * ctx =
  match Pos.unmark e with
  | Mvg.Unop (Ast.Not, e1) ->
    let se, conds, ctx = translate_logical_expression e1 ctx in
    (Pos.same_pos_as (Specifisc.Ast.LogicalNot se) e, conds, ctx)
  | Mvg.Binop ((Ast.And | Ast.Or as op, pos), e1, e2) ->
    let se1, conds1, ctx = translate_logical_expression e1 ctx in
    let se2, conds2, ctx = translate_logical_expression e2 ctx in
    let op = match op with
      | Ast.And -> Specifisc.Ast.And
      | Ast.Or -> Specifisc.Ast.Or
      | _ -> assert false (* should not happen*)
    in
    (Pos.same_pos_as (Specifisc.Ast.LogicalBinop ((op, pos), se1, se2)) e,
     conds2@conds1, ctx)
  | Mvg.Comparison (op, e1, e2) ->
    let se1, conds1, ctx = translate_arithmetic_expression e1 ctx in
    let se2, conds2, ctx = translate_arithmetic_expression e2 ctx in
    let sop = match Pos.unmark op with
      | Ast.Gt -> Specifisc.Ast.Gt
      | Ast.Gte -> Specifisc.Ast.Gte
      | Ast.Lt -> Specifisc.Ast.Lt
      | Ast.Lte -> Specifisc.Ast.Lte
      | Ast.Neq -> Specifisc.Ast.Neq
      | Ast.Eq -> Specifisc.Ast.Eq
    in
    (Pos.same_pos_as (Specifisc.Ast.Comparison (Pos.same_pos_as sop op , se1, se2)) e,
     conds2@conds1, ctx)
  | Mvg.Literal (Mvg.Bool b) -> (Pos.same_pos_as (Specifisc.Ast.BoolLiteral b) e, [], ctx)
  | Mvg.Var var ->
    begin match Mvg.VariableMap.find var ctx.ctx_var_mapping with
      | Bool bool_var -> (Pos.same_pos_as (Specifisc.Ast.BoolVar bool_var) e, [], ctx)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalVar lvar ->
    begin match Mvg.LocalVariableMap.find lvar ctx.ctx_local_var_mapping with
      | Bool bool_var -> (Pos.same_pos_as (Specifisc.Ast.BoolVar bool_var) e, [], ctx)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalLet (lvar, e1, e2) ->
    if Mvg.LocalVariableMap.find lvar ctx.ctx_typ_info.typ_info_local_var <> Mvg.Boolean then
      assert false (* should not happen *);
    let se1, conds1, ctx = translate_logical_expression e1 ctx in
    let bool_var = Specifisc.Ast.BoolVariable.new_var
        (Pos.same_pos_as ("t" ^ (string_of_int lvar.Mvg.LocalVariable.id)) e)
        (Pos.same_pos_as ("Local variable") e)
    in
    let ctx =
      { ctx with
        ctx_local_var_mapping = Mvg.LocalVariableMap.add lvar (Bool bool_var) ctx.ctx_local_var_mapping;
      } in
    let se2, conds2, ctx = translate_logical_expression e2 ctx in
    (se2, conds2@[Specifisc.Ast.BoolDef (bool_var, se1)]@conds1, ctx)
  | _ ->
    raise
      (Errors.UnsupportedBySpecifisc
         (Printf.sprintf "boolean expression %s %s"
            (Format_mvg.format_expression (Pos.unmark e))
            (Pos.format_position (Pos.get_position e))
         )
      )

and translate_arithmetic_expression
    (e: Mvg.expression Pos.marked)
    (ctx: ctx)
  : Specifisc.Ast.arithmetic_expression Pos.marked * Specifisc.Ast.command list * ctx =
  match Pos.unmark e with
  | Mvg.Unop (Ast.Minus, e1) ->
    let se, conds, ctx = translate_arithmetic_expression e1 ctx in
    (Pos.same_pos_as (Specifisc.Ast.ArithmeticMinus se) e, conds, ctx)
  | Mvg.Binop ((Ast.Add | Ast.Sub | Ast.Div | Ast.Mul as op, pos), e1, e2) ->
    let se1, conds1, ctx = translate_arithmetic_expression e1 ctx in
    let se2, conds2, ctx = translate_arithmetic_expression e2 ctx in
    let op = match op with
      | Ast.Add -> Specifisc.Ast.Add
      | Ast.Sub -> Specifisc.Ast.Sub
      | Ast.Div -> Specifisc.Ast.Div
      | Ast.Mul -> Specifisc.Ast.Mul
      | _ -> assert false (* should not happen*)
    in
    let se = Specifisc.Ast.ArithmeticBinop ((op, pos), se1, se2) in
    (Pos.same_pos_as (match op with
         (* so in Mvg everything is a real which we have to translate to fixed precisions integers
            in Ast. The precision is set using [!Cli.real_precision]. However,
            when performing multiplications or divisions, we have to offset this scaling factor.
            This is done with the cases below
         *)
         | Specifisc.Ast.Mul -> Specifisc.Ast.ArithmeticBinop (
             Pos.same_pos_as Specifisc.Ast.Div e,
             Pos.same_pos_as se e,
             Pos.same_pos_as (Specifisc.Ast.IntLiteral (Int64.of_int !Cli.real_precision)) e
           )
         | Specifisc.Ast.Div ->
           Specifisc.Ast.ArithmeticBinop (
             Pos.same_pos_as Specifisc.Ast.Mul e,
             Pos.same_pos_as se e,
             Pos.same_pos_as (Specifisc.Ast.IntLiteral (Int64.of_int !Cli.real_precision)) e
           )
         | Specifisc.Ast.Add | Specifisc.Ast.Sub -> se
       ) e,
     conds2@conds1, ctx)
  | Mvg.Conditional (e1, e2, e3) ->
    let se1, conds1, ctx = translate_logical_expression e1 ctx in
    let se2, conds2, ctx = translate_arithmetic_expression e2 ctx in
    let se3, conds3, ctx = translate_arithmetic_expression e3 ctx in
    (Pos.same_pos_as (Specifisc.Ast.Conditional (se1 , se2, se3)) e,
     conds3@conds2@conds1, ctx)
  | Mvg.Literal (Mvg.Int i) ->
    (Pos.same_pos_as (
        Specifisc.Ast.IntLiteral (Int64.of_int (i * !Cli.real_precision))
      ) e, [], ctx)
  | Mvg.Literal (Mvg.Float f) ->
    if f < (1.0 /. (float_of_int !Cli.real_precision)) then
      Cli.warning_print
        (Printf.sprintf "Float constant too small for choosen fixed precision (%f) : %f %s"
           (1.0 /. (float_of_int !Cli.real_precision))
           f
           (Pos.format_position (Pos.get_position e))
        );
    (Pos.same_pos_as (
        Specifisc.Ast.IntLiteral (Int64.of_float (f *. (float_of_int !Cli.real_precision)))
      ) e, [], ctx)
  | Mvg.Var var ->
    begin match Mvg.VariableMap.find var ctx.ctx_var_mapping with
      | Int int_var -> (Pos.same_pos_as (Specifisc.Ast.IntVar int_var) e, [], ctx)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalVar lvar ->
    begin match Mvg.LocalVariableMap.find lvar ctx.ctx_local_var_mapping with
      | Int int_var -> (Pos.same_pos_as (Specifisc.Ast.IntVar int_var) e, [], ctx)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalLet (lvar, e1, e2) ->
    if Mvg.LocalVariableMap.find lvar ctx.ctx_typ_info.typ_info_local_var = Mvg.Boolean then
      assert false; (* should not happen *)
    let se1, conds1, ctx =  translate_arithmetic_expression e1 ctx in
    let int_var = Specifisc.Ast.IntVariable.new_var
        (Pos.same_pos_as ("t" ^ (string_of_int lvar.Mvg.LocalVariable.id)) e)
        (Pos.same_pos_as ("Local variable") e)
    in
    let ctx =
      { ctx with
        ctx_local_var_mapping = Mvg.LocalVariableMap.add lvar (Int int_var) ctx.ctx_local_var_mapping;
      } in
    let se2, conds2, ctx = translate_arithmetic_expression e2 ctx in
    (se2, conds2@[Specifisc.Ast.IntDef (int_var, se1)]@conds1, ctx)
  | Mvg.FunctionCall (Mvg.ArrFunc, [arg]) ->
    let sarg, conds, ctx = translate_arithmetic_expression arg ctx in
    let e' =
      Specifisc.Ast.ArithmeticBinop (
        Pos.same_pos_as Specifisc.Ast.Mul e,
        Pos.same_pos_as (Specifisc.Ast.ArithmeticBinop (
            Pos.same_pos_as Specifisc.Ast.Div e,
            Pos.same_pos_as (Specifisc.Ast.ArithmeticBinop (
                Pos.same_pos_as Specifisc.Ast.Add e,
                sarg,
                Pos.same_pos_as (Specifisc.Ast.IntLiteral (Int64.of_int (!Cli.real_precision / 2))) e
              )) e,
            Pos.same_pos_as (Specifisc.Ast.IntLiteral (Int64.of_int !Cli.real_precision)) e
          )) e,
        Pos.same_pos_as (Specifisc.Ast.IntLiteral (Int64.of_int !Cli.real_precision)) e
      )
    in
    (Pos.same_pos_as e' e, conds, ctx)
  | Mvg.FunctionCall (Mvg.InfFunc, [arg]) ->
    let sarg, conds, ctx = translate_arithmetic_expression arg ctx in
    let e' =
      Specifisc.Ast.ArithmeticBinop (
        Pos.same_pos_as Specifisc.Ast.Mul e,
        Pos.same_pos_as (Specifisc.Ast.ArithmeticBinop (
            Pos.same_pos_as Specifisc.Ast.Div e,
            sarg,
            Pos.same_pos_as (Specifisc.Ast.IntLiteral (Int64.of_int !Cli.real_precision)) e
          )) e,
        Pos.same_pos_as (Specifisc.Ast.IntLiteral (Int64.of_int !Cli.real_precision)) e
      )
    in
    (Pos.same_pos_as e' e, conds, ctx)
  | _ ->
    raise
      (Errors.UnsupportedBySpecifisc
         (Printf.sprintf "arithmetic expression %s %s"
            (Format_mvg.format_expression (Pos.unmark e))
            (Pos.format_position (Pos.get_position e))
         )
      )

let translate_variable_data
    (var: Mvg.Variable.t)
    (data: Mvg.variable_data)
    (ctx: ctx)
  : Specifisc.Ast.command list * ctx =
  match data.Mvg.var_definition with
  | InputVar -> [], ctx
  | TableVar (_, def) ->
    raise
      (Errors.UnsupportedBySpecifisc
         (Printf.sprintf "table variable %s" (Pos.format_position (match def with
              | IndexGeneric e -> Pos.get_position e
              | IndexTable es -> Pos.get_position (snd (Mvg.IndexMap.choose es))
            ))
         )
      )
  | SimpleVar e ->
    begin match begin try Mvg.VariableMap.find var ctx.ctx_typ_info.Typechecker.typ_info_var with
      | Not_found -> assert false end with
    | (Mvg.Boolean, _) ->
      let bool_var = Specifisc.Ast.BoolVariable.new_var (var.Mvg.Variable.name) (var.Mvg.Variable.descr) in
      let se, conds, ctx = translate_logical_expression e ctx in
      let new_cmds = (Specifisc.Ast.BoolDef (bool_var, se))::conds in
      (new_cmds,
       { ctx with
         ctx_var_mapping = Mvg.VariableMap.add var (Bool bool_var) ctx.ctx_var_mapping
       })
    | ((Mvg.Integer | Mvg.Real), _) ->
      let int_var = Specifisc.Ast.IntVariable.new_var (var.Mvg.Variable.name) (var.Mvg.Variable.descr) in
      let se, conds, ctx = translate_arithmetic_expression e ctx in
      let new_cmds = (Specifisc.Ast.IntDef (int_var, se))::conds in
      (new_cmds,
       { ctx with
         ctx_var_mapping = Mvg.VariableMap.add var (Int int_var) ctx.ctx_var_mapping
       })
    end

let translate_cond
    (cond: Mvg.condition_data)
    (ctx: ctx)
  : Specifisc.Ast.command list * ctx =
  let se, conds, ctx = translate_logical_expression cond.cond_expr ctx in
  let new_cmds = (Specifisc.Ast.Constraint se)::conds in
  (new_cmds, ctx )

let translate_program (program: Mvg.program) (typing : Typechecker.typ_info) : Specifisc.Ast.program =
  let exec_order = Execution_order.get_execution_order program in
  let ctx = empty_ctx typing in
  (** We have to populate the context with the input variables and fetch the io variables *)
  let ctx, input_vars =
    Mvg.VariableMap.fold (fun var data (ctx, (int_inputs, bool_inputs)) ->
        match data.Mvg.var_io with
        | Mvg.Input ->
          let typ = Mvg.VariableMap.find var typing.Typechecker.typ_info_var in
          begin match fst typ with
            | Mvg.Real | Mvg.Integer ->
              let int_var = Specifisc.Ast.IntVariable.new_var (var.Mvg.Variable.name) (var.Mvg.Variable.descr) in
              ({ ctx with ctx_var_mapping = Mvg.VariableMap.add var (Int int_var) ctx.ctx_var_mapping },
               (int_var::int_inputs, bool_inputs))
            | Mvg.Boolean ->
              let bool_var = Specifisc.Ast.BoolVariable.new_var (var.Mvg.Variable.name) (var.Mvg.Variable.descr) in
              ({ ctx with ctx_var_mapping = Mvg.VariableMap.add var (Bool bool_var) ctx.ctx_var_mapping },
               (int_inputs, bool_var::bool_inputs))
          end
        | _ -> (ctx, (int_inputs, bool_inputs))
      ) program.program_vars (ctx, ([], []))
  in
  (* Then the main translation *)
  let func_body, ctx = List.fold_left (fun (cmds, ctx) scc  ->
      if Mvg.VariableMap.cardinal scc > 1 then
        raise (Errors.UnsupportedBySpecifisc
                 (Printf.sprintf "circular variable dependencies (%s)"
                    (String.concat "," (List.map (fun (var, _) ->
                         Pos.unmark var.Mvg.Variable.name) (Mvg.VariableMap.bindings scc)))))
      else
        Mvg.VariableMap.fold (fun var () (cmds, ctx) ->
            try let data = Mvg.VariableMap.find var program.program_vars in
              let new_cmds, ctx =  translate_variable_data var data ctx in
              (new_cmds@cmds, ctx)
            with
            | Not_found ->
              try
                let cond = Mvg.VariableMap.find var program.program_conds in
                let new_cmds, ctx = translate_cond cond ctx in
                (new_cmds@cmds, ctx)
              with
              | Not_found -> assert false
          )
          scc (cmds, ctx)) ([], ctx ) exec_order
  in
  let func_body = List.rev func_body in
  let output_vars =
    Mvg.VariableMap.fold (fun var data (int_outputs, bool_outputs) ->
        match data.Mvg.var_io with
        | Mvg.Output ->
          begin match Mvg.VariableMap.find var ctx.ctx_var_mapping with
            | Int int_var ->
              (int_var::int_outputs, bool_outputs)
            | Bool bool_var ->
              (int_outputs, bool_var::bool_outputs)
          end
        | _ -> (int_outputs, bool_outputs)
      ) program.program_vars ([], [])
  in
  let func_id = Specifisc.Ast.FunctionVariable.new_var ("Whole program", Pos.no_pos) ("", Pos.no_pos) in
  {
    Specifisc.Ast.program_functions = Specifisc.Ast.FunctionVariableMap.singleton func_id {
        Specifisc.Ast.body = func_body;
        Specifisc.Ast.inputs = input_vars;
        Specifisc.Ast.outputs = output_vars;
      };
    Specifisc.Ast.program_mult_factor = !Cli.real_precision;
    Specifisc.Ast.program_idmap = Pos.VarNameToID.fold (fun name l new_idmap ->
        try
          Pos.VarNameToID.add name (List.map (fun var ->
              match Mvg.VariableMap.find var ctx.ctx_var_mapping with
              | Bool bool_var ->
                Specifisc.Ast.IDBoolVar bool_var
              | Int int_var ->
                Specifisc.Ast.IDIntVar int_var

            ) l) new_idmap
        with
        | Not_found -> new_idmap
      ) program.Mvg.program_idmap Pos.VarNameToID.empty
  }
