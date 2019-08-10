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

type translated_var =
  | Bool of Specifisc.BoolVariable.t
  | Int of Specifisc.IntVariable.t

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

let mult_precision_factor_int_real = 100

let switch_div_mul (op: Ast.binop) : Specifisc.arithmetic_binop = match op with
  | Ast.Div -> Specifisc.Mul
  | Ast.Mul -> Specifisc.Div
  | _ -> assert false (* should not happen *)


let rec translate_logical_expression
    (e: Mvg.expression Ast.marked)
    (ctx: ctx)
  : Specifisc.logical_expression Ast.marked * Specifisc.command list * ctx =
  match Ast.unmark e with
  | Mvg.Unop (Ast.Not, e1) ->
    let se, conds, ctx = translate_logical_expression e1 ctx in
    (Ast.same_pos_as (Specifisc.LogicalNot se) e, conds, ctx)
  | Mvg.Binop ((Ast.And | Ast.Or as op, pos), e1, e2) ->
    let se1, conds1, ctx = translate_logical_expression e1 ctx in
    let se2, conds2, ctx = translate_logical_expression e2 ctx in
    let op = match op with
      | Ast.And -> Specifisc.And
      | Ast.Or -> Specifisc.Or
      | _ -> assert false (* should not happen*)
    in
    (Ast.same_pos_as (Specifisc.LogicalBinop ((op, pos), se1, se2)) e,
     conds2@conds1, ctx)
  | Mvg.Comparison (op, e1, e2) ->
    let se1, conds1, ctx = translate_arithmetic_expression e1 ctx in
    let se2, conds2, ctx = translate_arithmetic_expression e2 ctx in
    let sop = match Ast.unmark op with
      | Ast.Gt -> Specifisc.Gt
      | Ast.Gte -> Specifisc.Gte
      | Ast.Lt -> Specifisc.Lt
      | Ast.Lte -> Specifisc.Lte
      | Ast.Neq -> Specifisc.Neq
      | Ast.Eq -> Specifisc.Eq
    in
    (Ast.same_pos_as (Specifisc.Comparison (Ast.same_pos_as sop op , se1, se2)) e,
     conds2@conds1, ctx)
  | Mvg.Literal (Mvg.Bool b) -> (Ast.same_pos_as (Specifisc.BoolLiteral b) e, [], ctx)
  | Mvg.Var var ->
    begin match Mvg.VariableMap.find var ctx.ctx_var_mapping with
      | Bool bool_var -> (Ast.same_pos_as (Specifisc.BoolVar bool_var) e, [], ctx)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalVar lvar ->
    begin match Mvg.LocalVariableMap.find lvar ctx.ctx_local_var_mapping with
      | Bool bool_var -> (Ast.same_pos_as (Specifisc.BoolVar bool_var) e, [], ctx)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalLet (lvar, e1, e2) ->
    if Mvg.LocalVariableMap.find lvar ctx.ctx_typ_info.typ_info_local_var <> Mvg.Boolean then
      assert false (* should not happen *);
    let se1, conds1, ctx = translate_logical_expression e1 ctx in
    let bool_var = Specifisc.BoolVariable.new_var
        (Ast.same_pos_as ("t" ^ (string_of_int lvar.Mvg.LocalVariable.id)) e)
        (Ast.same_pos_as ("Local variable") e)
    in
    let ctx =
      { ctx with
        ctx_local_var_mapping = Mvg.LocalVariableMap.add lvar (Bool bool_var) ctx.ctx_local_var_mapping;
      } in
    let se2, conds2, ctx = translate_logical_expression e2 ctx in
    (se2, conds2@[Specifisc.BoolDef (bool_var, se1)]@conds1, ctx)
  | _ ->
    raise
      (Errors.UnsupportedBySpecifisc
         (Printf.sprintf "boolean expression %s %s"
            (Format_mvg.format_expression (Ast.unmark e))
            (Format_ast.format_position (Ast.get_position e))
         )
      )

and translate_arithmetic_expression
    (e: Mvg.expression Ast.marked)
    (ctx: ctx)
  : Specifisc.arithmetic_expression Ast.marked * Specifisc.command list * ctx =
  match Ast.unmark e with
  | Mvg.Unop (Ast.Minus, e1) ->
    let se, conds, ctx = translate_arithmetic_expression e1 ctx in
    (Ast.same_pos_as (Specifisc.ArithmeticMinus se) e, conds, ctx)
  | Mvg.Binop ((Ast.Add | Ast.Sub | Ast.Div | Ast.Mul as op, pos), e1, e2) ->
    let se1, conds1, ctx = translate_arithmetic_expression e1 ctx in
    let se2, conds2, ctx = translate_arithmetic_expression e2 ctx in
    let op = match op with
      | Ast.Add -> Specifisc.Add
      | Ast.Sub -> Specifisc.Sub
      | Ast.Div -> Specifisc.Div
      | Ast.Mul -> Specifisc.Mul
      | _ -> assert false (* should not happen*)
    in
    let se = Specifisc.ArithmeticBinop ((op, pos), se1, se2) in
    (Ast.same_pos_as (match op with
         (* so in Mvg everything is a real which we have to translate to fixed precisions integers
            in Specifisc. The precision is set using [mult_precision_factor_int_real]. However,
            when performing multiplications or divisions, we have to offset this scaling factor.
            This is done with the cases below
         *)
         | Specifisc.Mul -> Specifisc.ArithmeticBinop (
             Ast.same_pos_as Specifisc.Div e,
             Ast.same_pos_as se e,
             Ast.same_pos_as (Specifisc.IntLiteral (Int64.of_int mult_precision_factor_int_real)) e
           )
         | Specifisc.Div ->
           Specifisc.ArithmeticBinop (
             Ast.same_pos_as Specifisc.Mul e,
             Ast.same_pos_as se e,
             Ast.same_pos_as (Specifisc.IntLiteral (Int64.of_int mult_precision_factor_int_real)) e
           )
         | Specifisc.Add | Specifisc.Sub -> se
       ) e,
     conds2@conds1, ctx)
  | Mvg.Conditional (e1, e2, e3) ->
    let se1, conds1, ctx = translate_logical_expression e1 ctx in
    let se2, conds2, ctx = translate_arithmetic_expression e2 ctx in
    let se3, conds3, ctx = translate_arithmetic_expression e3 ctx in
    (Ast.same_pos_as (Specifisc.Conditional (se1 , se2, se3)) e,
     conds3@conds2@conds1, ctx)
  | Mvg.Literal (Mvg.Int i) ->
    (Ast.same_pos_as (
        Specifisc.IntLiteral (Int64.of_int (i * mult_precision_factor_int_real))
      ) e, [], ctx)
  | Mvg.Literal (Mvg.Float f) ->
    (Ast.same_pos_as (
        Specifisc.IntLiteral (Int64.of_float (f *. (float_of_int mult_precision_factor_int_real)))
      ) e, [], ctx)
  | Mvg.Var var ->
    begin match Mvg.VariableMap.find var ctx.ctx_var_mapping with
      | Int int_var -> (Ast.same_pos_as (Specifisc.IntVar int_var) e, [], ctx)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalVar lvar ->
    begin match Mvg.LocalVariableMap.find lvar ctx.ctx_local_var_mapping with
      | Int int_var -> (Ast.same_pos_as (Specifisc.IntVar int_var) e, [], ctx)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalLet (lvar, e1, e2) ->
    if Mvg.LocalVariableMap.find lvar ctx.ctx_typ_info.typ_info_local_var = Mvg.Boolean then
      assert false; (* should not happen *)
    let se1, conds1, ctx =  translate_arithmetic_expression e1 ctx in
    let int_var = Specifisc.IntVariable.new_var
        (Ast.same_pos_as ("t" ^ (string_of_int lvar.Mvg.LocalVariable.id)) e)
        (Ast.same_pos_as ("Local variable") e)
    in
    let ctx =
      { ctx with
        ctx_local_var_mapping = Mvg.LocalVariableMap.add lvar (Int int_var) ctx.ctx_local_var_mapping;
      } in
    let se2, conds2, ctx = translate_arithmetic_expression e2 ctx in
    (se2, conds2@[Specifisc.IntDef (int_var, se1)]@conds1, ctx)
  | Mvg.FunctionCall (Mvg.ArrFunc, [arg]) ->
    let sarg, conds, ctx = translate_arithmetic_expression arg ctx in
    let e' =
      Specifisc.ArithmeticBinop (
        Ast.same_pos_as Specifisc.Mul e,
        Ast.same_pos_as (Specifisc.ArithmeticBinop (
            Ast.same_pos_as Specifisc.Div e,
            Ast.same_pos_as (Specifisc.ArithmeticBinop (
                Ast.same_pos_as Specifisc.Add e,
                sarg,
                Ast.same_pos_as (Specifisc.IntLiteral (Int64.of_int (mult_precision_factor_int_real / 2))) e
              )) e,
            Ast.same_pos_as (Specifisc.IntLiteral (Int64.of_int mult_precision_factor_int_real)) e
          )) e,
        Ast.same_pos_as (Specifisc.IntLiteral (Int64.of_int mult_precision_factor_int_real)) e
      )
    in
    (Ast.same_pos_as e' e, conds, ctx)
  | Mvg.FunctionCall (Mvg.InfFunc, [arg]) ->
    let sarg, conds, ctx = translate_arithmetic_expression arg ctx in
    let e' =
      Specifisc.ArithmeticBinop (
        Ast.same_pos_as Specifisc.Mul e,
        Ast.same_pos_as (Specifisc.ArithmeticBinop (
            Ast.same_pos_as Specifisc.Div e,
            sarg,
            Ast.same_pos_as (Specifisc.IntLiteral (Int64.of_int mult_precision_factor_int_real)) e
          )) e,
        Ast.same_pos_as (Specifisc.IntLiteral (Int64.of_int mult_precision_factor_int_real)) e
      )
    in
    (Ast.same_pos_as e' e, conds, ctx)
  | _ ->
    raise
      (Errors.UnsupportedBySpecifisc
         (Printf.sprintf "arithmetic expression %s %s"
            (Format_mvg.format_expression (Ast.unmark e))
            (Format_ast.format_position (Ast.get_position e))
         )
      )

let translate_variable_data
    (var: Mvg.Variable.t)
    (data: Mvg.variable_data)
    (ctx: ctx)
  : Specifisc.command list * ctx =
  match data.Mvg.var_definition with
  | InputVar -> [], ctx
  | TableVar (_, def) ->
    raise
      (Errors.UnsupportedBySpecifisc
         (Printf.sprintf "table variable %s" (Format_ast.format_position (match def with
              | IndexGeneric e -> Ast.get_position e
              | IndexTable es -> Ast.get_position (snd (Mvg.IndexMap.choose es))
            ))
         )
      )
  | SimpleVar e ->
    begin match begin try Mvg.VariableMap.find var ctx.ctx_typ_info.Typechecker.typ_info_var with
      | Not_found -> assert false end with
    | (Mvg.Boolean, _) ->
      let bool_var = Specifisc.BoolVariable.new_var (var.Mvg.Variable.name) (var.Mvg.Variable.descr) in
      let se, conds, ctx = translate_logical_expression e ctx in
      let new_cmds = (Specifisc.BoolDef (bool_var, se))::conds in
      (new_cmds,
       { ctx with
         ctx_var_mapping = Mvg.VariableMap.add var (Bool bool_var) ctx.ctx_var_mapping
       })
    | ((Mvg.Integer | Mvg.Real), _) ->
      let int_var = Specifisc.IntVariable.new_var (var.Mvg.Variable.name) (var.Mvg.Variable.descr) in
      let se, conds, ctx = translate_arithmetic_expression e ctx in
      let new_cmds = (Specifisc.IntDef (int_var, se))::conds in
      (new_cmds,
       { ctx with
         ctx_var_mapping = Mvg.VariableMap.add var (Int int_var) ctx.ctx_var_mapping
       })
    end

let translate_cond
    (cond: Mvg.condition_data)
    (ctx: ctx)
  : Specifisc.command list * ctx =
  let se, conds, ctx = translate_logical_expression cond.cond_expr ctx in
  let new_cmds = (Specifisc.Constraint se)::conds in
  (new_cmds, ctx )

let translate_program (program: Mvg.program) (typing : Typechecker.typ_info) : Specifisc.program =
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
              let int_var = Specifisc.IntVariable.new_var (var.Mvg.Variable.name) (var.Mvg.Variable.descr) in
              ({ ctx with ctx_var_mapping = Mvg.VariableMap.add var (Int int_var) ctx.ctx_var_mapping },
               (int_var::int_inputs, bool_inputs))
            | Mvg.Boolean ->
              let bool_var = Specifisc.BoolVariable.new_var (var.Mvg.Variable.name) (var.Mvg.Variable.descr) in
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
                         Ast.unmark var.Mvg.Variable.name) (Mvg.VariableMap.bindings scc)))))
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
  let func_id = Specifisc.FunctionVariable.new_var ("Whole program", Ast.no_pos) ("", Ast.no_pos) in
  {
    Specifisc.program_functions = Specifisc.FunctionVariableMap.singleton func_id {
        Specifisc.body = func_body;
        Specifisc.inputs = input_vars;
        Specifisc.outputs = output_vars;
      };
    Specifisc.program_mult_factor = mult_precision_factor_int_real;
    Specifisc.program_idmap = Mvg.VarNameToID.fold (fun name l new_idmap ->
        try
          Mvg.VarNameToID.add name (List.map (fun var ->
              match Mvg.VariableMap.find var ctx.ctx_var_mapping with
              | Bool bool_var ->
                Specifisc.IDBoolVar bool_var
              | Int int_var ->
                Specifisc.IDIntVar int_var

            ) l) new_idmap
        with
        | Not_found -> new_idmap
      ) program.Mvg.program_idmap Mvg.VarNameToID.empty
  }
