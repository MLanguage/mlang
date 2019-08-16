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

module Pos = Verifisc.Pos

type translated_var =
  | Bool of Verifisc.Ast.BoolVariable.t
  | Int of Verifisc.Ast.IntVariable.t

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

let switch_div_mul (op: Ast.binop) : Verifisc.Ast.arithmetic_binop = match op with
  | Ast.Div -> Verifisc.Ast.Mul
  | Ast.Mul -> Verifisc.Ast.Div
  | _ -> assert false (* should not happen *)


let rec translate_logical_expression
    (e: Mvg.expression Pos.marked)
    (ctx: ctx)
  : Verifisc.Ast.logical_expression Pos.marked * Verifisc.Ast.command list * ctx =
  match Pos.unmark e with
  | Mvg.Unop (Ast.Not, e1) ->
    let se, conds, ctx = translate_logical_expression e1 ctx in
    (Pos.same_pos_as (Verifisc.Ast.LogicalNot se) e, conds, ctx)
  | Mvg.Binop ((Ast.And | Ast.Or as op, pos), e1, e2) ->
    let se1, conds1, ctx = translate_logical_expression e1 ctx in
    let se2, conds2, ctx = translate_logical_expression e2 ctx in
    let op = match op with
      | Ast.And -> Verifisc.Ast.And
      | Ast.Or -> Verifisc.Ast.Or
      | _ -> assert false (* should not happen*)
    in
    (Pos.same_pos_as (Verifisc.Ast.LogicalBinop ((op, pos), se1, se2)) e,
     conds2@conds1, ctx)
  | Mvg.Comparison (op, e1, e2) ->
    let se1, conds1, ctx = translate_arithmetic_expression e1 ctx in
    let se2, conds2, ctx = translate_arithmetic_expression e2 ctx in
    let sop = match Pos.unmark op with
      | Ast.Gt -> Verifisc.Ast.Gt
      | Ast.Gte -> Verifisc.Ast.Gte
      | Ast.Lt -> Verifisc.Ast.Lt
      | Ast.Lte -> Verifisc.Ast.Lte
      | Ast.Neq -> Verifisc.Ast.Neq
      | Ast.Eq -> Verifisc.Ast.Eq
    in
    (Pos.same_pos_as (Verifisc.Ast.Comparison (Pos.same_pos_as sop op , se1, se2)) e,
     conds2@conds1, ctx)
  | Mvg.Literal (Mvg.Bool b) -> (Pos.same_pos_as (Verifisc.Ast.BoolLiteral b) e, [], ctx)
  | Mvg.Var var ->
    begin match Mvg.VariableMap.find var ctx.ctx_var_mapping with
      | Bool bool_var -> (Pos.same_pos_as (Verifisc.Ast.BoolVar bool_var) e, [], ctx)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalVar lvar ->
    begin match Mvg.LocalVariableMap.find lvar ctx.ctx_local_var_mapping with
      | Bool bool_var -> (Pos.same_pos_as (Verifisc.Ast.BoolVar bool_var) e, [], ctx)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalLet (lvar, e1, e2) ->
    if Mvg.LocalVariableMap.find lvar ctx.ctx_typ_info.typ_info_local_var <> Mvg.Boolean then
      assert false (* should not happen *);
    let se1, conds1, ctx = translate_logical_expression e1 ctx in
    let bool_var = Verifisc.Ast.BoolVariable.new_var
        (Pos.same_pos_as ("t" ^ (string_of_int lvar.Mvg.LocalVariable.id)) e)
        (Pos.same_pos_as ("Local variable") e)
    in
    let ctx =
      { ctx with
        ctx_local_var_mapping = Mvg.LocalVariableMap.add lvar (Bool bool_var) ctx.ctx_local_var_mapping;
      } in
    let se2, conds2, ctx = translate_logical_expression e2 ctx in
    (se2, conds2@[Verifisc.Ast.BoolDef (bool_var, se1)]@conds1, ctx)
  | _ ->
    raise
      (Verifisc.Errors.UnsupportedByVerifisc
         (Printf.sprintf "boolean expression %s %s"
            (Format_mvg.format_expression (Pos.unmark e))
            (Pos.format_position (Pos.get_position e))
         )
      )

and translate_arithmetic_expression
    (e: Mvg.expression Pos.marked)
    (ctx: ctx)
  : Verifisc.Ast.arithmetic_expression Pos.marked * Verifisc.Ast.command list * ctx =
  match Pos.unmark e with
  | Mvg.Unop (Ast.Minus, e1) ->
    let se, conds, ctx = translate_arithmetic_expression e1 ctx in
    (Pos.same_pos_as (Verifisc.Ast.ArithmeticMinus se) e, conds, ctx)
  | Mvg.Binop ((Ast.Add | Ast.Sub | Ast.Div | Ast.Mul as op, pos), e1, e2) ->
    let se1, conds1, ctx = translate_arithmetic_expression e1 ctx in
    let se2, conds2, ctx = translate_arithmetic_expression e2 ctx in
    let op = match op with
      | Ast.Add -> Verifisc.Ast.Add
      | Ast.Sub -> Verifisc.Ast.Sub
      | Ast.Div -> Verifisc.Ast.Div
      | Ast.Mul -> Verifisc.Ast.Mul
      | _ -> assert false (* should not happen*)
    in
    let se = Verifisc.Ast.ArithmeticBinop ((op, pos), se1, se2) in
    (Pos.same_pos_as (match op with
         (* so in Mvg everything is a real which we have to translate to fixed precisions integers
            in Ast. The precision is set using [!Cli.real_precision]. However,
            when performing multiplications or divisions, we have to offset this scaling factor.
            This is done with the cases below
         *)
         | Verifisc.Ast.Mul -> Verifisc.Ast.ArithmeticBinop (
             Pos.same_pos_as Verifisc.Ast.Div e,
             Pos.same_pos_as se e,
             Pos.same_pos_as (Verifisc.Ast.IntLiteral (Int64.of_int !Cli.real_precision)) e
           )
         | Verifisc.Ast.Div ->
           Verifisc.Ast.ArithmeticBinop (
             Pos.same_pos_as Verifisc.Ast.Mul e,
             Pos.same_pos_as se e,
             Pos.same_pos_as (Verifisc.Ast.IntLiteral (Int64.of_int !Cli.real_precision)) e
           )
         | Verifisc.Ast.Add | Verifisc.Ast.Sub -> se
       ) e,
     conds2@conds1, ctx)
  | Mvg.Conditional (e1, e2, e3) ->
    let se1, conds1, ctx = translate_logical_expression e1 ctx in
    let se2, conds2, ctx = translate_arithmetic_expression e2 ctx in
    let se3, conds3, ctx = translate_arithmetic_expression e3 ctx in
    (Pos.same_pos_as (Verifisc.Ast.Conditional (se1 , se2, se3)) e,
     conds3@conds2@conds1, ctx)
  | Mvg.Literal (Mvg.Int i) ->
    (Pos.same_pos_as (
        Verifisc.Ast.IntLiteral (Int64.of_int (i * !Cli.real_precision))
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
        Verifisc.Ast.IntLiteral (Int64.of_float (f *. (float_of_int !Cli.real_precision)))
      ) e, [], ctx)
  | Mvg.Var var ->
    begin match Mvg.VariableMap.find var ctx.ctx_var_mapping with
      | Int int_var -> (Pos.same_pos_as (Verifisc.Ast.IntVar int_var) e, [], ctx)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalVar lvar ->
    begin match Mvg.LocalVariableMap.find lvar ctx.ctx_local_var_mapping with
      | Int int_var -> (Pos.same_pos_as (Verifisc.Ast.IntVar int_var) e, [], ctx)
      | _ -> assert false (* should not happen *)
    end
  | Mvg.LocalLet (lvar, e1, e2) ->
    if Mvg.LocalVariableMap.find lvar ctx.ctx_typ_info.typ_info_local_var = Mvg.Boolean then
      assert false; (* should not happen *)
    let se1, conds1, ctx =  translate_arithmetic_expression e1 ctx in
    let int_var = Verifisc.Ast.IntVariable.new_var
        (Pos.same_pos_as ("t" ^ (string_of_int lvar.Mvg.LocalVariable.id)) e)
        (Pos.same_pos_as ("Local variable") e)
    in
    let ctx =
      { ctx with
        ctx_local_var_mapping = Mvg.LocalVariableMap.add lvar (Int int_var) ctx.ctx_local_var_mapping;
      } in
    let se2, conds2, ctx = translate_arithmetic_expression e2 ctx in
    (se2, conds2@[Verifisc.Ast.IntDef (int_var, se1)]@conds1, ctx)
  | Mvg.FunctionCall (Mvg.ArrFunc, [arg]) ->
    let sarg, conds, ctx = translate_arithmetic_expression arg ctx in
    let e' =
      Verifisc.Ast.ArithmeticBinop (
        Pos.same_pos_as Verifisc.Ast.Mul e,
        Pos.same_pos_as (Verifisc.Ast.ArithmeticBinop (
            Pos.same_pos_as Verifisc.Ast.Div e,
            Pos.same_pos_as (Verifisc.Ast.ArithmeticBinop (
                Pos.same_pos_as Verifisc.Ast.Add e,
                sarg,
                Pos.same_pos_as (Verifisc.Ast.IntLiteral (Int64.of_int (!Cli.real_precision / 2))) e
              )) e,
            Pos.same_pos_as (Verifisc.Ast.IntLiteral (Int64.of_int !Cli.real_precision)) e
          )) e,
        Pos.same_pos_as (Verifisc.Ast.IntLiteral (Int64.of_int !Cli.real_precision)) e
      )
    in
    (Pos.same_pos_as e' e, conds, ctx)
  | Mvg.FunctionCall (Mvg.InfFunc, [arg]) ->
    let sarg, conds, ctx = translate_arithmetic_expression arg ctx in
    let e' =
      Verifisc.Ast.ArithmeticBinop (
        Pos.same_pos_as Verifisc.Ast.Mul e,
        Pos.same_pos_as (Verifisc.Ast.ArithmeticBinop (
            Pos.same_pos_as Verifisc.Ast.Div e,
            sarg,
            Pos.same_pos_as (Verifisc.Ast.IntLiteral (Int64.of_int !Cli.real_precision)) e
          )) e,
        Pos.same_pos_as (Verifisc.Ast.IntLiteral (Int64.of_int !Cli.real_precision)) e
      )
    in
    (Pos.same_pos_as e' e, conds, ctx)
  | _ ->
    raise
      (Verifisc.Errors.UnsupportedByVerifisc
         (Printf.sprintf "arithmetic expression %s %s"
            (Format_mvg.format_expression (Pos.unmark e))
            (Pos.format_position (Pos.get_position e))
         )
      )

let translate_variable_data
    (var: Mvg.Variable.t)
    (data: Mvg.variable_data)
    (ctx: ctx)
  : Verifisc.Ast.command list * ctx =
  match data.Mvg.var_definition with
  | InputVar -> [], ctx
  | TableVar (_, def) ->
    raise
      (Verifisc.Errors.UnsupportedByVerifisc
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
      let bool_var = Verifisc.Ast.BoolVariable.new_var (var.Mvg.Variable.name) (var.Mvg.Variable.descr) in
      let se, conds, ctx = translate_logical_expression e ctx in
      let new_cmds = (Verifisc.Ast.BoolDef (bool_var, se))::conds in
      (new_cmds,
       { ctx with
         ctx_var_mapping = Mvg.VariableMap.add var (Bool bool_var) ctx.ctx_var_mapping
       })
    | ((Mvg.Integer | Mvg.Real), _) ->
      let int_var = Verifisc.Ast.IntVariable.new_var (var.Mvg.Variable.name) (var.Mvg.Variable.descr) in
      let se, conds, ctx = translate_arithmetic_expression e ctx in
      let new_cmds = (Verifisc.Ast.IntDef (int_var, se))::conds in
      (new_cmds,
       { ctx with
         ctx_var_mapping = Mvg.VariableMap.add var (Int int_var) ctx.ctx_var_mapping
       })
    end

let translate_cond
    (cond: Mvg.condition_data)
    (ctx: ctx)
  : Verifisc.Ast.command list * ctx =
  let se, conds, ctx = translate_logical_expression cond.cond_expr ctx in
  (** Verifisc has assertions while M raises errors, so we need the negation *)
  let new_cmds = (Verifisc.Ast.Constraint
                    (Pos.same_pos_as
                       (Verifisc.Ast.LogicalNot (se))
                       cond.cond_expr))::conds
  in
  (new_cmds, ctx )

let translate_program (program: Mvg.program) (typing : Typechecker.typ_info) : Verifisc.Ast.program =
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
              let int_var = Verifisc.Ast.IntVariable.new_var (var.Mvg.Variable.name) (var.Mvg.Variable.descr) in
              ({ ctx with ctx_var_mapping = Mvg.VariableMap.add var (Int int_var) ctx.ctx_var_mapping },
               (int_var::int_inputs, bool_inputs))
            | Mvg.Boolean ->
              let bool_var = Verifisc.Ast.BoolVariable.new_var (var.Mvg.Variable.name) (var.Mvg.Variable.descr) in
              ({ ctx with ctx_var_mapping = Mvg.VariableMap.add var (Bool bool_var) ctx.ctx_var_mapping },
               (int_inputs, bool_var::bool_inputs))
          end
        | _ -> (ctx, (int_inputs, bool_inputs))
      ) program.program_vars (ctx, ([], []))
  in
  (* Then the main translation *)
  let func_body, ctx = List.fold_left (fun (cmds, ctx) scc  ->
      if Mvg.VariableMap.cardinal scc > 1 then
        raise (Verifisc.Errors.UnsupportedByVerifisc
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
  let func_id = Verifisc.Ast.FunctionVariable.new_var ("Whole program", Pos.no_pos) ("", Pos.no_pos) in
  {
    Verifisc.Ast.program_functions = Verifisc.Ast.FunctionVariableMap.singleton func_id {
        Verifisc.Ast.body = func_body;
        Verifisc.Ast.inputs = input_vars;
        Verifisc.Ast.outputs = output_vars;
      };
    Verifisc.Ast.program_mult_factor = !Cli.real_precision;
    Verifisc.Ast.program_idmap = Pos.VarNameToID.fold (fun name l new_idmap ->
        try
          Pos.VarNameToID.add name (List.map (fun var ->
              match Mvg.VariableMap.find var ctx.ctx_var_mapping with
              | Bool bool_var ->
                Verifisc.Ast.IDBoolVar bool_var
              | Int int_var ->
                Verifisc.Ast.IDIntVar int_var

            ) l) new_idmap
        with
        | Not_found -> new_idmap
      ) program.Mvg.program_idmap Pos.VarNameToID.empty
  }
