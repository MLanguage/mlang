open Cfg

type ctx = {
  ctx_generic_table_index: expression Ast.marked option;
  ctx_program : program;
}

let empty_ctx (p: program) : ctx = { ctx_generic_table_index = None ; ctx_program = p;}

let rec inline_vars_in_expr
    (ctx: ctx)
    (inlined_vars: unit VariableMap.t)
    (e: expression Ast.marked)
  : expression Ast.marked = match Ast.unmark e with
  | Comparison (op, e1, e2) ->
    Ast.same_pos_as
      (Comparison
         (op,
          inline_vars_in_expr ctx inlined_vars e1,
          inline_vars_in_expr ctx inlined_vars e2
         )) e
  | Binop(op, e1, e2) ->
    Ast.same_pos_as
      (Binop
         (op,
          inline_vars_in_expr ctx inlined_vars e1,
          inline_vars_in_expr ctx inlined_vars e2
         )) e
  | Unop (op, e') ->
    Ast.same_pos_as
      (Unop
         (op,
          inline_vars_in_expr ctx inlined_vars e'
         )) e
  | Conditional (e1, e2, e3) ->
    Ast.same_pos_as
      (Conditional
         (inline_vars_in_expr ctx inlined_vars e3,
          inline_vars_in_expr ctx inlined_vars e1,
          inline_vars_in_expr ctx inlined_vars e2
         )) e
  | FunctionCall (func, args) ->
    Ast.same_pos_as
      (FunctionCall
         (func,
          List.map (fun arg -> inline_vars_in_expr ctx inlined_vars arg) args
         )) e
  | Literal _ | LocalVar _ | Error -> e
  | GenericTableIndex -> begin match ctx.ctx_generic_table_index with
      | None -> e
      | Some gen_index -> gen_index
    end
  | LocalLet (lvar, e1, e2) ->
    Ast.same_pos_as
      (LocalLet
         (lvar,
          inline_vars_in_expr ctx inlined_vars e1,
          inline_vars_in_expr ctx inlined_vars e2
         )) e
  | Var var -> if VariableMap.mem var inlined_vars then
      begin match (VariableMap.find var ctx.ctx_program).var_definition with
        | SimpleVar new_e -> inline_vars_in_expr ctx inlined_vars new_e
        | InputVar -> e
        | TableVar _ -> assert false (* should not happen *)
      end else
      e
  | Index (var, index) ->
    let new_index = inline_vars_in_expr ctx inlined_vars index in
    if VariableMap.mem (Ast.unmark var) inlined_vars then
      begin match (VariableMap.find (Ast.unmark var) ctx.ctx_program).var_definition with
        | SimpleVar _  | InputVar -> assert false (* should not happen *)
        | TableVar (size, table_def) ->
          begin match table_def with
            | IndexGeneric new_e ->
              let new_index_var = LocalVariable.new_var () in
              Ast.same_pos_as
                (LocalLet(new_index_var, new_index, inline_vars_in_expr
                            { ctx with ctx_generic_table_index =
                                         Some (Ast.same_pos_as (LocalVar new_index_var) index) }
                            inlined_vars
                            new_e)) index
            | IndexTable indexes_def -> match Ast.unmark new_index with
              | Literal (Int i) when i < size ->
                let correct_def = IndexMap.find i indexes_def in
                inline_vars_in_expr ctx inlined_vars correct_def
              | _ ->
                raise (Errors.TypeError
                         (Errors.Inlining
                            (Printf.sprintf
                               "cannot inline access to table %s in expression %s because the variable \
                                is not defined generically and the accessing index cannot be \
                                computed at compile time"
                               (Ast.unmark (Ast.unmark var).Variable.name)
                               (Format_ast.format_position (Ast.get_position e))
                            )))
          end
      end else
      e


let inline_vars (inlined_vars:unit VariableMap.t) (p: program) : program =
  VariableMap.fold (fun var def acc ->
      if VariableMap.mem var inlined_vars then
        acc
      else begin
        let new_def = match def.var_definition with
          | InputVar -> InputVar
          | SimpleVar e -> SimpleVar (inline_vars_in_expr (empty_ctx p) inlined_vars e)
          | TableVar (size, def) -> begin match def with
              | IndexGeneric e ->
                TableVar (size, IndexGeneric (inline_vars_in_expr (empty_ctx p) inlined_vars e))
              | IndexTable es ->
                TableVar (size, IndexTable (IndexMap.map (fun e ->
                    inline_vars_in_expr (empty_ctx p) inlined_vars e
                  ) es))
            end
        in
        VariableMap.add var { def with var_definition = new_def } acc
      end
    ) p VariableMap.empty
