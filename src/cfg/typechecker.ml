open Cfg

type ctx = {
  foo: unit
}

let rec typecheck_top_down
    (ctx: ctx)
    (e: expression Ast.marked)
    (t: typ)
  : ctx =
  match (Ast.unmark e, t) with
  | (Comparison (op, e1, e2), Boolean) ->
    let (ctx, t1) = typecheck_bottom_up ctx e1 in
    let (ctx, t2) = typecheck_bottom_up ctx e2 in
    if t1 = t2 then
      ctx
    else
      raise (Errors.TypeError (
          Errors.Typing
            (Printf.sprintf "expression %s of type %s has not the same type than expression %s of type %s"
               (Format_ast.format_position (Ast.get_position e1))
               (Format_cfg.format_typ t1)
               (Format_ast.format_position (Ast.get_position e2))
               (Format_cfg.format_typ t2)
            )))
  | (Binop (((Ast.And | Ast.Or), _), e1, e2), Boolean)
  | (Binop (((Ast.Add | Ast.Sub | Ast.Mul | Ast.Div), _), e1, e2), (Integer | Real)) ->
    let ctx = typecheck_top_down ctx e1 t in
    let ctx = typecheck_top_down ctx e2 t in
    ctx
  | (Unop (Ast.Not, e), Boolean)
  | (Unop (Ast.Minus, e), (Integer | Real))  ->
    let ctx = typecheck_top_down ctx e t in
    ctx
  | (Conditional (e1, e2, e3), t) ->
    let (ctx, t1) = typecheck_bottom_up ctx e1 in
    if t1 <> Boolean then
      raise (Errors.TypeError (
          Errors.Typing
            (Printf.sprintf "expression %s of type %s should be of type boolean"
               (Format_ast.format_position (Ast.get_position e1))
               (Format_cfg.format_typ t1)
            )))
    else
      let (ctx, t2) = typecheck_bottom_up ctx e2 in
      assert false
(*
                     let (ctx, t3) = typecheck_bottom_up ctx e3 in
                     if t2 = t3 && t2 = t then
                     ctx
                     else
                     raise (Errors.TypeError (
                     Errors.Typing
                     (Printf.sprintf "expression %s of type %s has not the same type than expression %s of type %s, and both should be of type %s"
                     (Format_ast.format_position (Ast.get_position e1))
                     (Format_cfg.format_typ t1)
                     (Format_ast.format_position (Ast.get_position e2))
                     (Format_cfg.format_typ t2)
                     (Format_cfg.format_typ t)
                     )))*)
  | (FunctionCall (func, args), t) ->
    assert false
  | (Literal (Int _), Integer)
  | (Literal (Float _), Real)
  | (Literal (Bool _), Boolean) -> ctx
  | (Var var, t) ->
    assert false
  | (LocalLet (var, e1, e2), t) ->
    assert false
  | (LocalVar var, t) ->
    assert false
  | (GenericTableIndex, Integer) -> ctx
  | _ -> raise (Errors.TypeError (
      Errors.Typing
        (Printf.sprintf "expression %s should be of type %s, but is of type %s"
           (Format_ast.format_position (Ast.get_position e))
           (Format_cfg.format_typ t)
           (
             let (_, t') =  typecheck_bottom_up ctx e in
             Format_cfg.format_typ t'
           )
        )))

and typecheck_bottom_up (ctx: ctx) (e: expression Ast.marked) : (ctx * typ) =
  assert false

let typecheck (p: program) : typ VariableMap.t =
  let types_before_union = Cfg.VariableMap.fold (fun var def (acc, ctx) ->
      match def.var_typ with
      | Some t -> begin match def.var_definition with
          | SimpleVar e ->
            let new_ctx = typecheck_top_down ctx e t in
            (VariableMap.add var t acc, new_ctx)
          | TableVar (size, defs) ->
            assert false
        end
      | None -> assert false
    ) p (Cfg.VariableMap.empty, { foo = () })
  in
  assert false
