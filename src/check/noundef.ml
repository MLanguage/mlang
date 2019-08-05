let check_expr (expr: Mvg.expression Ast.marked) : unit =
  let open Mvg in
  let v = object
    inherit [_] iter
    method! visit_Literal _ l =
      match l with
      | Undefined ->
        raise (Errors.TypeError (Typing ("undef still in " ^ (Ast.format_position @@ Ast.get_position expr))))
      | _ -> ()
  end in
  v#visit_expression () (Ast.unmark expr)

(* let rec check_expr2 (expr: Mvg.expression Ast.marked) =
 *   match Ast.unmark expr with
 *   | Comparison (_, e1, e2)
 *   | Binop (_, e1, e2) ->
 *     check_expr2 e1; check_expr2 e2
 *   | Index(_, e)
 *   | Unop (_, e) -> check_expr2 e
 *   | Conditional (cond, tt, ff) ->
 *     check_expr2 cond; check_expr2 tt; check_expr2 ff
 *   | FunctionCall (_, l) ->
 *     List.iter check_expr2 l
 *   | Literal Undefined ->
 *     raise (Errors.TypeError (Typing ("check_expr2: undef still in " ^ (Ast.format_position @@ Ast.get_position expr))))
 *   | GenericTableIndex | Error
 *   | Literal _ -> ()
 *   | LocalLet (_, e1, e2) ->
 *     check_expr2 e1; check_expr2 e2
 *   | Var _ -> ()
 *   | LocalVar _ -> () *)



let check (program: Mvg.program) : unit =
  let vars = program.program_vars in
  let exec_order = Execution_order.get_execution_order program in
  List.fold_left (fun () scc ->
      let open Mvg in
      VariableMap.fold (fun var () ()  ->
          if VariableMap.mem var vars then
            let def = VariableMap.find var vars in
            match def.var_definition with
            | InputVar -> ()
            | SimpleVar e ->
              (* Cli.debug_print (Printf.sprintf "Checking %s at position %s" (Ast.unmark var.name) (Ast.format_position @@ Ast.get_position var.name));
               * Cli.debug_print (Printf.sprintf "Expr is %s" (Format_mvg.format_expression @@ Ast.unmark e)); *)
              check_expr e
            | TableVar _ -> assert false
        )
        scc ()) () exec_order
