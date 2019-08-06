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
