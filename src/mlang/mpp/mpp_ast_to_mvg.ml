(* Compile the mpp ast and the m codebase into an mvg program. Partitioning can be done by putting
   excluded inputs to undef and storing them into an auxiliary variable (which is merged back
   afterwards) *)

module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

type translation_ctx = {
  new_variables : Mvg.Variable.t StringMap.t;
  variables_used_as_inputs : unit Mvg.VariableMap.t;
}

let emtpy_translation_ctx : translation_ctx =
  { new_variables = StringMap.empty; variables_used_as_inputs = Mvg.VariableMap.empty }

let translate_to_binop (b : Cst.binop) : Ast.binop =
  match b with And -> And | Or -> Or | _ -> assert false

let translate_to_compop (b : Cst.binop) : Ast.comp_op =
  match b with
  | Gt -> Gt
  | Gte -> Gte
  | Lt -> Lt
  | Lte -> Lte
  | Eq -> Eq
  | Neq -> Neq
  | _ -> assert false

let rec list_map_opt (f : 'a -> 'b option) (l : 'a list) : 'b list =
  match l with
  | [] -> []
  | hd :: tl -> (
      match f hd with None -> list_map_opt f tl | Some fhd -> fhd :: list_map_opt f tl )

let generate_input_condition (crit : Mvg.Variable.t -> bool) (p : Interpreter.interpretable_program)
    (pos : Pos.t) =
  let variables_to_check =
    Mvg.VariableMap.filter (fun var _ -> crit var) p.ip_program.program_vars
  in
  let mk_call_present x = (Mvg.FunctionCall (PresentFunc, [ (Mvg.Var x, pos) ]), pos) in
  let mk_or e1 e2 = (Mvg.Binop ((Or, pos), e1, e2), pos) in
  let mk_false = (Mvg.Literal (Float 0.), pos) in
  Mvg.VariableMap.fold
    (fun var _ acc -> mk_or (mk_call_present var) acc)
    variables_to_check mk_false

let var_is_ (attr : string) (v : Mvg.Variable.t) : bool =
  List.exists
    (fun ((attr_name, _), (attr_value, _)) -> attr_name = attr && attr_value = Ast.Float 1.)
    v.Mvg.Variable.attributes

let cond_DepositDefinedVariables :
    Interpreter.interpretable_program -> Pos.t -> Mvg.expression Pos.marked =
  generate_input_condition (var_is_ "acompte")

let cond_TaxbenefitDefinedVariables :
    Interpreter.interpretable_program -> Pos.t -> Mvg.expression Pos.marked =
  generate_input_condition (var_is_ "avfisc")

let cond_TaxbenefitCeiledVariables (p : Interpreter.interpretable_program) (pos : Pos.t) :
    Mvg.expression Pos.marked =
  (* commented aliases do not exist in the 2018 version *)
  let aliases_list =
    [
      "7QK";
      "7QD";
      "7QB";
      "7QC";
      "4BA";
      "4BY";
      "4BB";
      "4BC";
      "7CL";
      "7CM";
      "7CN";
      "7QE";
      "7QF";
      "7QG";
      "7QH";
      "7QI";
      "7QJ";
      "7LG";
      (* "7MA"; *)
      "7QM";
      "2DC";
      (* "7KM"; *)
      (* "7KG"; *)
      "7QP";
      "7QS";
      "7QN";
      "7QO";
      "7QL";
      "7LS";
    ]
  in
  let supp_avfisc =
    List.fold_left
      (fun vmap var -> Mvg.VariableMap.add (Mvg.find_var_by_name p.ip_program var) () vmap)
      Mvg.VariableMap.empty aliases_list
  in
  generate_input_condition (fun v -> Mvg.VariableMap.mem v supp_avfisc) p pos

let reset_and_add_outputs (p : Interpreter.interpretable_program) (outputs : string list) :
    Interpreter.interpretable_program =
  let outputs = List.map (fun out -> Mvg.find_var_by_name p.ip_program out) outputs in
  let program =
    {
      p.ip_program with
      program_vars =
        Mvg.VariableMap.mapi
          (fun var data ->
            if List.mem var outputs then
              match data.Mvg.var_io with
              | Input ->
                  raise
                    (Interpreter.RuntimeError
                       ( Interpreter.IncorrectOutputVariable
                           (Format.asprintf "%a is an input" Format_mvg.format_variable var),
                         Interpreter.empty_ctx p.ip_program ))
              | Output -> data
              | Regular -> { data with var_io = Output }
            else
              match data.Mvg.var_io with
              | Input | Regular -> data
              | Output -> { data with var_io = Regular })
          p.ip_program.program_vars;
    }
  in
  { p with ip_program = program }

let rec translate_mpp_function (mpp_program : Mpp_ast.mpp_compute list)
    (m_program : Interpreter.interpretable_program) (compute_decl : Mpp_ast.mpp_compute)
    (args : Mpp_ast.scoped_var list) (ctx : translation_ctx) : translation_ctx * Mvg.stmt list =
  List.fold_left
    (fun (ctx, stmts) stmt ->
      let ctx, stmt' = translate_mpp_stmt mpp_program m_program args ctx stmt in
      (ctx, stmts @ stmt'))
    (ctx, []) compute_decl.Mpp_ast.body

and translate_mpp_expr (p : Interpreter.interpretable_program) (ctx : translation_ctx)
    (expr : Mpp_ast.mpp_expr_kind Pos.marked) : Mvg.expression =
  let pos = Pos.get_position expr in
  match Pos.unmark expr with
  | Mpp_ast.Constant i -> Mvg.Literal (Float (float_of_int i))
  | Variable (Mbased (var, _)) -> Var var
  | Variable (Local l) -> (
      try Var (StringMap.find l ctx.new_variables)
      with Not_found ->
        Cli.error_print "Local Variable %s not found in ctx" l;
        assert false )
  | Unop (Minus, e) -> Mvg.Unop (Ast.Minus, (translate_mpp_expr p ctx e, pos))
  | Binop (e1, ((And | Or) as b), e2) ->
      Mvg.Binop
        ( (translate_to_binop b, pos),
          (translate_mpp_expr p ctx e1, pos),
          (translate_mpp_expr p ctx e2, pos) )
  | Binop (e1, b, e2) ->
      Mvg.Comparison
        ( (translate_to_compop b, pos),
          (translate_mpp_expr p ctx e1, pos),
          (translate_mpp_expr p ctx e2, pos) )
  | Call (Present, [ l ]) ->
      Pos.unmark @@ Functions.expand_functions_expr
      @@ ( Mvg.FunctionCall
             (PresentFunc, [ (translate_mpp_expr p ctx (Mpp_ast.Variable l, pos), pos) ]),
           pos )
  | Call (Abs, [ l ]) ->
      Pos.unmark @@ Functions.expand_functions_expr
      @@ ( Mvg.FunctionCall (AbsFunc, [ (translate_mpp_expr p ctx (Mpp_ast.Variable l, pos), pos) ]),
           pos )
  | Call (Cast, [ l ]) ->
      Mvg.Binop
        ( (Ast.Add, pos),
          (translate_mpp_expr p ctx (Mpp_ast.Variable l, pos), pos),
          (Mvg.Literal (Float 0.), pos) )
  | Call (DepositDefinedVariables, []) -> Pos.unmark @@ cond_DepositDefinedVariables p pos
  | Call (TaxbenefitCeiledVariables, []) -> Pos.unmark @@ cond_TaxbenefitCeiledVariables p pos
  | Call (TaxbenefitDefinedVariables, []) -> Pos.unmark @@ cond_TaxbenefitDefinedVariables p pos
  | _ -> assert false

and translate_mpp_stmt mpp_program (m_program : Interpreter.interpretable_program) func_args
    (ctx : translation_ctx) stmt : translation_ctx * Mvg.stmt list =
  let pos = Pos.get_position stmt in
  match Pos.unmark stmt with
  | Mpp_ast.Assign (Local l, expr) ->
      let ctx, new_l =
        match StringMap.find_opt l ctx.new_variables with
        | None ->
            let new_l =
              Mvg.Variable.new_var (l, pos) None ("", pos)
                ({ rule_number = -1; seq_number = -1; pos } : Mvg.execution_number)
                ~attributes:[] ~is_income:false
            in
            let ctx = { ctx with new_variables = StringMap.add l new_l ctx.new_variables } in
            (ctx, new_l)
        | Some new_l -> (ctx, new_l)
      in
      ( ctx,
        [
          Pos.same_pos_as
            (Mvg.SAssign
               ( new_l,
                 {
                   var_definition = SimpleVar (translate_mpp_expr m_program ctx expr, pos);
                   var_typ = None;
                   var_io = Regular;
                 } ))
            stmt;
        ] )
  | Mpp_ast.Assign (Mbased (var, _), expr) ->
      ( {
          ctx with
          variables_used_as_inputs = Mvg.VariableMap.add var () ctx.variables_used_as_inputs;
        },
        [
          Pos.same_pos_as
            (Mvg.SAssign
               ( var,
                 {
                   var_definition = SimpleVar (translate_mpp_expr m_program ctx expr, pos);
                   var_typ = None;
                   var_io = (Mvg.VariableMap.find var m_program.ip_program.program_vars).var_io;
                 } ))
            stmt;
        ] )
  | Mpp_ast.Conditional (e, t, f) ->
      let e' = translate_mpp_expr m_program ctx e in
      let ctx1, rt' = translate_mpp_stmts mpp_program m_program func_args ctx t in
      let ctx2, rf' =
        translate_mpp_stmts mpp_program m_program func_args
          { ctx with new_variables = ctx1.new_variables }
          f
      in
      ( {
          ctx2 with
          variables_used_as_inputs =
            Mvg.VariableMap.union
              (fun _ _ _ -> Some ())
              ctx1.variables_used_as_inputs ctx2.variables_used_as_inputs;
        },
        [ Pos.same_pos_as (Mvg.SConditional (e', rt', rf')) stmt ] )
  | Mpp_ast.Delete (Mbased (var, _)) ->
      ( ctx,
        [
          Pos.same_pos_as
            (Mvg.SAssign
               ( var,
                 {
                   var_definition = SimpleVar (Mvg.Literal Undefined, pos);
                   var_typ = None;
                   var_io = (Mvg.VariableMap.find var m_program.ip_program.program_vars).var_io;
                 } ))
            stmt;
        ] )
  | Mpp_ast.Delete (Local l) ->
      let var = StringMap.find l ctx.new_variables in
      ( ctx,
        [
          Pos.same_pos_as
            (Mvg.SAssign
               ( var,
                 {
                   var_definition = SimpleVar (Mvg.Literal Undefined, pos);
                   var_typ = None;
                   var_io = Regular;
                 } ))
            stmt;
        ] )
  | Mpp_ast.Expr (Call (MppFunction f, args), _) ->
      let real_args = match args with [ Mpp_ast.Local "outputs" ] -> func_args | _ -> args in
      translate_mpp_function mpp_program m_program
        (List.find (fun decl -> decl.Mpp_ast.name = f) mpp_program)
        real_args ctx
  | Mpp_ast.Expr (Call (Program, args), _) ->
      let real_args = match args with [ Mpp_ast.Local "outputs" ] -> func_args | _ -> args in
      let m_program =
        reset_and_add_outputs m_program
          (List.map
             (function Mpp_ast.Mbased (s, _) -> Pos.unmark s.Mvg.Variable.name | Local l -> l)
             real_args)
      in
      let m_program =
        {
          m_program with
          ip_program =
            Interpreter.replace_undefined_with_input_variables m_program.ip_program
              (Mvg.VariableMap.map (fun () -> Mvg.Undefined) ctx.variables_used_as_inputs);
        }
      in
      let exec_order = m_program.ip_utils.utilities_execution_order in
      let inlined_program =
        list_map_opt
          (fun var ->
            try
              let vdef =
                Mvg.VariableMap.find var m_program.ip_program.program_vars
                (* should be a verification condition *)
              in
              match vdef.var_definition with
              | InputVar -> None
              | _ -> Some (Mvg.SAssign (var, vdef), var.Mvg.Variable.execution_number.pos)
            with Not_found -> None)
          exec_order
      in
      (ctx, inlined_program)
  | Mpp_ast.Partition (filter, body) ->
      let func_of_filter = match filter with Mpp_ast.VarIsTaxBenefit -> var_is_ "avfisc" in
      let ctx, partition_pre, partition_post =
        generate_partition mpp_program m_program func_args func_of_filter pos ctx
      in
      let ctx, body = translate_mpp_stmts mpp_program m_program func_args ctx body in
      (ctx, partition_pre @ body @ partition_post)
  | _ -> assert false

and translate_mpp_stmts (mpp_program : Mpp_ast.mpp_compute list)
    (m_program : Interpreter.interpretable_program) (func_args : Mpp_ast.scoped_var list)
    (ctx : translation_ctx) (stmts : Mpp_ast.mpp_stmt list) : translation_ctx * Mvg.stmt list =
  List.fold_left
    (fun (ctx, stmts) stmt ->
      let ctx, stmt = translate_mpp_stmt mpp_program m_program func_args ctx stmt in
      (ctx, stmts @ stmt))
    (ctx, []) stmts

and generate_partition mpp_program m_program func_args (filter : Mvg.Variable.t -> bool)
    (pos : Pos.t) (ctx : translation_ctx) : translation_ctx * Mvg.stmt list * Mvg.stmt list =
  let vars_to_move =
    Mvg.VariableMap.fold
      (fun var _ acc -> if filter var then var :: acc else acc)
      m_program.ip_program.program_vars []
  in
  let ctx, mpp_pre, mpp_post =
    List.fold_left
      (fun (ctx, stmts_pre, stmts_post) var ->
        let shadow_var_name = "_" ^ Pos.unmark var.Mvg.Variable.name in
        let open Mpp_ast in
        let shadow_var = Local shadow_var_name in
        let var = Mbased (var, Output) in
        ( ctx,
          (Assign (shadow_var, (Variable var, pos)), pos) :: (Delete var, pos) :: stmts_pre,
          (Assign (var, (Variable shadow_var, pos)), pos) :: (Delete shadow_var, pos) :: stmts_post
        ))
      (ctx, [], []) vars_to_move
  in
  let ctx, pre = translate_mpp_stmts mpp_program m_program func_args ctx mpp_pre in
  let ctx, post = translate_mpp_stmts mpp_program m_program func_args ctx mpp_post in
  (ctx, pre, post)

let create_combined_program (m_program : Interpreter.interpretable_program)
    (mpp_program : Mpp_ast.mpp_program) : Mvg.new_program =
  let mpp_program = List.rev mpp_program in
  {
    statements =
      snd
      @@ translate_mpp_function mpp_program m_program (List.hd mpp_program) [] emtpy_translation_ctx;
    conds = m_program.ip_program.program_conds;
    idmap = m_program.ip_program.program_idmap;
  }
