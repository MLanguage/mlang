(** AST to CFG translation of M *)

type io_status =
  | Input
  | Output
  | Constant
  | Regular

type var_decl_data = {
  var_decl_typ: Ast.value_typ option;
  var_decl_is_table: int option;
  var_decl_descr: string option;
  var_decl_io: io_status;
}

module VarNameToID = Map.Make(String)
type idmap = Cfg.Variable.t VarNameToID.t

let get_var_from_name (d:Cfg.Variable.t VarNameToID.t) (name:Ast.variable_name Ast.marked) : Cfg.Variable.t =
  match VarNameToID.find_opt (Ast.unmark name) d with
  | Some var -> var
  | None ->
    raise (Errors.TypeError
             (Errors.Variable
                (Printf.sprintf "variable %s used %s, has not been declared"
                   (Ast.unmark name)
                   (Format_ast.format_position (Ast.get_position name))
                )))

let get_variables_decl (p: Ast.program) : (var_decl_data Cfg.VariableMap.t * idmap) =
  let (vars, idmap, out_list) = List.fold_left (fun (vars, idmap, out_list) source_file ->
      List.fold_left (fun (vars, idmap, out_list) source_file_item ->
          match Ast.unmark source_file_item with
          | Ast.Variable var_decl ->
            begin match var_decl with
              | Ast.ComputedVar cvar ->
                let cvar = Ast.unmark cvar in
                let new_var = Cfg.Variable.new_var cvar.Ast.comp_name in
                let new_var_data = {
                  var_decl_typ = Ast.unmark_option cvar.Ast.comp_typ;
                  var_decl_is_table = Ast.unmark_option cvar.Ast.comp_table;
                  var_decl_descr = Some (Ast.unmark cvar.Ast.comp_description);
                  var_decl_io = Regular
                }
                in
                let new_vars = Cfg.VariableMap.add new_var new_var_data vars in
                let new_idmap = VarNameToID.add (Ast.unmark cvar.Ast.comp_name) new_var idmap in
                (new_vars, new_idmap, out_list)
              | Ast.InputVar ivar ->
                let ivar = Ast.unmark ivar in
                let new_var = Cfg.Variable.new_var ivar.Ast.input_name in
                let new_var_data = {
                  var_decl_typ = Ast.unmark_option ivar.Ast.input_typ;
                  var_decl_is_table = None;
                  var_decl_descr = Some (Ast.unmark ivar.Ast.input_description);
                  var_decl_io = Input
                } in
                let new_vars = Cfg.VariableMap.add new_var new_var_data vars in
                let new_idmap = VarNameToID.add (Ast.unmark ivar.Ast.input_name) new_var idmap in
                (new_vars, new_idmap, out_list)
              | Ast.ConstVar (marked_name, _) ->
                let new_var  = Cfg.Variable.new_var marked_name in
                let new_var_data = {
                  var_decl_typ = None;
                  var_decl_is_table = None;
                  var_decl_descr = None;
                  var_decl_io = Constant;
                } in
                let new_vars = Cfg.VariableMap.add new_var new_var_data vars in
                let new_idmap = VarNameToID.add (Ast.unmark marked_name) new_var idmap in
                (new_vars, new_idmap, out_list)
            end
          | Ast.Output out_name -> (vars, idmap, out_name::out_list)
          | _ -> (vars, idmap, out_list)
        ) (vars, idmap, out_list) source_file
    ) (Cfg.VariableMap.empty, VarNameToID.empty, []) p in
  let vars : var_decl_data Cfg.VariableMap.t = List.fold_left (fun vars out ->
      let out_var = get_var_from_name idmap out in
      match Cfg.VariableMap.find_opt out_var vars with
      | Some data -> Cfg.VariableMap.add out_var { data with var_decl_io = Output } vars
      | None -> assert false (* should not happen *)
    ) vars out_list in
  (vars, idmap)

type translating_context = {
  idmap : idmap;
}

let rec translate_variable (ctx: translating_context) (var: Ast.variable Ast.marked) : Cfg.expression Ast.marked =
  match Ast.unmark var with
  | Ast.Normal name ->
    Ast.same_pos_as (Cfg.Var (get_var_from_name ctx.idmap (Ast.same_pos_as name var))) var
  | Ast.Generic gen_name ->
    if List.length gen_name.Ast.parameters == 0 then
      translate_variable ctx (Ast.same_pos_as (Ast.Normal gen_name.Ast.base) var)
    else
      raise (Errors.Unimplemented ("TODO1", Ast.get_position var))

let translate_table_index (ctx: translating_context) (i: Ast.table_index Ast.marked) : Cfg.table_index Ast.marked =
  match Ast.unmark i with
  | Ast.LiteralIndex i' -> Ast.same_pos_as (Cfg.LiteralIndex i') i
  | Ast.GenericIndex -> raise (Errors.Unimplemented ("TODO2", Ast.get_position i))
  | Ast.SymbolIndex v ->
    let var = translate_variable ctx (Ast.same_pos_as v i) in
    Ast.same_pos_as (Cfg.SymbolIndex (match Ast.unmark var with
        | Cfg.Var v -> v
        | _ -> assert false (* should not happen *)))
      var

let translate_function_name (f_name : string Ast.marked) = match Ast.unmark f_name with
  | "somme" -> Cfg.SumFunc
  | "min" -> Cfg.MinFunc
  | "abs" -> Cfg.AbsFunc
  | "positif" -> Cfg.GtzFunc
  | "positif_ou_nul" -> Cfg.GtezFunc
  | x -> raise (Errors.TypeError (
      Errors.Function (
        Printf.sprintf "unknown function %s %s" x (Format_ast.format_position (Ast.get_position f_name))
      )))

module ParamsMap = Map.Make(String)

type loop_context = char ParamsMap.t

let translate_loop_variables (ctx: translating_context) (lvs: Ast.loop_variables Ast.marked) :
  ((loop_context -> Cfg.expression Ast.marked) -> Cfg.expression Ast.marked list) =
  match Ast.unmark lvs with
  | Ast.ValueSets lvs -> (fun translator ->
      assert false
    )
  | Ast.Ranges _ -> raise (Errors.Unimplemented ("TODO4", Ast.get_position lvs))

let rec translate_func_args (ctx: translating_context) (args: Ast.func_args) : Cfg.expression Ast.marked list =
  match args with
  | Ast.ArgList args -> List.map (fun arg ->
      translate_expression ctx arg
    ) args
  | Ast.LoopList (lvs, e) -> raise (Errors.Unimplemented ("TODO3", Ast.get_position lvs))

and translate_expression (ctx : translating_context) (f: Ast.expression Ast.marked) : Cfg.expression Ast.marked =
  Ast.same_pos_as
    (match Ast.unmark f with
     | Ast.TestInSet (negative, e, values) ->
       let new_e = translate_expression ctx e in
       let local_var = Cfg.LocalVariable.new_var () in
       let local_var_expr =  Cfg.LocalVar local_var in
       let or_chain = List.fold_left (fun or_chain set_value ->
           let equal_test = match set_value with
             | Ast.VarValue set_var ->  Cfg.Comparison (
                 Ast.same_pos_as Ast.Eq set_var,
                 Ast.same_pos_as local_var_expr e,
                 translate_variable ctx set_var
               )
             | Ast.Interval (bn,en) ->
               if Ast.unmark bn > Ast.unmark en then
                 raise (Errors.TypeError
                          (Errors.Numeric
                             (Printf.sprintf "wrong interval bounds %s"
                                (Format_ast.format_position (Ast.get_position bn)))))
               else
                 Cfg.Binop (
                   Ast.same_pos_as Ast.And bn,
                   Ast.same_pos_as (Cfg.Comparison (
                       Ast.same_pos_as Ast.Gte bn,
                       Ast.same_pos_as local_var_expr e,
                       Ast.same_pos_as (Cfg.Literal (Cfg.Int (Ast.unmark bn))) bn
                     )) bn,
                   Ast.same_pos_as (Cfg.Comparison (
                       Ast.same_pos_as Ast.Lte en,
                       Ast.same_pos_as local_var_expr e,
                       Ast.same_pos_as (Cfg.Literal (Cfg.Int (Ast.unmark en))) en
                     )) en
                 )
           in
           Ast.same_pos_as (Cfg.Binop (
               Ast.same_pos_as Ast.Or f,
               Ast.same_pos_as equal_test f,
               or_chain
             )) f
         ) (Ast.same_pos_as (Cfg.Literal (Cfg.Bool false)) f) values in
       Cfg.LocalLet (local_var, new_e, or_chain)
     | Ast.Comparison (op, e1, e2) ->
       let new_e1 = translate_expression ctx e1 in
       let new_e2 = translate_expression ctx e2 in
       Cfg.Comparison (op, new_e1, new_e2)
     | Ast.Binop (op, e1, e2) ->
       let new_e1 = translate_expression ctx e1 in
       let new_e2 = translate_expression ctx e2 in
       Cfg.Binop (op, new_e1, new_e2)
     | Ast.Unop (op, e) ->
       let new_e = translate_expression ctx e in
       Cfg.Unop (op, new_e)
     | Ast.Index (t, i) ->
       let t_var = translate_variable ctx t in
       let new_i = translate_table_index ctx i in
       Cfg.Index ((match Ast.unmark t_var with
           | Cfg.Var v -> Ast.same_pos_as v t_var
           | _ -> assert false (* should not happen *)), new_i)
     | Ast.Conditional (e1, e2, e3) ->
       let new_e1 = translate_expression ctx e1 in
       let new_e2 = translate_expression ctx e2 in
       let new_e3 = match e3 with
         | Some e3 -> Some (translate_expression ctx e3)
         | None -> None
       in
       Cfg.Conditional (new_e1, new_e2, new_e3)
     | Ast.FunctionCall (f_name, args) ->
       let f_correct = translate_function_name f_name in
       let new_args = translate_func_args ctx args in
       Cfg.FunctionCall (f_correct, new_args)
     | Ast.Literal l -> begin match l with
         | Ast.Variable var ->
           let new_var = translate_variable ctx (Ast.same_pos_as var f) in
           Ast.unmark new_var
         | Ast.Int i -> Cfg.Literal (Cfg.Int i)
         | Ast.Float f -> Cfg.Literal (Cfg.Float f)
       end
     | Ast.Loop (lvs, e) -> raise (Errors.Unimplemented ("TODO7", Ast.get_position lvs)))
    f

let translate_lvalue (ctx: translating_context) (lval: Ast.lvalue Ast.marked) : Cfg.Variable.t =
  match (Ast.unmark lval).Ast.index with
  | Some _ -> raise (Errors.Unimplemented ("TODO6", Ast.get_position lval))
  | None -> begin match
        Ast.unmark (translate_variable ctx (Ast.same_pos_as (Ast.unmark lval).Ast.var lval)) with
    | Cfg.Var var -> var
    | _ -> assert false (* should not happen *)
    end

let get_var_data (idmap: idmap) (p: Ast.program) : Cfg.variable_data Cfg.VariableMap.t =
  List.fold_left (fun var_data source_file ->
      List.fold_left (fun var_data source_file_item -> match Ast.unmark source_file_item with
          | Ast.Rule r -> List.fold_left (fun var_data formula ->
              match Ast.unmark formula with
              | Ast.SingleFormula f ->
                let ctx = { idmap } in
                let var_expr = translate_expression ctx f.Ast.formula in
                let var_lvalue = translate_lvalue ctx f.Ast.lvalue in
                Cfg.VariableMap.add var_lvalue { Cfg.var_expr = var_expr } var_data
              | Ast.MultipleFormulaes _ ->
                raise (Errors.Unimplemented ("TODO5", Ast.get_position formula))
            ) var_data r.Ast.rule_formulaes
          | _ -> var_data
        ) var_data source_file
    ) Cfg.VariableMap.empty p


let translate (p: Ast.program) : Cfg.program =
  let (var_decl_data, idmap) = get_variables_decl p in
  let var_data = get_var_data idmap p in
  assert false
