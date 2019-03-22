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

type var_data = {
  var_data_decls: var_decl_data Cfg.VariableMap.t;
  var_data_name_map: idmap;
}

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

let get_variables_decl (p: Ast.program) : var_data =
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
      | None -> assert false
    ) vars out_list in
  {
    var_data_decls = vars;
    var_data_name_map = idmap;
  }

type translating_context = {
  idmap : idmap;
}

let rec translate_formula (idmap : translating_context) (f: Ast.expression Ast.marked) : Cfg.expression Ast.marked =
  Ast.map_under_mark (fun e -> match e with
      | Ast.TestInSet (negative, e, values) ->
        let new_e = translate_formula idmap e in
        let local_var = Cfg.LocalVar (Cfg.LocalVariable.new_var ()) in
        let or_chain = List.fold_left (fun or_chain set_value ->
            let equal_test = match Ast.unmark set_value with
              | Ast.VarValue set_var ->  Cfg.Comparison (
                  Ast.same_pos_as Ast.Eq set_value,
                  Ast.same_pos_as local_var e,
                  Ast.same_pos_as (Cfg.Var (get_var_from_name idmap.idmap (assert false (*set_var*)))) set_var
                )
              | Ast.Interval (b,e) -> assert false
            in
            assert false
          ) (Ast.Literal (Ast.Bool false)) in
        assert false
      | _ -> assert false
    ) f

let get_var_data (var_decl_data: var_data) (p: Ast.program) : Cfg.variable_data Cfg.VariableMap.t =
  List.fold_left (fun var_data source_file ->
      List.fold_left (fun var_data source_file_item -> match Ast.unmark source_file_item with
          | Ast.Rule r -> List.fold_left (fun var_data formula ->
              match Ast.unmark formula with
              | Ast.SingleFormula f ->
                let var_expr = translate_formula { idmap = var_decl_data.var_data_name_map } f.Ast.formula in
                assert false
              | Ast.MultipleFormulaes _ ->
                assert false
            ) var_data r.Ast.rule_formulaes
          | _ -> var_data
        ) var_data source_file
    ) Cfg.VariableMap.empty p


let translate (p: Ast.program) : Cfg.program =
  let var_decl_data = get_variables_decl p in
  let var_data = get_var_data var_decl_data p in
  assert false
