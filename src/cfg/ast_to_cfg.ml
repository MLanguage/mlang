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

module ParamsMap = Map.Make(Char)

type loop_param_value =
  | VarName of Ast.variable_name
  | RangeInt of int

let format_loop_param_value (v: loop_param_value) : string = match v with
  | VarName v -> v
  | RangeInt i -> string_of_int i

type loop_context = loop_param_value ParamsMap.t

type loop_domain = loop_param_value list ParamsMap.t

let format_loop_context (ld: loop_context) : string = ParamsMap.fold (fun param value acc ->
    acc ^ "; " ^ (Printf.sprintf "%c=" param)  ^
    format_loop_param_value value
  ) ld ""


let format_loop_domain (ld: loop_domain) : string = ParamsMap.fold (fun param values acc ->
    acc ^ "; " ^ (Printf.sprintf "%c=" param)  ^
    (String.concat "," (List.map (fun value -> format_loop_param_value value) values))
  ) ld ""

module VarNameToID = Map.Make(String)
type idmap = Cfg.Variable.t VarNameToID.t

type translating_context = {
  idmap : idmap;
  lc: loop_context option;
}


let get_var_from_name (d:Cfg.Variable.t VarNameToID.t) (name:Ast.variable_name Ast.marked) : Cfg.Variable.t =
  try VarNameToID.find (Ast.unmark name) d with
  | Not_found ->
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
      try
        let data = Cfg.VariableMap.find out_var vars in
        Cfg.VariableMap.add out_var { data with var_decl_io = Output } vars
      with
      | Not_found -> assert false (* should not happen *)
    ) vars out_list in
  (vars, idmap)

let rec translate_variable (ctx: translating_context) (var: Ast.variable Ast.marked) : Cfg.expression Ast.marked =
  match Ast.unmark var with
  | Ast.Normal name ->
    Ast.same_pos_as (Cfg.Var (get_var_from_name ctx.idmap (Ast.same_pos_as name var))) var
  | Ast.Generic gen_name ->
    if List.length gen_name.Ast.parameters == 0 then
      translate_variable ctx (Ast.same_pos_as (Ast.Normal gen_name.Ast.base) var)
    else match ctx.lc with
      | None ->
        raise (Errors.TypeError
                 (Errors.LoopParam "variable contains loop parameters but is not used inside a loop context"))
      | Some lc ->
        let new_var_name = ParamsMap.fold (fun param value var_name ->
            match value with
            | VarName value ->
              Str.replace_first (Str.regexp (Printf.sprintf "%c" param)) value var_name
            | RangeInt i ->
              Str.replace_first (Str.regexp (Printf.sprintf "%c" param)) (string_of_int i) var_name
          ) lc gen_name.Ast.base
        in
        translate_variable ctx (Ast.same_pos_as (Ast.Normal new_var_name) var)

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
  | "max" -> Cfg.MaxFunc
  | "abs" -> Cfg.AbsFunc
  | "positif" -> Cfg.GtzFunc
  | "positif_ou_nul" -> Cfg.GtezFunc
  | "null" -> Cfg.NullFunc
  | x -> raise (Errors.TypeError (
      Errors.Function (
        Printf.sprintf "unknown function %s %s" x (Format_ast.format_position (Ast.get_position f_name))
      )))

let rec iterate_all_combinations (ld: loop_domain) (acc: loop_context list) : loop_context list =
  ParamsMap.fold (fun param values acc ->
      match values with
      | [] -> ParamsMap.empty::acc
      | hd::[] ->
        let new_ld = ParamsMap.remove param ld in
        let all_contexts = iterate_all_combinations new_ld acc in
        if List.length all_contexts = 0 then
          (ParamsMap.singleton param hd)::acc
        else
          (List.map (fun c -> ParamsMap.add param hd c) all_contexts)@acc
      | hd::tl ->
        let new_ld = ParamsMap.add param tl ld in
        let all_contexts_minus_hd_val_for_param = iterate_all_combinations new_ld acc in
        let new_ld = ParamsMap.add param [hd] ld in
        let all_context_with_hd_val_for_param = iterate_all_combinations new_ld acc in
        all_context_with_hd_val_for_param@all_contexts_minus_hd_val_for_param@acc
    ) ld acc

let rec make_range_list (i1: int) (i2: int) : loop_param_value list =
  if i1 > i2 then [] else
    let tl = make_range_list (i1 + 1) i2 in
    (RangeInt i1)::tl

let translate_loop_variables (ctx: translating_context) (lvs: Ast.loop_variables Ast.marked) :
  ((loop_context -> Cfg.expression Ast.marked) -> Cfg.expression Ast.marked list) =
  match Ast.unmark lvs with
  | Ast.ValueSets lvs -> (fun translator ->
      let varying_domain = List.fold_left (fun domain (param, values) ->
          let values = List.map (fun value -> match value with
              | Ast.VarParam v -> VarName (Ast.unmark v)
              | Ast.IntervalLoop _ -> assert false (* should not happen *)
            ) values in
          ParamsMap.add (Ast.unmark param) values domain
        ) ParamsMap.empty lvs in
      List.map (fun lc -> translator lc) (iterate_all_combinations varying_domain [])
    )
  | Ast.Ranges lvs -> (fun translator ->
      let varying_domain = List.fold_left (fun domain (param, values) ->
          let values = List.map (fun value -> match value with
              | Ast.VarParam v -> assert false (* should not happen *)
              | Ast.IntervalLoop (i1, i2) -> make_range_list (Ast.unmark i1) (Ast.unmark i2)
            ) values in
          ParamsMap.add (Ast.unmark param) (List.flatten values) domain
        ) ParamsMap.empty lvs in
      List.map (fun lc -> translator lc) (iterate_all_combinations varying_domain [])
    )

let rec translate_func_args (ctx: translating_context) (args: Ast.func_args) : Cfg.expression Ast.marked list =
  match args with
  | Ast.ArgList args -> List.map (fun arg ->
      translate_expression ctx arg
    ) args
  | Ast.LoopList (lvs, e) ->
    let loop_context_provider = translate_loop_variables ctx lvs in
    let translator = fun lc ->
      let new_ctx = {ctx with lc = Some lc } in
      translate_expression new_ctx e
    in
    loop_context_provider translator

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
       let or_chain = if negative then
           Ast.same_pos_as (Cfg.Unop (Ast.Not, or_chain)) or_chain
         else or_chain
       in
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
                let ctx = { idmap; lc = None } in
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
  raise (Errors.Unimplemented ("TODO6", Ast.no_pos))
