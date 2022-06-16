(* Backend OCaml *)

let none_value = "m_undef"

let get_var_pos (var : Bir.variable) : int = var.Bir.offset

let get_var_alias (v : Bir.variable) : string option = (Bir.var_to_mir v).alias

let generate_comp_op (op : Mast.comp_op) : string =
  match op with
  | Mast.Gt -> "m_greater_than"
  | Mast.Gte -> "m_greater_than_equal"
  | Mast.Lt -> "m_less_than"
  | Mast.Lte -> "m_less_than_equal"
  | Mast.Eq -> "m_equal"
  | Mast.Neq -> "m_not_equal"

let generate_binop (op : Mast.binop) : string =
  match op with
  | Mast.And -> "m_and"
  | Mast.Or -> "m_or"
  | Mast.Add -> "m_add"
  | Mast.Sub -> "m_subtract"
  | Mast.Mul -> "m_multiply"
  | Mast.Div -> "m_divide"

let generate_unop (op : Mast.unop) : string =
  match op with Mast.Not -> "m_not" | Mast.Minus -> "m_neg"

let rec generate_ocaml_expr (e : Bir.expression Pos.marked) :
    string * (Mir.LocalVariable.t * Bir.expression Pos.marked) list =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let expr1, local1 = generate_ocaml_expr e1 in
      let expr2, local2 = generate_ocaml_expr e2 in
      ( Format.asprintf "(%s %s %s)"
          (generate_comp_op (Pos.unmark op))
          expr1 expr2,
        local1 @ local2 )
  | Binop (op, e1, e2) ->
      let s1, local1 = generate_ocaml_expr e1 in
      let s2, local2 = generate_ocaml_expr e2 in
      ( Format.asprintf "(%s %s %s)" (generate_binop (Pos.unmark op)) s1 s2,
        local1 @ local2 )
  | Unop (op, e) ->
      let expr, local = generate_ocaml_expr e in
      (Format.asprintf "(%s %s)" (generate_unop op) expr, local)
  | Index (var, e) ->
      let expr, local = generate_ocaml_expr e in
      let unmarked_var = Pos.unmark var in
      let size =
        Option.get (Bir.var_to_mir unmarked_var).Mir.Variable.is_table
      in
      ( Format.asprintf "(m_table_value_at_index context.tgv %d %s %d)"
          (get_var_pos unmarked_var) expr size,
        local )
  | Conditional (e1, e2, e3) ->
      let se1, s1 = generate_ocaml_expr e1 in
      let se2, s2 = generate_ocaml_expr e2 in
      let se3, s3 = generate_ocaml_expr e3 in
      (Format.asprintf "(m_cond %s  %s  %s)" se1 se2 se3, s1 @ s2 @ s3)
  | FunctionCall (PresentFunc, [ arg ]) ->
      let s, local = generate_ocaml_expr arg in
      (Format.asprintf "(m_present %s)" s, local)
  | FunctionCall (NullFunc, [ arg ]) ->
      let s, local = generate_ocaml_expr arg in
      (Format.asprintf "(m_null %s)" s, local)
  | FunctionCall (ArrFunc, [ arg ]) ->
      let s, local = generate_ocaml_expr arg in
      (Format.asprintf "(m_round %s)" s, local)
  | FunctionCall (InfFunc, [ arg ]) ->
      let s, local = generate_ocaml_expr arg in
      (Format.asprintf "(m_floor %s)" s, local)
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let s1, local1 = generate_ocaml_expr e1 in
      let s2, local2 = generate_ocaml_expr e2 in
      (Format.asprintf "(%s %s %s)" "m_max" s1 s2, local1 @ local2)
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let s1, local1 = generate_ocaml_expr e1 in
      let s2, local2 = generate_ocaml_expr e2 in
      (Format.asprintf "(%s %s %s)" "m_min" s1 s2, local1 @ local2)
  | FunctionCall (Multimax, [ e1; (Var v2, _) ]) ->
      let s1, local1 = generate_ocaml_expr e1 in
      ( Format.asprintf "(m_multimax %s context.tgv %d)" s1 (get_var_pos v2),
        local1 )
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> (
      match f with
      | 0. -> (Format.asprintf "m_zero", [])
      | 1. -> (Format.asprintf "m_one", [])
      | _ ->
          ( Format.asprintf "{undefined = false ; value = %s}"
              (string_of_float f),
            [] ))
  | Literal Undefined -> (Format.asprintf "%s" none_value, [])
  | Var var ->
      ( Format.asprintf "(Array.get context.tgv %d (*%s*))" (get_var_pos var)
          (Pos.unmark var.mir_var.name),
        [] )
  | LocalVar lvar ->
      ( Format.asprintf "(Array.get context.local_variables %d)"
          lvar.Mir.LocalVariable.id,
        [] )
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let _, local1 = generate_ocaml_expr e1 in
      let se2, local2 = generate_ocaml_expr e2 in
      (Format.asprintf "%s" se2, local1 @ ((lvar, e1) :: local2))

let format_tgv_set (variable_expression : string) (oc : Format.formatter)
    (variable_position : int) : unit =
  Format.fprintf oc "Array.set context.tgv %d %s" variable_position
    variable_expression

let format_tgv_set_with_offset (variable_position : int)
    (offset_tgv_variable : Bir.variable) (oc : Format.formatter)
    (variable_expression : string) : unit =
  Format.fprintf oc
    "Array.set context.tgv (%d +  (((*%s*) Array.get context.tgv %d).value |> \
     int_of_float)) %s"
    variable_position
    (Pos.unmark offset_tgv_variable.mir_var.name)
    (get_var_pos offset_tgv_variable)
    variable_expression

let pp_statement_separator (f : Format.formatter) () : unit =
  Format.fprintf f ";@,"

let format_local_set (oc : Format.formatter) (lvar, expr) : unit =
  let se, _ = generate_ocaml_expr expr in
  Format.fprintf oc "Array.set context.local_variables %d %s"
    lvar.Mir.LocalVariable.id se

let generate_local_defs (oc : Format.formatter)
    (defs : (Mir.LocalVariable.t * Bir.expression Pos.marked) list) : unit =
  match defs with
  | [] -> ()
  | _ :: _ ->
      Format.fprintf oc "%a;@,"
        (Format.pp_print_list ~pp_sep:pp_statement_separator format_local_set)
        defs

let generate_var_def (variable : Bir.variable) (vdata : Bir.variable_data)
    (oc : Format.formatter) : unit =
  let generate_one_var position oc (e : Bir.expression Pos.marked) : unit =
    let tgv_expression, local_defs = generate_ocaml_expr e in
    Format.fprintf oc "%a(*%s*) %a" generate_local_defs local_defs
      (Pos.unmark variable.mir_var.name)
      (format_tgv_set tgv_expression)
      position
  in
  match vdata.var_definition with
  | SimpleVar e -> generate_one_var (get_var_pos variable) oc e
  | TableVar (_, IndexTable es) ->
      let bindings_list = Mir.IndexMap.bindings es in
      Format.pp_print_list ~pp_sep:pp_statement_separator
        (fun fmt (i, v) ->
          generate_one_var (get_var_pos variable |> ( + ) i) fmt v)
        oc bindings_list
  | TableVar (_size, IndexGeneric (v, e)) ->
      let tgv_expression, local_defs = generate_ocaml_expr e in
      Format.fprintf oc
        "if (Array.get context.tgv %d (*%s*)).undefined then %a(*Table %s*)@,%a"
        (get_var_pos v)
        (Pos.unmark v.mir_var.name)
        generate_local_defs local_defs
        (Pos.unmark variable.mir_var.name)
        (format_tgv_set_with_offset (get_var_pos variable) v)
        tgv_expression
  | InputVar -> assert false

let rec generate_stmts (program : Bir.program) (oc : Format.formatter)
    (stmts : Bir.stmt list) : unit =
  Format.pp_print_list ~pp_sep:pp_statement_separator (generate_stmt program) oc
    stmts

and generate_stmt (program : Bir.program) (oc : Format.formatter)
    (stmt : Bir.stmt) : unit =
  match Pos.unmark stmt with
  | SAssign (variable, variable_data) ->
      generate_var_def variable variable_data oc
  | SConditional (cond_expression, tt, ff) ->
      let pos = Pos.get_position stmt in
      let fname =
        String.map
          (fun c -> if c = '.' then '_' else c)
          (Filename.basename (Pos.get_file pos))
      in
      let cond_name =
        Format.asprintf "cond_%s_%d_%d_%d_%d" fname (Pos.get_start_line pos)
          (Pos.get_start_column pos) (Pos.get_end_line pos)
          (Pos.get_end_column pos)
      in
      let s, _ = generate_ocaml_expr (Pos.same_pos_as cond_expression stmt) in
      Format.fprintf oc
        "@[<v 1>let %s : m_value = %s in@,\
         (match %s with@,\
         | { undefined = true ; value = _ } -> ()@,\
         | { undefined = false ; value = 0.0 }-> (@[<v 0>%a@])@,\
         | _ -> (@[<v 0>%a@]))@]" cond_name s cond_name (generate_stmts program)
        tt (generate_stmts program) ff
  | SVerif _condition_data -> Format.fprintf oc "%s" "Verif"
  | SRuleCall rule_id ->
      let rule = Mir.RuleMap.find rule_id program.rules in
      Format.fprintf oc "m_rule_%s context" rule.rule_name
  | SFunctionCall (function_name, _) ->
      Format.fprintf oc "mpp_func_%s context" function_name

let pp_function_separator (f : Format.formatter) () : unit =
  Format.fprintf f "@,@,"

let generate_mpp_function (program : Bir.program) (oc : Format.formatter)
    (f_name : Bir.function_name) : unit =
  let Bir.{mppf_stmts; _} = Bir.FunctionMap.find f_name program.mpp_functions in
  Format.fprintf oc
    "@[<v 1>let mpp_func_%s (context : m_context) : unit =@,%a@]" f_name
    (generate_stmts program) mppf_stmts

let generate_mpp_functions (oc : Format.formatter) (program : Bir.program) =
  let functions =
    Bir.FunctionMap.bindings
      (Bir_interface.context_agnostic_mpp_functions program)
  in
  let function_names, _ = List.split functions in
  Format.pp_print_list ~pp_sep:pp_function_separator
    (generate_mpp_function program)
    oc function_names

let generate_rule_method (program : Bir.program) (oc : Format.formatter)
    (rule : Bir.rule) =
  Format.fprintf oc "@[<v 1>let m_rule_%s (context : m_context) : unit =@,%a@]"
    rule.rule_name (generate_stmts program) rule.rule_stmts

let generate_rule_methods (oc : Format.formatter) (program : Bir.program) : unit
    =
  let rules = Bir.RuleMap.bindings program.rules in
  let _, rules = List.split rules in
  Format.pp_print_list ~pp_sep:pp_function_separator
    (generate_rule_method program)
    oc rules

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc "@[<v 0>open Mvalue@,@]"

let generate_input_handler (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) : unit =
  let input_vars =
    List.map fst (Bir.VariableMap.bindings function_spec.func_variable_inputs)
  in
  let get_position_and_alias variable : (int * string) option =
    let tgv_pos = get_var_pos variable in
    Option.map (fun alias -> (tgv_pos, alias)) (get_var_alias variable)
  in
  let pp_print_line fmt variable : unit =
    match get_position_and_alias variable with
    | Some (position, alias) ->
        Format.fprintf fmt
          "let tgv_positions = TgvPositionMap.add \"%s\" %d tgv_positions in"
          alias position
    | None -> ()
  in
  let pp_print_position_map fmt input_vars =
    Format.pp_print_list pp_print_line fmt
      input_vars
  in
  Format.fprintf oc
    "let input_handler (tgv : m_array) (entry_list : revenue_code list) : unit \
     =@,\
     let tgv_positions = TgvPositionMap.empty in@,\
     %a@,\
     let init_tgv_var (entry_var : revenue_code) : unit =@,\
      Array.set tgv @,\
      (TgvPositionMap.find entry_var.alias tgv_positions)@, \
      {undefined = false ; value = entry_var.value} in@,\
     List.iter init_tgv_var entry_list"
    pp_print_position_map input_vars
    (* Prévoir les cas : variable manquante, variable en trop dans entry_list, 
       variable définie n fois*)

let generate_main_function (locals_size : int) (var_table_size : int)
    (oc : Format.formatter) (program : Bir.program) : unit =
  Format.fprintf oc
    "let calculate_tax entry_list : unit =@,\
     let tgv : m_array = Array.make %i m_undef in@,\
     let local_variables : m_array = Array.make %i m_undef in@,\
     let context : m_context = {tgv; local_variables} in@,\
     input_handler tgv entry_list;@,\
     %a;" var_table_size locals_size (generate_stmts program)
    (Bir.main_statements program)

let generate_ocaml_program (program : Bir.program)
    (function_spec : Bir_interface.bir_function) (_output_file : string) =
  let _oc = open_out _output_file in
  let oc = Format.formatter_of_out_channel _oc in
  let locals_size = Bir.get_locals_size program |> ( + ) 1 in
  let var_table_size = Bir.size_of_tgv () in
  Format.fprintf oc "@[<v 0>%a@,%a@,%a@,@,%a@,@,%a@]@."
    generate_header () generate_rule_methods program generate_mpp_functions program
    generate_input_handler function_spec
    (generate_main_function locals_size var_table_size) program;
  close_out _oc
  [@@ocamlformat "disable"]
