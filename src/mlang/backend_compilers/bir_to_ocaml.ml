(* Backend OCaml *)

let none_value = "m_undef"

let get_var_pos (var : Bir.variable) : int = var.Bir.offset

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
  match op with Mast.Not -> "mNot" | Mast.Minus -> "mNeg"

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
  | Index (_var, e) ->
      let _expr, local = generate_ocaml_expr e in
      ("Index", local)
  | Conditional (e1, e2, e3) ->
      let se1, s1 = generate_ocaml_expr e1 in
      let se2, s2 = generate_ocaml_expr e2 in
      let se3, s3 = generate_ocaml_expr e3 in
      (Format.asprintf "(m_cond %s  %s  %s)" se1 se2 se3, s1 @ s2 @ s3)
  | FunctionCall (PresentFunc, [ arg ]) ->
      let _expr, local = generate_ocaml_expr arg in
      ("PresentFunc", local)
  | FunctionCall (NullFunc, [ arg ]) ->
      let _expr, local = generate_ocaml_expr arg in
      ("NullFunc", local)
  | FunctionCall (ArrFunc, [ arg ]) ->
      let _expr, local = generate_ocaml_expr arg in
      ("ArrFunc", local)
  | FunctionCall (InfFunc, [ arg ]) ->
      let _expr, local = generate_ocaml_expr arg in
      ("InfFunc", local)
  | FunctionCall (MaxFunc, [ e1; _e2 ]) ->
      let _s1, local1 = generate_ocaml_expr e1 in
      ("MaxFunc", local1)
  | FunctionCall (MinFunc, [ e1; _e2 ]) ->
      let _s1, local1 = generate_ocaml_expr e1 in
      ("MinFunc", local1)
  | FunctionCall (Multimax, [ e1; (Var _v2, _) ]) ->
      let _s1, local1 = generate_ocaml_expr e1 in
      ("Multimax", local1)
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> (
      match f with
      | 0. -> (Format.asprintf "m_zero", [])
      | 1. -> (Format.asprintf "m_one", [])
      | _ ->
          (Format.asprintf "{undefined=false;value=%s}" (string_of_float f), [])
      )
  | Literal Undefined -> (Format.asprintf "%s" none_value, [])
  | Var var ->
      ( Format.asprintf "(get tgv %d (*%s*))" (get_var_pos var)
          (Pos.unmark var.mir_var.name),
        [] )
  | LocalVar _lvar -> ("localvar", []) (*TODO*)
  | GenericTableIndex -> ("generic table index", []) (* TODO *)
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let _, local1 = generate_ocaml_expr e1 in
      let se2, local2 = generate_ocaml_expr e2 in
      (Format.asprintf "%s" se2, local1 @ ((lvar, e1) :: local2))

let format_tgv_set (variable_expression : string) (oc : Format.formatter)
    (variable_position : int) : unit =
  Format.fprintf oc "Array.set tgv %d %s;@,"
    variable_position variable_expression

let format_local_defs (oc : Format.formatter)
    (defs : (Mir.LocalVariable.t * Bir.expression Pos.marked) list) : unit =
  Format.pp_print_list
    (fun fmt (lvar, expr) ->
      let se, _ = generate_ocaml_expr expr in
      Format.fprintf fmt
        "Array.set local_variables %d %s;@,"
        lvar.Mir.LocalVariable.id se)
    oc defs

let generate_var_def (variable : Bir.variable) (vdata : Bir.variable_data)
    (oc : Format.formatter) : unit =
  match vdata.var_definition with
  | SimpleVar e ->
      let tgv_expression, local_defs = generate_ocaml_expr e in
      Format.fprintf oc "%a(*%s*) %a" format_local_defs local_defs
        (Pos.unmark variable.mir_var.name)
        (format_tgv_set tgv_expression)
        (get_var_pos variable)
  | TableVar (_, IndexTable es) ->
      Format.fprintf oc "%a"
        (fun fmt ->
          Mir.IndexMap.iter (fun i v ->
              let tgv_expression, local_defs = generate_ocaml_expr v in
              Format.fprintf fmt "%a(*%s*) %a" format_local_defs local_defs
                (Pos.unmark variable.mir_var.name)
                (format_tgv_set tgv_expression)
                (get_var_pos variable |> ( + ) i)))
        es
  | TableVar (size, IndexGeneric e) ->
      let tgv_expression, local_defs = generate_ocaml_expr e in
      let list_pos =
        let rec integer_range first_item last_item =
          if first_item > last_item then []
          else first_item :: integer_range (first_item + 1) last_item
        in
        integer_range (get_var_pos variable) (get_var_pos variable + size)
      in
      let aux_print_list (list_pos : int list) (oc : Format.formatter)
          (tgv_expression : string) =
        List.iter (format_tgv_set tgv_expression oc) list_pos
      in
      Format.fprintf oc "%a(*Table %s*)@,%a" format_local_defs local_defs
        (Pos.unmark variable.mir_var.name)
        (aux_print_list list_pos) tgv_expression
  | InputVar -> assert false

let rec generate_stmts (program : Bir.program) (oc : Format.formatter)
    (stmts : Bir.stmt list) : unit =
  Format.pp_print_list ~pp_sep:Format.pp_print_if_newline
    (generate_stmt program) oc stmts

and generate_stmt (program : Bir.program) (oc : Format.formatter)
    (stmt : Bir.stmt) : unit =
  match Pos.unmark stmt with
  | SAssign (variable, variable_data) ->
      generate_var_def variable variable_data oc
  | SConditional (_expression, tt, ff) ->
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
      Format.fprintf oc "@[<v 1>Condition %s :@,true ->@,%a@,false ->@,%a@]"
        cond_name (generate_stmts program) tt (generate_stmts program) ff
  | SVerif _condition_data -> Format.fprintf oc "%s" "Verif"
  | SRuleCall _rule_id -> Format.fprintf oc "Rule %i call" _rule_id
  | SFunctionCall (function_name, _) -> Format.fprintf oc "%s" function_name

let generate_mpp_function (program : Bir.program) (oc : Format.formatter)
    (function_name : string) : unit =
  let stmts = Bir.FunctionMap.find function_name program.mpp_functions in
  Format.fprintf oc "@[<v 1>%s:@,%a@]@," function_name (generate_stmts program)
    stmts

let generate_mpp_functions (oc : Format.formatter) (program : Bir.program) =
  let functions =
    Bir.FunctionMap.bindings
      (Bir_interface.context_agnostic_mpp_functions program)
  in
  let function_names, _ = List.split functions in
  Format.pp_print_list (generate_mpp_function program) oc function_names

let generate_rule_method (program : Bir.program) (oc : Format.formatter)
    (rule : Bir.rule) =
  Format.fprintf oc
    "@[<v 1>let m_rule_%s (context : m_context) : unit =@,%a@]@," rule.rule_name
    (generate_stmts program) rule.rule_stmts

let generate_rule_methods (oc : Format.formatter) (program : Bir.program) : unit
    =
  let rules = Bir.RuleMap.bindings program.rules in
  let _, rules = List.split rules in
  Format.pp_print_list (generate_rule_method program) oc rules

let generate_header (locals_size : int) (oc : Format.formatter)
    (var_table_size : int) : unit =
  Format.fprintf oc
    "@[<v 0>type m_value = {undefined : bool; value : float}@,\
     type m_array = m_value array@,\
     type m_context = m_value list@,\
     let m_undef : m_value = {undefined = true ; value = 0.0}@,\
     let m_zero  : m_value = {undefined = false; value = 0.0}@,\
     let m_one   : m_value = {undefined = false; value = 1.0}@,\
     let tgv : m_array = Array.make %i m_undef@,\
     let local_variables : m_array = Array.make %i m_undef@,\
     @]"
    var_table_size locals_size

let generate_ocaml_program (program : Bir.program)
    (_function_spec : Bir_interface.bir_function) (_output_file : string) =
  let _oc = open_out _output_file in
  let oc = Format.formatter_of_out_channel _oc in
  let locals_size = Bir.get_locals_size program |> ( + ) 1 in
  let var_table_size = Bir.size_of_tgv () in
  Format.fprintf oc "@[<v 0>%a@,%a@,(*@,%a@,%a@]*)@."
    (generate_header locals_size)
    var_table_size generate_rule_methods program generate_mpp_functions program
    (generate_stmts program)
    (Bir.main_statements program);
  close_out _oc
  [@@ocamlformat disable]