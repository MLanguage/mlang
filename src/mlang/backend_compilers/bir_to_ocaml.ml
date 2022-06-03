(* Backend OCaml *)

let rec generate_stmts (program : Bir.program) (oc : Format.formatter)
    (stmts : Bir.stmt list) : unit =
  Format.pp_print_list (generate_stmt program) oc stmts

and generate_stmt (_program : Bir.program) (oc : Format.formatter)
    (stmt : Bir.stmt) : unit =
  Format.fprintf oc "%s"
    (match Pos.unmark stmt with
    | SAssign (variable, _variable_data) -> Pos.unmark variable.mir_var.name
    | SConditional (_expression, _tt, _ff) -> "Condition"
    | SVerif _condition_data -> "Verif"
    | SRuleCall _rule_id -> "Rule call"
    | SFunctionCall (function_name, _) -> function_name)

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
  Format.fprintf oc "@[<v 2>%s:@,%a@]" rule.rule_name (generate_stmts program)
    rule.rule_stmts

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
