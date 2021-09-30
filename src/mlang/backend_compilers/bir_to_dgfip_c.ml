(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Mir

let none_value = "m_undefined"

module ErrorSet = Set.Make (Error)

let generate_comp_op (op : Mast.comp_op) : string =
  match op with
  | Mast.Gt -> "m_gt"
  | Mast.Gte -> "m_gte"
  | Mast.Lt -> "m_lt"
  | Mast.Lte -> "m_lte"
  | Mast.Eq -> "m_eq"
  | Mast.Neq -> "m_neq"

let generate_binop (op : Mast.binop) : string =
  match op with
  | Mast.And -> "m_and"
  | Mast.Or -> "m_or"
  | Mast.Add -> "m_add"
  | Mast.Sub -> "m_sub"
  | Mast.Mul -> "m_mul"
  | Mast.Div -> "m_div"

let generate_unop (op : Mast.unop) : string =
  match op with Mast.Not -> "m_not" | Mast.Minus -> "m_neg"

type offset = GetValueConst of int | GetValueVar of string | PassPointer | None

let generate_variable (var_indexes : int Mir.VariableMap.t) (offset : offset)
    (fmt : Format.formatter) (var : Variable.t) : unit =
  let var_index =
    match Mir.VariableMap.find_opt var var_indexes with
    | Some i -> i
    | None ->
        Errors.raise_error
          (Format.asprintf "Variable %s not found in TGV" (Pos.unmark var.Mir.Variable.name))
  in
  match offset with
  | PassPointer ->
      Format.fprintf fmt "(TGV + %d/*%s*/)" var_index (Pos.unmark var.Mir.Variable.name)
  | _ ->
      Format.fprintf fmt "TGV[%d/*%s*/%s]" var_index
        (Pos.unmark var.Mir.Variable.name)
        (match offset with
        | None -> ""
        | GetValueVar offset -> " + " ^ offset
        | GetValueConst offset -> " + " ^ string_of_int offset
        | PassPointer -> assert false)

let print_error oc (m_error : Mir.Error.t) =
  Format.fprintf oc
    {|{.kind = "%s", .major_code = "%s", .minor_code = "%s", .description = "%s", .isisf = "%s", .has_occurred = false},
          |}
    (Pos.unmark m_error.descr.kind)
    (Pos.unmark m_error.descr.major_code)
    (Pos.unmark m_error.descr.minor_code)
    (Pos.unmark m_error.descr.description)
    (Pos.unmark m_error.descr.isisf)

let generate_raw_name (v : Variable.t) : string =
  match v.alias with Some v -> v | None -> Pos.unmark v.Variable.name

let generate_name (v : Variable.t) : string = "v_" ^ generate_raw_name v

let rec generate_c_expr (e : expression Pos.marked) (var_indexes : int Mir.VariableMap.t) :
    string * (LocalVariable.t * expression Pos.marked) list =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let se1, s1 = generate_c_expr e1 var_indexes in
      let se2, s2 = generate_c_expr e2 var_indexes in
      (Format.asprintf "%s(%s, %s)" (generate_comp_op (Pos.unmark op)) se1 se2, s1 @ s2)
  | Binop (op, e1, e2) ->
      let se1, s1 = generate_c_expr e1 var_indexes in
      let se2, s2 = generate_c_expr e2 var_indexes in
      (Format.asprintf "%s(%s, %s)" (generate_binop (Pos.unmark op)) se1 se2, s1 @ s2)
  | Unop (op, e) ->
      let se, s = generate_c_expr e var_indexes in
      (Format.asprintf "%s(%s)" (generate_unop op) se, s)
  | Index (var, e) ->
      let se, s = generate_c_expr e var_indexes in
      let size = Option.get (Pos.unmark var).Mir.Variable.is_table in
      ( Format.asprintf "m_array_index(%a, %s, %d)"
          (generate_variable var_indexes PassPointer)
          (Pos.unmark var) se size,
        s )
  | Conditional (e1, e2, e3) ->
      let se1, s1 = generate_c_expr e1 var_indexes in
      let se2, s2 = generate_c_expr e2 var_indexes in
      let se3, s3 = generate_c_expr e3 var_indexes in
      (Format.asprintf "m_cond(%s, %s, %s)" se1 se2 se3, s1 @ s2 @ s3)
  | FunctionCall (PresentFunc, [ arg ]) ->
      let se, s = generate_c_expr arg var_indexes in
      (Format.asprintf "m_present(%s)" se, s)
  | FunctionCall (NullFunc, [ arg ]) ->
      let se, s = generate_c_expr arg var_indexes in
      (Format.asprintf "m_null(%s)" se, s)
  | FunctionCall (ArrFunc, [ arg ]) ->
      let se, s = generate_c_expr arg var_indexes in
      (Format.asprintf "m_round(%s)" se, s)
  | FunctionCall (InfFunc, [ arg ]) ->
      let se, s = generate_c_expr arg var_indexes in
      (Format.asprintf "m_floor(%s)" se, s)
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let se1, s1 = generate_c_expr e1 var_indexes in
      let se2, s2 = generate_c_expr e2 var_indexes in
      (Format.asprintf "m_max(%s, %s)" se1 se2, s1 @ s2)
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let se1, s1 = generate_c_expr e1 var_indexes in
      let se2, s2 = generate_c_expr e2 var_indexes in
      (Format.asprintf "m_min(%s, %s)" se1 se2, s1 @ s2)
  | FunctionCall (Multimax, [ e1; (Var v2, _) ]) ->
      let se1, s1 = generate_c_expr e1 var_indexes in
      (Format.asprintf "m_multimax(%s, %a)" se1 (generate_variable var_indexes PassPointer) v2, s1)
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> (Format.asprintf "m_literal(%s)" (string_of_float f), [])
  | Literal Undefined -> (Format.asprintf "%s" none_value, [])
  | Var var -> (Format.asprintf "%a" (generate_variable var_indexes None) var, [])
  | LocalVar lvar -> (Format.asprintf "LOCAL[%d]" lvar.LocalVariable.id, [])
  | GenericTableIndex -> (Format.asprintf "m_literal(generic_index)", [])
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let _, s1 = generate_c_expr e1 var_indexes in
      let se2, s2 = generate_c_expr e2 var_indexes in
      (Format.asprintf "%s" se2, s1 @ ((lvar, e1) :: s2))

let format_local_vars_defs (var_indexes : int Mir.VariableMap.t) (fmt : Format.formatter)
    (defs : (LocalVariable.t * expression Pos.marked) list) =
  List.iter
    (fun (lvar, e) ->
      let se, _ = generate_c_expr e var_indexes in
      Format.fprintf fmt "LOCAL[%d] = %s;@\n" lvar.LocalVariable.id se)
    defs

let generate_var_def (var_indexes : int Mir.VariableMap.t) (var : Mir.Variable.t)
    (data : Mir.variable_data) (oc : Format.formatter) : unit =
  match data.var_definition with
  | SimpleVar e ->
      let se, defs = generate_c_expr e var_indexes in
      Format.fprintf oc "%a%a = %s;@\n"
        (format_local_vars_defs var_indexes)
        defs
        (generate_variable var_indexes None)
        var se
  | TableVar (_, IndexTable es) ->
      Format.fprintf oc "%a"
        (fun fmt ->
          IndexMap.iter (fun i v ->
              let sv, defs = generate_c_expr v var_indexes in
              Format.fprintf fmt "%a%a = %s;@\n"
                (format_local_vars_defs var_indexes)
                defs
                (generate_variable var_indexes (GetValueConst i))
                var sv))
        es
  | TableVar (size, IndexGeneric e) ->
      let sv, defs = generate_c_expr e var_indexes in
      Format.fprintf oc
        "for (int generic_index=0; generic_index < %d; generic_index++) {@\n\
        \ @[<h 4> %a%a = %s;@]@\n\
        \ }@\n"
        size
        (format_local_vars_defs var_indexes)
        defs
        (generate_variable var_indexes (GetValueVar "generic_index"))
        var sv
      (* Errors.raise_spanned_error "generic index table definitions not supported in C the backend"
       *   (Pos.get_position e) *)
  | InputVar -> assert false

let generate_var_cond (var_indexes : int Mir.VariableMap.t) (cond : condition_data)
    (oc : Format.formatter) =
  let scond, defs = generate_c_expr cond.cond_expr var_indexes in
  let percent = Re.Pcre.regexp "%" in
  Format.fprintf oc
    {|
    %acond = %s;
    if (m_is_defined_true(cond))
    {%a%a
    }
|}
    (format_local_vars_defs var_indexes)
    defs scond
    (* A019:anomalie :"A":"019":"00":"ATTENTION CALCUL NON EFFECTUE PAR L'ESI":"N"; *)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt err ->
         let error_descr =
           (Pos.unmark @@ err.Mir.Error.descr.kind)
           ^ (Pos.unmark @@ err.Mir.Error.descr.major_code)
           ^ Pos.unmark @@ err.Mir.Error.descr.minor_code
         in
         let error_descr = Re.Pcre.substitute ~rex:percent ~subst:(fun _ -> "%%") error_descr in
         Format.fprintf fmt
           {|
        output->errors[m_get_error_index("%s")].has_occurred = true;|}
           error_descr))
    cond.cond_errors
    (fun oc () ->
      if List.exists (fun err -> err.Mir.Error.typ = Mast.Anomaly) cond.cond_errors then
        Format.fprintf oc
          {|
        output->is_error = true;
        #ifndef ANOMALY_LIMIT
        free(TGV);
        free(LOCAL);
        return -1;
        #else /* ANOMALY_LIMIT */
        if (anomaly_count >= max_anomalies) {
            free(TGV);
            free(LOCAL);
            return -1;
        }
        #endif /* ANOMALY_LIMIT */
  |})
    ()

let fresh_cond_counter = ref 0

let rec generate_stmt (program : Bir.program) (var_indexes : int Mir.VariableMap.t)
    (oc : Format.formatter) (stmt : Bir.stmt) =
  match Pos.unmark stmt with
  | Bir.SAssign (var, vdata) -> generate_var_def var_indexes var vdata oc
  | SConditional (cond, tt, ff) ->
      let pos = Pos.get_position stmt in
      let fname =
        String.map (fun c -> if c = '.' then '_' else c) (Filename.basename (Pos.get_file pos))
      in
      let cond_name =
        Format.asprintf "cond_%s_%d_%d_%d_%d_%d" fname (Pos.get_start_line pos)
          (Pos.get_start_column pos) (Pos.get_end_line pos) (Pos.get_end_column pos)
          !fresh_cond_counter
      in
      fresh_cond_counter := !fresh_cond_counter + 1;
      let scond, defs = generate_c_expr (Pos.same_pos_as cond stmt) var_indexes in
      Format.fprintf oc
        "%am_value %s = %s;@\n\
         if (m_is_defined_true(%s)) {@\n\
         @[<h 4>    %a@]@\n\
         };@\n\
         if (m_is_defined_false(%s)) {@\n\
         @[<h 4>    %a@]@\n\n\
         };@\n"
        (format_local_vars_defs var_indexes)
        defs cond_name scond cond_name
        (generate_stmts program var_indexes)
        tt cond_name
        (generate_stmts program var_indexes)
        ff
  | SVerif v -> generate_var_cond var_indexes v oc
  | SRuleCall r ->
      let rule = Bir.RuleMap.find r program.rules in
      generate_rule_function_header ~definition:false oc rule

and generate_stmts (program : Bir.program) (var_indexes : int Mir.VariableMap.t)
    (oc : Format.formatter) (stmts : Bir.stmt list) =
  Format.pp_print_list (generate_stmt program var_indexes) oc stmts

and generate_rule_function_header ~(definition : bool) (oc : Format.formatter) (rule : Bir.rule) =
  let arg_type = if definition then "m_value *" else "" in
  let ret_type = if definition then "void " else "" in
  Format.fprintf oc "%sm_rule_%s(%sTGV, %sLOCAL)%s@\n" ret_type rule.rule_name arg_type arg_type
    (if definition then "" else ";")

let generate_rule_function (program : Bir.program) (var_indexes : int Mir.VariableMap.t)
    (oc : Format.formatter) (rule : Bir.rule) =
  Format.fprintf oc "%a@[<v 2>{@ %a@]@;}@\n"
    (generate_rule_function_header ~definition:true)
    rule
    (generate_stmts program var_indexes)
    rule.rule_stmts

let generate_rule_functions (program : Bir.program) (var_indexes : int Mir.VariableMap.t)
    (oc : Format.formatter) (rules : Bir.rule Bir.RuleMap.t) =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut
    (generate_rule_function program var_indexes)
    oc
    (Bir.RuleMap.bindings rules |> List.map snd)

let generate_main_function_signature (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "int m_extracted(m_output *output, const m_input *input)%s"
    (if add_semicolon then ";" else "")

let get_variables_indexes (p : Bir.program) (function_spec : Bir_interface.bir_function) :
    int Mir.VariableMap.t * int =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  let assigned_variables =
    List.map snd (Mir.VariableDict.bindings (Bir.get_assigned_variables p))
  in
  let output_vars = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  let all_relevant_variables =
    List.fold_left
      (fun acc var -> Mir.VariableMap.add var () acc)
      Mir.VariableMap.empty
      (input_vars @ assigned_variables @ output_vars)
  in
  let counter = ref 0 in
  let var_indexes =
    VariableMap.mapi
      (fun var _ ->
        let id = !counter in
        let size = match var.Mir.Variable.is_table with None -> 1 | Some size -> size in
        counter := !counter + size;
        id)
      all_relevant_variables
  in
  (var_indexes, !counter)

let generate_main_function_signature_and_var_decls (p : Bir.program)
    (var_indexes : int Mir.VariableMap.t) (var_table_size : int) (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  Format.fprintf oc "%a {@\n@[<h 4>    @\n" generate_main_function_signature false;
  Format.fprintf oc "// First we initialize the table of all the variables used in the program@\n";
  (* here, we need to generate a table that can host all the local vars. the index inside the table
     will be the id of the local var so we generate a table big enough so that the highest id is
     always in bounds *)
  let size_locals =
    List.hd
      (List.rev
         (List.sort compare
            (List.map
               (fun (x, _) -> x.LocalVariable.id)
               (Mir.LocalVariableMap.bindings (Bir.get_local_variables p)))))
    + 1
  in
  Format.fprintf oc "m_value *LOCAL = malloc(%d * sizeof(m_value));@\n@\n" size_locals;
  Format.fprintf oc "m_value *TGV = malloc(%d * sizeof(m_value));@\n@\n" var_table_size;
  Format.fprintf oc "// Then we extract the input variables from the dictionnary:@\n%a@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "%a = input->%s;"
           (generate_variable var_indexes None)
           var (generate_name var)))
    input_vars;

  Format.fprintf oc "m_value cond;@\n@\n";
  Format.fprintf oc
    {|
    #ifdef ANOMALY_LIMIT
    int anomaly_count = 0;
    int max_anomalies = ANOMALY_LIMIT;
    #endif /* ANOMALY_LIMIT */
  |}

let generate_return (var_indexes : int Mir.VariableMap.t) (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let returned_variables = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.fprintf oc
    "%a@\n@\nfree(TGV);@\nfree(LOCAL);@\noutput->is_error = false;@\nreturn 0;@]@\n}"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "output->%s = %a;" (generate_name var)
           (generate_variable var_indexes None)
           var))
    returned_variables

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc "// %s\n\n" Prelude.message;
  Format.fprintf oc "#ifndef IR_HEADER_ \n";
  Format.fprintf oc "#define IR_HEADER_ \n";
  Format.fprintf oc "#include \"m_value.h\"\n";
  Format.fprintf oc "#include \"m_error.h\"\n";
  Format.fprintf oc "#include <stdio.h>\n\n"

let generate_footer (oc : Format.formatter) () : unit =
  Format.fprintf oc "\n#endif /* IR_HEADER_ */"

let generate_get_error_index_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "int m_get_error_index(char *name)%s" (if add_semicolon then ";\n\n" else "")

let generate_get_error_index_func (oc : Format.formatter) (errors : ErrorSet.t) =
  let error_counter = ref 0 in
  Format.fprintf oc
    "%a {@\n%a @\n    printf(\"Error %%s not found!\", name);@\n    exit(-1); @\n}@\n"
    generate_get_error_index_prototype false
    (fun oc () ->
      ErrorSet.iter
        (fun (error : Error.t) ->
          Format.fprintf oc "    if (strcmp(\"%s%s%s\", name) == 0) { return %d; } \n"
            (Pos.unmark error.descr.kind)
            (Pos.unmark error.descr.major_code)
            (Pos.unmark error.descr.minor_code)
            !error_counter;
          error_counter := !error_counter + 1)
        errors)
    ()

let generate_get_error_count_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "int m_num_errors()%s" (if add_semicolon then ";\n\n" else "")

let generate_get_error_count_func (oc : Format.formatter) (errors : ErrorSet.t) =
  Format.fprintf oc
    {|
%a {
    return %d;
}

|}
    generate_get_error_count_prototype false (ErrorSet.cardinal errors)

let generate_empty_input_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "void m_empty_input(m_input *input)%s" (if add_semicolon then ";\n\n" else "")

let generate_empty_input_func (oc : Format.formatter) (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  Format.fprintf oc "%a {@\n@[<h 4>    %a@]@\n};@\n@\n" generate_empty_input_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var -> Format.fprintf fmt "input->%s = m_undefined;" (generate_name var)))
    input_vars

let generate_input_from_array_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "void m_input_from_array(m_input* input, m_value *array)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_input_from_array_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  Format.fprintf oc "%a {@\n@[<h 4>    %a@]@\n};@\n@\n" generate_input_from_array_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) -> Format.fprintf fmt "input->%s = array[%d];" (generate_name var) i))
    (List.mapi (fun i x -> (x, i)) input_vars)

let generate_get_input_index_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "int m_get_input_index(char *name)%s" (if add_semicolon then ";\n\n" else "")

let generate_get_input_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  Format.fprintf oc
    "%a {@\n@[<h 4>    %a@\nprintf(\"Input var %%s not found!\\n\", name);@\nexit(-1);@]@\n};@\n@\n"
    generate_get_input_index_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "if (strcmp(\"%s\", name) == 0) { return %d; }" (generate_raw_name var)
           i))
    (List.mapi (fun i x -> (x, i)) input_vars)

let generate_get_input_name_from_index_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "char* m_get_input_name_from_index(int index)%s"
    (if add_semicolon then ";\n" else "")

let generate_get_input_name_from_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  Format.fprintf oc
    "%a {@\n\
     @[<h 4>    %a@\n\
     printf(\"Input int %%d not found!\\n\", index);@\n\
     exit(-1);@]@\n\
     };@\n\
     @\n"
    generate_get_input_name_from_index_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "if (%d == index) { return \"%s\"; }" i (generate_raw_name var)))
    (List.mapi (fun i x -> (x, i)) input_vars)

let generate_get_input_num_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "int m_num_inputs()%s" (if add_semicolon then ";\n\n" else "")

let generate_get_input_num_func (oc : Format.formatter) (function_spec : Bir_interface.bir_function)
    =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  Format.fprintf oc "%a {@\n@[<h 4>    return %d;@]@\n};@\n@\n" generate_get_input_num_prototype
    false (List.length input_vars)

let generate_input_type (oc : Format.formatter) (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  Format.fprintf oc "typedef struct m_input {@[<h 2>%a@]@\n} m_input;@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "m_value %s; // %s" (generate_name var) (Pos.unmark var.Variable.descr)))
    input_vars

let generate_empty_output_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "void m_empty_output(m_output* output)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_empty_output_func (oc : Format.formatter) (function_spec : Bir_interface.bir_function)
    =
  let output_vars = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.fprintf oc "%a {@\n@[<h 4>    @\noutput->is_error = false;@\n%a@]@\n};@\n@\n"
    generate_empty_output_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var -> Format.fprintf fmt "output->%s = m_undefined;" (generate_name var)))
    output_vars

let generate_output_to_array_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "void m_output_to_array(m_value *array, m_output* output)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_output_to_array_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.fprintf oc "%a {@\n@[<h 4>    %a@]@\n};@\n@\n" generate_output_to_array_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) -> Format.fprintf fmt "array[%d] = output->%s;" i (generate_name var)))
    (List.mapi (fun i x -> (x, i)) output_vars)

let generate_get_output_index_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "int m_get_output_index(char *name)%s" (if add_semicolon then ";\n\n" else "")

let generate_get_output_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.fprintf oc
    "%a {@\n\
     @[<h 4>    %a@\n\
     printf(\"Output var %%s not found!\\n\", name);@\n\
     exit(-1);@]@\n\
     };@\n\
     @\n"
    generate_get_output_index_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "if (strcmp(\"%s\", name) == 0) { return %d; }"
           (Pos.unmark var.Mir.Variable.name)
           i))
    (List.mapi (fun i x -> (x, i)) output_vars)

let generate_get_output_name_from_index_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "char* m_get_output_name_from_index(int index)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_output_name_from_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.fprintf oc
    "%a {@\n\
     @[<h 4>    %a@\n\
     printf(\"Output index %%d not found!\\n\", index);@\n\
     exit(-1);@]@\n\
     };@\n\
     @\n"
    generate_get_output_name_from_index_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "if (index == %d) { return \"%s\"; }" i
           (Pos.unmark var.Mir.Variable.name)))
    (List.mapi (fun i x -> (x, i)) output_vars)

let generate_get_output_num_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "int m_num_outputs()%s" (if add_semicolon then ";\n\n" else "")

let generate_get_output_num_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.fprintf oc "%a {@\n@[<h 4>    return %d;@]@\n};@\n@\n" generate_get_output_num_prototype
    false (List.length output_vars)

let generate_output_type (oc : Format.formatter) (function_spec : Bir_interface.bir_function) =
  let output_vars = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.fprintf oc
    "@[<v 2>typedef struct m_output {@,\
     m_error *errors;@,\
     bool is_error;@,\
     %a@.@[<h>}@ m_output;@]@]@\n\
     @\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "m_value %s; // %s" (generate_name var) (Pos.unmark var.Variable.descr)))
    output_vars

let generate_implem_header oc header_filename =
  Format.fprintf oc "// File generated by the Mlang compiler\n\n";
  Format.fprintf oc "#include \"%s\"\n\n" header_filename;
  Format.fprintf oc "#include <string.h>\n"

let generate_cond_table _ v error_set =
  List.fold_left (fun acc err -> ErrorSet.add err acc) error_set v.cond_errors

let generate_c_program (program : Bir.program) (function_spec : Bir_interface.bir_function)
    (filename : string) (_vm : Dgfip_varid.var_id_map) : unit =
  if Filename.extension filename <> ".c" then
    Errors.raise_error
      (Format.asprintf "Output file should have a .c extension (currently %s)" filename);
  let header_filename = Filename.remove_extension filename ^ ".h" in
  let _oc = open_out header_filename in
  let var_indexes, var_table_size = get_variables_indexes program function_spec in
  let oc = Format.formatter_of_out_channel _oc in
  let error_set =
    VariableMap.fold generate_cond_table program.mir_program.program_conds ErrorSet.empty
  in
  let conds oc () =
    Format.fprintf oc "typedef m_error Errors[%d]; @\n@\n" (ErrorSet.cardinal error_set)
  in
  Format.fprintf oc "%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a" generate_header () conds ()
    generate_input_type function_spec generate_empty_input_prototype true
    generate_input_from_array_prototype true generate_get_input_index_prototype true
    generate_get_input_num_prototype true generate_get_input_name_from_index_prototype true
    generate_output_type function_spec generate_output_to_array_prototype true
    generate_get_output_index_prototype true generate_get_output_name_from_index_prototype true
    generate_get_output_num_prototype true generate_empty_output_prototype true
    generate_get_error_index_prototype true generate_get_error_count_prototype true
    generate_main_function_signature true generate_footer ();
  close_out _oc;
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a" generate_implem_header header_filename
    generate_get_error_index_func error_set generate_get_error_count_func error_set
    generate_empty_input_func function_spec generate_input_from_array_func function_spec
    generate_get_input_index_func function_spec generate_get_input_name_from_index_func
    function_spec generate_get_input_num_func function_spec generate_output_to_array_func
    function_spec generate_get_output_index_func function_spec
    generate_get_output_name_from_index_func function_spec generate_get_output_num_func
    function_spec generate_empty_output_func function_spec
    (generate_rule_functions program var_indexes)
    program.rules
    (generate_main_function_signature_and_var_decls program var_indexes var_table_size)
    function_spec
    (generate_stmts program var_indexes)
    program.statements (generate_return var_indexes) function_spec;
  close_out _oc
