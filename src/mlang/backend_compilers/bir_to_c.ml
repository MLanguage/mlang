(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

open Bir

let none_value = "m_undefined"

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

type offset =
  | GetValueConst of int
  | GetValueVar of variable
  | PassPointer
  | None

let rec generate_variable (offset : offset) (fmt : Format.formatter)
    (var : variable) : unit =
  let mvar = var_to_mir var in
  let var_index = var.offset in
  match offset with
  | PassPointer ->
      Format.fprintf fmt "(TGV + %d/*%s*/)" var_index
        (Pos.unmark mvar.Mir.Variable.name)
  | GetValueVar offset ->
      (* TODO: boundary checks *)
      Format.fprintf fmt "TGV[%d/*%s*/ + (int)%a.value]" var_index
        (Pos.unmark mvar.Mir.Variable.name)
        (generate_variable None) offset
  | _ ->
      Format.fprintf fmt "TGV[%d/*%s*/%s]" var_index
        (Pos.unmark mvar.Mir.Variable.name)
        (match offset with
        | None -> ""
        | GetValueConst offset -> " + " ^ string_of_int offset
        | PassPointer | GetValueVar _ -> assert false)

let generate_raw_name (v : variable) : string =
  let v = var_to_mir v in
  match v.alias with Some v -> v | None -> Pos.unmark v.Mir.Variable.name

let generate_name (v : variable) : string = "v_" ^ generate_raw_name v

let rec generate_c_expr (e : expression Pos.marked) :
    string * (Mir.LocalVariable.t * expression Pos.marked) list =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let se1, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      ( Format.asprintf "%s(%s, %s)" (generate_comp_op (Pos.unmark op)) se1 se2,
        s1 @ s2 )
  | Binop (op, e1, e2) ->
      let se1, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      ( Format.asprintf "%s(%s, %s)" (generate_binop (Pos.unmark op)) se1 se2,
        s1 @ s2 )
  | Unop (op, e) ->
      let se, s = generate_c_expr e in
      (Format.asprintf "%s(%s)" (generate_unop op) se, s)
  | Index (var, e) ->
      let se, s = generate_c_expr e in
      let size =
        Option.get (var_to_mir (Pos.unmark var)).Mir.Variable.is_table
      in
      ( Format.asprintf "m_array_index(%a, %s, %d)"
          (generate_variable PassPointer)
          (Pos.unmark var) se size,
        s )
  | Conditional (e1, e2, e3) ->
      let se1, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      let se3, s3 = generate_c_expr e3 in
      (Format.asprintf "m_cond(%s, %s, %s)" se1 se2 se3, s1 @ s2 @ s3)
  | FunctionCall (PresentFunc, [ arg ]) ->
      let se, s = generate_c_expr arg in
      (Format.asprintf "m_present(%s)" se, s)
  | FunctionCall (NullFunc, [ arg ]) ->
      let se, s = generate_c_expr arg in
      (Format.asprintf "m_null(%s)" se, s)
  | FunctionCall (ArrFunc, [ arg ]) ->
      let se, s = generate_c_expr arg in
      (Format.asprintf "m_round(%s)" se, s)
  | FunctionCall (InfFunc, [ arg ]) ->
      let se, s = generate_c_expr arg in
      (Format.asprintf "m_floor(%s)" se, s)
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let se1, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      (Format.asprintf "m_max(%s, %s)" se1 se2, s1 @ s2)
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let se1, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      (Format.asprintf "m_min(%s, %s)" se1 se2, s1 @ s2)
  | FunctionCall (Multimax, [ e1; (Var v2, _) ]) ->
      let se1, s1 = generate_c_expr e1 in
      ( Format.asprintf "m_multimax(%s, %a)" se1
          (generate_variable PassPointer)
          v2,
        s1 )
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) ->
      (Format.asprintf "m_literal(%s)" (string_of_float f), [])
  | Literal Undefined -> (Format.asprintf "%s" none_value, [])
  | Var var -> (Format.asprintf "%a" (generate_variable None) var, [])
  | LocalVar lvar -> (Format.asprintf "LOCAL[%d]" lvar.Mir.LocalVariable.id, [])
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let _, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      (Format.asprintf "%s" se2, s1 @ ((lvar, e1) :: s2))

let format_local_vars_defs (fmt : Format.formatter)
    (defs : (Mir.LocalVariable.t * expression Pos.marked) list) =
  List.iter
    (fun (lvar, e) ->
      let se, _ = generate_c_expr e in
      Format.fprintf fmt "LOCAL[%d] = %s;@\n" lvar.Mir.LocalVariable.id se)
    defs

let generate_var_def (var : variable) (data : variable_data)
    (oc : Format.formatter) : unit =
  match data.var_definition with
  | SimpleVar e ->
      let se, defs = generate_c_expr e in
      Format.fprintf oc "%a%a = %s;@\n" format_local_vars_defs defs
        (generate_variable None) var se
  | TableVar (_, IndexTable es) ->
      Format.fprintf oc "%a"
        (fun fmt ->
          Mir.IndexMap.iter (fun i v ->
              let sv, defs = generate_c_expr v in
              Format.fprintf fmt "%a%a = %s;@\n" format_local_vars_defs defs
                (generate_variable (GetValueConst i))
                var sv))
        es
  | TableVar (_size, IndexGeneric (v, e)) ->
      let sv, defs = generate_c_expr e in
      Format.fprintf oc "if(m_is_defined_true(%a))@[<hov 2>{%a%a = %s;@]@;}@\n"
        (generate_variable None) v format_local_vars_defs defs
        (generate_variable (GetValueVar v))
        var sv
  | InputVar -> assert false

let generate_var_cond (cond : condition_data) (oc : Format.formatter) =
  if (fst cond.cond_error).typ = Mast.Anomaly then
    let scond, defs = generate_c_expr cond.cond_expr in
    let percent = Re.Pcre.regexp "%" in
    Format.fprintf oc
      "%acond = %s;@\n\
       if (m_is_defined_true(cond)) {@\n\
      \    printf(\"Error triggered: %a\\n\");@\n\
      \    {@\n\
      \        output->is_error = true;@\n\
      \        free(TGV);@\n\
      \        free(LOCAL);@\n\
      \        return -1;@\n\
      \    }@\n\
       }@\n"
      format_local_vars_defs defs scond
      (fun fmt err ->
        let error_descr = Mir.Error.err_descr_string err |> Pos.unmark in
        let error_descr =
          Re.Pcre.substitute ~rex:percent ~subst:(fun _ -> "%%") error_descr
        in
        Format.fprintf fmt "%s: %s" (Pos.unmark err.Mir.Error.name) error_descr)
      (fst cond.cond_error)

let fresh_cond_counter = ref 0

let rec generate_stmt (program : program) (oc : Format.formatter) (stmt : stmt)
    =
  match Pos.unmark stmt with
  | SAssign (var, vdata) -> generate_var_def var vdata oc
  | SConditional (cond, tt, ff) ->
      let pos = Pos.get_position stmt in
      let fname =
        String.map
          (fun c -> if c = '.' then '_' else c)
          (Filename.basename (Pos.get_file pos))
      in
      let cond_name =
        Format.asprintf "cond_%s_%d_%d_%d_%d_%d" fname (Pos.get_start_line pos)
          (Pos.get_start_column pos) (Pos.get_end_line pos)
          (Pos.get_end_column pos) !fresh_cond_counter
      in
      fresh_cond_counter := !fresh_cond_counter + 1;
      let scond, defs = generate_c_expr (Pos.same_pos_as cond stmt) in
      Format.fprintf oc
        "%am_value %s = %s;@\n\
         if (m_is_defined_true(%s)) {@\n\
         @[<h 4>    %a@]@\n\
         };@\n\
         if (m_is_defined_false(%s)) {@\n\
         @[<h 4>    %a@]@\n\n\
         };@\n"
        format_local_vars_defs defs cond_name scond cond_name
        (generate_stmts program) tt cond_name (generate_stmts program) ff
  | SVerif v -> generate_var_cond v oc
  | SRuleCall r ->
      let rule = RuleMap.find r program.rules in
      generate_rule_function_header ~definition:false oc rule
  | SFunctionCall (f, _) ->
      Format.fprintf oc "if(%s(output, TGV, LOCAL)) {return -1;};\n" f

and generate_stmts (program : program) (oc : Format.formatter)
    (stmts : stmt list) =
  Format.pp_print_list (generate_stmt program) oc stmts

and generate_rule_function_header ~(definition : bool) (oc : Format.formatter)
    (rule : rule) =
  let arg_type = if definition then "m_value *" else "" in
  let ret_type = if definition then "void " else "" in
  Format.fprintf oc "%sm_rule_%s(%sTGV, %sLOCAL)%s@\n" ret_type rule.rule_name
    arg_type arg_type
    (if definition then "" else ";")

let generate_rule_function (program : program) (oc : Format.formatter)
    (rule : rule) =
  Format.fprintf oc "%a@[<v 2>{@ %a@]@;}@\n"
    (generate_rule_function_header ~definition:true)
    rule (generate_stmts program) rule.rule_stmts

let generate_rule_functions (program : program) (oc : Format.formatter)
    (rules : rule RuleMap.t) =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut
    (generate_rule_function program)
    oc
    (RuleMap.bindings rules |> List.map snd)

let generate_mpp_function (program : program) (oc : Format.formatter)
    (f : function_name) =
  let stmts = FunctionMap.find f program.mpp_functions in
  Format.fprintf oc
    "@[<hv 4>int %s(m_output*output, m_value* TGV, m_value* LOCAL) {@,\
     m_value cond;@,\
     %a@,\
     return 0;@]}@,"
    f (generate_stmts program) stmts

let generate_mpp_functions (oc : Format.formatter) (program : Bir.program) =
  Bir.FunctionMap.iter
    (fun fname _ -> generate_mpp_function program oc fname)
    (Bir_interface.context_agnostic_mpp_functions program)

let generate_main_function_signature (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_extracted(m_output *output, const m_input *input)%s"
    (if add_semicolon then ";" else "")

let generate_main_function_signature_and_var_decls (p : program)
    (var_table_size : int) (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
  Format.fprintf oc "%a {@\n@[<h 4>    @\n" generate_main_function_signature
    false;
  Format.fprintf oc
    "// First we initialize the table of all the variables used in the program@\n";
  (* here, we need to generate a table that can host all the local vars. the
     index inside the table will be the id of the local var so we generate a
     table big enough so that the highest id is always in bounds *)
  let size_locals = get_locals_size p + 1 in
  Format.fprintf oc "m_value *LOCAL = malloc(%d * sizeof(m_value));@\n@\n"
    size_locals;
  Format.fprintf oc "m_value *TGV = malloc(%d * sizeof(m_value));@\n@\n"
    var_table_size;
  Format.fprintf oc
    "// Then we extract the input variables from the dictionnary:@\n%a@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "%a = input->%s;" (generate_variable None) var
           (generate_name var)))
    input_vars;

  Format.fprintf oc "m_value cond;@\n@\n"

let generate_return (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let returned_variables =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc
    "%a@\n\
     @\n\
     free(TGV);@\n\
     free(LOCAL);@\n\
     output->is_error = false;@\n\
     return 0;@]@\n\
     }"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "output->%s = %a;" (generate_name var)
           (generate_variable None) var))
    returned_variables

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc "// %s\n\n" Prelude.message;
  Format.fprintf oc "#ifndef IR_HEADER_ \n";
  Format.fprintf oc "#define IR_HEADER_ \n";
  Format.fprintf oc "#include <stdio.h>\n";
  Format.fprintf oc "#include \"m_value.h\"\n\n"

let generate_footer (oc : Format.formatter) () : unit =
  Format.fprintf oc "\n#endif /* IR_HEADER_ */"

let generate_empty_input_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "void m_empty_input(m_input *input)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_empty_input_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
  Format.fprintf oc "%a {@\n@[<h 4>    %a@]@\n};@\n@\n"
    generate_empty_input_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "input->%s = m_undefined;" (generate_name var)))
    input_vars

let generate_input_from_array_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "void m_input_from_array(m_input* input, m_value *array)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_input_from_array_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
  Format.fprintf oc "%a {@\n@[<h 4>    %a@]@\n};@\n@\n"
    generate_input_from_array_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "input->%s = array[%d];" (generate_name var) i))
    (List.mapi (fun i x -> (x, i)) input_vars)

let generate_get_input_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_get_input_index(char *name)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_input_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
  Format.fprintf oc
    "%a {@\n\
     @[<h 4>    %a@\n\
     printf(\"Input var %%s not found!\\n\", name);@\n\
     exit(-1);@]@\n\
     };@\n\
     @\n"
    generate_get_input_index_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "if (strcmp(\"%s\", name) == 0) { return %d; }"
           (generate_raw_name var) i))
    (List.mapi (fun i x -> (x, i)) input_vars)

let generate_get_input_name_from_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "char* m_get_input_name_from_index(int index)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_input_name_from_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
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
         Format.fprintf fmt "if (%d == index) { return \"%s\"; }" i
           (generate_raw_name var)))
    (List.mapi (fun i x -> (x, i)) input_vars)

let generate_get_input_num_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_num_inputs()%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_input_num_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
  Format.fprintf oc "%a {@\n@[<h 4>    return %d;@]@\n};@\n@\n"
    generate_get_input_num_prototype false (List.length input_vars)

let generate_input_type (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
  Format.fprintf oc "typedef struct m_input {@[<h 4>    %a@]@\n} m_input;@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "m_value %s; // %s" (generate_name var)
           (Pos.unmark (var_to_mir var).Mir.Variable.descr)))
    input_vars

let generate_empty_output_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "void m_empty_output(m_output* output)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_empty_output_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc
    "%a {@\n@[<h 4>    @\noutput->is_error = false;@\n%a@]@\n};@\n@\n"
    generate_empty_output_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "output->%s = m_undefined;" (generate_name var)))
    output_vars

let generate_output_to_array_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "void m_output_to_array(m_value *array, m_output* output)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_output_to_array_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc "%a {@\n@[<h 4>    %a@]@\n};@\n@\n"
    generate_output_to_array_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "array[%d] = output->%s;" i (generate_name var)))
    (List.mapi (fun i x -> (x, i)) output_vars)

let generate_get_output_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_get_output_index(char *name)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_output_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
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
           (Pos.unmark (var_to_mir var).Mir.Variable.name)
           i))
    (List.mapi (fun i x -> (x, i)) output_vars)

let generate_get_output_name_from_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "char* m_get_output_name_from_index(int index)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_output_name_from_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
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
           (Pos.unmark (var_to_mir var).Mir.Variable.name)))
    (List.mapi (fun i x -> (x, i)) output_vars)

let generate_get_output_num_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_num_outputs()%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_output_num_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc "%a {@\n@[<h 4>    return %d;@]@\n};@\n@\n"
    generate_get_output_num_prototype false (List.length output_vars)

let generate_output_type (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc
    "typedef struct m_output {@\n\
     @[<h 4>    bool is_error;@\n\
     %a@]@\n\
     } m_output;@\n\
     @\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "m_value %s; // %s" (generate_name var)
           (Pos.unmark (var_to_mir var).Mir.Variable.descr)))
    output_vars

let generate_implem_header oc header_filename =
  Format.fprintf oc "// File generated by the Mlang compiler\n\n";
  Format.fprintf oc "#include <string.h>\n";
  Format.fprintf oc "#include \"%s\"\n\n" header_filename

let generate_c_program (program : program)
    (function_spec : Bir_interface.bir_function) (filename : string) : unit =
  if Filename.extension filename <> ".c" then
    Errors.raise_error
      (Format.asprintf "Output file should have a .c extension (currently %s)"
         filename);
  let header_filename = Filename.remove_extension filename ^ ".h" in
  let _oc = open_out header_filename in
  let var_table_size = Bir.size_of_tgv () in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a" generate_header ()
    generate_input_type function_spec generate_empty_input_prototype true
    generate_input_from_array_prototype true generate_get_input_index_prototype
    true generate_get_input_num_prototype true
    generate_get_input_name_from_index_prototype true generate_output_type
    function_spec generate_output_to_array_prototype true
    generate_get_output_index_prototype true
    generate_get_output_name_from_index_prototype true
    generate_get_output_num_prototype true generate_empty_output_prototype true
    generate_main_function_signature true generate_footer ();
  close_out _oc;
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a"
    generate_implem_header header_filename
    generate_empty_input_func function_spec
    generate_input_from_array_func function_spec
    generate_get_input_index_func function_spec
    generate_get_input_name_from_index_func function_spec
    generate_get_input_num_func function_spec
    generate_output_to_array_func function_spec
    generate_get_output_index_func function_spec
    generate_get_output_name_from_index_func function_spec
    generate_get_output_num_func function_spec
    generate_empty_output_func function_spec
    (generate_rule_functions program) program.rules
    generate_mpp_functions program
    (generate_main_function_signature_and_var_decls program
       var_table_size) function_spec
    (generate_stmts program) (Bir.main_statements program)
    generate_return function_spec;
  close_out _oc[@@ocamlformat "disable"]
