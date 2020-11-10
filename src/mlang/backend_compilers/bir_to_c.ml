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

type offset = GetValue of int | PassPointer | None

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
        ( match offset with
        | None -> ""
        | GetValue offset -> " + " ^ string_of_int offset
        | PassPointer -> assert false )

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
  | LocalVar lvar -> (Format.asprintf "v_%d" lvar.LocalVariable.id, [])
  | GenericTableIndex -> (Format.asprintf "generic_index", [])
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
      Format.fprintf fmt "v_%d = %s;@\n" lvar.LocalVariable.id se)
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
                (generate_variable var_indexes (GetValue i))
                var sv))
        es
  | TableVar (_size, IndexGeneric e) ->
      (* Format.asprintf "for (int generic_index=0; generic_index < %d; generic_index++) {@\n\ @[<h
         4> %a = %a;@]@\n\ }@\n" size generate_variable var generate_c_expr e *)
      Errors.raise_spanned_error "generic index table definitions not supported in C the backend"
        (Pos.get_position e)
  | InputVar -> assert false

let generate_var_cond (var_indexes : int Mir.VariableMap.t) (cond : condition_data)
    (oc : Format.formatter) =
  let scond, defs = generate_c_expr cond.cond_expr var_indexes in
  let percent = Re.Pcre.regexp "%" in
  Format.fprintf oc
    "%acond = %s;@\n\
     if (m_is_defined_true(cond)) {@\n\
    \    printf(\"Error triggered: %a\\n\");@\n\
    \    {@\n\
    \        output->is_error = true;@\n\
    \        return -1;@\n\
    \    }@\n\
     }@\n"
    (format_local_vars_defs var_indexes)
    defs scond
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt err ->
         let error_descr = Pos.unmark err.Error.descr in
         let error_descr = Re.Pcre.substitute ~rex:percent ~subst:(fun _ -> "%%") error_descr in
         Format.fprintf fmt "%s: %s" (Pos.unmark err.Error.name) error_descr))
    cond.cond_errors

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

and generate_stmts (program : Bir.program) (var_indexes : int Mir.VariableMap.t)
    (oc : Format.formatter) (stmts : Bir.stmt list) =
  Format.pp_print_list (generate_stmt program var_indexes) oc stmts

let generate_main_function_signature (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "int m_extracted(m_output *output, const m_input *input)%s"
    (if add_semicolon then ";" else "")

let get_variables_indexes (p : Bir.program) (function_spec : Bir_interface.bir_function) :
    int Mir.VariableMap.t * int =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  let assigned_variables = List.map fst (Mir.VariableMap.bindings (Bir.get_assigned_variables p)) in
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
  Format.fprintf oc "m_value *TGV = malloc(%d * sizeof(m_value));@\n@\n" var_table_size;
  Format.fprintf oc "// Then we extract the input variables from the dictionnary:@\n%a@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "%a = input->%s;"
           (generate_variable var_indexes None)
           var (generate_name var)))
    input_vars;
  Format.fprintf oc "%a@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, ()) -> Format.fprintf fmt "m_value v_%d;" var.Mir.LocalVariable.id))
    (Mir.LocalVariableMap.bindings (Bir.get_local_variables p));
  Format.fprintf oc "m_value cond;@\n@\n"

let generate_return (var_indexes : int Mir.VariableMap.t) (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let returned_variables = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.fprintf oc "%a@\n@\nfree(TGV);@\noutput->is_error = false;@\nreturn 0;@]@\n}"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "output->%s = %a;" (generate_name var)
           (generate_variable var_indexes None)
           var))
    returned_variables

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc "// %s\n\n" Prelude.message;
  Format.fprintf oc "#include <stdio.h>\n";
  Format.fprintf oc "#include <m_value.h>\n\n"

let generate_empty_input_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "m_input m_empty_input()%s" (if add_semicolon then ";\n\n" else "")

let generate_empty_input_func (oc : Format.formatter) (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  Format.fprintf oc "%a {@\n@[<h 4>    return (struct m_input){@\n@[<h 4>    %a@]@\n};@]@\n};@\n@\n"
    generate_empty_input_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var -> Format.fprintf fmt ".%s = m_undefined," (generate_name var)))
    input_vars

let generate_input_from_array_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "m_input m_input_from_array(m_value *array)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_input_from_array_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  Format.fprintf oc "%a {@\n@[<h 4>    return (struct m_input){@\n@[<h 4>    %a@]@\n};@]@\n};@\n@\n"
    generate_input_from_array_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) -> Format.fprintf fmt ".%s = array[%d]," (generate_name var) i))
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
    (if add_semicolon then ";\n\n" else "")

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
  Format.fprintf oc "typedef struct m_input {@[<h 4>    %a@]@\n} m_input;@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "m_value %s; // %s" (generate_name var) (Pos.unmark var.Variable.descr)))
    input_vars

let generate_empty_output_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "m_output m_empty_output()%s" (if add_semicolon then ";\n\n" else "")

let generate_empty_output_func (oc : Format.formatter) (function_spec : Bir_interface.bir_function)
    =
  let output_vars = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.fprintf oc
    "%a {@\n\
     @[<h 4>    return (struct m_output){@\n\
     @[<h 4>    .is_error = false,@\n\
     %a@]@\n\
     };@]@\n\
     };@\n\
     @\n"
    generate_empty_output_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var -> Format.fprintf fmt ".%s = m_undefined," (generate_name var)))
    output_vars

let generate_output_to_array_prototype (oc : Format.formatter) (add_semicolon : bool) =
  Format.fprintf oc "void m_output_to_array(m_value *array, m_output output)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_output_to_array_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.fprintf oc "%a {@\n@[<h 4>    %a@]@\n};@\n@\n" generate_output_to_array_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) -> Format.fprintf fmt "array[%d] = output.%s;" i (generate_name var)))
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
    "typedef struct m_output {@\n@[<h 4>    bool is_error;@\n%a@]@\n} m_output;@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "m_value %s; // %s" (generate_name var) (Pos.unmark var.Variable.descr)))
    output_vars

let generate_implem_header oc header_filename =
  Format.fprintf oc "// File generated by the Mlang compiler\n\n";
  Format.fprintf oc "#include <string.h>\n";
  Format.fprintf oc "#include \"%s\"\n\n" header_filename

let generate_c_program (program : Bir.program) (function_spec : Bir_interface.bir_function)
    (filename : string) : unit =
  if Filename.extension filename <> ".c" then
    Errors.raise_error
      (Format.asprintf "Output file should have a .c extension (currently %s)" filename);
  let header_filename = Filename.remove_extension filename ^ ".h" in
  let _oc = open_out header_filename in
  let var_indexes, var_table_size = get_variables_indexes program function_spec in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a%a%a%a%a%a%a%a%a%a%a%a" generate_header () generate_input_type
    function_spec generate_empty_input_prototype true generate_input_from_array_prototype true
    generate_get_input_index_prototype true generate_get_input_num_prototype true
    generate_get_input_name_from_index_prototype true generate_output_type function_spec
    generate_output_to_array_prototype true generate_get_output_index_prototype true
    generate_get_output_name_from_index_prototype true generate_get_output_num_prototype true
    generate_empty_output_prototype true generate_main_function_signature true;
  close_out _oc;
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a%a%a%a%a%a%a%a%a%a%a%a" generate_implem_header header_filename
    generate_empty_input_func function_spec generate_input_from_array_func function_spec
    generate_get_input_index_func function_spec generate_get_input_name_from_index_func
    function_spec generate_get_input_num_func function_spec generate_output_to_array_func
    function_spec generate_get_output_index_func function_spec
    generate_get_output_name_from_index_func function_spec generate_get_output_num_func
    function_spec generate_empty_output_func function_spec
    (generate_main_function_signature_and_var_decls program var_indexes var_table_size)
    function_spec
    (generate_stmts program var_indexes)
    program.statements (generate_return var_indexes) function_spec;
  close_out _oc
