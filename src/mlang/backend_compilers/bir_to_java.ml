(* Copyright (C) 2021 Inria, contributor: James Barnes
   <bureau.si-part-ircalcul@dgfip.finances.gouv.fr>

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

open Mir

let java_imports : string =
  {|
package com.mlang;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

import static com.mlang.MValue.*;
|}

let none_value = "MValue.mUndefined"

let generate_comp_op (op : Mast.comp_op) : string =
  match op with
  | Mast.Gt -> "mGreaterThan"
  | Mast.Gte -> "mGreaterThanEqual"
  | Mast.Lt -> "mLessThan"
  | Mast.Lte -> "mLessThanEqual"
  | Mast.Eq -> "mEqual"
  | Mast.Neq -> "mNotEqual"

let generate_binop (op : Mast.binop) : string =
  match op with
  | Mast.And -> "mAnd"
  | Mast.Or -> "mOr"
  | Mast.Add -> "mAdd"
  | Mast.Sub -> "mSubtract"
  | Mast.Mul -> "mMultiply"
  | Mast.Div -> "mDivide"

let generate_unop (op : Mast.unop) : string =
  match op with Mast.Not -> "mNot" | Mast.Minus -> "mNeg"

let generate_var_name (var : Variable.t) : string =
  let v = Pos.unmark var.Variable.name in
  String.uppercase_ascii v

let format_var_name (fmt : Format.formatter) (var : Variable.t) : unit =
  Format.fprintf fmt "%s" (generate_var_name var)

let generate_name (v : Variable.t) : string =
  match v.alias with Some v -> v | None -> Pos.unmark v.Variable.name

let print_double_cut oc () = Format.fprintf oc "@,@,"

let get_var_pos (var : Variable.t) var_indexes : int =
  match Mir.VariableMap.find_opt var var_indexes with
  | Some i -> i
  | None -> Errors.raise_error "Variable not found"

let rec generate_java_expr (e : expression Pos.marked)
    (var_indexes : int Mir.VariableMap.t) :
    string * (LocalVariable.t * expression Pos.marked) list =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 =
        ( Format.asprintf "%s(%s, %s)" (generate_comp_op (Pos.unmark op)) se1 se2,
          s1 @ s2 )
      in
      (se3, s3)
  | Binop (op, e1, e2) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 =
        ( Format.asprintf "%s(%s, %s)" (generate_binop (Pos.unmark op)) se1 se2,
          s1 @ s2 )
      in
      (se3, s3)
  | Unop (op, e) ->
      let se, s = generate_java_expr e var_indexes in
      let se2, s2 = (Format.asprintf "%s(%s)" (generate_unop op) se, s) in
      (se2, s2)
  | Index (var, e) ->
      let se, s = generate_java_expr e var_indexes in
      let unmarked_var = Pos.unmark var in
      let size = Option.get unmarked_var.Mir.Variable.is_table in
      let se2, s2 =
        ( Format.asprintf "m_array_index(tgv, %d ,%s, %d)"
            (get_var_pos unmarked_var var_indexes)
            se size,
          s )
      in
      (se2, s2)
  | Conditional (e1, e2, e3) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 = generate_java_expr e3 var_indexes in
      let se4, s4 =
        (Format.asprintf "m_cond(%s, %s, %s)" se1 se2 se3, s1 @ s2 @ s3)
      in
      (se4, s4)
  | FunctionCall (PresentFunc, [ arg ]) ->
      let se, s = generate_java_expr arg var_indexes in
      let se2, s2 = (Format.asprintf "mPresent(%s)" se, s) in
      (se2, s2)
  | FunctionCall (NullFunc, [ arg ]) ->
      let se, s = generate_java_expr arg var_indexes in
      let se2, s2 = (Format.asprintf "m_null(%s)" se, s) in
      (se2, s2)
  | FunctionCall (ArrFunc, [ arg ]) ->
      let se, s = generate_java_expr arg var_indexes in
      let se2, s2 = (Format.asprintf "m_round(%s)" se, s) in
      (se2, s2)
  | FunctionCall (InfFunc, [ arg ]) ->
      let se, s = generate_java_expr arg var_indexes in
      let se2, s2 = (Format.asprintf "m_floor(%s)" se, s) in
      (se2, s2)
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 = (Format.asprintf "m_max(%s, %s)" se1 se2, s1 @ s2) in
      (se3, s3)
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 = (Format.asprintf "m_min(%s, %s)" se1 se2, s1 @ s2) in
      (se3, s3)
  | FunctionCall (Multimax, [ e1; (Var v2, _) ]) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 =
        ( Format.asprintf "m_multimax(%s, tgv, %d)" se1
            (get_var_pos v2 var_indexes),
          [] )
      in
      (se2, s1 @ s2)
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> (
      match f with
      | 0. -> (Format.asprintf "MValue.zero", [])
      | 1. -> (Format.asprintf "MValue.one", [])
      | _ -> (Format.asprintf "new MValue(%s)" (string_of_float f), []))
  | Literal Undefined -> (Format.asprintf "%s" none_value, [])
  | Var var ->
      ( Format.asprintf "tgv[%d/*\"%a\"*/]"
          (get_var_pos var var_indexes)
          format_var_name var,
        [] )
  | LocalVar lvar ->
      (Format.asprintf "localVariables[%d]" lvar.LocalVariable.id, [])
  | GenericTableIndex -> (Format.asprintf "new MValue(genericIndex)", [])
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let _, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 = (Format.asprintf "%s" se2, s1 @ ((lvar, e1) :: s2)) in
      (se3, s3)

let format_local_vars_defs (var_indexes : int Mir.VariableMap.t)
    (oc : Format.formatter)
    (defs : (LocalVariable.t * expression Pos.marked) list) =
  Format.pp_print_list
    (fun fmt (lvar, expr) ->
      let se, _ = generate_java_expr expr var_indexes in
      Format.fprintf fmt "localVariables[%d] = %s;" lvar.LocalVariable.id se)
    oc defs

let generate_var_def (var_indexes : int Mir.VariableMap.t)
    (var : Mir.Variable.t) (data : Mir.variable_data) (oc : Format.formatter) =
  match data.var_definition with
  | SimpleVar e ->
      let se, defs = generate_java_expr e var_indexes in
      Format.fprintf oc "%atgv[%d /*\"%a\"*/] = %s;"
        (format_local_vars_defs var_indexes)
        defs
        (get_var_pos var var_indexes)
        format_var_name var se
  | TableVar (_, IndexTable es) ->
      Format.fprintf oc "%a"
        (fun fmt ->
          IndexMap.iter (fun i v ->
              let sv, defs = generate_java_expr v var_indexes in
              Format.fprintf fmt "%atgv[%d /* %a */] = %s;"
                (format_local_vars_defs var_indexes)
                defs
                (get_var_pos var var_indexes |> ( + ) i)
                format_var_name var sv))
        es
  | TableVar (size, IndexGeneric e) ->
      let se, s = generate_java_expr e var_indexes in
      Format.fprintf oc
        "@[<hv 2>for (int genericIndex = 0; genericIndex < %d; genericIndex++) \
         {@,\
         @[<h 4> %atgv[%d + genericIndex /* %a */] = %s;@]@]@,\
         }"
        size
        (format_local_vars_defs var_indexes)
        s
        (get_var_pos var var_indexes)
        format_var_name var se
  | InputVar -> assert false

let generate_input_handling (function_spec : Bir_interface.bir_function)
    (var_indexes : variable_id VariableMap.t) (oc : Format.formatter)
    (split_threshold : int) =
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
  let rec split_input_vars old_list filling_list acc =
    match old_list with
    | hd :: tl ->
        let filling_list, acc =
          if List.length filling_list >= split_threshold then
            ([ hd ], List.rev filling_list :: acc)
          else (hd :: filling_list, acc)
        in
        split_input_vars tl filling_list acc
    | [] -> List.rev (List.rev filling_list :: acc)
  in
  let input_vars = split_input_vars input_vars [] [] in
  let input_methods_count = ref 0 in
  let print_input fmt var =
    Format.fprintf fmt
      "tgv[/*\"%a\"*/%d] = inputVariables.get(\"%s\") != null ? \
       inputVariables.get(\"%s\") : MValue.mUndefined;"
      format_var_name var
      (get_var_pos var var_indexes)
      (generate_name var) (generate_name var)
  in
  let print_method fmt inputs =
    Format.fprintf fmt
      "@[<hv 2>private static void loadInputVariables_%d(Map<String, \
       MValue>inputVariables, MValue[] tgv) {@,\
       %a@]@,\
       }@,"
      !input_methods_count
      (Format.pp_print_list print_input)
      inputs;
    input_methods_count := !input_methods_count + 1
  in
  Format.pp_print_list print_method oc input_vars;
  let load_calls = List.init !input_methods_count (fun i -> i) in
  let print_call oc i =
    Format.fprintf oc "loadInputVariables_%d(inputVariables, tgv);" i
  in
  Format.fprintf oc
    "@,\
     @[<hov 2>static void loadInputVariables(Map<String, MValue> \
     inputVariables, MValue[] tgv) {@,\
     %a@]@,\
     }"
    (Format.pp_print_list print_call)
    load_calls

let generate_var_cond var_indexes oc cond =
  let open Strings in
  Format.fprintf oc "cond = %s;@,"
    (let se, _ = generate_java_expr cond.cond_expr var_indexes in
     se);
  let cond_error, var = cond.cond_error in
  let error_name = sanitize_str cond_error.Error.name in
  let error_kind = sanitize_str cond_error.Error.descr.kind in
  let error_major_code = sanitize_str cond_error.Error.descr.major_code in
  let error_minor_code = sanitize_str cond_error.Error.descr.minor_code in
  let error_description = sanitize_str cond_error.Error.descr.description in
  let error_alias =
    match var with
    | Some v -> (
        match v.Variable.alias with
        | Some alias -> "(( " ^ alias ^ " ))"
        | None -> "")
    | None -> ""
  in
  Format.fprintf oc
    "@[<hv 2>if (m_is_defined_true(cond)) {@,\
     MError error = new MError(\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", \
     \"%s\");@,\
     calculationErrors.add(error);@,"
    error_name error_kind error_major_code error_minor_code error_description
    error_alias;
  if cond_error.Error.typ = Anomaly then
    Format.fprintf oc
      "mCalculation.setCurrentAnomalies(mCalculation.getCurrentAnomalies() + \
       1);@,\
       @[<hv 2>if (mCalculation.getCurrentAnomalies() >= \
       mCalculation.getMaxAnomalies()) {@,\
      \ throw new MException(calculationErrors);@]@,\
       }";
  Format.fprintf oc "@]@,@[}@]"

let fresh_cond_counter = ref 0

let generate_rule_header (oc : Format.formatter) (rule : Bir.rule) =
  Format.fprintf oc "Rule.m_rule_%s(mCalculation, calculationErrors);"
    rule.rule_name

let rec generate_stmts (program : Bir.program)
    (var_indexes : int Mir.VariableMap.t) (oc : Format.formatter)
    (stmts : Bir.stmt list) =
  Format.pp_print_list (generate_stmt program var_indexes) oc stmts

and generate_stmt (program : Bir.program) (var_indexes : int Mir.VariableMap.t)
    (oc : Format.formatter) (stmt : Bir.stmt) : unit =
  match Pos.unmark stmt with
  | SRuleCall r ->
      let rule = Bir.RuleMap.find r program.rules in
      generate_rule_header oc rule
  | Bir.SAssign (var, vdata) -> generate_var_def var_indexes var vdata oc
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
      Format.fprintf oc
        "MValue %s = %s;@,@[<hv 2>if (m_is_defined_true(%s)) {@,%a@]@,}"
        cond_name
        (let s, _ =
           generate_java_expr (Pos.same_pos_as cond stmt) var_indexes
         in
         s)
        cond_name
        (generate_stmts program var_indexes)
        tt;
      Format.fprintf oc " @[<hv 2>if (m_is_defined_false(%s)) {@,%a@]@,}"
        cond_name
        (generate_stmts program var_indexes)
        ff
  | SVerif v -> generate_var_cond var_indexes oc v
  | SFunctionCall (f, _) ->
      Format.fprintf oc "MppFunction.%s(mCalculation, calculationErrors);" f

let generate_return (var_indexes : variable_id VariableMap.t)
    (oc : Format.formatter) (function_spec : Bir_interface.bir_function) =
  let returned_variables =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  let print_outputs oc returned_variables =
    Format.pp_print_list
      (fun oc var ->
        Format.fprintf oc "outputVariables.put(\"%a\",tgv[%d/*\"%a\"*/]);"
          format_var_name var
          (get_var_pos var var_indexes)
          format_var_name var)
      oc returned_variables
  in
  Format.fprintf oc
    "@[<v 2>private static Map<String, MValue> loadOutputVariables(MValue[] \
     tgv) {@,\
     Map<String, MValue> outputVariables = new HashMap<>();@,\
     @,\
     %a@,\
     return outputVariables;@]@,\
     }"
    print_outputs returned_variables

let generate_rule_method (program : Bir.program)
    (var_indexes : int Mir.VariableMap.t) (oc : Format.formatter)
    (rule : Bir.rule) =
  Format.fprintf oc
    "@[<v 2>static void m_rule_%s(MCalculation mCalculation, List<MError> \
     calculationErrors) {@,\
     MValue cond = MValue.mUndefined;@,\
     MValue[] tgv = mCalculation.getCalculationVariables();@,\
     MValue[] localVariables = mCalculation.getLocalVariables();@,\
     Map<String, List<MValue>> tableVariables = \
     mCalculation.getTableVariables();@,\
     %a@]@,\
     }"
    rule.rule_name
    (generate_stmts program var_indexes)
    rule.rule_stmts

let generate_rule_methods (program : Bir.program) (oc : Format.formatter)
    (var_indexes : int Mir.VariableMap.t) : unit =
  let rules = RuleMap.bindings program.rules in
  let _, rules = List.split rules in
  Format.pp_print_list ~pp_sep:print_double_cut
    (generate_rule_method program var_indexes)
    oc rules

let generate_calculateTax_method (calculation_vars_len : int)
    (program : Bir.program) (locals_size : int) (oc : Format.formatter)
    (var_indexes : variable_id VariableMap.t) =
  Format.fprintf oc
    "@[<v 0>/**@,\
     * Main calculation method for determining tax @,\
     * @param inputVariables Map of variables to be used for calculation, the \
     key is the variable name and the value is the variable value@,\
     * @return  Map of variables returned after calculation, the key is the \
     variable name and the value is the variable value@,\
     */@,\
     @[<hv 2>public static MOutput calculateTax(Map<String,MValue> \
     inputVariables) {@,\
     return calculateTax(inputVariables, 0);@]@,\
     }%a@[<v 2>public static MOutput calculateTax(Map<String,MValue> \
     inputVariables, int maxAnomalies) {@,\
     MValue cond = MValue.mUndefined;@,\
     List<MError> calculationErrors = new ArrayList<>();@,\
     MValue[] tgv = new MValue[%d];@,\
     MValue[] localVariables = new MValue[%d];@,\
     MCalculation mCalculation = new MCalculation(tgv, localVariables, \
     maxAnomalies);%a@[<hv 2>for (int i = 0; i < localVariables.length; i++) \
     {@,\
     localVariables[i] = mUndefined;@]@,\
     }%a@[<v 2>for (int i = 0; i < tgv.length; i++) {@,\
     tgv[i] = mUndefined;@]@,\
     }%aInputHandler.loadInputVariables(inputVariables, \
     mCalculation.getCalculationVariables());@,\
     %a@,\
     Map<String, MValue> outputVariables = \
     loadOutputVariables(mCalculation.getCalculationVariables());@,\
     return new MOutput(outputVariables, calculationErrors);@]@,\
     }@]@,\
     @,"
    print_double_cut () calculation_vars_len locals_size print_double_cut ()
    print_double_cut () print_double_cut ()
    (generate_stmts program var_indexes)
    (Bir.main_statements program)

let generate_mpp_function (program : Bir.program)
    (var_indexes : int Mir.VariableMap.t) (oc : Format.formatter)
    (f : Bir.function_name) =
  let stmts = Bir.FunctionMap.find f program.mpp_functions in
  Format.fprintf oc
    "@[<v 2>static void %s(MCalculation mCalculation, List<MError> \
     calculationErrors) {@,\
     MValue cond = MValue.mUndefined;@,\
     MValue[] tgv = mCalculation.getCalculationVariables();@,\
     MValue[] localVariables = mCalculation.getLocalVariables();@,\
     Map<String, List<MValue>> tableVariables = \
     mCalculation.getTableVariables();@,\
     %a@]@,\
     }"
    f
    (generate_stmts program var_indexes)
    stmts

let generate_mpp_functions (program : Bir.program) (oc : Format.formatter)
    (var_indexes : variable_id VariableMap.t) =
  let functions =
    Bir.FunctionMap.bindings
      (Bir_interface.context_agnostic_mpp_functions program)
  in
  let function_names, _ = List.split functions in
  Format.pp_print_list ~pp_sep:print_double_cut
    (generate_mpp_function program var_indexes)
    oc function_names

let generate_main_class (program : Bir.program) (var_table_size : int)
    (locals_size : int) (var_indexes : variable_id VariableMap.t)
    (function_spec : Bir_interface.bir_function) (fmt : Format.formatter)
    (filename : string) =
  let class_name =
    String.split_on_char '.' filename |> List.hd |> String.split_on_char '/'
    |> fun list -> List.nth list (List.length list - 1)
  in
  Format.fprintf fmt
    "@[<hv 0>// %s@,\
     %s@,\
     /**@,\
     * Main class containing calculation logic@,\
     */@,\
     @[<v 2>public class %s {@,\
     @,\
     %a%a@]@]@,\
     }"
    Prelude.message java_imports class_name
    (generate_calculateTax_method var_table_size program locals_size)
    var_indexes
    (generate_return var_indexes)
    function_spec

let generate_java_program (program : Bir.program) (function_spec : Bir_interface.bir_function)
    (filename : string) : unit =
  let split_treshold = 100 in
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  let locals_size = Bir.get_locals_size program |> ( + ) 1 in
  let var_indexes, var_table_size =
    Bir_interface.get_variables_indexes program function_spec
  in
   let program = Bir.squish_statements program split_treshold "java_rule_" in
  Format.fprintf oc
    "@[<v 0>%a%a\
     @[<v 2>class InputHandler {@,%a@]@,}%a\
     @[<v 2>class MppFunction {@,%a@]@,}%a\
     @[<hv 2>class Rule {@,%a@]@,}@]@." 
     (generate_main_class program var_table_size locals_size
            var_indexes function_spec) filename
     print_double_cut ()
     (generate_input_handling function_spec var_indexes) split_treshold
     print_double_cut ()
     (generate_mpp_functions program) var_indexes
     print_double_cut ()
     (generate_rule_methods program) var_indexes;
  close_out _oc[@@ocamlformat "disable"]
