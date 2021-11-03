(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(* TODO: Use an array for calculation rather than a map to improve performance*)

open Mir

let java_imports : string =
  {|
package com.mlang;

import java.util.Map;
import com.mlang.MValue;
import java.util.HashMap;
import java.util.List;
import java.util.Arrays;

import static com.mlang.MValue.*;
|}

let calculateTax_method_header  (calculation_vars_len : int) oc
    (body : Format.formatter -> 'a -> unit) =
    Format.fprintf oc
    "@[<hv 0>/**@,\
    * Main calculation method for determining tax @,\
    * @param inputVariables Map of variables to be used for calculation, the key is the variable name and the value is the variable value@,\
    * @return  Map of variables returned after calculation, the key is the variable name and the value is the variable value@,\
    */@]@,\
    @[<hv 2>public static Map<String, MValue> calculateTax(Map<String,MValue> inputVariables) {@,\
     MValue cond = MValue.mUndefined;@,\
     Map<String, MValue> outputVariables = new HashMap<>();@,\
     MValue[] calculationVariables = new MValue[%d];@,\
     Map<Integer, MValue> localVariables = new HashMap<>();@,\
     Map<String,List<MValue>> tableVariables = new HashMap<>();@,\
     @,\
     InputHandler.loadInputVariables(inputVariables, calculationVariables);\
     %a@,\
     loadOutputVariables(outputVariables, calculationVariables);@,\
     return outputVariables;@,\
     @]@,\
     @[}@]"
    calculation_vars_len body ()

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

let generate_variable (fmt : Format.formatter) (var : Variable.t) : unit =
  Format.fprintf fmt "%s" (generate_var_name var)

let generate_name (v : Variable.t) : string =
  match v.alias with Some v -> v | None -> Pos.unmark v.Variable.name

let get_var_pos (var : Variable.t) (var_indexes) : int = 
            match Mir.VariableMap.find_opt var var_indexes with
            | Some i -> i
            | None -> Errors.raise_error "Variable not found"

let add_expr_code_block (se, s) = (se, s)

let rec generate_java_expr (e : expression Pos.marked) (var_indexes : int Mir.VariableMap.t) :
    string * (LocalVariable.t * expression Pos.marked) list =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 =
        (Format.asprintf "%s(%s, %s)" (generate_comp_op (Pos.unmark op)) se1 se2, s1 @ s2)
      in
      add_expr_code_block (se3, s3)
  | Binop (op, e1, e2) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 =
        (Format.asprintf "%s(%s, %s)" (generate_binop (Pos.unmark op)) se1 se2, s1 @ s2)
      in
      add_expr_code_block (se3, s3)
  | Unop (op, e) ->
      let se, s = generate_java_expr e var_indexes in
      let se2, s2 = (Format.asprintf "%s(%s)" (generate_unop op) se, s) in
      add_expr_code_block (se2, s2)
  | Index (var, e) ->
      let se, s = generate_java_expr e var_indexes in
      let se2, s2 =
        ( Format.asprintf "m_array_index(tableVariables.get(\"%a\"),%s)" generate_variable
            (Pos.unmark var) se,
          s )
      in
      add_expr_code_block (se2, s2)
  | Conditional (e1, e2, e3) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 = generate_java_expr e3 var_indexes in
      let se4, s4 = (Format.asprintf "m_cond(%s, %s, %s)" se1 se2 se3, s1 @ s2 @ s3) in
      add_expr_code_block (se4, s4)
  | FunctionCall (PresentFunc, [ arg ]) ->
      let se, s = generate_java_expr arg var_indexes in
      let se2, s2 = (Format.asprintf "mPresent(%s)" se, s) in
      add_expr_code_block (se2, s2)
  | FunctionCall (NullFunc, [ arg ]) ->
      let se, s = generate_java_expr arg var_indexes in
      let se2, s2 = (Format.asprintf "m_null(%s)" se, s) in
      add_expr_code_block (se2, s2)
  | FunctionCall (ArrFunc, [ arg ]) ->
      let se, s = generate_java_expr arg var_indexes in
      let se2, s2 = (Format.asprintf "m_round(%s)" se, s) in
      add_expr_code_block (se2, s2)
  | FunctionCall (InfFunc, [ arg ]) ->
      let se, s = generate_java_expr arg var_indexes in
      let se2, s2 = (Format.asprintf "m_floor(%s)" se, s) in
      add_expr_code_block (se2, s2)
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 = (Format.asprintf "m_max(%s, %s)" se1 se2, s1 @ s2) in
      add_expr_code_block (se3, s3)
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 = (Format.asprintf "m_min(%s, %s)" se1 se2, s1 @ s2) in
      add_expr_code_block (se3, s3)
  | FunctionCall (Multimax, [ e1; (Var v2, _) ]) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 =
        (Format.asprintf "m_multimax(%s, tableVariables.get(\"%a\"))" se1 format_var_name v2, s1)
      in
      add_expr_code_block (se2, s2)
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> (
      match f with
      | 0. -> add_expr_code_block (Format.asprintf "MValue.zero", [])
      | 1. -> add_expr_code_block (Format.asprintf "MValue.one", [])
      | _ -> add_expr_code_block (Format.asprintf "new MValue(%s)" (string_of_float f), []))
  | Literal Undefined -> add_expr_code_block (Format.asprintf "%s" none_value, [])
  | Var var ->
      add_expr_code_block
        ( Format.asprintf "calculationVariables[%d/*\"%a\"*/]"
               (get_var_pos var var_indexes) format_var_name var,
          [] )
  | LocalVar lvar ->
      add_expr_code_block (Format.asprintf "localVariables.get(%d)" lvar.LocalVariable.id, [])
  | GenericTableIndex -> add_expr_code_block (Format.asprintf "generic_index", [])
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let _, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 = (Format.asprintf "%s" se2, s1 @ ((lvar, e1) :: s2)) in
      add_expr_code_block (se3, s3)

let format_local_vars_defs (var_indexes : int Mir.VariableMap.t) (oc : Format.formatter)
    (defs : (LocalVariable.t * expression Pos.marked) list) =
  Format.pp_print_list
    (fun fmt (lvar, expr) ->
      let se, _ = generate_java_expr expr var_indexes in
      Format.fprintf fmt "localVariables.put(%d,%s);@," lvar.LocalVariable.id se)
    oc defs

let generate_var_def (var_indexes : int Mir.VariableMap.t) (var : Mir.Variable.t)
    (data : Mir.variable_data) (oc : Format.formatter) =
  match data.var_definition with
  | SimpleVar e ->
      let se, defs = generate_java_expr e var_indexes in
      Format.fprintf oc "%acalculationVariables[%d /*\"%a\"*/] = %s;@,"
        (format_local_vars_defs var_indexes)
        defs (get_var_pos var var_indexes) format_var_name var se
  | TableVar (_, IndexTable es) ->
      Format.fprintf oc "tableVariables.put(\"%a\",Arrays.asList(%a));@," format_var_name var
        (Format.pp_print_list
           ~pp_sep:(fun oc _ -> Format.fprintf oc ", ")
           (fun oc (_, var) ->
             let var, _ = generate_java_expr var var_indexes in
             Format.fprintf oc "%s" var))
        (IndexMap.bindings es)
  | TableVar (_, IndexGeneric e) ->
      Errors.raise_spanned_error "generic index table definitions not supported in the java backend"
        (Pos.get_position e)
  | InputVar -> assert false

let generate_header (oc : Format.formatter) (class_name : string) : unit =
  Format.fprintf oc "@[// %s@,%s@,/**@,* Main class containing calculation logic@,*/@,public class %s {@]" Prelude.message java_imports class_name

let generate_input_handling (function_spec : Bir_interface.bir_function) (var_indexes: variable_id VariableMap.t) (oc : Format.formatter)
    (split_threshold : int)  =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  let print_header count =
    Format.fprintf oc
      "@[<hv 2>private static void loadInputVariables_%d(Map<String, MValue> inputVariables, \
       MValue[] calculationVariables){@,"
      count
  in
  let rec print_load_input curr oc len =
    if curr <= len then (
      Format.fprintf oc "loadInputVariables_%d(inputVariables, calculationVariables);@," curr;
      print_load_input (curr + 1) oc len)
  in

  let format_input_var var count =
    if count mod split_threshold = 0 then
      print_header (if count > 0 then count / split_threshold else 0);
    Format.fprintf oc
      "calculationVariables[/*\"%a\"*/%d] = inputVariables.get(\"%s\") != null ? \
       inputVariables.get(\"%s\") : MValue.mUndefined;@,"
      format_var_name var (get_var_pos var var_indexes) (generate_name var) (generate_name var);
    if (count + 1) mod split_threshold = 0 then Format.fprintf oc "@]@,@[}@]";
    count + 1
  in
  let count = List.fold_left (fun count assign -> format_input_var assign count) 0 input_vars in
  Format.fprintf oc "}@,static void loadInputVariables(Map<String, MValue> inputVariables, \
       MValue[] calculationVariables) {@,%a@,}"
  (print_load_input 0) (count / split_threshold)

let sanitize_str (s, p) =
  String.map
    (fun c ->
      if c >= Char.chr 128 then
        let () =
          Cli.warning_print "Replaced char code %d by space %a" (Char.code c) Pos.format_position p
        in
        ' '
      else c)
    s

let generate_var_cond var_indexes oc cond =
  Format.fprintf oc "cond = %s;@,"
    (let se, _ = generate_java_expr cond.cond_expr var_indexes in
     se);
  let cond_error, var = cond.cond_error in
  let error_message =
    Format.asprintf "%s: %s%s%s%s%s"
      (sanitize_str cond_error.Error.name)
      (sanitize_str cond_error.Error.descr.kind)
      (sanitize_str cond_error.Error.descr.major_code)
      (sanitize_str cond_error.Error.descr.minor_code)
      (sanitize_str cond_error.Error.descr.description)
      (match var with
      | Some v -> ( match v.Variable.alias with Some alias -> alias | None -> "")
      | None -> "")
  in
  Format.fprintf oc "if (m_is_defined_true(cond)) { @,";
  if cond_error.Error.typ = Anomaly then
    Format.fprintf oc "throw new RuntimeException(\"Error triggered\\n%s\");@," error_message
  else Format.fprintf oc "//System.out.println(\"Error occurred : %s\");@," error_message;
  Format.fprintf oc "}@,@,"

let fresh_cond_counter = ref 0

let generate_rule_header (oc : Format.formatter) (rule : Bir.rule) =
  Format.fprintf oc "m_rule_%s(calculationVariables, localVariables, tableVariables);"
    rule.rule_name

let rec generate_stmts (program : Bir.program) (var_indexes : int Mir.VariableMap.t)
    (oc : Format.formatter) (stmts : Bir.stmt list) =
  List.iter (fun stmt -> generate_stmt program var_indexes stmt oc) stmts

and generate_stmt (program : Bir.program) (var_indexes : int Mir.VariableMap.t) (stmt : Bir.stmt)
    (oc : Format.formatter) : unit =
  match Pos.unmark stmt with
  | SRuleCall r ->
      let rule = Bir.RuleMap.find r program.rules in
      generate_rule_header oc rule
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
      if List.length tt = 0 then assert false;
      Format.fprintf oc
        "/* SConditional (cond, tt, ff) */MValue %s = %s;@,if (m_is_defined_true(%s)) {@,%a}"
        cond_name
        (let s, _ = generate_java_expr (Pos.same_pos_as cond stmt) var_indexes in
         s)
        cond_name
        (generate_stmts program var_indexes)
        tt;
      Format.fprintf oc
        {|
   if (m_is_defined_false(%s)) {
      %a
    }
|}
        cond_name
        (generate_stmts program var_indexes)
        ff
  | SVerif v -> generate_var_cond var_indexes oc v

let generate_return (var_indexes: variable_id VariableMap.t) (oc : Format.formatter) (function_spec : Bir_interface.bir_function) =
  let returned_variables = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  let print_outputs oc returned_variables =
    Format.pp_print_list
      (fun oc var ->
        Format.fprintf oc "outputVariables.put(\"%a\",calculationVariables[%d/*\"%a\"*/]);"
          format_var_name var (get_var_pos var var_indexes) format_var_name var)
      oc returned_variables
  in
  Format.fprintf oc
    "@[<v 2>private static void loadOutputVariables(Map<String,MValue> outputVariables, MValue[] \
     calculationVariables){@,\
     %a@]@,\
     @[}@]"
    print_outputs returned_variables

let generate_rule_method (program : Bir.program) (var_indexes : int Mir.VariableMap.t)
    (oc : Format.formatter) (rule : Bir.rule) =
  Format.fprintf oc
    "@[<hv 2>private static void m_rule_%s(MValue[] calculationVariables,  Map<Integer, MValue> \
     localVariables, Map<String, List<MValue>> tableVariables){@,\
     MValue cond = MValue.mUndefined;@,\
     %a@,\
     @]@[}@]@,"
    rule.rule_name
    (generate_stmts program var_indexes)
    rule.rule_stmts

let generate_rule_methods (program : Bir.program) (oc : Format.formatter)
    (var_indexes : int Mir.VariableMap.t) : unit =
  RuleMap.iter
    (fun _ rule -> Format.fprintf oc "%a" (generate_rule_method program var_indexes) rule)
    program.rules

let generate_java_program (program : Bir.program) (function_spec : Bir_interface.bir_function)
    (filename : string) : unit =
  let split_treshold = 100 in
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  let var_indexes, var_table_size = Bir_interface.get_variables_indexes program function_spec in
  Printf.printf "var_table_size %d\n" var_table_size;
  let program = Bir.squish_statements program split_treshold "java_rule_" in
  let stmts oc () = generate_stmts program var_indexes oc program.statements in
  Format.fprintf oc
    "@[<hv 2>%a@,\
     @,\
     %a@.@,\
     %a@,\
     @,\
     %a}@]@,\
     @[<hv 2>class InputHandler {@,\
     %a\
     }\
     @]@." 
     generate_header (
       String.split_on_char '.' filename 
          |> List.hd 
          |> String.split_on_char '/' 
          |> fun list -> List.nth list (List.length list -1))
     (generate_rule_methods program) var_indexes
     (calculateTax_method_header var_table_size) stmts 
     (generate_return var_indexes) function_spec
     (generate_input_handling function_spec var_indexes) split_treshold;
  close_out _oc[@@ocamlformat "disable"]
