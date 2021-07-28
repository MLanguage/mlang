(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(* TODO: Refactor multiple method splitting functions *)
(* TODO: Use an array for calculation rather than a map to improve performance*)
(* TODO: Keep splitting for java compiler without holding whole program in memory *)

open Mir

let java_program : (Format.formatter -> unit) list ref = ref []

(** Add element to code_block type horizontally *)
let add_el_hor (el : Format.formatter -> unit) =
  java_program := el :: !java_program

let java_imports : string =
  {|
  package com.mlang;

  import java.util.Map;
  import java.util.OptionalDouble;
  import java.util.HashMap;
  import java.util.List;
  import java.util.Arrays;

  import static com.mlang.MValue.*;
|}

let calculateTax_method_header : string =
  {|

public static Map<String, OptionalDouble> calculateTax(Map<String,OptionalDouble> input_variables) {
  OptionalDouble cond = OptionalDouble.empty(); 
  Map<String, OptionalDouble> out = new HashMap<>();
  Map<String, OptionalDouble> calculationVariables = new HashMap<>();
  Map<Integer, OptionalDouble> localVariables = new HashMap<>();

  Map<String,List<OptionalDouble>> tableVariables = new HashMap<>();
|}

let none_value = "OptionalDouble.empty()"

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
  | Mast.Sub -> "mSubstract"
  | Mast.Mul -> "mMultiply"
  | Mast.Div -> "mDivide"

let generate_unop (op : Mast.unop) : string =
  match op with Mast.Not -> "mNot" | Mast.Minus -> "mNeg"

let generate_var_name (var : Variable.t) : string =
  let v = Pos.unmark var.Variable.name in
  String.uppercase_ascii v

let format_var_name (fmt : Format.formatter) (var : Variable.t) : unit =
  let v = generate_var_name var in
  Format.fprintf fmt "%s" v

let generate_variable (fmt : Format.formatter) (var : Variable.t) : unit =
  Format.fprintf fmt "%s" (generate_var_name var)

let generate_name (v : Variable.t) : string =
  match v.alias with Some v -> v | None -> Pos.unmark v.Variable.name

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
      let _, s = generate_java_expr e var_indexes in
      let size = Option.get (Pos.unmark var).Mir.Variable.is_table in
      let se2, s2 =
        ( Format.asprintf "tableVariables.get(\"%a\").get(%d)" generate_variable (Pos.unmark var)
            size,
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
  | Literal (Float f) ->
      add_expr_code_block (Format.asprintf "OptionalDouble.of(%s)" (string_of_float f), [])
  | Literal Undefined -> add_expr_code_block (Format.asprintf "%s" none_value, [])
  | Var var ->
      add_expr_code_block
        (Format.asprintf "calculationVariables.get(\"%a\")" format_var_name var, [])
  | LocalVar lvar ->
      add_expr_code_block (Format.asprintf "localVariables.get(%d)" lvar.LocalVariable.id, [])
  | GenericTableIndex -> add_expr_code_block (Format.asprintf "generic_index", [])
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let _, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 = (Format.asprintf "%s" se2, s1 @ ((lvar, e1) :: s2)) in
      add_expr_code_block (se3, s3)

let format_local_vars_defs (var_indexes : int Mir.VariableMap.t) (fmt : Format.formatter)
    (defs : (LocalVariable.t * expression Pos.marked) list) =
  List.iter
    (fun (lvar, e) ->
      let se, _ = generate_java_expr e var_indexes in
      Format.fprintf fmt "localVariables.put(%d,%s);@\n" lvar.LocalVariable.id se)
    defs

let generate_var_def (var_indexes : int Mir.VariableMap.t) (var : Mir.Variable.t)
    (data : Mir.variable_data) =
  match data.var_definition with
  | SimpleVar e ->
      let se, defs = generate_java_expr e var_indexes in
      add_el_hor (fun oc ->
          Format.fprintf oc "%a calculationVariables.put(\"%a\", %s); \n"
            (format_local_vars_defs var_indexes)
            defs format_var_name var se)
  | TableVar (_, IndexTable es) ->
      add_el_hor (fun oc ->
          Format.fprintf oc "@\n   tableVariables.put(\"%a\",Arrays.asList(%s));@\n" format_var_name
            var
            (String.concat ","
               (let array_of_variables = ref [] in
                IndexMap.iter
                  (fun _ v ->
                    let string_genere, _ = generate_java_expr v var_indexes in
                    array_of_variables := List.append !array_of_variables [ string_genere ])
                  es;
                !array_of_variables)))
  | TableVar (_, IndexGeneric _) -> assert false
  (*Format.asprintf "%a = %a;@\n@\n" generate_variable var (generate_java_expr) e :: java_stmts*)
  | InputVar -> assert false

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc "// %s\n\n %s\n\npublic class CalculImpot {@\n" Prelude.message java_imports

let generate_input_handling (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  let rec generate_input_list input_vars =
    match input_vars with
    | [] -> ()
    | hd :: tl ->
        let current_method oc =
          Format.fprintf oc
            "calculationVariables.put(\"%a\",input_variables.get(\"%s\") != null ? \
             input_variables.get(\"%s\") : OptionalDouble.empty()); \n\n\
            \ System.out.println(calculationVariables.get(\"%a\"));" format_var_name hd
            (generate_name hd) (generate_name hd) format_var_name hd;
        in
        add_el_hor current_method;
        generate_input_list tl
  in
  generate_input_list input_vars

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

let generate_var_cond var_indexes cond =
  add_el_hor (fun oc ->
      Format.fprintf oc
        "cond = %s;@\n\
         if (cond.isPresent() && (cond.getAsDouble() != 0)) { @\n\
        \   throw new RuntimeException(\"Error triggered\\n%s\");@\n\
         }@\n\
         @\n"
        (let se, _ = generate_java_expr cond.cond_expr var_indexes in
         se)
        (let cond_error = List.hd cond.cond_errors in
         Format.asprintf "%s: %s"
           (sanitize_str cond_error.Error.name)
           (sanitize_str cond_error.Error.descr)))

let rec generate_stmts (program : Bir.program) (var_indexes : int Mir.VariableMap.t)
    (stmts : Bir.stmt list) =
  let local_generate stmt = generate_stmt program var_indexes stmt in
  match stmts with
  | hd :: tl ->
      local_generate hd;
      generate_stmts program var_indexes tl
  | [] -> ()

and generate_stmt (program : Bir.program) (var_indexes : int Mir.VariableMap.t) (stmt : Bir.stmt) :
    unit =
  match Pos.unmark stmt with
  | Bir.SAssign (var, vdata) -> generate_var_def var_indexes var vdata
  | SConditional (cond, tt, ff) ->
      let pos = Pos.get_position stmt in
      let fname =
        String.map (fun c -> if c = '.' then '_' else c) (Filename.basename (Pos.get_file pos))
      in
      let cond_name =
        Format.asprintf "cond_%s_%d_%d_%d_%d" fname (Pos.get_start_line pos)
          (Pos.get_start_column pos) (Pos.get_end_line pos) (Pos.get_end_column pos)
      in
      add_el_hor (fun oc ->
          Format.fprintf oc
            {|
          /* SConditional (cond, tt, ff) */
          OptionalDouble %s = %s;
          if (! %s.isPresent() && %s.getAsDouble() != 0) {
        |}
            cond_name
            (let s, _ = generate_java_expr (Pos.same_pos_as cond stmt) var_indexes in
             s)
            cond_name cond_name;
          generate_stmts program var_indexes tt;
          Format.fprintf oc "} /* End of condition: %s */" cond_name;
          Format.fprintf oc {|
            else if (!%s.isPresent()) {
  |} cond_name;
          generate_stmts program var_indexes ff;
          Format.fprintf oc "}")
  | SVerif v -> generate_var_cond var_indexes v

let generate_return (function_spec : Bir_interface.bir_function) =
  let returned_variables = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  List.iter
    (fun var ->
      add_el_hor (fun oc ->
          Format.fprintf oc "out.put(\"%a\",calculationVariables.get(\"%a\"));@\n" format_var_name
            var format_var_name var))
    returned_variables

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

let split_list list bucket_size =
  let rec aux list new_list previous_list bucket_size count =
    match list with
    | hd :: tl -> (
        match count with
        | count when count mod bucket_size = 0 && count <> 0 ->
            let updated_new_list = List.rev (hd :: previous_list) :: new_list in
            aux tl updated_new_list [] bucket_size (count + 1)
        | _ -> aux tl new_list (hd :: previous_list) bucket_size (count + 1))
    | [] -> new_list
  in
  aux list [] [] bucket_size 0

let print_queue (lines : (Format.formatter -> unit) list list) fmt =
  let lines_count = List.length lines in
  let rec aux count =
    if count <= lines_count then
        (Format.fprintf fmt
          "tax_calculation_part%d(calculationVariables, localVariables, tableVariables, cond, out, input_variables);\n"
          count;
        aux (count + 1);)
    else ()
  in
  aux 0

let generate_java_program (program : Bir.program) (function_spec : Bir_interface.bir_function)
    (filename : string) : unit =
  let bs = 50 in
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  let var_indexes, _ = get_variables_indexes program function_spec in
  Format.fprintf oc "%a" generate_header ();
  Format.fprintf oc "%s" calculateTax_method_header;
  add_el_hor (fun oc -> Format.fprintf oc "/* GENERATE INPUTS */\n");
  generate_input_handling function_spec;
  add_el_hor (fun oc -> Format.fprintf oc "/*GENERATE STATEMENTS*/\n");
  generate_stmts program var_indexes program.statements;
  add_el_hor (fun oc -> Format.fprintf oc "/* GENERATE RETURN */\n");
  generate_return function_spec;
  let split_java_program = split_list !java_program bs in
  print_queue split_java_program oc;
  Format.fprintf oc "return out;\n";
  Format.fprintf oc "\n}";

  List.iteri
    (fun i (sl : (Format.formatter -> unit)) ->
      if i mod bs = 0 && i <> 0 then Format.fprintf oc "}\n";
      if i mod bs = 0 then 
      (Format.fprintf oc
        {| 
      public static void tax_calculation_part%d( 
            Map<String, OptionalDouble> calculationVariables, 
            Map<Integer, OptionalDouble> localVariables, 
            Map<String,List<OptionalDouble>> tableVariables,
            OptionalDouble cond,
            Map<String, OptionalDouble> out,
            Map<String, OptionalDouble> input_variables) {
        |}
        (i / bs));
        sl oc;) 
    (List.rev !java_program);
  Format.fprintf oc 
  {|    
      }
    } 
  |};
  close_out _oc
