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

open Mir

type code_block = AtomicBlock of string | IndentedBlock of code_block list

let pp_code_block oc cb =
  let rec pp_code_block_aux cb indent =
    match cb with
    | IndentedBlock cb -> List.iter (fun x -> pp_code_block_aux x (indent + 1)) (List.rev cb)
    | AtomicBlock ab -> Format.fprintf oc "@[<h %d> %s" indent ab
  in
  pp_code_block_aux cb 0

(** Add element to code_block type horizontally *)
let add_el_hor (el : string) (cb : code_block) : code_block =
  match cb with
  | IndentedBlock ib -> IndentedBlock (AtomicBlock el :: ib)
  | AtomicBlock ab ->
      let new_ab = AtomicBlock ab in
      IndentedBlock [ AtomicBlock el; new_ab ]

(*let add_el_vert (el : string) (cb : code_block) : code_block = match cb with | IndentedBlock ib ->
  IndentedBlock [ AtomicBlock el; IndentedBlock ib ] | AtomicBlock ab -> IndentedBlock [ AtomicBlock
  ab; IndentedBlock [ AtomicBlock ab ] ] *)
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

let rec generate_java_expr (e : expression Pos.marked) (var_indexes : int Mir.VariableMap.t) :
    string * (LocalVariable.t * expression Pos.marked) list =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      (Format.asprintf "%s(%s, %s)" (generate_comp_op (Pos.unmark op)) se1 se2, s1 @ s2)
  | Binop (op, e1, e2) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      (Format.asprintf "%s(%s, %s)" (generate_binop (Pos.unmark op)) se1 se2, s1 @ s2)
  | Unop (op, e) ->
      let se, s = generate_java_expr e var_indexes in
      (Format.asprintf "%s(%s)" (generate_unop op) se, s)
  | Index (var, e) ->
      let _, s = generate_java_expr e var_indexes in
      let size = Option.get (Pos.unmark var).Mir.Variable.is_table in
      ( Format.asprintf "tableVariables.get(\"%a\").get(%d)" generate_variable (Pos.unmark var) size,
        s )
  | Conditional (e1, e2, e3) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      let se3, s3 = generate_java_expr e3 var_indexes in
      (Format.asprintf "m_cond(%s, %s, %s)" se1 se2 se3, s1 @ s2 @ s3)
  | FunctionCall (PresentFunc, [ arg ]) ->
      let se, s = generate_java_expr arg var_indexes in
      (Format.asprintf "mPresent(%s)" se, s)
  | FunctionCall (NullFunc, [ arg ]) ->
      let se, s = generate_java_expr arg var_indexes in
      (Format.asprintf "m_null(%s)" se, s)
  | FunctionCall (ArrFunc, [ arg ]) ->
      let se, s = generate_java_expr arg var_indexes in
      (Format.asprintf "m_round(%s)" se, s)
  | FunctionCall (InfFunc, [ arg ]) ->
      let se, s = generate_java_expr arg var_indexes in
      (Format.asprintf "m_floor(%s)" se, s)
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      (Format.asprintf "m_max(%s, %s)" se1 se2, s1 @ s2)
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      (Format.asprintf "m_min(%s, %s)" se1 se2, s1 @ s2)
  | FunctionCall (Multimax, [ e1; (Var v2, _) ]) ->
      let se1, s1 = generate_java_expr e1 var_indexes in
      (Format.asprintf "m_multimax(%s, tableVariables.get(\"%a\"))" se1 format_var_name v2, s1)
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> (Format.asprintf "OptionalDouble.of(%s)" (string_of_float f), [])
  | Literal Undefined -> (Format.asprintf "%s" none_value, [])
  | Var var -> (Format.asprintf "calculationVariables.get(\"%a\")" format_var_name var, [])
  | LocalVar lvar -> (Format.asprintf "localVariables.get(%d)" lvar.LocalVariable.id, [])
  | GenericTableIndex -> (Format.asprintf "generic_index", [])
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let _, s1 = generate_java_expr e1 var_indexes in
      let se2, s2 = generate_java_expr e2 var_indexes in
      (Format.asprintf "%s" se2, s1 @ ((lvar, e1) :: s2))

let format_local_vars_defs (var_indexes : int Mir.VariableMap.t) (fmt : Format.formatter)
    (defs : (LocalVariable.t * expression Pos.marked) list) =
  List.iter
    (fun (lvar, e) ->
      let se, _ = generate_java_expr e var_indexes in
      Format.fprintf fmt "localVariables.put(%d,%s);@\n" lvar.LocalVariable.id se)
    defs

let generate_method (oc : Format.formatter) ((rule_number : string), (expression : string)) : unit =
  let regex_cond = Re.Pcre.regexp "cond.*" in
  let regex_assign = Re.Pcre.regexp "assign.*" in
  let regex_stmt = Re.Pcre.regexp "stmtMethod.*" in
  match rule_number with
  | _ when Re.Pcre.pmatch ~rex:regex_cond rule_number ->
      Format.fprintf oc
        "private static void generate_%s(OptionalDouble cond, Map<String,OptionalDouble> \
         calculationVariables, Map<Integer, OptionalDouble> localVariables, \
         Map<String,List<OptionalDouble>> tableVariables) { %s;} @\n"
        rule_number expression
  | _ when Re.Pcre.pmatch ~rex:regex_assign rule_number ->
      Format.fprintf oc
        "private static void generate_%s(Map<String,OptionalDouble> calculationVariables, \
         Map<Integer, OptionalDouble> localVariables, Map<String,List<OptionalDouble>> \
         tableVariables, OptionalDouble cond) { %s;} @\n"
        rule_number expression
  | _ when Re.Pcre.pmatch ~rex:regex_stmt rule_number ->
      Format.fprintf oc
        "private static void generate_%s(Map<String,OptionalDouble> calculationVariables, \
         Map<Integer, OptionalDouble> localVariables, Map<String,List<OptionalDouble>> \
         tableVariables, OptionalDouble cond) { %s;} @\n"
        rule_number expression
  | _ ->
      Format.fprintf oc
        "private static OptionalDouble generate_%s(Map<String,OptionalDouble> \
         calculationVariables, Map<Integer, OptionalDouble> localVariables, \
         Map<String,List<OptionalDouble>> tableVariables, OptionalDouble cond) {return (%s);} @\n"
        rule_number expression

let generate_var_def (var_indexes : int Mir.VariableMap.t) (var : Mir.Variable.t)
    (data : Mir.variable_data) methods_to_write java_stmts =
  match data.var_definition with
  | SimpleVar e ->
      let se, defs = generate_java_expr e var_indexes in
      let method_number =
        let v = generate_name var in
        if
          same_execution_number var.Variable.execution_number
            (Mast_to_mvg.dummy_exec_number (Pos.get_position var.Variable.name))
        then v
        else
          Format.asprintf "%s_%d_%d" v var.Variable.execution_number.Mir.rule_number
            var.Variable.execution_number.Mir.seq_number
      in
      let new_java_stmts =
        add_el_hor
          (Format.asprintf
             "%a calculationVariables.put(\"%a\",generate_%s(calculationVariables, localVariables, \
              tableVariables, cond));@\n"
             (format_local_vars_defs var_indexes)
             defs format_var_name var method_number)
          java_stmts
      in
      Hashtbl.replace methods_to_write method_number se;
      new_java_stmts
  | TableVar (_, IndexTable es) ->
      add_el_hor
        (Format.asprintf "@\n   tableVariables.put(\"%a\",Arrays.asList(%s));@\n" format_var_name
           var
           (String.concat ","
              (let array_of_variables = ref [] in
               IndexMap.iter
                 (fun _ v ->
                   let string_genere, _ = generate_java_expr v var_indexes in
                   array_of_variables := List.append !array_of_variables [ string_genere ])
                 es;
               !array_of_variables)))
        java_stmts
  | TableVar (_, IndexGeneric _) -> java_stmts
  (*Format.asprintf "%a = %a;@\n@\n" generate_variable var (generate_java_expr) e :: java_stmts*)
  | InputVar -> assert false

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc "// %s\n\n %s\n\npublic class CalculImpot {@\n" Prelude.message java_imports

let split_list (list_to_split : 'a list) =
  let rec split_list_aux (list_to_split : 'a list) (split_lists : 'a list list)
      (current_list : 'a list) count : 'a list list =
    match list_to_split with
    | [] -> split_lists
    | hd :: tl ->
        if count mod 100 = 0 then
          let new_list = current_list :: split_lists in
          split_list_aux tl new_list [] (count + 1)
        else
          let new_current_list = hd :: current_list in
          split_list_aux tl split_lists new_current_list (count + 1)
  in
  List.length list_to_split |> Format.asprintf "Length of list_to_split %d" |> print_endline;
  split_list_aux list_to_split [] [] 0

let rec generate_input_list variables (input_methods : string list) =
  match variables with
  | [] -> input_methods
  | hd :: tl ->
      let current_method =
        Format.asprintf
          "calculationVariables.put(\"%a\",input_variables.get(\"%s\") != null ? \
           input_variables.get(\"%s\") : OptionalDouble.empty());"
          format_var_name hd (generate_name hd) (generate_name hd)
      in
      let updated_array = current_method :: input_methods in
      generate_input_list tl updated_array

let rec write_input_methods oc (methods : string list list) count =
  match methods with
  | [] -> ()
  | hd :: tl ->
      print_endline ("write_input_methods count = " ^ string_of_int count);
      Format.fprintf oc
        "private static void loadInputVariables_%d(Map<String, OptionalDouble> \
         calculationVariables, Map<String, OptionalDouble> input_variables, OptionalDouble cond) {\n\
         %a}\n"
        count
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
           (fun fmt var ->
             print_endline ("write_input_methods" ^ var);
             Format.fprintf fmt "%s" var))
        hd;
      write_input_methods oc tl (count + 1)

let generate_input_handling (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  let input_methods = generate_input_list input_vars [] in
  let debug_item = split_list input_methods in
  print_endline
    (Format.asprintf "generate_input_handling return length %d" (List.length debug_item));
  debug_item

let rec generate_input_calls count input_methods oc =
  match input_methods with
  | [] -> ()
  | _ :: tl ->
      Format.fprintf oc "loadInputVariables_%d(calculationVariables, input_variables, cond);\n"
        count;
      generate_input_calls (count + 1) tl oc

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

let generate_var_cond var_indexes cond java_stmts =
  add_el_hor
    (Format.asprintf
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
    java_stmts

let rec generate_stmts (program : Bir.program) (var_indexes : int Mir.VariableMap.t)
    methods_to_write (java_stmts : code_block) (stmts : Bir.stmt list) oc =
  let local_generate stmt = generate_stmt program var_indexes methods_to_write java_stmts stmt oc in
  match stmts with
  | hd :: tl ->
      let list = local_generate hd in
      generate_stmts program var_indexes methods_to_write list tl oc
  | [] -> java_stmts

and generate_stmt (program : Bir.program) (var_indexes : int Mir.VariableMap.t) methods_to_write
    (java_stmts : code_block) (stmt : Bir.stmt) oc =
  match Pos.unmark stmt with
  | Bir.SAssign (var, vdata) -> generate_var_def var_indexes var vdata methods_to_write java_stmts
  | SConditional (cond, tt, ff) -> (
      let pos = Pos.get_position stmt in
      let fname =
        String.map (fun c -> if c = '.' then '_' else c) (Filename.basename (Pos.get_file pos))
      in
      let cond_name =
        Format.asprintf "cond_%s_%d_%d_%d_%d" fname (Pos.get_start_line pos)
          (Pos.get_start_column pos) (Pos.get_end_line pos) (Pos.get_end_column pos)
      in
      let true_case = generate_stmts program var_indexes methods_to_write java_stmts tt oc in
      let false_case = (generate_stmts program var_indexes methods_to_write java_stmts) ff oc in
      let if_begin =
        add_el_hor
          (Format.asprintf
             {|
          /* SConditional (cond, tt, ff) */
          OptionalDouble %s = %s;
          if (! %s.isPresent() && %s.getAsDouble() != 0) {
        |}
             cond_name
             (let s, _ = generate_java_expr (Pos.same_pos_as cond stmt) var_indexes in
              s)
             cond_name cond_name)
          java_stmts
      in
      let if_true_body =
        match if_begin with
        | IndentedBlock ib -> IndentedBlock [ IndentedBlock [ true_case ]; IndentedBlock ib ]
        | _ -> assert false
      in
      let else_begin =
        add_el_hor
          (Format.asprintf {|
           } else if (!%s.isPresent()) {
        |} cond_name)
          if_true_body
      in
      match else_begin with
      | IndentedBlock ib -> IndentedBlock [ IndentedBlock [ false_case ]; IndentedBlock ib ]
      | _ -> assert false)
  | SVerif v -> generate_var_cond var_indexes v java_stmts

let generate_return (oc : Format.formatter) (function_spec : Bir_interface.bir_function) =
  let returned_variables = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.pp_print_list
    (fun oc (var : Variable.t) ->
      Format.fprintf oc "out.put(\"%a\",calculationVariables.get(\"%a\"));@\n" format_var_name var
        format_var_name var)
    oc returned_variables;
  Format.fprintf oc "return out;@\n@]\n}"

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

let generate_calculation_methods (oc : Format.formatter) hashtbl : unit =
  Seq.iter (generate_method oc) (Hashtbl.to_seq hashtbl)

let generate_java_program (program : Bir.program) (function_spec : Bir_interface.bir_function)
    (filename : string) : unit =
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  let methods_to_write = Hashtbl.create 1 in
  let input_method_lists = generate_input_handling function_spec in
  print_endline
    ("input_method_lists length in generate_java_expr = "
    ^ string_of_int (List.length input_method_lists));
  let var_indexes, _ = get_variables_indexes program function_spec in
  Format.fprintf oc "%a" generate_header ();
  Format.fprintf oc "%s" calculateTax_method_header;
  generate_input_calls 0 input_method_lists oc;
  Format.fprintf oc "/*GENERATE STATEMENTS*/\n";
  generate_stmts program var_indexes methods_to_write (IndentedBlock []) program.statements oc
  |> pp_code_block oc;
  Format.fprintf oc "/* GENERATE RETURN */ \n%a\n" generate_return function_spec;
  Format.fprintf oc "/* GENERATE CALCULATION METHODS */\n%a\n" generate_calculation_methods
    methods_to_write;
  write_input_methods oc input_method_lists 0;
  Format.fprintf oc "\n}";
  close_out _oc
