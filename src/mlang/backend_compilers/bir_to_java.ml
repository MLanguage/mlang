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

let generate_comp_op (op : Com.comp_op) : string =
  let open Com in
  match op with
  | Gt -> "mGreaterThan"
  | Gte -> "mGreaterThanEqual"
  | Lt -> "mLessThan"
  | Lte -> "mLessThanEqual"
  | Eq -> "mEqual"
  | Neq -> "mNotEqual"

let generate_binop (op : Com.binop) : string =
  let open Com in
  match op with
  | And -> "mAnd"
  | Or -> "mOr"
  | Add -> "mAdd"
  | Sub -> "mSubtract"
  | Mul -> "mMultiply"
  | Div -> "mDivide"

let generate_unop (op : Com.unop) : string =
  match op with Com.Not -> "mNot" | Com.Minus -> "mNeg"

let generate_var_name (var : Com.Var.t) : string =
  let v = Pos.unmark var.name in
  String.uppercase_ascii v

let format_var_name (fmt : Format.formatter) (var : Com.Var.t) : unit =
  Format.fprintf fmt "%s" (generate_var_name var)

let generate_name (v : Com.Var.t) : string =
  match Com.Var.alias v with
  | Some v -> Pos.unmark v
  | None -> Pos.unmark v.name

let print_double_cut oc () = Format.fprintf oc "@,@,"

let get_var_pos (_var : Com.Var.t) : int = 0 (* var.Bir.offset *)

let get_tgv_position (var : Com.Var.t) : string =
  Format.asprintf "tgv[%d /* %s */]" (get_var_pos var) (generate_var_name var)

let rec generate_java_expr (e : Mir.expression Pos.marked) :
    string * (Com.Var.t * Mir.expression Pos.marked) list =
  match Pos.unmark e with
  | TestInSet _ -> assert false
  | Comparison (op, e1, e2) ->
      let se1, s1 = generate_java_expr e1 in
      let se2, s2 = generate_java_expr e2 in
      let se3, s3 =
        ( Format.asprintf "%s(%s, %s)" (generate_comp_op (Pos.unmark op)) se1 se2,
          s1 @ s2 )
      in
      (se3, s3)
  | Binop (op, e1, e2) ->
      let se1, s1 = generate_java_expr e1 in
      let se2, s2 = generate_java_expr e2 in
      let se3, s3 =
        ( Format.asprintf "%s(%s, %s)" (generate_binop (Pos.unmark op)) se1 se2,
          s1 @ s2 )
      in
      (se3, s3)
  | Unop (op, e) ->
      let se, s = generate_java_expr e in
      let se2, s2 = (Format.asprintf "%s(%s)" (generate_unop op) se, s) in
      (se2, s2)
  | Index (var, e) ->
      let se, s = generate_java_expr e in
      let unmarked_var = Pos.unmark var in
      let size = Option.get unmarked_var.is_table in
      let se2, s2 =
        ( Format.asprintf "m_array_index(tgv, %d ,%s, %d)"
            (get_var_pos unmarked_var) se size,
          s )
      in
      (se2, s2)
  | Conditional (e1, e2, e3) ->
      let se1, s1 = generate_java_expr e1 in
      let se2, s2 = generate_java_expr e2 in
      let e3 =
        match e3 with
        | None -> (Com.Literal Com.Undefined, Pos.no_pos)
        | Some e -> e
      in
      let se3, s3 = generate_java_expr e3 in
      let se4, s4 =
        (Format.asprintf "m_cond(%s, %s, %s)" se1 se2 se3, s1 @ s2 @ s3)
      in
      (se4, s4)
  | FuncCall ((PresentFunc, _), [ arg ]) ->
      let se, s = generate_java_expr arg in
      let se2, s2 = (Format.asprintf "mPresent(%s)" se, s) in
      (se2, s2)
  | FuncCall ((NullFunc, _), [ arg ]) ->
      let se, s = generate_java_expr arg in
      let se2, s2 = (Format.asprintf "m_null(%s)" se, s) in
      (se2, s2)
  | FuncCall ((ArrFunc, _), [ arg ]) ->
      let se, s = generate_java_expr arg in
      let se2, s2 = (Format.asprintf "m_round(%s)" se, s) in
      (se2, s2)
  | FuncCall ((InfFunc, _), [ arg ]) ->
      let se, s = generate_java_expr arg in
      let se2, s2 = (Format.asprintf "m_floor(%s)" se, s) in
      (se2, s2)
  | FuncCall ((AbsFunc, _), [ arg ]) ->
      let se, s = generate_java_expr arg in
      let se2, s2 = (Format.asprintf "m_abs(%s)" se, s) in
      (se2, s2)
  | FuncCall ((MaxFunc, _), [ e1; e2 ]) ->
      let se1, s1 = generate_java_expr e1 in
      let se2, s2 = generate_java_expr e2 in
      let se3, s3 = (Format.asprintf "m_max(%s, %s)" se1 se2, s1 @ s2) in
      (se3, s3)
  | FuncCall ((MinFunc, _), [ e1; e2 ]) ->
      let se1, s1 = generate_java_expr e1 in
      let se2, s2 = generate_java_expr e2 in
      let se3, s3 = (Format.asprintf "m_min(%s, %s)" se1 se2, s1 @ s2) in
      (se3, s3)
  | FuncCall ((Multimax, _), [ e1; (Var v2, _) ]) ->
      let se1, s1 = generate_java_expr e1 in
      let se2, s2 =
        (Format.asprintf "m_multimax(%s, tgv, %d)" se1 (get_var_pos v2), [])
      in
      (se2, s1 @ s2)
  | FuncCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> (
      match f with
      | 0. -> (Format.asprintf "MValue.zero", [])
      | 1. -> (Format.asprintf "MValue.one", [])
      | _ -> (Format.asprintf "new MValue(%s)" (string_of_float f), []))
  | Literal Undefined -> (Format.asprintf "%s" none_value, [])
  | Var var -> (get_tgv_position var, [])
  | Attribut _ | Size _ | NbAnomalies | NbDiscordances | NbInformatives
  | NbBloquantes ->
      Errors.raise_spanned_error "not yet implemented !!!" (Pos.get_position e)
  | NbCategory _ -> assert false
  | FuncCallLoop _ | Loop _ -> assert false

let generate_input_handling (oc : Format.formatter) (_split_threshold : int) =
  let input_methods_count = ref 0 in
  let print_input fmt var =
    Format.fprintf fmt
      "%s = inputVariables.get(\"%s\") != null ? inputVariables.get(\"%s\") : \
       MValue.mUndefined;"
      (get_tgv_position var) (generate_name var) (generate_name var)
  in
  let _print_method fmt inputs =
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

let fresh_cond_counter = ref 0

let rec generate_stmts (program : Mir.program) (oc : Format.formatter)
    (stmts : Mir.m_instruction list) =
  Format.pp_print_list (generate_stmt program) oc stmts

and generate_stmt (program : Mir.program) (oc : Format.formatter)
    (stmt : Mir.m_instruction) : unit =
  match Pos.unmark stmt with
  | Affectation _f -> assert false
  | IfThenElse (cond, tt, ff) ->
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
        (let s, _ = generate_java_expr cond in
         s)
        cond_name (generate_stmts program) tt;
      Format.fprintf oc " @[<hv 2>if (m_is_defined_false(%s)) {@,%a@]@,}"
        cond_name (generate_stmts program) ff
  | VerifBlock s -> generate_stmts program oc s
  | ComputeTarget (f, _) ->
      Format.fprintf oc "MppFunction.%s(mCalculation, calculationErrors);" f
  | Print (std, args) ->
      let print_std =
        match std with StdOut -> "System.out" | StdErr -> "System.err"
      in
      List.iter
        (function
          | Com.PrintString s ->
              Format.fprintf oc "%s(\"%%s\", %s);@," print_std s
          | Com.PrintName (_, pos) | Com.PrintAlias (_, pos) ->
              Errors.raise_spanned_error "not implemented yet !!!" pos
          | Com.PrintIndent _e ->
              Errors.raise_spanned_error "not implemented yet !!!"
                (Pos.get_position stmt)
          | Com.PrintExpr (e, _, _) ->
              Format.fprintf oc "cond = %s;@,%s(\"%%s\", cond.toString());@,"
                (fst (generate_java_expr e))
                print_std)
        (List.map Pos.unmark args)
  | Iterate _ ->
      Errors.raise_spanned_error "iterators not implemented in Java"
        (Pos.get_position stmt)
  | Restore _ ->
      Errors.raise_spanned_error "restorators not implemented in Java"
        (Pos.get_position stmt)
  | RaiseError _ | CleanErrors | ExportErrors | FinalizeErrors ->
      Errors.raise_spanned_error "errors not implemented in Java"
        (Pos.get_position stmt)
  | ComputeDomain _ | ComputeChaining _ | ComputeVerifs _ -> assert false

let generate_return (oc : Format.formatter) (_x : 'a) =
  let returned_variables = [] in
  let print_outputs oc returned_variables =
    Format.pp_print_list
      (fun oc var ->
        Format.fprintf oc "outputVariables.put(\"%a\",%s);" format_var_name var
          (get_tgv_position var))
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

let generate_calculateTax_method (calculation_vars_len : int)
    (program : Mir.program) (locals_size : int) (oc : Format.formatter) () =
  let main_statements =
    (Mir.TargetMap.find program.program_main_target program.program_targets)
      .target_prog
  in
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
    print_double_cut () print_double_cut () (generate_stmts program)
    main_statements

let generate_main_class (program : Mir.program) (var_table_size : int)
    (locals_size : int) (fmt : Format.formatter) (filename : string) =
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
    () generate_return []

let generate_java_program (program : Mir.program) 
    (filename : string) : unit =
  let split_treshold = 100 in
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  let locals_size = 0 in (* Bir.get_locals_size program |> ( + ) 1 in *)
  let var_table_size = 0 in (* Bir.size_of_tgv () in *)
  let program = program in (*Bir.squish_statements program split_treshold "java_rule_" in*)
  Format.fprintf oc
    "@[<v 0>%a%a\
     @[<v 2>class InputHandler {@,%a@]@,}%a\
     @[<v 2>class MppFunction {@,@]@,}%a\
     @,}@]@."
     (generate_main_class program var_table_size locals_size) filename
     print_double_cut ()
     generate_input_handling split_treshold
     print_double_cut ()
     print_double_cut ();
  close_out _oc[@@ocamlformat "disable"]
