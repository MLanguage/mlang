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

let verbose_output = ref false

let m_operation_class: string = {|

  private static OptionalDouble mGreaterThan(OptionalDouble value1, OptionalDouble value2) {
    return unopCondition((firstValue, secondValue) -> {
      return firstValue.getAsDouble() > secondValue.getAsDouble();
    }, value1, value2);
  }

  private static OptionalDouble mGreaterThanEqual(OptionalDouble value1, OptionalDouble value2) {
    return unopCondition((firstValue, secondValue) -> {
      return firstValue.getAsDouble() >= secondValue.getAsDouble();
    }, value1, value2);
  }

  private static OptionalDouble mLessThan(OptionalDouble value1, OptionalDouble value2) {
    return unopCondition((firstValue, secondValue) -> {
      return firstValue.getAsDouble() < secondValue.getAsDouble();
    }, value1, value2);
  }

  private static OptionalDouble mLessThanEqual(OptionalDouble value1, OptionalDouble value2) {
    return unopCondition((firstValue, secondValue) -> {
      return firstValue.getAsDouble() <= secondValue.getAsDouble();
    }, value1, value2);
  }

  private static OptionalDouble mEqual(OptionalDouble value1, OptionalDouble value2) {
    return unopCondition((firstValue, secondValue) -> {
      return firstValue.getAsDouble() == secondValue.getAsDouble();
    }, value1, value2);
  }

  private static OptionalDouble mNotEqual(OptionalDouble value1, OptionalDouble value2) {
    return unopCondition((firstValue, secondValue) -> {
      return firstValue.getAsDouble() != secondValue.getAsDouble();
    }, value1, value2);
  }

  private static OptionalDouble mAnd(OptionalDouble value1, OptionalDouble value2) {
    return unopCondition((firstValue, secondValue) -> {
      return firstValue.getAsDouble() != 0d && secondValue.getAsDouble() != 0d;
    }, value1, value2);
  }

  private static OptionalDouble mOr(OptionalDouble value1, OptionalDouble value2) {
    return unopCondition((firstValue, secondValue) -> {
      return firstValue.getAsDouble() != 0d || secondValue.getAsDouble() != 0d;
    }, value1, value2);
  }

  private static OptionalDouble mAdd(OptionalDouble value1, OptionalDouble value2) {
    return binopCondition((firstValue, secondValue) -> {
      return OptionalDouble.of(firstValue.getAsDouble() + secondValue.getAsDouble());
    }, value1, value2);
  }

  private static OptionalDouble mSubstract(OptionalDouble value1, OptionalDouble value2) {
    return binopCondition((firstValue, secondValue) -> {
      return OptionalDouble.of(firstValue.getAsDouble() - secondValue.getAsDouble());
    }, value1, value2);
  }

  private static OptionalDouble mMultiply(OptionalDouble value1, OptionalDouble value2) {
    return binopCondition((firstValue, secondValue) -> {
      return OptionalDouble.of(firstValue.getAsDouble() * secondValue.getAsDouble());
    }, value1, value2);
  }

  private static OptionalDouble mDivide(OptionalDouble value1, OptionalDouble value2) {

    if (valuesNotPresent(value1, value2)) {
      return OptionalDouble.empty();
    }

    double denominateur = value2.getAsDouble();

    if (denominateur == 0) {
      return OptionalDouble.of(0);
    }

    return OptionalDouble.of(value1.getAsDouble() / denominateur);

  }

  private static OptionalDouble unopCondition(BiFunction<OptionalDouble, OptionalDouble, Boolean> condition,
      OptionalDouble value1, OptionalDouble value2) {

    if (valuesNotPresent(value1, value2)) {
      return OptionalDouble.empty();
    }

    if (condition.apply(value1, value2)) {
      return OptionalDouble.of(1);
    } else {
      return OptionalDouble.of(0);
    }
  }

  private static OptionalDouble binopCondition(BiFunction<OptionalDouble, OptionalDouble, OptionalDouble> condition,
      OptionalDouble value1, OptionalDouble value2) {

    if (valuesNotPresent(value1, value2)) {
      return OptionalDouble.empty();
    }

    return condition.apply(value1, value2);
  }

  private static boolean valuesNotPresent(OptionalDouble value1, OptionalDouble value2) {
    return value1.isEmpty() || value2.isEmpty();
  }

  private static OptionalDouble m_round(OptionalDouble value) {
    if (!value.isPresent()) {
      return value;
    }
    double valueToRound = value.getAsDouble() + value.getAsDouble() < 0 ? -0.50005 : 0.50005;
    return OptionalDouble.of(Math.round(valueToRound));
  }

    private static OptionalDouble m_floor(OptionalDouble value) {
    if (!value.isPresent()) {
      return value;
    }
    double valueToFloor = value.getAsDouble() + 0.000001;
    return OptionalDouble.of(Math.floor(valueToFloor));
  }

   private static OptionalDouble m_cond(OptionalDouble value, OptionalDouble value2, OptionalDouble value3) {
    if (!value.isPresent()) {
      return value;
    } else if (value.getAsDouble() != 0) {
      return value2;
    } else {
      return value3;
    }
  }

    private static OptionalDouble m_max(OptionalDouble value1, OptionalDouble value2) {
    if (value1.isEmpty() && value2.isPresent()) {
      return value2;
    } else if (value1.isPresent() && value2.isEmpty()) {
      return value1;
    } else if (value1.isEmpty() && value2.isEmpty()) {
      return OptionalDouble.empty();
    } else {
      return OptionalDouble.of(Math.max(value1.getAsDouble(), value2.getAsDouble()));
    }
  }

  private static OptionalDouble m_min(OptionalDouble value1, OptionalDouble value2) {
    if (value1.isEmpty() && value2.isPresent()) {
      return value2;
    } else if (value1.isPresent() && value2.isEmpty()) {
      return value1;
    } else if (value1.isEmpty() && value2.isEmpty()) {
      return OptionalDouble.empty();
    } else {
      return OptionalDouble.of(Math.min(value1.getAsDouble(), value2.getAsDouble()));
    }
  }

  private static OptionalDouble mNeg(OptionalDouble value) {
    if (value.isEmpty()) {
      return value;
    }
    return OptionalDouble.of(-value.getAsDouble());
  }

}
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

let generate_unop (op : Mast.unop) : string = match op with Mast.Not -> "mNot" | Mast.Minus -> "mNeg"

let generate_variable fmt (var : Variable.t) : unit =
  let v = match var.alias with Some v -> v | None -> Pos.unmark var.Variable.name in
  let v = String.lowercase_ascii v in
  let v =
    if
      same_execution_number var.Variable.execution_number
        (Mast_to_mvg.dummy_exec_number (Pos.get_position var.Variable.name))
    then v
    else
      Format.asprintf "%s_%d_%d" v var.Variable.execution_number.Mir.rule_number
        var.Variable.execution_number.Mir.seq_number
  in
  if '0' <= v.[0] && v.[0] <= '9' then Format.fprintf fmt "var_%s" v else Format.fprintf fmt "%s" v

let generate_name (v : Variable.t) : string =
  match v.alias with Some v -> v | None -> Pos.unmark v.Variable.name

let autograd_ref = ref false

let autograd () : bool = !autograd_ref

let rec generate_java_expr safe_bool_binops fmt (e : expression Pos.marked) : unit =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      Format.fprintf fmt "%s((%a),(%a))"
        (generate_comp_op (Pos.unmark op))
        (generate_java_expr safe_bool_binops)
        e1     
        (generate_java_expr safe_bool_binops)
        e2
  | Binop ((op, _), e1, e2) ->
      Format.fprintf fmt "%s(%a,%a)" (generate_binop op)
        (generate_java_expr safe_bool_binops) e1
        (generate_java_expr safe_bool_binops) e2;
  | Unop (op, e) ->
      Format.fprintf fmt "%s (%a)" (generate_unop op) (generate_java_expr safe_bool_binops) e
  | Index (var, e) -> (
      match Pos.unmark e with
      | Literal (Float f) ->
          Format.fprintf fmt "%a.get(%d)" generate_variable (Pos.unmark var) (int_of_float f)
      | _ ->
          Format.fprintf fmt "%a.get(%a)"
            generate_variable (Pos.unmark var)
            (generate_java_expr safe_bool_binops)
            e)
  | Conditional (e1, e2, e3) ->
      Format.fprintf fmt "m_cond(%a, %a, %a)"
        (generate_java_expr safe_bool_binops)
        e1
        (generate_java_expr safe_bool_binops)
        e2
        (generate_java_expr safe_bool_binops)
        e3
  | FunctionCall (PresentFunc, [ arg ]) ->
      Format.fprintf fmt "m_present(%a)" (generate_java_expr safe_bool_binops) arg
  | FunctionCall (NullFunc, [ arg ]) ->
      Format.fprintf fmt "(%a == %s)" (generate_java_expr safe_bool_binops) arg none_value
  | FunctionCall (ArrFunc, [ arg ]) ->
      if autograd () then (generate_java_expr safe_bool_binops) fmt arg
      else Format.fprintf fmt "m_round(%a)" (generate_java_expr safe_bool_binops) arg
  | FunctionCall (InfFunc, [ arg ]) ->
      if autograd () then (generate_java_expr safe_bool_binops) fmt arg
      else Format.fprintf fmt "m_floor(%a)" (generate_java_expr safe_bool_binops) arg
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      Format.fprintf fmt "m_max(%a, %a)"
        (generate_java_expr safe_bool_binops)
        e1
        (generate_java_expr safe_bool_binops)
        e2
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      Format.fprintf fmt "m_min(%a, %a)"
        (generate_java_expr safe_bool_binops)
        e1
        (generate_java_expr safe_bool_binops)
        e2
  | FunctionCall (Multimax, [ e1; e2 ]) ->
      Format.fprintf fmt "m_multimax(%a, %a)"
        (generate_java_expr safe_bool_binops)
        e1
        (generate_java_expr safe_bool_binops)
        e2
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> Format.fprintf fmt "OptionalDouble.of(%s)" (string_of_float f)
  | Literal Undefined -> Format.fprintf fmt "%s" none_value
  | Var var -> Format.fprintf fmt "%a" generate_variable var
  | LocalVar lvar -> Format.fprintf fmt "v%d" lvar.LocalVariable.id
  | GenericTableIndex -> Format.fprintf fmt "generic_index"
  | Error -> assert false (* TODO *)
  | LocalLet (_, e1, e2) ->
      Format.fprintf fmt "%a(%a)"    
        (generate_java_expr safe_bool_binops)
        e2
        (generate_java_expr safe_bool_binops)
        e1

let generate_var_def var data (oc : Format.formatter) : unit =
  match data.var_definition with
  | SimpleVar e ->
      Format.fprintf oc "OptionalDouble %a = %a;@\n" generate_variable var (generate_java_expr false) e
  | TableVar (_, IndexTable es) ->
      Format.fprintf oc "List<OptionalDouble> %a = {%a}@\n" generate_variable var
        (fun fmt ->
          IndexMap.iter (fun _ v -> Format.fprintf fmt "%a, " (generate_java_expr false) v))
        es
  | TableVar (_, IndexGeneric e) ->
      Format.fprintf oc "%a = %a;@\n@\n" generate_variable var
        (generate_java_expr false) e
  | InputVar -> assert false

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc "// %s\n\n" Prelude.message;
  Format.fprintf oc "import java.util.Map;@\nimport java.util.OptionalDouble;@\nimport java.util.function.BiFunction;
@\nimport java.util.HashMap;";
  Format.fprintf oc "public class CalculImpot {@\n";
  Format.fprintf oc "private Map<String, OptionalDouble> local_variables = new HashMap<>();\n\n\n"

let generate_input_handling oc (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  Format.fprintf oc "%a@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "OptionalDouble %a = input_variables.get(\"%s\") != null ? input_variables.get(\"%s\") : OptionalDouble.empty();" generate_variable var
           (generate_name var) (generate_name var)))
    input_vars

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

let generate_var_cond cond oc =
  Format.fprintf oc
    "cond = %a;@\n\
     if (cond.isPresent() && (cond.getAsDouble() != 0)) { @\n\
     \   throw new RuntimeException(\"Error triggered\\n%a\");@\n\
     }@\n\
     @\n"
    (generate_java_expr true) cond.cond_expr
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt err ->
         Format.fprintf fmt "%s: %s" (sanitize_str err.Error.name) (sanitize_str err.Error.descr)))
    cond.cond_errors

let rec generate_stmts (program : Bir.program) oc stmts =
  Format.pp_print_list (generate_stmt program) oc stmts

and generate_stmt program oc stmt =
  match Pos.unmark stmt with
  | Bir.SAssign (var, vdata) -> generate_var_def var vdata oc
  | SConditional (cond, tt, []) ->
      let pos = Pos.get_position stmt in
      let fname =
        String.map (fun c -> if c = '.' then '_' else c) (Filename.basename (Pos.get_file pos))
      in
      let cond_name =
        Format.asprintf "cond_%s_%d_%d_%d_%d" fname (Pos.get_start_line pos)
          (Pos.get_start_column pos) (Pos.get_end_line pos) (Pos.get_end_column pos)
      in
      Format.fprintf oc
        "OptionalDouble %s = %a;@\nif (!%s.isPresent() || %s.getAsDouble() != 0){@\n@[<h 4>    %a@]}@\n" cond_name
        (generate_java_expr false) (Pos.same_pos_as cond stmt) cond_name cond_name
        (generate_stmts program) tt
  | SConditional (cond, tt, ff) ->
      let pos = Pos.get_position stmt in
      let fname =
        String.map (fun c -> if c = '.' then '_' else c) (Filename.basename (Pos.get_file pos))
      in
      let cond_name =
        Format.asprintf "cond_%s_%d_%d_%d_%d" fname (Pos.get_start_line pos)
          (Pos.get_start_column pos) (Pos.get_end_line pos) (Pos.get_end_column pos)
      in
      Format.fprintf oc
        "%s = %a@\n\
         if (!%s.isPresent() && %s != 0){@\n\
         @[<h 4>    %a@]}@\n\
         else if (!%s.isPresent()){@\n\
         @[<h 4>    %a@]}@\n"
        cond_name (generate_java_expr false) (Pos.same_pos_as cond stmt) cond_name cond_name
        (generate_stmts program) tt cond_name (generate_stmts program) ff
  | SVerif v -> generate_var_cond v oc

let generate_return oc (function_spec : Bir_interface.bir_function) =
  let returned_variables = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  if List.length returned_variables = 1 then
    Format.fprintf oc "return %a\n@]@\n" generate_variable (List.hd returned_variables)
  else begin
    Format.pp_print_list
      (fun fmt var ->
        Format.fprintf fmt "out.put(\"%a\",%a)@\n" generate_variable var generate_variable var)
      oc returned_variables;
    Format.fprintf oc "return out@\n@]\n"
  end;
  Format.fprintf oc "}"

let generate_java_program (program : Bir.program) (function_spec : Bir_interface.bir_function)
    (filename : string) : unit =
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%s%a%a%s%s" 
    generate_header () 
    "public static void enchainerCalcul(Map<String,OptionalDouble> input_variables) { \n OptionalDouble cond;\n"
    generate_input_handling function_spec
    (generate_stmts program) program.statements 
    "}\n"
    m_operation_class; 
  close_out _oc
