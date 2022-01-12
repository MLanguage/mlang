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

open Mir

let verbose_output = ref false

let undefined_class_prelude : string =
  "class Singleton(type):\n\
  \    _instances = {}\n\
  \    def __call__(cls, *args, **kwargs):\n\
  \        if cls not in cls._instances:\n\
  \            cls._instances[cls] = super(Singleton, cls).__call__(*args, \
   **kwargs)\n\
  \        return cls._instances[cls]\n\n\n\
   class Undefined(metaclass=Singleton):\n\
  \    def __init__(self):\n\
  \        pass\n\n\
  \    def __add__(self, rhs):\n\
  \        if isinstance(rhs, Undefined): return self\n\
  \        else: return rhs\n\n\
  \    def __radd__(self, rhs):\n\
  \        if isinstance(rhs, Undefined): return self\n\
  \        else: return rhs\n\n\
  \    def __sub__(self, rhs):\n\
  \        if isinstance(rhs, Undefined): return self\n\
  \        else: return -rhs\n\n\
  \    def __rsub__(self, rhs):\n\
  \        if isinstance(rhs, Undefined): return self\n\
  \        else: return rhs\n\n\
  \    def __neg__(self):\n\
  \        return self\n\n\
  \    def __mul__(self, rhs):\n\
  \        return self\n\n\
  \    def __rmul__(self, rhs):\n\
  \        return self\n\n\
  \    def __truediv__(self, rhs):\n\
  \        return self\n\n\
  \    def __rtruediv__(self, rhs):\n\
  \        return self\n\n\
  \    def __neg__(self):\n\
  \        return 0\n\n\
  \    def __lt__(self, rhs):\n\
  \        return self\n\n\
  \    def __le__(self, rhs):\n\
  \        return self\n\n\
  \    def __gt__(self, rhs):\n\
  \        return self\n\n\
  \    def __ge__(self, rhs):\n\
  \        return self\n\n\
  \    def __eq__(self, rhs):\n\
  \        return self\n\n\
  \    def __ne__(self, rhs):\n\
  \        return self\n\n\
   def m_cond(cond, true, false):\n\
  \    if isinstance(cond, Undefined): return cond\n\
  \    else: return true if cond else false\n\
   def m_div(lhs, rhs):\n\
  \   if not isinstance(rhs, Undefined) and rhs == 0: return 0\n\
  \   else: return lhs / rhs\n\
   def m_max(lhs, rhs):\n\
  \    return max(lhs + 0, rhs + 0)\n\n\
   def m_min(lhs, rhs):\n\
  \    return min(lhs + 0, rhs + 0)\n\n\
   def m_or(lhs, rhs):\n\
  \    return Undefined() if (isinstance(lhs, Undefined) and isinstance(rhs, \
   Undefined)) else lhs + 0 or rhs + 0\n\n\
   def m_and(lhs, rhs):\n\
  \    return Undefined() if (isinstance(lhs, Undefined) or isinstance(rhs, \
   Undefined)) else lhs and rhs\n\n\
   def m_present(e): return isinstance(e, Undefined) == False\n\
   def m_multimax(count, l):\n\
  \    m = l[0] + 0\n\
  \    for i in range(int(count)):\n\
  \        m = max(m, l[i+1] + 0)\n\
  \    return m\n\n\
   def m_round(x):\n\
  \    if isinstance(x, Undefined): return x\n\
  \    else: return float(int(x + (-0.50005 if x < 0 else 0.50005)))\n\n\
   def m_floor(x):\n\
  \    if isinstance(x, Undefined): return x\n\
  \    else: return floor(x + 0.000001)\n\n\
   class GenericIndex:\n\
  \    def __init__(self, lambda_function):\n\
  \      self.l = lambda_function\n\
  \    def __getitem__(self, x):\n\
  \      return self.l(x)"

let none_value = "Undefined()"

let generate_comp_op (op : Mast.comp_op) : string =
  match op with
  | Mast.Gt -> ">"
  | Mast.Gte -> ">="
  | Mast.Lt -> "<"
  | Mast.Lte -> "<="
  | Mast.Eq -> "=="
  | Mast.Neq -> "!="

let generate_binop (op : Mast.binop) : string =
  match op with
  | Mast.And -> "and"
  | Mast.Or -> "or"
  | Mast.Add -> "+"
  | Mast.Sub -> "-"
  | Mast.Mul -> "*"
  | Mast.Div -> "/"

let generate_unop (op : Mast.unop) : string =
  match op with Mast.Not -> "not" | Mast.Minus -> "-"

let generate_variable fmt (var : Variable.t) : unit =
  let v =
    match var.alias with Some v -> v | None -> Pos.unmark var.Variable.name
  in
  let v = String.lowercase_ascii v in
  let v =
    if
      same_execution_number var.Variable.execution_number
        (Mast_to_mir.dummy_exec_number (Pos.get_position var.Variable.name))
    then v
    else
      Format.asprintf "%s_%d_%d" v var.Variable.execution_number.Mir.rule_number
        var.Variable.execution_number.Mir.seq_number
  in
  if '0' <= v.[0] && v.[0] <= '9' then Format.fprintf fmt "var_%s" v
  else Format.fprintf fmt "%s" v

let generate_tgv_variable fmt (var : variable) : unit =
  Format.fprintf fmt "tgv['%a']" generate_variable var

let generate_name (v : Variable.t) : string =
  match v.alias with Some v -> v | None -> Pos.unmark v.Variable.name

let autograd_ref = ref false

let autograd () : bool = !autograd_ref

let rec generate_python_expr safe_bool_binops fmt (e : expression Pos.marked) :
    unit =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      Format.fprintf fmt "(%a) %s (%a)"
        (generate_python_expr safe_bool_binops)
        e1
        (generate_comp_op (Pos.unmark op))
        (generate_python_expr safe_bool_binops)
        e2
  | Binop ((Mast.Div, _), e1, e2) ->
      Format.fprintf fmt "m_div(%a, %a)"
        (generate_python_expr safe_bool_binops)
        e1
        (generate_python_expr safe_bool_binops)
        e2
  | Binop ((((Mast.Or | Mast.And) as f), _), e1, e2) when safe_bool_binops ->
      let f =
        match f with
        | Mast.Or -> "m_or"
        | Mast.And -> "m_and"
        | _ -> assert false
      in
      Format.fprintf fmt "%s(%a, %a)" f
        (generate_python_expr safe_bool_binops)
        e1
        (generate_python_expr safe_bool_binops)
        e2
  | Binop ((op, _), e1, e2) ->
      let left fmt () =
        match Pos.unmark e1 with
        | Binop ((opl, _), _, _) ->
            let left_paren =
              Mast.has_priority opl op
              || Mast.precedence opl = Mast.precedence op
                 && Mast.is_right_associative op
            in
            let lleft_paren, rleft_paren =
              if left_paren then ("(", ")") else ("", "")
            in
            Format.fprintf fmt "%s%a%s" lleft_paren
              (generate_python_expr safe_bool_binops)
              e1 rleft_paren
        | _ -> (generate_python_expr safe_bool_binops) fmt e1
      in
      let right fmt () =
        match Pos.unmark e2 with
        | Binop ((opr, _), _, _) ->
            let right_paren =
              Mast.has_priority opr op
              || Mast.precedence op = Mast.precedence opr
                 && Mast.is_left_associative op
            in
            let lright_paren, rright_paren =
              if right_paren then ("(", ")") else ("", "")
            in
            Format.fprintf fmt "%s%a%s" lright_paren
              (generate_python_expr safe_bool_binops)
              e2 rright_paren
        | _ -> (generate_python_expr safe_bool_binops) fmt e2
      in
      Format.fprintf fmt "%a %s %a" left () (generate_binop op) right ()
  | Unop (op, e) ->
      Format.fprintf fmt "%s (%a)" (generate_unop op)
        (generate_python_expr safe_bool_binops)
        e
  | Index (var, e) -> (
      match Pos.unmark e with
      | Literal (Float f) ->
          Format.fprintf fmt "%a[%d]" generate_tgv_variable (Pos.unmark var)
            (int_of_float f)
      | _ ->
          (* FIXME: int cast hack *)
          Format.fprintf fmt
            "%a[int(%a)] if not isinstance(%a, Undefined) else Undefined()"
            generate_tgv_variable (Pos.unmark var)
            (generate_python_expr safe_bool_binops)
            e
            (generate_python_expr safe_bool_binops)
            e)
  | Conditional (e1, e2, e3) ->
      Format.fprintf fmt "m_cond(%a, %a, %a)"
        (generate_python_expr safe_bool_binops)
        e1
        (generate_python_expr safe_bool_binops)
        e2
        (generate_python_expr safe_bool_binops)
        e3
  | FunctionCall (PresentFunc, [ arg ]) ->
      Format.fprintf fmt "m_present(%a)"
        (generate_python_expr safe_bool_binops)
        arg
  | FunctionCall (NullFunc, [ arg ]) ->
      Format.fprintf fmt "(%a == %s)"
        (generate_python_expr safe_bool_binops)
        arg none_value
  | FunctionCall (ArrFunc, [ arg ]) ->
      if autograd () then (generate_python_expr safe_bool_binops) fmt arg
      else
        Format.fprintf fmt "m_round(%a)"
          (generate_python_expr safe_bool_binops)
          arg
  | FunctionCall (InfFunc, [ arg ]) ->
      if autograd () then (generate_python_expr safe_bool_binops) fmt arg
      else
        Format.fprintf fmt "m_floor(%a)"
          (generate_python_expr safe_bool_binops)
          arg
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      Format.fprintf fmt "m_max(%a, %a)"
        (generate_python_expr safe_bool_binops)
        e1
        (generate_python_expr safe_bool_binops)
        e2
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      Format.fprintf fmt "m_min(%a, %a)"
        (generate_python_expr safe_bool_binops)
        e1
        (generate_python_expr safe_bool_binops)
        e2
  | FunctionCall (Multimax, [ e1; e2 ]) ->
      Format.fprintf fmt "m_multimax(%a, %a)"
        (generate_python_expr safe_bool_binops)
        e1
        (generate_python_expr safe_bool_binops)
        e2
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> Format.fprintf fmt "%s" (string_of_float f)
  | Literal Undefined -> Format.fprintf fmt "%s" none_value
  | Var var -> Format.fprintf fmt "%a" generate_tgv_variable var
  | LocalVar lvar -> Format.fprintf fmt "v%d" lvar.LocalVariable.id
  | GenericTableIndex -> Format.fprintf fmt "generic_index"
  | Error -> assert false (* TODO *)
  | LocalLet (lvar, e1, e2) ->
      Format.fprintf fmt "(lambda v%d: %a)(%a)" lvar.LocalVariable.id
        (generate_python_expr safe_bool_binops)
        e2
        (generate_python_expr safe_bool_binops)
        e1

let generate_var_def var data (oc : Format.formatter) : unit =
  match data.var_definition with
  | SimpleVar e ->
      if !verbose_output then
        Format.fprintf oc "# Defined %a@\n" Pos.format_position_short
          (Pos.get_position e);
      Format.fprintf oc "%a = %a@\n" generate_tgv_variable var
        (generate_python_expr false)
        e
  | TableVar (_, IndexTable es) ->
      Format.fprintf oc "%a = [%a]@\n" generate_tgv_variable var
        (fun fmt ->
          IndexMap.iter (fun _ v ->
              Format.fprintf fmt "%a, " (generate_python_expr false) v))
        es
  | TableVar (_, IndexGeneric e) ->
      if !verbose_output then
        Format.fprintf oc "# Defined %a@\n" Pos.format_position_short
          (Pos.get_position e);
      Format.fprintf oc "%a = GenericIndex(lambda generic_index: %a)@\n@\n"
        generate_tgv_variable var
        (generate_python_expr false)
        e
  | InputVar -> assert false

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc "# -*- coding: utf-8 -*-\n";
  Format.fprintf oc "# %s\n\n" Prelude.message;
  if autograd () then Format.fprintf oc "import numpy as np\n\n"
  else Format.fprintf oc "from math import floor, modf\n\n";
  Format.fprintf oc "%s\n\n" undefined_class_prelude;
  Format.fprintf oc "local_variables = dict()\n\n";
  Format.fprintf oc "out = dict()\n\n";
  Format.fprintf oc "tgv = dict()\n\n\n"

let generate_input_handling oc (function_spec : Bir_interface.bir_function) =
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
  if !verbose_output then
    Format.fprintf oc
      "# The following keys must be present in the input:@\n%a@\n"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun fmt var ->
           Format.fprintf fmt "# %s: %s" (generate_name var)
             (Pos.unmark var.Variable.descr)))
      input_vars;
  Format.fprintf oc "def extracted(input_variables):@\n@[<h 4>    @\n";
  Format.fprintf oc
    "# First we extract the input variables from the dictionnary:@\n%a@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "%a = input_variables[\"%s\"]" generate_tgv_variable
           var (generate_name var)))
    input_vars

let generate_var_cond cond oc =
  if (fst cond.cond_error).typ = Mast.Anomaly then
    Format.fprintf oc
      "# Verification condition %a@\n\
       cond = %a@\n\
       if not(isinstance(cond, Undefined)) and cond:@\n\
      \    raise TypeError(\"Error triggered\\n%a\")@\n\
       @\n"
      Pos.format_position_short
      (Pos.get_position cond.cond_expr)
      (generate_python_expr true)
      cond.cond_expr
      (fun fmt err ->
        Format.fprintf fmt "%s: %s"
          (Strings.sanitize_str err.Error.name)
          (Error.err_descr_string err |> Strings.sanitize_str))
      (fst cond.cond_error)

let rec generate_stmts (program : Bir.program) oc stmts =
  Format.pp_print_list (generate_stmt program) oc stmts

and generate_stmt program oc stmt =
  match Pos.unmark stmt with
  | Bir.SAssign (var, vdata) -> generate_var_def (Bir.var_to_mir var) vdata oc
  | SConditional (cond, tt, []) ->
      let pos = Pos.get_position stmt in
      let fname =
        String.map
          (fun c -> if c = '.' then '_' else c)
          (Filename.basename (Pos.get_file pos))
      in
      let cond_name =
        Format.asprintf "cond_%s_%d_%d_%d_%d" fname (Pos.get_start_line pos)
          (Pos.get_start_column pos) (Pos.get_end_line pos)
          (Pos.get_end_column pos)
      in
      Format.fprintf oc
        "%s = %a@\n\
         if not(isinstance(%s, Undefined)) and %s != 0:@\n\
         @[<h 4>    %a@]@\n"
        cond_name
        (generate_python_expr false)
        (Pos.same_pos_as cond stmt)
        cond_name cond_name (generate_stmts program) tt
  | SConditional (cond, tt, ff) ->
      let pos = Pos.get_position stmt in
      let fname =
        String.map
          (fun c -> if c = '.' then '_' else c)
          (Filename.basename (Pos.get_file pos))
      in
      let cond_name =
        Format.asprintf "cond_%s_%d_%d_%d_%d" fname (Pos.get_start_line pos)
          (Pos.get_start_column pos) (Pos.get_end_line pos)
          (Pos.get_end_column pos)
      in
      Format.fprintf oc
        "%s = %a@\n\
         if not(isinstance(%s, Undefined)) and %s != 0:@\n\
         @[<h 4>    %a@]@\n\
         elif not(isinstance(%s, Undefined)):@\n\
         @[<h 4>    %a@]@\n"
        cond_name
        (generate_python_expr false)
        (Pos.same_pos_as cond stmt)
        cond_name cond_name (generate_stmts program) tt cond_name
        (generate_stmts program) ff
  | SVerif v -> generate_var_cond v oc
  | SRuleCall _ | SFunctionCall _ -> assert false
(* Rule and mpp_function calls removed with [Bir.get_all_statements] below *)

let generate_return oc (function_spec : Bir_interface.bir_function) =
  let returned_variables =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc
    "# The following two lines help us keep all previously defined variable \
     bindings@\n\
     global local_variables@\n\
     local_variables = locals()@\n\
     @\n";
  if List.length returned_variables = 1 then
    Format.fprintf oc "return %a\n@]@\n" generate_tgv_variable
      (List.hd returned_variables)
  else begin
    Format.pp_print_list
      (fun fmt var ->
        Format.fprintf fmt "out[\"%a\"] = %a@\n" generate_variable var
          generate_tgv_variable var)
      oc returned_variables;
    Format.fprintf oc "return out@\n@]\n"
  end

let generate_python_program (program : Bir.program)
    (function_spec : Bir_interface.bir_function) (filename : string) : unit =
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a%a" 
    generate_header () 
    generate_input_handling function_spec 
    (generate_stmts program) (Bir.get_all_statements program)
    generate_return function_spec;
  close_out _oc[@@ocamlformat "disable"]
