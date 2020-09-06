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

let undefined_class_prelude : string =
  "class Undefined:\n\
  \    def __init__(self):\n\
  \        pass\n\n\
  \    def __add__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return rhs\n\n\
  \    def __radd__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return rhs\n\n\
  \    def __sub__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return rhs\n\n\
  \    def __rsub__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return rhs\n\n\
  \    def __mul__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return rhs\n\n\
  \    def __rmul__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return rhs\n\n\
  \    def __truediv__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return 0.0\n\n\
  \    def __rtruediv__(self, lhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return Undefined()\n\n\
  \    def __lt__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return Undefined()\n\n\
  \    def __lte__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return Undefined()\n\n\
  \    def __gt__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return Undefined()\n\n\
  \    def __gte__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return Undefined()\n\n\
  \    def __eq__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return Undefined()\n\n\
  \    def __neq__(self, rhs):\n\
  \        if isinstance(rhs, Undefined):\n\
  \            return Undefined()\n\
  \        else:\n\
  \            return Undefined()\n"

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

let generate_unop (op : Mast.unop) : string = match op with Mast.Not -> "not" | Mast.Minus -> "-"

let generate_variable (var : Variable.t) : string =
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
  if Re.Str.string_match (Re.Str.regexp "[0-9].+") v 0 then "var_" ^ v else v

let generate_name (v : Variable.t) : string =
  match v.alias with Some v -> v | None -> Pos.unmark v.Variable.name

let generate_typ (typ : typ) : string = match typ with Real -> "float"

let autograd () : bool = !Cli.backend = "autograd"

let rec generate_python_expr (e : expression) : string =
  match e with
  | Comparison (op, e1, e2) ->
      let s1 = generate_python_expr (Pos.unmark e1) in
      let s2 = generate_python_expr (Pos.unmark e2) in
      Format.asprintf "(%s %s %s)" s1 (generate_comp_op (Pos.unmark op)) s2
  | Binop ((Mast.Div, _), e1, e2) -> (
      let s1 = generate_python_expr (Pos.unmark e1) in
      let s2 = generate_python_expr (Pos.unmark e2) in
      match Pos.unmark e2 with
      | _ -> Format.asprintf "((%s / %s) if %s != 0.0 else %s)" s1 s2 s2 none_value )
  | Binop ((op, _), e1, e2) ->
      let left =
        let s1 = generate_python_expr (Pos.unmark e1) in
        match Pos.unmark e1 with
        | Binop ((opl, _), _, _) ->
            let left_paren =
              Mast.has_priority opl op
              || (Mast.precedence opl = Mast.precedence op && Mast.is_right_associative opl)
            in
            let lleft_paren, rleft_paren = if left_paren then ("(", ")") else ("", "") in
            Format.asprintf "%s%s%s" lleft_paren s1 rleft_paren
        | _ -> Format.asprintf "%s" s1
      in
      let right =
        let s2 = generate_python_expr (Pos.unmark e2) in
        match Pos.unmark e2 with
        | Binop ((opr, _), _, _) ->
            let right_paren =
              Mast.has_priority op opr
              || (Mast.precedence op = Mast.precedence opr && Mast.is_left_associative opr)
            in
            let lright_paren, rright_paren = if right_paren then ("(", ")") else ("", "") in
            Format.asprintf "%s%s%s" lright_paren s2 rright_paren
        | _ -> Format.asprintf "%s" s2
      in
      Format.asprintf "%s %s %s" left (generate_binop op) right
  | Unop (op, e) ->
      let s = generate_python_expr (Pos.unmark e) in
      Format.asprintf "(%s %s)" (generate_unop op) s
  | Index (var, e) ->
      let s = generate_python_expr (Pos.unmark e) in
      Format.asprintf "%s[%s]" (generate_variable (Pos.unmark var)) s
  | Conditional (e1, e2, e3) ->
      let s1 = generate_python_expr (Pos.unmark e1) in
      let s2 = generate_python_expr (Pos.unmark e2) in
      let s3 = generate_python_expr (Pos.unmark e3) in
      Format.asprintf "(%s if %s else %s)" s2 s1 s3
  | FunctionCall (PresentFunc, [ arg ]) ->
      let sarg = generate_python_expr (Pos.unmark arg) in
      Format.asprintf "(%s != %s)" sarg none_value
  | FunctionCall (NullFunc, [ arg ]) ->
      let sarg = generate_python_expr (Pos.unmark arg) in
      Format.asprintf "(%s == %s)" sarg none_value
  | FunctionCall (ArrFunc, [ arg ]) ->
      let sarg = generate_python_expr (Pos.unmark arg) in
      if autograd () then Format.asprintf "%s" sarg else Format.asprintf "round(%s)" sarg
  | FunctionCall (InfFunc, [ arg ]) ->
      let sarg = generate_python_expr (Pos.unmark arg) in
      if autograd () then Format.asprintf "%s" sarg else Format.asprintf "floor(%s)" sarg
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let s1 = generate_python_expr (Pos.unmark e1) in
      let s2 = generate_python_expr (Pos.unmark e2) in
      Format.asprintf "m_max(%s, %s)" s1 s2
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let s1 = generate_python_expr (Pos.unmark e1) in
      let s2 = generate_python_expr (Pos.unmark e2) in
      Format.asprintf "m_min(%s, %s)" s1 s2
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> Format.asprintf "%f" f
  | Literal Undefined -> none_value
  | Var var -> generate_variable var
  | LocalVar lvar -> Format.asprintf "v%d" lvar.LocalVariable.id
  | GenericTableIndex -> "generic_index"
  | Error -> assert false (* TODO *)
  | LocalLet (lvar, e1, e2) ->
      let s1 = generate_python_expr (Pos.unmark e1) in
      let s2 = generate_python_expr (Pos.unmark e2) in
      Format.asprintf "(lambda v%d: %s)(%s)" lvar.LocalVariable.id s2 s1

let generate_var_def var data (oc : Format.formatter) : unit =
  (* try
   *   let data = VariableMap.find var program.program_vars in
   *   if data.var_io = Regular || data.var_io = Output then begin
   *     Format.fprintf oc "    # %s: %s\n" (generate_name var) (Pos.unmark var.Variable.descr); *)
      match data.var_definition with
      | SimpleVar e ->
          Format.fprintf oc "    # Defined %a\n    %s = %s\n\n" Pos.format_position
            (Pos.get_position e) (generate_variable var)
            (generate_python_expr (Pos.unmark e))
      | TableVar (_, IndexTable es) ->
          IndexMap.iter
            (fun i e ->
              Format.fprintf oc "    # Defined %a\n    %s[%d] = %s\n" Pos.format_position
                (Pos.get_position e) (generate_variable var) i
                (generate_python_expr (Pos.unmark e)))
            es;
          Format.fprintf oc "\n"
      | TableVar (_, IndexGeneric e) ->
          Format.fprintf oc "    # Defined %a\n    %s = lambda generic_index: %s\n\n"
            Pos.format_position (Pos.get_position e) (generate_variable var)
            (generate_python_expr (Pos.unmark e))
      | InputVar -> assert false
      (* should not happen *)
  (*   end
   * with Not_found ->
   *   let cond = VariableMap.find var program.program_conds in
   *   Format.fprintf oc
   *     "    # Verification condition %a\n\
   *     \    cond = %s\n\
   *     \    if cond:\n\
   *     \        raise TypeError(\"Error triggered\\n%s\")\n\n"
   *     Pos.format_position (Pos.get_position cond.cond_expr)
   *     (generate_python_expr (Pos.unmark cond.cond_expr))
   *     (String.concat "\\n"
   *        (List.map
   *           (fun err ->
   *             Format.asprintf "%s: %s" (Pos.unmark err.Error.name) (Pos.unmark err.Error.descr))
   *           cond.cond_errors)) *)

let generate_header (oc: Format.formatter) : unit =
  Format.fprintf oc "# -*- coding: utf-8 -*-\n";
  Format.fprintf oc "# %s\n\n" Prelude.message;
  if autograd () then Format.fprintf oc "import numpy as np\n\n"
  else Format.fprintf oc "from math import floor\n\n";
  Format.fprintf oc "%s\n\n" undefined_class_prelude;
  Format.fprintf oc "local_variables = dict()\n\n\n"

let generate_input_handling (mvg_func:Mir_interface.mvg_function) oc =
  let input_vars = List.map fst (VariableMap.bindings mvg_func.func_variable_inputs) in
  Format.fprintf oc "# The following keys must be present in the input:\n%s\n"
    (String.concat "\n"
       (List.map
          (fun var ->
            Format.asprintf "# %s: %s" (generate_name var) (Pos.unmark var.Variable.descr))
          input_vars));
  Format.fprintf oc "def extracted(input_variables):\n\n";
  Format.fprintf oc "    # First we extract the input variables from the dictionnary:\n%s\n\n"
    (String.concat "\n"
       (List.map
          (fun var ->
            Format.asprintf "    %s = input_variables[\"%s\"]" (generate_variable var)
              (generate_name var))
          input_vars))

let generate_var_cond cond oc =
  Format.fprintf oc
    "    # Verification condition %a\n\
     \    cond = %s\n\
     \    if cond:\n\
     \        raise TypeError(\"Error triggered\\n%s\")\n\n"
    Pos.format_position (Pos.get_position cond.cond_expr)
    (generate_python_expr (Pos.unmark cond.cond_expr))
    (String.concat "\\n"
       (List.map
          (fun err ->
            Format.asprintf "%s: %s" (Pos.unmark err.Error.name) (Pos.unmark err.Error.descr))
          cond.cond_errors))

let rec generate_stmts (program:Bir.program) oc stmts =
  List.iter (fun stmt -> generate_stmt program oc stmt) stmts

and generate_stmt program oc stmt =
  match Pos.unmark stmt with
  | Bir.SAssign(var, vdata) ->
     generate_var_def var vdata oc
  | SConditional(cond, tt, []) ->
     Format.fprintf oc "if %s:@\n@[<h 4>    %a@]@\n"
       (generate_python_expr cond)
       (generate_stmts program) tt
  | SConditional(cond, tt, ff) ->
     Format.fprintf oc "if %s:@\n@[<h 4>    %a@]@\nelse:@\n@[<h 4>    %a@]@\n"
       (generate_python_expr cond)
       (generate_stmts program)  tt
       (generate_stmts program) ff
  | SVerif v ->
     generate_var_cond v oc

let generate_return (mvg_func:Mir_interface.mvg_function) oc =
  let returned_variables = List.map fst (VariableMap.bindings mvg_func.func_outputs) in
  Format.fprintf oc
    "    # The following two lines help us keep all previously defined variable bindings\n\
    \    global local_variables\n\
    \    local_variables = locals()\n\n";
  if List.length returned_variables = 1 then
    Format.fprintf oc "    return %s\n\n" (generate_variable (List.hd returned_variables))
  else
    begin
      Format.fprintf oc "    out = {}\n";
      List.iter
        (fun var ->
          Format.fprintf oc "    out[\"%s\"] = %s\n" (generate_variable var) (generate_variable var))
        returned_variables;
      Format.fprintf oc "    return out\n"
    end

let generate_python_program (program : Bir.program) (filename : string) : unit =
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  (* let exec_order = Mir_dependency_graph.get_execution_order dep_graph in
   * let input_vars =
   *   List.map
   *     (fun (var, _) -> var)
   *     (List.filter
   *        (fun (_, data) -> data.var_io = Input)
   *        (VariableMap.bindings program.program_vars))
   * in *)
  let mvg_func = Mir_interface.read_function_from_spec program.mir_program in
  generate_header oc;
  generate_input_handling mvg_func oc;
  generate_stmts program oc program.statements;
  generate_return mvg_func oc;
  close_out _oc
