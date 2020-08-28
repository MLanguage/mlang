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
  (* This special case has been added, because otherwise huge sums would produce too many
     parenthesis, causing the Python parser to crash *)
  | Binop
      ( (Mast.Add, _),
        e1,
        ( Binop
            ( (Mast.Add, _),
              e2,
              ( Binop
                  ( (Mast.Add, _),
                    e3,
                    ( Binop
                        ( (Mast.Add, _),
                          e4,
                          ( Binop
                              ( (Mast.Add, _),
                                e5,
                                ( Binop
                                    ( (Mast.Add, _),
                                      e6,
                                      ( Binop
                                          ( (Mast.Add, _),
                                            e7,
                                            ( Binop
                                                ( (Mast.Add, _),
                                                  e8,
                                                  ( Binop
                                                      ( (Mast.Add, _),
                                                        e9,
                                                        ( Binop
                                                            ( (Mast.Add, _),
                                                              e10,
                                                              ( Binop
                                                                  ( (Mast.Add, _),
                                                                    e11,
                                                                    ( Binop
                                                                        ( (Mast.Add, _),
                                                                          e12,
                                                                          ( Binop
                                                                              ( (Mast.Add, _),
                                                                                e13,
                                                                                ( Binop
                                                                                    ( (Mast.Add, _),
                                                                                      e14,
                                                                                      ( Binop
                                                                                          ( ( Mast
                                                                                              .Add,
                                                                                              _ ),
                                                                                            e15,
                                                                                            ( Binop
                                                                                                ( ( Mast
                                                                                                    .Add,
                                                                                                    _
                                                                                                  ),
                                                                                                  e16,
                                                                                                  ( Binop
                                                                                                    ( 
                                                                                                    ( 
                                                                                                    Mast
                                                                                                    .Add,
                                                                                                    _
                                                                                                    ),
                                                                                                    e17,
                                                                                                    ( 
                                                                                                    Binop
                                                                                                    ( 
                                                                                                    ( 
                                                                                                    Mast
                                                                                                    .Add,
                                                                                                    _
                                                                                                    ),
                                                                                                    e18,
                                                                                                    ( 
                                                                                                    Binop
                                                                                                    ( 
                                                                                                    ( 
                                                                                                    Mast
                                                                                                    .Add,
                                                                                                    _
                                                                                                    ),
                                                                                                    e19,
                                                                                                    e20
                                                                                                    ),
                                                                                                    _
                                                                                                    )
                                                                                                    ),
                                                                                                    _
                                                                                                    )
                                                                                                    ),
                                                                                                    _
                                                                                                  )
                                                                                                ),
                                                                                              _ ) ),
                                                                                        _ ) ),
                                                                                  _ ) ),
                                                                            _ ) ),
                                                                      _ ) ),
                                                                _ ) ),
                                                          _ ) ),
                                                    _ ) ),
                                              _ ) ),
                                        _ ) ),
                                  _ ) ),
                            _ ) ),
                      _ ) ),
                _ ) ),
          _ ) ) ->
      let s1 = generate_python_expr (Pos.unmark e1) in
      let s2 = generate_python_expr (Pos.unmark e2) in
      let s3 = generate_python_expr (Pos.unmark e3) in
      let s4 = generate_python_expr (Pos.unmark e4) in
      let s5 = generate_python_expr (Pos.unmark e5) in
      let s6 = generate_python_expr (Pos.unmark e6) in
      let s7 = generate_python_expr (Pos.unmark e7) in
      let s8 = generate_python_expr (Pos.unmark e8) in
      let s9 = generate_python_expr (Pos.unmark e9) in
      let s10 = generate_python_expr (Pos.unmark e10) in
      let s11 = generate_python_expr (Pos.unmark e11) in
      let s12 = generate_python_expr (Pos.unmark e12) in
      let s13 = generate_python_expr (Pos.unmark e13) in
      let s14 = generate_python_expr (Pos.unmark e14) in
      let s15 = generate_python_expr (Pos.unmark e15) in
      let s16 = generate_python_expr (Pos.unmark e16) in
      let s17 = generate_python_expr (Pos.unmark e17) in
      let s18 = generate_python_expr (Pos.unmark e18) in
      let s19 = generate_python_expr (Pos.unmark e19) in
      let s20 = generate_python_expr (Pos.unmark e20) in
      Format.asprintf
        "(%s + %s + %s + %s + %s + %s + %s + %s + %s + %s + %s + %s + %s + %s + %s + %s + %s + %s \
         + %s + %s)"
        s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20
  | Binop
      ( (Mast.Add, _),
        e1,
        ( Binop
            ( (Mast.Add, _),
              e2,
              ( Binop
                  ( (Mast.Add, _),
                    e3,
                    ( Binop
                        ( (Mast.Add, _),
                          e4,
                          ( Binop
                              ( (Mast.Add, _),
                                e5,
                                ( Binop
                                    ( (Mast.Add, _),
                                      e6,
                                      ( Binop
                                          ( (Mast.Add, _),
                                            e7,
                                            ( Binop
                                                ( (Mast.Add, _),
                                                  e8,
                                                  (Binop ((Mast.Add, _), e9, e10), _) ),
                                              _ ) ),
                                        _ ) ),
                                  _ ) ),
                            _ ) ),
                      _ ) ),
                _ ) ),
          _ ) ) ->
      let s1 = generate_python_expr (Pos.unmark e1) in
      let s2 = generate_python_expr (Pos.unmark e2) in
      let s3 = generate_python_expr (Pos.unmark e3) in
      let s4 = generate_python_expr (Pos.unmark e4) in
      let s5 = generate_python_expr (Pos.unmark e5) in
      let s6 = generate_python_expr (Pos.unmark e6) in
      let s7 = generate_python_expr (Pos.unmark e7) in
      let s8 = generate_python_expr (Pos.unmark e8) in
      let s9 = generate_python_expr (Pos.unmark e9) in
      let s10 = generate_python_expr (Pos.unmark e10) in
      Format.asprintf "(%s + %s + %s + %s + %s + %s + %s + %s + %s + %s)" s1 s2 s3 s4 s5 s6 s7 s8 s9
        s10
  | Binop
      ( (Mast.Add, _),
        e1,
        ( Binop
            ((Mast.Add, _), e2, (Binop ((Mast.Add, _), e3, (Binop ((Mast.Add, _), e4, e5), _)), _)),
          _ ) ) ->
      let s1 = generate_python_expr (Pos.unmark e1) in
      let s2 = generate_python_expr (Pos.unmark e2) in
      let s3 = generate_python_expr (Pos.unmark e3) in
      let s4 = generate_python_expr (Pos.unmark e4) in
      let s5 = generate_python_expr (Pos.unmark e5) in
      Format.asprintf "(%s + %s + %s + %s + %s)" s1 s2 s3 s4 s5
  | Binop (op, e1, e2) ->
      let s1 = generate_python_expr (Pos.unmark e1) in
      let s2 = generate_python_expr (Pos.unmark e2) in
      Format.asprintf "(%s %s %s)" s1 (generate_binop (Pos.unmark op)) s2
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

let generate_var_def (program : program) (var : Variable.t) (oc : Format.formatter) : unit =
  try
    let data = VariableMap.find var program.program_vars in
    if data.var_io = Regular || data.var_io = Output then begin
      Format.fprintf oc "    # %s: %s\n" (generate_name var) (Pos.unmark var.Variable.descr);
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
    end
  with Not_found ->
    let cond = VariableMap.find var program.program_conds in
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

let generate_python_program (program : program) (dep_graph : Mir_dependency_graph.G.t)
    (filename : string) : unit =
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  let exec_order = Mir_dependency_graph.get_execution_order dep_graph in
  let input_vars =
    List.map
      (fun (var, _) -> var)
      (List.filter
         (fun (_, data) -> data.var_io = Input)
         (VariableMap.bindings program.program_vars))
  in
  Format.fprintf oc "# -*- coding: utf-8 -*-\n";
  Format.fprintf oc "# %s\n\n" Prelude.message;
  if autograd () then Format.fprintf oc "import numpy as np\n\n"
  else Format.fprintf oc "from math import floor\n\n";
  Format.fprintf oc "%s\n\n" undefined_class_prelude;
  Format.fprintf oc "local_variables = dict()\n\n\n";
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
          input_vars));
  List.iter (fun var -> generate_var_def program var oc) exec_order;
  let returned_variables =
    List.map
      (fun (var, _) -> var)
      (List.filter
         (fun (_, data) -> data.var_io = Output)
         (VariableMap.bindings program.program_vars))
  in
  Format.fprintf oc
    "    # The following two lines help us keep all previously defined variable bindings\n\
    \    global local_variables\n\
    \    local_variables = locals()\n\n";
  if List.length returned_variables = 1 then
    Format.fprintf oc "    return %s\n\n" (generate_variable (List.hd returned_variables))
  else begin
    Format.fprintf oc "    out = {}\n";
    List.iter
      (fun var ->
        Format.fprintf oc "    out[\"%s\"] = %s\n" (generate_variable var) (generate_variable var))
      returned_variables;
    Format.fprintf oc "    return out\n"
  end;
  close_out _oc
