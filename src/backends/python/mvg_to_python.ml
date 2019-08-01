(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by the DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-C license and that you accept its terms.
*)

open Mvg

let undefined_class_prelude : string = "\
class Undefined:
    def __init__(self):
        pass

    def __add__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return rhs

    def __radd__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return rhs

    def __sub__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return rhs

    def __rsub__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return rhs

    def __mul__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return rhs

    def __rmul__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return rhs

    def __truediv__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return 0.0

    def __rtruediv__(self, lhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()

    def __lt__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()

    def __lte__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()

    def __gt__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()

    def __gte__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()

    def __eq__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()

    def __neq__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()
"


let none_value = "Undefined()"


let generate_comp_op (op: Ast.comp_op) : string = match op with
  | Ast.Gt -> ">"
  | Ast.Gte -> ">="
  | Ast.Lt -> "<"
  | Ast.Lte -> "<="
  | Ast.Eq -> "=="
  | Ast.Neq -> "!="

let generate_binop (op: Ast.binop) : string = match op with
  | Ast.And -> "and"
  | Ast.Or -> "or"
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"

let generate_variable (var:Variable.t) : string =
  let v = match var.alias with Some v -> v | None -> Ast.unmark var.Variable.name in
  let v = String.lowercase_ascii v in
  let v =
    if same_execution_number var.Variable.execution_number
        (Ast_to_mvg.dummy_exec_number (Ast.get_position var.Variable.name))
    then v else
      Printf.sprintf "%s_%d_%d" v
        (var.Variable.execution_number.Mvg.rule_number)
        (var.Variable.execution_number.Mvg.seq_number)
  in
  if Re.Str.string_match (Re.Str.regexp "[0-9].+") v 0 then
    "var_" ^ v
  else
    v

let generate_name (v:Variable.t) : string =
  match v.alias with Some v -> v | None -> Ast.unmark v.Variable.name

let generate_typ (typ: typ) : string = match typ with
  | Integer -> "int"
  | Real -> "float"
  | Boolean -> "bool"

let autograd () : bool =
  !Cli.backend = "autograd"

let rec generate_python_expr (e: expression) (scc: unit VariableMap.t) : string = match e with
  | Comparison (op, e1, e2) ->
    let s1 = generate_python_expr (Ast.unmark e1) scc in
    let s2 = generate_python_expr (Ast.unmark e2) scc in
    Printf.sprintf "(%s %s %s)" s1 (generate_comp_op (Ast.unmark op)) s2
  | Binop ((Ast.Div, _), e1, e2) ->
    let s1 = generate_python_expr (Ast.unmark e1) scc in
    let s2 = generate_python_expr (Ast.unmark e2) scc in
    Printf.sprintf "((%s / %s) if %s != 0.0 else %s)" s1 s2 s2 none_value
  | Binop (op, e1, e2) ->
    let s1 = generate_python_expr (Ast.unmark e1) scc in
    let s2 = generate_python_expr (Ast.unmark e2) scc in
    Printf.sprintf "(%s %s %s)" s1 (generate_binop (Ast.unmark op)) s2
  | Unop (op, e) ->
    let s = generate_python_expr (Ast.unmark e) scc in
    Printf.sprintf "(%s %s)" (Format_ast.format_unop op) s
  | Index (var, e) ->
    let s = generate_python_expr (Ast.unmark e) scc in
    Printf.sprintf "%s[%s]" (generate_variable (Ast.unmark var)) s
  | Conditional (e1, e2, e3) ->
    let s1 = generate_python_expr (Ast.unmark e1) scc in
    let s2 = generate_python_expr (Ast.unmark e2) scc in
    let s3 = generate_python_expr (Ast.unmark e3) scc in
    Printf.sprintf "(%s if %s else %s)" s2 s1 s3
  | FunctionCall (PresentFunc, [arg]) ->
    let sarg = generate_python_expr (Ast.unmark arg) scc in
    Printf.sprintf "(%s != %s)" sarg none_value
  | FunctionCall (NullFunc, [arg]) ->
    let sarg = generate_python_expr (Ast.unmark arg) scc in
    Printf.sprintf "(%s == %s)" sarg none_value
  | FunctionCall (ArrFunc, [arg]) ->
    let sarg = generate_python_expr (Ast.unmark arg) scc in
    if autograd () then
      Printf.sprintf "%s" sarg
    else
      Printf.sprintf "round(%s)" sarg
  | FunctionCall (InfFunc, [arg]) ->
    let sarg = generate_python_expr (Ast.unmark arg) scc in
    if autograd () then
      Printf.sprintf "%s" sarg
    else
      Printf.sprintf "floor(%s)" sarg
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Bool true) ->
    if autograd () then "1.0" else "True"
  | Literal (Bool false) ->
    if autograd () then "0.0" else "False"
  | Literal (Int i) ->
    if autograd () then
      Printf.sprintf "%.1f" (float_of_int i)
    else
      Printf.sprintf "%d" i
  | Literal (Float f) ->
    Printf.sprintf "%f" f
  | Literal Undefined ->
    none_value
  | Var var ->
    if VariableMap.mem var scc then
      Printf.sprintf "scc[\"%s\"]" (generate_variable var)
    else
      generate_variable var
  | LocalVar lvar -> Printf.sprintf "v%d" lvar.LocalVariable.id
  | GenericTableIndex -> "generic_index"
  | Error -> assert false (* TODO *)
  | LocalLet (lvar, e1, e2) ->
    let s1 = generate_python_expr (Ast.unmark e1) scc in
    let s2 = generate_python_expr (Ast.unmark e2) scc in
    Printf.sprintf "(lambda v%d: %s)(%s)"  lvar.LocalVariable.id s2 s1

let generate_var_def (program : program) (var: Variable.t) (scc: unit VariableMap.t) (oc: out_channel) : unit =
  let in_scc = VariableMap.cardinal scc > 1 in
  let extra_tab = if in_scc then "    " else "" in
  try
    let data = VariableMap.find var program.program_vars in
    if data.var_io = Regular || data.var_io = Output then begin
      Printf.fprintf oc "    %s# %s: %s\n"
        extra_tab
        (generate_name var)
        (Ast.unmark var.Variable.descr);
      match data.var_definition with
      | SimpleVar e ->
        Printf.fprintf oc "    %s# Defined %s\n    %s%s = %s\n\n"
          extra_tab
          (Format_ast.format_position (Ast.get_position e))
          extra_tab
          (if in_scc then Printf.sprintf "scc[\"%s\"]" (generate_variable var) else generate_variable var)
          (generate_python_expr (Ast.unmark e) scc)
      | TableVar (_, IndexTable es) -> begin
          IndexMap.iter (fun i e ->
              Printf.fprintf oc "    %s# Defined %s\n    %s%s[%d] = %s\n"
                extra_tab
                (Format_ast.format_position (Ast.get_position e))
                extra_tab
                (if in_scc then Printf.sprintf "scc[\"%s\"]" (generate_variable var) else generate_variable var)
                i
                (generate_python_expr (Ast.unmark e) scc)
            ) es;
          Printf.fprintf oc "\n"
        end
      | TableVar (_, IndexGeneric e) ->
        Printf.fprintf oc "    %s# Defined %s\n    %s%s = lambda generic_index: %s\n\n"
          extra_tab
          (Format_ast.format_position (Ast.get_position e))
          extra_tab
          (if in_scc then Printf.sprintf "scc[\"%s\"]" (generate_variable var) else generate_variable var)
          (generate_python_expr (Ast.unmark e) scc)
      | InputVar -> assert false (* should not happen *)
    end
  with
  | Not_found ->
    let cond = VariableMap.find var program.program_conds in
    Printf.fprintf oc
      "    %s# Verification condition %s\n    %scond = %s\n    %sif cond:\n    %s    raise TypeError(\"Error triggered\\n%s\")\n\n"
      extra_tab
      (Format_ast.format_position (Ast.get_position cond.cond_expr))
      extra_tab
      (generate_python_expr (Ast.unmark cond.cond_expr) scc)
      extra_tab
      extra_tab
      (String.concat "\\n"
         (List.map
            (fun err ->
               Printf.sprintf "%s: %s"
                 (Ast.unmark err.Error.name)
                 (Ast.unmark err.Error.descr)
            )
            cond.cond_errors
         )
      )

let generate_python_program (program: program) (filename : string) (number_of_passes: int) : unit =
  let oc = open_out filename in
  let exec_order = Execution_order.get_execution_order program in
  let input_vars =
    List.map
      (fun (var, _) -> var)
      (List.filter
         (fun (_, data) -> data.var_io = Input)
         (VariableMap.bindings program.program_vars)
      )
  in
  Printf.fprintf oc "# -*- coding: utf-8 -*-\n\n";
  if autograd () then
    Printf.fprintf oc "import numpy as np\n\n"
  else
    Printf.fprintf oc "from math import floor\n\n";
  Printf.fprintf oc "%s\n\n" undefined_class_prelude;
  Printf.fprintf oc "%s\n"
    (String.concat
       "\n"
       (List.map
          (fun var ->
             Printf.sprintf "# %s: %s"
               (generate_name var)
               (Ast.unmark var.Variable.descr)
          )
          input_vars
       )
    );
  Printf.fprintf oc "def main(%s):\n\n" (String.concat ", " (List.map (fun var -> generate_variable var) input_vars));
  List.iter (fun scc ->
      let in_scc = VariableMap.cardinal scc > 1 in
      if in_scc then begin
        Printf.fprintf oc "    scc = {}\n\n";
        VariableMap.iter (fun var _ ->
            Printf.fprintf oc "    scc[\"%s\"] = %s\n"
              (generate_variable var)
              (none_value)
          ) scc;
        Printf.fprintf oc "\n    for _ in range(%d):\n" number_of_passes
      end;
      VariableMap.iter (fun var _ ->
          generate_var_def program var scc oc
        ) scc;
      if in_scc then begin
        VariableMap.iter (fun var _ ->
            Printf.fprintf oc "    %s = scc[\"%s\"]\n"
              (generate_variable var)
              (generate_variable var)
          ) scc;
        Printf.fprintf oc "\n"
      end
    ) exec_order;
  let returned_variables =
    List.map (fun (var, _) -> var ) (
      List.filter
        (fun (_, data) -> data.var_io = Output)
        (VariableMap.bindings program.program_vars)
    )
  in
  begin if List.length returned_variables = 1 then
      Printf.fprintf oc "    return %s\n\n" (generate_variable (List.hd returned_variables))
    else begin
      Printf.fprintf oc "    out = {}\n";
      List.iter (fun var ->
          Printf.fprintf oc "    out[\"%s\"] = %s\n"
            (generate_variable var)
            (generate_variable var)
        ) returned_variables;
      Printf.fprintf oc "    return out\n"
    end
  end;
  close_out oc
