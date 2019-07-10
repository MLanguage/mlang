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

let none_value = "0.0"

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

let generate_variable (v:Variable.t) : string =
  let v = String.lowercase_ascii (Ast.unmark v.Variable.name) in
  if Re.Str.string_match (Re.Str.regexp "[0-9].+") v 0 then
    "var_" ^ v
  else
    v

let rec generate_python_expr (e: expression) : string = match e with
  | Comparison (op, e1, e2) ->
    let s1 = generate_python_expr (Ast.unmark e1) in
    let s2 = generate_python_expr (Ast.unmark e2) in
    Printf.sprintf "(%s %s %s)" s1 (generate_comp_op (Ast.unmark op)) s2
  | Binop ((Ast.Div, _), e1, e2) ->
    let s1 = generate_python_expr (Ast.unmark e1) in
    let s2 = generate_python_expr (Ast.unmark e2) in
    Printf.sprintf "((%s / %s) if %s != 0 else %s)" s1 s2 s2 none_value
  | Binop (op, e1, e2) ->
    let s1 = generate_python_expr (Ast.unmark e1) in
    let s2 = generate_python_expr (Ast.unmark e2) in
    Printf.sprintf "(%s %s %s)" s1 (generate_binop (Ast.unmark op)) s2
  | Unop (op, e) ->
    let s = generate_python_expr (Ast.unmark e) in
    Printf.sprintf "(%s %s)" (Format_ast.format_unop op) s
  | Index (var, e) ->
    let s = generate_python_expr (Ast.unmark e) in
    Printf.sprintf "%s[%s]" (generate_variable (Ast.unmark var)) s
  | Conditional (e1, e2, e3) ->
    let s1 = generate_python_expr (Ast.unmark e1) in
    let s2 = generate_python_expr (Ast.unmark e2) in
    let s3 = generate_python_expr (Ast.unmark e3) in
    Printf.sprintf "(%s if %s else %s)" s2 s1 s3
  | FunctionCall (PresentFunc, [arg]) ->
    let sarg = generate_python_expr (Ast.unmark arg) in
    Printf.sprintf "(%s != %s)" sarg none_value
  | FunctionCall (NullFunc, [arg]) ->
    let sarg = generate_python_expr (Ast.unmark arg) in
    Printf.sprintf "(%s == %s)" sarg none_value
  | FunctionCall (ArrFunc, [arg]) ->
    let sarg = generate_python_expr (Ast.unmark arg) in
    Printf.sprintf "round(%s)" sarg
  | FunctionCall (InfFunc, [arg]) ->
    let sarg = generate_python_expr (Ast.unmark arg) in
    Printf.sprintf "floor(%s)" sarg
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Bool true) ->
    "True"
  | Literal (Bool false) ->
    "False"
  | Literal (Int i) ->
    Printf.sprintf "%d" i
  | Literal (Float f) ->
    Printf.sprintf "%f" f
  | Literal Undefined ->
    none_value
  | Var var -> generate_variable var
  | LocalVar lvar -> Printf.sprintf "v%d" lvar.LocalVariable.id
  | GenericTableIndex -> "generic_index"
  | Error -> assert false (* TODO *)
  | LocalLet (lvar, e1, e2) ->
    let s1 = generate_python_expr (Ast.unmark e1) in
    let s2 = generate_python_expr (Ast.unmark e2) in
    Printf.sprintf "(lambda v%d: %s)(%s)"  lvar.LocalVariable.id s2 s1

let generate_python_program (program: program) (filename : string) : unit =
  let oc = open_out filename in
  let dep_graph = Dependency.create_dependency_graph program in
  let input_vars =
    List.map
      (fun (var, _) -> var)
      (List.filter
         (fun (_, data) -> data.var_io = Input)
         (VariableMap.bindings program.program_vars)
      )
  in
  Printf.fprintf oc "from math import floor\n\n";
  Printf.fprintf oc "def main(%s):\n" (String.concat "," (List.map (fun var -> generate_variable var) input_vars));
  (** First initialize all variables *)
  VariableMap.iter (fun var data ->
      if data.var_io = Regular then
        match data.var_definition with
        | SimpleVar _ ->
          Printf.fprintf oc "\t%s = %s\n" (generate_variable var) none_value
        | TableVar (size, IndexTable _) ->
          Printf.fprintf oc "\t%s = [%s]*%d\n" (generate_variable var) none_value size
        | TableVar (_, IndexGeneric _) ->
          Printf.fprintf oc "\t%s = lambda generic_index: %s\n" (generate_variable var) none_value
        | InputVar -> assert false (* should not happen *)
      else ()
    ) program.program_vars;
  Printf.fprintf oc "\n";
  (** Then print the actual program *)
  Dependency.TopologicalOrder.iter (fun var ->
      try
        let data = VariableMap.find var program.program_vars in
        if data.var_io = Regular || data.var_io = Output then begin
          match data.var_definition with
          | SimpleVar e ->
            Printf.fprintf oc "\t%s = %s\n\n" (generate_variable var) (generate_python_expr (Ast.unmark e))
          | TableVar (_, IndexTable es) -> begin
              IndexMap.iter (fun i e ->
                  Printf.fprintf oc "\t%s[%d] = %s\n" (generate_variable var) i (generate_python_expr (Ast.unmark e))
                ) es;
              Printf.fprintf oc "\n"
            end
          | TableVar (_, IndexGeneric e) ->
            Printf.fprintf oc "\t%s = lambda generic_index: %s\n\n" (generate_variable var) (generate_python_expr (Ast.unmark e))
          | InputVar -> assert false (* should not happen *)
        end;
        if data.var_io = Output then
          Printf.fprintf oc "\treturn %s\n\n" (generate_variable var)
      with
      | Not_found ->
        let cond = VariableMap.find var program.program_conds in
        Printf.fprintf oc "\tcond = %s\n\tif cond:\n\t\traise TypeError(\"Condition violated !\")\n\n"
          (generate_python_expr (Ast.unmark cond.cond_expr))
    ) dep_graph;

  close_out oc;
