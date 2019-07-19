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

let format_execution_number (exec_number: execution_number) : string =
  Printf.sprintf "rule %d, sequence index %d, %s"
    exec_number.rule_number
    exec_number.seq_number
    (Format_ast.format_position exec_number.pos)

let format_typ (t: typ) : string = match t with
  | Integer -> "integer"
  | Real -> "real"
  | Boolean -> "boolean"

let format_io (io: io) : string = match io with
  | Input -> "input"
  | Output -> "output"
  | Regular -> "regular"

let format_func (f: func) : string = match f with
  | SumFunc -> "somme"
  | AbsFunc -> "abs"
  | MinFunc -> "min"
  | MaxFunc -> "max"
  | GtzFunc -> "positif"
  | GtezFunc -> "positif_ou_nul"
  | NullFunc -> "null"
  | ArrFunc -> "arr"
  | InfFunc -> "inf"
  | PresentFunc -> "present"
  | Multimax -> "multimax"
  | Supzero -> "supzero"

let format_literal (l: literal) : string = match l with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Undefined -> "indéfini"

let rec format_expression (e: expression) : string = match e with
  | Comparison ((op, _), (e1, _), (e2, _)) ->
    "(" ^ (format_expression e1) ^ " " ^ (Format_ast.format_comp_op op) ^ " " ^ (format_expression e2) ^ ")"
  | Binop ((op, _), (e1, _), (e2, _)) ->
    "(" ^ (format_expression e1) ^ " " ^ (Format_ast.format_binop op) ^ " " ^ (format_expression e2) ^ ")"
  | Unop (op, (e, _)) ->
    (Format_ast.format_unop op) ^ " " ^ (format_expression e)
  | Conditional ((e1, _), (e2, _), (e3, _)) ->
    "(si " ^ (format_expression e1) ^ " alors " ^ (format_expression e2) ^
    " sinon " ^ (format_expression e3) ^ ")"
  | FunctionCall(f, args) -> Printf.sprintf "%s(%s)"
                               (format_func f )
                               (String.concat "," (List.map (fun e -> format_expression (Ast.unmark e)) args))
  | Literal lit -> format_literal lit
  | Var var -> Ast.unmark var.Variable.name
  | LocalVar lvar -> "t" ^ (string_of_int lvar.LocalVariable.id)
  | GenericTableIndex -> "X"
  | Error -> "erreur"
  | LocalLet (lvar, (e1, _), (e2, _)) ->
    "soit t" ^ (string_of_int lvar.LocalVariable.id) ^ " = ("^
    (format_expression e1) ^ ") dans " ^ (format_expression e2)
  | Index(var, i) ->
    Printf.sprintf "%s[%s]"
      (Ast.unmark (Ast.unmark var).Variable.name)
      (format_expression (Ast.unmark i))

let format_variable_def (def: variable_def) : string = match def  with
  | SimpleVar e -> format_expression (Ast.unmark e) ^ "\n"
  | InputVar -> "[User input]\n"
  | TableVar (_, IndexGeneric e) -> "X -> " ^ (format_expression (Ast.unmark e)) ^ "\n"
  | TableVar (_, IndexTable defs) -> IndexMap.fold (fun i e acc ->
      acc ^ (Printf.sprintf "%d -> %s\n" i (format_expression (Ast.unmark e)))
    ) defs ""

let format_program_vars (p: variable_data VariableMap.t) : string = VariableMap.fold (fun var def acc ->
    acc ^ (Printf.sprintf "Variable %s%s of type %s, io %s:\n%s"
             (Ast.unmark var.Variable.name)
             (match var.Variable.alias with Some x -> " (alias "^ x ^")" | None -> "")
             (match def.var_typ with | None -> "unknown" | Some t -> format_typ t)
             (format_io def.var_io)
             (format_variable_def def.var_definition))
  ) p ""

let format_error (e: Error.t) : string =
  Printf.sprintf "erreur %s (%s)"
    (Ast.unmark e.Error.name)
    (Ast.unmark e.Error.descr)

let format_precondition (precond: condition_data) : string =
  Printf.sprintf "Précondition : %s\nSinon %s"
    (format_expression (Ast.unmark precond.cond_expr))
    (String.concat "," (List.map (fun err -> format_error err) precond.cond_errors))

let format_program_conds (conds: condition_data VariableMap.t) : string =
  String.concat
    "\n"
    (List.map
       (fun (_, cond) -> format_precondition cond) (VariableMap.bindings conds)
    )
let format_program (p: program) : string =
  Printf.sprintf "%s\n\n%s" (format_program_vars p.program_vars) (format_program_conds p.program_conds)

let format_variable (v: Variable.t) : string =
  Printf.sprintf "%s: %s" (Ast.unmark v.Variable.name) (Ast.unmark v.Variable.descr)

let format_io (io: io) : string = match io with
  | Input -> "input"
  | Output -> "output"
  | Regular -> "regular"
