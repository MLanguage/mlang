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

type mvg_function = {
  func_variable_inputs: unit VariableMap.t;
  func_constant_inputs: expression VariableMap.t;
  func_outputs: unit VariableMap.t;
}

let fit_function (p: program) (f: mvg_function) : program =
  { p with
    program_vars =
      VariableMap.mapi
        (fun var var_data ->
           if VariableMap.mem var f.func_variable_inputs then
             { var_data with
               var_io = Input;
               var_definition = match var_data.var_definition with
                 | InputVar | SimpleVar _ -> InputVar
                 | TableVar _ ->
                   raise (Errors.TypeError (
                       Errors.Variable (
                         Printf.sprintf "Defining a variable input for a table variable (%s, %s) is not supported"
                           (Ast.unmark var.Variable.name)
                           (Format_ast.format_position (Ast.get_position var.Variable.name))
                       )
                     ))
             }
           else if VariableMap.mem var f.func_constant_inputs then
             {
               var_data with
               var_io = Regular ;
               var_definition = match var_data.var_definition with
                 | SimpleVar old_e -> SimpleVar (Ast.same_pos_as (VariableMap.find var f.func_constant_inputs) old_e)
                 | InputVar -> SimpleVar (Ast.same_pos_as (VariableMap.find var f.func_constant_inputs) var.Variable.name)
                 | TableVar _ ->
                   raise (Errors.TypeError (
                       Errors.Variable (
                         Printf.sprintf "Defining a constant input for a table variable (%s, %s) is not supported"
                           (Ast.unmark var.Variable.name)
                           (Format_ast.format_position (Ast.get_position var.Variable.name))
                       )
                     ))
             }
           else if VariableMap.mem var f.func_outputs then
             {
               var_data with
               var_io = Output ;
               var_definition = match var_data.var_definition with
                 | SimpleVar old_e -> SimpleVar old_e
                 | InputVar ->
                   raise (Errors.TypeError (
                       Errors.Variable (
                         Printf.sprintf "Defining an output for a input variable (%s, %s) who is not defined is not supported"
                           (Ast.unmark var.Variable.name)
                           (Format_ast.format_position (Ast.get_position var.Variable.name))
                       )
                     ))
                 | TableVar _ ->
                   raise (Errors.TypeError (
                       Errors.Variable (
                         Printf.sprintf "Defining an output for a table variable (%s, %s) is not supported"
                           (Ast.unmark var.Variable.name)
                           (Format_ast.format_position (Ast.get_position var.Variable.name))
                       )
                     ))
             }
           else
             { var_data with
               var_io = Regular;
               var_definition = match var_data.var_definition with
                 | InputVar -> SimpleVar (Ast.same_pos_as (Literal Undefined) var.Variable.name)
                 | SimpleVar old -> SimpleVar old
                 | TableVar (size, old) -> TableVar (size, old)
             }
        )
        p.program_vars
  }

let sample_test_case (p: program) : mvg_function =
  let v_0ac = find_var_by_alias p "0AC" in
  let v_0cf = find_var_by_alias p "0CF" in
  let v_1aj = find_var_by_alias p "1AJ" in
  let v_irnet = Mvg.VarNameToID.find "IRNET" p.Mvg.program_idmap in
  {
    func_constant_inputs =
      (VariableMap.singleton v_0ac (Literal (Bool true)));
    func_variable_inputs = VariableMap.add v_0cf () (VariableMap.singleton v_1aj ());
    func_outputs = VariableMap.singleton v_irnet ();
  }

let make_function_from_program
    (program: program)
  : expression VariableMap.t -> Interpreter.ctx =
  fun input_values ->
  Interpreter.evaluate_program program input_values

let print_output (p: program) (results: Interpreter.ctx) : unit =
  VariableMap.iter (fun var value ->
      if (VariableMap.find var p.program_vars).Mvg.var_io = Mvg.Output &&
         begin match VariableMap.find var results.ctx_vars with
           | Interpreter.SimpleVar (Bool false)
           | Interpreter.SimpleVar (Int 0)
           | Interpreter.SimpleVar (Float 0.)
           | Interpreter.SimpleVar Undefined -> false
           | _ -> true
         end then
        Cli.result_print
          (Interpreter.format_var_literal_with_var var value)
    )
    results.ctx_vars;
  Interpreter.repl_debugguer results p
