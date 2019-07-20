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
open Lexing
open Lexer

type mvg_function = {
  func_variable_inputs: unit VariableMap.t;
  func_constant_inputs: expression Ast.marked VariableMap.t;
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
                 | SimpleVar _ -> SimpleVar (VariableMap.find var f.func_constant_inputs)
                 | InputVar -> SimpleVar (VariableMap.find var f.func_constant_inputs)
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

let var_set_from_variable_name_list (p: program) (names : string list) : unit VariableMap.t =
  List.fold_left (fun acc alias ->
      let name = try find_var_name_by_alias p alias with
        | Errors.TypeError _ -> alias
      in
      let var =
        Ast_to_mvg.list_max_execution_number (VarNameToID.find name p.program_idmap)
      in
      (*  Printf.printf "Picked variable: %s (%s)\n" (Ast.unmark var.Mvg.Variable.name)
          (Format_mvg.format_execution_number var.Mvg.Variable.execution_number);
          Interpreter.repl_debugguer (Interpreter.empty_ctx) p;*)
      VariableMap.add var () acc
    ) VariableMap.empty names

let check_const_expression_is_really_const (e: expression Ast.marked) : unit =
  match Ast.unmark e with
  | Literal _ -> ()
  | _ ->
    raise
      (Errors.TypeError
         (Errors.Variable
            (Printf.sprintf
               "Constant input defined in function specification file is not a constant expression (%s)"
               (Format_ast.format_position (Ast.get_position e))
            )))

let const_var_set_from_list
    (p: program)
    (names : (string * Ast.expression Ast.marked) list)
  : Mvg.expression Ast.marked VariableMap.t =
  List.fold_left (fun acc (name, e) ->
      let var =
        try List.hd (List.sort (fun v1 v2 ->
            compare v1.Mvg.Variable.execution_number v2.Mvg.Variable.execution_number)
            (VarNameToID.find name p.program_idmap))
        with
        | Not_found ->
          raise
            (Errors.TypeError
               (Errors.Variable
                  (Printf.sprintf
                     "Unknown variable %s (%s)"
                     name
                     (Format_ast.format_position (Ast.get_position e))
                  )))
      in
      let new_e = Ast_to_mvg.translate_expression ({
          table_definition = false;
          idmap = p.program_idmap;
          lc = None;
          int_const_values = VariableMap.empty;
          exec_number = Ast_to_mvg.dummy_exec_number Ast.no_pos
        }) e
      in
      check_const_expression_is_really_const new_e;
      VariableMap.add var new_e acc
    ) VariableMap.empty names

let read_function_from_spec (p: program) : mvg_function =
  if !Cli.function_spec = "" then
    raise (Errors.ArgumentError "Function specification file is not specified using --function_spec");
  let input = open_in !Cli.function_spec in
  let filebuf =  Lexing.from_channel input in
  Cli.debug_print (Printf.sprintf "Parsing %s" !Cli.function_spec);
  let filebuf = {filebuf with
                 lex_curr_p = { filebuf.lex_curr_p with
                                pos_fname = Filename.basename !Cli.function_spec
                              }
                }
  in
  try
    Parse_utils.current_file := !Cli.function_spec;
    let func_spec = Parser.function_spec token filebuf in
    close_in input;
    {
      func_variable_inputs = var_set_from_variable_name_list p func_spec.Ast.spec_inputs;
      func_constant_inputs = const_var_set_from_list p func_spec.Ast.spec_consts;
      func_outputs = var_set_from_variable_name_list p func_spec.Ast.spec_outputs;
    }
  with
  | Errors.LexingError msg | Errors.ParsingError msg ->
    Cli.error_print msg; close_in input; exit 1
  | Parser.Error -> begin
      Cli.error_print
        (Printf.sprintf "Lexer error in file %s at position %s"
           (!Parse_utils.current_file)
           (Errors.print_lexer_position filebuf.lex_curr_p));
      close_in input;
      exit 1
    end

let make_function_from_program
    (program: program)
    (number_of_passes: int)
  : literal VariableMap.t -> Interpreter.ctx =
  fun input_values ->
  Interpreter.evaluate_program program input_values number_of_passes

let read_inputs_from_stdin (f: mvg_function) : literal VariableMap.t =
  Cli.result_print "Enter the input values of the program, followed by a semicolon:";
  VariableMap.mapi (fun var _ ->
      Printf.printf "%s (%s) = "
        (match var.Variable.alias with Some s -> s | None -> Ast.unmark var.Variable.name)
        (Ast.unmark var.Variable.descr);
      let value = read_line () in
      Parse_utils.current_file := "standard input";
      try
        let value_ast = Parser.literal_input token (Lexing.from_string value) in
        match value_ast with
        | Ast.Int i -> Mvg.Int i
        | Ast.Float f -> Mvg.Float f
        | Ast.Variable _ ->
          raise
            (Errors.TypeError
               (Errors.Variable
                  "Function input must be a numeric constant"
               ))
      with
      | Errors.LexingError msg | Errors.ParsingError msg ->
        Cli.error_print msg; exit 1
      | Parser.Error -> begin
          Cli.error_print
            (Printf.sprintf "Lexer error in input!");
          exit 1
        end
    ) f.func_variable_inputs

let print_output (f: mvg_function) (results: Interpreter.ctx) : unit =
  VariableMap.iter (fun var value ->
      if VariableMap.mem var f.func_outputs then
        Cli.result_print
          (Interpreter.format_var_literal_with_var var value)
    )
    results.ctx_vars;
