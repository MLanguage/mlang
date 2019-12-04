(*
Copyright (C) 2019 Inria, contributors:
  Denis Merigoux <denis.merigoux@inria.fr>
  Raphël Monat <raphael.monat@lip6.fr>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

module Pos = Verifisc.Pos
open Mvg
open Lexing
open Lexer

type mvg_function = {
  func_variable_inputs: unit VariableMap.t;
  func_constant_inputs: expression Pos.marked VariableMap.t;
  func_outputs: unit VariableMap.t;
  func_conds: condition_data VariableMap.t
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
                           (Pos.unmark var.Variable.name)
                           (Pos.format_position (Pos.get_position var.Variable.name))
                       )
                     ))
             }
           else if VariableMap.mem var f.func_constant_inputs then
             {
               var_data with
               var_io = Regular ;
               var_definition = match var_data.var_definition with
                 | SimpleVar _ ->
                   SimpleVar (VariableMap.find var f.func_constant_inputs)
                 | InputVar ->
                   SimpleVar (VariableMap.find var f.func_constant_inputs)
                 | TableVar _ ->
                   raise (Errors.TypeError (
                       Errors.Variable (
                         Printf.sprintf "Defining a constant input for a table variable (%s, %s) is not supported"
                           (Pos.unmark var.Variable.name)
                           (Pos.format_position (Pos.get_position var.Variable.name))
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
                           (Pos.unmark var.Variable.name)
                           (Pos.format_position (Pos.get_position var.Variable.name))
                       )
                     ))
                 | TableVar _ ->
                   raise (Errors.TypeError (
                       Errors.Variable (
                         Printf.sprintf "Defining an output for a table variable (%s, %s) is not supported"
                           (Pos.unmark var.Variable.name)
                           (Pos.format_position (Pos.get_position var.Variable.name))
                       )
                     ))
             }
           else
             { var_data with
               var_io = Regular;
               var_definition = match var_data.var_definition with
                 | InputVar -> SimpleVar (Pos.same_pos_as (Literal Undefined) var.Variable.name)
                 | SimpleVar old -> SimpleVar old
                 | TableVar (size, old) -> TableVar (size, old)
             }
        )
        p.program_vars;
    program_conds = VariableMap.union (fun _ _ _ -> assert false) p.program_conds f.func_conds
  }

let var_set_from_variable_name_list (p: program) (names : string Pos.marked list) : unit VariableMap.t =
  List.fold_left (fun acc alias ->
      let name = try find_var_name_by_alias p (Pos.unmark alias) with
        | Errors.TypeError _ -> Pos.unmark alias
      in
      let var =
        try
          Ast_to_mvg.list_max_execution_number (Pos.VarNameToID.find name p.program_idmap)
        with
        | Not_found ->
          raise
            (Errors.TypeError
               (Errors.Variable
                  (Printf.sprintf
                     "unknown variable %s %s"
                     name
                     (Pos.format_position (Pos.get_position alias))
                  )))
      in
      VariableMap.add var () acc
    ) VariableMap.empty names

let check_const_expression_is_really_const (e: expression Pos.marked) : unit =
  match Pos.unmark e with
  | Literal _ -> ()
  | _ ->
    raise
      (Errors.TypeError
         (Errors.Variable
            (Printf.sprintf
               "Constant input defined in function specification file is not a constant expression (%s)"
               (Pos.format_position (Pos.get_position e))
            )))

let const_var_set_from_list
    (p: program)
    (names : (string * Ast.expression Pos.marked) list)
  : Mvg.expression Pos.marked VariableMap.t =
  List.fold_left (fun acc (name, e) ->
      let var =
        try
          List.hd (List.sort (fun v1 v2 ->
              compare v1.Mvg.Variable.execution_number v2.Mvg.Variable.execution_number)
              (Pos.VarNameToID.find name p.program_idmap))
        with
        | Not_found ->
          try
            let name = find_var_name_by_alias p name in
            List.hd (List.sort (fun v1 v2 ->
                compare v1.Mvg.Variable.execution_number v2.Mvg.Variable.execution_number)
                (Pos.VarNameToID.find name p.program_idmap))
          with Errors.TypeError (Errors.Variable _) ->
            raise
              (Errors.TypeError
                 (Errors.Variable
                    (Printf.sprintf
                       "Unknown variable %s (%s)"
                       name
                       (Pos.format_position (Pos.get_position e))
                    )))
      in
      let new_e = Ast_to_mvg.translate_expression ({
          table_definition = false;
          idmap = p.program_idmap;
          lc = None;
          int_const_values = VariableMap.empty;
          exec_number = Ast_to_mvg.dummy_exec_number Pos.no_pos;
          current_lvalue = name;
        }) e
      in
      check_const_expression_is_really_const new_e;
      VariableMap.add var new_e acc
    ) VariableMap.empty names

let translate_cond idmap (conds:Ast.expression Pos.marked list) : condition_data VariableMap.t =
  let check_boolean (mexpr: Ast.expression Pos.marked) =
    match Pos.unmark mexpr with
    | Binop (((And | Or), _), _, _) -> true
    | Comparison (_, _, _) -> true
    | Unop (Not, _) -> true
    | TestInSet _ -> true
    (* TODO: check Literal Variable ? *)
    | _ -> false
  in
  let mk_neg (mexpr: Ast.expression Pos.marked) =
    Pos.same_pos_as (Ast.Unop (Ast.Not, mexpr)) mexpr in
  let test_error = Mvg.Error.new_error ("-1", Pos.no_pos) ("Condition error in tests", Pos.no_pos) Ast.Anomaly in
  let verif_conds =
    List.fold_left (fun acc cond ->
        if not (check_boolean cond) then
          raise (Errors.TypeError (
              Typing (
                Printf.sprintf "in spec: cond %s should have type bool"
                  (Format_ast.format_expression (Pos.unmark cond))
              )))
        else
          (Pos.same_pos_as {Ast.verif_cond_expr = mk_neg cond;
                            verif_cond_errors = [("-1", Pos.no_pos)]} cond) :: acc) [] conds in
  let program = Ast.Verification {verif_name = [("000", Pos.no_pos)];
                                  verif_applications = [];
                                  verif_conditions = verif_conds} in
  Ast_to_mvg.get_conds [test_error] idmap [[(program, Pos.no_pos)]] None

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
      func_conds = translate_cond p.program_idmap func_spec.Ast.spec_conditions;
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
        (match var.Variable.alias with Some s -> s | None -> Pos.unmark var.Variable.name)
        (Pos.unmark var.Variable.descr);
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
