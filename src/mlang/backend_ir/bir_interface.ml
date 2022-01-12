(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr> Raphaël Monat <raphael.monat@lip6.fr>

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

type bir_function = {
  func_variable_inputs : unit Mir.VariableMap.t;
  func_constant_inputs : Mir.expression Pos.marked Mir.VariableMap.t;
  func_outputs : unit Mir.VariableMap.t;
  func_conds : Mir.condition_data Mir.VariableMap.t;
}

let get_variables_indexes (p : Bir.program) (function_spec : bir_function) :
    int Mir.VariableMap.t * int =
  let open Mir in
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
  let assigned_variables =
    List.map snd (Mir.VariableDict.bindings (Bir.get_assigned_variables p))
  in
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  let all_relevant_variables =
    List.fold_left
      (fun acc var -> Mir.VariableMap.add var () acc)
      Mir.VariableMap.empty
      (input_vars @ assigned_variables @ output_vars)
  in
  let counter = ref 0 in
  let var_indexes =
    VariableMap.mapi
      (fun var _ ->
        let id = !counter in
        let size =
          match var.Mir.Variable.is_table with None -> 1 | Some size -> size
        in
        counter := !counter + size;
        id)
      all_relevant_variables
  in
  (var_indexes, !counter)

let var_set_from_variable_name_list (p : Bir.program)
    (names : string Pos.marked list) : unit Mir.VariableMap.t =
  List.fold_left
    (fun acc name ->
      let var = Mir.find_var_by_name p.mir_program name in
      Mir.VariableMap.add var () acc)
    Mir.VariableMap.empty names

let check_const_expression_is_really_const (e : Mir.expression Pos.marked) :
    unit =
  match Pos.unmark e with
  | Literal _ -> ()
  | _ ->
      Errors.raise_spanned_error
        "Constant input defined in function specification file is not a \
         constant expression"
        (Pos.get_position e)

let const_var_set_from_list (p : Bir.program)
    (names : (string Pos.marked * Mast.expression Pos.marked) list) :
    Mir.expression Pos.marked Mir.VariableMap.t =
  List.fold_left
    (fun acc ((name, e) : string Pos.marked * Mast.expression Pos.marked) ->
      let var =
        try
          List.hd
            (List.sort
               (fun v1 v2 ->
                 compare v1.Mir.Variable.execution_number
                   v2.Mir.Variable.execution_number)
               (Pos.VarNameToID.find (Pos.unmark name) p.idmap))
        with Not_found -> (
          try
            let name = Mir.find_var_name_by_alias p.mir_program name in
            List.hd
              (List.sort
                 (fun v1 v2 ->
                   compare v1.Mir.Variable.execution_number
                     v2.Mir.Variable.execution_number)
                 (Pos.VarNameToID.find name p.idmap))
          with Errors.StructuredError _ ->
            Errors.raise_spanned_error
              (Format.asprintf "unknown variable %s" (Pos.unmark name))
              (Pos.get_position e))
      in
      let new_e =
        Mast_to_mir.translate_expression
          {
            table_definition = false;
            idmap = p.idmap;
            lc = None;
            const_map = Mast_to_mir.ConstMap.empty;
            exec_number = Mast_to_mir.dummy_exec_number Pos.no_pos;
          }
          e
      in
      check_const_expression_is_really_const new_e;
      Mir.VariableMap.add var new_e acc)
    Mir.VariableMap.empty names

let translate_external_conditions idmap
    (conds : Mast.expression Pos.marked list) :
    Mir.condition_data Mir.VariableMap.t =
  let check_boolean (mexpr : Mast.expression Pos.marked) =
    match Pos.unmark mexpr with
    | Binop (((And | Or), _), _, _) -> true
    | Comparison (_, _, _) -> true
    | Unop (Not, _) -> true
    | TestInSet _ -> true
    (* TODO: check Literal Variable ? *)
    | _ -> false
  in
  let mk_neg (mexpr : Mast.expression Pos.marked) =
    Pos.same_pos_as (Mast.Unop (Mast.Not, mexpr)) mexpr
  in
  let dummy_entry = ("", Pos.no_pos) in
  let test_error =
    Mir.Error.new_error ("-1", Pos.no_pos)
      {
        error_name = ("", Pos.no_pos);
        error_typ = (Mast.Anomaly, Pos.no_pos);
        error_descr =
          [
            ("Condition error in tests", Pos.no_pos);
            dummy_entry;
            dummy_entry;
            dummy_entry;
          ];
      }
      Mast.Anomaly
  in
  let verif_conds =
    List.fold_left
      (fun acc cond ->
        if not (check_boolean cond) then
          Errors.raise_spanned_error "condition should have type bool"
            (Pos.get_position cond)
        else
          Pos.same_pos_as
            {
              Mast.verif_cond_expr = mk_neg cond;
              verif_cond_error = (("-1", Pos.no_pos), None);
            }
            cond
          :: acc)
      [] conds
  in
  let program =
    Mast.Verification
      {
        verif_name = [ ("000", Pos.no_pos) ];
        verif_applications = [ ("iliad", Pos.no_pos) ];
        verif_conditions = verif_conds;
      }
  in
  (* Leave a constant map empty is risky, it will fail if we allow tests to
     refer to M constants in their expressions *)
  Mast_to_mir.get_conds [ test_error ] Mast_to_mir.ConstMap.empty idmap
    [ [ (program, Pos.no_pos) ] ]

let generate_function_all_vars (p : Bir.program) : bir_function =
  let open Mir in
  let output_vars =
    VariableDict.fold
      (fun k acc -> VariableMap.add k () acc)
      (find_vars_by_io p.mir_program Output)
      VariableMap.empty
  in
  let input_vars =
    let program_input_vars = find_vars_by_io p.mir_program Input in
    let max_exec_vars =
      Pos.VarNameToID.fold
        (fun _ v acc ->
          let max_exec_var = Mast_to_mir.list_max_execution_number v in
          Mir.VariableDict.add max_exec_var acc)
        p.idmap VariableDict.empty
    in
    VariableDict.fold
      (fun k acc -> VariableMap.add k () acc)
      (VariableDict.inter program_input_vars max_exec_vars)
      VariableMap.empty
  in
  Cli.debug_print "Using all %d outputs and %d inputs from m sources"
    (VariableMap.cardinal output_vars)
    (VariableMap.cardinal input_vars);
  {
    func_variable_inputs = input_vars;
    func_constant_inputs = VariableMap.empty;
    func_outputs = output_vars;
    func_conds = VariableMap.empty;
  }

let read_function_from_spec (p : Bir.program) (spec_file : string) :
    bir_function =
  let input = open_in spec_file in
  let filebuf = Lexing.from_channel input in
  Cli.debug_print "Parsing %s" spec_file;
  let filebuf =
    {
      filebuf with
      lex_curr_p = { filebuf.lex_curr_p with pos_fname = spec_file };
    }
  in
  try
    let func_spec = Mparser.function_spec Mlexer.token filebuf in
    close_in input;
    Cli.debug_print "M_spec has %d inputs and %d outputs"
      (List.length func_spec.spec_inputs)
      (List.length func_spec.spec_outputs);
    {
      func_variable_inputs =
        var_set_from_variable_name_list p func_spec.Mast.spec_inputs;
      func_constant_inputs =
        const_var_set_from_list p func_spec.Mast.spec_consts;
      func_outputs =
        var_set_from_variable_name_list p func_spec.Mast.spec_outputs;
      func_conds =
        translate_external_conditions p.idmap func_spec.Mast.spec_conditions;
    }
  with
  | Errors.StructuredError e ->
      close_in input;
      raise (Errors.StructuredError e)
  | Mparser.Error ->
      close_in input;
      Errors.raise_spanned_error "Error while parsing the m_spec file"
        (Parse_utils.mk_position (filebuf.lex_start_p, filebuf.lex_curr_p))

let read_inputs_from_stdin (f : bir_function) : Mir.literal Mir.VariableMap.t =
  if Mir.VariableMap.cardinal f.func_variable_inputs > 0 then
    Cli.result_print "Enter the input values of the program:";
  Mir.VariableMap.mapi
    (fun var _ ->
      Format.printf "%s (%s) = @?"
        (match var.Mir.Variable.alias with
        | Some s -> s
        | None -> Pos.unmark var.Mir.Variable.name)
        (Pos.unmark var.Mir.Variable.descr);
      let value = read_line () in
      try
        let value_ast =
          Mparser.literal_input Mlexer.token (Lexing.from_string value)
        in
        match value_ast with
        | Mast.Float f -> Mir.Float f
        | Mast.Variable _ ->
            Errors.raise_error "input must be a numeric constant"
      with Mparser.Error -> Errors.raise_error "Lexer error in input!")
    f.func_variable_inputs

let context_function = "contextualize"

let context_agnostic_mpp_functions (p : Bir.program) :
    Bir.mpp_function Bir.FunctionMap.t =
  Bir.FunctionMap.remove context_function p.Bir.mpp_functions

(** Add varibles, constants, conditions and outputs from [f] to [p] *)
let adapt_program_to_function (p : Bir.program) (f : bir_function) :
    Bir.program * int =
  let const_input_stmts =
    Mir.VariableMap.fold
      (fun var e acc ->
        Pos.same_pos_as
          (Bir.SAssign
             ( Bir.var_from_mir var,
               {
                 Mir.var_typ = None;
                 Mir.var_io = Regular;
                 Mir.var_definition = Mir.SimpleVar e;
               } ))
          e
        :: acc)
      f.func_constant_inputs []
  in
  let unused_input_stmts =
    Mir.fold_vars
      (fun var def acc ->
        match def.Mir.var_definition with
        | Mir.InputVar ->
            if Mir.VariableMap.mem var f.func_variable_inputs then acc
            else
              let pos = Pos.no_pos in
              ( Bir.SAssign
                  ( Bir.var_from_mir var,
                    {
                      Mir.var_typ = None;
                      Mir.var_io = Regular;
                      Mir.var_definition =
                        begin
                          match var.Mir.Variable.is_table with
                          | None ->
                              Mir.SimpleVar (Mir.Literal Mir.Undefined, pos)
                          | Some size ->
                              Mir.TableVar
                                ( size,
                                  Mir.IndexGeneric
                                    (Pos.same_pos_as (Mir.Literal Mir.Undefined)
                                       var.Mir.Variable.name) )
                        end;
                    } ),
                pos )
              :: acc
        | _ -> acc)
      p.mir_program []
  in
  let conds_stmts =
    Mir.VariableMap.fold
      (fun _ cond acc ->
        Pos.same_pos_as (Bir.SVerif cond) cond.cond_expr :: acc)
      f.func_conds []
  in
  let mpp_functions =
    Bir.FunctionMap.add context_function
      (unused_input_stmts @ const_input_stmts
      @ Bir.[ (SFunctionCall (p.main_function, []), Pos.no_pos) ]
      @ conds_stmts)
      p.mpp_functions
  in
  ( {
      p with
      mpp_functions;
      main_function = context_function;
      outputs = f.func_outputs;
    },
    List.length unused_input_stmts + List.length const_input_stmts )
