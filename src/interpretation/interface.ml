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

let var_set_from_alias_list (p: program) (aliases : string list) =
  List.fold_left (fun acc alias ->
      let var = find_var_by_alias p alias in
      VariableMap.add var () acc
    ) VariableMap.empty aliases

let var_set_from_name_list (p: program) (names : (string * expression) list) =
  List.fold_left (fun acc (name, e) ->
      let var = VarNameToID.find name p.program_idmap in
      VariableMap.add var e acc
    ) VariableMap.empty names

let simulateur_simplifie_ir_2017 (p: program) : mvg_function =
  let input_aliases = [
    (** List of input variables taken from https://www3.impots.gouv.fr/simulateur/calcul_impot/2017/simplifie/index.htm *)
    "0AO" ; "0AM"; "0AB"; "0AC"; "0AD"; "0AV"; "0AX"; "0AY"; "0AZ";
    "0DA"; "0DB";
    "0BT";
    "0AL"; "0AN"; "0AP"; "0AF"; "0AW"; "0AS"; "0AG";
    "0CF"; "0F0"; "0F1"; "0F2"; "0F3"; "0F4"; "0F5";
    "0CG"; "0G0"; "0G1"; "0G2";
    "0CH"; "0H0"; "0H1"; "0H2"; "0H3"; "0H4"; "0H5";
    "0CI"; "0I0"; "0I1"; "0I2";
    "0CR"; "0R0"; "0R1"; "0R2";
    "0DJ"; "0DN";
    "1AJ"; "1BJ"; "1CJ"; "1DJ";
    "1GA"; "1HA"; "1IA"; "1JA";
    "1AP"; "1BP"; "1CP"; "1DP";
    "1AK"; "1BK"; "1CK"; "1DK";
    "1AI"; "1BI"; "1CI"; "1DI";
    "1AF"; "1BF"; "1CF"; "1DF";
    "1AG"; "1BG"; "1CG"; "1DG";
    "1AS"; "1BS"; "1CS"; "1DS";
    "1AT"; "1BT";
    "1AZ"; "1BZ"; "1CZ"; "1DZ";
    "1AO"; "1BO"; "1CO"; "1DO";
    "1AL"; "1BL"; "1CL"; "1DL";
    "1AM"; "1BM"; "1CM"; "1DM";
    "1AW"; "1BW"; "1CW"; "1DW";
    "1AR"; "1BR"; "1CR"; "1DR";
    "2DH"; "2EE";
    "2DC"; "2FU"; "2CH";
    "2TS"; "2GO"; "2TR"; "2TT"; "2TU"; "2TV"; "2FA";
    "2CG"; "2BH"; "2CA"; "2AB"; "2CK"; "2BG"; "2AA"; "2AL"; "2AM"; "2AN"; "2AQ"; "2AR"; "2DM";
    "3VG"; "3SG"; "3VH";
    "4BE"; "4BK"; "4BA"; "4BL"; "4BB"; "4BC"; "4BD"; "4BN";
    "0XX";
    "6DE"; "6GI"; "6GJ"; "6EL"; "6EM"; "6GP"; "6GU"; "6DD";
    "6RS"; "6RT"; "6RU";
    "6PS"; "6PT"; "6PU";
    "6QR"; "6QW";
    "6QS"; "6QT"; "6QU";
    "7UD"; "7UF"; "7UH"; "7XS"; "7XT"; "7XU"; "7XW"; "7XY";
    "7VA"; "7VC";
    "7AC"; "7AE"; "7AG";
    "7DB"; "7DL"; "7DQ"; "7DG";
    "7VZ"; "7VT"; "7VX";
    "7CD"; "7CE";
    "7GA"; "7GB"; "7GC"; "7GE"; "7GF"; "7GG";
    "7EA"; "7EB"; "7EC"; "7ED"; "7EF"; "7EG";
    "7GZ"; "7UK"; "7VO"; "7TD";
    "7WN"; "7WO"; "7WM"; "7WP";
    "7CB"; "7AD"; "7AF";
    "7AG"; "7AK"; "7AL"; "7AM"; "7AN"; "7AQ";
    "7AR"; "7AV"; "7AX"; "7AY"; "7AZ"; "7BB";
    "7BC"; "7BD"; "7BE"; "7BF"; "7BH"; "7BK"; "7BL";
    "7WJ"; "7WL";
    "8UT"; "8TF"; "8TI"; "8TK"
  ] in
  let input_vars = var_set_from_alias_list p input_aliases in
  let constant_names_and_values = [
    ("V_ANREV", Literal (Int 2017));
  ] in
  let constant_inputs = var_set_from_name_list p constant_names_and_values in
  let v_irnet = Mvg.VarNameToID.find "IRNET" p.Mvg.program_idmap in
  {
    func_constant_inputs = constant_inputs;
    func_variable_inputs =
      input_vars;
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
