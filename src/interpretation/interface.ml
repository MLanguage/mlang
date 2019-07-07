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

let define_input_by_alias
    (p: program)
    (alias: string)
    (init: expression)
    (acc : expression VariableMap.t) =
  let v = find_var_by_alias p alias in
  VariableMap.add v init acc

let all_undefined_input (p: program) (_: Typechecker.typ_info): expression VariableMap.t =
  VariableMap.mapi
    (fun _ _ ->
       Literal Undefined
    )
    (VariableMap.filter (fun _ def -> def.var_io = Input) p.program_vars)

let sample_test_case (p: program) (ti: Typechecker.typ_info): expression VariableMap.t =
  let input = all_undefined_input p ti in
  let input = define_input_by_alias p "0AC" (Literal (Bool true)) input in
  let input = define_input_by_alias p "0CF" (Literal (Int 0)) input in
  let input = define_input_by_alias p "1AJ" (Literal (Int 30000)) input in
  input

let print_output (p: program) (idmap: Ast_to_mvg.idmap) (results: Interpreter.ctx) : unit =
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
  Interpreter.repl_debugguer results p idmap
