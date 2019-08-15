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

let translate_and_launch_query
    (program: Mvg.program)
    (typing_info: Typechecker.typ_info)
  : unit  =
  Cli.debug_print (Printf.sprintf "Translating the program into a Z3 query...");
  let mvg = [("model", "true"); ("timeout", (string_of_int (1000 * 18000)))] in
  let ctx = (Z3.mk_context mvg) in
  let s = Z3.Solver.mk_solver ctx None in
  (* ignore (Z3.Log.open_ "z3.log"); *)
  let typing_info = Z3_encoding.find_bitvec_repr program typing_info in
  Cli.debug_print (Printf.sprintf "added dummy bitsize info (everything to %d bits repr)" !Z3_encoding.bitvec_size);
  let z3_program = Mvg_to_z3.translate_program program typing_info ctx s in
  let t0 = Sys.time () in
  Cli.debug_print
    (Printf.sprintf
       "The Z3 query will contain %d different variables, and has been exported in query.z3"
       (Mvg.VariableMap.cardinal z3_program.Z3_encoding.repr_data_var +
        Mvg.LocalVariableMap.cardinal z3_program.Z3_encoding.repr_data_local_var)
    );
  let z3_file = open_out "query.z3" in
  Printf.fprintf z3_file "%s" (Z3.Solver.to_string s);
  close_out z3_file;
  match Z3.Solver.check s [] with
  | Z3.Solver.UNSATISFIABLE ->
    Cli.result_print "Z3 found that the constraints are unsatisfiable!";
  | Z3.Solver.UNKNOWN ->
    Cli.result_print (Printf.sprintf "Z3 didn't find an answer...\nReason: %s" (Z3.Solver.get_reason_unknown s))
  | Z3.Solver.SATISFIABLE ->
    let t1 = Sys.time () in
    Cli.result_print "Z3 found an answer!";
    let filename = "results.json" in
    Cli.result_print (Printf.sprintf "The values of all variables are written in %s" filename);
    let file = open_out filename in
    Printf.fprintf file "%s" (Format_z3.format_z3_program z3_program.Z3_encoding.repr_data_var s);
    close_out file;
    Cli.result_print
      (Printf.sprintf
         "The query took %f seconds to execute. Here are some statistics about it:\n%s"
         (t1 -. t0)
         (Z3.Statistics.to_string (Z3.Solver.get_statistics s))
      );
