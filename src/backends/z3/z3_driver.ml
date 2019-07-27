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

let translate_and_launch_query
    (program: Mvg.program)
    (dep_graph: Dependency.DepGraph.t)
    (typing_info: Typechecker.typ_info)
  : unit  =
  Cli.debug_print (Printf.sprintf "Translating the program into a Z3 query...");
  let mvg = [("model", "true"); ("timeout", (string_of_int (1000 * 30)))] in
  let ctx = (Z3.mk_context mvg) in
  let s = Z3.Solver.mk_solver ctx None in
  let typing_info = Z3_encoding.find_bitvec_repr program dep_graph typing_info in
  Cli.debug_print "added dummy typing info (everything to 50 bits repr)";
  (* Cli.debug_print @@ Printf.sprintf "repr_info_var: %s\nrepr_info_local_var: %s\n"
   *   (Mvg.VariableMap.show Z3_encoding.show_repr typing_info.Z3_encoding.repr_info_var)
   *   (Mvg.LocalVariableMap.show Z3_encoding.show_repr typing_info.Z3_encoding.repr_info_local_var); *)
  let z3_program = Mvg_to_z3.translate_program program dep_graph typing_info ctx s in
  let t0 = Sys.time () in
  Cli.debug_print
    (Printf.sprintf
       "The Z3 query will contain %d different variables"
       (Mvg.VariableMap.cardinal z3_program.Z3_encoding.repr_data_var +
        Mvg.LocalVariableMap.cardinal z3_program.Z3_encoding.repr_data_local_var)
    );
  Cli.debug_print "VMap:\n";
  Mvg.VariableMap.iter (fun v _ ->   Cli.debug_print @@ Mvg.Variable.show v) z3_program.repr_data_var;
  Cli.debug_print "LVMap:\n";
  Mvg.LocalVariableMap.iter (fun v _ -> Cli.debug_print @@ Mvg.LocalVariable.show v) z3_program.repr_data_local_var;
  match Z3.Solver.check s [] with
  | Z3.Solver.UNSATISFIABLE -> Cli.result_print "Z3 found that the constraints are unsatisfiable!";
  | Z3.Solver.UNKNOWN -> Cli.result_print "Z3 didn't find an answer..."
  | Z3.Solver.SATISFIABLE ->
    let t1 = Sys.time () in
    Cli.result_print "Z3 found an answer!";
    let filename = "results.json" in
    Cli.result_print (Printf.sprintf "The values of all variables are written in %s" filename);
    let file = open_out filename in
    Printf.fprintf file "%s" (Format_z3.format_z3_program z3_program.Z3_encoding.repr_data_var ctx s);
    Cli.result_print
      (Printf.sprintf
         "The query took %f seconds to execute. Here are some statistics about it:\n%s"
         (t1 -. t0)
         (Z3.Statistics.to_string (Z3.Solver.get_statistics s))
      );
