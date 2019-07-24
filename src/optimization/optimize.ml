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

(**
   Program optimization mostly means removing dead code. The tax source code takes more than 1500
   variables as input, most of them are undefined all the time.
*)

(** Unused variables are determined by areachability analysis from the outputs *)
let remove_unused_variables (program:Mvg.program) : Mvg.program =
  let g = Dependency.create_dependency_graph program in
  let is_output = fun var ->
    try
      (Mvg.VariableMap.find var program.program_vars).Mvg.var_io = Mvg.Output
    with
    | Not_found ->
      let _ = Mvg.VariableMap.find var program.program_conds in
      true
  in
  let is_necessary_to_output = Dependency.OutputToInputReachability.analyze is_output g in
  let program = {
    program with
    Mvg.program_vars =
      Mvg.VariableMap.filter
        (fun var _ -> is_necessary_to_output var)
        program.program_vars;
    Mvg.program_conds = Mvg.VariableMap.filter
        (fun var _ ->
           List.for_all (fun used -> is_necessary_to_output used) (Dependency.DepGraph.pred g var)
        ) program.program_conds;
  } in
  program



(**
   Right now, the interpretation model for variables defined circularly is not properly defined.
   We have to repeat partial evaluation until all the undefined loops have been reduced.
*)
let optimize
    (program: Mvg.program)
  : Mvg.program =

  Cli.debug_print (Printf.sprintf "Optimizing program with %d variables..." (Mvg.VariableMap.cardinal program.program_vars));
  (* TODO: fix when cycles interpretation is correct *)
  let program = ref program in
  let nb_removed = ref max_int in
  Cli.debug_print (Printf.sprintf "Partially evaluating expressions...");
  while !nb_removed > 0 do
    let new_program = Partial_evaluation.partially_evaluate !program  in
    let new_program = remove_unused_variables new_program in
    let new_nb_removed =
      Mvg.VariableMap.cardinal !program.program_vars -
      Mvg.VariableMap.cardinal new_program.program_vars
    in
    Cli.debug_marker false;
    (Printf.printf
       "Removing %d unused variables out of %d..."
       new_nb_removed
       (Mvg.VariableMap.cardinal !program.program_vars));
    if new_nb_removed > 0 then begin
      ANSITerminal.erase ANSITerminal.Below;
      ANSITerminal.move_bol ();
    end;
    program := new_program;
    nb_removed := new_nb_removed;
  done;
  Printf.printf "\n";
  let program = !program in
  Cli.debug_print
    (Printf.sprintf "Program variables count down to %d!"
       (Mvg.VariableMap.cardinal program.program_vars));
  let dep_graph = Dependency.create_dependency_graph program in
  Dependency.print_dependency_graph !Cli.dep_graph_file dep_graph program;
  program
