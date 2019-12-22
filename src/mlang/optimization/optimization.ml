(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

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
        (fun var _ ->  is_necessary_to_output var )
        program.program_vars;
    Mvg.program_conds = Mvg.VariableMap.filter
        (fun var _ ->
           List.for_all (fun used -> is_necessary_to_output used ) (Dependency.DepGraph.pred g var)
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

  Cli.debug_print "Optimizing program with %d variables..." (Mvg.VariableMap.cardinal program.program_vars);
  (* TODO: fix when cycles interpretation is correct *)
  let program = ref program in
  let nb_removed = ref max_int in
  let current_progress, finish = Cli.create_progress_bar "Optimizing program" in
  while !nb_removed > 0 do
    current_progress
      (Format.asprintf
         "performing partial evaluation..."
      );
    let new_program = Partial_evaluation.partially_evaluate !program  in
    current_progress
      (Format.asprintf
         "performing global value numbering..."
      );
    let new_program = Global_value_numbering.optimize new_program in
    current_progress
      (Format.asprintf
         "performing partial evaluation..."
      );
    let new_program = Partial_evaluation.partially_evaluate new_program in
    current_progress
      (Format.asprintf
         "removing unused variables..."
      );
    let new_program = remove_unused_variables new_program in
    let new_nb_removed =
      Mvg.VariableMap.cardinal !program.program_vars -
      Mvg.VariableMap.cardinal new_program.program_vars
    in
    current_progress
      (Format.asprintf
         "removing %d unused variables out of %d..."
         new_nb_removed
         (Mvg.VariableMap.cardinal !program.program_vars));
    program := new_program;
    nb_removed := new_nb_removed;
  done;
  finish "completed!";
  let program = !program in
  Cli.debug_print
    "Program variables count down to %d!"
    (Mvg.VariableMap.cardinal program.program_vars);
  let dep_graph = Dependency.create_dependency_graph program in
  Dependency.print_dependency_graph !Cli.dep_graph_file dep_graph program;
  program
