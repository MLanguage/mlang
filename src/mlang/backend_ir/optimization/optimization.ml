(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** Program optimization mostly means removing dead code. The tax source code takes more than 1500
    variables as input, most of them are undefined all the time. *)

(** Unused variables are determined by areachability analysis from the outputs *)
let remove_unused_variables (program : Mir.program) : Mir.program =
  let g = Dependency.create_dependency_graph program in
  let is_output var =
    try (Mir.VariableMap.find var program.program_vars).Mir.var_io = Mir.Output
    with Not_found ->
      let _ = Mir.VariableMap.find var program.program_conds in
      true
  in
  let is_necessary_to_output = Dependency.OutputToInputReachability.analyze is_output g in
  let program =
    {
      program with
      Mir.program_vars =
        Mir.VariableMap.filter (fun var _ -> is_necessary_to_output var) program.program_vars;
      Mir.program_conds =
        Mir.VariableMap.filter
          (fun var _ ->
            List.for_all (fun used -> is_necessary_to_output used) (Dependency.DepGraph.pred g var))
          program.program_conds;
    }
  in
  program

(** Right now, the interpretation model for variables defined circularly is not properly defined. We
    have to repeat partial evaluation until all the undefined loops have been reduced. *)
let optimize (program : Mir.program) (dep_graph : Dependency.DepGraph.t) : Mir.program =
  Cli.debug_print "Optimizing program with %d variables..."
    (Mir.VariableMap.cardinal program.program_vars);
  (* TODO: fix when cycles interpretation is correct *)
  let program = ref program in
  let nb_removed = ref max_int in
  let dep_graph = ref dep_graph in
  let current_progress, finish = Cli.create_progress_bar "Optimizing program" in
  ( try
      while !nb_removed > 0 do
        let remaining_vars = Mir.VariableMap.cardinal !program.program_vars in
        current_progress
          (Format.asprintf "%d variables, performing partial evaluation..." remaining_vars);
        let remaining_vars = Mir.VariableMap.cardinal !program.program_vars in
        let new_program = Partial_evaluation.partially_evaluate !program !dep_graph in
        current_progress
          (Format.asprintf "%d variables, performing global value numbering..." remaining_vars);
        let remaining_vars = Mir.VariableMap.cardinal new_program.program_vars in
        dep_graph := Dependency.create_dependency_graph new_program;
        current_progress
          (Format.asprintf "%d variables, removing unused variables..." remaining_vars);
        let new_program = remove_unused_variables new_program in
        dep_graph := Dependency.create_dependency_graph new_program;
        let remaining_vars = Mir.VariableMap.cardinal !program.program_vars in
        let new_program = Global_value_numbering.optimize new_program !dep_graph in
        current_progress
          (Format.asprintf "%d variables, performing partial evaluation..." remaining_vars);
        let new_program = Partial_evaluation.partially_evaluate new_program !dep_graph in
        let new_nb_removed =
          Mir.VariableMap.cardinal !program.program_vars
          - Mir.VariableMap.cardinal new_program.program_vars
        in
        current_progress
          (Format.asprintf "removing %d unused variables out of %d..." new_nb_removed
             (Mir.VariableMap.cardinal !program.program_vars));
        program := new_program;
        nb_removed := new_nb_removed
      done
    with Interpreter.RuntimeError (e, ctx) ->
      Cli.error_print "%a" Interpreter.format_runtime_error e;
      flush_all ();
      flush_all ();
      finish "error";
      Interpreter.repl_debugguer ctx !program;
      exit 1 );
  finish "completed!";
  let program = !program in
  Cli.debug_print "Optimziation resulted in number of variables down to %d!"
    (Mir.VariableMap.cardinal program.program_vars);
  let dep_graph = Dependency.create_dependency_graph program in
  Dependency.print_dependency_graph !Cli.dep_graph_file dep_graph program;
  program
