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


let optimize
    (program: Mvg.program)
    (typing_info: Typechecker.typ_info)
  : Mvg.program =

  Cli.debug_print (Printf.sprintf "Optimizing program with %d variables..." (Mvg.VariableMap.cardinal program.program_vars));
  let program : Mvg.program ref = ref program in
  let typing_info : Typechecker.typ_info ref = ref typing_info in
  let nb_unused_vars : int ref = ref max_int in
  while (0 < !nb_unused_vars) do
    if !nb_unused_vars > 0 then begin
      Cli.debug_print (Printf.sprintf "Partially evaluating expressions...");
      let new_program = Partial_evaluation.partially_evaluate !program in
      let dep_graph = Dependency.create_dependency_graph new_program in

      let unused_variables = Dependency.get_unused_variables dep_graph new_program in
      Cli.debug_print (Printf.sprintf "Removing %d unused variables..." (Mvg.VariableMap.cardinal unused_variables));
      let new_program =
        { new_program with
          program_vars =
            Mvg.VariableMap.filter
              (fun var _ -> not (Mvg.VariableMap.mem var unused_variables)) new_program.program_vars
        }
      in
      nb_unused_vars := (Mvg.VariableMap.cardinal unused_variables);
      program := new_program;
    end
  done;
  let program = !program in
  let typing_info = !typing_info in

  Cli.debug_print
    (Printf.sprintf "Program variables count down to %d!"
       (Mvg.VariableMap.cardinal program.program_vars));

  let minimal_input_variables =
    List.map
      (fun (v, _) -> Format_mvg.format_variable v ^ " | Type: " ^
                     (Format_mvg.format_typ
                        (fst (Mvg.VariableMap.find v typing_info.Typechecker.typ_info_var))))
      (Mvg.VariableMap.bindings
         (Mvg.VariableMap.filter (fun _ var_data -> var_data.Mvg.var_io = Mvg.Input) program.program_vars)
      )
  in
  let input_needed = "input_needed.txt" in
  Cli.debug_print @@ Printf.sprintf "Input variables needed (%d) written to %s"
    (List.length minimal_input_variables)
    input_needed;
  let oc = open_out input_needed in
  Printf.fprintf oc "%s" (String.concat "\n" minimal_input_variables);
  close_out oc;
  program
