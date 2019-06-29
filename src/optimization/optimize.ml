(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by thge DGFiP.

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


let optimize (program: Mvg.program) (typing_info: Typechecker.typ_info) : Mvg.program =
  let dep_graph = Dependency.create_dependency_graph program in
  Cli.debug_print (Printf.sprintf "Optimizing program with %d variables..." (Mvg.VariableMap.cardinal program));
  let unused_variables = Dependency.get_unused_variables dep_graph program in
  Cli.debug_print (Printf.sprintf "Removing %d unused variables..." (Mvg.VariableMap.cardinal unused_variables));
  let program = Mvg.VariableMap.filter (fun var _ -> not (Mvg.VariableMap.mem var unused_variables)) program in

  Cli.debug_print (Printf.sprintf "Propagating constants variables...");
  let dep_graph = Dependency.create_dependency_graph program in
  let program = Constant_propagation.propagate_constants dep_graph program in

  let program : Mvg.program ref = ref program in
  let typing_info : Typechecker.typ_info ref = ref typing_info in
  let nb_inlined_vars : int ref = ref max_int in
  while (0 < !nb_inlined_vars) do
    let dep_graph = Dependency.create_dependency_graph !program in
    let single_use_vars = Dependency.single_use_vars dep_graph in
    let to_inline_vars = Mvg.VariableMap.filter
        (fun var _ -> try
            match (Mvg.VariableMap.find var !program).Mvg.var_io with
            | Mvg.Input | Mvg.Output -> false
            | Mvg.Regular -> true
          with
          | Not_found -> false (* TODO: figure out why it's happening *)
        ) single_use_vars in
    nb_inlined_vars := Mvg.VariableMap.cardinal to_inline_vars;
    if !nb_inlined_vars > 0 then begin
      Cli.debug_print (Printf.sprintf "Inlining %d variables..." !nb_inlined_vars);
      let (new_program, new_typing_info) =
        Inlining.inline_vars to_inline_vars !typing_info !program
      in
      program := new_program;
      typing_info := new_typing_info;
    end
  done;
  let program = !program in
  let typing_info = !typing_info in

  Cli.debug_print (Printf.sprintf "Partially evaluating expressions...");
  let program = Constant_propagation.partially_evaluate program in
  Cli.debug_print
    (Printf.sprintf "Program variables count down to %d!"
       (Mvg.VariableMap.cardinal program));


  let dep_graph = Dependency.create_dependency_graph program in
  Dependency.print_dependency_graph (!Cli.dep_graph_file ^ "_after_optimization.dot") dep_graph program;

  let minimal_input_variables =
    List.map
      (fun (v, _) -> Format_mvg.format_variable v ^ " | Type: " ^
                     (Format_mvg.format_typ
                        (fst (Mvg.VariableMap.find v typing_info.Typechecker.typ_info_var))))
      (Mvg.VariableMap.bindings
         (Mvg.VariableMap.filter (fun _ var_data -> var_data.Mvg.var_io = Mvg.Input) program)
      )
  in
  Cli.debug_print @@ Printf.sprintf "Number of input variables needed: %d."
    (List.length minimal_input_variables);
  program
