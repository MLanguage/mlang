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


(* The edges in the graph go from output to inputs *)
module DepGraph = Graph.Persistent.Digraph.ConcreteBidirectional(struct
    type t = Mvg.Variable.t
    let hash v = v.Mvg.Variable.id
    let compare v1 v2 = compare v1.Mvg.Variable.id v2.Mvg.Variable.id
    let equal v1 v2 = v1.Mvg.Variable.id = v2.Mvg.Variable.id
  end)

let rec add_usages (lvar: Mvg.Variable.t) (e: Mvg.expression Ast.marked) (acc: DepGraph.t) : DepGraph.t =
  let acc = DepGraph.add_vertex acc lvar in
  match Ast.unmark e with
  | Mvg.Comparison (_, e1, e2) | Mvg.Binop (_, e1, e2 )
  | Mvg.LocalLet (_, e1, e2) ->
    let acc = add_usages lvar e1 acc in
    let acc = add_usages lvar e2 acc in
    acc
  | Mvg.Unop (_, e) ->
    add_usages lvar e acc
  | Mvg.Index ((var,_ ), e) ->
    let acc = DepGraph.add_edge acc var lvar in
    let acc = add_usages lvar e acc in
    acc
  | Mvg.Conditional (e1, e2, e3) ->
    let acc = add_usages lvar e1 acc in
    let acc = add_usages lvar e2 acc in
    let acc = add_usages lvar e3 acc in
    acc
  | Mvg.FunctionCall (_, args) ->
    List.fold_left (fun acc arg ->
        add_usages lvar arg acc) acc args
  | Mvg.LocalVar _
  | Mvg.Literal _
  | Mvg.GenericTableIndex
  | Mvg.Error -> acc
  | Mvg.Var var ->
    DepGraph.add_edge acc var lvar


let create_dependency_graph (p: Mvg.program) : DepGraph.t =
  Mvg.VariableMap.fold (fun var def acc ->
      match def.Mvg.var_definition with
      | Mvg.InputVar -> DepGraph.add_vertex acc var
      | Mvg.SimpleVar e -> add_usages var e acc
      | Mvg.TableVar (_, def) -> begin match def with
          | Mvg.IndexGeneric e -> add_usages var e acc
          | Mvg.IndexTable es -> Mvg.IndexMap.fold (fun _ e acc ->
              add_usages var e acc
            ) es acc
        end
    ) p DepGraph.empty

let program_when_printing : Mvg.program option ref = ref None

module Dot = Graph.Graphviz.Dot(struct
    include DepGraph (* use the graph module from above *)

    let edge_attributes _ = []
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = begin match !program_when_printing with
      | None -> []
      | Some p ->
        let var_data = Mvg.VariableMap.find v p in
        match var_data.Mvg.var_io with
        | Mvg.Input -> [`Color 1; `Shape `Box]
        | Mvg.Regular -> []
        | Mvg.Output -> [`Color 2; `Shape `Diamond]
    end
    let vertex_name v = Ast.unmark v.Mvg.Variable.name
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let print_dependency_graph (filename: string) (graph: DepGraph.t) (p: Mvg.program): unit =
  let file = open_out_bin filename in
  program_when_printing:= Some p;
  Cli.debug_print (Printf.sprintf
                     "Writing variables dependency graph to %s (%d variables)"
                     filename
                     (DepGraph.nb_vertex graph));
  if !Cli.debug_flag then
    Dot.output_graph file graph;
  close_out file


module CycleDetector = Graph.Components.Make(DepGraph)

let check_for_cycle (g: DepGraph.t) (p: Mvg.program) : unit =
  (* if there is a cycle, there will be an strongly connected component of cardinality > 1 *)
  let sccs = CycleDetector.scc_list g in
  if List.length sccs < DepGraph.nb_vertex g then begin
    let sccs = List.filter (fun scc -> List.length scc > 1) sccs in
    let cycles_strings = ref [] in
    let dir = "variable_cycles" in
    begin try Unix.mkdir dir 0o750 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end;
    List.iteri (fun i scc ->
        let new_g = DepGraph.fold_vertex (fun vertex new_g ->
            if List.mem vertex scc then
              new_g
            else
              DepGraph.remove_vertex new_g vertex
          ) g g in
        let filename = Printf.sprintf "%s/strongly_connected_component_%d.dot" dir i in
        print_dependency_graph filename new_g p;
        cycles_strings := (Printf.sprintf "The following variables are defined circularly: %s\n\
                                           The dependency graph of this circular definition has been written to %s"
                             (String.concat " <-> "
                                (List.map
                                   (fun var -> Ast.unmark var.Mvg.Variable.name)
                                   scc))
                             filename
                          )::!cycles_strings;
      ) sccs;
    let oc = open_out (dir ^ "/variable_cycles.txt") in
    Printf.fprintf oc "%s" (String.concat "\n\n" !cycles_strings);
    close_out oc;
    raise
      (Errors.TypeError
         (Errors.Variable
            (Printf.sprintf "The program contains circularly defined variables, see folder %s"
               dir

            )))
  end


let single_use_vars (g: DepGraph.t) : unit Mvg.VariableMap.t =
  DepGraph.fold_vertex (fun var acc ->
      if DepGraph.in_degree g var <= 1 then
        Mvg.VariableMap.add var () acc
      else
        acc
    ) g Mvg.VariableMap.empty

module OutputToInputReachability = Graph.Fixpoint.Make(DepGraph)
    (struct
      type vertex = DepGraph.E.vertex
      type edge = DepGraph.E.t
      type g = DepGraph.t
      type data = bool
      let direction = Graph.Fixpoint.Backward
      let equal = (=)
      let join = (||)
      let analyze _ = (fun x -> x)
    end)

module InputToOutputReachability = Graph.Fixpoint.Make(DepGraph)
    (struct
      type vertex = DepGraph.E.vertex
      type edge = DepGraph.E.t
      type g = DepGraph.t
      type data = bool
      let direction = Graph.Fixpoint.Forward
      let equal = (=)
      let join = (||)
      let analyze _ = (fun x -> x)
    end)

let get_unused_variables (g: DepGraph.t) (p:Mvg.program) : unit Mvg.VariableMap.t =
  let is_output = fun var ->
    try
      (Mvg.VariableMap.find var p).Mvg.var_io = Mvg.Output
    with
    | Not_found -> assert false (* should not happen *)
  in
  let is_necessary_to_output = OutputToInputReachability.analyze is_output g in
  Mvg.VariableMap.filter (fun var _ ->
      not (is_necessary_to_output var)
    ) (Mvg.VariableMap.map (fun _ -> ()) p)

let correctly_defined_outputs
    (g: DepGraph.t)
    (p: Mvg.program)
  : unit Mvg.VariableMap.t =
  let is_output = fun var ->
    try
      (Mvg.VariableMap.find var p).Mvg.var_io = Mvg.Output
    with
    | Not_found -> assert false (* should not happen *)
  in
  let is_undefined = fun var ->
    try
      (Mvg.VariableMap.find var p).Mvg.var_is_undefined
    with
    | Not_found -> assert false (* should not happen *)
  in
  let contains_undefined = InputToOutputReachability.analyze is_undefined g in
  let is_output_and_does_not_contain_undefined = Mvg.VariableMap.filter (fun v _ -> is_output v)
      (Mvg.VariableMap.filter (fun v _ ->
           not (contains_undefined v)
         ) p)
  in
  Mvg.VariableMap.map (fun _ -> ()) is_output_and_does_not_contain_undefined

let try_and_fix_undefined_dependencies
    (g: DepGraph.t)
    (p: Mvg.program)
    (var_defs_not_in_app: Mvg.program)
  : Mvg.program  =
  let is_output = fun var ->
    try
      (Mvg.VariableMap.find var p).Mvg.var_io = Mvg.Output
    with
    | Not_found -> assert false (* should not happen *)
  in
  let is_undefined = fun var ->
    try
      (Mvg.VariableMap.find var p).Mvg.var_is_undefined
    with
    | Not_found -> assert false (* should not happen *)
  in
  let in_needed_by_output = OutputToInputReachability.analyze is_output g in
  let is_needed_by_ouptput_and_undefined =
    Mvg.VariableMap.filter (fun v _ -> is_undefined v)
      (Mvg.VariableMap.filter (fun v _ -> in_needed_by_output v) p)
  in
  let is_needed_by_ouptput_and_undefined_fix =
    Mvg.VariableMap.mapi (fun var undef ->
        Mvg.VariableMap.find_opt var var_defs_not_in_app
      )
      is_needed_by_ouptput_and_undefined
  in
  let is_still_undefined _ x = match x with None -> true | Some _ -> false in
  begin if Mvg.VariableMap.exists
      is_still_undefined
      is_needed_by_ouptput_and_undefined_fix
    then
      let is_needed_by_ouptput_and_still_undefined =
        List.map
          (fun (v, _) -> Format_mvg.format_variable v)
          (Mvg.VariableMap.bindings
             (Mvg.VariableMap.map is_still_undefined
                is_needed_by_ouptput_and_undefined_fix))
      in
      Cli.warning_print
        (Printf.sprintf
           ("There are variables needed to computed that are undefined (%d):\n%s")
           (List.length is_needed_by_ouptput_and_still_undefined)
           (String.concat "\n" is_needed_by_ouptput_and_still_undefined);
        )
  end;
  Mvg.VariableMap.merge (fun var normal_def fixed_def -> match (normal_def, fixed_def) with
      | Some normal_def, Some None
      | Some normal_def, None -> Some normal_def
      | Some normal_def, Some (Some fixed_def) -> Some fixed_def
      | None, Some _
      | None, None -> assert false (* should not happen *)
    ) p is_needed_by_ouptput_and_undefined_fix


let requalify_outputs (p: Mvg.program) (only_output : unit Mvg.VariableMap.t) : Mvg.program =
  Mvg.VariableMap.mapi (fun v var_data ->
      { var_data with
        Mvg.var_io = if var_data.Mvg.var_io = Mvg.Output && not (Mvg.VariableMap.mem v only_output) then
            Mvg.Regular
          else
            var_data.Mvg.var_io
      }
    ) p


module Constability = Graph.Fixpoint.Make(DepGraph)
    (struct
      type vertex = DepGraph.E.vertex
      type edge = DepGraph.E.t
      type g = DepGraph.t
      type data = bool
      let direction = Graph.Fixpoint.Backward
      let equal = (=)
      let join = (&&)
      let analyze _ = (fun x -> x)
    end)

module TopologicalOrder = Graph.Topological.Make(DepGraph)
