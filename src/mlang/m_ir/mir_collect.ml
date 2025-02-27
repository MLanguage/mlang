(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

module G = Graph.Persistent.Digraph.Concrete (struct
  type t = Com.Var.t

  let compare = compare

  let hash _ = 0

  let equal = ( = )
end)

let var_graph (targets : Mir.target_data Com.TargetMap.t) : G.t =
  Com.TargetMap.fold
    (fun _ t graph ->
      let instrs = t.Mir.target_prog in
      List.fold_left
        (fun graph instr ->
          match Pos.unmark instr with
          | Com.Affectation f ->
              let var, vl =
                match Pos.unmark f with
                | SingleFormula (var, _, e) ->
                    (var, Com.get_used_variables (Pos.unmark e))
                | MultipleFormulaes _ -> failwith "multiple formulae ?"
              in
              List.fold_left
                (fun graph var_dep -> G.add_edge graph (Pos.unmark var) var_dep)
                graph vl
          | _ -> graph)
        graph instrs)
    targets G.empty

module VertexMap = MapExt.Make (G.V)

type color = White | Grey | Black

let remove_unused_vertices (g : G.t) =
  let module GC_LIKE : sig
    val parcours : G.t -> color VertexMap.t
  end = struct
    (* type color = White | Grey | Black *)

    let parcours (g : G.t) =
      let all_vertices = G.fold_vertex (fun v l -> v :: l) g [] in
      let root_vertices =
        List.filter
          (fun var ->
            try
              Com.Var.is_given_back var
              || Com.Var.cat_var_loc var = Some Com.CatVar.LocInput
              || Com.Var.in_verif var
              (* || Com.Var.is_base var *)
            with Errors.StructuredError _ -> true)
          all_vertices
      in
      (* let module VertexMap = Map.Make (G.V) in *)
      let vmap = VertexMap.empty in
      let vmap =
        G.fold_vertex (fun v map -> VertexMap.add v White map) g vmap
      in
      let vmap =
        List.fold_right
          (fun v map -> VertexMap.add v Grey map)
          root_vertices vmap
      in
      let rec mark (g : G.t) (grey : G.vertex list) (vmap : color VertexMap.t) =
        match grey with
        | [] -> vmap
        | v :: l ->
            let succs = G.succ g v in
            let succs =
              List.filter (fun v -> VertexMap.find v vmap = White) succs
            in
            let vmap =
              List.fold_right
                (fun v map ->
                  if VertexMap.find v map = White then VertexMap.add v Grey map
                  else map)
                succs vmap
            in
            let vmap = VertexMap.add v Black vmap in
            mark g (succs @ l) vmap
      in
      let vmap = mark g root_vertices vmap in
      let white_vertices, black_vertices =
        G.fold_vertex
          (fun v (w, b) ->
            let color = VertexMap.find v vmap in
            if color = White then (v :: w, b)
            else if color = Black then (w, v :: b)
            else
              (* a bit verbose but this is just to make sure, should never happen anyways *)
              failwith
                (Format.sprintf "Neither black or white found on name %s"
                   (Pos.unmark v.Com.Var.name)))
          g ([], [])
      in
      let name_map = StrMap.empty in
      let name_map =
        G.fold_vertex
          (fun var nmap ->
            let vname = Pos.unmark var.Com.Var.name in
            let color = try StrMap.find vname nmap with Not_found -> White in
            if color <> Black then
              StrMap.add vname (VertexMap.find var vmap) nmap
            else nmap)
          g name_map
      in
      let module O = Graph.Oper.P (G) in
      let m = O.mirror g in
      let names_in_degrees = StrMap.empty in
      let names_in_degrees =
        G.fold_vertex
          (fun var dmap ->
            let vname = Pos.unmark var.Com.Var.name in
            let d = G.out_degree m var in
            let past_degree =
              match StrMap.find_opt vname dmap with
              | Some deg -> deg
              | None -> d
            in
            let d = max d past_degree in
            StrMap.add vname d dmap)
          m names_in_degrees
      in
      (* Format.printf "V_FORVA degree : %d@."
         (StrMap.find "V_FORVA" names_in_degrees); *)
      (* we have to use the mirror because if we used in_degree the complexity would be awful *)
      let white_names, black_names =
        StrMap.fold
          (fun s color (w, b) ->
            if color = White then (s :: w, b)
            else if color = Black then (w, s :: b)
            else failwith "Not black and white found")
          name_map ([], [])
      in
      let white_names =
        List.fast_sort
          (fun s1 s2 ->
            compare
              (StrMap.find s1 names_in_degrees)
              (StrMap.find s2 names_in_degrees))
          white_names
      in
      (* List.iter (fun s -> Format.printf "%s@." s) white_names; *)
      List.iter
        (fun name ->
          Cli.warning_print "Unused variable : %s - in_degree %d" name
            (StrMap.find name names_in_degrees))
        white_names;
      Format.printf "vertices -- all: %d, white : %d, black : %d@."
        (G.nb_vertex g)
        (List.length white_vertices)
        (List.length black_vertices);
      Format.printf "names -- all: %d, white : %d, black : %d@."
        (StrMap.cardinal name_map) (List.length white_names)
        (List.length black_names);
      vmap
  end in
  let vmap = GC_LIKE.parcours g in
  vmap

let var_graph_act (targets : Mir.target_data Com.TargetMap.t) : unit =
  let g = var_graph targets in
  ignore (remove_unused_vertices g)
