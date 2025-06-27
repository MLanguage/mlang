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
                | MultipleFormulaes _ -> assert false
              in
              List.fold_left
                (fun graph var_dep -> G.add_edge graph (Pos.unmark var) var_dep)
                graph vl
          | _ -> graph)
        graph instrs)
    targets G.empty

module VertexMap = MapExt.Make (G.V)

let warn_unused_vertices (g : G.t) =
  let module GC_LIKE : sig
    val parcours : G.t -> unit
  end = struct
    type color = White | Grey | Black

    let parcours (g : G.t) =
      let all_vertices = G.fold_vertex (fun v l -> v :: l) g [] in
      let root_vertices =
        List.filter
          (fun var ->
            try
              Com.Var.is_given_back var
              || Com.Var.cat_var_loc var = Some Com.CatVar.LocInput
              || Com.Var.in_verif var
            with Errors.StructuredError _ -> true)
          all_vertices
      in
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
      let white_vertices, _black_vertices =
        G.fold_vertex
          (fun v (w, b) ->
            let color = VertexMap.find v vmap in
            match color with
            | White -> (v :: w, b)
            | Black -> (w, v :: b)
            | Grey ->
                (* shouldn't happen *)
                failwith
                  (Format.sprintf "Neither black or white found on name %s"
                     (Pos.unmark v.Com.Var.name)))
          g ([], [])
      in
      let module O = Graph.Oper.P (G) in
      let m = O.mirror g in
      let vars_in_degrees = VertexMap.empty in
      let vars_in_degrees =
        G.fold_vertex
          (fun var dmap ->
            let d = G.out_degree m var in
            let past_degree =
              match VertexMap.find_opt var dmap with
              | Some deg -> deg
              | None -> 0
            in
            let d = max d past_degree in
            VertexMap.add var d dmap)
          m vars_in_degrees
      in
      (* we have to use the mirror graph because if we used in_degree the complexity would be awful *)
      (* keeping track of the in_degrees makes it *slightly* easier to track which vertices will be the easiest to remove *)
      let white_vertices =
        List.fast_sort
          (fun s1 s2 ->
            compare
              (VertexMap.find s1 vars_in_degrees)
              (VertexMap.find s2 vars_in_degrees))
          white_vertices
      in
      List.iter
        (fun v ->
          Cli.warning_print
            "Variable %s isn't useful to compute any given back variable or \
             verification"
            (Pos.unmark v.Com.Var.name))
        white_vertices
  end in
  GC_LIKE.parcours g

let warn_unused_variables (targets : Mir.target_data Com.TargetMap.t) : unit =
  targets |> var_graph |> warn_unused_vertices
