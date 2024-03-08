module type GRAPH = sig
  type 'a t

  type vertex

  type edge

  type 'a vertexMap

  val vertexMapEmpty : 'a vertexMap

  val vertexMapAdd : vertex -> 'a -> 'a vertexMap -> 'a vertexMap

  val vertexMapRemove : vertex -> 'a vertexMap -> 'a vertexMap

  val vertexMapFindOpt : vertex -> 'a vertexMap -> 'a option

  val vertexMapFold : (vertex -> 'a -> 'b -> 'b) -> 'a vertexMap -> 'b -> 'b

  val vertices : 'a t -> edge option vertexMap

  val edges : 'a t -> vertex -> edge option vertexMap
end

module type T = sig
  type 'a graph

  type vertex

  type edge

  exception Cycle of (vertex * edge option) list

  exception AutoCycle of (vertex * edge)

  val sort :
    ?auto_cycle:(vertex * edge -> unit) option -> 'a graph -> vertex list
end

module Make (G : GRAPH) :
  T
    with type 'a graph = 'a G.t
     and type vertex = G.vertex
     and type edge = G.edge = struct
  type 'a graph = 'a G.t

  type vertex = G.vertex

  type edge = G.edge

  type couleur = White | Grey | Black

  exception Cycle of (vertex * edge option) list

  exception AutoCycle of (vertex * edge)

  let sort ?(auto_cycle = None) (g : 'a graph) =
    let couls =
      let fold nd _ couls = G.vertexMapAdd nd White couls in
      G.vertexMapFold fold (G.vertices g) G.vertexMapEmpty
    in
    let rec parcours nd orig_opt (couls, ord, tmp) =
      match Option.get (G.vertexMapFindOpt nd couls) with
      | Grey ->
          let rec get_cycle res = function
            | (nd', edge) :: _ when nd' = nd -> (nd', edge) :: res
            | nde :: tl -> get_cycle (nde :: res) tl
            | [] -> res
          in
          raise (Cycle (get_cycle [ (nd, orig_opt) ] tmp))
      | Black -> (couls, ord, tmp)
      | White ->
          let couls = G.vertexMapAdd nd Grey couls in
          let ndfs =
            let ndfs = G.edges g nd in
            match G.vertexMapFindOpt nd ndfs with
            | Some edge_opt -> (
                match auto_cycle with
                | None -> raise (AutoCycle (nd, Option.get edge_opt))
                | Some f ->
                    f (nd, Option.get edge_opt);
                    G.vertexMapRemove nd ndfs)
            | None -> ndfs
          in
          let couls, ord, _ =
            G.vertexMapFold parcours ndfs (couls, ord, (nd, orig_opt) :: tmp)
          in
          let couls = G.vertexMapAdd nd Black couls in
          (couls, (nd, orig_opt) :: ord, tmp)
    in
    let _, ord, _ = G.vertexMapFold parcours (G.vertices g) (couls, [], []) in
    List.map fst ord
end
