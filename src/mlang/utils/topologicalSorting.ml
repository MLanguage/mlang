module type GRAPH = sig
  type 'a t

  type vertex

  type vertexSet

  val vertexSetFold : (vertex -> 'a -> 'a) -> vertexSet -> 'a -> 'a

  val vertexSetMem : vertex -> vertexSet -> bool

  val vertexSetRemove : vertex -> vertexSet -> vertexSet

  type 'a vertexMap

  val vertexMapEmpty : 'a vertexMap

  val vertexMapAdd : vertex -> 'a -> 'a vertexMap -> 'a vertexMap

  val vertexMapFind : vertex -> 'a vertexMap -> 'a

  val vertices : 'a t -> vertexSet

  val edges : 'a t -> vertex -> vertexSet
end

module type T = sig
  type 'a graph

  type vertex

  exception Cycle of vertex list

  exception AutoCycle of vertex

  val sort : ?auto_cycle:(vertex -> unit) option -> 'a graph -> vertex list
end

module Make (G : GRAPH) :
  T with type 'a graph = 'a G.t and type vertex = G.vertex = struct
  type 'a graph = 'a G.t

  type vertex = G.vertex

  type couleur = White | Grey | Black

  exception Cycle of vertex list

  exception AutoCycle of vertex

  let sort ?(auto_cycle = None) (g : 'a graph) =
    let couls =
      let fold nd couls = G.vertexMapAdd nd White couls in
      G.vertexSetFold fold (G.vertices g) G.vertexMapEmpty
    in
    let rec parcours nd (couls, ord) =
      match G.vertexMapFind nd couls with
      | Grey -> raise (Cycle (nd :: ord))
      | Black -> (couls, ord)
      | White ->
          let couls = G.vertexMapAdd nd Grey couls in
          let ndfs =
            let ndfs = G.edges g nd in
            if G.vertexSetMem nd ndfs then (
              match auto_cycle with
              | None -> raise (AutoCycle nd)
              | Some f ->
                  f nd;
                  G.vertexSetRemove nd ndfs)
            else ndfs
          in
          let couls, ord = G.vertexSetFold parcours ndfs (couls, ord) in
          let couls = G.vertexMapAdd nd Black couls in
          (couls, nd :: ord)
    in
    snd (G.vertexSetFold parcours (G.vertices g) (couls, []))
end
