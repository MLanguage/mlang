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

module Make : functor (G : GRAPH) ->
  T with type 'a graph = 'a G.t and type vertex = G.vertex
