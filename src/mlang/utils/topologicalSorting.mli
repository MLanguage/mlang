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

module Make : functor (G : GRAPH) ->
  T
    with type 'a graph = 'a G.t
     and type vertex = G.vertex
     and type edge = G.edge
