include Set.S with type elt = String.t

val from_list : string list -> t

val from_marked_list : string Pos.marked list -> t

val pp : string -> Format.formatter -> t -> unit
