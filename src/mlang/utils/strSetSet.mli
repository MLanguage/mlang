include Set.S with type elt = StrSet.t

val from_list_list : string list list -> t

val from_marked_list_list : string Pos.marked list Pos.marked list -> t

val pp : string -> string -> Format.formatter -> t -> unit
