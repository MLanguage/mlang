open Format

type t = formatter

val nil : t -> 'a -> unit

val string : t -> string -> unit

val option : (t -> 'a -> unit) -> t -> 'a option -> unit

val list : (unit, t, unit) format -> (t -> 'a -> unit) -> t -> 'a list -> unit

val list_endline : (t -> 'a -> unit) -> t -> 'a list -> unit

val list_comma : (t -> 'a -> unit) -> t -> 'a list -> unit

val list_space : (t -> 'a -> unit) -> t -> 'a list -> unit

val unmark : (t -> 'a -> unit) -> t -> 'a Pos.marked -> unit
