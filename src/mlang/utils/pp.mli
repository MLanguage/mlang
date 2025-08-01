open Format

type t = formatter

val fpr : t -> ('a, t, unit) format -> 'a

val spr : ('a, t, unit, string) format4 -> 'a

val pr : ('a, t, unit) format -> 'a

val epr : ('a, t, unit) format -> 'a

val nil : t -> 'a -> unit

val string : t -> string -> unit

val int : t -> int -> unit

val float : t -> float -> unit

val option : (t -> 'a -> unit) -> t -> 'a option -> unit

val list : (unit, t, unit) format -> (t -> 'a -> unit) -> t -> 'a list -> unit

val list_endline : (t -> 'a -> unit) -> t -> 'a list -> unit

val list_comma : (t -> 'a -> unit) -> t -> 'a list -> unit

val list_space : (t -> 'a -> unit) -> t -> 'a list -> unit

val unmark : (t -> 'a -> unit) -> t -> 'a Pos.marked -> unit
