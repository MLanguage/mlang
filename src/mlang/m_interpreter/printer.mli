open M_ir

type t

val make : Com.print_std -> ('a, 'b) Context.t -> t

val flush : t -> unit

val out_indent : t -> unit

val raw : t -> string -> unit

val set_indent : t -> int -> unit

val info : t -> Com.print_info -> Com.variable_space -> Com.Var.t -> unit

val string : t -> string -> unit

val access : t -> Com.print_info -> Com.variable_space * Com.Var.t * 'a -> unit
