(** Printer of the interpreter.

    This module implements the basic helpers for the print ("afficher") M
    instruction. *)

open M_ir

type t
(** A printer for a given formatter. It keeps track of the indentation. *)

val make : Com.print_std -> t
(** Creates a fresh printer on StdOut or on StdErr. *)

val flush : t -> unit
(** Flushes the formatter if it is an StdErr. *)
(* But... why? *)

val raw : t -> string -> unit
(** Prints a string on a given printer. *)

val set_indent : t -> int -> unit
(** Adds or removes indentation. The minimal indentation is zero. *)

val info : t -> Com.print_info -> Com.variable_space -> Com.Var.t -> unit

val string : t -> string -> unit
(** Prints a string and flushes *)

val access : t -> Com.print_info -> Com.variable_space * Com.Var.t * 'a -> unit
