
(** A module for dependency graphs. This graph will be saved after the project
    compilation for future compilation. When the script starts, it will read the
    old graph and compare the files digests. If a file does not have the same
    digest in the two graph, its compilation (and the compilation of all the
    files depending on it) must be restarted. *)

type t

exception MissingFileDeclaration of string
(** This is raised when we try to add a file to the graph that is not in the
    mlang_generated list nor in the ext_dep. *)

val pp : Format.formatter -> t -> unit
(** Pretty prints a dependency graph. *)

val make :
  cfiles_dir:string -> ext_dep:(string * string) list -> t
(** Reads the cfiles_dir directory and builds the corresponding dependency graph. *)

val compile: cfiles_dir:string -> old:t -> new_:t -> bool Utils.StrMap.t
(** Compiles the graph files in the correct dependency order.
    [old] holds the previous dependency graph, so that only files that have been updated
    are recompiled. *)

val write : t -> unit

val read : unit -> t
