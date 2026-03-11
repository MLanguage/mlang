
(** A module for dependency graphs. This graph will be saved after the project
    compilation for future compilation. When the script starts, it will read the
    old graph and compare the files digests. If a file does not have the same
    digest in the two graph, its compilation (and the compilation of all the
    files depending on it) must be restarted. *)

type file =
  | Mlang_gen of { mname : string; mhash : Digest.t; mdeps : string list; }
  | Ext_dep of { edname : string; edvers : string; }

type t = {
    graph : file Utils.StrMap.t;
    mlang_generated : string list;
    ext_dep : (string * string) list;
  }

exception MissingFileDeclaration of string

val pp_file : Format.formatter -> file -> unit

val pp : Format.formatter -> t -> unit

val line_states_it_depends_on : string -> string option

val file_states_it_depends_on : string -> string list

val add_file_to_graph : cfiles_dir:string -> t -> string -> t

val build_graph :
  cfiles_dir:string -> string list -> (string * string) list -> t

val write : t -> unit

val read : unit -> t
