
(** Environment variables used through the script. *)
module Env : sig
  val output_dir : string
  (** The directory where compiled files are written into. *)

  val graph_filename : string
  (** The filename in which we write the results of the compilation. *)

  val debug : string
  (** The verbosity of the script. Should be an integer. *)

  val pedantic : string
  (** The verbosity of the compuler. Should be an integer. *)

  val cc : string
  (** The used compiler. *)
end

(** Collections *)

module StrSet : Set.S with type elt = string
module StrMap : Map.S with type key = string

(** Pretty printers *)

val pp_list :
  sep:(unit, Format.formatter, unit) format ->
  pp:(Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

val pp_str_map :
  sep:(unit, Format.formatter, unit) format *
      (unit, Format.formatter, unit) format ->
  pp:(Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a StrMap.t -> unit

val run_command : string -> string
(** Runs a command and outputs its result as a string *)

(** Different logs helpers, using [Env.debug] to select which
    are active. *)
module Log : sig  
  val log : ('a, Format.formatter, unit) format -> 'a
  (** Prints in stdout *)

  val err : ('a, Format.formatter, unit) format -> 'a
  (** Prints in stderr *)

  val warn : ('a, Format.formatter, unit) format -> 'a
  (** Prints in stdout if debug >= 1 *)

  val debug : ('a, Format.formatter, unit) format -> 'a
  (** Prints in stdout if debug >= 2 *)
end
