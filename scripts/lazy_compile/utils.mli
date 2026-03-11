module Env : sig
  val output_dir : string
  val graph_filename : string
  val debug : string
  val pedantic : string
  val cc : string
end

module StrSet : Set.S with type elt = string
module StrMap : Map.S with type key = string

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

module Log : sig
  val debug : ('a, Format.formatter, unit) format -> 'a
  val warn : ('a, Format.formatter, unit) format -> 'a
  val err : ('a, Format.formatter, unit) format -> 'a
  val log : ('a, Format.formatter, unit) format -> 'a
end
