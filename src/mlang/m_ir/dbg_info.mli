(** This module stores the debug info derived from tracing an execution of some
    M code. In order to save some space, we split tracing information into two
    categories, static information and runtime information. Runtime information
    includes the name of the variable, the point in time at which its value is
    assigned, and the value it's assigned.

    Static information includes the name and location of the variable (filename,
    line, ...), its description. Static Information is indexed by some hash
    derived from some runtime info. *)

(** Origin describes the origin of variables. *)
module Origin : sig
  type code =
    | Rule of int
    | Declared
    | Input
    | Target of string
    | Const
        (** Where the variable is declared: In a rule, as input but not given,
            as input but given, in a target, or a const. *)

  type t = { filename : string; sline : int; eline : int; code_orig : code }
  (** The origin of variable *)

  val make : string -> int -> int -> code -> t
  (** [make filename start_line end_line code_origin] *)

  val make_from_pos : Utils.Pos.t -> code -> t
  (** [make_from_pos position code_origin] *)

  val hash : t -> int
  (** A simple hash. *)

  val to_json : t -> string
  (** Transforms [t] into a json string. *)
end

module Tick : sig
  type t = int
  (** One unit of calculation *)

  val tick : unit -> t
  (** Returns a new, non-used tick *)

  module Map = IntMap
end

module Graph : sig
  include Graph.Sig.P with type V.label = int
end

module TickMap : sig
  include StrMap.T with type 'a t = 'a StrMap.t

  val find_opt : key -> int t -> int option
  (** [find_opt name map] returns the tick associated with [name] in [map] if iT
      is found. *)
end

module Info : sig
  (** This module implements the different datatypes linked to information about
      variable assignments *)

  type t = {
    tick : Tick.t;
    name : string;
    pos : Utils.Pos.t;
    rule : Origin.code;
    value : Com.literal;
    descr : string option;
    is_input : bool;
  }
  (** Tracing assignment info as the interperter outputs them. [tick] is
      supposed to be different for different assignments *)

  val make :
    Tick.t ->
    string ->
    Utils.Pos.t ->
    Origin.code ->
    Com.literal ->
    string option ->
    bool ->
    t
  (** [make tick name position rule value descr is_input] creates a new [t]
      based on argument info. *)

  val make_from_var :
    Tick.t ->
    Com.Var.t ->
    Origin.code ->
    Com.literal ->
    string option ->
    bool ->
    t
  (** [make_from_var tick var rule description is_input] *)

  module Runtime : sig
    type t = { hash : int; value : Com.literal; name : string option }
    (** Runtime info about variables, eg their value, and where to find their
        static info. *)

    val make : Origin.t -> Com.literal -> string option -> t
    (** [make origin value description] *)
  end

  module Static : sig
    type t = {
      name : string;
      origin : Origin.t;
      is_input : bool;
      descr : string option;
    }
    (** Static information about variables eg. stuff that doesn't depend on
        execution. *)

    val make : string -> Origin.t -> bool -> string option -> t
    (** [make name origin is_input description] *)
  end
end

module Const : sig
  type t = { name : string; value : Com.literal; origin : Origin.t }
  (** Information about constants. *)

  val make : string -> Com.literal -> string -> int -> int -> t
  (** [make name value file_name start_line end_line] *)

  val make_from_pos : string -> Com.literal -> Utils.Pos.t -> t
  (** [make_from_pos name value position] *)
end

type interp_error = {
  name : string;
  value : Com.literal;
  expected : Com.literal;
}
(** An interpretation error -- variable [name] was equal to [value] while we
    expected [expected] *)

type t = {
  graph : Graph.t;
  runtimes : Info.Runtime.t Tick.Map.t;
  (* Runtime info map. Indexed by the tick at which the value was set *)
  statics : Info.Static.t IntMap.t;
  (* Static info map. Indexed by the hash derived by the runtime *)
  consts : Const.t IntMap.t;
  literals : string IntMap.t;
  ledger : Tick.t StrMap.t;
  (* The map making the link between the variable name, and the last tick it has been assigned to *)
  interp_errors : interp_error Tick.Map.t;
      (* map of the errors raised by the execution *)
}

val empty : t
(** Empty dbg_info. *)

val register : t -> Info.t -> t
(** Takes the current env and tracing info. It splits it into runtime and static
    info, then registers it in the correct maps. *)

val to_json : Format.formatter -> t -> unit
(** Outputs json from some dbg_info. *)

val write_json_file : string -> t -> unit
(** Writes a json serialization of dbg_info into a file. Uses `to_json`. *)
