module Origin = struct
  type code = Rule of int | Declared | Input | Target of string | Const

  type t = { filename : string; sline : int; eline : int; code_orig : code }

  let make filename sline eline code_orig =
    { filename; sline; eline; code_orig }

  let make_from_pos pos code_orig =
    let filename = Pos.get_file pos in
    let sline = Pos.get_start_line pos in
    let eline = Pos.get_end_line pos in
    { filename; sline; eline; code_orig }

  let hash (t : t) = Hashtbl.hash t

  let to_json origin =
    let code_orig =
      match origin.code_orig with
      | Rule i -> Format.asprintf "%d" i
      | Input -> "input"
      | Declared -> "Declared"
      | Target s -> Format.asprintf "target-%s" s
      | Const -> "const"
    in
    Format.asprintf
      {|"origin": {"code_orig": "%s", "file": "%s", "sline": %d, "eline": %d }|}
      code_orig origin.filename origin.sline origin.eline
end

module Tick = struct
  include Int

  let inner = ref (-1)

  let tick () =
    incr inner;
    !inner

  module Map = struct
    include IntMap
  end
end

module Info = struct
  type t = {
    name : string;
    var : Com.Var.t;
    value : Com.literal;
    origin : Origin.t;
  }
  (* We've removed idx_opt, it may be needed for tables. *)

  let make name var value origin = { name; var; value; origin }

  module Runtime = struct
    type t = { hash : int; value : Com.literal; name : string option }

    let make origin value name = { hash = Origin.hash origin; value; name }
  end

  module Static = struct
    type t = {
      name : string;
      origin : Origin.t;
      is_input : bool;
      descr : string option;
    }

    let make name origin is_input descr = { name; origin; is_input; descr }
  end
end

module Const = struct
  type t = { name : string; value : Com.literal; origin : Origin.t }

  let make name value fname sline eline =
    let origin = Origin.make fname sline eline Const in
    { name; value; origin }

  let make_from_pos name value pos =
    let origin = Origin.make_from_pos pos Const in
    { name; value; origin }
end

module Vertex = struct
  type kind = Literal | Var

  include Tick

  (* Invariant (to be verified): All ticks are different *)
  type t = Tick.t

  (* This feels weird, but String.hash was introduced in 5.0 *)
  let hash t = Hashtbl.hash t
end

module Graph = Graph.Persistent.Digraph.Concrete (Vertex)

module TickMap = struct
  include StrMap

  let find name map =
    match StrMap.find_opt name map with
    | None ->
        let msg =
          if StrMap.card map > 100 then
            Format.asprintf "could not find %s in tick_map (too long).@." name
          else
            Format.asprintf "could not find %s in tick_map %a.@." name
              (StrMap.pp (fun fmt -> Format.fprintf fmt "%d"))
              map
        in
        raise @@ Failure msg
    | Some tick -> tick
end

type t = {
  graph : Graph.t;
  runtimes : Info.Runtime.t Tick.Map.t;
  statics : Info.Static.t IntMap.t;
  consts : Const.t IntMap.t;
  literals : string IntMap.t;
  ledger : Tick.t StrMap.t;
}

let empty =
  {
    graph = Graph.empty;
    runtimes = Tick.Map.empty;
    statics = IntMap.empty;
    consts = IntMap.empty;
    literals = IntMap.empty;
    ledger = StrMap.empty;
  }

let to_json (fmt : Format.formatter) info : unit =
  let open Format in
  let open Info.Static in
  let open Info.Runtime in
  let open Const in
  let delim = ref "" in
  Format.fprintf fmt {|{"graph":{@. "nodes": [|};
  let pp_vertex v =
    let var = Graph.V.label v in
    Format.fprintf fmt {|%s@.{"data": "%d"}|} !delim var;
    (* Small hack to avoid trailing commas *)
    delim := ","
  in
  Format.printf "writing vertices...@.";
  Graph.iter_vertex pp_vertex info.graph;
  fprintf fmt {|],@. "edges": [|};
  let print_edge (e : Graph.E.t) =
    let src = Graph.E.src e in
    let dst = Graph.E.dst e in
    let src = Graph.V.label src in
    let dst = Graph.V.label dst in
    Format.fprintf fmt {|%s@.{"data": {"source": "%d", "target": "%d"}}|} !delim
      src dst;
    delim := ","
  in
  delim := "";
  Format.printf "writing edges...@.";
  Graph.iter_edges_e print_edge info.graph;
  let print_static_info hash { name; origin; is_input; descr } =
    let origin = Origin.to_json origin in
    let descr =
      match descr with
      | None -> ""
      | Some descr ->
          let descr = Yojson.Safe.to_string (`String descr) in
          asprintf {|"descr": %s,|} descr
    in
    Format.fprintf fmt {|%s@."%d": {"name": %S, "is_input": %b, %s %s}|} !delim
      hash name is_input descr origin;
    delim := ","
  in
  Format.fprintf fmt "]},@.";
  Format.printf "writing info...@.";
  delim := "";
  Format.fprintf fmt {|"statics": {@.|};
  IntMap.iter print_static_info info.statics;
  Format.fprintf fmt "},@.";
  delim := "";
  Format.fprintf fmt {|"runtimes": {@.|};
  let print_runtime_info tick { value; hash; name } =
    let name =
      match name with
      | None -> ""
      | Some name -> asprintf {|, "name" : %S|} name
    in
    Format.fprintf fmt {|%s@."%d": {"value": "%a", "hash": %d %s}|} !delim tick
      Com.format_literal value hash name;
    delim := ","
  in
  Tick.Map.iter print_runtime_info info.runtimes;
  let print_const id const =
    Format.printf "Printing consts!!!!@.";
    let origin = Origin.to_json const.origin in
    Format.fprintf fmt
      {|%s@."%d": {"name": %S, "value": "%a", "kind": "const", %s}|} !delim id
      const.name Com.format_literal const.value origin;
    delim := ","
  in
  IntMap.iter print_const info.consts;
  let print_lit id lit =
    Format.fprintf fmt {|%s@."%d": {"name": %S}|} !delim id lit;
    delim := ","
  in
  IntMap.iter print_lit info.literals;
  Format.fprintf fmt "}}@."

let write_json_file filename info =
  let filename = filename ^ ".json" in
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@." to_json info
