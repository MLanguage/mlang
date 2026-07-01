(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2026 DGFiP - INRIA                                      *)
(*                                                                            *)
(* Ce programme est distribué sous la licence CeCILL-C: vous pouvez le        *)
(* redistribuer et/ou le modifier sous les contraintes de celle-ci.           *)
(*                                                                            *)
(* L'accessibilité au code source et les droits de copie, de modification et  *)
(* de redistribution qui découlent de ce contrat ont pour contrepartie de     *)
(* n'offrir aux utilisateurs qu'une garantie limitée et de ne faire peser sur *)
(* l'auteur du logiciel, le titulaire des droits patrimoniaux et les          *)
(* concédants successifs qu'une responsabilité restreinte.                    *)
(*                                                                            *)
(******************************************************************************)

open M_ir

module Origin = struct
  type code =
    | Rule of int
    | Declared
    | Input
    | Target of string
    | Anomaly
    | Const

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
      | Declared -> "declared"
      | Target s -> Format.asprintf "target-%s" s
      | Const -> "const"
      | Anomaly -> "anomaly"
    in
    Format.asprintf
      {|{"code_orig": "%s", "file": "%s", "sline": %d, "eline": %d }|} code_orig
      origin.filename origin.sline origin.eline
end

module Tick = struct
  include Int

  let inner = ref (-1)

  let tick () =
    incr inner;
    !inner

  module Map = IntMap
end

module Info = struct
  type t = {
    tick : Tick.t;
    name : string;
    pos : Pos.t;
    rule : Origin.code;
    value : Com.literal;
    descr : string option;
    is_input : bool;
    decl_origin : Origin.t;
  }

  let make tick name pos rule value descr is_input decl =
    let decl_origin = Origin.make_from_pos decl Declared in
    { tick; name; pos; rule; value; descr; is_input; decl_origin }

  let make_from_var tick var rule value descr is_input =
    let name = Com.Var.name_str var in
    let pos = Com.Var.name var |> Pos.get in
    let decl_pos = Pos.get var.name in
    make tick name pos rule value descr is_input decl_pos

  (* We've removed idx_opt, it may be needed for tables. *)

  module Runtime = struct
    type t = { hash : int; value : Com.literal; name : string option }

    type hash_entry = { origin : Origin.t; name : string option }

    let make origin value name =
      let hash = Hashtbl.hash { origin; name } in
      { hash; value; name }
  end

  module Static = struct
    type t = {
      name : string;
      origin : Origin.t;
      is_input : bool;
      descr : string option;
      decl_origin : Origin.t;
    }

    let make name ~origin is_input descr ~decl_origin =
      { name; origin; is_input; descr; decl_origin }
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
  include Tick

  (* This feels weird, but String.hash was introduced in 5.0 *)
  let hash t = Hashtbl.hash t
end

module Graph = Graph.Imperative.Digraph.ConcreteBidirectional (Vertex)

module TickMap = struct
  include StrMap

  let find_opt name map = StrMap.find_opt name map
end

type interp_error = {
  name : string;
  value : Com.literal;
  expected : Com.literal;
}

type anomaly = { name : string; origin : Origin.t; raised_origin : Origin.t }

type t = {
  graph : Graph.t;
  runtimes : Info.Runtime.t Tick.Map.t;
  statics : Info.Static.t IntMap.t;
  consts : Const.t IntMap.t;
  literals : string IntMap.t;
  ledger : Tick.t StrMap.t;
  interp_errors : interp_error Tick.Map.t;
  anomalies : anomaly list;
  aliases : string StrMap.t;
}

let make_empty ~aliases =
  {
    graph = Graph.create ~size:10000 ();
    runtimes = Tick.Map.empty;
    statics = IntMap.empty;
    consts = IntMap.empty;
    literals = IntMap.empty;
    ledger = StrMap.empty;
    interp_errors = Tick.Map.empty;
    anomalies = [];
    aliases;
  }

let register dbg_info
    Info.{ tick; name; pos; rule; value; descr; is_input; decl_origin } =
  let origin = Origin.make_from_pos pos rule in
  let runtime = Info.Runtime.make origin value (Some name) in
  let runtimes = Tick.Map.add tick runtime dbg_info.runtimes in
  let static = Info.Static.make name ~origin is_input descr ~decl_origin in
  let statics = IntMap.add runtime.hash static dbg_info.statics in
  let ledger = TickMap.add name tick dbg_info.ledger in
  let dbg_info = { dbg_info with runtimes; statics; ledger } in
  dbg_info

let json_of_graph_matrix fmt info =
  let open Format in
  let delim = ref "" in
  let delim2 = ref "" in
  fprintf fmt {|{"kind": "matrix", "graph":{@. |};
  let iter_vertex v =
    fprintf fmt {|%s"%d": {|} !delim v;
    let pp_succ s =
      fprintf fmt "%s%d" !delim2 s;
      delim2 := ","
    in
    delim2 := "";
    fprintf fmt {|"outcoming": [|};
    Graph.iter_succ pp_succ info.graph v;
    fprintf fmt {|]@., "incoming": [|};
    delim2 := "";
    Graph.iter_pred pp_succ info.graph v;
    fprintf fmt "]@.}";
    delim := ","
  in
  Graph.iter_vertex iter_vertex info.graph;
  fprintf fmt "},@."

let to_json (fmt : Format.formatter) info : unit =
  let open Format in
  let open Info.Static in
  let open Info.Runtime in
  let open Const in
  let delim = ref "" in
  json_of_graph_matrix fmt info;
  let print_static_info hash { name; origin; is_input; descr; decl_origin } =
    let origin = Origin.to_json origin in
    let descr =
      match descr with
      | None -> ""
      | Some descr -> asprintf {|"descr": %S,|} descr
    in
    fprintf fmt {|%s@."%d": {"name": %S, "is_input": %b, "decl": %s, %s %s}|}
      !delim hash name is_input
      (Origin.to_json decl_origin)
      descr origin;
    delim := ","
  in
  Ppf.debug_print "writing info...@.";
  delim := "";
  fprintf fmt {|"statics": {@.|};
  IntMap.iter print_static_info info.statics;
  fprintf fmt "},@.";
  delim := "";
  fprintf fmt {|"runtimes": {@.|};
  let print_runtime_info tick { value; hash; name } =
    let name =
      match name with
      | None -> ""
      | Some name -> asprintf {|, "name" : %S|} name
    in
    fprintf fmt {|%s@."%d": {"value": "%a", "hash": %d %s}|} !delim tick
      Com.format_literal value hash name;
    delim := ","
  in
  Tick.Map.iter print_runtime_info info.runtimes;
  let print_const id const =
    let origin = Origin.to_json const.origin in
    fprintf fmt
      {|%s@."%d": {"name": %S, "value": "%a", "kind": "const", "origin": %s}|}
      !delim id const.name Com.format_literal const.value origin;
    delim := ","
  in
  IntMap.iter print_const info.consts;
  let print_lit id lit =
    fprintf fmt {|%s@."%d": {"name": %S}|} !delim id lit;
    delim := ","
  in
  IntMap.iter print_lit info.literals;
  delim := "";
  let print_interp_errors tick (error : interp_error) =
    fprintf fmt {|%s"%d": {"name": %S, "value": %a, "expected": %a}|} !delim
      tick error.name Com.format_literal error.value Com.format_literal
      error.expected;
    delim := ","
  in
  fprintf fmt {|},@."interp_errors": {@.|};
  Tick.Map.iter print_interp_errors info.interp_errors;
  delim := "";
  let print_alias alias name =
    fprintf fmt {|%s@.%S: %S|} !delim name alias;
    delim := ","
  in
  fprintf fmt {|},@."aliases": {|};
  StrMap.iter print_alias info.aliases;
  fprintf fmt "@.}}@."

let write_json_file filename info =
  let filename = filename ^ ".json" in
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@." to_json info
