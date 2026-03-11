

module Env = struct
  (** Returns the value of an env variable [k]. If absent, returns [default]. *)
  let getenv ~default k =
    match Sys.getenv k with v -> v | exception Not_found -> default

  (** The output dir *)
  let output_dir = getenv ~default:"output" "OUTPUT_DIR"

  (** The graph filename *)
  let graph_filename = getenv ~default:".depgraph" "DEPGRAPH_FILENAME"

  (** If set to something else than "0", display debug messages.*)
  let debug = getenv ~default:"0" "DEBUG"

  let pedantic = getenv ~default:"1" "PEDANTIC"

  let cc = getenv ~default:"gcc" "CC"
end

module StrSet = Set.Make (String)
module StrMap = Map.Make (String)

(** Pretty prints a list. *)
let pp_list ~sep ~pp fmt l =
  Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt sep) pp fmt l

(** Pretty prints a string map. *)
let pp_str_map ~sep ~pp fmt m =
  let skb, sl = sep in
  StrMap.iter
    (fun k b ->
      Format.fprintf fmt "%s%t%a%t" k
        (fun fmt -> Format.fprintf fmt skb)
        pp b
        (fun fmt -> Format.fprintf fmt sl))
    m

(** Runs a command and returns its output as a string *)
let run_command (cmd : string) : string =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  Buffer.contents buf

(** Debug & error logs. *)
module Log = struct
  let dbg = int_of_string_opt Env.debug

  let debug : 'a. ('a, Format.formatter, unit) format -> 'a =
    fun ppf ->
    match dbg with
    | Some i when i >= 2 ->
       Format.(fprintf std_formatter ("[DBG] " ^^ ppf ^^ "@."))
    | _ -> Format.(ifprintf std_formatter ppf)

  let warn : 'a. ('a, Format.formatter, unit) format -> 'a =
    fun ppf ->
    match dbg with
    | Some i when i >= 1 ->
       Format.(fprintf std_formatter ("[WRN] " ^^ ppf ^^ "@."))
    | _ -> Format.(ifprintf std_formatter ppf)

  let err : 'a. ('a, Format.formatter, unit) format -> 'a =
    fun ppf ->
    Format.(fprintf err_formatter ("[ERR] " ^^ ppf ^^ "@."))

  let log : 'a. ('a, Format.formatter, unit) format -> 'a =
    fun ppf -> Format.(fprintf std_formatter ("[APP] " ^^ ppf ^^ "@."))
end
