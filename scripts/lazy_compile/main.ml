(** Usage:
    
    $ lazy_compile --files [FILEDIR] --config [CONFIGFILE]

    Can be configured with additional environment variables:
    - [OUTPUT_DIR]: the dir to write the .o files (default: output). Generated if
      it does not exist.
    - [DEPGRAPH_FILENAME]: the file in which is serialized the dependency graph. Written
      in [OUTPUT_DIR].
    - [DEBUG]: displays debug messages (default: 0).
    - [PEDANTIC]: makes clang pedantic (default: 1).

    How to compile:
    
    $ ocamlfind ocamlc -package str -package unix -linkpkg -o lazy_compile main.ml
    
    TODOs:
    - a serious cli management;
    - logs in files;
    - versioning depgraph files or stop using Marshal (that may deserialize something
      badly and make the script fail even badlier). 
*)

module StrSet = Set.Make (String)
module StrMap = Map.Make (String)

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
end

(** Debug & error logs. *)
module Log = struct
  let debug : 'a. ('a, Format.formatter, unit) format -> 'a =
   fun pp ->
    match Env.debug with
    | "0" -> Format.(ifprintf std_formatter) pp
    | _ -> Format.printf pp

  let err : 'a. ('a, Format.formatter, unit) format -> 'a = Format.eprintf

  let log : 'a. ('a, Format.formatter, unit) format -> 'a = Format.printf
end

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

(** Returns the name of the compilation output file. *)
let output_file_name cfile =
  Filename.concat Env.output_dir (Filename.chop_extension cfile ^ ".o")

(** Compiles [cfile]. *)
let compile_file ~cfile ~ofile =
  let pedantic = if Env.pedantic = "0" then "" else "--pedantic " in
  let cmd =
    Format.sprintf "clang -std=c89 %s -O2 -c %s -o %s" pedantic cfile ofile
  in
  Log.log "Compiling file %S...@." cfile;
  let res = run_command cmd in
  Log.log "%s@." res;
  Log.log "Compilation of file %S complete -> %S@." cfile ofile;
  res

(** Returns the full C file name from its base name.
    By default, unless we are reading files, we only manipulate files through their
    basename. *)
let full_file ~cfiles_dir f = Filename.concat cfiles_dir f

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

(** A module for dependency graphs. This graph will be saved after the project
    compilation for future compilation.
    When the script starts, it will read the old graph and compare the files digests.
    If a file does not have the same digest in the two graph, its compilation (and the
    compilation of all the files depending on it) must be restarted. *)
module DepGraph = struct
  type file =
    (* Files that needs to be compiled.  *)
    | Mlang_gen of {
        mname : string;
        (* The base name of the file *)
        mhash : Digest.t;
        (* Its content's digest *)
        mdeps : string list; (* Its dependencies *)
      }
    (* External dependencies, no need to compile them *)
    | Ext_dep of {
        edname : string;
        (* The basename of the dependency *)
        edvers : string; (* The dependency verison *)
      }

  type t = {
    graph : file StrMap.t;
    (* map of file basenames to their file representation *)
    mlang_generated : string list;
    (* The list of files to compile *)
    ext_dep : (string * string) list; (* name * command to get version *)
  }

  exception MissingFileDeclaration of string
  (** This is raised when we try to add a file to the graph that is not in the
      mlang_generated list nor in the ext_dep. *)

  (** Pretty prints a file. For debug only. *)
  let pp_file fmt (f : file) =
    match f with
    | Mlang_gen { mdeps; mhash; _ } ->
        Format.fprintf fmt "M(%s)[%a]" (Digest.to_hex mhash)
          (pp_list ~sep:";@," ~pp:Format.pp_print_string)
          mdeps
    | Ext_dep { edvers; _ } -> Format.fprintf fmt "E(%s)" edvers

  (** Pretty prints a graph. For debug only. *)
  let pp fmt (t : t) =
    Format.fprintf fmt
      "Files to compile: [%a]@;External dependencies: [%a]@;Graph: %a@;"
      (pp_list ~sep:";" ~pp:Format.pp_print_string)
      t.mlang_generated
      (pp_list ~sep:";" ~pp:(fun fmt (v, _) -> Format.pp_print_string fmt v))
      t.ext_dep
      (pp_str_map ~sep:(",", "@,") ~pp:pp_file)
      t.graph

  (** The regexp that matches the following substrings:
      #include<str>
      #include"str"
      #include<str"
      #include"str>

      Why the last two? Because we will compile the C files eventually and invalid
      C intructions will be rejected, so why bother. TODO: make it better if you want. *)
  let magic_regexp = Str.regexp {|^.*#include \(<\|"\)\(.*\)\(>\|"\)|}

  (** Checks if a line is a C include. If so, returns the file included.
      Otherwise, returns [None]. *)
  let line_states_it_depends_on l =
    if Str.string_match magic_regexp l 0 then Some (Str.matched_group 2 l)
    else None

  (** Returns the list of dependencies of a given file. *)
  let file_states_it_depends_on f =
    let i = open_in f in
    let rec loop acc =
      match input_line i with
      | exception End_of_file ->
          close_in i;
          acc
      | l -> (
          match line_states_it_depends_on l with
          | None -> loop acc
          | Some f -> loop (f :: acc))
    in
    loop []

  (** Adds a file to the graph. The file must have been declared in either
      the field [mlang_generated] or the [ext_dep one]; otherwise, raises
      [MissingFileDeclaration].
      If it already belongs to the graph, does nothing. *)
  let rec add_file_to_graph ~cfiles_dir t filename =
    if StrMap.mem filename t.graph then (* Already treated *)
      t
    else if List.mem filename t.mlang_generated then
      (* File to compile: calculating its digest & dependencies. *)
      let cfile = full_file ~cfiles_dir filename in
      let hash = Digest.file cfile in
      let deps = file_states_it_depends_on cfile in
      let t =
        {
          t with
          graph =
            StrMap.add filename
              (Mlang_gen { mname = filename; mhash = hash; mdeps = deps })
              t.graph;
        }
      in
      (* Recursively adds its dependencies to the graph. *)
      List.fold_left (add_file_to_graph ~cfiles_dir) t deps
    else
      match List.find (fun f -> filename = fst f) t.ext_dep with
      | _, cmd ->
          (* This is an external dependency. Running the command version to add
             it to the graph. *)
          let edvers = run_command cmd in
          {
            t with
            graph =
              StrMap.add filename
                (Ext_dep { edname = filename; edvers })
                t.graph;
          }
      | exception Not_found ->
          (* File is neither a mlang file nor an external dependency. *)
          raise (MissingFileDeclaration filename)

  (** From a list of mlang files and external dependencies, returns the graph
      with all the mlang files and its dependencies.
      Fails with [MissingFileDeclaration] if an mlang file depends on a file that is
      neither in [mlang_generated] nor [ext_dep]. *)
  let build_graph ~cfiles_dir mlang_generated ext_dep =
    let empty_graph = { graph = StrMap.empty; mlang_generated; ext_dep } in
    List.fold_left (add_file_to_graph ~cfiles_dir) empty_graph mlang_generated

  (** Writes a (marshaled) graph. *)
  let write g =
    let fname = Filename.concat Env.output_dir Env.graph_filename in
    let out = open_out fname in
    Marshal.to_channel out g [ No_sharing ];
    close_out out

  (** Reads a graph serialized by [write]. In case of failure, returns
      an empty graph. *)
  let read () =
    try
      let c = open_in (Filename.concat Env.output_dir Env.graph_filename) in
      Marshal.from_channel c
    with Failure _ | Sys_error _ ->
      { graph = StrMap.empty; mlang_generated = []; ext_dep = [] }
end

(** Handles the configuration file.
    The configuration file syntax is the following:
    - "# Files to compile"
    - A list of files to compile (one by line)
    - "# External dependencies"
    - A list of pairs "file:command" where 'file' is the name of the external
      dependency as it would appear in the C file including it, and 'command' is
      a command returning the version of the file, which will be used to check
      if it changed between two compilations. *)
module Config = struct
  let mdeps_header = "# Files to compile"

  let ext_deps = "# External dependencies"

  (** The regexp for reading the external dependencies pairs. *)
  let ext_dep_regexp = Str.regexp {|^\(.*\):\(.*\)$|}

  (** Reads [config_file] and builds the depenency graph of the project. *)
  let read ~cfiles_dir ~config_file =
    let chan = open_in config_file in
    let rec empty_header () =
      match input_line chan with
      | "" -> empty_header ()
      | l ->
          if l <> mdeps_header then (
            Log.err "[Error] File should start with %s, not %S" mdeps_header l;
            raise (Failure "Config.read"))
    and mdeps acc =
      match input_line chan with
      | "" -> mdeps acc
      | l -> if l = ext_deps then acc else mdeps (l :: acc)
    and edeps acc =
      match input_line chan with
      | exception End_of_file -> acc
      | "" -> edeps acc
      | l ->
          if Str.string_match ext_dep_regexp l 0 then
            edeps ((Str.matched_group 1 l, Str.matched_group 2 l) :: acc)
          else (
            Log.err
              "[Error] Invalid external dependency line %s. Expected : \
               'filename':'command'"
              l;
            raise (Failure "Config.read"))
    in
    try
      empty_header ();
      let mdeps = mdeps [] in
      let edeps = edeps [] in
      close_in chan;
      DepGraph.build_graph ~cfiles_dir mdeps edeps
    with exn ->
      close_in chan;
      raise exn
end

(** Intermediary function;
    From an [old] dependency map corresponding to an old compilation, and
    a [new_] dependency map built from a configuration file, compiles a graph node
    (that should come from [new_]). The [compiled] map stores for each file basename
    a boolean stating the files depending on it will need to be recompiled ([true]) or
    do not need recompilation ([false]).
    If the node is an external dependency, checks if the version is the same than in
    [old]. If so, maps it in [compiled] to [false], otherwise to [true].
    If the node is an mlang generated file, compiles all its dependencies &
    checks if one needed to be recompiled: if so, maps it in [compiled] to [true],
    otherwise to [false]. *)
let rec compile_node_ ~cfiles_dir ~(old : DepGraph.t) ~(new_ : DepGraph.t)
    (compiled : bool StrMap.t) : DepGraph.file -> bool StrMap.t * bool =
  function
  | Ext_dep { edname; edvers } ->
      let should_recompile =
        match StrMap.find edname old.graph with
        | exception Not_found ->
            Log.err "[Warning] External dependency %S not found in old graph"
              edname;
            true
        | Mlang_gen _ ->
            Log.err
              "[Warning] External dependency %S defined as mlang file in old \
               graph"
              edname;
            true
        | Ext_dep { edvers = edvers'; _ } -> edvers <> edvers'
      in
      (compiled, should_recompile)
  | Mlang_gen { mname; mhash; mdeps } -> (
      let ofile = output_file_name mname in
      let compile () =
        let (_ : string) =
          compile_file ~cfile:(full_file ~cfiles_dir mname) ~ofile
        in
        (StrMap.add mname true compiled, true)
      in
      let dont_recompile () = (StrMap.add mname false compiled, false) in
      match StrMap.find mname compiled with
      | b -> (compiled, b)
      | exception Not_found -> (
          match StrMap.find mname old.graph with
          | Ext_dep _ | (exception Not_found) -> compile ()
          | Mlang_gen { mhash = mhash'; _ } when mhash <> mhash' -> compile ()
          | Mlang_gen _ ->
              (* Compiles dependencies *)
              let compiled, should_recompile =
                List.fold_left
                  (fun (set, should_recomp_acc) dep ->
                    let set, should_recomp =
                      compile_node_ ~cfiles_dir ~old ~new_ set
                        (StrMap.find dep new_.graph)
                    in
                    (set, should_recomp_acc || should_recomp))
                  (compiled, false) mdeps
              in
              if should_recompile || not (Sys.file_exists ofile) then compile ()
              else dont_recompile ()))

(** Compiles the mlang_generated files of a graph. *)
let compile_graph ~cfiles_dir ~old ~new_ =
  List.fold_left
    (fun compiled d ->
      let compiled, _ =
        compile_node_ ~cfiles_dir ~old ~new_ compiled (StrMap.find d new_.graph)
      in
      compiled)
    StrMap.empty new_.mlang_generated

(** Compiles the file specified in the [config_file]. *)
let compile ~cfiles_dir ~config_file =
  Log.log "Stating compilation...@.";
  let old = DepGraph.read () in
  Log.debug "Old graph: %a" DepGraph.pp old;
  let new_ = Config.read ~cfiles_dir ~config_file in
  Log.debug "New graph: %a" DepGraph.pp new_;
  let m : bool StrMap.t = compile_graph ~cfiles_dir ~old ~new_ in
  let newly_compiled =
    StrMap.fold (fun k b acc -> if b then k :: acc else acc) m []
  in
  if newly_compiled = [] then
    Log.log "Nothing changed. Not recompiling project.@."
  else (
    Log.log "Compilation over.@.";
    Log.log "Files compiled: %a@."
      (pp_list ~sep:", " ~pp:Format.pp_print_string)
      newly_compiled;
    DepGraph.write new_)

(** Checks the cfiles dir exists. Also, creates the output dir if it does not exist. *)
let init ~cfiles_dir =
  (* Checking existence of cfiles_dir *)
  let () =
    match Sys.is_directory cfiles_dir with
    | exception Sys_error _ ->
        Format.ksprintf failwith "Directory %S does not exist" cfiles_dir
    | true -> ()
    | false -> Format.ksprintf failwith "File %S is not a directory" cfiles_dir
  in
  (* Checking existence of output dir *)
  let () =
    match Sys.is_directory Env.output_dir with
    | exception Sys_error _ -> Sys.mkdir Env.output_dir 0x700
    | true -> ()
    | false ->
        Format.ksprintf failwith "File %S is not a directory" Env.output_dir
  in
  ()

let main () =
  match Array.to_list Sys.argv with
  | [] -> assert false
  | _ :: "--config" :: config_file :: "--files" :: cfiles_dir :: _
  | _ :: "--files" :: cfiles_dir :: "--config" :: config_file :: _ ->
      compile ~cfiles_dir ~config_file
  | _ ->
      Printf.printf "Syntaxe :\n%s --config cfiles_dir --files fichier_config\n"
        Sys.argv.(0);
      exit 31

let () = main ()
