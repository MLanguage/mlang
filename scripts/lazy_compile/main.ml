(** Usage:

    $ lazy_compile -F [FILEDIR] -C [CONFIGFILE]

    Can be configured with additional environment variables.
    - [OUTPUT_DIR]: the dir to write the .o files (default: output). Generated
      if it does not exist (default: output).
    - [DEPGRAPH_FILENAME]: the file in which is serialized the dependency graph.
      Written in [OUTPUT_DIR] (default: .depgraph).
    - [DEBUG]: displays debug messages (default: 0).
    - [PEDANTIC]: makes gcc pedantic (default: 1).
    - [CC]: the C compiler to use (default: gcc).

    How to compile:

    $ bash build.sh

    TODOs:
    - logs in files;
    - versioning depgraph files or stop using Marshal (that may deserialize
      something badly and make the script fail even badlier). *)

open Utils

(** Returns the name of the compilation output file. *)
let output_file_name cfile =
  Filename.concat Env.output_dir (Filename.chop_extension cfile ^ ".o")

(** Compiles [cfile]. *)
let compile_file ~cfiles_dir ~cfile ~ofile =
  let pedantic = if Env.pedantic = "0" then "" else "--pedantic " in
  let cmd =
    Format.sprintf "%s -std=c89 -I%s %s -O2 -c %s -o %s" Env.cc
      cfiles_dir pedantic cfile ofile
  in
  Log.log "Compiling file %S..." cfile;
  let res = run_command cmd in
  Log.log "%s" res;
  Log.log "Compilation of file %S complete -> %S" cfile ofile;
  res

(** Handles the configuration file. The configuration file syntax is the
    following:
    - "# External dependencies"
    - A list of pairs "file:command" where 'file' is the name of the external
      dependency as it would appear in the C file including it, and 'command' is
      a command returning the version of the file, which will be used to check
      if it changed between two compilations. *)
module Config = struct
  let header = "# External dependencies"

  (** The regexp for reading the external dependencies pairs. *)
  let ext_dep_regexp = Str.regexp {|^\(.*\):\(.*\)$|}

  (** Reads [config_file] and builds the external depenency list of the project.
  *)
  let read ~config_file =
    let chan = open_in config_file in
    let rec empty_header_then_deps () =
      match input_line chan with
      | "" -> empty_header_then_deps ()
      | l ->
          if l <> header then (
            Log.err "[Error] File should start with %s, not %S" header l;
            raise (Failure "Config.read"))
          else edeps []
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
      let edeps = empty_header_then_deps () in
      close_in chan;
      edeps
    with exn ->
      close_in chan;
      raise exn
end

let get_cfiles_of_dir cfiles_dir =
  let files = Sys.readdir cfiles_dir in
  Array.fold_left
    (fun acc f ->
      if f = "" then acc
      else
        match (Filename.extension f, f.[0]) with
        | (".c" | ".h"), ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') -> f :: acc
        | _ -> acc)
    [] files

(** Intermediary function; From an [old] dependency map corresponding to an old
    compilation, and a [new_] dependency map built from a configuration file,
    compiles a graph node (that should come from [new_]). The [compiled] map
    stores for each file basename a boolean stating the files depending on it
    will need to be recompiled ([true]) or do not need recompilation ([false]).
    If the node is an external dependency, checks if the version is the same
    than in [old]. If so, maps it in [compiled] to [false], otherwise to [true].
    If the node is an mlang generated file, compiles all its dependencies &
    checks if one needed to be recompiled: if so, maps it in [compiled] to
    [true], otherwise to [false]. *)
let rec compile_node_ ~cfiles_dir ~(old : Dep_graph.t) ~(new_ : Dep_graph.t)
    (compiled : bool StrMap.t) : Dep_graph.file -> bool StrMap.t * bool =
  function
  | Ext_dep { edname; edvers } ->
      let should_recompile =
        match StrMap.find edname old.graph with
        | exception Not_found ->
            Log.warn "External dependency %S not found in old graph" edname;
            true
        | Mlang_gen _ ->
            Log.warn "External dependency %S defined as mlang file in old graph"
              edname;
            true
        | Ext_dep { edvers = edvers'; _ } -> edvers <> edvers'
      in
      (compiled, should_recompile)
  | Mlang_gen { mname; mhash; mdeps } -> (
      Log.debug "Compiling mlang generated file %S" mname;
      Log.debug "Dependencies: %i" (List.length mdeps);
      let ofile = output_file_name mname in
      let compile () =
        let (_ : string) =
          compile_file ~cfiles_dir ~cfile:(Filename.concat cfiles_dir mname) ~ofile
        in
        (StrMap.add mname true compiled, true)
      in
      let dont_recompile () = (StrMap.add mname false compiled, false) in
      match StrMap.find mname compiled with
      | b -> (compiled, b)
      | exception Not_found -> (
          (* Compiles dependencies *)
          let compiled, should_recompile =
            List.fold_left
              (fun (set, should_recomp_acc) dep ->
                Log.debug "Compile dependency %S" dep;
                let set, should_recomp =
                  compile_node_ ~cfiles_dir ~old ~new_ set
                    (StrMap.find dep new_.graph)
                in
                (set, should_recomp_acc || should_recomp))
              (compiled, false) mdeps
          in
          (* TODO: recompile here *)
          match StrMap.find mname old.graph with
          | Ext_dep _ | (exception Not_found) -> compile ()
          | Mlang_gen { mhash = mhash'; _ }
            when mhash <> mhash' || should_recompile
                 || not (Sys.file_exists ofile) ->
              compile ()
          | Mlang_gen _ -> dont_recompile ()))

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
  Log.log "Starting compilation...";
  let old = Dep_graph.read () in
  Log.debug "Old graph: %a" Dep_graph.pp old;
  let new_ =
    let ext_deps = Config.read ~config_file
    and mlang_generated = get_cfiles_of_dir cfiles_dir in
    Dep_graph.build_graph ~cfiles_dir mlang_generated ext_deps
  in
  Log.debug "New graph: %a" Dep_graph.pp new_;
  let m : bool StrMap.t = compile_graph ~cfiles_dir ~old ~new_ in
  let newly_compiled =
    StrMap.fold (fun k b acc -> if b then k :: acc else acc) m []
  in
  if newly_compiled = [] then
    Log.log "Nothing changed. Not recompiling project."
  else (
    Log.log "Compilation over.";
    Log.log "Files compiled: %a"
      (pp_list ~sep:", " ~pp:Format.pp_print_string)
      newly_compiled;
    Dep_graph.write new_)

(** Checks the cfiles dir exists. Also, creates the output dir if it does not
    exist. *)
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
    | exception Sys_error _ -> Sys.mkdir Env.output_dir 0o777
    | true -> ()
    | false ->
        Format.ksprintf failwith "File %S is not a directory" Env.output_dir
  in
  ()

let main () =
  let () = Cli.read_args () in
  let cfiles_dir = Cli.cfiles_dir () and config_file = Cli.config_file () in
  init ~cfiles_dir;
  compile ~cfiles_dir ~config_file

let () = main ()
