open Utils

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
    cfiles : string list;
    (* The list of files to compile *)
    ext_dep : (string * string) list; (* name * command to get version *)
  }

exception MissingFileDeclaration of string

let empty = 
  { graph = StrMap.empty; cfiles = []; ext_dep = [] }

(** The regexp that matches the following substrings: #include<str>
    #include"str" #include<str" #include"str>

    Why the last two? Because we will compile the C files eventually and
    invalid C intructions will be rejected, so why bother. TODO: make it
    better if you want. *)
let magic_regexp = Str.regexp {|^.*#include \(<\|"\)\(.*\)\(>\|"\)|}

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
    t.cfiles
    (pp_list ~sep:";" ~pp:(fun fmt (v, _) -> Format.pp_print_string fmt v))
    t.ext_dep
    (pp_str_map ~sep:(",", "@,") ~pp:pp_file)
    t.graph

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

(** Adds a file to the graph. The file must have been declared in either the
    field [mlang_generated] or the [ext_dep one]; otherwise, raises
    [MissingFileDeclaration]. If it already belongs to the graph, does
    nothing. *)
let rec add_file_to_graph ~cfiles_dir t filename =
  if StrMap.mem filename t.graph then (* Already treated *)
    t
  else if List.mem filename t.cfiles then
    (* File to compile: calculating its digest & dependencies. *)
    let cfile = Filename.concat cfiles_dir filename in
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
  Log.log "Compilation of file %S complete-> %S" cfile ofile;
  res

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
let rec compile_node_ ~cfiles_dir ~(old : t) ~(new_ : t)
    (compiled : bool StrMap.t) : file -> bool StrMap.t * bool =
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
let compile ~cfiles_dir ~old ~new_ =
  List.fold_left
    (fun compiled d ->
      let compiled, _ =
        compile_node_ ~cfiles_dir ~old ~new_ compiled (StrMap.find d new_.graph)
      in
      compiled)
    StrMap.empty new_.cfiles

(** From a list of mlang files and external dependencies, returns the graph
    with all the mlang files and its dependencies. Fails with
    [MissingFileDeclaration] if an mlang file depends on a file that is
    neither in [files_of_dir] nor [ext_dep]. *)
let make ~cfiles_dir ~ext_dep =
  let cfiles = get_cfiles_of_dir cfiles_dir in
  let empty_graph = { graph = StrMap.empty; cfiles; ext_dep } in
  List.fold_left (add_file_to_graph ~cfiles_dir) empty_graph cfiles

(* -- Graph serialization -- *)

let lazy_compile_version () = Digest.file Sys.argv.(0)

(** Writes a (marshaled) graph. *)
let write (g : t) =
  let fname = Filename.concat Env.output_dir Env.graph_filename
  and lc_version = lazy_compile_version () in
  let out = open_out fname in
  try
    Marshal.to_channel out (lc_version, g) [ No_sharing ];
    close_out out
  with
  | exn ->
     Log.err "Failing to write graph in %S: %s." fname (Printexc.to_string exn);
     close_out out;
     Sys.remove fname;
     raise exn

(** Reads a graph serialized by [write]. In case of failure, returns an empty
    graph. *)
let read () =
  try
    let c = open_in (Filename.concat Env.output_dir Env.graph_filename) in
    let (lcversion, g) = Marshal.from_channel c in
    if lcversion = lazy_compile_version () then
      g
    else begin
      Log.warn "Newer version of lazy compile: ignoring old data.";
      empty
    end
  with Failure _ | Sys_error _ | End_of_file -> empty
