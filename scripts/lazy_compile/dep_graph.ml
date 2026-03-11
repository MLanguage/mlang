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

(** The regexp that matches the following substrings: #include<str>
    #include"str" #include<str" #include"str>

    Why the last two? Because we will compile the C files eventually and
    invalid C intructions will be rejected, so why bother. TODO: make it
    better if you want. *)
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

(** Adds a file to the graph. The file must have been declared in either the
    field [mlang_generated] or the [ext_dep one]; otherwise, raises
    [MissingFileDeclaration]. If it already belongs to the graph, does
    nothing. *)
let rec add_file_to_graph ~cfiles_dir t filename =
  if StrMap.mem filename t.graph then (* Already treated *)
    t
  else if List.mem filename t.mlang_generated then
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

(** From a list of mlang files and external dependencies, returns the graph
    with all the mlang files and its dependencies. Fails with
    [MissingFileDeclaration] if an mlang file depends on a file that is
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

(** Reads a graph serialized by [write]. In case of failure, returns an empty
    graph. *)
let read () =
  try
    let c = open_in (Filename.concat Env.output_dir Env.graph_filename) in
    Marshal.from_channel c
  with Failure _ | Sys_error _ ->
                    { graph = StrMap.empty; mlang_generated = []; ext_dep = [] }
