open Utils

type file_assoc = string * string

type filemap = file_assoc list

type callbacks = {
  start_parsing : string -> unit Lwt.t;
  end_parsing : string -> unit Lwt.t;
}

let parse_file callbacks (name, contents) =
  let%lwt () = callbacks.start_parsing name in
  print_string "parsing ";
  print_endline name;
  try
    let filebuf = Lexing.from_string contents in
    print_endline "after lexing.";
    let parsed = Mlang.Parsing.parse_lexbuf filebuf name in
    let%lwt () = callbacks.end_parsing name in
    Lwt.return parsed
  with Errors.StructuredError (msg, pos_list, _kont) as _e ->
    Cli.error_print "%a" Errors.format_structured_error (msg, pos_list);
    Lwt.return []

let parse_files callbacks flat_filemap =
  let t = Lwt_list.map_s (parse_file callbacks) flat_filemap in
  t

let run callbacks flat_filemap irj_contents target application =
  Config.mpp_function := target;
  Config.application_names := [ application ];
  Config.plain_output := true;
  let filemap =
    List.fold_left
      (fun map (name, contents) -> StrMap.add name contents map)
      StrMap.empty flat_filemap
  in
  Config.platform := Server filemap;
  let dgfip_m = Mlang.Parsing.parse_m_dgfip (fun _ -> ()) [] in
  let%lwt m_program = parse_files callbacks flat_filemap in
  let m_program =
    dgfip_m @ m_program
    |> Mlang.Parsing.patch_rule_1 !Config.backend !Config.dgfip_flags
  in
  let m_program = M_frontend.Expander.proceed m_program in
  print_endline "proceeding";
  let m_program = M_frontend.Validator.proceed !Config.mpp_function m_program in
  print_endline "translating";
  let m_program = M_frontend.Mast_to_mir.translate m_program in
  print_endline "expanding functions";
  let m_program = M_ir.Mir.expand_functions m_program in
  print_endline "before runnning";
  let dbg_infos =
    Irj_utils.Test_interpreter.check_test m_program (Contents irj_contents)
      !Config.value_sort !Config.round_ops
  in
  let buf = Buffer.create 10000 in
  let fmt = Format.formatter_of_buffer buf in
  let delim = ref "" in
  Buffer.add_char buf '[';
  dbg_infos
  |> List.iter (fun Irj_utils.Test_interpreter.{ target; dbg_info } ->
         Buffer.add_string buf !delim;
         delim := ",";
         Format.fprintf fmt {|{"target": "%s", "dbg_info": |} target;
         M_ir.Dbg_info.to_json fmt dbg_info;
         Buffer.add_string buf "}");
  Buffer.add_char buf ']';
  Lwt.return @@ (Buffer.to_bytes buf |> Bytes.to_string)
