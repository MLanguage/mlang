open M_frontend
open Lexing
open Mlexer
open M_ir

(* The legacy compiler plays a nasty trick on us, that we have to reproduce:
   rule 1 is modified to add assignments to APPLI_XXX variables according to the
   target application (OCEANS, BATCH and ILIAD). *)
let patch_rule_1 (backend : Config.backend)
    (dgfip_flags : Config.Dgfip_options.flags) (program : Mast.program) :
    Mast.program =
  let open Mast in
  let var_exists name =
    List.exists
      (List.exists (fun m_item ->
           match Pos.unmark m_item with
           | VariableDecl (ComputedVar m_cv) ->
               Pos.unmark (Pos.unmark m_cv).comp_name = name
           | VariableDecl (InputVar m_iv) ->
               Pos.unmark (Pos.unmark m_iv).input_name = name
           | _ -> false))
      program
  in
  let mk_assign name value l =
    if var_exists name then
      let m_access =
        Pos.without (Com.VarAccess (None, Pos.without (Com.Normal name)))
      in
      let litt = Com.mk_lit (Com.Float (if value then 1.0 else 0.0)) in
      let cmd = Com.SingleFormula (VarDecl (m_access, Pos.without litt)) in
      Pos.without cmd :: l
    else l
  in
  let oceans, batch, iliad =
    match backend with
    | Dgfip_c ->
        (dgfip_flags.flg_cfir, dgfip_flags.flg_gcos, dgfip_flags.flg_iliad)
    | UnknownBackend -> (false, false, true)
  in
  List.map
    (List.map (fun m_item ->
         match Pos.unmark m_item with
         | Rule r when Pos.unmark r.rule_number = 1 ->
             let fl =
               List.map
                 (fun f -> Pos.same (Com.Affectation f) f)
                 ([]
                 |> mk_assign "APPLI_OCEANS" oceans
                 |> mk_assign "APPLI_BATCH" batch
                 |> mk_assign "APPLI_ILIAD" iliad)
             in
             let r' = { r with rule_formulaes = r.rule_formulaes @ fl } in
             Pos.same (Rule r') m_item
         | _ -> m_item))
    program

(** Entry function for the executable. Returns a negative number in case of
    error. *)
let parse_lexbuf filebuf source_file =
  let lex_curr_p = { filebuf.lex_curr_p with pos_fname = source_file } in
  let filebuf = { filebuf with lex_curr_p } in
  match Mparser.source_file token filebuf with
  | commands -> commands
  | exception Mparser.Error ->
      let loc =
        Parse_utils.make_loc (filebuf.lex_start_p, filebuf.lex_curr_p)
      in
      Errors.raise_spanned_error "M syntax error" (Parse_utils.mk_position loc)

let parse_file source_file =
  let input = open_in source_file in
  let filebuf = Lexing.from_channel input in
  try
    parse_lexbuf filebuf source_file
    (* We're catching exceptions to properly close the input channel *)
  with Errors.StructuredError _ as e ->
    close_in input;
    raise e

let parse_m_dgfip current_progress m_program =
  let parse_internal str =
    let filebuf = Lexing.from_string str in
    let source_file = Dgfip_m.internal_m in
    current_progress source_file;
    parse_lexbuf filebuf source_file
  in
  let decs = parse_internal Dgfip_m.declarations in
  let events = parse_internal Dgfip_m.event_declaration in
  events :: decs :: m_program

let parse_m_files files current_progress m_program =
  let parse_file_progress source_file =
    current_progress source_file;
    parse_file source_file
  in
  (*FIXME: use a fold here *)
  let prog = List.map parse_file_progress files in
  List.rev prog @ m_program

let parse files progress_bar =
  let current_progress, finish = progress_bar in
  let m_program =
    []
    |> parse_m_dgfip current_progress
    |> parse_m_files files current_progress
    |> List.rev
    |> patch_rule_1 !Config.backend !Config.dgfip_flags
  in
  finish "completed!";
  m_program
