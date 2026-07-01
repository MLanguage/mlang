(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2024 - 2026 DGFiP - INRIA                               *)
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

(** The Irj_checker Module is a simple entry point to use the Mlang IRJ file
    parser in order to perform syntactic checks on test files or produce other
    IR test formats.

    Usage: irj_checker.exe [--message-format=VAL] <test_file.irj>
    [transformation-target]*)

open Cmdliner
open Irj_utils
open Utils

type validation_mode_enum = Strict | Corrective | Primitive

type transformation_target = None | PasCalcP | PasCalcC

let gen_file generator test_data =
  let mode = 'v' in
  (* use h for a monoline json *)
  let out_fmt = Format.std_formatter in
  generator out_fmt test_data mode;
  Format.pp_print_newline out_fmt ();
  Format.pp_print_flush out_fmt ()

let irj_check_file (f : string) (validation_mode : validation_mode_enum)
    (transform_target : transformation_target) : unit =
  try
    let test_data = Irj_file.parse_input (Filename f) in
    let test_data =
      match validation_mode with
      | Primitive ->
          if Option.is_some test_data.rapp then
            Errors.raise_error
              (Format.asprintf "%s: is a corrective file!" test_data.nom)
          else test_data
      | Corrective ->
          if Option.is_none test_data.rapp then
            Errors.raise_error
              (Format.asprintf "%s: is a primitive file!" test_data.nom)
          else test_data
      | _ -> test_data
    in
    match transform_target with
    | None ->
        Ppf.result_print "%s: checked as %s with %d primitive codes!"
          test_data.nom
          (match test_data.rapp with
          | Some _ -> "corrective"
          | None -> "primitive")
          (List.length test_data.prim.entrees)
    | PasCalcP -> gen_file Pas_calc.gen_pas_calc_json_primitif test_data.prim
    | PasCalcC -> gen_file Pas_calc.gen_pas_calc_json_correctif test_data
  with Errors.StructuredError (msg, kont) ->
    Ppf.error_print "There has been an error in %S: %a" f
      Ppf.format_structured_message msg;
    (match kont with None -> () | Some kont -> kont ());
    exit 123

let rec irj_checker (f : string) (validation_mode : validation_mode_enum)
    (transform_target : transformation_target) : unit =
  if not (Sys.file_exists f) then (
    Ppf.error_print "%s: this path is not a valid file in the filesystem" f;
    exit 124);
  if Sys.is_directory f then
    Array.iter
      (fun sub ->
        irj_checker (Filename.concat f sub) validation_mode transform_target)
      (Sys.readdir f)
  else irj_check_file f validation_mode transform_target

let irj_checker (f : string) (message_format : Config.message_format)
    (validation_mode : validation_mode_enum)
    (transform_target : transformation_target) : unit =
  Config.message_format := message_format;
  irj_checker f validation_mode transform_target

let validation_mode_opt =
  [ ("strict", Strict); ("corrective", Corrective); ("primitive", Primitive) ]

let validation_mode =
  Arg.(
    value
    & opt (enum validation_mode_opt) Strict
    & info [ "v"; "validation-mode" ]
        ~doc:
          "Select the validation criteria. If set to $(i,strict), the whole \
           grammar is applied. If set to $(i,corrective) or $(i,primitive), \
           only the corresponding files are accepted, for instance primitive \
           file in corrective mode will raise an error.")

let message_format_opt = [ ("human", Config.ANSI); ("gnu", GNU) ]

let message_format =
  Arg.(
    value
    & opt (enum message_format_opt) Config.ANSI
    & info [ "m"; "message-format" ]
        ~doc:
          "Selects the format of error and warning messages emitted by the \
           compiler. If set to $(i,human), the messages will be nicely \
           displayed and meant to be read by a human. If set to $(i,gnu), the \
           messages will be rendered according to the GNU coding standards.")

let file =
  let doc = "Test file (usually with the .irj extension)" in
  Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc)

let transformation_target_opt =
  [ ("none", None); ("pasp", PasCalcP); ("pasc", PasCalcC) ]

let transform_target =
  let doc =
    "Transformation target, among the following list: $(i,none) (only checks \
     test syntax), $(i,pasp) (API PAS-CALC for primitive computation \
     resources), $(i,pasc) (API PAS-CALC for corrective computation \
     resources)."
  in
  Arg.(
    value
    & pos 1 (enum transformation_target_opt) None
    & info [] ~docv:"TARGET" ~doc)

let irj_checker_t =
  Term.(
    const irj_checker $ file $ message_format $ validation_mode
    $ transform_target)

let cmd =
  let doc = "parses, validates and transforms IRJ test files" in
  let man =
    [
      `S Manpage.s_bugs;
      `P "File bug reports at <https://github.com/MLanguage/mlang/issues>.";
    ]
  in
  Cmd.v
    (Cmd.info "irj_checker"
       ~version:
         (match Build_info.V1.version () with
         | None -> "n/a"
         | Some v -> Build_info.V1.Version.to_string v)
       ~doc ~man)
    irj_checker_t

let () = exit (Cmd.eval cmd)
