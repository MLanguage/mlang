(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2019 - 2026 DGFiP - INRIA                               *)
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

(** Command-line interface helpers *)

(**{1 Command line arguments}*)

(**{2 Argument parsing}*)

(** The command line interface is declared using {!module Cmdliner} *)

open Cmdliner
open Config
module Cmdliner = Cmdliner
module Term = Cmdliner.Term
module ANSITerminal = ANSITerminal

let files =
  Arg.(
    value & pos_all file []
    & info [] ~docv:"FILES" ~doc:"M files to be compiled")

let applications =
  Arg.(value & opt (list string) [] & info [ "A" ] ~doc:"Application name(s)")

let without_dgfip_m =
  Arg.(
    value & flag
    & info [ "without_dfgip_m" ]
        ~doc:"Don't parse M definitions of DGFiP idiosyncratic datas")

let debug =
  Arg.(value & flag & info [ "debug"; "d" ] ~doc:"Prints debug information")

let var_info_debug =
  Arg.(
    value & opt_all string []
    & info [ "var_info_debug" ]
        ~doc:"Prints debug information for variables passed as arguments")

let display_time =
  Arg.(
    value & flag
    & info [ "display_time"; "t" ]
        ~doc:"Displays timing information (use with --debug)")

let no_print_cycles =
  let doc = "If set, disable the eventual circular dependencies repport" in
  Arg.(value & flag & info [ "no_print_cycles"; "c" ] ~doc)

let optimize_unsafe_float =
  let doc =
    "Activate unsafe floating point optimizations (such as x * 0 ~> 0)"
  in
  Arg.(value & flag & info [ "fast-math" ] ~doc)

let backend =
  Arg.(
    value
    & opt (some string) None
    & info [ "backend"; "b" ] ~docv:"BACKEND"
        ~doc:"Backend selection: interpreter, Python, C, dgfip_c")

let mpp_function =
  Arg.(
    value
    & opt (some string) None
    & info [ "mpp_function" ] ~docv:"MPP_FUNCTION" ~doc:"M++ file main function")

let output =
  Arg.(
    value
    & opt (some string) None
    & info [ "output"; "o" ] ~docv:"OUTPUT"
        ~doc:
          "$(i, OUTPUT) is the file that will contain the extracted function \
           (for compiler backends)")

let run_all_tests =
  Arg.(
    value
    & opt (some file) None
    & info [ "run_all_tests"; "R" ] ~docv:"TESTS"
        ~doc:"Run all tests in folder specified folder")

let dgfip_test_filter =
  let doc =
    "Filter test files on filename: keep only tests beginning by an uppercase \
     character (use with --run-al-tests)"
  in
  Arg.(value & flag & info [ "dgfip_test_filter" ] ~doc)

let run_test =
  Arg.(
    value
    & opt (some file) None
    & info [ "run_test"; "r" ] ~docv:"TESTS"
        ~doc:"Run specific test passed as argument")

let precision =
  Arg.(
    value
    & opt (some string) (Some "double")
    & info [ "precision"; "p" ] ~docv:"PRECISION"
        ~doc:
          "Precision of the interpreter: double, mpfr<n> (where n > 0 it the \
           bit size of the multi-precision floats), fixed<n> (where n > 0 is \
           the fixpoint precision), interval (64-bits IEEE754 floats, with up \
           and down rounding mode), mpq (multi-precision rationals) . Default \
           is double")

let roundops =
  Arg.(
    value
    & opt (some string) (Some "default")
    & info [ "roundops" ] ~docv:"ROUNDOPS"
        ~doc:
          "Rounding operations to use in the interpreter: default, multi, \
           mainframe<n> (where n is the size in bits of the long type to \
           simulate). Each corresponds to the behavior of the legacy DGFiP \
           code in different environments: default when running on a regular \
           PC, multi when running in a multithread context, and mainframe when \
           running on a mainframe. In this case, the size of the long type has \
           to be specified; it can be either 32 or 64.")

let plain_output =
  Arg.(
    value & flag
    & info [ "plain_output" ] ~doc:"Do not print terminal characters.")

let comparison_error_margin_cli =
  Arg.(
    value
    & opt (some float) None
    & info
        [ "comparison_error_margin" ]
        ~docv:"COMP_ERROR_MARGIN"
        ~doc:
          "When executing comparisons between numbers, the M semantics allows \
           a little bit of slack to account for imprecise number \
           representation. This number defines how much slack is allowed and \
           in which comparisons return true even if they should return false. \
           This slack also affects the rounding operations by tweaking results \
           around .5. This option defaults to 10^(-6).")

let income_year_cli =
  Arg.(
    value
    & opt int (1900 + (Unix.localtime (Unix.time ())).Unix.tm_year - 1)
    & info [ "income-year" ] ~docv:"INCOME_YEAR"
        ~doc:"Set the year of the income.")

let m_clean_calls =
  Arg.(
    value & flag
    & info
        [ "clean_between_m_calls" ]
        ~doc:
          "Clean the value of computed variables between two m calls (to check \
           that there is no hidden state kept between two calls)")

let dgfip_options =
  Arg.(
    value
    & opt (some (list string)) None
    & info [ "dgfip_options" ]
        ~doc:
          "Specify DGFiP options (use --dgfip_options=--help to display DGFiP \
           specific options)")

let no_nondet_display =
  Arg.(
    value & flag
    & info [ "no_nondet_display" ] ~docv:""
        ~doc:
          "Hides all non deterministic displays (display time, progress bar). \
           Used for cram tests.")

let trace =
  Arg.(
    value & flag
    & info [ "trace" ]
        ~doc:"Controls whether the interpreter traces the execution")

let trace_output_file =
  Arg.(
    value
    & opt (some string) None
    & info [ "trace_output_file" ] ~doc:"The file where the trace is stored.")

let message_format_opt =
  [
    ("h", Config.ANSI);
    ("human", Config.ANSI);
    ("a", Config.ANSI);
    ("ansi", Config.ANSI);
    ("g", GNU);
    ("gnu", GNU);
  ]

let message_format =
  Arg.(
    value
    & opt (enum message_format_opt) Config.ANSI
    & info [ "message_format" ] ~docv:""
        ~doc:"Selects the message format: human/GNU")

let optim_flags =
  [
    ("lvfa", Config.Local_vars_for_arrays);
    ("ncur", No_check_unstoppable_rules);
    ("nrbf", No_redundant_boolean_formulae);
    ("sbo", Simple_binary_op);
    ("*", All_optims);
  ]

let optims =
  Arg.(
    value
    & opt (list @@ enum optim_flags) [ All_optims ]
    & info [ "optim"; "O" ] ~docv:"OPTIM"
        ~doc:
          "Several optimizations modify the generated C program to make it \
           more than a simple translation. Here are the different available \
           optimsizations: \n\
           - 'lvfa': each rule defines local variables for the irdata accesses \
           of the TGV;\n\
           - 'ncur': when a rule is not stopped (because it does not have a \
           stop instruction), does not add the subsequent check;\n\
           - 'nrbf': removes redundant boolean formulae from OR and AND \
           operators (when checking for variable definitions);\n\
           - 'sbo': replaces binary operators '&&' and '||' by '&' and '|'\n\
           - '*': all of the above (default).")

let test_var_defs =
  let open Arg in
  let litteral =
    let parser s =
      let litteral_of_string = function
        | "defaut" -> None
        | "indefini" -> Some None
        | sf -> Some (Some (float_of_string sf))
      in
      let err s =
        Pp.spr
          "@[invalid litteral \'%s\', expected \'defaut\', \'indefini\' or a \
           float@]"
          s
      in
      try Ok (litteral_of_string s) with Failure _ -> Error (err s)
    in
    let pp fmt = function
      | None -> Format.pp_print_string fmt "default"
      | Some None -> Format.pp_print_string fmt "indefini"
      | Some (Some f) -> Format.pp_print_float fmt f
    in
    conv' ~docv:"LITTERAL" (parser, pp)
  in
  value
  & opt_all (pair ~sep:'=' string litteral) []
  & info [ "def"; "D" ] ~doc:"Initialise variables with values"

let mlang_t f =
  Term.(
    const f $ files $ applications $ without_dgfip_m $ debug $ var_info_debug
    $ display_time $ no_print_cycles $ backend $ output $ run_all_tests
    $ dgfip_test_filter $ run_test $ mpp_function $ optimize_unsafe_float
    $ precision $ roundops $ comparison_error_margin_cli $ income_year_cli
    $ m_clean_calls $ dgfip_options $ no_nondet_display $ plain_output $ trace
    $ trace_output_file $ message_format $ optims $ test_var_defs)

let info =
  let doc =
    "Intepreter and compiler for M, the language created by the French \
     Direction Generale des Finances Publiques (DGFiP)."
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "The M language is used by the DGFiP to encode the rules describing \
         the computation of the French income tax. An M program consists in \
         several *.m files in no particular order. $(tname) will parse all the \
         rules contained in those files that correspond to a particular \
         application tag. Then, it will extract from this set of rules an \
         user-specified function, than can be interpreted with a command-line \
         prompt or compiled to a function in the language of your choice.";
      `S Manpage.s_authors;
      `P "Denis Merigoux <denis.merigoux@inria.fr>";
      `P "Raphael Monat <raphael.monat@lip6.fr>";
      `S Manpage.s_examples;
      `P "Typical usage:";
      `Pre
        "mlang -a iliad -f query.m_spec -b interpreter \
         ir-calcul/sources2018m_6_3/*.m";
      `S Manpage.s_bugs;
      `P "Please file bug reports at https://github.com/MLanguage/mlang/issues";
    ]
  in
  let exits =
    Cmd.Exit.defaults
    @ [
        Cmd.Exit.info ~doc:"on M parsing error." 1;
        Cmd.Exit.info ~doc:"on M typechecking error." 2;
      ]
  in
  Cmd.info "mlang"
    ~version:
      (match Build_info.V1.version () with
      | None -> "n/a"
      | Some v -> Build_info.V1.Version.to_string v)
    ~doc ~exits ~man

(**{1 Terminal formatting}*)

let concat_with_line_depending_prefix_and_suffix (prefix : int -> string)
    (suffix : int -> string) (ss : string list) =
  match ss with
  | hd :: rest ->
      let out, _ =
        List.fold_left
          (fun (acc, i) s ->
            ( (acc ^ prefix i ^ s
              ^ if i = List.length ss - 1 then "" else suffix i),
              i + 1 ))
          ((prefix 0 ^ hd ^ if 0 = List.length ss - 1 then "" else suffix 0), 1)
          rest
      in
      out
  | [] -> prefix 0

let add_prefix_to_each_line (s : string) (prefix : int -> string) =
  concat_with_line_depending_prefix_and_suffix
    (fun i -> prefix i)
    (fun _ -> "\n")
    (String.split_on_char '\n' s)

let indent_number (s : string) : int =
  try
    let rec aux (i : int) = if s.[i] = ' ' then aux (i + 1) else i in
    aux 0
  with Invalid_argument _ -> String.length s

let format_with_style (styles : ANSITerminal.style list)
    (str : ('a, unit, string) format) =
  if !Config.plain_output (* can depend on a stylr flag *) then
    Printf.sprintf str
  else ANSITerminal.sprintf styles str

let format_matched_line pos (line : string) (line_no : int) : string =
  let line_indent = indent_number line in
  let error_indicator_style = [ ANSITerminal.red; ANSITerminal.Bold ] in
  let sline = Pos.get_start_line pos in
  let eline = Pos.get_end_line pos in
  let line_start_col =
    if line_no = sline then Pos.get_start_column pos else 1
  in
  let line_end_col =
    if line_no = eline then Pos.get_end_column pos else String.length line + 1
  in
  let line_length = String.length line + 1 in
  line
  ^
  if line_no >= sline && line_no <= eline then
    "\n"
    ^
    if line_no = sline && line_no = eline then
      format_with_style error_indicator_style "%*s" (line_end_col - 1)
        (String.make (line_end_col - line_start_col) '^')
    else if line_no = sline && line_no <> eline then
      format_with_style error_indicator_style "%*s" (line_length - 1)
        (String.make (line_length - line_start_col) '^')
    else if line_no <> sline && line_no <> eline then
      format_with_style error_indicator_style "%*s%s" line_indent ""
        (String.make (line_length - line_indent) '^')
    else if line_no <> sline && line_no = eline then
      format_with_style error_indicator_style "%*s%*s" line_indent ""
        (line_end_col - 1 - line_indent)
        (String.make (line_end_col - line_indent) '^')
    else assert false (* should not happen *)
  else ""

let format_lines pos lines =
  let filename = Pos.get_file pos in
  let sline = Pos.get_start_line pos in
  let eline = Pos.get_end_line pos in
  let blue_style = [ ANSITerminal.Bold; ANSITerminal.blue ] in
  let spaces = int_of_float (log10 (float_of_int eline)) + 1 in
  let lines =
    List.mapi (fun i line -> format_matched_line pos line (i + sline)) lines
  in
  format_with_style blue_style "%*s--> %s\n%s" spaces "" filename
    (add_prefix_to_each_line
       (Printf.sprintf "\n%s" (String.concat "\n" lines))
       (fun i ->
         let cur_line = sline + i - 1 in
         if
           cur_line >= sline
           && cur_line <= sline + (2 * (eline - sline))
           && cur_line mod 2 = sline mod 2
         then
           format_with_style blue_style "%*d | " spaces
             (sline + ((cur_line - sline) / 2))
         else if cur_line >= sline && cur_line < sline then
           format_with_style blue_style "%*d | " spaces cur_line
         else if
           cur_line <= sline + (2 * (eline - sline)) + 1
           && cur_line > sline + (2 * (eline - sline)) + 1
         then
           format_with_style blue_style "%*d | " spaces
             (cur_line - (eline - sline + 1))
         else format_with_style blue_style "%*s | " spaces ""))

let retrieve_loc_text (pos : Pos.t) : string =
  let filename = Pos.get_file pos in
  if filename = "" then "No position information"
  else
    let lines =
      match !Config.filesystem with
      | Contents filemap -> begin
          match StrMap.find_opt filename filemap with
          | None -> failwith "Pos error"
          | Some contents ->
              let lines = String.split_on_char '\n' contents in
              [ List.nth lines (Pos.get_start_line pos - 1) ]
        end
      | Local ->
          let get_lines =
            match File.open_file_for_text_extraction pos with
            | exception Sys_error _ ->
                Format.ksprintf failwith
                  "File not found for displaying position : %S" filename
            | get_lines -> get_lines
          in
          get_lines 1
    in
    format_lines pos lines
