type value_sort =
  | RegularFloat
  | MPFR of int (* bitsize of the floats *)
  | BigInt of int (* precision of the fixed point *)
  | Interval
  | Rational

type round_ops = RODefault | ROMulti | ROMainframe of int
(* size of type long, either 32 or 64 *)

type backend = Dgfip_c | UnknownBackend

type execution_mode =
  | SingleTest of string
  | MultipleTests of string
  | Extraction

type files = NonEmpty of string list

(* Flags inherited from the old compiler *)

let get_files = function NonEmpty l -> l

(* This feels weird to put here, but by construction it should not happen.*)
let source_files : files ref = ref (NonEmpty [])

let application_names : string list ref = ref []

let without_dgfip_m = ref false

let verify_flag = ref false

let debug_flag = ref false

let var_info_flag = ref false

let var_info_debug = ref []

let warning_flag = ref true

let no_print_cycles_flag = ref false

let display_time = ref false

let output_file = ref ""

let optimize_unsafe_float = ref false

let m_clean_calls = ref false

let value_sort = ref RegularFloat

let round_ops = ref RODefault

let backend = ref UnknownBackend

let dgfip_test_filter = ref false

let mpp_function = ref ""

let dgfip_flags = ref Dgfip_options.default_flags

let execution_mode = ref Extraction

(* Default value for the epsilon slack when comparing things in the
   interpreter *)
let comparison_error_margin = ref 0.000001

let income_year = ref 0

let set_all_arg_refs (files_ : files) applications_ (without_dgfip_m_ : bool)
    (debug_ : bool) (var_info_debug_ : string list) (display_time_ : bool)
    (no_print_cycles_ : bool) (output_file_ : string option)
    (optimize_unsafe_float_ : bool) (m_clean_calls_ : bool)
    (comparison_error_margin_ : float option) (income_year_ : int)
    (value_sort_ : value_sort) (round_ops_ : round_ops) (backend_ : backend)
    (dgfip_test_filter_ : bool) (mpp_function_ : string)
    (dgfip_flags_ : Dgfip_options.flags) (execution_mode_ : execution_mode) =
  source_files := files_;
  application_names := applications_;
  without_dgfip_m := without_dgfip_m_;
  debug_flag := debug_;
  var_info_debug := var_info_debug_;
  var_info_flag := !var_info_debug <> [];
  display_time := display_time_;
  no_print_cycles_flag := no_print_cycles_;
  optimize_unsafe_float := optimize_unsafe_float_;
  m_clean_calls := m_clean_calls_;
  execution_mode := execution_mode_;
  income_year := income_year_;
  value_sort := value_sort_;
  round_ops := round_ops_;
  backend := backend_;
  dgfip_test_filter := dgfip_test_filter_;
  mpp_function := mpp_function_;
  dgfip_flags := dgfip_flags_;
  match output_file_ with
  | None -> ()
  | Some o -> (
      output_file := o;
      match comparison_error_margin_ with
      | None -> ()
      | Some m -> comparison_error_margin := m)

let process_dgfip_options (backend : backend) ~(application_names : string list)
    (dgfip_options : string list option) =
  (* Parsing dgfip options even if we don't need them, because we may be in the case
     --dgfip_options=--help. *)
  let opts =
    Option.map
      (Dgfip_options.process_dgfip_options ~application_names)
      dgfip_options
  in
  match (backend, opts) with
  | Dgfip_c, None ->
      `Error "When using the DGFiP backend, DGFiP options MUST be provided."
  | Dgfip_c, Some (Ok (`Ok v)) -> `Dgfip_options v
  | UnknownBackend, None -> `Dgfip_options Dgfip_options.default_flags
  | UnknownBackend, Some (Ok (`Ok _)) ->
      (* warning_print "Backend unknown, discarding dgfip_options."; *)
      `Dgfip_options Dgfip_options.default_flags
  | _, Some (Ok `Help) | _, Some (Ok `Version) -> `Dgfip_options_version
  | _, Some (Error `Term) -> `Error "Invalid term in --dgfip_options"
  | _, Some (Error `Parse) -> `Error "Failed parsing of --dgfip_options"
  | _, Some (Error `Exn) ->
      `Error "Uncaught exception while reading --dgfip_options"

let set_opts ~(files : string list) ~(application_names : string list)
    ~(without_dgfip_m : bool) ~(debug : bool) ~(var_info_debug : string list)
    ~(display_time : bool) ~(print_cycles : bool) ~(backend : string option)
    ~(output : string option) ~(run_tests : string option)
    ~(dgfip_test_filter : bool) ~(run_test : string option)
    ~(mpp_function : string option) ~(optimize_unsafe_float : bool)
    ~(precision : string option) ~(roundops : string option)
    ~(comparison_error_margin : float option) ~(income_year : int)
    ~(m_clean_calls : bool) ~(dgfip_options : string list option) :
    [ `Run | `Displayed_dgfip_help | `Error of string ] =
  let exception INTERNAL_FAIL of string in
  let exception DGFIP_HELP in
  let err m = Format.kasprintf (fun s -> raise (INTERNAL_FAIL s)) m in
  try
    (* Reading backend first because we need it for parsing dgfip_flags *)
    let backend =
      match backend with Some "dgfip_c" -> Dgfip_c | _ -> UnknownBackend
    in
    let dgfip_flags =
      match process_dgfip_options backend ~application_names dgfip_options with
      | `Dgfip_options_help -> raise DGFIP_HELP
      | `Dgfip_options_version -> raise DGFIP_HELP
      | `Error m -> err "%s" m
      | `Dgfip_options f -> f
    in
    let mpp_function =
      match mpp_function with
      | None -> err "Option --mpp_function required"
      | Some m -> m
    in
    let value_sort =
      let precision = Option.get precision in
      if precision = "double" then RegularFloat
      else
        let mpfr_regex = Re.Pcre.regexp "^mpfr(\\d+)$" in
        if Re.Pcre.pmatch ~rex:mpfr_regex precision then
          let mpfr_prec =
            Re.Pcre.get_substring (Re.Pcre.exec ~rex:mpfr_regex precision) 1
          in
          MPFR (int_of_string mpfr_prec)
        else if precision = "interval" then Interval
        else
          let bigint_regex = Re.Pcre.regexp "^fixed(\\d+)$" in
          if Re.Pcre.pmatch ~rex:bigint_regex precision then
            let fixpoint_prec =
              Re.Pcre.get_substring (Re.Pcre.exec ~rex:bigint_regex precision) 1
            in
            BigInt (int_of_string fixpoint_prec)
          else if precision = "mpq" then Rational
          else err "Unkown precision option: %s" precision
    in
    let round_ops =
      match roundops with
      | Some "default" -> RODefault
      | Some "multi" -> ROMulti
      | Some roundops ->
          let mf_regex = Re.Pcre.regexp "^mainframe(\\d+)$" in
          if Re.Pcre.pmatch ~rex:mf_regex roundops then
            let mf_long_size =
              Re.Pcre.get_substring (Re.Pcre.exec ~rex:mf_regex roundops) 1
            in
            match int_of_string mf_long_size with
            | (32 | 64) as sz -> ROMainframe sz
            | _ -> err "Invalid long size for mainframe: %s" mf_long_size
          else err "Unknown roundops option: %s" roundops
      | None -> err "Unspecified roundops@."
    in
    let execution_mode =
      match (run_tests, run_test) with
      | Some s, _ -> MultipleTests s
      | None, Some s -> SingleTest s
      | None, None -> Extraction
    in
    let files =
      match List.length files with
      | 0 -> err "please provide at least one M source file"
      | _ -> NonEmpty files
    in
    set_all_arg_refs files application_names without_dgfip_m debug
      var_info_debug display_time print_cycles output optimize_unsafe_float
      m_clean_calls comparison_error_margin income_year value_sort round_ops
      backend dgfip_test_filter mpp_function dgfip_flags execution_mode;
    `Run
  with
  | INTERNAL_FAIL m -> `Error m
  | DGFIP_HELP -> `Displayed_dgfip_help
