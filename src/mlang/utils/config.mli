(** According on the [value_sort], a specific interpreter will be called with
    the right kind of floating-point value *)
type value_sort =
  | RegularFloat
  | MPFR of int  (** bitsize of the floats *)
  | BigInt of int  (** precision of the fixed point *)
  | Interval
  | Rational

(** Rounding operations to use in the interpreter. They correspond to the
    rounding operations used by the DGFiP calculator in different execution
    contexts.
    
    - RODefault: rounding operations used in the PC/single-thread context
    - ROMulti: rouding operations used in the PC/multi-thread context
    - ROMainframe rounding operations used in the mainframe context *)
type round_ops =
  | RODefault
  | ROMulti
  | ROMainframe of int  (** size of type long, either 32 or 64 *)

type backend = Dgfip_c | UnknownBackend

type execution_mode =
  | SingleTest of string
  | MultipleTests of string
  | Extraction

type files = NonEmpty of string list

val get_files : files -> string list

val source_files : files ref
(** M source files to be compiled *)

val application_names : string list ref

val without_dgfip_m : bool ref

val verify_flag : bool ref
(** Use Z3 to check if verif rules hold all the time *)

val debug_flag : bool ref
(** Prints debug information *)

val var_info_flag : bool ref
(** Print infomation about variables declared, defined ou used incorrectly *)

val var_info_debug : string list ref
(** Prints even more information but only about some variables members of a
    list *)

val warning_flag : bool ref
(** Print warning info *)

val no_print_cycles_flag : bool ref
(** Dump circular definitions of variables *)

val display_time : bool ref
(** Displays timing information *)

val output_file : string ref
(** Output file *)

val optimize_unsafe_float : bool ref
(** Activate unsafe floating point optimizations *)

val m_clean_calls : bool ref
(** Clean regular variables between M calls *)

val comparison_error_margin : float ref

val income_year : int ref

val value_sort : value_sort ref

val round_ops : round_ops ref

val backend : backend ref

val dgfip_test_filter : bool ref

val mpp_function : string ref

val dgfip_flags : Dgfip_options.flags ref

val execution_mode : execution_mode ref

val no_nondet_display : bool ref

val set_opts :
  files:string list ->
  application_names:string list ->
  without_dgfip_m:bool ->
  debug:bool ->
  var_info_debug:string list ->
  display_time:bool ->
  print_cycles:bool ->
  backend:string option ->
  output:string option ->
  run_tests:string option ->
  dgfip_test_filter:bool ->
  run_test:string option ->
  mpp_function:string option ->
  optimize_unsafe_float:bool ->
  precision:string option ->
  roundops:string option ->
  comparison_error_margin:float option ->
  income_year:int ->
  m_clean_calls:bool ->
  dgfip_options:string list option ->
  no_nondet_display:bool ->
  [ `Displayed_dgfip_help | `Error of string | `Run ]
