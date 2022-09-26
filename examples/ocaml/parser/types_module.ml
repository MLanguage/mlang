(*From utils/pos.ml*)

type t = { pos_filename : string; pos_loc : Lexing.position * Lexing.position }
(** A position in the source code is a file, as well as begin and end location
    of the form col:line *)

let make_position (f : string) (loc : Lexing.position * Lexing.position) =
      { pos_filename = f; pos_loc = loc }


(*From test_ast.ml*)

type literal = I of int | F of float

type var_values = (string * literal * t) list

type test_file = {
  nom : string;
  ep : var_values;
  cp : var_values;
  rp : var_values;
  corr : (var_values * var_values * var_values) option;
}

(*For both lexer and parser, from m_frontend/parse_utils*)
let mk_position sloc = make_position (fst sloc).Lexing.pos_fname sloc
(*For lexer, from utils/errors.ml*)

exception
  StructuredError of
    (string * (string option * t) list * (unit -> unit) option)

let raise_spanned_error (msg : string) ?(span_msg : string option)
    (span : t) : 'a =
  raise (StructuredError (msg, [ (span_msg, span) ], None))