(*From utils/pos.ml*)

type t = { pos_filename : string; pos_loc : Lexing.position * Lexing.position }
(** A position in the source code is a file, as well as begin and end location
    of the form col:line *)

let make_position (f : string) (loc : Lexing.position * Lexing.position) =
      { pos_filename = f; pos_loc = loc }


(*From test_ast.ml*)

type literal = I of int | F of float

type var_values = (string * literal * t) list

type errors = (string * t) list

type rappels = (string * string * (string * literal * t) * string * string * string * string * string) list

type irj_file = {
  nom : string;
  prim : (var_values * errors * var_values);
  rapp : (rappels * errors * var_values) option;
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