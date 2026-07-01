(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2023 - 2026 DGFiP - INRIA                               *)
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

open Irj_ast

type input = Filename of string | Contents of string

(* Implement a parsing error handling following François Pottier’s example
   in https://gitlab.inria.fr/fpottier/menhir/blob/master/demos/calc-syntax-errors/calc.ml *)

(* [env checkpoint] extracts a parser environment out of a checkpoint,
   which must be of the form [HandlingError env]. *)
let env checkpoint =
  match checkpoint with
  | Irj_parser.MenhirInterpreter.HandlingError env -> env
  | _ -> assert false

(* [state checkpoint] extracts the number of the current state out of a
   checkpoint. *)

let state checkpoint : int =
  match Irj_parser.MenhirInterpreter.top (env checkpoint) with
  | Some (Irj_parser.MenhirInterpreter.Element (s, _, _, _)) ->
      Irj_parser.MenhirInterpreter.number s
  | None ->
      (* Hmm... The parser is in its initial state. The incremental API
         currently lacks a way of finding out the number of the initial
         state. It is usually 0, so we return 0. This is unsatisfactory
         and should be fixed in the future. *)
      0

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)

let show text positions =
  MenhirLib.ErrorReports.extract text positions
  |> MenhirLib.ErrorReports.sanitize |> MenhirLib.ErrorReports.compress
  |> MenhirLib.ErrorReports.shorten 20
(* max width 43 *)

(* [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero. *)

let get text checkpoint i =
  match Irj_parser.MenhirInterpreter.get i (env checkpoint) with
  | Some (Irj_parser.MenhirInterpreter.Element (_, _, pos1, pos2)) ->
      show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      failwith "should not happen"

(* [succeed v] is invoked when the parser has succeeded and produced a
   semantic value [v]. *)
let succeed v = v

(* [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error. *)

let fail text buffer (checkpoint : _ Irj_parser.MenhirInterpreter.checkpoint) =
  (* Fetch an error message from the database. *)
  let message = ParserMessages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = MenhirLib.ErrorReports.expand (get text checkpoint) message in
  (* Show the tokens just before and just after the error. *)
  let indication =
    Printf.sprintf "Syntax error %s: %s"
      (MenhirLib.ErrorReports.show (show text) buffer)
      (String.trim message)
  in
  (* Show these three components. *)
  Errors.raise_spanned_error indication
    (mk_position (MenhirLib.ErrorReports.last buffer))

let parse_input (test_name : input) : Irj_ast.irj_file =
  let text, filebuf =
    match test_name with
    | Contents contents -> (contents, Lexing.from_string contents)
    | Filename filename -> (
        try MenhirLib.LexerUtil.read filename
        with Sys_error msg ->
          Errors.raise_error
            (Format.asprintf "Unable to open file %s (%s)" filename msg))
  in
  let supplier =
    Irj_parser.MenhirInterpreter.lexer_lexbuf_to_supplier Irj_lexer.token
      filebuf
  in
  (* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. *)
  let buffer, supplier = MenhirLib.ErrorReports.wrap_supplier supplier in
  (* Fetch the parser's initial checkpoint. *)
  let checkpoint = Irj_parser.Incremental.irj_file filebuf.lex_curr_p in
  (* Run the parser. *)
  (* We do not handle [Lexer.Error] because we know that we will not
     encounter a lexical error during this second parsing run. *)
  Irj_parser.MenhirInterpreter.loop_handle succeed (fail text buffer) supplier
    checkpoint
