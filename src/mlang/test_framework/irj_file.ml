(* Copyright Inria, contributors: Raphaël Monat <raphael.monat@lip6.fr> (2019)
   Mathieu Durero <mathieu.durero@dgfip.finances.gouv.fr> (2023)

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

open Irj_ast

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

let parse_file (test_name : string) : Irj_ast.irj_file =
  let text, filebuf = MenhirLib.LexerUtil.read test_name in
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
