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

let parse_file (test_name : string) : irj_file =
  let _text, filebuf = MenhirLib.LexerUtil.read test_name in
  (*Je comprends pas bien l’intérêt, pos_fname c’est déjà le nom du fichier.*)
  (* let filebuf =
       {
         filebuf with
         lex_curr_p = { filebuf.lex_curr_p with pos_fname = test_name };
       }
     in *)
  let f =
    try Irj_parser.irj_file Irj_lexer.token filebuf with
    | TestLexingError e -> raise (TestLexingError e)
    | TestParsingError e -> raise (TestParsingError e)
    | Irj_parser.Error -> raise TestErrorActivating2ndPass
  in
  f

(* Implement a parsing error handling following François Pottier’s example
   in https://gitlab.inria.fr/fpottier/menhir/blob/master/demos/calc-syntax-errors/calc.ml *)

(* [env checkpoint] extracts a parser environment out of a checkpoint,
   which must be of the form [HandlingError env]. *)

let env checkpoint =
  match checkpoint with
  | UnitActionsParser.MenhirInterpreter.HandlingError env -> env
  | _ -> assert false

(* [state checkpoint] extracts the number of the current state out of a
   checkpoint. *)

let state checkpoint : int =
  match UnitActionsParser.MenhirInterpreter.top (env checkpoint) with
  | Some (UnitActionsParser.MenhirInterpreter.Element (s, _, _, _)) ->
      UnitActionsParser.MenhirInterpreter.number s
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
  match UnitActionsParser.MenhirInterpreter.get i (env checkpoint) with
  | Some (UnitActionsParser.MenhirInterpreter.Element (_, _, pos1, pos2)) ->
      show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"

(* [succeed v] is invoked when the parser has succeeded and produced a
   semantic value [v]. In our setting, this cannot happen, since the
   table-based parser is invoked only when we know that there is a
   syntax error in the input file. *)

let succeed _v = assert false

(* [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error. *)

let fail text buffer
    (checkpoint : _ UnitActionsParser.MenhirInterpreter.checkpoint) =
  (* Indicate where in the input file the error occurred. *)
  let location =
    MenhirLib.LexerUtil.range (MenhirLib.ErrorReports.last buffer)
  in
  (* Show the tokens just before and just after the error. *)
  let indication =
    Printf.sprintf "Syntax error %s.\n"
      (MenhirLib.ErrorReports.show (show text) buffer)
  in
  (* Fetch an error message from the database. *)
  let message = ParserMessages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = MenhirLib.ErrorReports.expand (get text checkpoint) message in
  (* Show these three components. *)
  Printf.eprintf "%s%s%s%!" location indication message;
  exit 1

let dummy_parse_file_with_incremental (test_name : string) : unit =
  let text, filebuf = MenhirLib.LexerUtil.read test_name in
  (*Je comprends pas bien l’intérêt, pos_fname c’est déjà le nom du fichier.*)
  (* let filebuf =
       {
         filebuf with
         lex_curr_p = { filebuf.lex_curr_p with pos_fname = test_name };
       }
     in *)
  let supplier =
    UnitActionsParser.MenhirInterpreter.lexer_lexbuf_to_supplier Irj_lexer.token
      filebuf
  in

  (* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. *)
  let buffer, supplier = MenhirLib.ErrorReports.wrap_supplier supplier in
  (* Fetch the parser's initial checkpoint. *)
  let checkpoint = UnitActionsParser.Incremental.irj_file filebuf.lex_curr_p in
  (* Run the parser. *)
  (* We do not handle [Lexer.Error] because we know that we will not
     encounter a lexical error during this second parsing run. *)
  UnitActionsParser.MenhirInterpreter.loop_handle succeed (fail text buffer)
    supplier checkpoint
