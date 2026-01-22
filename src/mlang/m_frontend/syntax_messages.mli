(** Parsing error messages handler.
    The .ml file corresponding to this mli is generated automatically generated
    by menhir (check the dune file) from the [mparser.messages] file. This file
    is the core of parsing message handling: this is where all error messages
    must be added.

    The structure of the [mparser.messages] strongly depends on the parser's. If
    the parser changes, it may be necessary to rework on it to update the error
    messages. Hopefully, [menhir] comes with a way to automatically update
    messages when the parser changes. This can be automatically done by running
    [dune runtest --auto-promote]. Check the [src/mlang/m_frontend/dune] file
    for more information.

    Each possible error is mapped to a state (an integer) and a custom message
    in the [mparser.messages] file. When the parser raises an exception, it also
    raises the state where it failed. This module allows to get the
    corresponding error message.

    There is then three possibilities:
    * the state has a custom error message, which can be printed;
    * the state has the default error message (<"YOUR SYNTAX ERROR MESSAGE HERE>"),
      which has not been written yet;
    * the state has no error message, which should not happen unless the
      [mparser.messages] is not sync with [mparser.mly].
 *)

val message : int -> string
(** Returns the error message corresponding to a parser error state.
    Raises [Not_found] if the state has no error message. *)
