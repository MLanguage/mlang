(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

(** {2 M messages} *)

(** This module provides multiple-language logging utils and defines the several
    printers for mlang (for now, Warnings and Errors). The environment variable
    'LANG' is used to define the messages language, which redirect to the actual
    translation modules ({!module M_messages.En} for English messages,
    {!module M_messages.Fr} for french). The module is automatically selected at
    runtime and included in this module, so refering to this module is
    equivalent to refering to the corresponding translation module. *)

module Types = Types

include Types.LANG

val select_parse_error_message : string -> string
(** When failing, the parser raises a [Mparser.Error] with an integer error
    code. Each code is associated to a error message in the
    [M_frontend.Syntax_messages] module. Note that this module does not define
    textual error messages, but strings under the format
    ["Lang:name_of_the_method"]. This function fetches the string associated to
    the error code and, if it corresponds to properly formatted string, returns
    the associated translated error message. *)

(* val print_validator_error : *)
(*   M_frontend.Validator.Err.t -> Pos.t -> Log.structured_msg *)
(* (\** Prints the string representation of a validator error case. *\) *)
