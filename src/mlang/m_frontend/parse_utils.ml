(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

module E = Errors

(** Helpers for parsing *)

let current_file : string ref = ref ""

let mk_position sloc = { Pos.pos_filename = !current_file; Pos.pos_loc = sloc }

(** {1 Frontend variable names}*)

(** Checks whether the string is entirely capitalized *)
let parse_variable_name sloc (s : string) : Mast.variable_name =
  if not (String.equal (String.uppercase_ascii s) s) then
    E.parser_error sloc "invalid variable name"
  else s

(** Checks for duplicate generic parameters *)
let dup_exists l =
  let rec dup_consecutive = function
    | [] | [ _ ] -> false
    | c1 :: (c2 as h2) :: tl -> Char.equal c1 c2 || dup_consecutive (h2 :: tl)
  in
  let sort_on_third s1 s2 = Char.compare s1 s2 in
  dup_consecutive (List.sort sort_on_third l)

(** Parse variable with parameters, parameters have to be lowercase letters *)
let parse_variable_generic_name sloc (s : string) : Mast.variable_generic_name =
  let parameters = ref [] in
  for i = String.length s - 1 downto 0 do
    let p = s.[i] in
    if
      p = '_'
      || Re.Str.string_match (Re.Str.regexp "[0-9]+") (String.make 1 p) 0
      || not (Char.equal (Char.lowercase_ascii p) p)
    then ()
    else parameters := p :: !parameters
  done;
  if dup_exists !parameters then
    E.parser_error sloc "variable parameters should have distinct names";
  { Mast.parameters = !parameters; Mast.base = s }

(** Checks whether the variable contains parameters *)
let parse_variable sloc (s : string) =
  try Mast.Normal (parse_variable_name sloc s)
  with E.ParsingError _ -> (
    try Mast.Generic (parse_variable_generic_name sloc s)
    with E.ParsingError _ -> E.parser_error sloc "invalid variable name" )

(** A parsed variable can be a regular variable or an integer literal *)
type parse_val = ParseVar of Mast.variable | ParseInt of int

let parse_variable_or_int sloc (s : string) : parse_val =
  try ParseInt (int_of_string s)
  with Failure _ -> (
    try ParseVar (Mast.Normal (parse_variable_name sloc s))
    with E.ParsingError _ -> (
      try ParseVar (Mast.Generic (parse_variable_generic_name sloc s))
      with E.ParsingError _ -> E.parser_error sloc "invalid variable name" ) )

(** Table index can be integer or [X], the generic table index variable *)
let parse_table_index sloc (s : string) : Mast.table_index =
  try Mast.LiteralIndex (int_of_string s)
  with Failure _ -> (
    try Mast.SymbolIndex (parse_variable sloc s)
    with E.ParsingError _ ->
      Format.printf "s: %s, %b\n" s (String.equal s "X");
      E.parser_error sloc "table index should be an integer" )

(**{1 Literal parsing}*)

let parse_literal sloc (s : string) : Mast.literal =
  try Mast.Float (float_of_string s) with Failure _ -> Mast.Variable (parse_variable sloc s)

let parse_func_name _ (s : string) : Mast.func_name = s

let parse_int sloc (s : string) : int =
  try int_of_string s with Failure _ -> E.parser_error sloc "should be an integer"

let parse_string (s : string) : string =
  (* we remove the quotes *)
  let s = Re.Str.string_after s 1 in
  Re.Str.string_before s (String.length s - 1)
