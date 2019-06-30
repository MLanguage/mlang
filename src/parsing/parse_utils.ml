(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by thge DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-C license and that you accept its terms.
*)

(** Helpers for parsing *)

let current_file: string ref = ref ""

let mk_position sloc = {
  Ast.pos_filename = !current_file;
  Ast.pos_loc = sloc;
}

(** {1 Parsing variable names }*)

(** Checks whether the string is entirely capitalized *)
let parse_variable_name sloc (s: string) : Ast.variable_name =
  if not (String.equal (String.uppercase_ascii s) s) then
    Errors.parser_error sloc "invalid variable name"
  else
    s

(** Checks for duplicate generic parameters *)
let dup_exists l =
  let rec dup_consecutive = function
    | [] | [_] -> false
    | c1::(c2 as h2)::tl -> Char.equal c1 c2 || dup_consecutive (h2::tl)
  in
  let sort_on_third s1 s2 = Char.compare s1 s2 in
  dup_consecutive (List.sort sort_on_third l)


(** Parse variable with parameters, parameters have to be lowercase letters *)
let parse_variable_generic_name sloc (s: string) : Ast.variable_generic_name =
  let parameters = ref [] in
  for i = String.length s - 1 downto 0 do
    let p = String.get s i in
    if p = '_' || Str.string_match (Str.regexp "[0-9]+") (String.make 1 p) 0 ||
       not (Char.equal (Char.lowercase_ascii p) p)
    then
      ()
    else begin
      parameters := p::!parameters;
    end
  done;
  if dup_exists !parameters then
    Errors.parser_error sloc "variable parameters should have distinct names";
  { Ast.parameters = !parameters; Ast.base = s }

(** Checks whether the variable contains parameters *)
let parse_variable sloc (s:string) =
  try Ast.Normal (parse_variable_name sloc s) with
  | Errors.ParsingError _ ->
    try Ast.Generic (parse_variable_generic_name sloc s) with
    | Errors.ParsingError _ ->
      Errors.parser_error sloc "invalid variable name"

(** A parsed variable can be a regular variable or an integer literal *)
type parse_val =
  | ParseVar of Ast.variable
  | ParseInt of int

let parse_variable_or_int sloc (s:string) : parse_val  =
  try ParseInt (int_of_string s) with
  | Failure _ ->
    try ParseVar (Ast.Normal (parse_variable_name sloc s)) with
    | Errors.ParsingError _ ->
      try ParseVar (Ast.Generic (parse_variable_generic_name sloc s)) with
      | Errors.ParsingError _ ->
        Errors.parser_error sloc "invalid variable name"

(** Table index can be integer or [X], the generic table index variable *)
let parse_table_index sloc (s: string) : Ast.table_index =
  if String.equal s "X" then
    Ast.GenericIndex
  else
    try Ast.LiteralIndex(int_of_string s) with
    | Failure _ ->
      begin try Ast.SymbolIndex (parse_variable sloc s) with
        | Errors.ParsingError _ ->
          Printf.printf "s: %s, %b\n" s (String.equal s "X");
          Errors.parser_error sloc "table index should be an integer"
      end

(**{1 Literal parsing }*)

let parse_literal sloc (s: string) : Ast.literal =
  try Ast.Int (int_of_string s) with
  | Failure _ -> try Ast.Float (float_of_string s) with
    | Failure _ ->
      Ast.Variable (parse_variable sloc s)

let parse_func_name sloc (s: string) : Ast.func_name =
  (s)

let parse_int sloc (s: string) : int =
  try int_of_string s with
  | Failure _ ->
    Errors.parser_error sloc "should be an integer"

let parse_string (s: string) : string =
  (** we remove the quotes *)
  let s = Str.string_after s 1 in
  Str.string_before s (String.length s - 1)
