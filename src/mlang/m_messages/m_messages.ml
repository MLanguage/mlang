(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

module Types = Types

module Fr : Types.LANG = Fr

module Main : Types.LANG =
  (val match Utils.selected_lang ~default:`Francais with
       | `Francais -> (module Fr : Types.LANG)
       | `English -> (module En : Types.LANG))

include Main

(** Parsing errors are accessible through the [LANG.Parser] module. This
    function allows to access it through a string key of the format
    ["Lang:name_of_the_method"]. If ["name_of_the_method"] corresponds to an
    actual method of [LANG.Parser], it will return the [`Message] with the right
    translation. If the ["name_of_the_method"] is an unknown code, this function
    will return [`Unknown_code]. At last, if the input string is not properly
    formatted, returns [`Not_a_code]. *)
let select_parse_error_message parse_error_message =
  let open Parser in
  try
    Scanf.sscanf parse_error_message "Lang:%s" @@ function
    | "incomplete_attr_definition" -> incomplete_attr_definition
    | "incomplete_attr_list" -> incomplete_attr_list
    | "missing_value_after_equal" -> missing_value_after_equal
    | "unexpected_symbol" -> unexpected_symbol
    | s -> "Code " ^ s
  with Scanf.Scan_failure _ ->
    Format.sprintf "%s : %s" unexpected_syntax_error parse_error_message

(* let print_validator_warning (v : M_frontend.Validator.Warning.t) = *)
(*   let open Validator.Warning in *)
(*   match v with *)
(*   | Autocycle { rule_id; var_name } -> autocycle ~rule_id ~var_name *)
(*   | Reference_used_to_set_reference { var_name; pos } -> *)
(*       reference_used_to_set_reference ~var_name ~pos *)
(*   | Variable_defined_several_times { var_name; pos_list } -> *)
(*       variable_defined_several_times ~var_name ~pos_list *)

(* (\* This is where we associate every warning/error to its message *\) *)
(* let () = *)
(*   (\* Warning messages*\) *)
(*   let make_str_message (t : Log.Warning.t) = *)
(*     match t with *)
(*     | M_frontend.Validator.Warning.Validator v -> *)
(*         Some (print_validator_warning v) *)
(*     | _ -> None *)
(*   in *)
(*   Log.Warning.register make_str_message; *)
(*   (\* Error messages. *)
(*      Using Printexc.register_printer is a last resort solution, we rather should *)
(*      treat exceptions at the driver level. But if an exception is not *)
(*      caught at the right place, at least this handles it more or less *)
(*      correctly. *\) *)
(*   Printexc.register_printer (function *)
(*     | M_frontend.Validator.Err.T (Pos.Mark (e, pos)) -> *)
(*         Some (print_validator_error e pos).msg *)
(*     | M_frontend.Parse_utils.Parsing_error { msg; _ } -> Some msg *)
(*     | M_frontend.Mparser.Error i -> Some (print_parse_error_from_code i) *)
(*     | _ -> None) *)
