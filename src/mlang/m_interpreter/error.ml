open M_messages.Interpreter
open M_ir

(* exception RuntimeError of Types.run_error *)

(* (\** The different kinds of errors the interpretor may raise. *\) *)
(* type run_error = *)
(*   | NanOrInf of string * Mir.expression Pos.marked *)
(*   | StructuredError of (Ppf.structured_msg * (unit -> unit) option) *)

module Make (N : Number.S) = struct
  let raise ?pos m =
    match pos with
    | None -> Errors.raise_error m
    | Some pos -> Errors.raise_spanned_error m pos

  let invalid_expression_value (Pos.Mark (expr, pos)) value =
    let expr = Format.asprintf "%a" (Com.format_expression Com.Var.pp) expr in
    let value = Format.asprintf "%a" N.format_t value in
    raise ~pos @@ invalid_expression_value ~expr ~value

  let invalid_matching_in_switch ~case ~matched ~pos =
    let case = Format.asprintf "%a" (Com.format_case Com.Var.pp) case in
    let matched =
      match matched with
      | `Undefined -> `Undefined
      | `Value v -> `Value (Format.asprintf "%a" N.format_t v)
      | `Var v ->
          `Var (Format.asprintf "%a" Com.(format_access Var.pp) (Pos.unmark v))
    in
    raise ~pos @@ invalid_matching_in_switch ~case ~matched

  let wrong_arity ~func ~args ~pos =
    raise ~pos
    @@ wrong_arity
         ~func:(Format.asprintf "%a" Com.format_func func)
         ~arity:(Com.function_arity func) ~args

  let unimplemented ~func ~pos =
    raise ~pos
    @@ unimplemented ~func:(Format.asprintf "%a" Com.format_func func)
end
