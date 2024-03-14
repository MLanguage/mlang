(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

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

open Mir

let format_typ fmt (t : typ) =
  Format.pp_print_string fmt (match t with Real -> "real")

let format_func fmt (f : func) =
  Format.pp_print_string fmt
    (match f with
    | SumFunc -> "somme"
    | AbsFunc -> "abs"
    | MinFunc -> "min"
    | MaxFunc -> "max"
    | GtzFunc -> "positif"
    | GtezFunc -> "positif_ou_nul"
    | NullFunc -> "null"
    | ArrFunc -> "arr"
    | InfFunc -> "inf"
    | PresentFunc -> "present"
    | Multimax -> "multimax"
    | Supzero -> "supzero"
    | VerifNumber -> "numero_verif"
    | ComplNumber -> "numero_compl")

let format_literal fmt (l : literal) =
  Format.pp_print_string fmt
    (match l with Float f -> string_of_float f | Undefined -> "indéfini")

let format_set_value fmt (sv : set_value) =
  match sv with
  | VarValue v -> Format.fprintf fmt "%s" (Pos.unmark (Pos.unmark v).name)
  | Interval (i1, i2) ->
      Format.fprintf fmt "%d..%d" (Pos.unmark i1) (Pos.unmark i2)
  | FloatValue i -> Format.fprintf fmt "%f" (Pos.unmark i)

let rec format_expression fmt (e : expression) =
  match e with
  | TestInSet (belong, e, values) ->
      Format.fprintf fmt "(%a %sdans %a)" format_expression (Pos.unmark e)
        (if belong then "" else "non ")
        (Format_mast.pp_print_list_comma format_set_value)
        values
  | Comparison ((op, _), (e1, _), (e2, _)) ->
      Format.fprintf fmt "(%a %a %a)" format_expression e1
        Format_mast.format_comp_op op format_expression e2
  | Binop ((op, _), (e1, _), (e2, _)) ->
      Format.fprintf fmt "(%a %a %a)" format_expression e1
        Format_mast.format_binop op format_expression e2
  | Unop (op, (e, _)) ->
      Format.fprintf fmt "%a %a" Format_mast.format_unop op format_expression e
  | Conditional ((e1, _), (e2, _), (e3, _)) ->
      Format.fprintf fmt "(si %a alors %a sinon %a)" format_expression e1
        format_expression e2 format_expression e3
  | FunctionCall (f, args) ->
      Format.fprintf fmt "%a(%a)" format_func f
        (Format_mast.pp_print_list_comma
           (Format_mast.pp_unmark format_expression))
        args
  | Literal lit -> format_literal fmt lit
  | Var var -> Format.fprintf fmt "%s" (Pos.unmark (Pos.unmark var).name)
  | Index (var, i) ->
      Format.fprintf fmt "%s[%a]"
        (Pos.unmark (Pos.unmark var).name)
        format_expression (Pos.unmark i)
  | NbCategory cats ->
      Format.fprintf fmt "nb_categorie(%a)" (Mir.CatVarSet.pp ()) cats
  | Attribut (v, _, a) ->
      Format.fprintf fmt "attribut(%s, %s)" (Pos.unmark v) (Pos.unmark a)
  | Size var ->
      Format.fprintf fmt "taille(%s)" (Pos.unmark (Pos.unmark var).name)
  | NbAnomalies -> Format.fprintf fmt "nb_anomalies()"
  | NbDiscordances -> Format.fprintf fmt "nb_discordances()"
  | NbInformatives -> Format.fprintf fmt "nb_informatives()"
  | NbBloquantes -> Format.fprintf fmt "nb_bloquantes()"

let format_error fmt (e : Error.t) =
  Format.fprintf fmt "erreur %s (%s)" (Pos.unmark e.Error.name)
    (Error.err_descr_string e |> Pos.unmark)

let format_variable fmt (v : Variable.t) =
  Format.fprintf fmt "%s: %s" (Pos.unmark v.name) (Pos.unmark v.descr)
