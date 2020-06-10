(* Copyright (C) 2020 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** This modules define the logic used to call the M code several times in order to compute the
    amount of tax correctly *)

open Mvg

let var_is_deposit (v : Variable.t) : bool =
  List.exists
    (fun ((attr_name, _), (attr_value, _)) -> attr_name = "acompte" && attr_value = Ast.Float 1.)
    v.Mvg.Variable.attributes

let var_is_revenue_but_not_deposit (v : Variable.t) : bool =
  List.for_all
    (fun ((attr_name, _), (attr_value, _)) ->
      not (attr_name = "acompte" && attr_value = Ast.Float 1.))
    v.Mvg.Variable.attributes
  && v.Variable.is_income

(** Equivalent to AC_IsCalculAcptes in the DGFiP's logic *)
let exists_deposit_defined_variables (input_values : literal VariableMap.t) : bool =
  VariableMap.cardinal (VariableMap.filter (fun v _ -> var_is_deposit v) input_values) > 0

(** Equivalent to AC_GetCodesAvFisc in the DGFiP's logic *)
let all_deposit_defined_variables (input_values : literal VariableMap.t) : unit VariableMap.t =
  VariableMap.map (fun _ -> ()) (VariableMap.filter (fun v _ -> var_is_deposit v) input_values)

(** Equivalent to AC_GetCodesAcompte in the DGFiP's logic *)
let all_revenues_defined_not_deposit (input_values : literal VariableMap.t) : unit VariableMap.t =
  VariableMap.map
    (fun _ -> ())
    (VariableMap.filter (fun v _ -> var_is_revenue_but_not_deposit v) input_values)
