(*
Copyright (C) 2019 Inria, contributors:
  Denis Merigoux <denis.merigoux@inria.fr>
  Raphël Monat <raphael.monat@lip6.fr>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

let bitvec_size = ref 30


type repr_kind =
  | Real of int
  | Boolean


type repr = { repr_kind: repr_kind; is_table: bool }


type repr_info = {
  repr_info_var : repr Mvg.VariableMap.t;
  repr_info_local_var : repr Mvg.LocalVariableMap.t
}

type var_repr =
  | Regular of Z3.Expr.expr
  | Table of (Z3.Expr.expr -> Z3.Expr.expr)


type repr_data = {
  repr_data_var : (var_repr * repr) Mvg.VariableMap.t;
  repr_data_local_var : (var_repr * repr) Mvg.LocalVariableMap.t
}

let find_bitvec_repr
    (p: Mvg.program)
    (old_typing: Typechecker.typ_info)
  : repr_info =
  let size = !bitvec_size in
  let new_typing = { repr_info_var = Mvg.VariableMap.empty;
                     repr_info_local_var =
                       Mvg.LocalVariableMap.map (fun ty ->
                           match ty with
                           | Mvg.Boolean ->
                             {repr_kind = Boolean; is_table = false}
                           | Mvg.Real ->
                             {repr_kind = Real size; is_table = false}
                         )
                         old_typing.typ_info_local_var } in
  Execution_order.fold_on_vars
    (fun var new_typing ->
       if Mvg.VariableMap.mem var old_typing.Typechecker.typ_info_var then
         match Mvg.VariableMap.find var old_typing.Typechecker.typ_info_var with
         | (Mvg.Boolean, is_table) ->
           {new_typing with
            repr_info_var =
              Mvg.VariableMap.add var { repr_kind = Boolean; is_table }
                new_typing.repr_info_var }
         | (Mvg.Real, is_table) ->
           let (bitvec_order, new_typing) =
             size, new_typing
             (* find_bitvec_order p var new_typing old_typing *) in
           {new_typing with
            repr_info_var =
              Mvg.VariableMap.add var { repr_kind = Real bitvec_order; is_table}
                new_typing.repr_info_var }
       else
         (* let () = Cli.warning_print (Printf.sprintf "var %s not used when computing sizes\n" (Mvg.Variable.show var)) in *)
         new_typing
    ) p new_typing
