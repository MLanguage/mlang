(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by the DGFiP.

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

let bitvec_size = ref 30


type repr_kind =
  | Integer of int
  | Real of int
  | Boolean
[@@deriving show]

type repr = { repr_kind: repr_kind; is_table: bool }
[@@deriving show]

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
  let exec_order = Execution_order.get_execution_order p in
  let new_typing = { repr_info_var = Mvg.VariableMap.empty;
                     repr_info_local_var =
                       Mvg.LocalVariableMap.map (fun ty ->
                           match ty with
                           | Mvg.Boolean ->
                             {repr_kind = Boolean; is_table = false}
                           | Mvg.Integer ->
                             {repr_kind = Integer size; is_table = false}
                           | Mvg.Real ->
                             {repr_kind = Real size; is_table = false}
                         )
                       old_typing.typ_info_local_var } in
  List.fold_left (fun new_typing scc ->
      Mvg.VariableMap.fold
        (fun var () new_typing ->
           if Mvg.VariableMap.mem var old_typing.Typechecker.typ_info_var then
             match Mvg.VariableMap.find var old_typing.Typechecker.typ_info_var with
             | (Mvg.Boolean, is_table) ->
               {new_typing with
                repr_info_var =
                  Mvg.VariableMap.add var { repr_kind = Boolean; is_table }
                    new_typing.repr_info_var }
             | (Mvg.Integer, is_table) ->
               let (bitvec_order, new_typing) =
                 size, new_typing
                 (* find_bitvec_order p var new_typing old_typing *) in
               {new_typing with
                repr_info_var =
                  Mvg.VariableMap.add var { repr_kind = Integer bitvec_order ; is_table }
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
        ) scc new_typing
    ) new_typing exec_order
