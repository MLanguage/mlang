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

let format_z3_repr (t: Z3_repr.repr) : string =
  let first = match t.Z3_repr.repr_kind with
    | Z3_repr.Integer _ -> Format_cfg.format_typ Cfg.Integer
    | Z3_repr.Real _ -> Format_cfg.format_typ Cfg.Real
    | Z3_repr.Boolean -> Format_cfg.format_typ Cfg.Boolean
  in
  let second = if t.Z3_repr.is_table then "[X]" else "" in
  first ^ second

let format_z3_program
    (p: (Z3_repr.var_repr * Z3_repr.repr) Cfg.VariableMap.t)
    (ctx: Z3.context)
    (s: Z3.Solver.solver)
  : string =
  match Z3.Solver.get_model s with
  | Some model ->
    let l = Cfg.VariableMap.fold (fun var (e, typ) acc ->
        match e with
        | Z3_repr.Regular e ->
          (Ast.unmark var.Cfg.Variable.name, begin match Z3.Model.eval model e true with
              | Some new_e -> begin match typ.Z3_repr.repr_kind with
                  | Z3_repr.Integer _ -> string_of_int (Z3.BitVector.get_int new_e)
                  | Z3_repr.Real _ -> string_of_float ((float_of_int (Z3.BitVector.get_int new_e)) /. 100.0)
                  | Z3_repr.Boolean -> (match Z3.Boolean.get_bool_value new_e with
                      | Z3enums.L_FALSE -> "false"
                      | Z3enums.L_TRUE -> "true"
                      | Z3enums.L_UNDEF -> "undefined boolean"
                    )
                end
              | None -> "could not evaluate variable" end,
           format_z3_repr typ
          )::acc
        | Z3_repr.Table f ->
          (Ast.unmark var.Cfg.Variable.name,
           Z3.Expr.to_string
             (f (Z3.BitVector.mk_const_s ctx "X" Cfg_to_z3.bv_repr_ints_base)),
           format_z3_repr typ)::acc
      ) p []
    in
    "{\n"^
    (String.concat
       ",\n"
       (List.map
          (fun (n, v, t) ->
             Printf.sprintf
               "  \"%s\" : { \"value\" : \"%s\", \"type\": \"%s\" }"
               n
               v
               t
          )
          l
       ))
    ^ "\n}\n"
  | None -> "model not available"
