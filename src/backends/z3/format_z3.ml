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

let format_Z3_encoding (t: Z3_encoding.repr) : string =
  let first = match t.Z3_encoding.repr_kind with
    | Z3_encoding.Integer _ -> Format_mvg.format_typ Mvg.Integer
    | Z3_encoding.Real _ -> Format_mvg.format_typ Mvg.Real
    | Z3_encoding.Boolean -> Format_mvg.format_typ Mvg.Boolean
  in
  let second = if t.Z3_encoding.is_table then "[X]" else "" in
  first ^ second

let convert_to_signed (f:float) (bsize:int) =
  if f <= 2. ** (float_of_int (bsize - 1)) then f
  else f -. 2. ** (float_of_int bsize)

let format_z3_program
    (p: (Z3_encoding.var_repr * Z3_encoding.repr) Mvg.VariableMap.t)
    (s: Z3.Solver.solver)
  : string =
  match Z3.Solver.get_model s with
  | Some model ->
    Cli.warning_print (Printf.sprintf "Z3 model: %s" (Z3.Model.to_string model));
    let l = Mvg.VariableMap.fold (fun var (e, typ) acc ->
        match e with
        | Z3_encoding.Regular e ->
          (Ast.unmark var.Mvg.Variable.name, begin match Z3.Model.eval model e true with
              | Some new_e ->
                begin match typ.Z3_encoding.repr_kind with
                  | Z3_encoding.Integer _
                  | Z3_encoding.Real _ ->
                    string_of_float (convert_to_signed (Big_int.float_of_big_int (Big_int.big_int_of_string  (Z3.BitVector.numeral_to_string new_e))) !Z3_encoding.bitvec_size /. (float_of_int Mvg_to_z3.mult_factor))
                  | Z3_encoding.Boolean -> (match Z3.Boolean.get_bool_value new_e with
                      | Z3enums.L_FALSE -> "false"
                      | Z3enums.L_TRUE -> "true"
                      | Z3enums.L_UNDEF -> "undefined boolean"
                    )
                end
              | None -> "could not evaluate variable" end,
           format_Z3_encoding typ
          )::acc
        | Z3_encoding.Table _ ->
          assert false (* not implemented yet *)
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
