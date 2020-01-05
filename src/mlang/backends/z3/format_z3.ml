(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

let format_Z3_encoding fmt (t : Z3_encoding.repr) =
  Format.fprintf fmt "%s%s"
    ( match t.Z3_encoding.repr_kind with
    | Z3_encoding.Real _ -> Format.asprintf "%a" Format_mvg.format_typ Mvg.Real
    | Z3_encoding.Boolean -> Format.asprintf "%a" Format_mvg.format_typ Mvg.Boolean )
    (if t.Z3_encoding.is_table then "[X]" else "")

let convert_to_signed (f : float) (bsize : int) =
  if f <= 2. ** float_of_int (bsize - 1) then f else f -. (2. ** float_of_int bsize)

let format_z3_program (p : (Z3_encoding.var_repr * Z3_encoding.repr) Mvg.VariableMap.t)
    (s : Z3.Solver.solver) : string =
  match Z3.Solver.get_model s with
  | Some model ->
      Cli.warning_print "Z3 model: %s" (Z3.Model.to_string model);
      let l =
        Mvg.VariableMap.fold
          (fun var (e, typ) acc ->
            match e with
            | Z3_encoding.Regular e ->
                ( Pos.unmark var.Mvg.Variable.name,
                  begin
                    match Z3.Model.eval model e true with
                    | Some new_e -> (
                        match typ.Z3_encoding.repr_kind with
                        | Z3_encoding.Real _ ->
                            string_of_float
                              ( convert_to_signed
                                  (Big_int.float_of_big_int
                                     (Big_int.big_int_of_string
                                        (Z3.BitVector.numeral_to_string new_e)))
                                  !Z3_encoding.bitvec_size
                              /. float_of_int Mvg_to_z3.mult_factor )
                        | Z3_encoding.Boolean -> (
                            match Z3.Boolean.get_bool_value new_e with
                            | Z3enums.L_FALSE -> "false"
                            | Z3enums.L_TRUE -> "true"
                            | Z3enums.L_UNDEF -> "undefined boolean" ) )
                    | None -> "could not evaluate variable"
                  end,
                  Format.asprintf "%a" format_Z3_encoding typ )
                :: acc
            | Z3_encoding.Table _ -> assert false
            (* not implemented yet *))
          p []
      in
      "{\n"
      ^ String.concat ",\n"
          (List.map
             (fun (n, v, t) ->
               Format.asprintf "  \"%s\" : { \"value\" : \"%s\", \"type\": \"%s\" }" n v t)
             l)
      ^ "\n}\n"
  | None -> "model not available"
