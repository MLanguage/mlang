(*From test_ast.ml*)

type literal = I of int | F of float

type var_value = string * literal * Pos.t

type var_values = var_value list

type errors = (string * Pos.t) list

type rappels =
  (string * string * var_value * string * string * string * string * string)
  list

type irj_file = {
  nom : string;
  prim : var_values * errors * var_values;
  rapp : (rappels * errors * var_values) option;
}
