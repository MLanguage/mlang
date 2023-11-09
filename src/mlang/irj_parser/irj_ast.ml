(*From test_ast.ml*)

type literal = I of int | F of float

type var_value = string * literal * Pos.t

type var_values = var_value list

type errors = (string * Pos.t) list

type rappels =
  (string * string * var_value * string * string * string * string * string)
  list

type prim_data_block = {
  entrees : var_values;
  controles_attendus : errors;
  resultats_attendus : var_values;
}

type corr_data_block = {
  entrees_rappels : rappels;
  controles_attendus : errors;
  resultats_attendus : var_values;
}

type irj_file = {
  nom : string;
  prim : prim_data_block;
  rapp : corr_data_block option;
}
