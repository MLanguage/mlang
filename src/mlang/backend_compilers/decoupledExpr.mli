type offset =
  | GetValueConst of int
  | GetValueVar of Bir.variable
  | PassPointer
  | None

val generate_variable :
  Dgfip_varid.var_id_map ->
  offset ->
  ?def_flag:bool ->
  ?debug_flag:bool ->
  Bir.variable ->
  string

type dflag = Def | Val

type local_var

type t

type local_decls

type constr

type expression_composition = { def_test : constr; value_comp : constr }

val is_always_true : t -> bool

val locals_from_m : Mir.LocalVariable.t -> local_var * local_var

val new_local : unit -> local_var

val dtrue : constr

val dfalse : constr

val lit : float -> constr

val m_var : Bir.variable -> offset -> dflag -> constr

val let_local : local_var -> constr -> constr -> constr

val local_var : local_var -> constr

val dand : constr -> constr -> constr

val dor : constr -> constr -> constr

val dnot : constr -> constr

val minus : constr -> constr

val plus : constr -> constr -> constr

val sub : constr -> constr -> constr

val mult : constr -> constr -> constr

val div : constr -> constr -> constr

val comp : string -> constr -> constr -> constr

val dfun : string -> constr list -> constr

val access : Bir.variable -> dflag -> constr -> constr

val ite : constr -> constr -> constr -> constr

val build_transitive_composition :
  ?safe_def:bool -> expression_composition -> expression_composition

val build_expression : expression_composition -> local_decls * t * t

val format_local_declarations : Format.formatter -> local_decls -> unit

val format_assign :
  Dgfip_options.flags ->
  Dgfip_varid.var_id_map ->
  string ->
  Format.formatter ->
  t ->
  unit
