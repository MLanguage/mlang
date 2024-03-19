type cat_computed = Base | GivenBack

let pp_cat_computed fmt = function
  | Base -> Format.fprintf fmt "base"
  | GivenBack -> Format.fprintf fmt "restituee"

module CatCompSet = struct
  include SetExt.Make (struct
    type t = cat_computed

    let compare = compare
  end)

  let pp ?(sep = " ") ?(pp_elt = pp_cat_computed) (_ : unit)
      (fmt : Format.formatter) (set : t) : unit =
    pp ~sep ~pp_elt () fmt set
end

type cat_variable = CatInput of StrSet.t | CatComputed of CatCompSet.t

let pp_cat_variable fmt = function
  | CatInput id ->
      let pp fmt set = StrSet.iter (Format.fprintf fmt " %s") set in
      Format.fprintf fmt "saisie%a" pp id
  | CatComputed id ->
      let pp fmt set =
        CatCompSet.iter (Format.fprintf fmt " %a" pp_cat_computed) set
      in
      Format.fprintf fmt "calculee%a" pp id

let compare_cat_variable a b =
  match (a, b) with
  | CatInput _, CatComputed _ -> 1
  | CatComputed _, CatInput _ -> -1
  | CatInput id0, CatInput id1 -> StrSet.compare id0 id1
  | CatComputed c0, CatComputed c1 -> CatCompSet.compare c0 c1

module CatVarSet = struct
  include SetExt.Make (struct
    type t = cat_variable

    let compare = compare_cat_variable
  end)

  let pp ?(sep = ", ") ?(pp_elt = pp_cat_variable) (_ : unit)
      (fmt : Format.formatter) (set : t) : unit =
    pp ~sep ~pp_elt () fmt set
end

module CatVarMap = struct
  include MapExt.Make (struct
    type t = cat_variable

    let compare = compare_cat_variable
  end)

  let pp ?(sep = "; ") ?(pp_key = pp_cat_variable) ?(assoc = " => ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

type cat_variable_loc = LocCalculated | LocBase | LocInput

type cat_variable_data = {
  id : cat_variable;
  id_str : string;
  id_int : int;
  loc : cat_variable_loc;
  pos : Pos.t;
  attributs : Pos.t StrMap.t;
}

type literal = Float of float | Undefined

type 'v atom = AtomVar of 'v | AtomLiteral of literal

type 'v set_value_loop =
  | Single of 'v atom Pos.marked
  | Range of 'v atom Pos.marked * 'v atom Pos.marked
  | Interval of 'v atom Pos.marked * 'v atom Pos.marked

type 'v loop_variable = char Pos.marked * 'v set_value_loop list

type 'v loop_variables =
  | ValueSets of 'v loop_variable list
  | Ranges of 'v loop_variable list

(** Unary operators *)
type unop = Not | Minus

(** Binary operators *)
type binop = And | Or | Add | Sub | Mul | Div

(** Comparison operators *)
type comp_op = Gt | Gte | Lt | Lte | Eq | Neq

type 'v set_value =
  | FloatValue of float Pos.marked
  | VarValue of 'v Pos.marked
  | Interval of int Pos.marked * int Pos.marked

type func =
  | SumFunc  (** Sums the arguments *)
  | AbsFunc  (** Absolute value *)
  | MinFunc  (** Minimum of a list of values *)
  | MaxFunc  (** Maximum of a list of values *)
  | GtzFunc  (** Greater than zero (strict) ? *)
  | GtezFunc  (** Greater or equal than zero ? *)
  | NullFunc  (** Equal to zero ? *)
  | ArrFunc  (** Round to nearest integer *)
  | InfFunc  (** Truncate to integer *)
  | PresentFunc  (** Different than zero ? *)
  | Multimax  (** ??? *)
  | Supzero  (** ??? *)
  | VerifNumber
  | ComplNumber

type 'v expression_ =
  | TestInSet of bool * 'v m_expression_ * 'v set_value list
      (** Test if an expression is in a set of value (or not in the set if the
          flag is set to [false]) *)
  | Unop of unop * 'v m_expression_
  | Comparison of comp_op Pos.marked * 'v m_expression_ * 'v m_expression_
  | Binop of binop Pos.marked * 'v m_expression_ * 'v m_expression_
  | Index of 'v Pos.marked * 'v m_expression_
  | Conditional of 'v m_expression_ * 'v m_expression_ * 'v m_expression_ option
  | FuncCall of func Pos.marked * 'v m_expression_ list
  | FuncCallLoop of
      func Pos.marked * 'v loop_variables Pos.marked * 'v m_expression_
  | Literal of literal
  | Var of 'v
  | Loop of 'v loop_variables Pos.marked * 'v m_expression_
      (** The loop is prefixed with the loop variables declarations *)
  | NbCategory of CatVarSet.t Pos.marked
  | Attribut of 'v Pos.marked * string Pos.marked
  | Size of 'v Pos.marked
  | NbAnomalies
  | NbDiscordances
  | NbInformatives
  | NbBloquantes

and 'v m_expression_ = 'v expression_ Pos.marked

let format_literal fmt l =
  Format.pp_print_string fmt
    (match l with Float f -> string_of_float f | Undefined -> "indefini")

let format_atom form_var fmt vl =
  match vl with
  | AtomVar v -> form_var fmt v
  | AtomLiteral l -> format_literal fmt l

let format_set_value_loop form_var fmt sv =
  let form_atom = format_atom form_var in
  match sv with
  | Single l -> Format.fprintf fmt "%a" form_atom (Pos.unmark l)
  | Range (i1, i2) ->
      Format.fprintf fmt "%a..%a" form_atom (Pos.unmark i1) form_atom
        (Pos.unmark i2)
  | Interval (i1, i2) ->
      Format.fprintf fmt "%a-%a" form_atom (Pos.unmark i1) form_atom
        (Pos.unmark i2)

let format_loop_variable_ranges form_var fmt (v, vs) =
  Format.fprintf fmt "un %c dans %a" (Pos.unmark v)
    (Pp.list_comma (format_set_value_loop form_var))
    vs

let format_loop_variable_value_set form_var fmt (v, vs) =
  Format.fprintf fmt "%c=%a" (Pos.unmark v)
    (Pp.list_comma (format_set_value_loop form_var))
    vs

let format_loop_variables form_var fmt lvs =
  match lvs with
  | ValueSets vvs ->
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
        (format_loop_variable_value_set form_var)
        fmt vvs
  | Ranges vvs ->
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " et ")
        (format_loop_variable_ranges form_var)
        fmt vvs

let format_unop fmt op =
  Format.pp_print_string fmt (match op with Not -> "non" | Minus -> "-")

let format_binop fmt op =
  Format.pp_print_string fmt
    (match op with
    | And -> "et"
    | Or -> "ou"
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/")

let format_comp_op fmt op =
  Format.pp_print_string fmt
    (match op with
    | Gt -> ">"
    | Gte -> ">="
    | Lt -> "<"
    | Lte -> "<="
    | Eq -> "="
    | Neq -> "!=")

let format_set_value format_variable fmt sv =
  let open Format in
  match sv with
  | VarValue v -> format_variable fmt (Pos.unmark v)
  | Interval (i1, i2) -> fprintf fmt "%d..%d" (Pos.unmark i1) (Pos.unmark i2)
  | FloatValue i -> fprintf fmt "%f" (Pos.unmark i)

let format_func fmt f =
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

let rec format_expression form_var fmt =
  let form_expr = format_expression form_var in
  function
  | TestInSet (belong, e, values) ->
      Format.fprintf fmt "(%a %sdans %a)" form_expr (Pos.unmark e)
        (if belong then "" else "non ")
        (Pp.list_comma (format_set_value form_var))
        values
  | Comparison (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" form_expr (Pos.unmark e1) format_comp_op
        (Pos.unmark op) form_expr (Pos.unmark e2)
  | Binop (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" form_expr (Pos.unmark e1) format_binop
        (Pos.unmark op) form_expr (Pos.unmark e2)
  | Unop (op, e) ->
      Format.fprintf fmt "%a %a" format_unop op form_expr (Pos.unmark e)
  | Index (v, i) ->
      Format.fprintf fmt "%a[%a]" form_var (Pos.unmark v) form_expr
        (Pos.unmark i)
  | Conditional (e1, e2, e3) ->
      let pp_sinon fmt e = Format.fprintf fmt " sinon %a" form_expr e in
      Format.fprintf fmt "(si %a alors %a%a finsi)" form_expr (Pos.unmark e1)
        form_expr (Pos.unmark e2)
        (Pp.option (Pp.unmark pp_sinon))
        e3
  | FuncCall (f, args) ->
      Format.fprintf fmt "%a(%a)" format_func (Pos.unmark f)
        (Pp.list_space (Pp.unmark form_expr))
        args
  | FuncCallLoop (f, lvs, e) ->
      Format.fprintf fmt "%a(%a%a)" format_func (Pos.unmark f)
        (format_loop_variables form_var)
        (Pos.unmark lvs) form_expr (Pos.unmark e)
  | Literal l -> format_literal fmt l
  | Var v -> form_var fmt v
  | Loop (lvs, e) ->
      Format.fprintf fmt "pour %a%a"
        (format_loop_variables form_var)
        (Pos.unmark lvs) form_expr (Pos.unmark e)
  | NbCategory cs ->
      Format.fprintf fmt "nb_categorie(%a)" (Pp.unmark (CatVarSet.pp ())) cs
  | Attribut (v, a) ->
      Format.fprintf fmt "attribut(%a, %s)" form_var (Pos.unmark v)
        (Pos.unmark a)
  | Size v -> Format.fprintf fmt "taille(%a)" form_var (Pos.unmark v)
  | NbAnomalies -> Format.fprintf fmt "nb_anomalies()"
  | NbDiscordances -> Format.fprintf fmt "nb_discordances()"
  | NbInformatives -> Format.fprintf fmt "nb_informatives()"
  | NbBloquantes -> Format.fprintf fmt "nb_bloquantes()"
