module CatVar = struct
  type t = Input of StrSet.t | Computed of { is_base : bool }

  let pp fmt = function
    | Input id ->
        let pp fmt set = StrSet.iter (Format.fprintf fmt " %s") set in
        Format.fprintf fmt "saisie%a" pp id
    | Computed id ->
        Format.fprintf fmt "calculee%s" (if id.is_base then " base" else "")

  let compare a b =
    match (a, b) with
    | Input _, Computed _ -> 1
    | Computed _, Input _ -> -1
    | Input id0, Input id1 -> StrSet.compare id0 id1
    | Computed c0, Computed c1 -> compare c0.is_base c1.is_base

  type cat_var_t = t

  let cat_var_pp = pp

  let cat_var_compare = compare

  module Set = struct
    include SetExt.Make (struct
      type t = cat_var_t

      let compare = cat_var_compare
    end)

    let pp ?(sep = ", ") ?(pp_elt = cat_var_pp) (_ : unit)
        (fmt : Format.formatter) (set : t) : unit =
      pp ~sep ~pp_elt () fmt set
  end

  module Map = struct
    include MapExt.Make (struct
      type t = cat_var_t

      let compare = cat_var_compare
    end)

    let pp ?(sep = "; ") ?(pp_key = cat_var_pp) ?(assoc = " => ")
        (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
        (map : 'a t) : unit =
      pp ~sep ~pp_key ~assoc pp_val fmt map
  end

  type loc = LocCalculated | LocBase | LocInput

  type data = {
    id : t;
    id_str : string;
    id_int : int;
    loc : loc;
    pos : Pos.t;
    attributs : Pos.t StrMap.t;
  }
end

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

type 'v expression =
  | TestInSet of bool * 'v m_expression * 'v set_value list
      (** Test if an expression is in a set of value (or not in the set if the
          flag is set to [false]) *)
  | Unop of unop * 'v m_expression
  | Comparison of comp_op Pos.marked * 'v m_expression * 'v m_expression
  | Binop of binop Pos.marked * 'v m_expression * 'v m_expression
  | Index of 'v Pos.marked * 'v m_expression
  | Conditional of 'v m_expression * 'v m_expression * 'v m_expression option
  | FuncCall of func Pos.marked * 'v m_expression list
  | FuncCallLoop of
      func Pos.marked * 'v loop_variables Pos.marked * 'v m_expression
  | Literal of literal
  | Var of 'v
  | Loop of 'v loop_variables Pos.marked * 'v m_expression
      (** The loop is prefixed with the loop variables declarations *)
  | NbCategory of Pos.t CatVar.Map.t
  | Attribut of 'v Pos.marked * string Pos.marked
  | Size of 'v Pos.marked
  | NbAnomalies
  | NbDiscordances
  | NbInformatives
  | NbBloquantes

and 'v m_expression = 'v expression Pos.marked

module Error = struct
  type typ = Anomaly | Discordance | Information

  let compare_typ e1 e2 =
    match (e1, e2) with
    | Anomaly, (Discordance | Information) -> -1
    | (Discordance | Information), Anomaly -> 1
    | Information, Discordance -> -1
    | Discordance, Information -> 1
    | _ -> 0

  type t = {
    name : string Pos.marked;
    kind : string Pos.marked;
    major_code : string Pos.marked;
    minor_code : string Pos.marked;
    description : string Pos.marked;
    isisf : string Pos.marked;
    typ : typ;
  }

  let pp_descr fmt err =
    Format.fprintf fmt "%s:%s:%s:%s:%s" (Pos.unmark err.kind)
      (Pos.unmark err.major_code)
      (Pos.unmark err.minor_code)
      (Pos.unmark err.description)
      (Pos.unmark err.isisf)

  let compare (var1 : t) (var2 : t) = compare var1.name var2.name
end

type print_std = StdOut | StdErr

type 'v print_arg =
  | PrintString of string
  | PrintName of 'v Pos.marked
  | PrintAlias of 'v Pos.marked
  | PrintIndent of 'v m_expression
  | PrintExpr of 'v m_expression * int * int

type 'v instruction =
  | Affectation of 'v * (int * 'v m_expression) option * 'v m_expression
  | IfThenElse of
      'v m_expression * 'v m_instruction list * 'v m_instruction list
  | ComputeTarget of string Pos.marked
  | VerifBlock of 'v m_instruction list
  | Print of print_std * 'v print_arg Pos.marked list
  | Iterate of 'v * Pos.t CatVar.Map.t * 'v m_expression * 'v m_instruction list
  | Restore of
      'v list
      * ('v * Pos.t CatVar.Map.t * 'v m_expression) list
      * 'v m_instruction list
  | RaiseError of Error.t * string Pos.marked option
  | CleanErrors
  | ExportErrors
  | FinalizeErrors

and 'v m_instruction = 'v instruction Pos.marked

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
      Format.fprintf fmt "nb_categorie(%a)" (CatVar.Map.pp_keys ()) cs
  | Attribut (v, a) ->
      Format.fprintf fmt "attribut(%a, %s)" form_var (Pos.unmark v)
        (Pos.unmark a)
  | Size v -> Format.fprintf fmt "taille(%a)" form_var (Pos.unmark v)
  | NbAnomalies -> Format.fprintf fmt "nb_anomalies()"
  | NbDiscordances -> Format.fprintf fmt "nb_discordances()"
  | NbInformatives -> Format.fprintf fmt "nb_informatives()"
  | NbBloquantes -> Format.fprintf fmt "nb_bloquantes()"

let format_print_arg form_var fmt = function
  | PrintString s -> Format.fprintf fmt "\"%s\"" s
  | PrintName v -> Format.fprintf fmt "nom(%a)" (Pp.unmark form_var) v
  | PrintAlias v -> Format.fprintf fmt "alias(%a)" (Pp.unmark form_var) v
  | PrintIndent e ->
      Format.fprintf fmt "indenter(%a)"
        (Pp.unmark (format_expression form_var))
        e
  | PrintExpr (e, min, max) ->
      if min = max_int then
        Format.fprintf fmt "(%a)" (Pp.unmark (format_expression form_var)) e
      else if max = max_int then
        Format.fprintf fmt "(%a):%d"
          (Pp.unmark (format_expression form_var))
          e min
      else
        Format.fprintf fmt "(%a):%d..%d"
          (Pp.unmark (format_expression form_var))
          e min max

let rec format_instruction form_var =
  let form_expr = format_expression form_var in
  let form_instrs = format_instructions form_var in
  fun fmt instr ->
    match instr with
    | Affectation (v, vi_opt, ve) ->
        let pr_idx fmt = function
          | Some (_, vi) -> Format.fprintf fmt "[%a]" form_expr (Pos.unmark vi)
          | None -> ()
        in
        Format.fprintf fmt "%a%a = %a" form_var v pr_idx vi_opt form_expr
          (Pos.unmark ve)
    | IfThenElse (cond, t, []) ->
        Format.fprintf fmt "if(%a):@\n@[<h 2>  %a@]@\n" form_expr
          (Pos.unmark cond) form_instrs t
    | IfThenElse (cond, t, f) ->
        Format.fprintf fmt "if(%a):@\n@[<h 2>  %a@]else:@\n@[<h 2>  %a@]@\n"
          form_expr (Pos.unmark cond) form_instrs t form_instrs f
    | VerifBlock vb ->
        Format.fprintf fmt
          "@[<v 2># debut verif block@\n%a@]@\n# fin verif block@\n" form_instrs
          vb
    | ComputeTarget tname ->
        Format.fprintf fmt "call_target: %s@," (Pos.unmark tname)
    | Print (std, args) ->
        let print_cmd =
          match std with StdOut -> "afficher" | StdErr -> "afficher_erreur"
        in
        Format.fprintf fmt "%s %a;" print_cmd
          (Pp.list_space (Pp.unmark (format_print_arg form_var)))
          args
    | Iterate (var, vcs, expr, itb) ->
        Format.fprintf fmt
          "iterate variable %a@\n: categorie %a@\n: avec %a@\n: dans (" form_var
          var (CatVar.Map.pp_keys ()) vcs form_expr (Pos.unmark expr);
        Format.fprintf fmt "@[<h 2>  %a@]@\n)@\n" form_instrs itb
    | Restore (vars, var_params, rb) ->
        let format_var_param fmt (var, vcs, expr) =
          Format.fprintf fmt ": variable %a : categorie %a : avec %a@\n"
            form_var var (CatVar.Map.pp_keys ()) vcs form_expr (Pos.unmark expr)
        in
        Format.fprintf fmt "restaure@;: %a@\n%a: apres ("
          (Pp.list_comma form_var) vars
          (Pp.list_space format_var_param)
          var_params;
        Format.fprintf fmt "@[<h 2>  %a@]@\n)@\n" form_instrs rb
    | RaiseError (err, var_opt) ->
        Format.fprintf fmt "leve_erreur %s %s\n" (Pos.unmark err.name)
          (match var_opt with Some var -> " " ^ Pos.unmark var | None -> "")
    | CleanErrors -> Format.fprintf fmt "nettoie_erreurs\n"
    | ExportErrors -> Format.fprintf fmt "exporte_erreurs\n"
    | FinalizeErrors -> Format.fprintf fmt "finalise_erreurs\n"

and format_instructions form_var fmt instrs =
  Pp.list "" (Pp.unmark (format_instruction form_var)) fmt instrs
