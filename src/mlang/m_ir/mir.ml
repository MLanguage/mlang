(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr> Raphaël Monat <raphael.monat@lip6.fr>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

(** Main data structure for M analysis *)

(**{1 Variables} *)

(** Variables are first-class objects *)

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

type variable_id = string
(** Each variable has an unique ID *)

module Variable = struct
  type id = variable_id

  type t = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    alias : string Pos.marked option;  (** Input variable have an alias *)
    id : variable_id;
    descr : string Pos.marked;
        (** Description taken from the variable declaration *)
    attributes : int Pos.marked StrMap.t;
    cats : cat_variable option;
    typ : Mast.value_typ option;
    is_table : int option;
    is_temp : bool;
    is_it : bool;
  }

  let new_var (name : string Pos.marked) (alias : string Pos.marked option)
      (descr : string Pos.marked) ~(attributes : int Pos.marked StrMap.t)
      ~(cats : cat_variable option) ~(typ : Mast.value_typ option)
      ~(is_table : int option) ~(is_temp : bool) ~(is_it : bool) : t =
    {
      name;
      id = Pos.unmark name;
      descr;
      alias;
      attributes;
      cats;
      typ;
      is_table;
      is_temp;
      is_it;
    }

  let compare (var1 : t) (var2 : t) = compare var1.id var2.id
end

(** Local variables don't appear in the M source program but can be introduced
    by let bindings when translating to MIR. They should be De Bruijn indices
    but instead are unique globals identifiers out of laziness. *)

type local_variable = { id : int }

module LocalVariable = struct
  type t = local_variable = { id : int }

  let counter : int ref = ref 0

  let fresh_id () : int =
    let v = !counter in
    counter := !counter + 1;
    v

  let new_var () : t = { id = fresh_id () }

  let compare (var1 : t) (var2 : t) = compare var1.id var2.id
end

(** Type of MIR values *)
type typ = Real

type literal = Float of float | Undefined

let false_literal = Float 0.

let true_literal = Float 1.

(** MIR only supports a restricted set of functions *)
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

type 'v set_value_ =
  | FloatValue of float Pos.marked
  | VarValue of 'v Pos.marked
  | Interval of int Pos.marked * int Pos.marked

type 'v expression_ =
  | TestInSet of bool * 'v m_expression_ * 'v set_value_ list
      (** Test if an expression is in a set of value (or not in the set if the
          flag is set to [false]) *)
  | Unop of Mast.unop * 'v m_expression_
  | Comparison of Mast.comp_op Pos.marked * 'v m_expression_ * 'v m_expression_
  | Binop of Mast.binop Pos.marked * 'v m_expression_ * 'v m_expression_
  | Index of 'v Pos.marked * 'v m_expression_
  | Conditional of 'v m_expression_ * 'v m_expression_ * 'v m_expression_
  | FunctionCall of func * 'v m_expression_ list
  | Literal of literal
  | Var of 'v Pos.marked
  | NbCategory of CatVarSet.t
  | Attribut of string Pos.marked * 'v * string Pos.marked
  | Size of 'v Pos.marked
  | NbAnomalies
  | NbDiscordances
  | NbInformatives
  | NbBloquantes

and 'v m_expression_ = 'v expression_ Pos.marked

type set_value = Variable.t set_value_

type expression = Variable.t expression_

let rec map_expr_var (f : 'v -> 'v2) (e : 'v expression_) : 'v2 expression_ =
  let map = Pos.map_under_mark (map_expr_var f) in
  match (e :> 'v expression_) with
  | TestInSet (positive, e, values) ->
      let map_values = function
        | FloatValue f -> FloatValue f
        | VarValue mv -> VarValue (Pos.map_under_mark f mv)
        | Interval (bv, ev) -> Interval (bv, ev)
      in
      TestInSet (positive, map e, List.map map_values values)
  | Unop (op, e) -> Unop (op, map e)
  | Comparison (op, e1, e2) -> Comparison (op, map e1, map e2)
  | Binop (op, e1, e2) -> Binop (op, map e1, map e2)
  | Index ((v, pos), e) -> Index ((f v, pos), map e)
  | Conditional (e1, e2, e3) -> Conditional (map e1, map e2, map e3)
  | FunctionCall (func, es) -> FunctionCall (func, List.map map es)
  | Var mv -> Var (Pos.map_under_mark f mv)
  | Literal l -> Literal l
  | NbCategory l -> NbCategory l
  | Attribut (v, var, a) -> Attribut (v, f var, a)
  | Size mv -> Size (Pos.map_under_mark f mv)
  | NbAnomalies -> NbAnomalies
  | NbDiscordances -> NbDiscordances
  | NbInformatives -> NbInformatives
  | NbBloquantes -> NbBloquantes

let rec fold_expr_var (f : 'a -> 'v -> 'a) (acc : 'a) (e : 'v expression_) : 'a
    =
  let fold acc e = fold_expr_var f acc (Pos.unmark e) in
  match (e :> 'v expression_) with
  | TestInSet (_, e, values) ->
      let fold_values acc = function
        | FloatValue _ | Interval _ -> acc
        | VarValue (v, _) -> f acc v
      in
      List.fold_left fold_values (fold acc e) values
  | Unop (_, e) -> fold acc e
  | Comparison (_, e1, e2) | Binop (_, e1, e2) -> fold (fold acc e1) e2
  | Index ((v, _), e) -> fold (f acc v) e
  | Conditional (e1, e2, e3) -> fold (fold (fold acc e1) e2) e3
  | FunctionCall (_, es) -> List.fold_left fold acc es
  | Var v -> f acc (Pos.unmark v)
  | Literal _ | NbCategory _ | Attribut _ | Size _ | NbAnomalies
  | NbDiscordances | NbInformatives | NbBloquantes ->
      acc

(** MIR programs are just mapping from variables to their definitions, and make
    a massive use of [VariableMap]. *)
module VariableMap = struct
  include MapExt.Make (Variable)

  let pp_key fmt key =
    Format.fprintf fmt "Variable %s%s"
      (Pos.unmark key.Variable.name)
      (match key.alias with
      | Some x -> " (alias " ^ Pos.unmark x ^ ")"
      | None -> "")

  let pp ?(sep = ", ") ?(pp_key = pp_key) ?(assoc = " -> ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

module VariableSet = SetExt.Make (Variable)

module LocalVariableMap = struct
  include MapExt.Make (LocalVariable)

  let pp_key fmt key = Format.fprintf fmt "%d" key.id

  let pp ?(sep = ", ") ?(pp_key = pp_key) ?(assoc = " -> ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

(** This map is used to store the definitions of all the cells of a table
    variable that is not not defined generically *)
module IndexMap = struct
  include IntMap

  let pp ?(sep = ", ") ?(pp_key = Format.pp_print_int) ?(assoc = " -> ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

(** The definitions here are modeled closely to the source M language. One could
    also adopt a more lambda-calculus-compatible model with functions used to
    model tables. *)

type 'a domain = {
  dom_id : Mast.DomainId.t Pos.marked;
  dom_names : Pos.t Mast.DomainIdMap.t;
  dom_by_default : bool;
  dom_min : Mast.DomainIdSet.t;
  dom_max : Mast.DomainIdSet.t;
  dom_rov : IntSet.t;
  dom_data : 'a;
  dom_used : int Pos.marked option;
}

type rule_domain_data = { rdom_computable : bool }

type rule_domain = rule_domain_data domain

type 'variable print_arg =
  | PrintString of string
  | PrintName of string Pos.marked * Variable.t
  | PrintAlias of string Pos.marked * Variable.t
  | PrintIndent of 'variable expression_ Pos.marked
  | PrintExpr of 'variable expression_ Pos.marked * int * int

(** Errors are first-class objects *)

type error = {
  name : string Pos.marked;  (** The position is the variable declaration *)
  kind : string Pos.marked;
  major_code : string Pos.marked;
  minor_code : string Pos.marked;
  description : string Pos.marked;
  isisf : string Pos.marked;
  typ : Mast.error_typ;
}

module Error = struct
  type t = error = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    kind : string Pos.marked;
    major_code : string Pos.marked;
    minor_code : string Pos.marked;
    description : string Pos.marked;
    isisf : string Pos.marked;
    typ : Mast.error_typ;
  }

  let new_error (name : string Pos.marked) (error : Mast.error_)
      (error_typ : Mast.error_typ) : t =
    {
      name;
      kind = List.nth error.error_descr 0;
      major_code = List.nth error.error_descr 1;
      minor_code = List.nth error.error_descr 2;
      description = List.nth error.error_descr 3;
      isisf =
        (match List.nth_opt error.error_descr 4 with
        | Some s -> s
        | None -> ("", Pos.no_pos));
      typ = error_typ;
    }

  let err_descr_string (err : t) =
    Pos.same_pos_as
      (String.concat ":"
         [
           err.kind |> Pos.unmark;
           err.major_code |> Pos.unmark;
           err.minor_code |> Pos.unmark;
           err.description |> Pos.unmark;
           err.isisf |> Pos.unmark;
         ])
      err.name

  let compare (var1 : t) (var2 : t) = compare var1.name var2.name
end

type instruction =
  | Affectation of
      variable_id * (int * expression Pos.marked) option * expression Pos.marked
  | IfThenElse of
      expression * instruction Pos.marked list * instruction Pos.marked list
  | ComputeTarget of string Pos.marked
  | VerifBlock of instruction Pos.marked list
  | Print of Mast.print_std * Variable.t print_arg Pos.marked list
  | Iterate of
      variable_id
      * CatVarSet.t
      * expression Pos.marked
      * instruction Pos.marked list
  | Restore of
      Pos.t VariableMap.t
      * (Variable.t * CatVarSet.t * expression Pos.marked) list
      * instruction Pos.marked list
  | RaiseError of error * string option
  | CleanErrors
  | ExportErrors
  | FinalizeErrors

module TargetMap = StrMap

type target_data = {
  target_name : string Pos.marked;
  target_file : string option;
  target_apps : string Pos.marked StrMap.t;
  target_tmp_vars : (Variable.t * Pos.t * int option) StrMap.t;
  target_prog : instruction Pos.marked list;
}

(**{1 Verification conditions}*)

type verif_domain_data = { vdom_auth : CatVarSet.t; vdom_verifiable : bool }

type verif_domain = verif_domain_data domain

let cond_cats_to_set cats =
  CatVarMap.fold
    (fun cv nb res -> if nb > 0 then CatVarSet.add cv res else res)
    cats CatVarSet.empty

type program = {
  program_safe_prefix : string;
  program_applications : Pos.t StrMap.t;
  program_var_categories : cat_variable_data CatVarMap.t;
  program_rule_domains : rule_domain Mast.DomainIdMap.t;
  program_verif_domains : verif_domain Mast.DomainIdMap.t;
  program_vars : Variable.t StrMap.t;
      (** A static register of all variables that can be used during a
          calculation *)
  program_targets : target_data TargetMap.t;
}

(** {1 Helpers}*)

(** Throws an error in case of alias not found *)
let find_var_name_by_alias (p : program) (alias : string Pos.marked) : string =
  let v =
    StrMap.fold
      (fun _ v acc ->
        match (acc, v.Variable.alias) with
        | Some _, _ | None, None -> acc
        | None, Some v_alias ->
            if Pos.unmark v_alias = Pos.unmark alias then
              Some (Pos.unmark v.name)
            else None)
      p.program_vars None
  in
  match v with
  | Some v -> v
  | None ->
      Errors.raise_spanned_error
        (Format.asprintf "alias not found: %s" (Pos.unmark alias))
        (Pos.get_position alias)

let find_var_by_name (p : program) (name : string Pos.marked) : Variable.t =
  try StrMap.find (Pos.unmark name) p.program_vars
  with Not_found -> (
    try
      let name = find_var_name_by_alias p name in
      StrMap.find name p.program_vars
    with Not_found ->
      Errors.raise_spanned_error "unknown variable" (Pos.get_position name))

let mast_to_catvar (cats : 'a CatVarMap.t)
    (l : string Pos.marked list Pos.marked) : cat_variable =
  match l with
  | ("saisie", _) :: id, pos ->
      let vcat = CatInput (StrSet.from_marked_list id) in
      if CatVarMap.mem vcat cats then vcat
      else Errors.raise_spanned_error "unknown variable category" pos
  | ("calculee", _) :: id, id_pos -> begin
      match id with
      | [] -> CatComputed CatCompSet.empty
      | [ ("base", _) ] -> CatComputed (CatCompSet.singleton Base)
      | [ ("restituee", _) ] -> CatComputed (CatCompSet.singleton GivenBack)
      | [ ("base", _); ("restituee", _) ] | [ ("restituee", _); ("base", _) ] ->
          CatComputed (CatCompSet.singleton Base |> CatCompSet.add GivenBack)
      | _ ->
          Errors.raise_spanned_error "unlnown calculated variable category"
            id_pos
    end
  | _, pos -> Errors.raise_spanned_error "unknown variable category" pos

let rec expand_functions_expr (e : 'var expression_ Pos.marked) :
    'var expression_ Pos.marked =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      Pos.same_pos_as (Comparison (op, new_e1, new_e2)) e
  | Binop (op, e1, e2) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      Pos.same_pos_as (Binop (op, new_e1, new_e2)) e
  | Unop (op, e1) ->
      let new_e1 = expand_functions_expr e1 in
      Pos.same_pos_as (Unop (op, new_e1)) e
  | Conditional (e1, e2, e3) ->
      let new_e1 = expand_functions_expr e1 in
      let new_e2 = expand_functions_expr e2 in
      let new_e3 = expand_functions_expr e3 in
      Pos.same_pos_as (Conditional (new_e1, new_e2, new_e3)) e
  | Index (var, e1) ->
      let new_e1 = expand_functions_expr e1 in
      Pos.same_pos_as (Index (var, new_e1)) e
  | Literal _ -> e
  | Var _ -> e
  | FunctionCall (SumFunc, args) ->
      let expr_opt =
        List.fold_left
          (fun acc_opt arg ->
            match acc_opt with
            | None -> Some (Pos.unmark (expand_functions_expr arg))
            | Some acc ->
                Some
                  (Binop
                     ( Pos.same_pos_as Mast.Add e,
                       Pos.same_pos_as acc e,
                       expand_functions_expr arg )))
          None args
      in
      let expr =
        match expr_opt with None -> Literal (Float 0.0) | Some expr -> expr
      in
      Pos.same_pos_as expr e
  | FunctionCall (GtzFunc, [ arg ]) ->
      Pos.same_pos_as
        (Comparison
           ( Pos.same_pos_as Mast.Gt e,
             expand_functions_expr arg,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FunctionCall (GtezFunc, [ arg ]) ->
      Pos.same_pos_as
        (Comparison
           ( Pos.same_pos_as Mast.Gte e,
             expand_functions_expr arg,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FunctionCall (((MinFunc | MaxFunc) as f), [ arg1; arg2 ]) ->
      let earg1 = expand_functions_expr arg1 in
      let earg2 = expand_functions_expr arg2 in
      Pos.same_pos_as (FunctionCall (f, [ earg1; earg2 ])) e
  | FunctionCall (AbsFunc, [ arg ]) ->
      Pos.same_pos_as (FunctionCall (AbsFunc, [ expand_functions_expr arg ])) e
  | FunctionCall (NullFunc, [ arg ]) ->
      Pos.same_pos_as
        (Comparison
           ( Pos.same_pos_as Mast.Eq e,
             expand_functions_expr arg,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FunctionCall (PresentFunc, [ arg ]) ->
      (* we do not expand this function as it deals specifically with undefined
         variables *)
      Pos.same_pos_as
        (FunctionCall (PresentFunc, [ expand_functions_expr arg ]))
        e
  | FunctionCall (ArrFunc, [ arg ]) ->
      (* we do not expand this function as it requires modulo or modf *)
      Pos.same_pos_as (FunctionCall (ArrFunc, [ expand_functions_expr arg ])) e
  | FunctionCall (InfFunc, [ arg ]) ->
      (* we do not expand this function as it requires modulo or modf *)
      Pos.same_pos_as (FunctionCall (InfFunc, [ expand_functions_expr arg ])) e
  | _ -> e

let expand_functions (p : program) : program =
  let program_targets =
    let rec map_instr m_instr =
      let instr, instr_pos = m_instr in
      match instr with
      | Affectation (v_id, v_idx_opt, v_expr) ->
          let m_idx_opt =
            match v_idx_opt with
            | Some (sz, v_idx) -> Some (sz, expand_functions_expr v_idx)
            | None -> None
          in
          let m_expr = expand_functions_expr v_expr in
          (Affectation (v_id, m_idx_opt, m_expr), instr_pos)
      | IfThenElse (i, t, e) ->
          let i' = Pos.unmark (expand_functions_expr (i, Pos.no_pos)) in
          let t' = List.map map_instr t in
          let e' = List.map map_instr e in
          (IfThenElse (i', t', e'), instr_pos)
      | ComputeTarget _ -> m_instr
      | VerifBlock instrs ->
          let instrs' = List.map map_instr instrs in
          (VerifBlock instrs', instr_pos)
      | Print (out, pr_args) ->
          let pr_args' =
            List.map
              (fun m_arg ->
                let arg, arg_pos = m_arg in
                match arg with
                | PrintIndent e ->
                    let e' = expand_functions_expr e in
                    (PrintIndent e', arg_pos)
                | PrintExpr (e, mi, ma) ->
                    let e' = expand_functions_expr e in
                    (PrintExpr (e', mi, ma), arg_pos)
                | PrintString _ | PrintName _ | PrintAlias _ -> m_arg)
              pr_args
          in
          (Print (out, pr_args'), instr_pos)
      | Iterate (v_id, cats, e, instrs) ->
          let e' = expand_functions_expr e in
          let instrs' = List.map map_instr instrs in
          (Iterate (v_id, cats, e', instrs'), instr_pos)
      | Restore (vars, filters, instrs) ->
          let filters' =
            List.map
              (fun (v, cs, e) -> (v, cs, expand_functions_expr e))
              filters
          in
          let instrs' = List.map map_instr instrs in
          (Restore (vars, filters', instrs'), instr_pos)
      | RaiseError _ | CleanErrors | ExportErrors | FinalizeErrors -> m_instr
    in
    TargetMap.map
      (fun t ->
        let target_prog = List.map map_instr t.target_prog in
        { t with target_prog })
      p.program_targets
  in
  { p with program_targets }
