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

type loc_tgv = {
  loc_id : string;
  loc_cat : Com.cat_variable_loc;
  loc_idx : int;
  loc_int : int;
}

type loc =
  | LocTgv of string * loc_tgv
  | LocTmp of string * int
  | LocIt of string * int

let set_loc_int loc loc_int =
  match loc with
  | LocTgv (id, tgv) -> LocTgv (id, { tgv with loc_int })
  | LocTmp (id, _) -> LocTmp (id, loc_int)
  | LocIt (id, _) -> LocIt (id, loc_int)

let set_loc_tgv loc loc_cat loc_idx =
  match loc with
  | LocTgv (id, tgv) -> LocTgv (id, { tgv with loc_cat; loc_idx })
  | LocTmp (id, _) | LocIt (id, _) ->
      Errors.raise_error (Format.sprintf "%s has not a TGV location" id)

module Var = struct
  type id = string

  type tgv = {
    alias : string Pos.marked option;  (** Input variable have an alias *)
    descr : string Pos.marked;
        (** Description taken from the variable declaration *)
    attrs : int Pos.marked StrMap.t;
    cat : Com.cat_variable option;
    typ : Mast.value_typ option;
  }

  type scope = Tgv of tgv | Temp | It

  type t = {
    name : string Pos.marked;  (** The position is the variable declaration *)
    id : id;
    is_table : int option;
    loc : loc;
    scope : scope;
  }

  let tgv v =
    match v.scope with
    | Tgv s -> s
    | _ ->
        Errors.raise_error
          (Format.sprintf "%s is not a TGV variable" (Pos.unmark v.name))

  let alias v = (tgv v).alias

  let alias_str v = Option.fold ~none:"" ~some:Pos.unmark (tgv v).alias

  let descr v = (tgv v).descr

  let descr_str v = Pos.unmark (tgv v).descr

  let attrs v = (tgv v).attrs

  let cat v = (tgv v).cat

  let loc_tgv v =
    match v.loc with
    | LocTgv (_, l) -> l
    | _ ->
        Errors.raise_error
          (Format.sprintf "%s is not a TGV variable" (Pos.unmark v.name))

  let loc_int v =
    match v.loc with
    | LocTgv (_, tgv) -> tgv.loc_int
    | LocTmp (_, li) | LocIt (_, li) -> li

  let is_temp v = v.scope = Temp

  let is_it v = v.scope = It

  let init_loc =
    { loc_id = ""; loc_cat = Com.LocInput; loc_idx = 0; loc_int = 0 }

  let new_tgv ~(name : string Pos.marked) ~(is_table : int option)
      ~(alias : string Pos.marked option) ~(descr : string Pos.marked)
      ~(attrs : int Pos.marked StrMap.t) ~(cat : Com.cat_variable option)
      ~(typ : Mast.value_typ option) : t =
    {
      name;
      id = Pos.unmark name;
      is_table;
      loc = LocTgv (Pos.unmark name, init_loc);
      scope = Tgv { alias; descr; attrs; cat; typ };
    }

  let new_temp ~(name : string Pos.marked) ~(is_table : int option)
      ~(loc_int : int) : t =
    let loc = LocTmp (Pos.unmark name, loc_int) in
    { name; id = Pos.unmark name; is_table; loc; scope = Temp }

  let new_it ~(name : string Pos.marked) ~(is_table : int option)
      ~(loc_int : int) : t =
    let loc = LocIt (Pos.unmark name, loc_int) in
    { name; id = Pos.unmark name; is_table; loc; scope = It }

  let int_of_scope = function Tgv _ -> 0 | Temp -> 1 | It -> 2

  let compare (var1 : t) (var2 : t) =
    let c = compare (int_of_scope var1.scope) (int_of_scope var2.scope) in
    if c <> 0 then c else compare var1.id var2.id
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

let false_literal = Com.Float 0.

let true_literal = Com.Float 1.

type set_value = Var.t Com.set_value

type expression = Var.t Com.expression

let rec map_expr_var (f : 'v -> 'v2) (e : 'v Com.expression) :
    'v2 Com.expression =
  let map = Pos.map_under_mark (map_expr_var f) in
  match e with
  | TestInSet (positive, e, values) ->
      let map_values = function
        | Com.FloatValue f -> Com.FloatValue f
        | Com.VarValue mv -> Com.VarValue (Pos.map_under_mark f mv)
        | Com.Interval (bv, ev) -> Com.Interval (bv, ev)
      in
      TestInSet (positive, map e, List.map map_values values)
  | Unop (op, e) -> Unop (op, map e)
  | Comparison (op, e1, e2) -> Comparison (op, map e1, map e2)
  | Binop (op, e1, e2) -> Binop (op, map e1, map e2)
  | Index ((v, pos), e) -> Index ((f v, pos), map e)
  | Conditional (e1, e2, e3) -> Conditional (map e1, map e2, Option.map map e3)
  | FuncCall (func, es) -> FuncCall (func, List.map map es)
  | Var v -> Var (f v)
  | Literal l -> Literal l
  | NbCategory l -> NbCategory l
  | Attribut (var, a) -> Attribut (Pos.map_under_mark f var, a)
  | Size mv -> Size (Pos.map_under_mark f mv)
  | NbAnomalies -> NbAnomalies
  | NbDiscordances -> NbDiscordances
  | NbInformatives -> NbInformatives
  | NbBloquantes -> NbBloquantes
  | FuncCallLoop _ | Loop _ -> assert false

let rec fold_expr_var (f : 'a -> 'v -> 'a) (acc : 'a) (e : 'v Com.expression) :
    'a =
  let fold acc e = fold_expr_var f acc (Pos.unmark e) in
  match e with
  | TestInSet (_, e, values) ->
      let fold_values acc = function
        | Com.FloatValue _ | Com.Interval _ -> acc
        | Com.VarValue (v, _) -> f acc v
      in
      List.fold_left fold_values (fold acc e) values
  | Unop (_, e) -> fold acc e
  | Comparison (_, e1, e2) | Binop (_, e1, e2) -> fold (fold acc e1) e2
  | Index ((v, _), e) -> fold (f acc v) e
  | Conditional (e1, e2, e3) ->
      let acc = fold (fold acc e1) e2 in
      Option.fold ~none:acc ~some:(fold acc) e3
  | FuncCall (_, es) -> List.fold_left fold acc es
  | Var v -> f acc v
  | Literal _ | NbCategory _ | Attribut _ | Size _ | NbAnomalies
  | NbDiscordances | NbInformatives | NbBloquantes ->
      acc
  | FuncCallLoop _ | Loop _ -> assert false

(** MIR programs are just mapping from variables to their definitions, and make
    a massive use of [VariableMap]. *)
module VariableMap = struct
  include MapExt.Make (Var)

  let pp_key fmt key =
    Format.fprintf fmt "Variable %s%s" (Pos.unmark key.Var.name)
      (match Var.alias key with
      | Some x -> " (alias " ^ Pos.unmark x ^ ")"
      | None -> "")

  let pp ?(sep = ", ") ?(pp_key = pp_key) ?(assoc = " -> ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

module VariableSet = SetExt.Make (Var)

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

type instruction = Var.t Com.instruction

module TargetMap = StrMap

type target_data = {
  target_name : string Pos.marked;
  target_file : string option;
  target_apps : string Pos.marked StrMap.t;
  target_tmp_vars : (Var.t * Pos.t * int option) StrMap.t;
  target_prog : instruction Pos.marked list;
}

(**{1 Verification conditions}*)

type verif_domain_data = { vdom_auth : Com.CatVarSet.t; vdom_verifiable : bool }

type verif_domain = verif_domain_data domain

let cond_cats_to_set cats =
  Com.CatVarMap.fold
    (fun cv nb res -> if nb > 0 then Com.CatVarSet.add cv res else res)
    cats Com.CatVarSet.empty

type stats = {
  nb_calculated : int;
  nb_base : int;
  nb_input : int;
  nb_vars : int;
}

type program = {
  program_safe_prefix : string;
  program_applications : Pos.t StrMap.t;
  program_var_categories : Com.cat_variable_data Com.CatVarMap.t;
  program_rule_domains : rule_domain Mast.DomainIdMap.t;
  program_verif_domains : verif_domain Mast.DomainIdMap.t;
  program_vars : Var.t StrMap.t;
      (** A static register of all variables that can be used during a
          calculation *)
  program_targets : target_data TargetMap.t;
  program_stats : stats;
}

(** {1 Helpers}*)

(** Throws an error in case of alias not found *)
let find_var_name_by_alias (p : program) (alias : string Pos.marked) : string =
  let v =
    StrMap.fold
      (fun _ v acc ->
        match (acc, Var.alias v) with
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

let find_var_by_name (p : program) (name : string Pos.marked) : Var.t =
  try StrMap.find (Pos.unmark name) p.program_vars
  with Not_found -> (
    try
      let name = find_var_name_by_alias p name in
      StrMap.find name p.program_vars
    with Not_found ->
      Errors.raise_spanned_error "unknown variable" (Pos.get_position name))

let rec expand_functions_expr (e : 'var Com.expression Pos.marked) :
    'var Com.expression Pos.marked =
  let open Com in
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
      let new_e3 = Option.map expand_functions_expr e3 in
      Pos.same_pos_as (Conditional (new_e1, new_e2, new_e3)) e
  | Index (var, e1) ->
      let new_e1 = expand_functions_expr e1 in
      Pos.same_pos_as (Index (var, new_e1)) e
  | Literal _ -> e
  | Var _ -> e
  | FuncCall ((SumFunc, _), args) ->
      let expr_opt =
        List.fold_left
          (fun acc_opt arg ->
            match acc_opt with
            | None -> Some (Pos.unmark (expand_functions_expr arg))
            | Some acc ->
                Some
                  (Binop
                     ( Pos.same_pos_as Com.Add e,
                       Pos.same_pos_as acc e,
                       expand_functions_expr arg )))
          None args
      in
      let expr =
        match expr_opt with None -> Literal (Float 0.0) | Some expr -> expr
      in
      Pos.same_pos_as expr e
  | FuncCall ((GtzFunc, _), [ arg ]) ->
      Pos.same_pos_as
        (Comparison
           ( Pos.same_pos_as Com.Gt e,
             expand_functions_expr arg,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FuncCall ((GtezFunc, _), [ arg ]) ->
      Pos.same_pos_as
        (Comparison
           ( Pos.same_pos_as Com.Gte e,
             expand_functions_expr arg,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FuncCall ((((MinFunc | MaxFunc) as f), pos), [ arg1; arg2 ]) ->
      let earg1 = expand_functions_expr arg1 in
      let earg2 = expand_functions_expr arg2 in
      Pos.same_pos_as (FuncCall ((f, pos), [ earg1; earg2 ])) e
  | FuncCall ((AbsFunc, pos), [ arg ]) ->
      Pos.same_pos_as
        (FuncCall ((AbsFunc, pos), [ expand_functions_expr arg ]))
        e
  | FuncCall ((NullFunc, _), [ arg ]) ->
      Pos.same_pos_as
        (Comparison
           ( Pos.same_pos_as Com.Eq e,
             expand_functions_expr arg,
             Pos.same_pos_as (Literal (Float 0.0)) e ))
        e
  | FuncCall ((PresentFunc, pos), [ arg ]) ->
      (* we do not expand this function as it deals specifically with undefined
         variables *)
      Pos.same_pos_as
        (FuncCall ((PresentFunc, pos), [ expand_functions_expr arg ]))
        e
  | FuncCall ((ArrFunc, pos), [ arg ]) ->
      (* we do not expand this function as it requires modulo or modf *)
      Pos.same_pos_as
        (FuncCall ((ArrFunc, pos), [ expand_functions_expr arg ]))
        e
  | FuncCall ((InfFunc, pos), [ arg ]) ->
      (* we do not expand this function as it requires modulo or modf *)
      Pos.same_pos_as
        (FuncCall ((InfFunc, pos), [ expand_functions_expr arg ]))
        e
  | _ -> e

let expand_functions (p : program) : program =
  let open Com in
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
          let i' = expand_functions_expr i in
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
                | Com.PrintIndent e ->
                    let e' = expand_functions_expr e in
                    (Com.PrintIndent e', arg_pos)
                | Com.PrintExpr (e, mi, ma) ->
                    let e' = expand_functions_expr e in
                    (Com.PrintExpr (e', mi, ma), arg_pos)
                | Com.PrintString _ | Com.PrintName _ | Com.PrintAlias _ ->
                    m_arg)
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
