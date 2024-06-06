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

type set_value = Com.Var.t Com.set_value

type expression = Com.Var.t Com.expression

(** MIR programs are just mapping from variables to their definitions, and make
    a massive use of [VariableMap]. *)
module VariableMap = struct
  include MapExt.Make (Com.Var)

  let pp_key fmt key =
    Format.fprintf fmt "Variable %s%s"
      (Pos.unmark key.Com.Var.name)
      (match Com.Var.alias key with
      | Some x -> " (alias " ^ Pos.unmark x ^ ")"
      | None -> "")

  let pp ?(sep = ", ") ?(pp_key = pp_key) ?(assoc = " -> ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

module VariableSet = SetExt.Make (Com.Var)

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

type instruction = (Com.Var.t, Com.Error.t) Com.instruction

type m_instruction = instruction Pos.marked

module TargetMap = StrMap

type target_data = {
  target_name : string Pos.marked;
  target_file : string option;
  target_apps : string Pos.marked StrMap.t;
  target_args : (Com.Var.t * Pos.t) list;
  target_result : (Com.Var.t * Pos.t) option;
  target_tmp_vars : (Com.Var.t * Pos.t * int option) StrMap.t;
  target_nb_tmps : int;
  target_sz_tmps : int;
  target_nb_refs : int;
  target_prog : m_instruction list;
}

(**{1 Verification conditions}*)

type verif_domain_data = {
  vdom_auth : Pos.t Com.CatVar.Map.t;
  vdom_verifiable : bool;
}

type verif_domain = verif_domain_data domain

type stats = {
  nb_calculated : int;
  nb_base : int;
  nb_input : int;
  nb_vars : int;
  nb_all_tmps : int;
  nb_all_refs : int;
  sz_calculated : int;
  sz_base : int;
  sz_input : int;
  sz_vars : int;
  sz_all_tmps : int;
}

type program = {
  program_safe_prefix : string;
  program_applications : Pos.t StrMap.t;
  program_var_categories : Com.CatVar.data Com.CatVar.Map.t;
  program_rule_domains : rule_domain Mast.DomainIdMap.t;
  program_verif_domains : verif_domain Mast.DomainIdMap.t;
  program_vars : Com.Var.t StrMap.t;
      (** A static register of all variables that can be used during a
          calculation *)
  program_functions : target_data TargetMap.t;
  program_targets : target_data TargetMap.t;
  program_main_target : string;
  program_stats : stats;
}

(** {1 Helpers}*)

(** Throws an error in case of alias not found *)
let find_var_name_by_alias (p : program) (alias : string Pos.marked) : string =
  let v =
    StrMap.fold
      (fun _ v acc ->
        match (acc, Com.Var.alias v) with
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

let find_var_by_name (p : program) (name : string Pos.marked) : Com.Var.t =
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
      | Affectation (SingleFormula (v_id, v_idx_opt, v_expr), pos) ->
          let m_idx_opt =
            match v_idx_opt with
            | Some v_idx -> Some (expand_functions_expr v_idx)
            | None -> None
          in
          let m_expr = expand_functions_expr v_expr in
          (Affectation (SingleFormula (v_id, m_idx_opt, m_expr), pos), instr_pos)
      | Affectation _ -> assert false
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
      | Iterate (v_id, vars, var_params, instrs) ->
          let var_params' =
            List.map
              (fun (cats, e) ->
                let e' = expand_functions_expr e in
                (cats, e'))
              var_params
          in
          let instrs' = List.map map_instr instrs in
          (Iterate (v_id, vars, var_params', instrs'), instr_pos)
      | Restore (vars, filters, instrs) ->
          let filters' =
            List.map
              (fun (v, cs, e) -> (v, cs, expand_functions_expr e))
              filters
          in
          let instrs' = List.map map_instr instrs in
          (Restore (vars, filters', instrs'), instr_pos)
      | RaiseError _ | CleanErrors | ExportErrors | FinalizeErrors -> m_instr
      | ComputeDomain _ | ComputeChaining _ | ComputeVerifs _ -> assert false
    in
    TargetMap.map
      (fun t ->
        let target_prog = List.map map_instr t.target_prog in
        { t with target_prog })
      p.program_targets
  in
  { p with program_targets }
