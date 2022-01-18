(* Copyright (C) 2019-2021-2020 Inria, contributors: Denis Merigoux
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

type rule_id = Mir.rule_id

module RuleMap = Mir.RuleMap

type variable = Mir.Variable.t

type variable_id = Mir.variable_id

module VariableMap = Mir.VariableMap
module VariableDict = Mir.VariableDict

(* unify SSA variables *)
let var_from_mir (v : Mir.Variable.t) : variable =
  match v.origin with Some v -> v | None -> v

let var_to_mir v = v

let compare_variable = Mir.Variable.compare

let map_from_mir_map map =
  Mir.VariableMap.fold
    (fun var -> VariableMap.add (var_from_mir var))
    map VariableMap.empty

let dict_from_mir_dict dict =
  Mir.VariableDict.fold
    (fun var -> VariableDict.add (var_from_mir var))
    dict VariableDict.empty

type expression = variable Mir.expression_

type condition_data = variable Mir.condition_data_

type variable_def = variable Mir.variable_def_

type variable_data = variable Mir.variable_data_

type function_name = string

type rule = { rule_id : rule_id; rule_name : string; rule_stmts : stmt list }

and stmt = stmt_kind Pos.marked

and stmt_kind =
  | SAssign of variable * variable_data
  | SConditional of expression * stmt list * stmt list
  | SVerif of condition_data
  | SRuleCall of rule_id
  | SFunctionCall of function_name * Mir.Variable.t list

type mpp_function = stmt list

module FunctionMap = Map.Make (struct
  type t = function_name

  let compare = String.compare
end)

type program = {
  mpp_functions : mpp_function FunctionMap.t;
  rules : rule RuleMap.t;
  main_function : function_name;
  idmap : Mir.idmap;
  mir_program : Mir.program;
  outputs : unit VariableMap.t;
}

let main_statements (p : program) : stmt list =
  try FunctionMap.find p.main_function p.mpp_functions
  with Not_found ->
    Errors.raise_error "Unable to find main function of Bir program"

let rec get_block_statements (p : program) (stmts : stmt list) : stmt list =
  List.fold_left
    (fun stmts stmt ->
      match Pos.unmark stmt with
      | SRuleCall r -> List.rev (RuleMap.find r p.rules).rule_stmts @ stmts
      | SConditional (e, t, f) ->
          let t = get_block_statements p t in
          let f = get_block_statements p f in
          Pos.same_pos_as (SConditional (e, t, f)) stmt :: stmts
      | SFunctionCall (f, _) ->
          (get_block_statements p (FunctionMap.find f p.mpp_functions)
          |> List.rev)
          @ stmts
      | _ -> stmt :: stmts)
    [] stmts
  |> List.rev

(** Returns program statements with all rules inlined *)
let get_all_statements (p : program) : stmt list =
  main_statements p |> get_block_statements p

let rec count_instr_blocks (p : program) (stmts : stmt list) : int =
  List.fold_left
    (fun acc stmt ->
      match Pos.unmark stmt with
      | SAssign _ | SVerif _ | SRuleCall _ | SFunctionCall _ -> acc + 1
      | SConditional (_, s1, s2) ->
          acc + 1 + count_instr_blocks p s1 + count_instr_blocks p s2)
    0 stmts

let squish_statements (program : program) (threshold : int)
    (rule_suffix : string) =
  let rule_from_stmts stmts =
    let id = Mir.fresh_rule_id () in
    {
      rule_id = id;
      rule_name = rule_suffix ^ string_of_int id;
      rule_stmts = List.rev stmts;
    }
  in
  let rec browse_bir (old_stmts : stmt list) (new_stmts : stmt list)
      (curr_stmts : stmt list) (rules : rule RuleMap.t) =
    match old_stmts with
    | [] -> (rules, List.rev (curr_stmts @ new_stmts))
    | hd :: tl ->
        let give_pos stmt = Pos.same_pos_as stmt hd in
        let rules, curr_stmts =
          match Pos.unmark hd with
          | SConditional (expr, t, f) ->
              let t_rules, t_curr_list = browse_bir t [] [] rules in
              let f_rules, f_curr_list = browse_bir f [] [] t_rules in
              let cond =
                give_pos (SConditional (expr, t_curr_list, f_curr_list))
              in
              (f_rules, cond :: curr_stmts)
          | _ -> (rules, hd :: curr_stmts)
        in
        if count_instr_blocks program curr_stmts < threshold then
          browse_bir tl new_stmts curr_stmts rules
        else
          let squish_rule = rule_from_stmts curr_stmts in
          browse_bir tl
            (give_pos (SRuleCall squish_rule.rule_id) :: new_stmts)
            []
            (RuleMap.add squish_rule.rule_id squish_rule rules)
  in
  let rules, mpp_functions =
    FunctionMap.fold
      (fun f mpp_func (rules, mpp_functions) ->
        let rules, stmts = browse_bir mpp_func [] [] rules in
        (rules, FunctionMap.add f stmts mpp_functions))
      program.mpp_functions
      (program.rules, FunctionMap.empty)
  in
  { program with rules; mpp_functions }

let get_assigned_variables (p : program) : VariableDict.t =
  let rec get_assigned_variables_block acc (stmts : stmt list) : VariableDict.t
      =
    List.fold_left
      (fun acc stmt ->
        match Pos.unmark stmt with
        | SVerif _ -> acc
        | SAssign (var, _) -> VariableDict.add var acc
        | SConditional (_, s1, s2) ->
            let acc = get_assigned_variables_block acc s1 in
            get_assigned_variables_block acc s2
        | SRuleCall _ | SFunctionCall _ -> assert false
        (* Cannot happen get_all_statements inlines all rule and mpp_function
           calls *))
      acc stmts
  in
  get_assigned_variables_block VariableDict.empty (get_all_statements p)

let get_local_variables (p : program) : unit Mir.LocalVariableMap.t =
  let rec get_local_vars_expr acc (e : expression Pos.marked) :
      unit Mir.LocalVariableMap.t =
    match Pos.unmark e with
    | Mir.Unop (_, e) | Mir.Index (_, e) -> get_local_vars_expr acc e
    | Mir.Comparison (_, e1, e2) | Mir.Binop (_, e1, e2) ->
        let acc = get_local_vars_expr acc e1 in
        get_local_vars_expr acc e2
    | Mir.Conditional (e1, e2, e3) ->
        let acc = get_local_vars_expr acc e1 in
        let acc = get_local_vars_expr acc e2 in
        get_local_vars_expr acc e3
    | Mir.FunctionCall (_, args) ->
        List.fold_left
          (fun (acc : unit Mir.LocalVariableMap.t) arg ->
            get_local_vars_expr acc arg)
          acc args
    | Mir.Literal _ | Mir.Var _ | Mir.GenericTableIndex | Mir.Error -> acc
    | Mir.LocalVar lvar -> Mir.LocalVariableMap.add lvar () acc
    | Mir.LocalLet (lvar, e1, e2) ->
        let acc = get_local_vars_expr acc e1 in
        let acc = get_local_vars_expr acc e2 in
        Mir.LocalVariableMap.add lvar () acc
  in
  let rec get_local_vars_block acc (stmts : stmt list) :
      unit Mir.LocalVariableMap.t =
    List.fold_left
      (fun acc stmt ->
        match Pos.unmark stmt with
        | SVerif cond -> get_local_vars_expr acc cond.Mir.cond_expr
        | SAssign (_, data) -> (
            match data.Mir.var_definition with
            | Mir.SimpleVar e -> get_local_vars_expr acc e
            | Mir.TableVar (_, defs) -> (
                match defs with
                | Mir.IndexTable es ->
                    Mir.IndexMap.fold
                      (fun _ e acc -> get_local_vars_expr acc e)
                      es acc
                | Mir.IndexGeneric e -> get_local_vars_expr acc e)
            | _ -> acc)
        | SConditional (cond, s1, s2) ->
            let acc = get_local_vars_expr acc (cond, Pos.no_pos) in
            let acc = get_local_vars_block acc s1 in
            get_local_vars_block acc s2
        | SFunctionCall _ | SRuleCall _ -> assert false
        (* Can't happen because SFunctionCall and SRuleCall are eliminated by
           get_all_statements below*))
      acc stmts
  in
  get_local_vars_block Mir.LocalVariableMap.empty (get_all_statements p)

let get_locals_size (p : program) : int =
  Mir.LocalVariableMap.fold
    (fun v () top -> max top v.Mir.LocalVariable.id)
    (get_local_variables p) 0

let rec remove_empty_conditionals (stmts : stmt list) : stmt list =
  List.rev
    (List.fold_left
       (fun acc stmt ->
         match Pos.unmark stmt with
         | SConditional (e, b1, b2) ->
             let b1 = remove_empty_conditionals b1 in
             let b2 = remove_empty_conditionals b2 in
             if List.length b1 = 0 && List.length b2 = 0 then acc
               (* empty conditional, we can discard it *)
             else Pos.same_pos_as (SConditional (e, b1, b2)) stmt :: acc
         | _ -> stmt :: acc)
       [] stmts)

let rec get_used_variables_ (e : expression Pos.marked) (acc : VariableDict.t) :
    VariableDict.t =
  match Pos.unmark e with
  | Mir.Comparison (_, e1, e2) | Mir.Binop (_, e1, e2) | Mir.LocalLet (_, e1, e2)
    ->
      let acc = get_used_variables_ e1 acc in
      let acc = get_used_variables_ e2 acc in
      acc
  | Mir.Unop (_, e) -> get_used_variables_ e acc
  | Mir.Index ((var, _), e) ->
      let acc = VariableDict.add var acc in
      let acc = get_used_variables_ e acc in
      acc
  | Mir.Conditional (e1, e2, e3) ->
      let acc = get_used_variables_ e1 acc in
      let acc = get_used_variables_ e2 acc in
      let acc = get_used_variables_ e3 acc in
      acc
  | Mir.FunctionCall (_, args) ->
      List.fold_left (fun acc arg -> get_used_variables_ arg acc) acc args
  | Mir.LocalVar _ | Mir.Literal _ | Mir.GenericTableIndex | Mir.Error -> acc
  | Mir.Var var -> VariableDict.add var acc

let get_used_variables (e : expression Pos.marked) : VariableDict.t =
  get_used_variables_ e VariableDict.empty
