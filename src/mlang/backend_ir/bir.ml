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

type function_name = string

type rule = { rule_id : rule_id; rule_name : string; rule_stmts : stmt list }

and stmt = stmt_kind Pos.marked

and stmt_kind =
  | SAssign of Mir.Variable.t * Mir.variable_data
  | SConditional of Mir.expression * stmt list * stmt list
  | SVerif of Mir.condition_data
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
  outputs : unit Mir.VariableMap.t;
}

let main_statements (p : program) : stmt list =
  try FunctionMap.find p.main_function p.mpp_functions
  with Not_found ->
    Errors.raise_error "Unable to find main function of Bir program"

let rec get_block_statements (rules : rule RuleMap.t) (p : program)
    (stmts : stmt list) : stmt list =
  List.fold_left
    (fun stmts stmt ->
      match Pos.unmark stmt with
      | SRuleCall r -> List.rev (RuleMap.find r rules).rule_stmts @ stmts
      | SConditional (e, t, f) ->
          let t = get_block_statements rules p t in
          let f = get_block_statements rules p f in
          Pos.same_pos_as (SConditional (e, t, f)) stmt :: stmts
      | SFunctionCall (f, _) -> List.rev (FunctionMap.find f p.mpp_functions)
      | _ -> stmt :: stmts)
    [] stmts
  |> List.rev

(** Returns program statements with all rules inlined *)
let get_all_statements (p : program) : stmt list =
  main_statements p |> get_block_statements p.rules p

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
        if
          List.length (get_block_statements rules program curr_stmts)
          < threshold
        then browse_bir tl new_stmts curr_stmts rules
        else
          let squish_rule = rule_from_stmts curr_stmts in
          browse_bir tl
            (give_pos (SRuleCall squish_rule.rule_id) :: new_stmts)
            []
            (RuleMap.add squish_rule.rule_id squish_rule rules)
  in
  let new_rules, new_stmts =
    browse_bir (main_statements program) [] [] program.rules
  in
  {
    program with
    rules = new_rules;
    mpp_functions =
      (* TODO: this is not longer enough to slice the whole program *)
      FunctionMap.add program.main_function new_stmts program.mpp_functions;
  }

let count_instructions (p : program) : int =
  let rec cond_instr_blocks (stmts : stmt list) : int =
    List.fold_left
      (fun acc stmt ->
        match Pos.unmark stmt with
        | SAssign _ | SVerif _ | SRuleCall _ | SFunctionCall _ -> acc + 1
        | SConditional (_, s1, s2) ->
            acc + 1 + cond_instr_blocks s1 + cond_instr_blocks s2)
      0 stmts
  in
  (* TODO: same *)
  cond_instr_blocks (main_statements p)

let get_assigned_variables (p : program) : Mir.VariableDict.t =
  let rec get_assigned_variables_block acc (stmts : stmt list) :
      Mir.VariableDict.t =
    List.fold_left
      (fun acc stmt ->
        match Pos.unmark stmt with
        | SVerif _ -> acc
        | SAssign (var, _) -> Mir.VariableDict.add var acc
        | SConditional (_, s1, s2) ->
            let acc = get_assigned_variables_block acc s1 in
            get_assigned_variables_block acc s2
        | SRuleCall _ | SFunctionCall _ -> assert false
        (* Cannot happen get_all_statements inlines all rule and mpp_function
           calls *))
      acc stmts
  in
  get_assigned_variables_block Mir.VariableDict.empty (get_all_statements p)

let get_local_variables (p : program) : unit Mir.LocalVariableMap.t =
  let rec get_local_vars_expr acc (e : Mir.expression Pos.marked) :
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
        | SFunctionCall _ -> acc
        | SRuleCall _ -> assert false)
      acc stmts
  in
  get_local_vars_block Mir.LocalVariableMap.empty (get_all_statements p)

let get_locals_size (p : program) : int =
  Mir.LocalVariableMap.cardinal (get_local_variables p)

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
