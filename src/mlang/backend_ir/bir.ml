(* Copyright (C) 2019-2020 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr> Raphël
   Monat <raphael.monat@lip6.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

type rule_id = int

module RuleMap = Map.Make (struct
  type t = rule_id

  let compare = compare
end)

type rule = { rule_id : rule_id; rule_name : string; rule_stmts : stmt list }

and stmt = stmt_kind Pos.marked

and stmt_kind =
  | SAssign of Mir.Variable.t * Mir.variable_data
  | SConditional of Mir.expression * stmt list * stmt list
  | SVerif of Mir.condition_data
  | SRuleCall of rule_id

type program = {
  rules : rule RuleMap.t;
  statements : stmt list;
  idmap : Mir.idmap;
  mir_program : Mir.program;
  outputs : unit Mir.VariableMap.t;
}

let get_all_statements program =
  let rec get_block_statements stmts =
    List.fold_left
      (fun stmts stmt ->
        match Pos.unmark stmt with
        | SRuleCall r -> List.rev (RuleMap.find r program.rules).rule_stmts @ stmts
        | SConditional (e, t, f) ->
            let t = get_block_statements t in
            let f = get_block_statements f in
            Pos.same_pos_as (SConditional (e, t, f)) stmt :: stmts
        | _ -> stmt :: stmts)
      [] stmts
    |> List.rev
  in
  get_block_statements program.statements

let count_instructions (p : program) : int =
  let rec cond_instr_blocks (stmts : stmt list) : int =
    List.fold_left
      (fun acc stmt ->
        match Pos.unmark stmt with
        | SAssign _ | SVerif _ | SRuleCall _ -> acc + 1
        | SConditional (_, s1, s2) -> acc + 1 + cond_instr_blocks s1 + cond_instr_blocks s2)
      0 stmts
  in
  cond_instr_blocks p.statements

let get_assigned_variables (p : program) : unit Mir.VariableMap.t =
  let rec get_assigned_variables_block acc (stmts : stmt list) : unit Mir.VariableMap.t =
    List.fold_left
      (fun acc stmt ->
        match Pos.unmark stmt with
        | SVerif _ -> acc
        | SAssign (var, _) -> Mir.VariableMap.add var () acc
        | SConditional (_, s1, s2) ->
            let acc = get_assigned_variables_block acc s1 in
            get_assigned_variables_block acc s2
        | SRuleCall _ -> assert false)
      acc stmts
  in
  get_assigned_variables_block Mir.VariableMap.empty (get_all_statements p)

let get_local_variables (p : program) : unit Mir.LocalVariableMap.t =
  let rec get_local_vars_expr acc (e : Mir.expression Pos.marked) : unit Mir.LocalVariableMap.t =
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
          (fun (acc : unit Mir.LocalVariableMap.t) arg -> get_local_vars_expr acc arg)
          acc args
    | Mir.Literal _ | Mir.Var _ | Mir.GenericTableIndex | Mir.Error -> acc
    | Mir.LocalVar lvar -> Mir.LocalVariableMap.add lvar () acc
    | Mir.LocalLet (lvar, e1, e2) ->
        let acc = get_local_vars_expr acc e1 in
        let acc = get_local_vars_expr acc e2 in
        Mir.LocalVariableMap.add lvar () acc
  in
  let rec get_local_vars_block acc (stmts : stmt list) : unit Mir.LocalVariableMap.t =
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
                    Mir.IndexMap.fold (fun _ e acc -> get_local_vars_expr acc e) es acc
                | Mir.IndexGeneric e -> get_local_vars_expr acc e)
            | _ -> acc)
        | SConditional (cond, s1, s2) ->
            let acc = get_local_vars_expr acc (cond, Pos.no_pos) in
            let acc = get_local_vars_block acc s1 in
            get_local_vars_block acc s2
        | SRuleCall _ -> assert false)
      acc stmts
  in
  get_local_vars_block Mir.LocalVariableMap.empty (get_all_statements p)

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
