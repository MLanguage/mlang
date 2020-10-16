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

type stmt = stmt_kind Pos.marked

and stmt_kind =
  | SAssign of Mir.Variable.t * Mir.variable_data
  | SConditional of Mir.expression * stmt list * stmt list
  | SVerif of Mir.condition_data

type program = {
  statements : stmt list;
  idmap : Mir.idmap;
  mir_program : Mir.program;
  outputs : unit Mir.VariableMap.t;
}

let count_instructions (p : program) : int =
  let rec cond_instr_blocks (stmts : stmt list) : int =
    List.fold_left
      (fun acc stmt ->
        match Pos.unmark stmt with
        | SAssign _ | SVerif _ -> acc + 1
        | SConditional (_, s1, s2) -> acc + 1 + cond_instr_blocks s1 + cond_instr_blocks s2)
      0 stmts
  in
  cond_instr_blocks p.statements

let get_assigned_variables (p : program) : unit Mir.VariableMap.t =
  let rec get_assigned_variables_block (stmts : stmt list) : unit Mir.VariableMap.t =
    List.fold_left
      (fun acc stmt ->
        match Pos.unmark stmt with
        | SVerif _ -> acc
        | SAssign (var, _) -> Mir.VariableMap.add var () acc
        | SConditional (_, s1, s2) ->
            Mir.VariableMap.union
              (fun _ _ _ -> Some ())
              acc
              (Mir.VariableMap.union
                 (fun _ _ _ -> Some ())
                 (get_assigned_variables_block s1) (get_assigned_variables_block s2)))
      Mir.VariableMap.empty stmts
  in
  get_assigned_variables_block p.statements

let get_local_variables (p : program) : unit Mir.LocalVariableMap.t =
  let rec get_local_vars_expr (e : Mir.expression Pos.marked) : unit Mir.LocalVariableMap.t =
    match Pos.unmark e with
    | Mir.Unop (_, e) | Mir.Index (_, e) -> get_local_vars_expr e
    | Mir.Comparison (_, e1, e2) | Mir.Binop (_, e1, e2) ->
        Mir.LocalVariableMap.union
          (fun _ _ _ -> Some ())
          (get_local_vars_expr e1) (get_local_vars_expr e2)
    | Mir.Conditional (e1, e2, e3) ->
        Mir.LocalVariableMap.union
          (fun _ _ _ -> Some ())
          (Mir.LocalVariableMap.union
             (fun _ _ _ -> Some ())
             (get_local_vars_expr e1) (get_local_vars_expr e2))
          (get_local_vars_expr e3)
    | Mir.FunctionCall (_, args) ->
        List.fold_left
          (fun (acc : unit Mir.LocalVariableMap.t) arg ->
            Mir.LocalVariableMap.union (fun _ _ _ -> Some ()) (get_local_vars_expr arg) acc)
          Mir.LocalVariableMap.empty args
    | Mir.Literal _ | Mir.Var _ | Mir.GenericTableIndex | Mir.Error | Mir.LocalVar _ ->
        Mir.LocalVariableMap.empty
    | Mir.LocalLet (lvar, e1, e2) ->
        Mir.LocalVariableMap.add lvar ()
          (Mir.LocalVariableMap.union
             (fun _ _ _ -> Some ())
             (get_local_vars_expr e1) (get_local_vars_expr e2))
  in
  let rec get_local_vars_block (stmts : stmt list) : unit Mir.LocalVariableMap.t =
    List.fold_left
      (fun acc stmt ->
        match Pos.unmark stmt with
        | SVerif cond ->
            Mir.LocalVariableMap.union
              (fun _ _ _ -> Some ())
              (get_local_vars_expr cond.Mir.cond_expr)
              acc
        | SAssign (_, data) -> (
            match data.Mir.var_definition with
            | Mir.SimpleVar e ->
                Mir.LocalVariableMap.union (fun _ _ _ -> Some ()) (get_local_vars_expr e) acc
            | Mir.TableVar (_, defs) -> (
                match defs with
                | Mir.IndexTable es ->
                    Mir.IndexMap.fold
                      (fun _ e acc ->
                        Mir.LocalVariableMap.union
                          (fun _ _ _ -> Some ())
                          (get_local_vars_expr e) acc)
                      es acc
                | Mir.IndexGeneric e ->
                    Mir.LocalVariableMap.union (fun _ _ _ -> Some ()) (get_local_vars_expr e) acc )
            | _ -> acc )
        | SConditional (cond, s1, s2) ->
            Mir.LocalVariableMap.union
              (fun _ _ _ -> Some ())
              (get_local_vars_expr (cond, Pos.no_pos))
              (Mir.LocalVariableMap.union
                 (fun _ _ _ -> Some ())
                 (get_local_vars_block s1) (get_local_vars_block s2)))
      Mir.LocalVariableMap.empty stmts
  in
  get_local_vars_block p.statements

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
