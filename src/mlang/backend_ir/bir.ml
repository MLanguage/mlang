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

type rov_id = Mir.rov_id

module ROVMap = Mir.RuleMap

type tgv_id = string

let default_tgv = "primitif"

type variable = { on_tgv : tgv_id; offset : int; mir_var : Mir.Variable.t }

let compare_variable v1 v2 =
  let c = String.compare v1.on_tgv v2.on_tgv in
  if c <> 0 then c
  else
    let c = Stdlib.compare v1.offset v2.offset in
    if c <> 0 then c else Mir.Variable.compare v1.mir_var v2.mir_var

module VariableMap = Map.Make (struct
  type t = variable

  let compare = compare_variable
end)

module VariableSet = Set.Make (struct
  type t = variable

  let compare = compare_variable
end)

module NameMap = Map.Make (String)

type offset_alloc = { mutable name_map : int NameMap.t; mutable size : int }

(* Mutable state hidden away in the signature. Used for black-magicaly
   transition variable representations from SSA duplications to offsets of TGV.
   An issue though: this disregards tetantives to reduce the size of the TGV
   through optimisations *)
let offset_alloc = { name_map = NameMap.empty; size = 0 }

let allocate_variable (var : Mir.variable) : int =
  let name = Pos.unmark var.Mir.Variable.name in
  match NameMap.find_opt name offset_alloc.name_map with
  | Some offset -> offset
  | None ->
      let var_size =
        match var.Mir.Variable.is_table with None -> 1 | Some s -> s
      in
      let offset = offset_alloc.size in
      offset_alloc.name_map <- NameMap.add name offset offset_alloc.name_map;
      offset_alloc.size <- offset_alloc.size + var_size;
      offset

let size_of_tgv () = offset_alloc.size

(* unify SSA variables *)
let var_from_mir (on_tgv : tgv_id) (v : Mir.Variable.t) : variable =
  let mir_var = match v.origin with Some v -> v | None -> v in
  { offset = allocate_variable mir_var; on_tgv; mir_var }

let var_to_mir v = v.mir_var

let map_from_mir_map on_tgv map =
  Mir.VariableMap.fold
    (fun var -> VariableMap.add (var_from_mir on_tgv var))
    map VariableMap.empty

let set_from_mir_dict on_tgv dict =
  Mir.VariableDict.fold
    (fun var -> VariableSet.add (var_from_mir on_tgv var))
    dict VariableSet.empty

type expression = variable Mir.expression_

type condition_data = variable Mir.condition_data_

type variable_def = variable Mir.variable_def_

type variable_data = variable Mir.variable_data_

type function_name = string

type rule_or_verif_code = Rule of stmt list | Verif of stmt

and rule_or_verif = {
  rov_id : rov_id;
  rov_name : string Pos.marked;
  rov_code : rule_or_verif_code;
}

and stmt = stmt_kind Pos.marked

and stmt_kind =
  | SAssign of variable * variable_data
  | SConditional of expression * stmt list * stmt list
  | SVerif of condition_data
  | SRovCall of rov_id
  | SFunctionCall of function_name * Mir.Variable.t list

let rule_or_verif_as_statements (rov : rule_or_verif) : stmt list =
  match rov.rov_code with Rule stmts -> stmts | Verif stmt -> [ stmt ]

type mpp_function = { mppf_stmts : stmt list; mppf_is_verif : bool }

module FunctionMap = Map.Make (struct
  type t = function_name

  let compare = String.compare
end)

type program = {
  mpp_functions : mpp_function FunctionMap.t;
  rules_and_verifs : rule_or_verif ROVMap.t;
  main_function : function_name;
  context_function : function_name;
  context_with_reset_function : function_name;
  idmap : Mir.idmap;
  mir_program : Mir.program;
  outputs : unit VariableMap.t;
}

let main_statements (p : program) : stmt list =
  try (FunctionMap.find p.context_function p.mpp_functions).mppf_stmts
  with Not_found ->
    Errors.raise_error
      "Unable to find contextualized main function of Bir program"

let main_statements_no_context (p : program) : stmt list =
  try (FunctionMap.find p.main_function p.mpp_functions).mppf_stmts
  with Not_found ->
    Errors.raise_error "Unable to find main function of Bir program"

let main_statements_with_reset (p : program) : stmt list =
  try
    (FunctionMap.find p.context_with_reset_function p.mpp_functions).mppf_stmts
  with Not_found ->
    Errors.raise_error
      "Unable to find contextualized main function with reset of Bir program"

let rec get_block_statements (p : program) (stmts : stmt list) : stmt list =
  List.fold_left
    (fun stmts stmt ->
      match Pos.unmark stmt with
      | SRovCall r -> (
          match (ROVMap.find r p.rules_and_verifs).rov_code with
          | Rule rstmts -> List.rev rstmts @ stmts
          | Verif stmt -> stmt :: stmts)
      | SConditional (e, t, f) ->
          let t = get_block_statements p t in
          let f = get_block_statements p f in
          Pos.same_pos_as (SConditional (e, t, f)) stmt :: stmts
      | SFunctionCall (f, _) ->
          (get_block_statements p
             (FunctionMap.find f p.mpp_functions).mppf_stmts
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
      | SAssign _ | SVerif _ | SRovCall _ | SFunctionCall _ -> acc + 1
      | SConditional (_, s1, s2) ->
          acc + 1 + count_instr_blocks p s1 + count_instr_blocks p s2)
    0 stmts

let squish_statements (program : program) (threshold : int)
    (rule_suffix : string) =
  let rule_from_stmts stmts =
    let id = Mir.RuleID (Mir.fresh_rule_num ()) in
    {
      rov_id = id;
      rov_name =
        ( rule_suffix ^ string_of_int (Mir.num_of_rule_or_verif_id id),
          Pos.no_pos );
      rov_code = Rule (List.rev stmts);
    }
  in
  let rec browse_bir (old_stmts : stmt list) (new_stmts : stmt list)
      (curr_stmts : stmt list) (rules : rule_or_verif ROVMap.t) =
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
            (give_pos (SRovCall squish_rule.rov_id) :: new_stmts)
            []
            (ROVMap.add squish_rule.rov_id squish_rule rules)
  in
  let rules_and_verifs, mpp_functions =
    FunctionMap.fold
      (fun f mpp_func (rules, mpp_functions) ->
        let rules, mppf_stmts = browse_bir mpp_func.mppf_stmts [] [] rules in
        let func = { mppf_stmts; mppf_is_verif = mpp_func.mppf_is_verif } in
        (rules, FunctionMap.add f func mpp_functions))
      program.mpp_functions
      (program.rules_and_verifs, FunctionMap.empty)
  in
  { program with rules_and_verifs; mpp_functions }

let get_assigned_variables (p : program) : VariableSet.t =
  let rec get_assigned_variables_block acc (stmts : stmt list) : VariableSet.t =
    List.fold_left
      (fun acc stmt ->
        match Pos.unmark stmt with
        | SVerif _ -> acc
        | SAssign (var, _) -> VariableSet.add var acc
        | SConditional (_, s1, s2) ->
            let acc = get_assigned_variables_block acc s1 in
            get_assigned_variables_block acc s2
        | SRovCall _ | SFunctionCall _ -> assert false
        (* Cannot happen get_all_statements inlines all rule and mpp_function
           calls *))
      acc stmts
  in
  get_assigned_variables_block VariableSet.empty (get_all_statements p)

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
    | Mir.Literal _ | Mir.Var _ | Mir.Error -> acc
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
                | Mir.IndexGeneric (_v, e) -> get_local_vars_expr acc e)
            | _ -> acc)
        | SConditional (cond, s1, s2) ->
            let acc = get_local_vars_expr acc (cond, Pos.no_pos) in
            let acc = get_local_vars_block acc s1 in
            get_local_vars_block acc s2
        | SFunctionCall _ | SRovCall _ -> assert false
        (* Can't happen because SFunctionCall and SRovCall are eliminated by
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

let get_used_variables_ (e : expression Pos.marked) (acc : VariableSet.t) :
    VariableSet.t =
  Mir.fold_expr_var (fun acc var -> VariableSet.add var acc) acc (Pos.unmark e)

let get_used_variables (e : expression Pos.marked) : VariableSet.t =
  get_used_variables_ e VariableSet.empty
