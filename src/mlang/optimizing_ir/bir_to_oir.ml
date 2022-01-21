(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr>

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

let block_id_counter = ref 0

let fresh_block_id () : Oir.block_id =
  let out = !block_id_counter in
  block_id_counter := out + 1;
  out

let append_to_block (s : Oir.stmt) (bid : Oir.block_id)
    (blocks : Oir.block Oir.BlockMap.t) : Oir.block Oir.BlockMap.t =
  match Oir.BlockMap.find_opt bid blocks with
  | None -> assert false (* should not happen *)
  | Some stmts -> Oir.BlockMap.add bid (s :: stmts) blocks

let initialize_block (bid : Oir.block_id) (blocks : Oir.block Oir.BlockMap.t) :
    Oir.block Oir.BlockMap.t =
  Oir.BlockMap.add bid [] blocks

let rec translate_statement_list (p : Bir.program) (l : Bir.stmt list)
    (curr_block_id : Oir.block_id) (blocks : Oir.block Oir.BlockMap.t) :
    Oir.block_id * Oir.block Oir.BlockMap.t =
  List.fold_left
    (fun (current_block_id, blocks) stmt ->
      translate_statement p stmt current_block_id blocks)
    (curr_block_id, blocks) l

and translate_statement (p : Bir.program) (s : Bir.stmt)
    (curr_block_id : Oir.block_id) (blocks : Oir.block Oir.BlockMap.t) :
    Oir.block_id * Oir.block Oir.BlockMap.t =
  match Pos.unmark s with
  | Bir.SAssign (var, data) ->
      ( curr_block_id,
        append_to_block
          (Pos.same_pos_as (Oir.SAssign (var, data)) s)
          curr_block_id blocks )
  | Bir.SVerif cond ->
      ( curr_block_id,
        append_to_block
          (Pos.same_pos_as (Oir.SVerif cond) s)
          curr_block_id blocks )
  | Bir.SConditional (e, l1, l2) ->
      let b1id = fresh_block_id () in
      let blocks = initialize_block b1id blocks in
      let b2id = fresh_block_id () in
      let blocks = initialize_block b2id blocks in
      let join_block = fresh_block_id () in
      let blocks = initialize_block join_block blocks in
      let blocks =
        append_to_block
          (Pos.same_pos_as (Oir.SConditional (e, b1id, b2id, join_block)) s)
          curr_block_id blocks
      in
      let last_b1id, blocks = translate_statement_list p l1 b1id blocks in
      let blocks =
        append_to_block (Oir.SGoto join_block, Pos.no_pos) last_b1id blocks
      in
      let last_b2id, blocks = translate_statement_list p l2 b2id blocks in
      let blocks =
        append_to_block (Oir.SGoto join_block, Pos.no_pos) last_b2id blocks
      in
      (join_block, blocks)
  | Bir.SRuleCall rule_id ->
      (* To properly optimize M code, we have to instanciate each call as
         independent code *)
      let rule = Bir.RuleMap.find rule_id p.Bir.rules in
      let instance_id = Mir.fresh_rule_id () in
      let instance_name = rule.rule_name ^ "_i" ^ string_of_int instance_id in
      let stmts =
        let dummy = fresh_block_id () in
        let dummy_blocks = initialize_block dummy Oir.BlockMap.empty in
        let dummy, dummy_blocks =
          translate_statement_list p rule.rule_stmts dummy dummy_blocks
        in
        Oir.BlockMap.find dummy dummy_blocks
      in
      let blocks =
        append_to_block
          (Pos.same_pos_as
             (Oir.SRuleCall (instance_id, instance_name, List.rev stmts))
             s)
          curr_block_id blocks
      in
      (curr_block_id, blocks)

let bir_program_to_oir (p : Bir.program) : Oir.program =
  let entry_block = fresh_block_id () in
  let blocks = initialize_block entry_block Oir.BlockMap.empty in
  let exit_block, blocks =
    translate_statement_list p (Bir.main_statements p) entry_block blocks
  in
  let blocks = Oir.BlockMap.map (fun stmts -> List.rev stmts) blocks in
  {
    blocks;
    entry_block;
    exit_block;
    idmap = p.idmap;
    mir_program = p.mir_program;
    outputs = p.outputs;
    main_function = p.main_function;
  }

let rec re_translate_statement (s : Oir.stmt) (rules : Bir.rule Bir.RuleMap.t)
    (blocks : Oir.block Oir.BlockMap.t) :
    Oir.block_id option * Bir.stmt option * Bir.rule Bir.RuleMap.t =
  match Pos.unmark s with
  | Oir.SAssign (var, data) ->
      (None, Some (Pos.same_pos_as (Bir.SAssign (var, data)) s), rules)
  | Oir.SVerif cond -> (None, Some (Pos.same_pos_as (Bir.SVerif cond) s), rules)
  | Oir.SConditional (e, b1, b2, join_block) ->
      let b1, rules =
        re_translate_blocks_until b1 blocks rules (Some join_block)
      in
      let b2, rules =
        re_translate_blocks_until b2 blocks rules (Some join_block)
      in
      ( Some join_block,
        Some (Pos.same_pos_as (Bir.SConditional (e, b1, b2)) s),
        rules )
  | Oir.SGoto b -> (Some b, None, rules)
  | Oir.SRuleCall (rule_id, rule_name, stmts) ->
      let _, stmts, rules = re_translate_statement_list stmts rules blocks in
      let rule_stmts = List.rev stmts in
      if rule_stmts = [] then (None, None, rules)
      else
        let rule = Bir.{ rule_id; rule_name; rule_stmts } in
        ( None,
          Some (Pos.same_pos_as (Bir.SRuleCall rule_id) s),
          Bir.RuleMap.add rule_id rule rules )

and re_translate_statement_list (stmts : Oir.stmt list)
    (rules : Bir.rule Bir.RuleMap.t) (blocks : Oir.block Oir.BlockMap.t) =
  List.fold_left
    (fun (_, acc, rules) s ->
      let next_block, stmt, rules = re_translate_statement s rules blocks in
      (next_block, (match stmt with None -> acc | Some s -> s :: acc), rules))
    (None, [], rules) stmts

and re_translate_blocks_until (block_id : Oir.block_id)
    (blocks : Oir.block Oir.BlockMap.t) (rules : Bir.rule Bir.RuleMap.t)
    (stop : Oir.block_id option) : Bir.stmt list * Bir.rule Bir.RuleMap.t =
  let next_block, stmts, rules = re_translate_block block_id rules blocks in
  let next_stmts, rules =
    match (next_block, stop) with
    | None, Some _ -> assert false (* should not happen *)
    | None, None -> ([], rules)
    | Some next_block, None ->
        re_translate_blocks_until next_block blocks rules stop
    | Some next_block, Some stop ->
        if next_block = stop then ([], rules)
        else re_translate_blocks_until next_block blocks rules (Some stop)
  in
  (stmts @ next_stmts, rules)

and re_translate_block (block_id : Oir.block_id)
    (rules : Bir.rule Bir.RuleMap.t) (blocks : Oir.block Oir.BlockMap.t) :
    Oir.block_id option * Bir.stmt list * Bir.rule Bir.RuleMap.t =
  match Oir.BlockMap.find_opt block_id blocks with
  | None -> assert false (* should not happen *)
  | Some block ->
      let next_block_id, stmts, rules =
        re_translate_statement_list block rules blocks
      in
      let stmts = List.rev stmts in
      (next_block_id, stmts, rules)

let oir_program_to_bir (p : Oir.program) : Bir.program =
  let statements, rules =
    re_translate_blocks_until p.entry_block p.blocks Bir.RuleMap.empty None
  in
  let mpp_functions =
    Bir.FunctionMap.add p.main_function
      (Bir.remove_empty_conditionals statements)
      Bir.FunctionMap.empty
  in
  {
    mpp_functions;
    rules;
    idmap = p.idmap;
    mir_program = p.mir_program;
    outputs = p.outputs;
    main_function = p.main_function;
  }
