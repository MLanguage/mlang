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

let rec translate_statement_list (l : Bir.stmt list)
    (curr_block_id : Oir.block_id) (blocks : Oir.block Oir.BlockMap.t) :
    Oir.block_id * Oir.block Oir.BlockMap.t =
  List.fold_left
    (fun (current_block_id, blocks) stmt ->
      translate_statement stmt current_block_id blocks)
    (curr_block_id, blocks) l

and translate_statement (s : Bir.stmt) (curr_block_id : Oir.block_id)
    (blocks : Oir.block Oir.BlockMap.t) :
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
  | Bir.SVerifBlock stmts ->
      let bid = fresh_block_id () in
      let blocks = initialize_block bid blocks in
      let end_block = fresh_block_id () in
      let blocks = initialize_block end_block blocks in
      let blocks =
        append_to_block
          (Pos.same_pos_as (Oir.SVerifBlock (bid, end_block)) s)
          curr_block_id blocks
      in
      let last_bid, blocks = translate_statement_list stmts bid blocks in
      let blocks =
        append_to_block (Oir.SGoto end_block, Pos.no_pos) last_bid blocks
      in
      (end_block, blocks)
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
      let last_b1id, blocks = translate_statement_list l1 b1id blocks in
      let blocks =
        append_to_block (Oir.SGoto join_block, Pos.no_pos) last_b1id blocks
      in
      let last_b2id, blocks = translate_statement_list l2 b2id blocks in
      let blocks =
        append_to_block (Oir.SGoto join_block, Pos.no_pos) last_b2id blocks
      in
      (join_block, blocks)
  | Bir.SRovCall rov_id ->
      let blocks =
        append_to_block
          (Pos.same_pos_as (Oir.SRovCall rov_id) s)
          curr_block_id blocks
      in
      (curr_block_id, blocks)
  | Bir.SFunctionCall (f, args) ->
      let blocks =
        append_to_block
          (Pos.same_pos_as (Oir.SFunctionCall (f, args)) s)
          curr_block_id blocks
      in
      (curr_block_id, blocks)
  | Bir.SPrint (std, args) ->
      ( curr_block_id,
        append_to_block
          (Pos.same_pos_as (Oir.SPrint (std, args)) s)
          curr_block_id blocks )
  | Bir.SIterate (v, vcs, expr, stmts) ->
      let bid = fresh_block_id () in
      let blocks = initialize_block bid blocks in
      let end_block = fresh_block_id () in
      let blocks = initialize_block end_block blocks in
      let blocks =
        append_to_block
          (Pos.same_pos_as (Oir.SIterate (v, vcs, expr, bid, end_block)) s)
          curr_block_id blocks
      in
      let last_bid, blocks = translate_statement_list stmts bid blocks in
      let blocks =
        append_to_block (Oir.SGoto end_block, Pos.no_pos) last_bid blocks
      in
      (end_block, blocks)
  | Bir.SRestore (vars, var_params, stmts) ->
      let bid = fresh_block_id () in
      let blocks = initialize_block bid blocks in
      let end_block = fresh_block_id () in
      let blocks = initialize_block end_block blocks in
      let blocks =
        append_to_block
          (Pos.same_pos_as (Oir.SRestore (vars, var_params, bid, end_block)) s)
          curr_block_id blocks
      in
      let last_bid, blocks = translate_statement_list stmts bid blocks in
      let blocks =
        append_to_block (Oir.SGoto end_block, Pos.no_pos) last_bid blocks
      in
      (end_block, blocks)
  | Bir.SRaiseError (err, var_opt) ->
      ( curr_block_id,
        append_to_block
          (Pos.same_pos_as (Oir.SRaiseError (err, var_opt)) s)
          curr_block_id blocks )
  | Bir.SCleanErrors ->
      ( curr_block_id,
        append_to_block
          (Pos.same_pos_as Oir.SCleanErrors s)
          curr_block_id blocks )
  | Bir.SExportErrors ->
      ( curr_block_id,
        append_to_block
          (Pos.same_pos_as Oir.SExportErrors s)
          curr_block_id blocks )
  | Bir.SFinalizeErrors ->
      ( curr_block_id,
        append_to_block
          (Pos.same_pos_as Oir.SFinalizeErrors s)
          curr_block_id blocks )

let bir_stmts_to_cfg (stmts : Bir.stmt list) : Oir.cfg =
  let entry_block = fresh_block_id () in
  let blocks = initialize_block entry_block Oir.BlockMap.empty in
  let exit_block, blocks = translate_statement_list stmts entry_block blocks in
  let blocks = Oir.BlockMap.map (fun stmts -> List.rev stmts) blocks in
  { blocks; entry_block; exit_block }

let bir_program_to_oir (p : Bir.program) : Oir.program =
  let mpp_functions =
    Bir.FunctionMap.map
      (fun Bir.{ mppf_stmts; mppf_is_verif } ->
        Oir.{ cfg = bir_stmts_to_cfg mppf_stmts; is_verif = mppf_is_verif })
      p.mpp_functions
  in
  let rules_and_verifs =
    Bir.ROVMap.map
      (fun Bir.{ rov_id; rov_name; rov_code } ->
        Oir.
          {
            id = rov_id;
            name = rov_name;
            code =
              (match rov_code with
              | Rule stmts -> Rule (bir_stmts_to_cfg stmts)
              | Verif stmt -> Verif (bir_stmts_to_cfg [ stmt ]));
          })
      p.rules_and_verifs
  in
  let targets =
    Mir.TargetMap.map
      (fun tf ->
        Oir.
          {
            file = tf.Bir.file;
            tmp_vars = tf.Bir.tmp_vars;
            cfg = bir_stmts_to_cfg tf.Bir.stmts;
            is_verif = tf.Bir.is_verif;
          })
      p.targets
  in
  {
    mpp_functions;
    targets;
    rules_and_verifs;
    idmap = p.idmap;
    mir_program = p.mir_program;
    outputs = p.outputs;
    main_function = p.main_function;
    context = p.context;
  }

let rec re_translate_statement (s : Oir.stmt)
    (blocks : Oir.block Oir.BlockMap.t) : Oir.block_id option * Bir.stmt option
    =
  match Pos.unmark s with
  | Oir.SAssign (var, data) ->
      (None, Some (Pos.same_pos_as (Bir.SAssign (var, data)) s))
  | Oir.SVerif cond -> (None, Some (Pos.same_pos_as (Bir.SVerif cond) s))
  | Oir.SVerifBlock (b, b_end) ->
      let stmts = re_translate_blocks_until b blocks (Some b_end) in
      (Some b_end, Some (Pos.same_pos_as (Bir.SVerifBlock stmts) s))
  | Oir.SConditional (e, b1, b2, join_block) ->
      let b1 = re_translate_blocks_until b1 blocks (Some join_block) in
      let b2 = re_translate_blocks_until b2 blocks (Some join_block) in
      (Some join_block, Some (Pos.same_pos_as (Bir.SConditional (e, b1, b2)) s))
  | Oir.SGoto b -> (Some b, None)
  | Oir.SRovCall rov_id -> (None, Some (Pos.same_pos_as (Bir.SRovCall rov_id) s))
  | Oir.SFunctionCall (f, args) ->
      (None, Some (Pos.same_pos_as (Bir.SFunctionCall (f, args)) s))
  | Oir.SPrint (std, args) ->
      (None, Some (Pos.same_pos_as (Bir.SPrint (std, args)) s))
  | Oir.SIterate (var, vcs, expr, b, b_end) ->
      let stmts = re_translate_blocks_until b blocks (Some b_end) in
      ( Some b_end,
        Some (Pos.same_pos_as (Bir.SIterate (var, vcs, expr, stmts)) s) )
  | Oir.SRestore (vars, var_params, b, b_end) ->
      let stmts = re_translate_blocks_until b blocks (Some b_end) in
      ( Some b_end,
        Some (Pos.same_pos_as (Bir.SRestore (vars, var_params, stmts)) s) )
  | Oir.SRaiseError (err, var_opt) ->
      (None, Some (Pos.same_pos_as (Bir.SRaiseError (err, var_opt)) s))
  | Oir.SCleanErrors -> (None, Some (Pos.same_pos_as Bir.SCleanErrors s))
  | Oir.SExportErrors -> (None, Some (Pos.same_pos_as Bir.SExportErrors s))
  | Oir.SFinalizeErrors -> (None, Some (Pos.same_pos_as Bir.SFinalizeErrors s))

and re_translate_statement_list (stmts : Oir.stmt list)
    (blocks : Oir.block Oir.BlockMap.t) : int option * Bir.stmt list =
  List.fold_left
    (fun (_, acc) s ->
      let next_block, stmt = re_translate_statement s blocks in
      (next_block, match stmt with None -> acc | Some s -> s :: acc))
    (None, []) stmts

and re_translate_blocks_until (block_id : Oir.block_id)
    (blocks : Oir.block Oir.BlockMap.t) (stop : Oir.block_id option) :
    Bir.stmt list =
  let next_block, stmts = re_translate_block block_id blocks in
  let next_stmts =
    match (next_block, stop) with
    | None, Some _ -> assert false (* should not happen *)
    | None, None -> []
    | Some next_block, None -> re_translate_blocks_until next_block blocks stop
    | Some next_block, Some stop ->
        if next_block = stop then []
        else re_translate_blocks_until next_block blocks (Some stop)
  in
  stmts @ next_stmts

and re_translate_block (block_id : Oir.block_id)
    (blocks : Oir.block Oir.BlockMap.t) : Oir.block_id option * Bir.stmt list =
  match Oir.BlockMap.find_opt block_id blocks with
  | None -> assert false (* should not happen *)
  | Some block ->
      let next_block_id, stmts = re_translate_statement_list block blocks in
      let stmts = List.rev stmts in
      (next_block_id, stmts)

let cfg_to_bir_stmts (cfg : Oir.cfg) : Bir.stmt list =
  re_translate_blocks_until cfg.entry_block cfg.blocks None

let oir_program_to_bir (p : Oir.program) : Bir.program =
  let mpp_functions =
    Bir.FunctionMap.map
      (fun (Oir.{ cfg; is_verif } : Oir.mpp_function) ->
        Bir.{ mppf_stmts = cfg_to_bir_stmts cfg; mppf_is_verif = is_verif })
      p.mpp_functions
  in
  let rules_and_verifs =
    Bir.ROVMap.map
      (fun Oir.{ id; name; code } ->
        Bir.
          {
            rov_id = id;
            rov_name = name;
            rov_code =
              (match code with
              | Rule cfg -> Rule (cfg_to_bir_stmts cfg)
              | Verif cfg -> (
                  match cfg_to_bir_stmts cfg with
                  | [ stmt ] -> Verif stmt
                  | _ -> assert false));
          })
      p.rules_and_verifs
  in
  let targets =
    Mir.TargetMap.map
      (fun tf ->
        Bir.
          {
            file = tf.Oir.file;
            tmp_vars = tf.Oir.tmp_vars;
            stmts = cfg_to_bir_stmts tf.Oir.cfg;
            is_verif = tf.Oir.is_verif;
          })
      p.targets
  in
  {
    targets;
    mpp_functions;
    rules_and_verifs;
    idmap = p.idmap;
    mir_program = p.mir_program;
    outputs = p.outputs;
    main_function = p.main_function;
    context = p.context;
  }
