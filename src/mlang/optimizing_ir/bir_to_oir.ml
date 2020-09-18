(* Copyright (C) 2020 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

let block_id_counter = ref 0

let fresh_block_id () : Oir.block_id =
  let out = !block_id_counter in
  block_id_counter := out + 1;
  out

let rec translate_statement_list (l : Bir.stmt list) : Oir.block_id * Oir.block Oir.BlockMap.t =
  let first_block_id = fresh_block_id () in
  let blocks, new_stmts, last_block_id =
    List.fold_left
      (fun (blocks, new_list, current_block_id) stmt ->
        let new_stmt, new_blocks = translate_statement stmt in
        match Pos.unmark new_stmt with
        | Oir.SConditional (_, b1_id, b2_id) ->
            let next_block_id = fresh_block_id () in
            let new_blocks =
              Oir.BlockMap.add current_block_id (List.rev (new_stmt :: new_list)) new_blocks
            in
            let add_goto b1 =
              match b1 with
              | None -> assert false (* should not happen *)
              | Some b1 -> Some (b1 @ [ (Oir.SGoto next_block_id, Pos.no_pos) ])
            in
            let new_blocks = Oir.BlockMap.update b1_id add_goto new_blocks in
            let new_blocks = Oir.BlockMap.update b2_id add_goto new_blocks in
            ( Oir.BlockMap.union
                (fun _ _ _ -> assert false (* should not happen *))
                blocks new_blocks,
              [],
              next_block_id )
        | _ ->
            ( Oir.BlockMap.union
                (fun _ _ _ -> assert false (* should not happen *))
                blocks new_blocks,
              new_stmt :: new_list,
              current_block_id ))
      (Oir.BlockMap.empty, [], first_block_id)
      l
  in
  (first_block_id, Oir.BlockMap.add last_block_id (List.rev new_stmts) blocks)

and translate_statement (s : Bir.stmt) : Oir.stmt * Oir.block Oir.BlockMap.t =
  match Pos.unmark s with
  | Bir.SAssign (var, data) -> (Pos.same_pos_as (Oir.SAssign (var, data)) s, Oir.BlockMap.empty)
  | Bir.SVerif cond -> (Pos.same_pos_as (Oir.SVerif cond) s, Oir.BlockMap.empty)
  | Bir.SConditional (e, l1, l2) ->
      let b1_id, new_blocks1 = translate_statement_list l1 in
      let b2_id, new_blocks2 = translate_statement_list l2 in
      ( Pos.same_pos_as (Oir.SConditional (e, b1_id, b2_id)) s,
        Oir.BlockMap.union
          (fun _ _ _ -> assert false (* should not happen *))
          new_blocks1 new_blocks2 )

let bir_program_to_oir (p : Bir.program) : Oir.program =
  let entry_block, blocks = translate_statement_list p.statements in
  { blocks; entry_block; idmap = p.idmap; mir_program = p.mir_program; outputs = p.outputs }

type result =
  | Stmt of Bir.stmt
  | Nextblock of Oir.block_id
  | Conditional of Bir.stmt * Oir.block_id

let rec re_translate_statement (s : Oir.stmt) (blocks : Oir.block Oir.BlockMap.t) : result =
  match Pos.unmark s with
  | Oir.SAssign (var, data) -> Stmt (Pos.same_pos_as (Bir.SAssign (var, data)) s)
  | Oir.SVerif cond -> Stmt (Pos.same_pos_as (Bir.SVerif cond) s)
  | Oir.SConditional (e, b1, b2) -> (
      let b1 = Oir.BlockMap.find b1 blocks in
      let b1, next_block1 = re_translate_statement_list b1 blocks in
      let b2 = Oir.BlockMap.find b2 blocks in
      let b2, next_block2 = re_translate_statement_list b2 blocks in
      match (next_block1, next_block2) with
      | Some n1, Some n2 when n1 = n2 ->
          Conditional (Pos.same_pos_as (Bir.SConditional (e, b1, b2)) s, n1)
      | _ -> assert false
      (* should not happen *) )
  | Oir.SGoto b -> Nextblock b

and re_translate_statement_list (ss : Oir.block) (blocks : Oir.block Oir.BlockMap.t) :
    Bir.stmt list * Oir.block_id option =
  let next_block, stmts =
    List.fold_left
      (fun (_, acc) s ->
        match re_translate_statement s blocks with
        | Stmt s -> (None, s :: acc)
        | Nextblock b -> (Some b, acc)
        | Conditional (s, b) -> (Some b, s :: acc))
      (None, []) ss
  in
  (List.rev stmts, next_block)

let rec translate_program_statements (entry_block : Oir.block_id)
    (blocks : Oir.block Oir.BlockMap.t) : Bir.stmt list =
  let new_statements, next_block =
    re_translate_statement_list (Oir.BlockMap.find entry_block blocks) blocks
  in
  new_statements
  @
  match next_block with
  | None -> []
  | Some next_block -> translate_program_statements next_block blocks

let oir_program_to_bir (p : Oir.program) : Bir.program =
  {
    statements = translate_program_statements p.entry_block p.blocks;
    idmap = p.idmap;
    mir_program = p.mir_program;
    outputs = p.outputs;
  }
