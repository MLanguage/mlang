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
