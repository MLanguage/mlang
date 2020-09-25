(* Copyright (C) 2020 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Oir

let optimize (p : program) : program =
  let instrs = ref (count_instr p) in
  Cli.debug_print "Intruction count: %d" !instrs;
  Cli.debug_print "Dead code removal...";
  let p = Dead_code_removal.dead_code_removal p in
  let p = ref p in
  while !instrs <> count_instr !p do
    Cli.debug_print "Intruction count: %d" (count_instr !p);
    Cli.debug_print "Partial evaluation...";
    instrs := count_instr !p;
    p := Partial_evaluation.partial_evaluation !p;
    p := Dead_code_removal.dead_code_removal !p
  done;
  let p = !p in
  Cli.debug_print "Intruction count: %d" (count_instr p);
  p
