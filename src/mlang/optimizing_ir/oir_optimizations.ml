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

exception Exit

let get_reduction_percent (init : int) (old : int) (new_ : int) : float =
  float_of_int (old - new_) /. float_of_int init *. 100.

let print_done ?msg (init : int) (old : int) (new_ : int) : unit =
  let reduction_percent = get_reduction_percent init old new_ in
  let strict_reduction = reduction_percent > 0. in
  Cli.debug_print "%s%s (%s) "
    (match msg with None -> "" | Some msg -> msg ^ ": ")
    (ANSITerminal.sprintf [ ANSITerminal.magenta ] "%d → %d" old new_)
    (ANSITerminal.sprintf
       [ (if strict_reduction then ANSITerminal.green else ANSITerminal.yellow) ]
       "%s %.1f%%"
       (if strict_reduction then "↘" else "~")
       reduction_percent)

let optimize (p : program) : program =
  let start_instrs = count_instr p in
  Cli.debug_print "Dead code removal...";
  let p = Dead_code_removal.dead_code_removal p in
  print_done start_instrs start_instrs (count_instr p);
  let p = ref p in
  ( try
      while true do
        Cli.debug_print "Partial evaluation...";
        let old_instrs = count_instr !p in
        p := Partial_evaluation.partial_evaluation !p;
        p := Dead_code_removal.dead_code_removal !p;
        let intermediate_instrs = count_instr !p in
        print_done start_instrs old_instrs intermediate_instrs;
        if intermediate_instrs = old_instrs then raise Exit
        else begin
          Cli.debug_print "Inlining...";
          p := Inlining.inlining !p;
          p := Dead_code_removal.dead_code_removal !p;
          let end_instrs = count_instr !p in
          print_done start_instrs intermediate_instrs end_instrs;
          if end_instrs = intermediate_instrs then raise Exit
        end
      done
    with Exit -> () );
  let p = !p in
  let end_instrs = count_instr p in
  print_done ~msg:"Optimizations done! Total effect" start_instrs start_instrs end_instrs;
  p
