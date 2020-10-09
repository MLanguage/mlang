(* Copyright (C) 2020 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

module CodeLocationMap = Map.Make (struct
  type t = Bir_interpreter.code_location

  let compare x y = compare x y
end)

type code_coverage_result = Bir_interpreter.var_literal CodeLocationMap.t Mir.VariableMap.t
(** The result of the code coverage measurement is a map of the successive definitions of all the
    non-array variables during interpretation of the program *)

let empty_code_coverage_result : code_coverage_result = Mir.VariableMap.empty

let code_coverage_acc : code_coverage_result ref = ref empty_code_coverage_result

let code_coverage_init () : unit =
  code_coverage_acc := empty_code_coverage_result;
  Bir_interpreter.assign_hook :=
    fun var literal code_loc ->
      code_coverage_acc :=
        Mir.VariableMap.update var
          (fun old_defs ->
            match old_defs with
            | None -> Some (CodeLocationMap.singleton code_loc literal)
            | Some old_defs -> Some (CodeLocationMap.add code_loc literal old_defs))
          !code_coverage_acc

let code_coverage_result () : code_coverage_result = !code_coverage_acc

type code_coverage_map_value =
  | OnlyOneDef of Bir_interpreter.var_literal
  | OnlyOneDefAndUndefined of Bir_interpreter.var_literal
  | MultipleDefs

type code_coverage_acc = code_coverage_map_value CodeLocationMap.t Mir.VariableMap.t

let merge_code_coverage_single_results_with_acc (results : code_coverage_result)
    (acc : code_coverage_acc) : code_coverage_acc =
  Mir.VariableMap.fold
    (fun var (new_defs : Bir_interpreter.var_literal CodeLocationMap.t) acc ->
      match Mir.VariableMap.find_opt var acc with
      | None -> Mir.VariableMap.add var (CodeLocationMap.map (fun x -> OnlyOneDef x) new_defs) acc
      | Some old_defs ->
          Mir.VariableMap.add var
            (CodeLocationMap.fold
               (fun code_loc new_def defs ->
                 match CodeLocationMap.find_opt code_loc defs with
                 | None -> CodeLocationMap.add code_loc (OnlyOneDef new_def) defs
                 | Some old_def -> (
                     match old_def with
                     | OnlyOneDef old_def ->
                         if old_def = new_def then defs
                         else if new_def = SimpleVar Undefined then
                           CodeLocationMap.add code_loc (OnlyOneDefAndUndefined old_def) defs
                         else if old_def = SimpleVar Undefined then
                           CodeLocationMap.add code_loc (OnlyOneDefAndUndefined new_def) defs
                         else CodeLocationMap.add code_loc MultipleDefs defs
                     | OnlyOneDefAndUndefined old_def ->
                         if old_def = new_def then defs
                         else CodeLocationMap.add code_loc MultipleDefs defs
                     | MultipleDefs -> CodeLocationMap.add code_loc MultipleDefs defs ))
               new_defs old_defs)
            acc)
    results acc

let merge_code_coverage_acc (acc1 : code_coverage_acc) (acc2 : code_coverage_acc) :
    code_coverage_acc =
  Mir.VariableMap.union
    (fun _ defs1 defs2 ->
      Some
        (CodeLocationMap.union
           (fun _ def1 def2 ->
             match (def1, def2) with
             | MultipleDefs, _ | _, MultipleDefs -> Some MultipleDefs
             | OnlyOneDefAndUndefined def1, OnlyOneDefAndUndefined def2
             | OnlyOneDefAndUndefined def1, OnlyOneDef def2
             | OnlyOneDef def1, OnlyOneDefAndUndefined def2 ->
                 if def1 = def2 then Some (OnlyOneDefAndUndefined def1) else Some MultipleDefs
             | OnlyOneDef def1, OnlyOneDef def2 ->
                 if def1 = def2 then Some (OnlyOneDef def1)
                 else if def1 = SimpleVar Undefined then Some (OnlyOneDefAndUndefined def2)
                 else if def2 = SimpleVar Undefined then Some (OnlyOneDefAndUndefined def1)
                 else Some MultipleDefs)
           defs1 defs2))
    acc1 acc2

type code_locs = Mir.Variable.t CodeLocationMap.t

let rec get_code_locs_stmt (stmt : Bir.stmt) (loc : Bir_interpreter.code_location) : code_locs =
  match Pos.unmark stmt with
  | Bir.SConditional (_, t, f) ->
      CodeLocationMap.union
        (fun _ _ _ -> assert false)
        (get_code_locs_stmts t (Bir_interpreter.ConditionalBranch true :: loc))
        (get_code_locs_stmts f (Bir_interpreter.ConditionalBranch false :: loc))
  | Bir.SVerif _ -> CodeLocationMap.empty
  | Bir.SAssign (var, _) -> CodeLocationMap.singleton loc var

and get_code_locs_stmts (stmts : Bir.stmt list) (loc : Bir_interpreter.code_location) : code_locs =
  let locs, _ =
    List.fold_left
      (fun (locs, i) stmt ->
        ( CodeLocationMap.union
            (fun _ _ _ -> assert false)
            (get_code_locs_stmt stmt (Bir_interpreter.InsideBlock i :: loc))
            locs,
          i + 1 ))
      (CodeLocationMap.empty, 0) stmts
  in
  locs

let get_code_locs (p : Bir.program) : code_locs = get_code_locs_stmts p.statements []
