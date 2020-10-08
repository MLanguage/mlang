(* Copyright (C) 2020 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

type code_coverage_result = Bir_interpreter.var_literal list Mir.VariableMap.t
(** The result of the code coverage measurement is a map of the successive definitions of all the
    non-array variables during interpretation of the program *)

let empty_code_coverage_result : code_coverage_result = Mir.VariableMap.empty

let code_coverage_acc : code_coverage_result ref = ref empty_code_coverage_result

let code_coverage_init () : unit =
  code_coverage_acc := empty_code_coverage_result;
  Bir_interpreter.assign_hook :=
    fun var literal ->
      code_coverage_acc :=
        Mir.VariableMap.update var
          (fun old_defs ->
            match old_defs with
            | None -> Some [ literal ]
            | Some old_defs -> Some (literal :: old_defs))
          !code_coverage_acc

let code_coverage_result () : code_coverage_result = !code_coverage_acc

type code_coverage_map_value =
  | OnlyUndefined
  | OnlyOneDef of Bir_interpreter.var_literal
  | OnlyOneDefAndUndefined of Bir_interpreter.var_literal
  | MultipleDefs
  | OnlyOneDefPattern of Bir_interpreter.var_literal list
  | MultipleDefPatterns

type code_coverage_acc = code_coverage_map_value Mir.VariableMap.t

let same_def_pattern (old_def_pattern : Bir_interpreter.var_literal list)
    (new_def_pattern : Bir_interpreter.var_literal list) : bool =
  try
    List.for_all2
      (fun old_def new_def ->
        match (old_def, new_def) with
        | Bir_interpreter.SimpleVar old_l, Bir_interpreter.SimpleVar new_l -> old_l = new_l
        | ( Bir_interpreter.TableVar (old_size, old_table),
            Bir_interpreter.TableVar (new_size, new_table) ) ->
            old_size = new_size
            && Array.for_all
                 (fun x -> x)
                 (Array.map2 (fun old_idx new_idx -> old_idx = new_idx) old_table new_table)
        | _ -> false)
      old_def_pattern new_def_pattern
  with Invalid_argument _ -> false

let merge_code_coverage_single_results_with_acc (results : code_coverage_result)
    (acc : code_coverage_acc) : code_coverage_acc =
  Mir.VariableMap.fold
    (fun var (new_def_pattern : Bir_interpreter.var_literal list) acc ->
      match (Mir.VariableMap.find_opt var acc, new_def_pattern) with
      | None, [ SimpleVar Undefined ] -> Mir.VariableMap.add var OnlyUndefined acc
      | None, [ new_def ] -> Mir.VariableMap.add var (OnlyOneDef new_def) acc
      | None, _ -> Mir.VariableMap.add var (OnlyOneDefPattern new_def_pattern) acc
      | Some MultipleDefPatterns, _ | Some MultipleDefs, _ -> acc
      | Some (OnlyOneDefPattern old_def_pattern), _ ->
          if same_def_pattern old_def_pattern new_def_pattern then acc
          else Mir.VariableMap.add var MultipleDefPatterns acc
      | Some (OnlyOneDef old_def), [ SimpleVar Undefined ] ->
          Mir.VariableMap.add var (OnlyOneDefAndUndefined old_def) acc
      | Some (OnlyOneDef old_def), [ new_def ] ->
          if old_def = new_def then acc else Mir.VariableMap.add var MultipleDefs acc
      | Some (OnlyOneDef _), _ -> Mir.VariableMap.add var MultipleDefPatterns acc
      | Some OnlyUndefined, [ SimpleVar Undefined ] -> Mir.VariableMap.add var OnlyUndefined acc
      | Some OnlyUndefined, [ new_def ] ->
          Mir.VariableMap.add var (OnlyOneDefAndUndefined new_def) acc
      | Some OnlyUndefined, _ -> Mir.VariableMap.add var MultipleDefPatterns acc
      | Some (OnlyOneDefAndUndefined _), [ SimpleVar Undefined ] -> acc
      | Some (OnlyOneDefAndUndefined _), [ SimpleVar _ ] -> Mir.VariableMap.add var MultipleDefs acc
      | Some (OnlyOneDefAndUndefined _), _ -> Mir.VariableMap.add var MultipleDefPatterns acc)
    results acc

let merge_code_coverage_acc (acc1 : code_coverage_acc) (acc2 : code_coverage_acc) :
    code_coverage_acc =
  Mir.VariableMap.union
    (fun _ def1 def2 ->
      match (def1, def2) with
      | MultipleDefPatterns, _ | _, MultipleDefPatterns -> Some MultipleDefPatterns
      | OnlyOneDefPattern _, (MultipleDefs | OnlyOneDef _ | OnlyOneDefAndUndefined _ | OnlyUndefined)
      | ( (MultipleDefs | OnlyOneDef _ | OnlyOneDefAndUndefined _ | OnlyUndefined),
          OnlyOneDefPattern _ ) ->
          Some MultipleDefPatterns
      | OnlyOneDefPattern def1, OnlyOneDefPattern def2 ->
          if same_def_pattern def1 def2 then Some (OnlyOneDefPattern def1)
          else Some MultipleDefPatterns
      | MultipleDefs, (MultipleDefs | OnlyOneDefAndUndefined _ | OnlyOneDef _ | OnlyUndefined)
      | (OnlyOneDefAndUndefined _ | OnlyOneDef _ | OnlyUndefined), MultipleDefs ->
          Some MultipleDefs
      | OnlyOneDefAndUndefined _, (OnlyOneDefAndUndefined _ | OnlyOneDef _)
      | OnlyOneDef _, OnlyOneDefAndUndefined _ ->
          Some MultipleDefs
      | OnlyOneDefAndUndefined def, OnlyUndefined | OnlyUndefined, OnlyOneDefAndUndefined def ->
          Some (OnlyOneDefAndUndefined def)
      | OnlyOneDef def1, OnlyOneDef def2 ->
          if def1 = def2 then Some (OnlyOneDef def1) else Some MultipleDefs
      | OnlyOneDef def, OnlyUndefined | OnlyUndefined, OnlyOneDef def ->
          Some (OnlyOneDefAndUndefined def)
      | OnlyUndefined, OnlyUndefined -> Some OnlyUndefined)
    acc1 acc2
