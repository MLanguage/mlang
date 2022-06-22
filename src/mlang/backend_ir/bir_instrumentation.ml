(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
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

module CodeLocationMap = Map.Make (struct
  type t = Bir_interpreter.code_location

  let compare x y = compare x y
end)

type code_coverage_result =
  Bir_interpreter.var_literal CodeLocationMap.t Bir.VariableMap.t
(** The result of the code coverage measurement is a map of the successive
    definitions of all the non-array variables during interpretation of the
    program *)

let empty_code_coverage_result : code_coverage_result = Bir.VariableMap.empty

let code_coverage_acc : code_coverage_result ref =
  ref empty_code_coverage_result

let code_coverage_init () : unit =
  code_coverage_acc := empty_code_coverage_result;
  Bir_interpreter.assign_hook :=
    fun var literal code_loc ->
      code_coverage_acc :=
        Bir.VariableMap.update var
          (fun old_defs ->
            match old_defs with
            | None -> Some (CodeLocationMap.singleton code_loc (literal ()))
            | Some old_defs ->
                Some (CodeLocationMap.add code_loc (literal ()) old_defs))
          !code_coverage_acc

let code_coverage_result () : code_coverage_result = !code_coverage_acc

module VarLiteralSet = Set.Make (struct
  type t = Bir_interpreter.var_literal

  let compare x y =
    match (x, y) with
    | Bir_interpreter.SimpleVar l1, Bir_interpreter.SimpleVar l2 ->
        compare l1 l2
    | Bir_interpreter.TableVar (size1, t1), Bir_interpreter.TableVar (size2, t2)
      -> (
        if size1 <> size2 then compare size1 size2
        else
          let different = ref None in
          Array.iter2
            (fun t1i t2i ->
              if t1i = t2i then () else different := Some (compare t1i t2i))
            t1 t2;
          match !different with None -> 0 | Some i -> i)
    | _ -> compare x y
end)

type code_coverage_map_value = VarLiteralSet.t

type code_coverage_acc =
  code_coverage_map_value CodeLocationMap.t Bir.VariableMap.t

let merge_code_coverage_single_results_with_acc (results : code_coverage_result)
    (acc : code_coverage_acc) : code_coverage_acc =
  Bir.VariableMap.fold
    (fun var (new_defs : Bir_interpreter.var_literal CodeLocationMap.t) acc ->
      match Bir.VariableMap.find_opt var acc with
      | None ->
          Bir.VariableMap.add var
            (CodeLocationMap.map (fun x -> VarLiteralSet.singleton x) new_defs)
            acc
      | Some old_defs ->
          Bir.VariableMap.add var
            (CodeLocationMap.fold
               (fun code_loc new_def defs ->
                 match CodeLocationMap.find_opt code_loc defs with
                 | None ->
                     CodeLocationMap.add code_loc
                       (VarLiteralSet.singleton new_def)
                       defs
                 | Some old_def ->
                     CodeLocationMap.add code_loc
                       (VarLiteralSet.add new_def old_def)
                       defs)
               new_defs old_defs)
            acc)
    results acc

let merge_code_coverage_acc (acc1 : code_coverage_acc)
    (acc2 : code_coverage_acc) : code_coverage_acc =
  Bir.VariableMap.union
    (fun _ defs1 defs2 ->
      Some
        (CodeLocationMap.union
           (fun _ def1 def2 -> Some (VarLiteralSet.union def1 def2))
           defs1 defs2))
    acc1 acc2

type code_locs = Bir.variable CodeLocationMap.t

let rec get_code_locs_stmt (p : Bir.program) (stmt : Bir.stmt)
    (loc : Bir_interpreter.code_location) : code_locs =
  match Pos.unmark stmt with
  | Bir.SConditional (_, t, f) ->
      CodeLocationMap.union
        (fun _ _ _ -> assert false)
        (get_code_locs_stmts p t
           (Bir_interpreter.ConditionalBranch true :: loc))
        (get_code_locs_stmts p f
           (Bir_interpreter.ConditionalBranch false :: loc))
  | Bir.SVerif _ -> CodeLocationMap.empty
  | Bir.SAssign (var, _) -> CodeLocationMap.singleton loc var
  | Bir.SRuleCall r ->
      get_code_locs_stmts p
        (Bir.rule_or_verif_as_statements (Bir.ROVMap.find r p.rules_and_verifs))
        (Bir_interpreter.InsideRule r :: loc)
  | Bir.SFunctionCall (f, _) ->
      get_code_locs_stmts p (Bir.FunctionMap.find f p.mpp_functions).mppf_stmts
        (Bir_interpreter.InsideFunction f :: loc)

and get_code_locs_stmts (p : Bir.program) (stmts : Bir.stmt list)
    (loc : Bir_interpreter.code_location) : code_locs =
  let locs, _ =
    List.fold_left
      (fun (locs, i) stmt ->
        ( CodeLocationMap.union
            (fun _ _ _ -> assert false)
            (get_code_locs_stmt p stmt (Bir_interpreter.InsideBlock i :: loc))
            locs,
          i + 1 ))
      (CodeLocationMap.empty, 0) stmts
  in
  locs

let get_code_locs (p : Bir.program) : code_locs =
  get_code_locs_stmts p (Bir.main_statements p) []
