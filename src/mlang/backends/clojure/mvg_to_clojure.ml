(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Mvg

let generate_variable (var : Variable.t) : string =
  let v = match var.alias with Some v -> v | None -> Pos.unmark var.Variable.name in
  let v = String.lowercase_ascii v in
  let v =
    if
      same_execution_number var.Variable.execution_number
        (Ast_to_mvg.dummy_exec_number (Pos.get_position var.Variable.name))
    then v
    else
      Format.asprintf "%s_%d_%d" v var.Variable.execution_number.Mvg.rule_number
        var.Variable.execution_number.Mvg.seq_number
  in
  if Re.Str.string_match (Re.Str.regexp "[0-9].+") v 0 then "var_" ^ v else v

let generate_name (v : Variable.t) : string =
  match v.alias with Some v -> v | None -> Pos.unmark v.Variable.name

let generate_comp_op (op : Ast.comp_op) : string =
  match op with
  | Ast.Gt -> "gt_mvalue"
  | Ast.Gte -> "gte_mvalue"
  | Ast.Lt -> "lt_mvalue"
  | Ast.Lte -> "lte_mvalue"
  | Ast.Eq -> "eq_mvalue"
  | Ast.Neq -> "neq_mvalue"

let generate_binop (op : Ast.binop) : string =
  match op with
  | Ast.And -> "and_mvalue"
  | Ast.Or -> "or_mvalue"
  | Ast.Add -> "add_mvalue"
  | Ast.Sub -> "sub_mvalue"
  | Ast.Mul -> "mul_mvalue"
  | Ast.Div -> "div_mvalue"

(* Since there is no way to have inline let bindings, we have to collect all local variables
   created... *)
let rec generate_clj_expr (e : expression Pos.marked) : string =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let s1 = generate_clj_expr e1 in
      let s2 = generate_clj_expr e2 in
      Format.asprintf "%s (%s) (%s)" (generate_comp_op (Pos.unmark op)) s1 s2
  | Binop (op, e1, e2) ->
      let s1 = generate_clj_expr e1 in
      let s2 = generate_clj_expr e2 in
      Format.asprintf "%s (%s) (%s)" (generate_binop (Pos.unmark op)) s1 s2
  | Unop (Ast.Minus, e) ->
      let s = generate_clj_expr e in
      Format.asprintf "minus_mvalue (%s)" s
  | Unop (Ast.Not, e) ->
      let s = generate_clj_expr e in
      Format.asprintf "not_mvalue (%s)" s
  | Index _ -> assert false (* unimplemented *)
  | Conditional (((LocalVar _, _) as e1), e2, e3) ->
      let s1 = generate_clj_expr e1 in
      let s2 = generate_clj_expr e2 in
      let s3 = generate_clj_expr e3 in
      Format.asprintf "if (get (%s) \"is_undef\") (undef nil) (if (get (%s) \"b\") (%s) (%s))" s1 s1
        s2 s3
  | Conditional (e1, e2, e3) ->
      let v1 = LocalVariable.new_var () in
      let new_e =
        Pos.same_pos_as
          (LocalLet
             (v1, e1, Pos.same_pos_as (Conditional (Pos.same_pos_as (LocalVar v1) e1, e2, e3)) e))
          e
      in
      generate_clj_expr new_e
  | FunctionCall (PresentFunc, [ arg ]) ->
      let sarg = generate_clj_expr arg in
      Format.asprintf "present_mvalue (%s)" sarg
  | FunctionCall (NullFunc, [ arg ]) ->
      let sarg = generate_clj_expr arg in
      Format.asprintf "null_mvalue (%s)" sarg
  | FunctionCall (ArrFunc, [ arg ]) ->
      let sarg = generate_clj_expr arg in
      Format.asprintf "round_mvalue (%s)" sarg
  | FunctionCall (InfFunc, [ arg ]) ->
      let sarg = generate_clj_expr arg in
      Format.asprintf "floor_mvalue (%s)" sarg
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float 0.0) -> "zero nil"
  | Literal (Float f) -> Format.asprintf "mk_value %f" f
  | Literal Undefined -> "undef nil"
  | Var var -> Format.asprintf "get values \"%s\"" (generate_variable var)
  | LocalVar lvar -> Format.asprintf "(fn [_] v%d) nil" lvar.LocalVariable.id
  | GenericTableIndex -> assert false (* unimplemented *)
  | Error -> assert false (* TODO *)
  | LocalLet (lvar, e1, e2) ->
      let s1 = generate_clj_expr e1 in
      let s2 = generate_clj_expr e2 in
      let v = Format.asprintf "v%d" lvar.LocalVariable.id in
      Format.asprintf "let [%s (%s)] (%s)" v s1 s2

let generate_var_def (program : program) (var : Variable.t) (oc : Format.formatter) : unit =
  try
    let data = VariableMap.find var program.program_vars in
    if data.var_io = Regular || data.var_io = Output then
      match data.var_definition with
      | SimpleVar e ->
          Format.fprintf oc "; Defined %a\n" Pos.format_position (Pos.get_position e);
          let s = generate_clj_expr e in
          Format.fprintf oc "(defn %s [values] (\n    assoc values \"%s\" (%s)\n"
            (generate_variable var) (generate_variable var) s;
          Format.fprintf oc "))\n\n"
      | TableVar (_, _) -> assert false (* unimplemented *)
      | InputVar -> assert false (* should not happen *)
    else
      Format.fprintf oc
        "; Input value\n(defn %s [values] (assoc values \"%s\" (get values \"%s\")))\n\n"
        (generate_variable var) (generate_variable var) (generate_name var)
  with Not_found ->
    let cond = VariableMap.find var program.program_conds in
    let s = generate_clj_expr cond.cond_expr in
    let fresh = LocalVariable.new_var () in
    let cond_name = Format.asprintf "cond%d" fresh.LocalVariable.id in
    Format.fprintf oc
      "; Verification condition %a\n\
       (defn %s [values] (\n\
      \    let [%s (%s)] (\n\
      \    if (and (not (get %s \"is_undef\")) (get %s \"b\")) (throw (AssertionError. \"Error \
       triggered\\n%s\")) values)\n\
       ))\n\n"
      Pos.format_position (Pos.get_position cond.cond_expr) (generate_variable var) cond_name s
      cond_name cond_name
      (String.concat "\\n"
         (List.map
            (fun err ->
              Format.asprintf "%s: %s" (Pos.unmark err.Error.name) (Pos.unmark err.Error.descr))
            cond.cond_errors))

let generate_clj_program (program : program) (dep_graph : Dependency.DepGraph.t) (filename : string)
    : unit =
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  let exec_order = Execution_order.get_execution_order dep_graph in
  Format.fprintf oc "; %s\n\n" Prelude.message;
  let output_vars =
    List.map
      (fun (var, _) -> var)
      (List.filter
         (fun (_, data) -> data.var_io = Output)
         (VariableMap.bindings program.program_vars))
  in
  Format.fprintf oc
    "\n\
     (defn undef [_] { \"is_undef\" true \"v\" 0.0 })\n\n\
     (defn mtrue [_] { \"b\" true \"is_undef\" false })\n\n\
     (defn mfalse [_] { \"b\" false \"is_undef\" false })\n\n\
     (defn zero [_] { \"v\" 0.0 \"is_undef\" false })\n\n\
     (defn mk_value [v] { \"v\" v \"is_undef\" false })\n\n\
     (defn mk_bool [b] { \"b\" b \"is_undef\" false })\n\n\
     (defn present_mvalue [v] (if (get v \"is_undef\") (mk_bool false) (mk_bool true)))\n\n\
     (defn null_mvalue [v] (if (v \"is_undef\") (mk_bool true) (mk_bool false)))\n\n\
     (defn add_mvalue [v1 v2] (if (and (get v1 \"is_undef\") (get v2 \"is_undef\")) (undef nil) \
     (mk_value (+ (get v1 \"v\") (get v2 \"v\")))))\n\n\
     (defn sub_mvalue [v1 v2] (if (and (get v1 \"is_undef\") (get v2 \"is_undef\")) (undef nil) \
     (mk_value (- (get v1 \"v\") (get v2 \"v\")))))\n\n\
     (defn mul_mvalue [v1 v2] (if (or (get v1 \"is_undef\") (get v2 \"is_undef\")) (undef nil) \
     (mk_value (* (get v1 \"v\") (get v2 \"v\")))))\n\n\
     (defn div_mvalue [v1 v2] (if (or (get v1 \"is_undef\") (get v2 \"is_undef\")) (undef nil) (if \
     (= (get v2 \"v\") 0.0) (undef nil) (mk_value (/ (get v1 \"v\") (get v2 \"v\"))))))\n\n\
     (defn minus_mvalue [v] (if (get v \"is_undef\") (undef nil) (mk_value (- (v \"v\")))))\n\n\
     (defn round_mvalue [v] (if (get v \"is_undef\") (undef nil) (mk_value (Math/round (get v \
     \"v\")))))\n\n\
     (defn floor_mvalue [v] (if (get v \"is_undef\") (undef nil) (mk_value (Math/floor (get v \
     \"v\")))))\n\n\
     (defn not_mvalue [v] (if (get v \"is_undef\") (undef nil) (mk_value (not (get v \"b\")))))\n\n\
     (defn and_mvalue [v1 v2] (if (or (get v1 \"is_undef\") (get v2 \"is_undef\")) (undef nil) \
     (mk_bool (and (get v1 \"b\") (get v2 \"b\")))))\n\n\
     (defn or_mvalue [v1 v2] (if (or (get v1 \"is_undef\") (get v2 \"is_undef\")) (undef nil) \
     (mk_bool (or (get v1 \"b\") (get v2 \"b\")))))\n\n\
     (defn eq_mvalue [v1 v2] (if (or (get v1 \"is_undef\") (get v2 \"is_undef\")) (undef nil) \
     (mk_bool (= (get v1 \"v\") (get v2 \"v\")))))\n\n\
     (defn neq_mvalue [v1 v2] (if (or (get v1 \"is_undef\") (get v2 \"is_undef\")) (undef nil) \
     (mk_bool (not (= (get v1 \"v\") (get v2 \"v\"))))))\n\n\
     (defn lt_mvalue [v1 v2] (if (or (get v1 \"is_undef\") (get v2 \"is_undef\")) (undef nil) \
     (mk_bool (< (get v1 \"v\") (get v2 \"v\")))))\n\n\
     (defn lte_mvalue [v1 v2] (if (or (get v1 \"is_undef\") (get v2 \"is_undef\")) (undef nil) \
     (mk_bool (<= (get v1 \"v\") (get v2 \"v\")))))\n\n\
     (defn gt_mvalue [v1 v2] (if (or (get v1 \"is_undef\") (get v2 \"is_undef\")) (undef nil) \
     (mk_bool (> (get v1 \"v\") (get v2 \"v\")))))\n\n\
     (defn gte_mvalue [v1 v2] (if (or (get v1 \"is_undef\") (get v2 \"is_undef\")) (undef nil) \
     (mk_bool (>= (get v1 \"v\") (get v2 \"v\")))))\n\n";
  List.iter (fun var -> generate_var_def program var oc) exec_order;
  Format.fprintf oc
    "(defn compute_ir [values_0] (let [\n%s\n] (\n    ; Main tax computation.\n    %s\n)))"
    (String.concat "\n"
       (List.mapi
          (fun i (var : Mvg.Variable.t) ->
            Format.asprintf "    values_%d (%s values_%d)\n" (i + 1) (generate_variable var) i)
          exec_order))
    begin
      match output_vars with
      | [ ovar ] ->
          Format.asprintf "get values_%d \"%s\"" (List.length exec_order) (generate_variable ovar)
      | _ -> assert false (* unimplemented *)
    end;
  close_out _oc
