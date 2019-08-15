(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

module Pos = Verifisc.Pos
open Mvg

module ValueNumber = struct
  type t = int

  let counter = ref 0

  let fresh () =
    let out = !counter in
    counter := out + 1;
    out

  let compare = compare
end

module ValueNumberMap = Map.Make(ValueNumber)

module NumberExp = struct
  type t =
    | Comparison of Ast.comp_op * ValueNumber.t * ValueNumber.t
    | Binop of Ast.binop * ValueNumber.t * ValueNumber.t
    | Unop of Ast.unop * ValueNumber.t
    | Index of Variable.t * ValueNumber.t
    | Conditional of ValueNumber.t * ValueNumber.t * ValueNumber.t
    | FunctionCall of func * ValueNumber.t list
    | Literal of literal
    | Var of Variable.t
    | LocalVar of LocalVariable.t

  let compare = compare
end

module NumberExpMap = Map.Make(NumberExp)

type definition =
  | DefVar of Variable.t Pos.marked
  | DefLiteral of literal Pos.marked

let definition_to_expression (def: definition) : expression Pos.marked = match def with
  | DefVar var -> Pos.same_pos_as (Var (Pos.unmark var)) var
  | DefLiteral l -> Pos.same_pos_as (Literal (Pos.unmark l)) l

type data = {
  numbering : ValueNumber.t NumberExpMap.t;
  definitions :  definition ValueNumberMap.t;
}

let empty_data = {
  numbering = NumberExpMap.empty;
  definitions = ValueNumberMap.empty;
}

let rec exp_to_value_number
    (e : expression Pos.marked)
    (data : data)
  : (ValueNumber.t * data) =
  let update_data expn data =
    begin match NumberExpMap.find_opt expn data.numbering with
      | Some vn -> vn, data
      | None ->
        let vn = ValueNumber.fresh () in
        (vn, {data with numbering = NumberExpMap.add expn vn data.numbering})
    end
  in
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
    let ne1, data = exp_to_value_number e1 data in
    let ne2, data = exp_to_value_number e2 data in
    let expn = NumberExp.Comparison (Pos.unmark op, ne1, ne2) in
    update_data expn data
  | Binop (op, e1, e2) ->
    let ne1, data = exp_to_value_number e1 data in
    let ne2, data = exp_to_value_number e2 data in
    let expn = NumberExp.Binop (Pos.unmark op, ne1, ne2) in
    update_data expn data
  | Unop (op, e1) ->
    let ne1, data = exp_to_value_number e1 data in
    let expn = NumberExp.Unop (op, ne1) in
    update_data expn data
  | Index(var, e1) ->
    let ne1, data = exp_to_value_number e1 data in
    let expn = NumberExp.Index (Pos.unmark var, ne1) in
    update_data expn data
  | Conditional (e1, e2, e3) ->
    let ne1, data = exp_to_value_number e1 data in
    let ne2, data = exp_to_value_number e2 data in
    let ne3, data = exp_to_value_number e3 data in
    let expn = NumberExp.Conditional (ne1, ne2, ne3) in
    update_data expn data
  | FunctionCall (func, args) ->
    let nargs, data = List.fold_left (fun (nargs,data) arg ->
        let narg, data = exp_to_value_number arg data in
        narg::nargs, data
      ) ([], data) args in
    let nargs = List.rev nargs in
    let expn = NumberExp.FunctionCall (func, nargs) in
    update_data expn data
  | Literal lit ->
    let expn = NumberExp.Literal lit in
    update_data expn data
  | Var var ->
    let expn = NumberExp.Var var in
    update_data expn data
  | LocalVar lvar ->
    let expn = NumberExp.LocalVar lvar in
    update_data expn data
  | GenericTableIndex | Error ->
    let vn = ValueNumber.fresh () in
    (vn, data)
  | LocalLet (lvar, e1, e2) ->
    let ne1, data = exp_to_value_number e1 data in
    let expne1 = NumberExp.LocalVar lvar in
    let data = { data with
                 numbering = NumberExpMap.add expne1 ne1 data.numbering;
               } in
    exp_to_value_number e2 data

(**
   So normally GVN requires SSA form. What happens here with the strongly connected components ?
   Well the answer is that it does not matter. Indeed, the evaluation of the program inside SCC takes
   all the variables of a SCC as a block, updating all their values in parallel. There is no
   situation where you are in the middle of the loop, seeing some variables with their old values
   while the new ones have their new value.

   So we don't have to take any precaution to apply SSA to variables inside SCCs. However, the GVN
   will not fully work with a single pass on the SCC, because it will see some variables that are
   have not been defined yet.
*)
let rec gvn_exp
    (e: expression Pos.marked)
    (data : data)
  : expression Pos.marked * data * ValueNumber.t =
  let expn, data = exp_to_value_number e data in
  match Pos.unmark e with
  | Literal _ -> (e, data, expn)
  | _ -> begin match ValueNumberMap.find_opt expn data.definitions with
      | None ->
        begin match Pos.unmark e with
          | Comparison (op, e1, e2) ->
            let new_e1, data, _ = gvn_exp e1 data in
            let new_e2, data, _ = gvn_exp e2 data in
            Pos.same_pos_as (Comparison (op, new_e1, new_e2)) e, data, expn
          | Binop (op, e1, e2) ->
            let new_e1, data, _ = gvn_exp e1 data in
            let new_e2, data, _ = gvn_exp e2 data in
            Pos.same_pos_as (Binop (op, new_e1, new_e2)) e, data, expn
          | Unop (op, e1) ->
            let new_e1, data, _ = gvn_exp e1 data in
            Pos.same_pos_as (Unop (op, new_e1)) e, data, expn
          | Index (var, e1) ->
            let new_e1, data, _ = gvn_exp e1 data in
            Pos.same_pos_as (Index (var, new_e1)) e, data, expn
          | Conditional (e1, e2, e3) ->
            let new_e1, data, _ = gvn_exp e1 data in
            let new_e2, data, _ = gvn_exp e2 data in
            let new_e3, data, _ = gvn_exp e3 data in
            Pos.same_pos_as (Conditional (new_e1, new_e2, new_e3)) e, data, expn
          | FunctionCall (func, args) ->
            let new_args, data = List.fold_left (fun (new_args, data) arg ->
                let new_arg, data, _ = gvn_exp arg data in
                new_arg::new_args, data
              ) ([], data) args
            in
            let new_args = List.rev new_args in
            Pos.same_pos_as (FunctionCall (func, new_args)) e, data, expn
          | Literal _ | Var _ | LocalVar _ | GenericTableIndex | Error -> e, data, expn
          | LocalLet (lvar, e1, e2) ->
            let new_e1, data, _ = gvn_exp e1 data in
            let new_e2, data, _ = gvn_exp e2 data in
            Pos.same_pos_as (LocalLet (lvar, new_e1, new_e2)) e, data, expn
        end
      | Some def -> (definition_to_expression def, data, expn)
    end

let optimize (p: program) : program =
  let exec_order = Execution_order.get_execution_order p in
  let data = empty_data in
  let p, _ = List.fold_left (fun (p, data) scc ->
      VariableMap.fold (fun var _ (p, data) ->
          try
            let def = VariableMap.find var p.program_vars in
            let new_def, data = match def.var_definition with
              | InputVar -> InputVar, data
              | SimpleVar e ->
                let new_e, data, expn = gvn_exp e data in
                let data =
                  { data with
                    definitions = ValueNumberMap.update expn (fun def -> match def with
                        | None -> Some (DefVar (Pos.same_pos_as var e))
                        | Some _ -> def (* we always keep the old definition ! *)
                      ) data.definitions
                  } in
                SimpleVar new_e, data
              | TableVar (size, def) -> begin match def with
                  | IndexGeneric e ->
                    let new_e, data, _ = gvn_exp e data in
                    TableVar(
                      size,
                      IndexGeneric new_e),
                    data
                  | IndexTable es ->
                    let new_es, data = IndexMap.fold (fun idx e (es, data) ->
                        let new_e, data, _ = gvn_exp e data in
                        IndexMap.add idx new_e es, data
                      ) es (IndexMap.empty, data) in
                    TableVar(
                      size,
                      IndexTable new_es
                    ), data
                end
            in
            { p with program_vars =
                       VariableMap.add var { def with var_definition = new_def } p.program_vars
            }, data
          with
          | Not_found ->
            try
              let cond = VariableMap.find var p.program_conds in
              match let e, _ , _ = gvn_exp cond.cond_expr data in e with
              | (Literal (Bool false), _) | (Literal Undefined, _) ->
                { p with
                  program_conds =
                    VariableMap.remove
                      var
                      p.program_conds
                }, data
              | (Literal (Bool true) , _) ->   raise (Interpreter.RuntimeError (
                  Interpreter.ConditionViolated (
                    Printf.sprintf "%s. Errors thrown:\n%s\nViolated condition:\n%s"
                      (Pos.format_position (Pos.get_position cond.cond_expr))
                      (String.concat "\n" (List.map (fun err ->
                           Printf.sprintf "Error %s [%s]" (Pos.unmark err.Error.name) (Pos.unmark err.Error.descr)
                         ) cond.cond_errors))
                      (Format_mvg.format_expression (Pos.unmark cond.cond_expr))
                  ), Interpreter.empty_ctx
                ))
              | new_cond_expr ->
                { p with
                  program_conds =
                    VariableMap.add
                      var
                      { cond with cond_expr = new_cond_expr }
                      p.program_conds
                }, data
            with
            | Not_found -> assert false (* should not happen *)
        ) scc (p, data)
    ) (p, data) exec_order
  in
  p
