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

open Specifisc

module GlobalValueNumbering = struct

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

  module BooleanNumberExp = struct
    type t =
      | Comparison of comparison_op * ValueNumber.t * ValueNumber.t
      | LogicalBinop of logical_binop * ValueNumber.t * ValueNumber.t
      | LogicalNot of ValueNumber.t
      | BoolLiteral of bool
      | BoolVar of BoolVariable.t
    let compare = compare
  end

  module BooleanNumberExpMap = Map.Make(BooleanNumberExp)

  module ArithmeticNumberExp = struct
    type t =
      | ArithmeticBinop of arithmetic_binop * ValueNumber.t * ValueNumber.t
      | ArithmeticMinus of ValueNumber.t
      | Conditional of ValueNumber.t * ValueNumber.t * ValueNumber.t
      | IntLiteral of Int64.t
      | IntVar of IntVariable.t
    let compare = compare
  end

  module ArithmeticNumberExpMap = Map.Make(ArithmeticNumberExp)

  type int_definition =
    | DefIntVar of IntVariable.t Ast.marked
    | DefIntLiteral of Int64.t Ast.marked

  type bool_definition =
    | DefBoolVar of BoolVariable.t Ast.marked
    | DefBoolLiteral of bool Ast.marked

  type data = {
    int_numbering : ValueNumber.t ArithmeticNumberExpMap.t;
    int_definitions :  int_definition ValueNumberMap.t;
    bool_numbering : ValueNumber.t BooleanNumberExpMap.t;
    bool_definitions :  bool_definition ValueNumberMap.t;
  }

  let empty_data = {
    int_numbering = ArithmeticNumberExpMap.empty;
    int_definitions = ValueNumberMap.empty;
    bool_numbering = BooleanNumberExpMap.empty;
    bool_definitions = ValueNumberMap.empty;
  }

  let update_data_bool (expn : BooleanNumberExp.t) (data : data) : ValueNumber.t * data =
    begin match BooleanNumberExpMap.find_opt expn data.bool_numbering with
      | Some vn -> vn, data
      | None ->
        let vn = ValueNumber.fresh () in
        (vn, {data with bool_numbering = BooleanNumberExpMap.add expn vn data.bool_numbering})
    end

  let update_data_int (expn : ArithmeticNumberExp.t) (data : data) : ValueNumber.t * data =
    begin match ArithmeticNumberExpMap.find_opt expn data.int_numbering with
      | Some vn -> vn, data
      | None ->
        let vn = ValueNumber.fresh () in
        (vn, {data with int_numbering = ArithmeticNumberExpMap.add expn vn data.int_numbering})
    end

  let rec logical_expr_to_value_number
      (e: logical_expression Ast.marked)
      (data : data)
    : ValueNumber.t * data = match Ast.unmark e with
    | Comparison (op, e1, e2) ->
      let ne1, data = arithmetic_expr_to_value_number e1 data in
      let ne2, data = arithmetic_expr_to_value_number e2 data in
      let expn = BooleanNumberExp.Comparison (Ast.unmark op, ne1, ne2) in
      update_data_bool expn data
    | LogicalBinop (op, e1, e2) ->
      let ne1, data = logical_expr_to_value_number e1 data in
      let ne2, data = logical_expr_to_value_number e2 data in
      let expn = BooleanNumberExp.LogicalBinop (Ast.unmark op, ne1, ne2) in
      update_data_bool expn data
    | LogicalNot e1 ->
      let ne1, data = logical_expr_to_value_number e1 data in
      let expn = BooleanNumberExp.LogicalNot ne1 in
      update_data_bool expn data
    | BoolLiteral b ->
      let expn = BooleanNumberExp.BoolLiteral b in
      update_data_bool expn data
    | BoolVar var ->
      let expn = BooleanNumberExp.BoolVar var in
      update_data_bool expn data

  and arithmetic_expr_to_value_number
      (e: arithmetic_expression Ast.marked)
      (data : data)
    : ValueNumber.t * data = match Ast.unmark e with
    | ArithmeticBinop (op, e1, e2) ->
      let ne1, data = arithmetic_expr_to_value_number e1 data in
      let ne2, data = arithmetic_expr_to_value_number e2 data in
      let expn = ArithmeticNumberExp.ArithmeticBinop (Ast.unmark op, ne1, ne2) in
      update_data_int expn data
    | ArithmeticMinus e1 ->
      let ne1, data = arithmetic_expr_to_value_number e1 data in
      let expn = ArithmeticNumberExp.ArithmeticMinus ne1 in
      update_data_int expn data
    | Conditional (e1, e2, e3) ->
      let ne1, data = logical_expr_to_value_number e1 data in
      let ne2, data = arithmetic_expr_to_value_number e2 data in
      let ne3, data = arithmetic_expr_to_value_number e3 data in
      let expn = ArithmeticNumberExp.Conditional (ne1, ne2, ne3) in
      update_data_int expn data
    | IntLiteral i ->
      let expn = ArithmeticNumberExp.IntLiteral i in
      update_data_int expn data
    | IntVar var ->
      let expn = ArithmeticNumberExp.IntVar var in
      update_data_int expn data

  let bool_definition_to_expression (def: bool_definition) : logical_expression Ast.marked =
    match def with
    | DefBoolLiteral b -> Ast.same_pos_as (BoolLiteral (Ast.unmark b)) b
    | DefBoolVar v -> Ast.same_pos_as (BoolVar (Ast.unmark v)) v

  let int_definition_to_expression (def: int_definition) : arithmetic_expression Ast.marked =
    match def with
    | DefIntLiteral b -> Ast.same_pos_as (IntLiteral (Ast.unmark b)) b
    | DefIntVar v -> Ast.same_pos_as (IntVar (Ast.unmark v)) v

  let rec gvn_bool_exp (e: logical_expression Ast.marked) (data: data)
    : logical_expression Ast.marked * data * ValueNumber.t =
    let expn, data = logical_expr_to_value_number e data in
    match Ast.unmark e with
    | BoolLiteral _ -> (e, data, expn)
    | _ -> begin match ValueNumberMap.find_opt expn  data.bool_definitions with
        | Some def -> (bool_definition_to_expression def, data, expn)
        | None -> begin match Ast.unmark e with
            | BoolLiteral _ -> assert false
            | Comparison (op, e1, e2) ->
              let ne1, data, _ = gvn_int_exp e1 data in
              let ne2, data, _ = gvn_int_exp e2 data in
              Ast.same_pos_as (Comparison (op, ne1, ne2)) e, data, expn
            | LogicalBinop (op, e1, e2) ->
              let ne1, data, _ = gvn_bool_exp e1 data in
              let ne2, data, _ = gvn_bool_exp e2 data in
              Ast.same_pos_as (LogicalBinop (op, ne1, ne2)) e, data, expn
            | LogicalNot e1 ->
              let ne1, data, _ = gvn_bool_exp e1 data in
              Ast.same_pos_as (LogicalNot ne1) e, data, expn
            | BoolVar _ ->
              e, data, expn
          end
      end

  and gvn_int_exp (e: arithmetic_expression Ast.marked) (data: data)
    : arithmetic_expression Ast.marked * data * ValueNumber.t =
    let expn, data = arithmetic_expr_to_value_number e data in
    match Ast.unmark e with
    | IntLiteral _ -> (e, data, expn)
    | _ -> begin match ValueNumberMap.find_opt expn  data.int_definitions with
        | Some def -> (int_definition_to_expression def, data, expn)
        | None -> begin match Ast.unmark e with
            | IntLiteral _ -> assert false
            | Conditional (e1, e2, e3) ->
              let ne1, data, _ = gvn_bool_exp e1 data in
              let ne2, data, _ = gvn_int_exp e2 data in
              let ne3, data, _ = gvn_int_exp e3 data in
              Ast.same_pos_as (Conditional (ne1, ne2,ne3)) e, data, expn
            | ArithmeticBinop (op, e1, e2) ->
              let ne1, data, _ = gvn_int_exp e1 data in
              let ne2, data, _ = gvn_int_exp e2 data in
              Ast.same_pos_as (ArithmeticBinop (op, ne1, ne2)) e, data, expn
            | ArithmeticMinus e1 ->
              let ne1, data, _ = gvn_int_exp e1 data in
              Ast.same_pos_as (ArithmeticMinus ne1) e, data, expn
            | IntVar _ ->
              e, data, expn
          end
      end

  let gvn_command
      (c: command)
      (data: data)
    : (command * data) = match c with
    | BoolDef (var, e) ->
      let new_e,data, expn = gvn_bool_exp e data in
      let data =
        { data with
          bool_definitions = ValueNumberMap.update expn (fun def -> match def with
              | None -> Some (DefBoolVar (Ast.same_pos_as var e))
              | Some _ -> def (* we always keep the old definition ! *)
            ) data.bool_definitions
        } in
      BoolDef (var, new_e), data
    | IntDef (var, e) ->
      let new_e, data, expn = gvn_int_exp e data in
      let data =
        { data with
          int_definitions = ValueNumberMap.update expn (fun def -> match def with
              | None -> Some (DefIntVar (Ast.same_pos_as var e))
              | Some _ -> def (* we always keep the old definition ! *)
            ) data.int_definitions
        } in
      IntDef (var, new_e), data
    | Constraint e ->
      let new_e,data, _ = gvn_bool_exp e data in
      Constraint new_e, data

  let optimize (p: program) : program =
    { p with
      program_functions = FunctionVariableMap.map (fun func ->
          { func with
            body =
              let data = empty_data in
              let new_body, _ = List.fold_left (fun (new_body, data) cmd ->
                  let new_cmd, data = gvn_command cmd data in
                  new_cmd::new_body, data
                ) ([], data) func.body
              in
              List.rev new_body
          }
        ) p.program_functions
    }

end

module DeadCodeElimination = struct

  type data = {
    used_bool_vars : unit BoolVariableMap.t;
    used_int_vars: unit IntVariableMap.t;
  }

  let empty_data (func: func) = {
    used_bool_vars = List.fold_left (fun acc var ->
        BoolVariableMap.add var () acc
      ) BoolVariableMap.empty (snd func.outputs);
    used_int_vars =  List.fold_left (fun acc var ->
        IntVariableMap.add var () acc
      ) IntVariableMap.empty (fst func.outputs);
  }

  let add_bool_use (var: BoolVariable.t) (data: data) : data =
    { data with
      used_bool_vars = BoolVariableMap.add var () data.used_bool_vars
    }

  let add_int_use (var: IntVariable.t) (data: data) : data =
    { data with
      used_int_vars = IntVariableMap.add var () data.used_int_vars
    }

  let rec process_bool_expr (e: logical_expression Ast.marked) (data: data) : data =
    match Ast.unmark e with
    | Comparison (_, e1, e2) ->
      let data = process_int_expr e1 data in
      let data = process_int_expr e2 data in
      data
    | LogicalBinop (_, e1, e2) ->
      let data = process_bool_expr e1 data in
      let data = process_bool_expr e2 data in
      data
    | LogicalNot e1 ->
      let data = process_bool_expr e1 data in
      data
    | BoolLiteral _ -> data
    | BoolVar v -> add_bool_use v data

  and process_int_expr (e: arithmetic_expression Ast.marked) (data: data) : data =
    match Ast.unmark e with
    | ArithmeticBinop (_, e1, e2) ->
      let data = process_int_expr e1 data in
      let data = process_int_expr e2 data in
      data
    | ArithmeticMinus e1 ->
      let data = process_int_expr e1 data in
      data
    | Conditional (e1, e2, e3) ->
      let data = process_bool_expr e1 data in
      let data = process_int_expr e2 data in
      let data = process_int_expr e3 data in
      data
    | IntLiteral _ -> data
    | IntVar v -> add_int_use v data

  let process_command (c: command) (data: data)
    : bool * data = match c with
    | BoolDef (var, e) ->
      let is_necessary = BoolVariableMap.mem var data.used_bool_vars in
      let data =
        if is_necessary then
          process_bool_expr e data
        else data
      in
      (is_necessary, data)
    | IntDef (var, e) ->
      let is_necessary = IntVariableMap.mem var data.used_int_vars in
      let data =
        if is_necessary then
          process_int_expr e data
        else data
      in
      (is_necessary, data)
    | Constraint e ->
      (true, process_bool_expr e data)

  let optimize (p: program) : program =
    { p with
      program_functions = FunctionVariableMap.map (fun func ->
          { func with
            body =
              let data = empty_data func in
              let new_body, _ = List.fold_right (fun cmd (new_body, data) ->
                  let is_necessary, data = process_command cmd data in
                  (if is_necessary then cmd::new_body else new_body), data
                ) func.body ([], data)
              in
              new_body
          }
        ) p.program_functions
    }
end
