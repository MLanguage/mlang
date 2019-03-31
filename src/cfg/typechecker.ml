(*
  Copyright 2018 Denis Merigoux and INRIA

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

open Cfg

module Typ =
struct
  let t_eq t1 t2 =  match (t1, t2) with
    | (None, None) -> true
    | (Some _ , None) | (None, Some _) -> false
    | (Some Integer, Some Integer)
    | (Some Real, Some Real) | (Some Boolean, Some Boolean) -> true
    | _ -> false

  module UF = Union_find.Make(struct
      type t = typ option
      let equal t1 t2 = t_eq t1 t2
    end)

  type t = UF.t

  let integer : t = UF.create (Some Integer)
  let real : t = UF.create (Some Real)
  let boolean : t = UF.create (Some Boolean)

  let create_concrete (t: typ) : t =
    match t with
    | Integer -> integer
    | Real -> real
    | Boolean -> boolean

  let create_variable () : t = UF.create None

  let is_boolean (t: t) : bool =
    t_eq (UF.find_repr t) (UF.find_repr boolean)

  let is_integer (t: t) : bool =
    t_eq (UF.find_repr t) (UF.find_repr integer)

  let is_real (t:t) : bool =
    t_eq (UF.find_repr t) (UF.find_repr real)

  exception UnificationError of typ * typ

  let unify (t1: t) (t2: t) : unit =
    UF.union t1 t2;
    if t_eq (UF.find_repr integer) (UF.find_repr real) then
      raise (UnificationError (Integer, Real))
    else if t_eq (UF.find_repr integer) (UF.find_repr boolean) then
      raise (UnificationError (Integer, Boolean))
    else if t_eq (UF.find_repr real) (UF.find_repr boolean) then
      raise (UnificationError (Real, Boolean))
    else
      ()

  let to_concrete (t: t) : typ =
    if t_eq (UF.find_repr t) (UF.find_repr real) then
      Real
    else if t_eq (UF.find_repr t) (UF.find_repr boolean) then
      Boolean
    else if t_eq (UF.find_repr real) (UF.find_repr integer) then
      Integer
    else Boolean

  let format_typ (t:t) =
    if t_eq (UF.find_repr t) (UF.find_repr real) then
      Format_cfg.format_typ Real
    else if t_eq (UF.find_repr t) (UF.find_repr boolean) then
      Format_cfg.format_typ Boolean
    else if t_eq (UF.find_repr real) (UF.find_repr integer) then
      Format_cfg.format_typ Integer
    else "unknown"

end

type ctx = {
  ctx_program : program;
  ctx_var_typ: Typ.t VariableMap.t;
}

let rec typecheck_top_down
    (ctx: ctx)
    (e: expression Ast.marked)
    (t: typ)
  : ctx =
  match (Ast.unmark e, t) with
  | (Comparison (op, e1, e2), Boolean) ->
    let (ctx, t1) = typecheck_bottom_up ctx e1 in
    let (ctx, t2) = typecheck_bottom_up ctx e2 in
    begin try Typ.unify t1 t2; ctx with
      | Typ.UnificationError _ ->
        raise (Errors.TypeError (
            Errors.Typing
              (Printf.sprintf "expression %s of type %s has not the same type than expression %s of type %s"
                 (Format_ast.format_position (Ast.get_position e1))
                 (Typ.format_typ t1)
                 (Format_ast.format_position (Ast.get_position e2))
                 (Typ.format_typ t2)
              )))
    end
  | (Binop (((Ast.And | Ast.Or), _), e1, e2), Boolean)
  | (Binop (((Ast.Add | Ast.Sub | Ast.Mul | Ast.Div), _), e1, e2), (Integer | Real)) ->
    let ctx = typecheck_top_down ctx e1 t in
    let ctx = typecheck_top_down ctx e2 t in
    ctx
  | (Unop (Ast.Not, e), Boolean)
  | (Unop (Ast.Minus, e), (Integer | Real))  ->
    let ctx = typecheck_top_down ctx e t in
    ctx
  | (Conditional (e1, e2, e3), t) ->
    let (ctx, t1) = typecheck_bottom_up ctx e1 in
    begin try
        Typ.unify t1 Typ.boolean;
        let (ctx, t2) = typecheck_bottom_up ctx e2 in
        let (ctx, t3) = typecheck_bottom_up ctx e3 in
        begin try
            Typ.unify t2 t3; Typ.unify t2 (Typ.create_concrete t); ctx
          with
          | Typ.UnificationError _ ->
            raise (Errors.TypeError (
                Errors.Typing
                  (Printf.sprintf "expression %s of type %s has not the same type than expression %s of type %s, and both should be of type %s"
                     (Format_ast.format_position (Ast.get_position e1))
                     (Typ.format_typ t1)
                     (Format_ast.format_position (Ast.get_position e2))
                     (Typ.format_typ t2)
                     (Format_cfg.format_typ t)
                  )))
        end
      with
      | Typ.UnificationError _ ->
        raise (Errors.TypeError (
            Errors.Typing
              (Printf.sprintf "expression %s of type %s should be of type boolean"
                 (Format_ast.format_position (Ast.get_position e1))
                 (Typ.format_typ t1)
              )))
    end
  | (FunctionCall (func, args), t) ->
    let typechecker = typecheck_func_args func (Ast.get_position e) in
    let (ctx, t') = typechecker ctx args in
    begin try Typ.unify t' (Typ.create_concrete t); ctx with
      | Typ.UnificationError _ ->
        raise (Errors.TypeError (
            Errors.Typing
              (Printf.sprintf "function call %s return type is %s but should be %s"
                 (Format_ast.format_position (Ast.get_position e))
                 (Typ.format_typ t')
                 (Format_cfg.format_typ t)
              )))
    end
  | (Literal (Int _), Integer)
  | (Literal (Float _), Real)
  | (Literal (Bool _), Boolean) -> ctx
  | (Var var, t) ->
    begin try match (VariableMap.find var ctx.ctx_program).var_typ with
      | Some t' -> if t = t' then ctx else
          raise (Errors.TypeError (
              Errors.Typing
                (Printf.sprintf "variable %s used %s should be of type %s but has been declared of type %s"
                   (Ast.unmark var.Variable.name)
                   (Format_ast.format_position (Ast.get_position e))
                   (Format_cfg.format_typ t)
                   (Format_cfg.format_typ t')
                )))
      | None -> assert false
      with
      | Not_found -> assert false (* should not happen *)
    end
  | (LocalLet (var, e1, e2), t) ->
    assert false
  | (Error, t) ->
    ctx
  | (LocalVar var, t) ->
    assert false
  | (GenericTableIndex, Integer) -> ctx
  | _ -> raise (Errors.TypeError (
      Errors.Typing
        (Printf.sprintf "expression %s should be of type %s, but is of type %s"
           (Format_ast.format_position (Ast.get_position e))
           (Format_cfg.format_typ t)
           (
             let (_, t') =  typecheck_bottom_up ctx e in
             Typ.format_typ t'
           )
        )))

and typecheck_func_args (f: func) (pos: Ast.position) :
  (ctx -> Cfg.expression Ast.marked list -> ctx * Typ.t) =
  match f with
  | SumFunc -> fun ctx args ->
    if List.length args = 0 then
      raise (Errors.TypeError
               (Errors.Typing
                  (Printf.sprintf "sum function must be called %s with at least one argument"
                     (Format_ast.format_position pos)
                  )))
    else
      let (ctx, t1) = typecheck_bottom_up ctx (List.hd args) in
      if Typ.is_integer t1 || Typ.is_real t1 then
        let ctx = List.fold_left (fun ctx arg ->
            let (ctx, t_arg) = typecheck_bottom_up ctx arg in
            begin try Typ.unify t_arg t1; ctx with
              | Typ.UnificationError _ ->
                raise (Errors.TypeError
                         (Errors.Typing
                            (Printf.sprintf "function argument %s has type %s but should have type %s"
                               (Format_ast.format_position (Ast.get_position arg))
                               (Typ.format_typ t_arg)
                               (Typ.format_typ t1)
                            )))
            end
          ) ctx args
        in
        (ctx, t1)
      else
        raise (Errors.TypeError
                 (Errors.Typing
                    (Printf.sprintf "sum function argument %s has type %s which is incompatible"
                       (Format_ast.format_position (Ast.get_position  (List.hd args)))
                       (Typ.format_typ t1)
                    )))
  | _ -> assert false

and typecheck_bottom_up (ctx: ctx) (e: expression Ast.marked) : (ctx * Typ.t) =
  match Ast.unmark e with
  | Var var ->
    begin try (ctx, VariableMap.find var ctx.ctx_var_typ) with
      | Not_found ->
        let t = Typ.create_variable () in
        let ctx = { ctx with ctx_var_typ = VariableMap.add var t ctx.ctx_var_typ } in
        (ctx, t)
    end
  | Literal (Int _) -> (ctx, Typ.integer)
  | Literal (Float _) -> (ctx, Typ.real)
  | Literal (Bool _) -> (ctx, Typ.boolean)
  | Binop ((Ast.And, _ | Ast.Or, _), e1, e2) ->
    let ctx = typecheck_top_down ctx e1 Boolean in
    let ctx = typecheck_top_down ctx e2 Boolean in
    (ctx, Typ.boolean)
  | Binop ((Ast.Add, _ | Ast.Sub, _ | Ast.Mul, _ | Ast.Div, _) as op, e1, e2) ->
    let (ctx, t1) = typecheck_bottom_up ctx e1 in
    let (ctx, t2) = typecheck_bottom_up ctx e2 in
    begin try Typ.unify t1 t2; (ctx, t1) with
      | Typ.UnificationError (t1, t2) ->
        raise (Errors.TypeError
                 (Errors.Typing
                    (Printf.sprintf "arguments of operator (%s) %s have to be of the same type but instead are of type %s and %s"
                       (Format_ast.format_binop (Ast.unmark op))
                       (Format_ast.format_position (Ast.get_position op))
                       (Format_cfg.format_typ t1)
                       (Format_cfg.format_typ t2)
                    )))
    end
  | _ -> raise (Errors.TypeError (
      Errors.Typing
        (Printf.sprintf "cannot determine type of expression %s"
           (Format_ast.format_position (Ast.get_position e))
        )))

let typecheck (p: program) : typ VariableMap.t =
  let (types, _) = Cfg.VariableMap.fold (fun var def (acc, ctx) ->
      match def.var_typ with
      | Some t -> begin match def.var_definition with
          | SimpleVar e ->
            let new_ctx = typecheck_top_down ctx e t in
            (VariableMap.add var t acc, new_ctx)
          | TableVar (size, defs) -> begin match defs with
              | IndexGeneric e ->
                let new_ctx = typecheck_top_down ctx e t in
                (VariableMap.add var t acc, new_ctx)
              | _ -> assert false
            end
          | InputVar ->
            (acc, ctx)
        end
      | None -> assert false
    ) p (Cfg.VariableMap.empty,
         { ctx_program = p;
           ctx_var_typ = VariableMap.merge (fun _ _ def ->
               match def with
               | None -> assert false (* should not happen *)
               | Some def -> begin match def.var_typ with
                   | Some t -> Some (Typ.create_concrete t)
                   | None -> None
                 end
             ) VariableMap.empty p
         })
  in
  types
