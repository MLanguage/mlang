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

(**
   This modules defines M's static semantic. The typechecking is done with a standard bidirectionnal
   algorithm featuring unification. There are only four types : undefined, boolean , integer and real.
   The unification is done according to strict order between those types, as almost all operators
   are polymorphic.
*)

module Pos = Specifisc.Pos
open Mvg

(** This module defines the internal representation of type variables during the typechecking. *)
module Typ =
struct

  (** [Up] corresponds to type inference, [Down] to type checking *)
  type direction = Up | Down

  (** Used to display nice error messages for type errors *)
  type infer_info = (Pos.position * direction)

  (** Only 4 different types in [T]. There is a strict order on those types. *)
  module T = struct
    type t =
      | Real
      | Integer
      | Boolean
      | All
    [@@deriving eq, ord]
  end

  module UF = Union_find.Make(T)

  type t = (UF.t * infer_info)

  exception UnificationError of string * string

  let format_typ (t:t) : string =
    Printf.sprintf "%s (%s %s)"
      (match UF.find (fst t) with
       | T.Integer -> "integer"
       | T.Real -> "real"
       | T.Boolean -> "boolean"
       | T.All -> "unconstrained")
      (match snd (snd t) with Up -> "inferred" | Down -> "constrained")
      (Pos.format_position (fst (snd t)))

  let create_variable (pos: infer_info) : t = (UF.create T.All, pos)

  let to_concrete (t: t) : typ =
    match UF.find (fst t) with
    | T.Integer -> Mvg.Integer
    | T.Real -> Mvg.Real
    | T.Boolean -> Mvg.Boolean
    | T.All -> Mvg.Boolean

  let boolean (pos: infer_info) = (UF.create T.Boolean, pos)
  let integer (pos: infer_info) = (UF.create T.Integer, pos)
  let real (pos: infer_info) = (UF.create T.Real, pos)
  let integer_or_real (pos: infer_info) = (UF.create T.All, pos)

  let create_concrete (t: typ) (pos: infer_info) : t = match t with
    | Mvg.Integer -> integer pos
    | Mvg.Real -> real pos
    | Mvg.Boolean -> boolean pos

  let is_lattice_transition (t1: t) (t2:t) : bool =
    match (UF.find (fst t1),UF.find (fst t2)) with
    | (T.All, (T.Real | T.Boolean | T.Integer))
    | (T.Boolean, (T.Integer | T.Real))
    | (T.Integer, T.Real)
      -> true
    | (t1, t2) when t1 = t2 -> true
    | _ -> false


  let coerce (t: t) (coerce: t) : unit =
    if UF.find (fst coerce) = T.All || is_lattice_transition t coerce then
      ()
    else
      raise (UnificationError (
          format_typ t,
          format_typ coerce
        ))


  let unify (t1: t) (t2:t) : t =
    if is_lattice_transition t1 t2 then begin
      UF.union (fst t1) (fst t2); t2
    end else if is_lattice_transition t2 t1 then begin
      UF.union (fst t1) (fst t2); t1
    end else
      raise (UnificationError (
          format_typ t1,
          format_typ t2
        ))
end

type ctx = {
  ctx_program : program;
  ctx_var_typ: Typ.t VariableMap.t;
  ctx_local_var_typ: Typ.t LocalVariableMap.t;
  ctx_is_generic_table: bool;
}



type typ_info = {
  typ_info_var : (Mvg.typ * bool) Mvg.VariableMap.t; (* the bool flag is_table *)
  typ_info_local_var : Mvg.typ Mvg.LocalVariableMap.t
}

let rec typecheck_top_down
    (ctx: ctx)
    (e: expression Pos.marked)
    (t: typ)
  : ctx =
  match (Pos.unmark e, t) with
  | (Comparison (_, e1, e2), Boolean) ->
    let (ctx, t1) = typecheck_bottom_up ctx e1 in
    let (ctx, t2) = typecheck_bottom_up ctx e2 in
    begin try
        ignore (Typ.unify t1 t2);
        ctx
      with
      | Typ.UnificationError _ ->
        raise (Errors.TypeError (
            Errors.Typing
              (Printf.sprintf "expression %s (%s) of type %s has not the same type than expression %s of type %s"
                 (Format_mvg.format_expression (Pos.unmark e1))
                 (Pos.format_position (Pos.get_position e1))
                 (Typ.format_typ t1)
                 (Pos.format_position (Pos.get_position e2))
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
        Typ.coerce t1 (Typ.boolean (Pos.get_position e1, Typ.Down));
        let (ctx, t2) = typecheck_bottom_up ctx e2 in
        let (ctx, t3) = typecheck_bottom_up ctx e3 in
        begin try
            let t' = Typ.unify t2 t3 in
            Typ.coerce t' (Typ.create_concrete t (Pos.get_position e, Typ.Down));
            ctx
          with
          | Typ.UnificationError _ ->
            raise (Errors.TypeError (
                Errors.Typing
                  (Printf.sprintf "expression %s (%s) of type %s has not the same type than expression %s of type %s, and both should be of type %s"
                     (Format_mvg.format_expression (Pos.unmark e1))
                     (Pos.format_position (Pos.get_position e1))
                     (Typ.format_typ t1)
                     (Pos.format_position (Pos.get_position e2))
                     (Typ.format_typ t2)
                     (Format_mvg.format_typ t)
                  )))
        end
      with
      | Typ.UnificationError _ ->
        raise (Errors.TypeError (
            Errors.Typing
              (Printf.sprintf "expression %s (%s) of type %s should be of type boolean"
                 (Format_mvg.format_expression (Pos.unmark e1))
                 (Pos.format_position (Pos.get_position e1))
                 (Typ.format_typ t1)
              )))
    end
  | (FunctionCall (func, args), t) ->
    let typechecker = typecheck_func_args func (Pos.get_position e) in
    let (ctx, t') = typechecker ctx args in
    begin try Typ.coerce t' (Typ.create_concrete t (Pos.get_position e, Typ.Down)); ctx with
      | Typ.UnificationError _ ->
        raise (Errors.TypeError (
            Errors.Typing
              (Printf.sprintf "function call to %s (%s) return type is %s but should be %s"
                 (Format_mvg.format_func func)
                 (Pos.format_position (Pos.get_position e))
                 (Typ.format_typ t')
                 (Format_mvg.format_typ t)
              )))
    end
  | (Literal (Bool _), t)
  | (Literal (Int 0), t)
  | (Literal (Int 1), t)
    ->
    Typ.coerce
      ((Typ.boolean (Pos.get_position e, Typ.Up)))
      ((Typ.create_concrete t (Pos.get_position e, Typ.Down)));
    ctx
  | (Literal (Int _), t) ->
    Typ.coerce
      ((Typ.integer (Pos.get_position e, Typ.Up)))
      ((Typ.create_concrete t (Pos.get_position e, Typ.Down)));
    ctx
  | (Literal (Float _), t) ->
    Typ.coerce
      ((Typ.real (Pos.get_position e, Typ.Up)))
      ((Typ.create_concrete t (Pos.get_position e, Typ.Down)));
    ctx
  | (Literal Undefined, _) ->
    (* Literal has all the types *)
    ctx
  | (Var var, t) ->
    begin try match (VariableMap.find var ctx.ctx_program.program_vars).var_typ with
      | Some t' ->
        let t = Typ.create_concrete t (Pos.get_position e, Typ.Down) in
        let t' = Typ.create_concrete t' (Pos.get_position e, Typ.Up) in
        begin try let t' = Typ.unify t t' in
            { ctx with
              ctx_var_typ = VariableMap.add var t' ctx.ctx_var_typ;
            }
          with
          |  Typ.UnificationError (string_t, string_t') ->
            raise (Errors.TypeError (
                Errors.Typing
                  (Printf.sprintf "variable %s used %s should be of type %s but has been declared of type %s"
                     (Pos.unmark var.Variable.name)
                     (Pos.format_position (Pos.get_position e))
                     (string_t)
                     (string_t')
                  )))
        end
      | None ->
        let (ctx, t') = typecheck_bottom_up ctx e in
        begin try let t' = Typ.unify t' (Typ.create_concrete t (Pos.get_position e, Typ.Down)) in
            { ctx with
              ctx_var_typ = VariableMap.add var t' ctx.ctx_var_typ;
            }
          with
          | Typ.UnificationError (t_msg, _) ->
            raise (Errors.TypeError (
                Errors.Typing
                  (Printf.sprintf "variable %s %s is of type %s but should be %s"
                     (Pos.unmark var.Variable.name)
                     (Pos.format_position (Pos.get_position var.Variable.name))
                     t_msg
                     (Format_mvg.format_typ t)
                  )))
        end
      with
      | Not_found ->
        assert false (* should not happen *)
    end
  | (LocalLet (local_var, e1, e2), t) ->
    let (ctx, t1) = typecheck_bottom_up ctx e1 in
    let ctx = { ctx with ctx_local_var_typ = LocalVariableMap.add local_var t1 ctx.ctx_local_var_typ } in
    let (ctx, t2) = typecheck_bottom_up ctx e2 in
    begin try Typ.coerce t2 (Typ.create_concrete t (Pos.get_position e, Typ.Down)); ctx with
      | Typ.UnificationError (t2_msg, _) ->
        raise (Errors.TypeError (
            Errors.Typing
              (Printf.sprintf "expression %s has type %s but should be %s"
                 (Pos.format_position (Pos.get_position e))
                 t2_msg
                 (Format_mvg.format_typ t)
              )))
    end
  | (Error, _) ->
    ctx
  | (LocalVar local_var, t) ->
    let t' = try LocalVariableMap.find local_var ctx.ctx_local_var_typ with
      | Not_found -> assert false (* should not happen *)
    in
    begin try Typ.coerce t' (Typ.create_concrete t (Pos.get_position e, Typ.Down)); ctx with
      | Typ.UnificationError (t_msg, _) ->
        raise (Errors.TypeError (
            Errors.Typing
              (Printf.sprintf "expression %s has type %s but should be %s"
                 (Pos.format_position (Pos.get_position e))
                 t_msg
                 (Format_mvg.format_typ t)
              )))
    end
  | (GenericTableIndex, Integer) ->
    if ctx.ctx_is_generic_table then
      ctx
    else
      raise (Errors.TypeError
               (Errors.Variable
                  (Printf.sprintf "Generic table index appears outside of table %s"
                     (Pos.format_position (Pos.get_position e)))))
  | (Index ((var, var_pos), e'), t) ->
    let ctx = typecheck_top_down ctx e' Integer in
    let var_data = VariableMap.find var ctx.ctx_program.program_vars in
    begin match var_data.Mvg.var_definition with
      | SimpleVar _ | InputVar ->
        raise (Errors.TypeError
                 (Errors.Typing
                    (Printf.sprintf "variable %s is accessed %s as a table but it is not defined as one %s"
                       (Pos.unmark var.Variable.name)
                       (Pos.format_position var_pos)
                       (Pos.format_position (Pos.get_position var.Variable.name))
                    )))
      | TableVar _ ->
        begin try
            let var_typ = VariableMap.find var ctx.ctx_var_typ in
            begin try
                Typ.coerce
                  var_typ
                  (Typ.create_concrete t (Pos.get_position e, Typ.Down));
                ctx
              with
              | Typ.UnificationError (t_msg, _) ->
                raise (Errors.TypeError (
                    Errors.Typing
                      (Printf.sprintf "expression %s has type %s but should be %s"
                         (Pos.format_position (Pos.get_position e))
                         t_msg
                         (Format_mvg.format_typ t)
                      )))
            end
          with
          | Not_found ->
            { ctx with
              ctx_var_typ = VariableMap.add var
                  (Typ.create_concrete t (Pos.get_position e, Typ.Down)) ctx.ctx_var_typ;
            }
        end
    end
  | _ -> raise (Errors.TypeError (
      Errors.Typing
        (Printf.sprintf "expression %s (%s) should be of type %s, but is of type %s"
           (Format_mvg.format_expression (Pos.unmark e))
           (Pos.format_position (Pos.get_position e))
           (Format_mvg.format_typ t)
           (
             let (_, t') =  typecheck_bottom_up ctx e in
             Typ.format_typ t'
           )
        )))

and typecheck_func_args (f: func) (pos: Pos.position) :
  (ctx -> Mvg.expression Pos.marked list -> ctx * Typ.t) =
  match f with
  | SumFunc | MinFunc | MaxFunc -> fun ctx args ->
    if List.length args = 0 then
      raise (Errors.TypeError
               (Errors.Typing
                  (Printf.sprintf "sum function must be called %s with at least one argument"
                     (Pos.format_position pos)
                  )))
    else
      let (ctx, t1) = typecheck_bottom_up ctx (List.hd args) in
      begin try
          Typ.coerce t1 (Typ.integer_or_real (Pos.get_position (List.hd args), Typ.Down));
          let (ctx, t1) = List.fold_left (fun (ctx, t1) arg ->
              let (ctx, t_arg) = typecheck_bottom_up ctx arg in
              begin try
                  let t1 = Typ.unify t_arg t1 in
                  (ctx, t1) with
              | Typ.UnificationError (t_arg_msg,t2_msg) ->
                raise (Errors.TypeError
                         (Errors.Typing
                            (Printf.sprintf "function argument %s (%s) has type %s but should have type %s"
                               (Format_mvg.format_expression (Pos.unmark arg))
                               (Pos.format_position (Pos.get_position arg))
                               t_arg_msg
                               t2_msg
                            )))
              end
            ) (ctx, t1) args
          in
          (ctx, t1)
        with
        | Typ.UnificationError (t_arg_msg,t2_msg) ->
          raise (Errors.TypeError
                   (Errors.Typing
                      (Printf.sprintf "function argument %s (%s) has type %s but should have type %s"
                         (Format_mvg.format_expression (Pos.unmark (List.hd args)))
                         (Pos.format_position (Pos.get_position (List.hd args)))
                         t_arg_msg
                         t2_msg
                      )))
      end
  | AbsFunc ->
    fun ctx args ->
      begin match args with
        | [arg] ->
          let (ctx, t_arg) = typecheck_bottom_up ctx arg in
          begin try Typ.coerce t_arg (Typ.integer_or_real (Pos.get_position arg, Typ.Down)); (ctx, t_arg) with
            | Typ.UnificationError (t_arg_msg,t2_msg) ->
              raise (Errors.TypeError
                       (Errors.Typing
                          (Printf.sprintf "function argument %s (%s) has type %s but should have type %s"
                             (Format_mvg.format_expression (Pos.unmark arg))
                             (Pos.format_position (Pos.get_position arg))
                             t_arg_msg
                             t2_msg
                          )))
          end
        | _ -> raise (Errors.TypeError
                        (Errors.Typing
                           (Printf.sprintf "function abs %s should have only one arguemnt"
                              (Pos.format_position pos)
                           )))
      end
  | PresentFunc | NullFunc | GtzFunc | GtezFunc | Supzero ->
    (* These functions return a integer value encoding a boolean; 0 for false and 1 for true *)
    fun ctx args ->
      begin match args with
        | [arg] ->
          let (ctx, t_arg) = typecheck_bottom_up ctx arg in
          begin try Typ.coerce t_arg (Typ.integer_or_real (Pos.get_position arg, Typ.Down)); (ctx, t_arg) with
            | Typ.UnificationError (t_arg_msg,t2_msg) ->
              raise (Errors.TypeError
                       (Errors.Typing
                          (Printf.sprintf "function argument %s (%s) has type %s but should have type %s"
                             (Format_mvg.format_expression (Pos.unmark arg))
                             (Pos.format_position (Pos.get_position arg))
                             t_arg_msg
                             t2_msg
                          )))
          end
        | _ -> raise (Errors.TypeError
                        (Errors.Typing
                           (Printf.sprintf "function %s should have only one arguemnt"
                              (Pos.format_position pos)
                           )))
      end
  | ArrFunc | InfFunc ->
    fun ctx args ->
      begin match args with
        | [arg] ->
          let (ctx, t_arg) = typecheck_bottom_up ctx arg in
          begin try
              Typ.coerce t_arg (Typ.real (Pos.get_position arg, Typ.Down));
              (ctx, Typ.integer (pos, Typ.Down))
            with
            | Typ.UnificationError (t_arg_msg,t2_msg) ->
              raise (Errors.TypeError
                       (Errors.Typing
                          (Printf.sprintf "function argument %s (%s) has type %s but should have type %s"
                             (Format_mvg.format_expression (Pos.unmark arg))
                             (Pos.format_position (Pos.get_position arg))
                             t_arg_msg
                             t2_msg
                          )))
          end
        | _ -> raise (Errors.TypeError
                        (Errors.Typing
                           (Printf.sprintf "function %s should have only one argument"
                              (Pos.format_position pos)
                           )))
      end
  | Mvg.Multimax  ->
    fun ctx args ->
      begin match args with
        | [bound; table] ->
          let ctx = typecheck_top_down ctx bound Integer in
          typecheck_bottom_up ctx table
        | _ -> raise (Errors.TypeError
                        (Errors.Typing
                           (Printf.sprintf "function %s should have two arguments"
                              (Pos.format_position pos)
                           )))
      end

and typecheck_bottom_up (ctx: ctx) (e: expression Pos.marked) : (ctx * Typ.t) =
  match Pos.unmark e with
  | Var var ->
    begin try (ctx, VariableMap.find var ctx.ctx_var_typ) with
      | Not_found ->
        let t = Typ.create_variable (Pos.get_position e, Typ.Up) in
        let ctx = { ctx with ctx_var_typ = VariableMap.add var t ctx.ctx_var_typ } in
        (ctx, t)
    end
  | Literal (Int _) -> (ctx, Typ.integer (Pos.get_position e, Typ.Up))
  | Literal (Float _) -> (ctx, Typ.real (Pos.get_position e, Typ.Up))
  | Literal (Bool _) -> (ctx, Typ.boolean (Pos.get_position e, Typ.Up))
  | Literal (Undefined) -> (ctx, Typ.create_variable (Pos.get_position e, Typ.Up))
  | Binop ((Ast.And, _ | Ast.Or, _), e1, e2) ->
    let ctx = typecheck_top_down ctx e1 Boolean in
    let ctx = typecheck_top_down ctx e2 Boolean in
    (ctx, Typ.boolean (Pos.get_position e, Typ.Up))
  | Binop ((Ast.Add, _ | Ast.Sub, _ | Ast.Mul, _ | Ast.Div, _) as op, e1, e2) ->
    let (ctx, t1) = typecheck_bottom_up ctx e1 in
    let (ctx, t2) = typecheck_bottom_up ctx e2 in
    begin try
        let t' = Typ.unify t1 t2 in
        (ctx, t')
      with
      | Typ.UnificationError (t1_msg, t2_msg) ->
        raise (Errors.TypeError
                 (Errors.Typing
                    (Printf.sprintf "arguments of operator (%s) %s have to be of the same type but instead are of type %s and %s"
                       (Format_ast.format_binop (Pos.unmark op))
                       (Pos.format_position (Pos.get_position op))
                       t1_msg
                       t2_msg
                    )))
    end
  | Comparison (_, e1, e2) ->
    let (ctx, t1) = typecheck_bottom_up ctx e1 in
    let (ctx, t2) = typecheck_bottom_up ctx e2 in
    begin try
        ignore (Typ.unify t1 t2);
        (ctx, Typ.boolean (Pos.get_position e, Typ.Up)) with
    | Typ.UnificationError _ ->
      raise (Errors.TypeError (
          Errors.Typing
            (Printf.sprintf "expression %s (%s) of type %s has not the same type than expression %s of type %s"
               (Format_mvg.format_expression (Pos.unmark e1))
               (Pos.format_position (Pos.get_position e1))
               (Typ.format_typ t1)
               (Pos.format_position (Pos.get_position e2))
               (Typ.format_typ t2)
            )))
    end
  | Unop (Ast.Not, e) ->
    let ctx = typecheck_top_down ctx e Boolean in
    (ctx, Typ.boolean (Pos.get_position e, Typ.Up))
  | Unop (Ast.Minus, e') ->
    let (ctx, t) = typecheck_bottom_up ctx e' in
    begin try Typ.coerce t (Typ.integer_or_real (Pos.get_position e', Typ.Down)); (ctx, t) with
      | Typ.UnificationError (t1_msg, t2_msg) ->
        raise (Errors.TypeError
                 (Errors.Typing
                    (Printf.sprintf "arguments of operator (%s) %s has type %s but should be of type %s"
                       (Format_ast.format_unop (Ast.Minus))
                       (Pos.format_position (Pos.get_position e))
                       t1_msg
                       t2_msg
                    )))
    end
  | GenericTableIndex ->
    if ctx.ctx_is_generic_table then
      (ctx, Typ.integer (Pos.get_position e, Typ.Up))
    else
      raise (Errors.TypeError
               (Errors.Variable
                  (Printf.sprintf "Generic table index appears outside of table %s"
                     (Pos.format_position (Pos.get_position e)))))
  | Index ((var, var_pos), e') ->
    let ctx = typecheck_top_down ctx e' Integer in
    let var_data = VariableMap.find var ctx.ctx_program.program_vars in
    begin match var_data.Mvg.var_definition with
      | SimpleVar _ | InputVar ->
        raise (Errors.TypeError
                 (Errors.Typing
                    (Printf.sprintf "variable %s is accessed %s as a table but it is not defined as one %s"
                       (Pos.unmark var.Variable.name)
                       (Pos.format_position var_pos)
                       (Pos.format_position (Pos.get_position var.Variable.name))
                    )))
      | TableVar _ ->
        begin try (ctx, VariableMap.find var ctx.ctx_var_typ) with
          | Not_found ->
            let t = Typ.create_variable (Pos.get_position e', Typ.Up) in
            let ctx = { ctx with ctx_var_typ = VariableMap.add var t ctx.ctx_var_typ } in
            (ctx, t)
        end
    end
  | Conditional (e1, e2, e3) ->
    let (ctx, t1) = typecheck_bottom_up ctx e1 in
    begin try
        Typ.coerce t1 (Typ.boolean (Pos.get_position e1, Typ.Down));
        let (ctx, t2) = typecheck_bottom_up ctx e2 in
        let (ctx, t3) = typecheck_bottom_up ctx e3 in
        begin try
            let t' = Typ.unify t2 t3 in (ctx, t')
          with
          | Typ.UnificationError _ ->
            raise (Errors.TypeError (
                Errors.Typing
                  (Printf.sprintf "expression %s (%s) of type %s has not the same type than expression %s of type %s"
                     (Format_mvg.format_expression (Pos.unmark e1))
                     (Pos.format_position (Pos.get_position e1))
                     (Typ.format_typ t1)
                     (Pos.format_position (Pos.get_position e2))
                     (Typ.format_typ t2)
                  )))
        end
      with
      | Typ.UnificationError _ ->
        raise (Errors.TypeError (
            Errors.Typing
              (Printf.sprintf "expression %s (%s) of type %s should be of type boolean"
                 (Format_mvg.format_expression (Pos.unmark e1))
                 (Pos.format_position (Pos.get_position e1))
                 (Typ.format_typ t1)
              )))
    end
  | FunctionCall(func, args) ->
    let typechecker = typecheck_func_args func (Pos.get_position e) in
    let (ctx, t') = typechecker ctx args in
    (ctx, t')
  | Error  -> (ctx, Typ.create_variable (Pos.get_position e, Typ.Up))
  | LocalVar local_var ->
    begin try (ctx, LocalVariableMap.find local_var ctx.ctx_local_var_typ) with
      | Not_found -> assert false (* should not happen *)
    end
  | LocalLet (local_var, e1, e2) ->
    let (ctx, t1) = typecheck_bottom_up ctx e1 in
    let ctx = { ctx with ctx_local_var_typ = LocalVariableMap.add local_var t1 ctx.ctx_local_var_typ } in
    let (ctx, t2) = typecheck_bottom_up ctx e2 in
    (ctx, t2)

let determine_def_complete_cover
    (table_var: Mvg.Variable.t)
    (size: int)
    (defs: (int * Pos.position) list)
  : int list =
  (* Return all undefined indexes *)
  let defs_array = Array.make size false in
  List.iter (fun (def, def_pos) ->
      try
        defs_array.(def) <- true
      with
      | Invalid_argument _ ->
        raise (Errors.TypeError (
            Errors.Variable
              (Printf.sprintf "the definitions of index %d %s, from table %s of size %d declared %s is out of bounds"
                 def
                 (Pos.format_position def_pos)
                 (Pos.unmark table_var.Variable.name)
                 size
                 (Pos.format_position (Pos.get_position table_var.Variable.name))
              )))
    ) defs;
  let undefined = ref [] in
  Array.iteri (fun index defined ->
      if not defined then
        undefined := index::!undefined
    ) defs_array;
  List.sort compare !undefined

let typecheck_program_conds (ctx: ctx) (conds: condition_data VariableMap.t) : ctx =
  VariableMap.fold (fun _ cond ctx ->
      typecheck_top_down ctx cond.cond_expr Mvg.Boolean
    ) conds ctx

let rec check_non_recursivity_expr (e: expression Pos.marked) (lvar :Variable.t) : unit =
  match Pos.unmark e with
  | Comparison (_, e1, e2) | Binop (_, e1, e2) | LocalLet (_, e1, e2) ->
    check_non_recursivity_expr e1 lvar;
    check_non_recursivity_expr e2 lvar
  | Unop (_, e1) ->
    check_non_recursivity_expr e1 lvar
  | Index (_, e1) ->
    (*
      We don't check for recursivity in indexes because tables can refer to themselves in definition.
      It is only at runtime that we return [Undefined] for index definitions that refer to indexes
      of the same table greater than themselves.
    *)
    check_non_recursivity_expr e1 lvar
  | Conditional (e1, e2, e3) ->
    check_non_recursivity_expr e1 lvar;
    check_non_recursivity_expr e2 lvar;
    check_non_recursivity_expr e3 lvar
  | FunctionCall (_, args) ->
    List.iter (fun arg -> check_non_recursivity_expr arg lvar) args
  | Literal _ | LocalVar _ | GenericTableIndex | Error -> ()
  | Var var -> if var = lvar then
      raise (Errors.TypeError (
          Errors.Variable
            (Printf.sprintf "You cannot refer to the variable %s since you are defining it (%s)"
               (Pos.unmark var.Variable.name)
               (Pos.format_position (Pos.get_position e))
            )))
    else ()

let check_non_recursivity_of_variable_defs (var: Variable.t) (def: variable_def) : unit =
  match def with
  | SimpleVar e ->
    check_non_recursivity_expr e var
  | TableVar _ | InputVar -> ()

(* The typechecker returns a new program because it defines missing table entries as "undefined" *)
let typecheck (p: program) : typ_info * program =
  let (are_tables, ctx, p_vars) = Mvg.VariableMap.fold (fun var def (acc, ctx, p_vars) ->
      check_non_recursivity_of_variable_defs var def.var_definition;
      match def.var_typ with
      | Some t -> begin match def.var_definition with
          | SimpleVar e ->
            let new_ctx = typecheck_top_down { ctx with ctx_is_generic_table = false } e t in
            (VariableMap.add var false acc,
             new_ctx,
             p_vars)
          | TableVar (size, defs) -> begin match defs with
              | IndexGeneric e ->
                let new_ctx = typecheck_top_down { ctx with ctx_is_generic_table = true } e t in
                (VariableMap.add var true acc,
                 new_ctx,
                 p_vars)
              | IndexTable es ->
                let new_ctx = IndexMap.fold (fun _ e ctx ->
                    let new_ctx = typecheck_top_down { ctx with ctx_is_generic_table = false } e t in
                    new_ctx
                  ) es ctx in
                let undefined_indexes = determine_def_complete_cover var size
                    (List.map (fun (x,e) -> (x, Pos.get_position e)) (IndexMap.bindings es))
                in
                if List.length undefined_indexes = 0 then
                  (VariableMap.add var true acc,
                   new_ctx,
                   p_vars)
                else begin
                  let previous_var_def =
                    Ast_to_mvg.get_var_from_name
                      p.program_idmap
                      var.Variable.name
                      var.Variable.execution_number
                      false
                  in
                  let new_es = List.fold_left (fun es undef_index ->
                      Mvg.IndexMap.add undef_index
                        (Pos.same_pos_as
                           (Mvg.Index (
                               Pos.same_pos_as previous_var_def var.Mvg.Variable.name,
                               Pos.same_pos_as (Mvg.Literal (Int undef_index)) var.Mvg.Variable.name))
                           var.Mvg.Variable.name) es
                    ) es undefined_indexes in
                  (VariableMap.add var true acc,
                   new_ctx,
                   Mvg.VariableMap.add var
                     { def with Mvg.var_definition = Mvg.TableVar (size, Mvg.IndexTable new_es)}
                     p_vars
                  )
                end
            end
          | InputVar ->
            (VariableMap.add var false acc,
             ctx,
             p_vars)
        end
      | None -> begin match def.var_definition with
          | SimpleVar e ->
            let (new_ctx, t) = typecheck_bottom_up { ctx with ctx_is_generic_table = false } e in
            let t = try
                Typ.unify t (Mvg.VariableMap.find var ctx.ctx_var_typ)
              with
              | Not_found -> t
            in
            let new_ctx =
              { new_ctx with
                ctx_var_typ = Mvg.VariableMap.add var t new_ctx.ctx_var_typ
              } in
            (VariableMap.add var false acc, new_ctx, p_vars)
          | TableVar (size, defs) -> begin match defs with
              | IndexGeneric e ->
                let (new_ctx, t) = typecheck_bottom_up { ctx with ctx_is_generic_table = true } e in
                let t = try
                    Typ.unify t (Mvg.VariableMap.find var ctx.ctx_var_typ)
                  with
                  | Not_found -> t
                in
                let new_ctx =
                  { new_ctx with
                    ctx_var_typ = Mvg.VariableMap.add var t new_ctx.ctx_var_typ
                  } in
                (VariableMap.add var true acc, new_ctx, p_vars)
              | IndexTable es ->
                let (new_ctx, t) = IndexMap.fold (fun _ e (ctx, old_t) ->
                    let (new_ctx, t) = typecheck_bottom_up { ctx with ctx_is_generic_table = false } e in
                    begin try
                        let t = Typ.unify t old_t in
                        let new_ctx =
                          { new_ctx with
                            ctx_var_typ = Mvg.VariableMap.add var t new_ctx.ctx_var_typ
                          } in
                        (new_ctx, t)
                      with
                      | Typ.UnificationError (t1_msg, t2_msg) ->
                        raise (Errors.TypeError (
                            Errors.Typing
                              (Printf.sprintf "different definitions of specific index of table variable %s declared %s have different indexes: %s and %s"
                                 (Pos.unmark var.Variable.name)
                                 (Pos.format_position (Pos.get_position var.Variable.name))
                                 t1_msg
                                 t2_msg
                              )))
                    end
                  ) es (ctx, Typ.create_variable (Pos.get_position var.Variable.name, Typ.Up)) in
                let t = try
                    Typ.unify t (Mvg.VariableMap.find var ctx.ctx_var_typ)
                  with
                  | Not_found -> t
                in
                let new_ctx =
                  { new_ctx with
                    ctx_var_typ = Mvg.VariableMap.add var t new_ctx.ctx_var_typ
                  } in
                let undefined_indexes = determine_def_complete_cover var size
                    (List.map (fun (x,e) -> (x, Pos.get_position e)) (IndexMap.bindings es))
                in
                if List.length undefined_indexes = 0 then
                  (VariableMap.add var true acc, new_ctx, p_vars)
                else begin
                  let previous_var_def =
                    Ast_to_mvg.get_var_from_name
                      p.program_idmap
                      var.Variable.name
                      var.Variable.execution_number
                      false
                  in
                  let new_es = List.fold_left (fun es undef_index ->
                      Mvg.IndexMap.add undef_index
                        (Pos.same_pos_as
                           (Mvg.Index (
                               Pos.same_pos_as previous_var_def var.Mvg.Variable.name,
                               Pos.same_pos_as (Mvg.Literal (Int undef_index)) var.Mvg.Variable.name))
                           var.Mvg.Variable.name) es
                    ) es undefined_indexes in
                  (VariableMap.add var true acc, new_ctx,
                   Mvg.VariableMap.add var
                     { def with Mvg.var_definition = Mvg.TableVar (size, Mvg.IndexTable new_es)}
                     p_vars
                  )
                end
            end
          | InputVar ->
            if VariableMap.mem var acc then
              (acc, ctx, p_vars)
            else begin
              (VariableMap.add var false acc, ctx, p_vars)
            end
        end;
    ) p.program_vars (Mvg.VariableMap.empty,
                      { ctx_program = p;
                        ctx_var_typ = VariableMap.merge (fun var _ def ->
                            match def with
                            | None -> assert false (* should not happen *)
                            | Some def -> begin match def.var_typ with
                                | Some t -> Some (Typ.create_concrete t (Pos.get_position var.Variable.name, Typ.Down))
                                | None -> None
                              end
                          ) VariableMap.empty p.program_vars;
                        ctx_local_var_typ = LocalVariableMap.empty;
                        ctx_is_generic_table = false
                      }, p.program_vars)
  in
  let ctx = typecheck_program_conds ctx p.program_conds in
  ({
    typ_info_var = VariableMap.merge (fun _ t is_table -> match (t, is_table) with
        | (Some t, Some is_table) ->
          Some (Typ.to_concrete t, is_table)
        | (None, Some _) ->
          (*
            This case is needed because of declared but undefined and unused variables
          *)
          None
        | (Some t, None) ->
          (*
            In this case, the variable is declared without type, used but never defined : we say it's not a table
          *)
          Some (Typ.to_concrete t, false)
        | _ -> assert false (* should not happen *)
      ) ctx.ctx_var_typ are_tables;
    typ_info_local_var = LocalVariableMap.map (fun t -> Typ.to_concrete t) ctx.ctx_local_var_typ;
  },
    { p with program_vars = p_vars }
  )