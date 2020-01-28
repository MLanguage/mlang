(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** This modules defines M's static semantic. The typechecking is done with a standard
    bidirectionnal algorithm featuring unification. There are only four types : undefined, boolean ,
    integer and real. The unification is done according to strict order between those types, as
    almost all operators are polymorphic. *)

open Mvg

(** This module defines the internal representation of type variables during the typechecking. *)
module Typ = struct
  (** [Up] corresponds to type inference, [Down] to type checking *)
  type direction = Up | Down

  type infer_info = Pos.position * direction
  (** Used to display nice error messages for type errors *)

  (** Only 4 different types in [T]. There is a strict order on those types. *)
  module T = struct
    type t = Real | Boolean | All [@@deriving eq, ord]
  end

  module UF = Union_find.Make (T)

  type t = UF.t * infer_info

  exception UnificationError of string * string

  let format_typ fmt (t : t) =
    Format.fprintf fmt "%s (%s %a)"
      ( match UF.find (fst t) with
      | T.Real -> "real"
      | T.Boolean -> "boolean"
      | T.All -> "unconstrained" )
      (match snd (snd t) with Up -> "inferred" | Down -> "constrained")
      Pos.format_position
      (fst (snd t))

  let create_variable (pos : infer_info) : t = (UF.create T.All, pos)

  let to_concrete (t : t) : typ =
    match UF.find (fst t) with
    | T.Real -> Mvg.Real
    | T.Boolean -> Mvg.Boolean
    | T.All -> Mvg.Boolean

  let boolean (pos : infer_info) = (UF.create T.Boolean, pos)

  let real (pos : infer_info) = (UF.create T.Real, pos)

  let create_concrete (t : typ) (pos : infer_info) : t =
    match t with Mvg.Real -> real pos | Mvg.Boolean -> boolean pos

  let is_lattice_transition (t1 : t) (t2 : t) : bool =
    match (UF.find (fst t1), UF.find (fst t2)) with
    | T.All, (T.Real | T.Boolean) | T.Boolean, T.Real -> true
    | t1, t2 when t1 = t2 -> true
    | _ -> false

  let coerce (t : t) (coerce : t) : unit =
    if UF.find (fst coerce) = T.All || is_lattice_transition t coerce then ()
    else
      raise
        (UnificationError (Format.asprintf "%a" format_typ t, Format.asprintf "%a" format_typ coerce))

  let unify (t1 : t) (t2 : t) : t =
    if is_lattice_transition t1 t2 then begin
      UF.union (fst t1) (fst t2);
      t2
    end
    else if is_lattice_transition t2 t1 then begin
      UF.union (fst t1) (fst t2);
      t1
    end
    else
      raise
        (UnificationError (Format.asprintf "%a" format_typ t1, Format.asprintf "%a" format_typ t2))
end

type ctx = {
  ctx_program : program;
  ctx_var_typ : Typ.t VariableMap.t;
  ctx_local_var_typ : Typ.t LocalVariableMap.t;
  ctx_is_generic_table : bool;
}

type typ_info = {
  typ_info_var : (Mvg.typ * bool) Mvg.VariableMap.t;
  (* the bool flag is_table *)
  typ_info_local_var : Mvg.typ Mvg.LocalVariableMap.t;
}

let rec typecheck_top_down (ctx : ctx) (e : expression Pos.marked) (t : typ) : ctx =
  match (Pos.unmark e, t) with
  | Comparison (_, e1, e2), Boolean -> (
      let ctx, t1 = typecheck_bottom_up ctx e1 in
      let ctx, t2 = typecheck_bottom_up ctx e2 in
      try
        ignore (Typ.unify t1 t2);
        ctx
      with Typ.UnificationError _ ->
        Errors.raise_typ_error Typing
          "expression %a (%a) of type %a has not the same type than expression %a of type %a"
          Format_mvg.format_expression (Pos.unmark e1) Pos.format_position (Pos.get_position e1)
          Typ.format_typ t1 Pos.format_position (Pos.get_position e2) Typ.format_typ t2 )
  | Binop (((Ast.And | Ast.Or), _), e1, e2), Boolean
  | Binop (((Ast.Add | Ast.Sub | Ast.Mul | Ast.Div), _), e1, e2), Real ->
      let ctx = typecheck_top_down ctx e1 t in
      let ctx = typecheck_top_down ctx e2 t in
      ctx
  | Unop (Ast.Not, e), Boolean | Unop (Ast.Minus, e), Real ->
      let ctx = typecheck_top_down ctx e t in
      ctx
  | Conditional (e1, e2, e3), t -> (
      let ctx, t1 = typecheck_bottom_up ctx e1 in
      try
        Typ.coerce t1 (Typ.boolean (Pos.get_position e1, Typ.Down));
        let ctx, t2 = typecheck_bottom_up ctx e2 in
        let ctx, t3 = typecheck_bottom_up ctx e3 in
        try
          let t' = Typ.unify t2 t3 in
          Typ.coerce t' (Typ.create_concrete t (Pos.get_position e, Typ.Down));
          ctx
        with Typ.UnificationError _ ->
          Errors.raise_typ_error Typing
            "expression %a (%a) of type %a has not the same type than expression %a of type %a, \
             and both should be of type %a"
            Format_mvg.format_expression (Pos.unmark e1) Pos.format_position (Pos.get_position e1)
            Typ.format_typ t1 Pos.format_position (Pos.get_position e2) Typ.format_typ t2
            Format_mvg.format_typ t
      with Typ.UnificationError _ ->
        Errors.raise_typ_error Typing "expression %a (%a) of type %a should be of type boolean"
          Format_mvg.format_expression (Pos.unmark e1) Pos.format_position (Pos.get_position e1)
          Typ.format_typ t1 )
  | FunctionCall (func, args), t -> (
      let typechecker = typecheck_func_args func (Pos.get_position e) in
      let ctx, t' = typechecker ctx args in
      try
        Typ.coerce t' (Typ.create_concrete t (Pos.get_position e, Typ.Down));
        ctx
      with Typ.UnificationError _ ->
        Errors.raise_typ_error Typing "function call to %a (%a) return type is %a but should be %a"
          Format_mvg.format_func func Pos.format_position (Pos.get_position e) Typ.format_typ t'
          Format_mvg.format_typ t )
  | Literal (Bool _), t -> (
      try
        Typ.coerce
          (Typ.boolean (Pos.get_position e, Typ.Up))
          (Typ.create_concrete t (Pos.get_position e, Typ.Down));
        ctx
      with Typ.UnificationError _ ->
        Errors.raise_typ_error Typing "literal %a is a boolean but should be %a" Pos.format_position
          (Pos.get_position e) Format_mvg.format_typ t )
  | Literal (Float _), t -> (
      try
        Typ.coerce
          (Typ.real (Pos.get_position e, Typ.Up))
          (Typ.create_concrete t (Pos.get_position e, Typ.Down));
        ctx
      with Typ.UnificationError _ ->
        Errors.raise_typ_error Typing "literal %a is a float but should be %a" Pos.format_position
          (Pos.get_position e) Format_mvg.format_typ t )
  | Literal Undefined, _ ->
      (* Literal has all the types *)
      ctx
  | Var var, t -> (
      try
        match (VariableMap.find var ctx.ctx_program.program_vars).var_typ with
        | Some t' -> (
            let t = Typ.create_concrete t (Pos.get_position e, Typ.Down) in
            let t' = Typ.create_concrete t' (Pos.get_position e, Typ.Up) in
            try
              let t' = Typ.unify t t' in
              { ctx with ctx_var_typ = VariableMap.add var t' ctx.ctx_var_typ }
            with Typ.UnificationError (string_t, string_t') ->
              Errors.raise_typ_error Typing
                "variable %s used %a should be of type %s but has been declared of type %s"
                (Pos.unmark var.Variable.name) Pos.format_position (Pos.get_position e) string_t
                string_t' )
        | None -> (
            let ctx, t' = typecheck_bottom_up ctx e in
            try
              let t' = Typ.unify t' (Typ.create_concrete t (Pos.get_position e, Typ.Down)) in
              { ctx with ctx_var_typ = VariableMap.add var t' ctx.ctx_var_typ }
            with Typ.UnificationError (t_msg, _) ->
              Errors.raise_typ_error Typing "variable %s %a is of type %s but should be %a"
                (Pos.unmark var.Variable.name) Pos.format_position
                (Pos.get_position var.Variable.name)
                t_msg Format_mvg.format_typ t )
      with Not_found -> assert false (* should not happen *) )
  | LocalLet (local_var, e1, e2), t -> (
      let ctx, t1 = typecheck_bottom_up ctx e1 in
      let ctx =
        { ctx with ctx_local_var_typ = LocalVariableMap.add local_var t1 ctx.ctx_local_var_typ }
      in
      let ctx, t2 = typecheck_bottom_up ctx e2 in
      try
        Typ.coerce t2 (Typ.create_concrete t (Pos.get_position e, Typ.Down));
        ctx
      with Typ.UnificationError (t2_msg, _) ->
        Errors.raise_typ_error Typing "expression %a has type %s but should be %a"
          Pos.format_position (Pos.get_position e) t2_msg Format_mvg.format_typ t )
  | Error, _ -> ctx
  | LocalVar local_var, t -> (
      let t' =
        try LocalVariableMap.find local_var ctx.ctx_local_var_typ with Not_found -> assert false
        (* should not happen *)
      in
      try
        Typ.coerce t' (Typ.create_concrete t (Pos.get_position e, Typ.Down));
        ctx
      with Typ.UnificationError (t_msg, _) ->
        Errors.raise_typ_error Typing "expression %a has type %s but should be %a"
          Pos.format_position (Pos.get_position e) t_msg Format_mvg.format_typ t )
  | GenericTableIndex, Real ->
      if ctx.ctx_is_generic_table then ctx
      else
        Errors.raise_typ_error Variable "Generic table index appears outside of table %a"
          Pos.format_position (Pos.get_position e)
  | Index ((var, var_pos), e'), t -> (
      let ctx = typecheck_top_down ctx e' Real in
      let var_data = VariableMap.find var ctx.ctx_program.program_vars in
      match var_data.Mvg.var_definition with
      | SimpleVar _ | InputVar ->
          Errors.raise_typ_error Typing
            "variable %s is accessed %a as a table but it is not defined as one %a"
            (Pos.unmark var.Variable.name) Pos.format_position var_pos Pos.format_position
            (Pos.get_position var.Variable.name)
      | TableVar _ -> (
          try
            let var_typ = VariableMap.find var ctx.ctx_var_typ in
            try
              Typ.coerce var_typ (Typ.create_concrete t (Pos.get_position e, Typ.Down));
              ctx
            with Typ.UnificationError (t_msg, _) ->
              Errors.raise_typ_error Typing "expression %a has type %s but should be %a"
                Pos.format_position (Pos.get_position e) t_msg Format_mvg.format_typ t
          with Not_found ->
            {
              ctx with
              ctx_var_typ =
                VariableMap.add var
                  (Typ.create_concrete t (Pos.get_position e, Typ.Down))
                  ctx.ctx_var_typ;
            } ) )
  | _ ->
      Errors.raise_typ_error Typing "expression %a (%a) should be of type %a, but is of type %a"
        Format_mvg.format_expression (Pos.unmark e) Pos.format_position (Pos.get_position e)
        Format_mvg.format_typ t Typ.format_typ
        (snd @@ typecheck_bottom_up ctx e)

and typecheck_func_args (f : func) (pos : Pos.position) :
    ctx -> Mvg.expression Pos.marked list -> ctx * Typ.t =
  match f with
  | SumFunc | MinFunc | MaxFunc -> (
      fun ctx args ->
        if List.length args = 0 then
          Errors.raise_typ_error Typing "sum function must be called %a with at least one argument"
            Pos.format_position pos
        else
          let ctx, t1 = typecheck_bottom_up ctx (List.hd args) in
          try
            Typ.coerce t1 (Typ.real (Pos.get_position (List.hd args), Typ.Down));
            let ctx, t1 =
              List.fold_left
                (fun (ctx, t1) arg ->
                  let ctx, t_arg = typecheck_bottom_up ctx arg in
                  try
                    let t1 = Typ.unify t_arg t1 in
                    (ctx, t1)
                  with Typ.UnificationError (t_arg_msg, t2_msg) ->
                    Errors.raise_typ_error Typing
                      "function argument %a (%a) has type %s but should have type %s"
                      Format_mvg.format_expression (Pos.unmark arg) Pos.format_position
                      (Pos.get_position arg) t_arg_msg t2_msg)
                (ctx, t1) args
            in
            (ctx, t1)
          with Typ.UnificationError (t_arg_msg, t2_msg) ->
            Errors.raise_typ_error Typing
              "function argument %a (%a) has type %s but should have type %s"
              Format_mvg.format_expression
              (Pos.unmark (List.hd args))
              Pos.format_position
              (Pos.get_position (List.hd args))
              t_arg_msg t2_msg )
  | AbsFunc -> (
      fun ctx args ->
        match args with
        | [ arg ] -> (
            let ctx, t_arg = typecheck_bottom_up ctx arg in
            try
              Typ.coerce t_arg (Typ.real (Pos.get_position arg, Typ.Down));
              (ctx, t_arg)
            with Typ.UnificationError (t_arg_msg, t2_msg) ->
              Errors.raise_typ_error Typing
                "function argument %a (%a) has type %s but should have type %s"
                Format_mvg.format_expression (Pos.unmark arg) Pos.format_position
                (Pos.get_position arg) t_arg_msg t2_msg )
        | _ ->
            Errors.raise_typ_error Typing "function abs %a should have only one argument"
              Pos.format_position pos )
  | PresentFunc | NullFunc | GtzFunc | GtezFunc | Supzero -> (
      fun (* These functions return a integer value encoding a boolean; 0 for false and 1 for true *)
            ctx args ->
        match args with
        | [ arg ] -> (
            let ctx, t_arg = typecheck_bottom_up ctx arg in
            try
              Typ.coerce t_arg (Typ.real (Pos.get_position arg, Typ.Down));
              (ctx, t_arg)
            with Typ.UnificationError (t_arg_msg, t2_msg) ->
              Errors.raise_typ_error Typing
                "function argument %a (%a) has type %s but should have type %s"
                Format_mvg.format_expression (Pos.unmark arg) Pos.format_position
                (Pos.get_position arg) t_arg_msg t2_msg )
        | _ ->
            Errors.raise_typ_error Typing "function %a should have only one argument"
              Pos.format_position pos )
  | ArrFunc | InfFunc -> (
      fun ctx args ->
        match args with
        | [ arg ] -> (
            let ctx, t_arg = typecheck_bottom_up ctx arg in
            try
              Typ.coerce t_arg (Typ.real (Pos.get_position arg, Typ.Down));
              (ctx, Typ.real (pos, Typ.Down))
            with Typ.UnificationError (t_arg_msg, t2_msg) ->
              Errors.raise_typ_error Typing
                "function argument %a (%a) has type %s but should have type %s"
                Format_mvg.format_expression (Pos.unmark arg) Pos.format_position
                (Pos.get_position arg) t_arg_msg t2_msg )
        | _ ->
            Errors.raise_typ_error Typing "function %a should have only one argument"
              Pos.format_position pos )
  | Mvg.Multimax -> (
      fun ctx args ->
        match args with
        | [ bound; table ] ->
            let ctx = typecheck_top_down ctx bound Real in
            typecheck_bottom_up ctx table
        | _ ->
            Errors.raise_typ_error Typing "function %a should have two arguments"
              Pos.format_position pos )

and typecheck_bottom_up (ctx : ctx) (e : expression Pos.marked) : ctx * Typ.t =
  match Pos.unmark e with
  | Var var -> (
      try (ctx, VariableMap.find var ctx.ctx_var_typ)
      with Not_found ->
        let t = Typ.create_variable (Pos.get_position e, Typ.Up) in
        let ctx = { ctx with ctx_var_typ = VariableMap.add var t ctx.ctx_var_typ } in
        (ctx, t) )
  | Literal (Float _) -> (ctx, Typ.real (Pos.get_position e, Typ.Up))
  | Literal (Bool _) -> (ctx, Typ.boolean (Pos.get_position e, Typ.Up))
  | Literal Undefined -> (ctx, Typ.create_variable (Pos.get_position e, Typ.Up))
  | Binop ((Ast.And, _ | Ast.Or, _), e1, e2) ->
      let ctx = typecheck_top_down ctx e1 Boolean in
      let ctx = typecheck_top_down ctx e2 Boolean in
      (ctx, Typ.boolean (Pos.get_position e, Typ.Up))
  | Binop (((Ast.Add, _ | Ast.Sub, _ | Ast.Mul, _ | Ast.Div, _) as op), e1, e2) -> (
      let ctx, t1 = typecheck_bottom_up ctx e1 in
      let ctx, t2 = typecheck_bottom_up ctx e2 in
      try
        let t' = Typ.unify t1 t2 in
        (ctx, t')
      with Typ.UnificationError (t1_msg, t2_msg) ->
        Errors.raise_typ_error Typing
          "arguments of operator (%a) %a have to be of the same type but instead are of type %s \
           and %s"
          Format_ast.format_binop (Pos.unmark op) Pos.format_position (Pos.get_position op) t1_msg
          t2_msg )
  | Comparison (_, e1, e2) -> (
      let ctx, t1 = typecheck_bottom_up ctx e1 in
      let ctx, t2 = typecheck_bottom_up ctx e2 in
      try
        ignore (Typ.unify t1 t2);
        (ctx, Typ.boolean (Pos.get_position e, Typ.Up))
      with Typ.UnificationError _ ->
        Errors.raise_typ_error Typing
          "expression %a (%a) of type %a has not the same type than expression %a of type %a"
          Format_mvg.format_expression (Pos.unmark e1) Pos.format_position (Pos.get_position e1)
          Typ.format_typ t1 Pos.format_position (Pos.get_position e2) Typ.format_typ t2 )
  | Unop (Ast.Not, e) ->
      let ctx = typecheck_top_down ctx e Boolean in
      (ctx, Typ.boolean (Pos.get_position e, Typ.Up))
  | Unop (Ast.Minus, e') -> (
      let ctx, t = typecheck_bottom_up ctx e' in
      try
        Typ.coerce t (Typ.real (Pos.get_position e', Typ.Down));
        (ctx, t)
      with Typ.UnificationError (t1_msg, t2_msg) ->
        Errors.raise_typ_error Typing
          "arguments of operator (%a) %a has type %s but should be of type %s"
          Format_ast.format_unop Ast.Minus Pos.format_position (Pos.get_position e) t1_msg t2_msg )
  | GenericTableIndex ->
      if ctx.ctx_is_generic_table then (ctx, Typ.real (Pos.get_position e, Typ.Up))
      else
        Errors.raise_typ_error Variable "Generic table index appears outside of table %a"
          Pos.format_position (Pos.get_position e)
  | Index ((var, var_pos), e') -> (
      let ctx = typecheck_top_down ctx e' Real in
      let var_data = VariableMap.find var ctx.ctx_program.program_vars in
      match var_data.Mvg.var_definition with
      | SimpleVar _ | InputVar ->
          Errors.raise_typ_error Typing
            "variable %s is accessed %a as a table but it is not defined as one %a"
            (Pos.unmark var.Variable.name) Pos.format_position var_pos Pos.format_position
            (Pos.get_position var.Variable.name)
      | TableVar _ -> (
          try (ctx, VariableMap.find var ctx.ctx_var_typ)
          with Not_found ->
            let t = Typ.create_variable (Pos.get_position e', Typ.Up) in
            let ctx = { ctx with ctx_var_typ = VariableMap.add var t ctx.ctx_var_typ } in
            (ctx, t) ) )
  | Conditional (e1, e2, e3) -> (
      let ctx, t1 = typecheck_bottom_up ctx e1 in
      try
        Typ.coerce t1 (Typ.boolean (Pos.get_position e1, Typ.Down));
        let ctx, t2 = typecheck_bottom_up ctx e2 in
        let ctx, t3 = typecheck_bottom_up ctx e3 in
        try
          let t' = Typ.unify t2 t3 in
          (ctx, t')
        with Typ.UnificationError _ ->
          Errors.raise_typ_error Typing
            "expression %a (%a) of type %a has not the same type than expression %a of type %a"
            Format_mvg.format_expression (Pos.unmark e1) Pos.format_position (Pos.get_position e1)
            Typ.format_typ t1 Pos.format_position (Pos.get_position e2) Typ.format_typ t2
      with Typ.UnificationError _ ->
        Errors.raise_typ_error Typing "expression %a (%a) of type %a should be of type boolean"
          Format_mvg.format_expression (Pos.unmark e1) Pos.format_position (Pos.get_position e1)
          Typ.format_typ t1 )
  | FunctionCall (func, args) ->
      let typechecker = typecheck_func_args func (Pos.get_position e) in
      let ctx, t' = typechecker ctx args in
      (ctx, t')
  | Error -> (ctx, Typ.create_variable (Pos.get_position e, Typ.Up))
  | LocalVar local_var -> (
      try (ctx, LocalVariableMap.find local_var ctx.ctx_local_var_typ)
      with Not_found -> assert false (* should not happen *) )
  | LocalLet (local_var, e1, e2) ->
      let ctx, t1 = typecheck_bottom_up ctx e1 in
      let ctx =
        { ctx with ctx_local_var_typ = LocalVariableMap.add local_var t1 ctx.ctx_local_var_typ }
      in
      let ctx, t2 = typecheck_bottom_up ctx e2 in
      (ctx, t2)

let determine_def_complete_cover (table_var : Mvg.Variable.t) (size : int)
    (defs : (int * Pos.position) list) : int list =
  (* Return all undefined indexes *)
  let defs_array = Array.make size false in
  List.iter
    (fun (def, def_pos) ->
      try defs_array.(def) <- true
      with Invalid_argument _ ->
        Errors.raise_typ_error Variable
          "the definitions of index %d %a, from table %s of size %d declared %a is out of bounds"
          def Pos.format_position def_pos
          (Pos.unmark table_var.Variable.name)
          size Pos.format_position
          (Pos.get_position table_var.Variable.name))
    defs;
  let undefined = ref [] in
  Array.iteri (fun index defined -> if not defined then undefined := index :: !undefined) defs_array;
  List.sort compare !undefined

let typecheck_program_conds (ctx : ctx) (conds : condition_data VariableMap.t) : ctx =
  VariableMap.fold (fun _ cond ctx -> typecheck_top_down ctx cond.cond_expr Mvg.Boolean) conds ctx

let rec check_non_recursivity_expr (e : expression Pos.marked) (lvar : Variable.t) : unit =
  match Pos.unmark e with
  | Comparison (_, e1, e2) | Binop (_, e1, e2) | LocalLet (_, e1, e2) ->
      check_non_recursivity_expr e1 lvar;
      check_non_recursivity_expr e2 lvar
  | Unop (_, e1) -> check_non_recursivity_expr e1 lvar
  | Index (_, e1) ->
      (* We don't check for recursivity in indexes because tables can refer to themselves in
         definition. It is only at runtime that we return [Undefined] for index definitions that
         refer to indexes of the same table greater than themselves. *)
      check_non_recursivity_expr e1 lvar
  | Conditional (e1, e2, e3) ->
      check_non_recursivity_expr e1 lvar;
      check_non_recursivity_expr e2 lvar;
      check_non_recursivity_expr e3 lvar
  | FunctionCall (_, args) -> List.iter (fun arg -> check_non_recursivity_expr arg lvar) args
  | Literal _ | LocalVar _ | GenericTableIndex | Error -> ()
  | Var var ->
      if var = lvar then
        Errors.raise_typ_error Variable
          "You cannot refer to the variable %s since you are defining it (%a)"
          (Pos.unmark var.Variable.name) Pos.format_position (Pos.get_position e)
      else ()

let check_non_recursivity_of_variable_defs (var : Variable.t) (def : variable_def) : unit =
  match def with SimpleVar e -> check_non_recursivity_expr e var | TableVar _ | InputVar -> ()

(* The typechecker returns a new program because it defines missing table entries as "undefined" *)
let typecheck (p : program) : typ_info * program =
  let are_tables, ctx, p_vars =
    Mvg.VariableMap.fold
      (fun var def (acc, ctx, p_vars) ->
        check_non_recursivity_of_variable_defs var def.var_definition;
        match def.var_typ with
        | Some t -> (
            match def.var_definition with
            | SimpleVar e ->
                let new_ctx = typecheck_top_down { ctx with ctx_is_generic_table = false } e t in
                (VariableMap.add var false acc, new_ctx, p_vars)
            | TableVar (size, defs) -> (
                match defs with
                | IndexGeneric e ->
                    let new_ctx = typecheck_top_down { ctx with ctx_is_generic_table = true } e t in
                    (VariableMap.add var true acc, new_ctx, p_vars)
                | IndexTable es ->
                    let new_ctx =
                      IndexMap.fold
                        (fun _ e ctx ->
                          let new_ctx =
                            typecheck_top_down { ctx with ctx_is_generic_table = false } e t
                          in
                          new_ctx)
                        es ctx
                    in
                    let undefined_indexes =
                      determine_def_complete_cover var size
                        (List.map (fun (x, e) -> (x, Pos.get_position e)) (IndexMap.bindings es))
                    in
                    if List.length undefined_indexes = 0 then
                      (VariableMap.add var true acc, new_ctx, p_vars)
                    else
                      let previous_var_def =
                        Ast_to_mvg.get_var_from_name p.program_idmap var.Variable.name
                          var.Variable.execution_number false
                      in
                      let new_es =
                        List.fold_left
                          (fun es undef_index ->
                            Mvg.IndexMap.add undef_index
                              (Pos.same_pos_as
                                 (Mvg.Index
                                    ( Pos.same_pos_as previous_var_def var.Mvg.Variable.name,
                                      Pos.same_pos_as
                                        (Mvg.Literal (Float (float_of_int undef_index)))
                                        var.Mvg.Variable.name ))
                                 var.Mvg.Variable.name)
                              es)
                          es undefined_indexes
                      in
                      ( VariableMap.add var true acc,
                        new_ctx,
                        Mvg.VariableMap.add var
                          {
                            def with
                            Mvg.var_definition = Mvg.TableVar (size, Mvg.IndexTable new_es);
                          }
                          p_vars ) )
            | InputVar -> (VariableMap.add var false acc, ctx, p_vars) )
        | None -> (
            match def.var_definition with
            | SimpleVar e ->
                let new_ctx, t = typecheck_bottom_up { ctx with ctx_is_generic_table = false } e in
                let t =
                  try Typ.unify t (Mvg.VariableMap.find var ctx.ctx_var_typ) with
                  | Not_found -> t
                  | Typ.UnificationError (t1_msg, t2_msg) ->
                      Errors.raise_typ_error Typing
                        "variable %s declared %a should have type %s but is found to have type %s"
                        (Pos.unmark var.Variable.name) Pos.format_position
                        (Pos.get_position var.Variable.name)
                        t2_msg t1_msg
                in
                let new_ctx =
                  { new_ctx with ctx_var_typ = Mvg.VariableMap.add var t new_ctx.ctx_var_typ }
                in
                (VariableMap.add var false acc, new_ctx, p_vars)
            | TableVar (size, defs) -> (
                match defs with
                | IndexGeneric e ->
                    let new_ctx, t =
                      typecheck_bottom_up { ctx with ctx_is_generic_table = true } e
                    in
                    let t =
                      try Typ.unify t (Mvg.VariableMap.find var ctx.ctx_var_typ) with
                      | Not_found -> t
                      | Typ.UnificationError (t1_msg, t2_msg) ->
                          Errors.raise_typ_error Typing
                            "table variable %s declared %a should have type %s but is found to \
                             have type %s"
                            (Pos.unmark var.Variable.name) Pos.format_position
                            (Pos.get_position var.Variable.name)
                            t2_msg t1_msg
                    in
                    let new_ctx =
                      { new_ctx with ctx_var_typ = Mvg.VariableMap.add var t new_ctx.ctx_var_typ }
                    in
                    (VariableMap.add var true acc, new_ctx, p_vars)
                | IndexTable es ->
                    let new_ctx, t =
                      IndexMap.fold
                        (fun _ e (ctx, old_t) ->
                          let new_ctx, t =
                            typecheck_bottom_up { ctx with ctx_is_generic_table = false } e
                          in
                          try
                            let t = Typ.unify t old_t in
                            let new_ctx =
                              {
                                new_ctx with
                                ctx_var_typ = Mvg.VariableMap.add var t new_ctx.ctx_var_typ;
                              }
                            in
                            (new_ctx, t)
                          with Typ.UnificationError (t1_msg, t2_msg) ->
                            Errors.raise_typ_error Typing
                              "different definitions of specific index of table variable %s \
                               declared %a have different types: %s and %s"
                              (Pos.unmark var.Variable.name) Pos.format_position
                              (Pos.get_position var.Variable.name)
                              t1_msg t2_msg)
                        es
                        (ctx, Typ.create_variable (Pos.get_position var.Variable.name, Typ.Up))
                    in
                    let t =
                      try Typ.unify t (Mvg.VariableMap.find var ctx.ctx_var_typ) with
                      | Not_found -> t
                      | Typ.UnificationError (t1_msg, t2_msg) ->
                          Errors.raise_typ_error Typing
                            "table variable %s declared %a with type %s is defined with type %s"
                            (Pos.unmark var.Variable.name) Pos.format_position
                            (Pos.get_position var.Variable.name)
                            t2_msg t1_msg
                    in
                    let new_ctx =
                      { new_ctx with ctx_var_typ = Mvg.VariableMap.add var t new_ctx.ctx_var_typ }
                    in
                    let undefined_indexes =
                      determine_def_complete_cover var size
                        (List.map (fun (x, e) -> (x, Pos.get_position e)) (IndexMap.bindings es))
                    in
                    if List.length undefined_indexes = 0 then
                      (VariableMap.add var true acc, new_ctx, p_vars)
                    else
                      let previous_var_def =
                        Ast_to_mvg.get_var_from_name p.program_idmap var.Variable.name
                          var.Variable.execution_number false
                      in
                      let new_es =
                        List.fold_left
                          (fun es undef_index ->
                            Mvg.IndexMap.add undef_index
                              (Pos.same_pos_as
                                 (Mvg.Index
                                    ( Pos.same_pos_as previous_var_def var.Mvg.Variable.name,
                                      Pos.same_pos_as
                                        (Mvg.Literal (Float (float_of_int undef_index)))
                                        var.Mvg.Variable.name ))
                                 var.Mvg.Variable.name)
                              es)
                          es undefined_indexes
                      in
                      ( VariableMap.add var true acc,
                        new_ctx,
                        Mvg.VariableMap.add var
                          {
                            def with
                            Mvg.var_definition = Mvg.TableVar (size, Mvg.IndexTable new_es);
                          }
                          p_vars ) )
            | InputVar ->
                if VariableMap.mem var acc then (acc, ctx, p_vars)
                else (VariableMap.add var false acc, ctx, p_vars) ))
      p.program_vars
      ( Mvg.VariableMap.empty,
        {
          ctx_program = p;
          ctx_var_typ =
            VariableMap.merge
              (fun var _ def ->
                match def with
                | None -> assert false (* should not happen *)
                | Some def -> (
                    match def.var_typ with
                    | Some t ->
                        Some (Typ.create_concrete t (Pos.get_position var.Variable.name, Typ.Down))
                    | None -> None ))
              VariableMap.empty p.program_vars;
          ctx_local_var_typ = LocalVariableMap.empty;
          ctx_is_generic_table = false;
        },
        p.program_vars )
  in
  let ctx = typecheck_program_conds ctx p.program_conds in
  ( {
      typ_info_var =
        VariableMap.merge
          (fun _ t is_table ->
            match (t, is_table) with
            | Some t, Some is_table -> Some (Typ.to_concrete t, is_table)
            | None, Some _ ->
                (* This case is needed because of declared but undefined and unused variables *)
                None
            | Some t, None ->
                (* In this case, the variable is declared without type, used but never defined : we
                   say it's not a table *)
                Some (Typ.to_concrete t, false)
            | _ -> assert false
            (* should not happen *))
          ctx.ctx_var_typ are_tables;
      typ_info_local_var = LocalVariableMap.map (fun t -> Typ.to_concrete t) ctx.ctx_local_var_typ;
    },
    { p with program_vars = p_vars } )
