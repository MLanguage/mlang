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

open Bir

type offset =
  | GetValueConst of int
  | GetValueVar of variable
  | PassPointer
  | None

let rec generate_variable (vm : Dgfip_varid.var_id_map) (offset : offset)
    ?(def_flag = false) ?(debug_flag = false) (var : Bir.variable) : string =
  let mvar = Bir.var_to_mir var in
  try
    match offset with
    | PassPointer ->
        if def_flag then Dgfip_varid.gen_access_def_pointer vm mvar
        else Dgfip_varid.gen_access_pointer vm mvar
    | _ ->
        let offset =
          match offset with
          | None -> ""
          | GetValueVar offset -> " + (int)" ^ generate_variable vm None offset
          | GetValueConst offset -> " + " ^ string_of_int offset
          | PassPointer -> assert false
        in
        if def_flag then Dgfip_varid.gen_access_def vm mvar offset
        else
          let access_val = Dgfip_varid.gen_access_val vm mvar offset in
          if debug_flag then
            let vn = Pos.unmark mvar.Mir.Variable.name in
            let pos_tgv = Dgfip_varid.gen_access_pos_from_start vm mvar in
            Format.asprintf "(aff3(\"%s\",irdata, %s), %s)" vn pos_tgv
              access_val
          else access_val
  with Not_found ->
    Errors.raise_error
      (Format.asprintf "Variable %s not found in TGV"
         (Pos.unmark mvar.Mir.Variable.name))

module DecoupledExpr : sig
  type dflag = Def | Val

  type local_var

  type t

  type constr

  type expression_composition = { def_test : constr; value_comp : constr }

  val is_always_true : t -> bool

  val locals_from_m : Mir.LocalVariable.t -> local_var * local_var

  val new_local : unit -> local_var

  val dtrue : constr

  val dfalse : constr

  val lit : float -> constr

  val m_var : variable -> offset -> dflag -> constr

  val let_local : local_var -> constr -> constr -> constr

  val local_var : local_var -> constr

  val dand : constr -> constr -> constr

  val dor : constr -> constr -> constr

  val dnot : constr -> constr

  val minus : constr -> constr

  val binop : string -> constr -> constr -> constr

  val dfun : string -> constr list -> constr

  val access : variable -> dflag -> constr -> constr

  val ite : constr -> constr -> constr -> constr

  val build_transitive_composition :
    ?safe_def:bool -> expression_composition -> expression_composition

  val build_expression : expression_composition -> t * t

  (* val format_local_var : Format.formatter -> local_var * dflag -> unit *)

  val format_assign :
    Dgfip_options.flags ->
    Dgfip_varid.var_id_map ->
    string ->
    Format.formatter ->
    t ->
    unit
end = struct
  type local_var = Anon | Refered of int

  type dflag = Def | Val

  type stack_slot = { kind : dflag; depth : int }

  type stack_assignment = { slot : stack_slot; subexpr : expr }

  and local_stacks = {
    def_top : int;
    val_top : int;
    var_substs : (int * (expr * dflag)) list;
  }

  and local_vars = (local_var * stack_assignment) list

  and expr =
    | Dtrue
    | Dfalse
    | Dlit of float
    | Dvar of expr_var
    | Dand of expr * expr
    | Dor of expr * expr
    | Dunop of string * expr
    | Dbinop of string * expr * expr
    | Dfun of string * expr list
    | Daccess of variable * dflag * expr
    | Dite of expr * expr * expr

  and expr_var = Local of stack_slot | M of variable * offset * dflag

  and t = expr * dflag * local_vars

  and constr = local_stacks -> local_vars -> t

  type expression_composition = { def_test : constr; value_comp : constr }

  type stack_position = Not_to_stack | Must_be_pushed | On_top of dflag

  let is_always_true ((expr, _kind, _lv) : t) = expr = Dtrue

  let cast (kind : dflag) (expr : expr) =
    match (expr, kind) with
    | Dtrue, Val -> Dlit 1.
    | Dfalse, Val -> Dlit 0.
    | Dlit 0., Def -> Dfalse
    | Dlit _, Def -> Dtrue
    | _, Def -> Dbinop ("!=", expr, Dlit 0.)
    | _, Val -> expr

  (** local stacks operations *)

  let bump_stack (kind : dflag) (st : local_stacks) =
    match kind with
    | Def -> { st with def_top = st.def_top + 1 }
    | Val -> { st with val_top = st.val_top + 1 }

  let add_substitution (st : local_stacks) (v : local_var) (kind : dflag)
      (expr : expr) =
    match v with
    | Anon -> st
    | Refered v -> { st with var_substs = (v, (expr, kind)) :: st.var_substs }

  let stack_top (kind : dflag) (st : local_stacks) =
    match kind with Def -> st.def_top | Val -> st.val_top

  let is_in_stack_scope ({ kind; depth } : stack_slot) (st : local_stacks) =
    match kind with Def -> depth < st.def_top | Val -> depth < st.val_top

  let is_on_top ({ kind; depth } : stack_slot) (st : local_stacks) =
    match kind with Def -> depth = st.def_top | Val -> depth = st.val_top

  let expr_position (expr : expr) (st : local_stacks) =
    match expr with
    | Dtrue | Dfalse | Dlit _ | Dvar (M _) -> Not_to_stack
    | Dvar (Local slot) ->
        if is_in_stack_scope slot st then Not_to_stack
        else if is_on_top slot st then On_top slot.kind
        else Must_be_pushed
    | _ -> Must_be_pushed

  let store_local (st : local_stacks) (ctx : local_vars) (v : local_var)
      (kind : dflag) (subexpr : expr) =
    match expr_position subexpr st with
    | Not_to_stack -> (add_substitution st v kind subexpr, ctx, subexpr)
    | On_top kind ->
        (add_substitution st v kind subexpr |> bump_stack kind, ctx, subexpr)
    | Must_be_pushed ->
        let depth = stack_top kind st in
        let st = bump_stack kind st in
        let slot = { kind; depth } in
        let assignment = { slot; subexpr } in
        let ctx = (v, assignment) :: ctx in
        (st, ctx, Dvar (Local slot))

  let collapse_constr (st : local_stacks) (ctx : local_vars) (constr : constr) =
    let expr, kind, lv = constr st ctx in
    (expr, kind, lv @ ctx)

  let push_as (st : local_stacks) (ctx : local_vars) (kind : dflag)
      (constr : constr) =
    let expr, ekind, lv = constr st ctx in
    let expr = if kind = ekind then expr else cast kind expr in
    store_local st lv Anon kind expr

  (** smart constructors *)

  let locals_from_m (lvar : Mir.local_variable) =
    ( Refered (-(2 * lvar.Mir.LocalVariable.id)),
      Refered (-((2 * lvar.Mir.LocalVariable.id) + 1)) )

  let new_local : unit -> local_var =
    let c = ref 0 in
    fun () ->
      let i = !c in
      incr c;
      Refered i

  let let_local (v : local_var) (bound : constr) (body : constr)
      (st : local_stacks) (ctx : local_vars) =
    let bound, kind, lv = collapse_constr st ctx bound in
    let st, ctx, _ = store_local st lv v kind bound in
    collapse_constr st ctx body

  let dtrue _st _lv : t = (Dtrue, Def, [])

  let dfalse _st _lv : t = (Dfalse, Def, [])

  let lit (f : float) _st _lv : t = (Dlit f, Val, [])

  let m_var (v : variable) (offset : offset) (df : dflag) _st _lv : t =
    (Dvar (M (v, offset, df)), df, [])

  let local_var (lvar : local_var) (st : local_stacks) (ctx : local_vars) : t =
    match lvar with
    | Anon -> Errors.raise_error "Tried to access anonymous local variable"
    | Refered v -> (
        match List.assoc_opt v st.var_substs with
        | Some (e, kind) -> (e, kind, [])
        | None -> (
            match List.assoc_opt lvar ctx with
            | Some { slot; _ } -> (Dvar (Local slot), slot.kind, [])
            | None -> Errors.raise_error "Local variable not found in context"))

  let dand (e1 : constr) (e2 : constr) (st : local_stacks) (ctx : local_vars) :
      t =
    let st', lv1, e1 = push_as st ctx Def e1 in
    let _, lv2, e2 = push_as st' ctx Def e2 in
    match (e1, e2) with
    | Dtrue, _ -> (e2, Def, lv2)
    | _, Dtrue -> (e1, Def, lv1)
    | Dfalse, _ | _, Dfalse -> (Dfalse, Def, [])
    | _ -> (Dand (e1, e2), Def, lv2 @ lv1)

  let dor (e1 : constr) (e2 : constr) (st : local_stacks) (ctx : local_vars) : t
      =
    let st', lv1, e1 = push_as st ctx Def e1 in
    let _, lv2, e2 = push_as st' ctx Def e2 in
    match (e1, e2) with
    | Dtrue, _ | _, Dtrue -> (Dtrue, Def, [])
    | Dfalse, _ -> (e2, Def, lv2)
    | _, Dfalse -> (e1, Def, lv1)
    | _ -> (Dor (e1, e2), Def, lv2 @ lv1)

  let dnot (e : constr) (st : local_stacks) (ctx : local_vars) : t =
    let _, lv, e = push_as st ctx Def e in
    match e with
    | Dtrue -> (Dfalse, Def, [])
    | Dfalse -> (Dtrue, Def, [])
    | Dunop ("!", e) -> (e, Def, lv)
    | _ -> (Dunop ("!", e), Def, lv)

  let minus (e : constr) (st : local_stacks) (ctx : local_vars) : t =
    let _, lv, e = push_as st ctx Val e in
    match e with
    | Dlit f -> (Dlit (-.f), Val, [])
    | Dunop ("-", e) -> (e, Val, lv)
    | _ -> (Dunop ("-", e), Val, lv)

  (* TODO optimize neutral ops *)
  let binop op (e1 : constr) (e2 : constr) (st : local_stacks)
      (ctx : local_vars) : t =
    let st', lv1, e1 = push_as st ctx Val e1 in
    let _, lv2, e2 = push_as st' ctx Val e2 in
    let arith o =
      match (e1, e2) with
      | Dlit f1, Dlit f2 -> (Dlit (o f1 f2), Val)
      | _ -> (Dbinop (op, e1, e2), Val)
    in
    let comp o =
      match (e1, e2) with
      | Dlit f1, Dlit f2 -> ((if o f1 f2 then Dtrue else Dfalse), Def)
      | _ -> (Dbinop (op, e1, e2), Def)
    in
    let e, kind =
      match op with
      | "+" -> arith ( +. )
      | "-" -> arith ( -. )
      | "*" -> arith ( *. )
      | "/" -> arith (fun f1 f2 -> if f2 = 0. then 0. else f1 /. f2)
      | "==" -> comp ( = )
      | "!=" -> comp ( <> )
      | "<=" -> comp ( <= )
      | "<" -> comp ( < )
      | ">=" -> comp ( >= )
      | ">" -> comp ( > )
      | _ -> assert false
    in
    (e, kind, lv2 @ lv1)

  let dfun (f : string) (args : constr list) (st : local_stacks)
      (ctx : local_vars) : t =
    let (_, lv), args =
      List.fold_left_map
        (fun (st, lv) e ->
          let st, lv', e = push_as st ctx Val e in
          ((st, lv' @ lv), e))
        (st, []) args
    in
    (* TODO : distinguish kinds *)
    (Dfun (f, args), Val, lv)

  let access (var : variable) (df : dflag) (e : constr) (st : local_stacks)
      (ctx : local_vars) : t =
    let _, lv, e = push_as st ctx Val e in
    (Daccess (var, df, e), df, lv)

  let ite (c : constr) (t : constr) (e : constr) (st : local_stacks)
      (ctx : local_vars) : t =
    let st', lvc, c = push_as st ctx Def c in
    let st', lvt, t = push_as st' ctx Val t in
    let _, lve, e = push_as st' ctx Val e in
    match (c, t, e) with
    | Dtrue, _, _ -> (t, Val, lvt)
    | Dfalse, _, _ -> (e, Val, lve)
    | _, Dlit 1., Dlit 0. -> (c, Val, lvc)
    | _, Dlit f, Dlit f' when f = f' -> (Dlit f, Val, [])
    | _ -> (Dite (c, t, e), Val, lve @ lvt @ lvc)

  let build_transitive_composition ?(safe_def = false)
      ({ def_test; value_comp } : expression_composition) =
    (* `safe_def` can be set on call when we are sure that `value_comp` will
       always happen to be zero when `def_test` ends up false. E.g. arithmetic
       operation have such semantic property (funny question is what's the
       causality ?). This allows to remove a check to the definition flag when
       we compute the value, avoiding a lot of unnecessary code. *)
    let value_comp =
      if safe_def then value_comp else ite def_test value_comp (lit 0.)
    in
    { def_test; value_comp }

  let build_expression (expr_comp : expression_composition) =
    let empty_stacks = { def_top = 0; val_top = 0; var_substs = [] } in
    let empty_locals = [] in
    ( collapse_constr empty_stacks empty_locals expr_comp.def_test,
      collapse_constr empty_stacks empty_locals expr_comp.value_comp )

  let format_slot fmt ({ kind; depth } : stack_slot) =
    let kind = match kind with Def -> "int" | Val -> "real" in
    Format.fprintf fmt "%s%d" kind depth

  let format_expr_var (dgfip_flags : Dgfip_options.flags)
      (vm : Dgfip_varid.var_id_map) fmt (ev : expr_var) =
    match ev with
    | Local slot -> format_slot fmt slot
    | M (var, offset, df) ->
        let def_flag = df = Def in
        Format.fprintf fmt "%s"
          (generate_variable ~debug_flag:dgfip_flags.flg_trace vm offset
             ~def_flag var)

  let rec format_dexpr (dgfip_flags : Dgfip_options.flags)
      (vm : Dgfip_varid.var_id_map) fmt (de : expr) =
    let format_dexpr = format_dexpr dgfip_flags vm in
    match de with
    | Dtrue -> Format.fprintf fmt "1"
    | Dfalse -> Format.fprintf fmt "0"
    | Dlit f -> (
        match Float.modf f with
        | 0., _ ->
            (* Print at least one decimal, distinction from integers *)
            Format.fprintf fmt "%.1f" f
        | _ ->
            (* Print literal floats as precisely as possible *)
            Format.fprintf fmt "%#.19g" f)
    | Dvar evar -> format_expr_var dgfip_flags vm fmt evar
    | Dand (de1, de2) ->
        Format.fprintf fmt "@[<hov 2>(%a@ && %a@])" format_dexpr de1
          format_dexpr de2
    | Dor (de1, de2) ->
        Format.fprintf fmt "@[<hov 2>(%a@ || %a@])" format_dexpr de1
          format_dexpr de2
    | Dunop (op, de) ->
        Format.fprintf fmt "@[<hov 2>(%s%a@])" op format_dexpr de
    | Dbinop (op, de1, de2) ->
        Format.fprintf fmt "@[<hov 2>(%a@ %s %a@])" format_dexpr de1 op
          format_dexpr de2
    | Dfun (funname, des) ->
        Format.fprintf fmt "@[<hov 2>%s(%a@])" funname
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
             format_dexpr)
          des
    | Daccess (var, dflag, de) ->
        Format.fprintf fmt "(%s[(int)%a])"
          (generate_variable ~def_flag:(dflag = Def)
             ~debug_flag:dgfip_flags.flg_trace vm PassPointer var)
          format_dexpr de
    | Dite (dec, det, dee) ->
        Format.fprintf fmt "@[<hov 2>(%a ?@ %a@ : %a@])" format_dexpr dec
          format_dexpr det format_dexpr dee

  let format_local_vars_defs (dgfip_flags : Dgfip_options.flags)
      (vm : Dgfip_varid.var_id_map) fmt (lv : local_vars) =
    let lv = List.rev lv in
    let _ =
      List.fold_left
        (fun seen (_, { slot; _ }) ->
          if List.mem slot seen then seen
          else begin
            Format.fprintf fmt "@[<hov 2>%s %a;@]@,"
              (match slot.kind with Def -> "int" | Val -> "double")
              format_slot slot;
            slot :: seen
          end)
        [] lv
    in
    let format_one_assign fmt (_, { slot; subexpr }) =
      Format.fprintf fmt "@[<hov 2>%a =@ %a;@]@," format_slot slot
        (format_dexpr dgfip_flags vm)
        subexpr
    in
    List.iter (format_one_assign fmt) lv

  let format_assign (dgfip_flags : Dgfip_options.flags)
      (var_indexes : Dgfip_varid.var_id_map) (var : string) fmt
      ((e, _kind, lv) : t) =
    Format.fprintf fmt "%a@[<hov 2>%s =@ %a;@]"
      (format_local_vars_defs dgfip_flags var_indexes)
      lv var
      (format_dexpr dgfip_flags var_indexes)
      e
end

module D = DecoupledExpr

let rec generate_c_expr (e : expression Pos.marked)
    (var_indexes : Dgfip_varid.var_id_map) : D.expression_composition =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let safe_def =
        match (Pos.unmark op, Pos.unmark e2) with
        | Mast.Gt, Mir.(Literal (Undefined | Float 0.)) ->
            (* hack to catch positive test in M *) true
        | _ -> false
      in
      let def_test = D.dand se1.def_test se2.def_test in
      let value_comp =
        let op =
          match Pos.unmark op with
          | Mast.Gt -> ">"
          | Mast.Gte -> ">="
          | Mast.Lt -> "<"
          | Mast.Lte -> "<="
          | Mast.Eq -> "=="
          | Mast.Neq -> "!="
        in
        D.binop op se1.value_comp se2.value_comp
      in
      D.build_transitive_composition ~safe_def { def_test; value_comp }
  | Binop ((Mast.Div, _), e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = D.dand se1.def_test se2.def_test in
      let value_comp =
        D.ite se2.value_comp
          (D.binop "/" se1.value_comp se2.value_comp)
          (D.lit 0.)
      in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | Binop (op, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test =
        match Pos.unmark op with
        | Mast.And | Mast.Mul -> D.dand se1.def_test se2.def_test
        | Mast.Or | Mast.Add | Mast.Sub -> D.dor se1.def_test se2.def_test
        | Mast.Div -> assert false
        (* see above *)
      in
      let op e1 e2 =
        match Pos.unmark op with
        | Mast.And -> D.dand e1 e2
        | Mast.Or -> D.dor e1 e2
        | Mast.Add -> D.binop "+" e1 e2
        | Mast.Sub -> D.binop "-" e1 e2
        | Mast.Mul -> D.binop "*" e1 e2
        | Mast.Div -> assert false
        (* see above *)
      in
      let value_comp = op se1.value_comp se2.value_comp in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | Unop (op, e) ->
      let se = generate_c_expr e var_indexes in
      let def_test = se.def_test in
      let op, safe_def =
        match op with
        | Mast.Not -> (D.dnot, false)
        | Mast.Minus -> (D.minus, true)
      in
      let value_comp = op se.value_comp in
      D.build_transitive_composition ~safe_def { def_test; value_comp }
  | Index (var, e) ->
      let idx = generate_c_expr e var_indexes in
      let size =
        Option.get (Bir.var_to_mir (Pos.unmark var)).Mir.Variable.is_table
      in
      let idx_var = D.new_local () in
      let def_test =
        D.let_local idx_var idx.value_comp
          (D.dand
             (D.dand idx.def_test
                (D.binop "<" (D.local_var idx_var) (D.lit (float_of_int size))))
             (D.access (Pos.unmark var) Def (D.local_var idx_var)))
      in
      let value_comp =
        D.let_local idx_var idx.value_comp
          (D.ite
             (D.binop "<" (D.local_var idx_var) (D.lit 0.))
             (D.lit 0.)
             (D.access (Pos.unmark var) Val (D.local_var idx_var)))
      in
      D.build_transitive_composition { def_test; value_comp }
  | Conditional (c, t, f) ->
      let cond = generate_c_expr c var_indexes in
      let thenval = generate_c_expr t var_indexes in
      let elseval = generate_c_expr f var_indexes in
      let def_test =
        D.dand cond.def_test
          (D.ite cond.value_comp thenval.def_test elseval.def_test)
      in
      let value_comp =
        D.ite cond.value_comp thenval.value_comp elseval.value_comp
      in
      D.build_transitive_composition { def_test; value_comp }
  | FunctionCall (PresentFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = D.dtrue in
      let value_comp = se.def_test in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (NullFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp =
        D.dand def_test (D.binop "==" se.value_comp (D.lit 0.))
      in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (ArrFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dfun "my_arr" [ se.value_comp ] in
      (* Here we boldly assume that rounding value of `undef` will give zero,
         given the invariant. Pretty sure that not true, in case of doubt, turn
         `safe_def` to false *)
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (InfFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dfun "my_floor" [ se.value_comp ] in
      (* same as above *)
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (AbsFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dfun "fabs" [ se.value_comp ] in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = D.dor se1.def_test se2.def_test in
      let value_comp = D.dfun "max" [ se1.value_comp; se2.value_comp ] in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = D.dor se1.def_test se2.def_test in
      let value_comp = D.dfun "min" [ se1.value_comp; se2.value_comp ] in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (Multimax, [ e1; (Var v2, _) ]) ->
      let bound = generate_c_expr e1 var_indexes in
      let def_test =
        D.dfun "multimax_def" [ bound.value_comp; D.m_var v2 PassPointer Def ]
      in
      let value_comp =
        D.dfun "multimax" [ bound.value_comp; D.m_var v2 PassPointer Val ]
      in
      D.build_transitive_composition { def_test; value_comp }
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> { def_test = D.dtrue; value_comp = D.lit f }
  | Literal Undefined -> { def_test = D.dfalse; value_comp = D.lit 0. }
  | Var var ->
      { def_test = D.m_var var None Def; value_comp = D.m_var var None Val }
  | LocalVar lvar ->
      let ldef, lval = D.locals_from_m lvar in
      { def_test = D.local_var ldef; value_comp = D.local_var lval }
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let ldef, lval = D.locals_from_m lvar in
      let declare_local constr =
        D.let_local ldef se1.def_test (D.let_local lval se1.value_comp constr)
      in
      {
        def_test = declare_local se2.def_test;
        value_comp = declare_local se2.value_comp;
      }

let generate_m_assign (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (var : variable) (offset : offset)
    (oc : Format.formatter) (se : D.expression_composition) : unit =
  let def_var = generate_variable ~def_flag:true var_indexes offset var in
  let val_var = generate_variable var_indexes offset var in
  let def, value = D.build_expression se in
  if D.is_always_true def then
    Format.fprintf oc "%a@,@[<v 2>{@,%a@,@]}"
      (D.format_assign dgfip_flags var_indexes def_var)
      def
      (D.format_assign dgfip_flags var_indexes val_var)
      value
  else
    Format.fprintf oc "%a@,@[<v 2>if(%s){@;%a@]@,}@,else %s = 0.;"
      (D.format_assign dgfip_flags var_indexes def_var)
      def def_var
      (D.format_assign dgfip_flags var_indexes val_var)
      value val_var;
  if dgfip_flags.flg_trace then
    let var = Bir.var_to_mir var in
    Format.fprintf oc "@;aff2(\"%s\", irdata, %s);"
      (Pos.unmark var.Mir.Variable.name)
      (Dgfip_varid.gen_access_pos_from_start var_indexes var)

let generate_var_def (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (var : variable)
    (data : variable_data) (fmt : Format.formatter) : unit =
  match data.var_definition with
  | SimpleVar e ->
      let se = generate_c_expr e var_indexes in
      generate_m_assign dgfip_flags var_indexes var None fmt se
  | TableVar (_, IndexTable es) ->
      Mir.IndexMap.iter
        (fun i v ->
          let sv = generate_c_expr v var_indexes in
          Format.fprintf fmt "@[<hov 2>{@,%a@]}@,"
            (generate_m_assign dgfip_flags var_indexes var (GetValueConst i))
            sv)
        es
  | TableVar (_size, IndexGeneric (v, e)) ->
      (* TODO: boundary checks *)
      let sv = generate_c_expr e var_indexes in
      Format.fprintf fmt "if(%s)@[<hov 2>{%a@]@;}"
        (generate_variable var_indexes None ~def_flag:true v)
        (generate_m_assign dgfip_flags var_indexes var (GetValueVar v))
        sv
  | InputVar -> assert false

let generate_var_cond (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (cond : condition_data)
    (oc : Format.formatter) =
  let def, value =
    D.build_expression @@ generate_c_expr cond.cond_expr var_indexes
  in
  let erreur = Pos.unmark (fst cond.cond_error).Mir.Error.name in
  let code =
    match snd cond.cond_error with
    | None -> "NULL"
    | Some v ->
        Format.sprintf "\"%s\""
          (Pos.unmark (Bir.var_to_mir v).Mir.Variable.name)
  in
  Format.fprintf oc
    "%a@,@[<hov 2>{@,%a@]@,}@,@[<hov 2>if(cond_def && (cond != 0.0)){@,"
    (D.format_assign dgfip_flags var_indexes "cond_def")
    def
    (D.format_assign dgfip_flags var_indexes "cond")
    value;
  Format.fprintf oc "add_erreur(irdata, &erreur_%s, %s);@]@,}" erreur code

let fresh_c_local =
  let c = ref 0 in
  fun name ->
    let s = name ^ string_of_int !c in
    incr c;
    s

let rec generate_stmt (dgfip_flags : Dgfip_options.flags) (program : program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter) (stmt : stmt)
    =
  match Pos.unmark stmt with
  | SAssign (var, vdata) ->
      Format.fprintf oc "@[<v 2>{@,";
      generate_var_def dgfip_flags var_indexes var vdata oc;
      Format.fprintf oc "@]@,}"
  | SConditional (cond, iftrue, iffalse) ->
      Format.fprintf oc "@[<v 2>{@,";
      let cond_val = fresh_c_local "mpp_cond" in
      let cond_def = cond_val ^ "_d" in
      let def, value =
        D.build_expression
        @@ generate_c_expr (Pos.same_pos_as cond stmt) var_indexes
      in
      Format.fprintf oc "%a@;%a"
        (D.format_assign dgfip_flags var_indexes cond_def)
        def
        (D.format_assign dgfip_flags var_indexes cond_val)
        value;
      Format.fprintf oc "@[<hov 2>if(%s && %s){@,%a@]@,}" cond_def cond_val
        (generate_stmts dgfip_flags program var_indexes)
        iftrue;
      if iffalse <> [] then
        Format.fprintf oc "@[<hov 2>else if(%s){@,%a@]@,}" cond_def
          (generate_stmts dgfip_flags program var_indexes)
          iffalse;
      Format.fprintf oc "@]@,}"
  | SVerif v -> generate_var_cond dgfip_flags var_indexes v oc
  | SRovCall r ->
      let rov = ROVMap.find r program.rules_and_verifs in
      generate_rov_function_header ~definition:false oc rov
  | SFunctionCall (f, _) -> Format.fprintf oc "%s(irdata);" f

and generate_stmts (dgfip_flags : Dgfip_options.flags) (program : program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter)
    (stmts : stmt list) =
  Format.fprintf oc "@[<v>";
  Format.pp_print_list (generate_stmt dgfip_flags program var_indexes) oc stmts;
  Format.fprintf oc "@]"

and generate_rov_function_header ~(definition : bool) (oc : Format.formatter)
    (rov : rule_or_verif) =
  let arg_type = if definition then "T_irdata *" else "" in
  let tname, ret_type =
    match rov.rov_code with
    | Rule _ -> ("regle", "int ")
    | Verif _ -> ("verif", "void ")
  in
  let ret_type = if definition then ret_type else "" in
  Format.fprintf oc "%s%s_%s(%sirdata)%s" ret_type tname
    (Pos.unmark rov.rov_name) arg_type
    (if definition then "" else ";")

let generate_rov_function (dgfip_flags : Dgfip_options.flags)
    (program : program) (var_indexes : Dgfip_varid.var_id_map)
    (oc : Format.formatter) (rov : rule_or_verif) =
  let decl, ret =
    let noprint _ _ = () in
    match rov.rov_code with
    | Rule _ -> (noprint, fun fmt () -> Format.fprintf fmt "@,return 0;")
    | Verif _ ->
        ( (fun fmt () -> Format.fprintf fmt "int cond_def;@ double cond;@;"),
          noprint )
  in
  Format.fprintf oc "@[<v 2>%a{@,%a%a%a@]@,}"
    (generate_rov_function_header ~definition:true)
    rov decl ()
    (generate_stmts (dgfip_flags : Dgfip_options.flags) program var_indexes)
    (Bir.rule_or_verif_as_statements rov)
    ret ()

let generate_rov_functions (dgfip_flags : Dgfip_options.flags)
    (program : program) (var_indexes : Dgfip_varid.var_id_map)
    (oc : Format.formatter) (rovs : rule_or_verif list) =
  Format.fprintf oc "@[<v>";
  Format.pp_print_list ~pp_sep:Format.pp_print_cut
    (generate_rov_function
       (dgfip_flags : Dgfip_options.flags)
       program var_indexes)
    oc rovs;
  Format.fprintf oc "@]"

let generate_main_function_signature (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_extracted(T_irdata* irdata)%s"
    (if add_semicolon then ";" else "")

let _generate_main_function_signature_and_var_decls (oc : Format.formatter) () =
  Format.fprintf oc "%a {@\n@[<h 4>    @\n" generate_main_function_signature
    false;
  Format.fprintf oc "int cond_def;@\ndouble cond;@\n@\n";
  Format.fprintf oc
    {|
  #ifdef ANOMALY_LIMIT
  int anomaly_count = 0;
  int max_anomalies = ANOMALY_LIMIT;
  #endif /* ANOMALY_LIMIT */
|}

let _generate_return (oc : Format.formatter) () =
  Format.fprintf oc "@\nreturn 0;@]@\n}\n"

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc
    {|
/* %s */

#ifndef IR_HEADER_
#define IR_HEADER_

#include <stdio.h>

#include "irdata.h"
#include "const.h"
#include "var.h"

double my_var1;

#ifndef FLG_MULTITHREAD
#define add_erreur(a,b,c) add_erreur(b,c)
#endif

|}
    Prelude.message

let generate_footer (oc : Format.formatter) () : unit =
  Format.fprintf oc "\n#endif /* IR_HEADER_ */"

let _generate_get_input_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_get_input_index(char *name)%s"
    (if add_semicolon then ";\n\n" else "")

let _generate_get_input_name_from_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "char* m_get_input_name_from_index(int index)%s"
    (if add_semicolon then ";\n" else "")

let generate_get_input_num_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_num_inputs()%s"
    (if add_semicolon then ";\n\n" else "")

let _generate_get_input_num_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
  Format.fprintf oc "%a {@\n@[<h 4>    return %d;@]@\n}@\n@\n"
    generate_get_input_num_prototype false (List.length input_vars)

let generate_get_output_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_get_output_index(char *name)%s"
    (if add_semicolon then ";\n\n" else "")

let _generate_get_output_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc
    "%a {@\n\
     @[<h 4>    %a@\n\
     printf(\"Output var %%s not found!\\n\", name);@\n\
     exit(-1);@]@\n\
     }@\n\
     @\n"
    generate_get_output_index_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "if (strcmp(\"%s\", name) == 0) { return %d; }"
           (Pos.unmark (Bir.var_to_mir var).Mir.Variable.name)
           i))
    (List.mapi (fun i x -> (x, i)) output_vars)

let generate_get_output_name_from_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "char* m_get_output_name_from_index(int index)%s"
    (if add_semicolon then ";\n\n" else "")

let _generate_get_output_name_from_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc
    "%a {@\n\
     @[<h 4>    %a@\n\
     printf(\"Output index %%d not found!\\n\", index);@\n\
     exit(-1);@]@\n\
     }@\n\
     @\n"
    generate_get_output_name_from_index_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "if (index == %d) { return \"%s\"; }" i
           (Pos.unmark (Bir.var_to_mir var).Mir.Variable.name)))
    (List.mapi (fun i x -> (x, i)) output_vars)

let generate_get_output_num_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_num_outputs()%s"
    (if add_semicolon then ";\n\n" else "")

let _generate_get_output_num_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc "%a {@\n@[<h 4>    return %d;@]@\n}@\n@\n"
    generate_get_output_num_prototype false (List.length output_vars)

let _error_table_definitions (oc : Format.formatter) (program : program) =
  let error_set_size =
    Mir.VariableMap.cardinal program.mir_program.program_conds
  in
  Format.fprintf oc "typedef m_error_occurrence error_occurrences[%d];\n"
    error_set_size;
  Format.fprintf oc "typedef m_error errors[%d];\n" error_set_size

let print_error_line (oc : Format.formatter) (cond_data : Mir.condition_data) =
  let err, var = cond_data.cond_error in
  Format.fprintf oc
    "{.kind = \"%s\", .major_code = \"%s\", .minor_code = \"%s\", .isisf = \
     \"%s\",.description = \"%s\", .code_information = %s},@,"
    (Strings.sanitize_str err.descr.kind)
    (Strings.sanitize_str err.descr.major_code)
    (Strings.sanitize_str err.descr.minor_code)
    (Strings.sanitize_str err.descr.isisf)
    (Strings.sanitize_str err.descr.description)
    (match var with
    | None -> "\"\""
    | Some v -> (
        match v.alias with
        | Some alias -> "\"" ^ alias ^ "\""
        | None -> assert false))

let _generate_errors_table (oc : Format.formatter) (program : program) =
  Format.fprintf oc "@[<hv 2>static const errors m_errors = {@,%a@]};"
    (fun oc conds ->
      Mir.VariableMap.iter
        (fun _ cond_data -> print_error_line oc cond_data)
        conds)
    program.mir_program.program_conds

let generate_get_error_count_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_num_errors()%s"
    (if add_semicolon then ";\n\n" else "")

let _generate_get_error_count_func (oc : Format.formatter) (program : program) =
  Format.fprintf oc {|
%a {
  return %d;
}

|}
    generate_get_error_count_prototype false
    (Mir.VariableMap.cardinal program.mir_program.program_conds)

let generate_mpp_function_protoype (add_semicolon : bool) (return_type : bool)
    (oc : Format.formatter) (function_name : Bir.function_name) =
  let ret_type = if return_type then "struct S_discord *" else "void" in
  Format.fprintf oc "%s %s(T_irdata* irdata)%s" ret_type function_name
    (if add_semicolon then ";" else "")

let generate_mpp_function (dgfip_flags : Dgfip_options.flags)
    (program : Bir.program) (var_indexes : Dgfip_varid.var_id_map)
    (oc : Format.formatter) ((f, ret_type) : Bir.function_name * bool) =
  let { mppf_stmts; mppf_is_verif } =
    Bir.FunctionMap.find f program.mpp_functions
  in
  Format.fprintf oc "@[<v 2>%a{@,%a%s@]@,}@,"
    (generate_mpp_function_protoype false mppf_is_verif)
    f
    (generate_stmts dgfip_flags program var_indexes)
    mppf_stmts
    (if ret_type then
     {|
#ifdef FLG_MULTITHREAD
      return irdata->discords;
#else
      return discords;
#endif
|}
    else "")

let generate_mpp_functions (dgfip_flags : Dgfip_options.flags)
    (program : Bir.program) (oc : Format.formatter)
    (var_indexes : Dgfip_varid.var_id_map) =
  let funcs = Bir.FunctionMap.bindings program.Bir.mpp_functions in
  List.iter
    (fun (fname, { mppf_is_verif; _ }) ->
      generate_mpp_function
        (dgfip_flags : Dgfip_options.flags)
        program var_indexes oc (fname, mppf_is_verif))
    funcs

let generate_mpp_functions_signatures (oc : Format.formatter)
    (program : Bir.program) =
  let funcs = Bir.FunctionMap.bindings program.Bir.mpp_functions in
  Format.fprintf oc "@[<v 0>%a@]@,"
    (Format.pp_print_list (fun ppf (func, { mppf_is_verif; _ }) ->
         generate_mpp_function_protoype true mppf_is_verif ppf func))
    funcs

let generate_rovs_files (dgfip_flags : Dgfip_options.flags) (program : program)
    (vm : Dgfip_varid.var_id_map) =
  let module StringMap = Map.Make (String) in
  let default_file = "default" in
  let filemap =
    ROVMap.fold
      (fun _rov_id rov filemap ->
        let file =
          let pos = Pos.get_position rov.rov_name in
          if pos = Pos.no_pos then default_file
          else
            (Pos.get_file pos |> Filename.basename |> Filename.remove_extension)
            ^ ".c"
        in
        let filerovs =
          match StringMap.find_opt file filemap with
          | None -> []
          | Some fr -> fr
        in
        StringMap.add file (rov :: filerovs) filemap)
      program.rules_and_verifs StringMap.empty
  in
  StringMap.fold
    (fun file rovs orphan ->
      if String.equal file default_file then rovs @ orphan
      else
        let oc = open_out file in
        let fmt = Format.formatter_of_out_channel oc in
        Format.fprintf fmt
          {|
#include <math.h>
#include <stdio.h>
#include "var.h"

#ifndef FLG_MULTITHREAD
#define add_erreur(a,b,c) add_erreur(b,c)
#endif

|};
        generate_rov_functions dgfip_flags program vm fmt rovs;
        Format.fprintf fmt "@\n@.";
        close_out oc;
        orphan)
    filemap []

let generate_implem_header oc header_filename =
  Format.fprintf oc
    {|
/* %s */

#include <string.h>
#include "enchain_static.c.inc"

#include "%s"


|}
    Prelude.message header_filename

let generate_c_program (dgfip_flags: Dgfip_options.flags) (program : program)
    (_function_spec : Bir_interface.bir_function) (filename : string)
    (vm : Dgfip_varid.var_id_map) : unit =
  if Filename.extension filename <> ".c" then
    Errors.raise_error
      (Format.asprintf "Output file should have a .c extension (currently %s)"
         filename);
  let orphan_rovs = generate_rovs_files dgfip_flags program vm in
  let header_filename = Filename.remove_extension filename ^ ".h" in
  let _oc = open_out header_filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a@\n@."
    generate_header ()
    (* error_table_definitions program  *)
    (* generate_get_input_index_prototype true *)
    (* generate_get_input_num_prototype true *)
    (* generate_get_input_name_from_index_prototype true *)
    (* generate_get_output_index_prototype true *)
    (* generate_get_output_name_from_index_prototype true *)
    (* generate_get_output_num_prototype true  *)
    (* generate_get_error_count_prototype true  *)
    generate_mpp_functions_signatures program
    (* generate_main_function_signature true  *)
    generate_footer ();
  close_out _oc;
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a@\n@."
    generate_implem_header header_filename
    (* generate_errors_table program *)
    (* generate_get_error_count_func program  *)
    (* generate_get_input_num_func function_spec *)
    (* generate_get_output_index_func function_spec *)
    (* generate_get_output_name_from_index_func function_spec *)
    (* generate_get_output_num_func function_spec *)
    (generate_rov_functions dgfip_flags program vm) orphan_rovs
    (generate_mpp_functions dgfip_flags program) vm
    (* generate_main_function_signature_and_var_decls ()
     * (generate_stmt program vm)
     *   (Bir.SFunctionCall (program.Bir.main_function, []), Pos.no_pos)
     * generate_return () *) ;
  close_out _oc[@@ocamlformat "disable"]
