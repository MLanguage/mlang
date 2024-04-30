type offset =
  | GetValueConst of int
  | GetValueExpr of string
  | GetValueVar of Com.Var.t
  | PassPointer
  | None

let rec generate_variable (vm : Dgfip_varid.var_id_map) (offset : offset)
    ?(def_flag = false) ?(trace_flag = false) (var : Com.Var.t) : string =
  try
    match offset with
    | PassPointer ->
        if def_flag then Dgfip_varid.gen_access_def_pointer vm var
        else Dgfip_varid.gen_access_pointer vm var
    | _ ->
        let offset =
          match offset with
          | None -> ""
          | GetValueVar offset -> " + (int)" ^ generate_variable vm None offset
          | GetValueConst offset -> " + " ^ string_of_int offset
          | GetValueExpr offset -> Format.sprintf " + (%s)" offset
          | PassPointer -> assert false
        in
        if def_flag then Dgfip_varid.gen_access_def vm var offset
        else
          let access_val = Dgfip_varid.gen_access_val vm var offset in
          (* When the trace flag is present, we print the value of the
             non-temporary variable being used *)
          if trace_flag && not (Com.Var.is_temp var) then
            let vn = Pos.unmark var.Com.Var.name in
            let pos_tgv = Dgfip_varid.gen_access_pos_from_start vm var in
            Format.asprintf "(aff3(\"%s\",irdata, %s), %s)" vn pos_tgv
              access_val
          else access_val
  with Not_found ->
    Errors.raise_error
      (Format.asprintf "Variable %s not found in TGV"
         (Pos.unmark var.Com.Var.name))

type local_var =
  | Anon (* inlined sub-expression, not intended for reuse *)
  | Refered of int
(* declared local variable, either M local or locally bound in the constructors
   below *)

type dflag = Def | Val (* distinguish C types int and double *)

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
  | Daccess of Com.Var.t * dflag * expr
  | Dite of expr * expr * expr
  | Dinstr of string

and expr_var = Local of stack_slot | M of Com.Var.t * offset * dflag

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

let rec expr_position (expr : expr) (st : local_stacks) =
  match expr with
  | Dtrue | Dfalse | Dlit _ | Dvar (M _) -> Not_to_stack
  | Dvar (Local slot) ->
      if is_in_stack_scope slot st then Not_to_stack
      else if is_on_top slot st then On_top slot.kind
      else Must_be_pushed
  | Dbinop ("/", e1, e2) -> begin
      (* avoid storage of division by zero. It assumes all division are
         guarded *)
      match (expr_position e1 st, expr_position e2 st) with
      | On_top s, _ | _, On_top s ->
          On_top s
          (* Needed to bumb the stack to avoid erasing subexpressions *)
      | _, _ -> Not_to_stack (* Either already stored, or duplicatable *)
    end
  | _ -> Must_be_pushed

(* allocate to local variable if necessary *)
let store_local (stacks : local_stacks) (ctx : local_vars) (v : local_var)
    (kind : dflag) (subexpr : expr) =
  (* we allocate only expression that are non-atomic *)
  match expr_position subexpr stacks with
  | Not_to_stack -> (add_substitution stacks v kind subexpr, ctx, subexpr)
  | On_top kind ->
      (* this happens when a subexpression is lifted through several
         constructors. e.g. [ 0 || (0 || e) ] where [e] is allocated and is
         trivially lifted from the or constructors. *)
      (add_substitution stacks v kind subexpr |> bump_stack kind, ctx, subexpr)
  | Must_be_pushed ->
      let depth = stack_top kind stacks in
      let stacks = bump_stack kind stacks in
      let slot = { kind; depth } in
      let assignment = { slot; subexpr } in
      let ctx = (v, assignment) :: ctx in
      (stacks, ctx, Dvar (Local slot))

(* the following functions resolve [constr] values by applying them to a given
   context (both stacks state and existing local allocations) *)

let collapse_constr (stacks : local_stacks) (ctx : local_vars) (constr : constr)
    =
  let expr, kind, lv = constr stacks ctx in
  (expr, kind, lv @ ctx)

(* eval and store with enforced kind *)
let push_with_kind (stacks : local_stacks) (ctx : local_vars) (kind : dflag)
    (constr : constr) =
  let expr, ekind, lv = constr stacks ctx in
  let expr = if kind = ekind then expr else cast kind expr in
  let stacks, lv, expr = store_local stacks lv Anon kind expr in
  (stacks, lv, expr)

(* eval and store without enforcing kind *)
let push (stacks : local_stacks) (ctx : local_vars) (constr : constr) =
  let expr, kind, lv = constr stacks ctx in
  let stacks, lv, expr = store_local stacks lv Anon kind expr in
  (stacks, lv, expr, kind)

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
    (stacks : local_stacks) (ctx : local_vars) =
  let bound, kind, lv = collapse_constr stacks ctx bound in
  let stacks, ctx, _ = store_local stacks lv v kind bound in
  collapse_constr stacks ctx body

let dtrue _stacks _lv : t = (Dtrue, Def, [])

let dfalse _stacks _lv : t = (Dfalse, Def, [])

let lit (f : float) _stacks _lv : t = (Dlit f, Val, [])

let m_var (v : Com.Var.t) (offset : offset) (df : dflag) _stacks _lv : t =
  (Dvar (M (v, offset, df)), df, [])

let local_var (lvar : local_var) (stacks : local_stacks) (ctx : local_vars) : t
    =
  match lvar with
  | Anon -> Errors.raise_error "Tried to access anonymous local variable"
  | Refered v -> (
      match List.assoc_opt v stacks.var_substs with
      | Some (e, kind) -> (e, kind, [])
      | None -> (
          match List.assoc_opt lvar ctx with
          | Some { slot; _ } -> (Dvar (Local slot), slot.kind, [])
          | None -> Errors.raise_error "Local variable not found in context"))

(* Note on constructors with several subexpressions. To correctly allocate
   subvalues in the stacks, the stacks state must flow through all constructor
   arguments to increment "pointers" accordingly. The state at entry represents
   the point at which the constructed expression is expected to be allocated (if
   needed). *)

let dand (e1 : constr) (e2 : constr) (stacks : local_stacks) (ctx : local_vars)
    : t =
  let stacks', lv1, e1 = push_with_kind stacks ctx Def e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Def e2 in
  match (e1, e2) with
  | Dtrue, _ -> (e2, Def, lv2)
  | _, Dtrue -> (e1, Def, lv1)
  | Dfalse, _ | _, Dfalse -> (Dfalse, Def, [])
  | Dvar v1, Dvar v2 when v1 = v2 -> (e1, Def, lv1)
  | _ -> (Dand (e1, e2), Def, lv2 @ lv1)

let dor (e1 : constr) (e2 : constr) (stacks : local_stacks) (ctx : local_vars) :
    t =
  let stacks', lv1, e1 = push_with_kind stacks ctx Def e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Def e2 in
  match (e1, e2) with
  | Dtrue, _ | _, Dtrue -> (Dtrue, Def, [])
  | Dfalse, _ -> (e2, Def, lv2)
  | _, Dfalse -> (e1, Def, lv1)
  | Dvar v1, Dvar v2 when v1 = v2 -> (e1, Def, lv1)
  | _ -> (Dor (e1, e2), Def, lv2 @ lv1)

let dnot (e : constr) (stacks : local_stacks) (ctx : local_vars) : t =
  let _, lv, e = push_with_kind stacks ctx Def e in
  match e with
  | Dtrue -> (Dfalse, Def, [])
  | Dfalse -> (Dtrue, Def, [])
  | Dunop ("!", e) -> (e, Def, lv)
  | _ -> (Dunop ("!", e), Def, lv)

let minus (e : constr) (stacks : local_stacks) (ctx : local_vars) : t =
  let _, lv, e = push_with_kind stacks ctx Val e in
  match e with
  | Dlit f -> (Dlit (-.f), Val, [])
  | Dunop ("-", e) -> (e, Val, lv)
  | _ -> (Dunop ("-", e), Val, lv)

let plus (e1 : constr) (e2 : constr) (stacks : local_stacks) (ctx : local_vars)
    : t =
  (* This optimisation causes some valuation to end at -0.0 where +0.0 was
     expected. Staying conservative for now *)
  let reduce_zero_add = false in
  let stacks', lv1, e1 = push_with_kind stacks ctx Val e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Val e2 in
  match (e1, e2) with
  | Dlit 0., _ when reduce_zero_add -> (e2, Val, lv2)
  | _, Dlit 0. when reduce_zero_add -> (e1, Val, lv1)
  | Dlit f1, Dlit f2 -> (Dlit (f1 +. f2), Val, [])
  | _ -> (Dbinop ("+", e1, e2), Val, lv2 @ lv1)

let sub (e1 : constr) (e2 : constr) (stacks : local_stacks) (ctx : local_vars) :
    t =
  let stacks', lv1, e1 = push_with_kind stacks ctx Val e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Val e2 in
  match (e1, e2) with
  | Dlit 0., _ -> (Dunop ("-", e2), Val, lv2)
  | _, Dlit 0. -> (e1, Val, lv1)
  | Dlit f1, Dlit f2 -> (Dlit (f1 -. f2), Val, [])
  | _ -> (Dbinop ("-", e1, e2), Val, lv2 @ lv1)

let mult (e1 : constr) (e2 : constr) (stacks : local_stacks) (ctx : local_vars)
    : t =
  let stacks', lv1, e1 = push_with_kind stacks ctx Val e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Val e2 in
  match (e1, e2) with
  | Dlit 1., _ -> (e2, Val, lv2)
  | _, Dlit 1. -> (e1, Val, lv1)
  | Dlit 0., _ | _, Dlit 0. -> (Dlit 0., Val, [])
  | Dlit f1, Dlit f2 -> (Dlit (f1 *. f2), Val, [])
  | _ -> (Dbinop ("*", e1, e2), Val, lv2 @ lv1)

let div (e1 : constr) (e2 : constr) (stacks : local_stacks) (ctx : local_vars) :
    t =
  let stacks', lv1, e1 = push_with_kind stacks ctx Val e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Val e2 in
  match (e1, e2) with
  | _, Dlit 1. -> (e1, Val, lv1)
  | Dlit f1, Dlit f2 ->
      let f = f1 /. f2 in
      (Dlit f, Val, [])
  | _ -> (Dbinop ("/", e1, e2), Val, lv2 @ lv1)

let comp op (e1 : constr) (e2 : constr) (stacks : local_stacks)
    (ctx : local_vars) : t =
  let stacks', lv1, e1 = push_with_kind stacks ctx Val e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Val e2 in
  let comp (o : Com.comp_op) =
    match (e1, e2) with
    | Dlit f1, Dlit f2 ->
        if
          Bir_interpreter.FloatDefInterp.compare_numbers o
            (Bir_number.RegularFloatNumber.of_float f1)
            (Bir_number.RegularFloatNumber.of_float f2)
        then Dtrue
        else Dfalse
    | Dvar v1, Dvar v2 ->
        if String.equal op "==" && v1 = v2 then Dtrue else Dbinop (op, e1, e2)
    | _ -> Dbinop (op, e1, e2)
  in
  let e =
    match op with
    | "==" -> comp Com.Eq
    | "!=" -> comp Com.Neq
    | "<=" -> comp Com.Lte
    | "<" -> comp Com.Lt
    | ">=" -> comp Com.Gte
    | ">" -> comp Com.Gt
    | _ -> assert false
  in
  (e, Def, lv2 @ lv1)

let dfun (f : string) (args : constr list) (stacks : local_stacks)
    (ctx : local_vars) : t =
  let (_, lv), args =
    List.fold_left_map
      (fun (stacks, lv) e ->
        let stacks, lv', e = push_with_kind stacks ctx Val e in
        ((stacks, lv' @ lv), e))
      (stacks, []) args
  in
  (* TODO : distinguish kinds *)
  (Dfun (f, args), Val, lv)

let dinstr (i : string) (_stacks : local_stacks) (_ctx : local_vars) : t =
  (Dinstr i, Val, [])

let access (var : Com.Var.t) (df : dflag) (e : constr) (stacks : local_stacks)
    (ctx : local_vars) : t =
  let _, lv, e = push_with_kind stacks ctx Val e in
  (Daccess (var, df, e), df, lv)

let ite (c : constr) (t : constr) (e : constr) (stacks : local_stacks)
    (ctx : local_vars) : t =
  let stacks', lvc, c = push_with_kind stacks ctx Def c in
  let stacks', lvt, t, tkind = push stacks' ctx t in
  let _, lve, e, ekind = push stacks' ctx e in
  let ite_kind =
    if tkind = ekind then tkind
    else (* this will happen. Staying on the safe side *) Val
  in
  match (c, t, e) with
  | Dtrue, _, _ -> (t, tkind, lvt)
  | Dfalse, _, _ -> (e, ekind, lve)
  | _, Dtrue, Dtrue | _, Dfalse, Dfalse -> (t, tkind, lvt)
  | _, Dlit 1., Dlit 0. -> (c, Def, lvc)
  | _, Dlit f, Dlit f' when f = f' -> (Dlit f, ite_kind, [])
  | _ -> (Dite (c, t, e), ite_kind, lve @ lvt @ lvc)

let it0 (c : constr) (t : constr) (stacks : local_stacks) (ctx : local_vars) : t
    =
  (* Version of [ite] where the else is zero with kind matching the then to
     avoid casting later *)
  let stacks', lvc, c = push_with_kind stacks ctx Def c in
  let _, lvt, t, tkind = push stacks' ctx t in
  let e, ekind =
    match tkind with Def -> (Dfalse, Def) | Val -> (Dlit 0., Val)
  in
  match (c, t) with
  | Dtrue, _ -> (t, tkind, lvt)
  | Dfalse, _ -> (e, ekind, [])
  | _, (Dlit 1. | Dtrue) -> (c, Def, lvc)
  | _, (Dlit 0. | Dfalse) -> (t, tkind, [])
  | _ -> (Dite (c, t, e), tkind, lvt @ lvc)

let build_transitive_composition ?(safe_def = false)
    ({ def_test; value_comp } : expression_composition) : expression_composition
    =
  (* `safe_def` can be set on call when we are sure that `value_comp` will
     always happen to be zero when `def_test` ends up false. E.g. arithmetic
     operation have such semantic property (funny question is what's the
     causality ?). This allows to remove a check to the definition flag when we
     compute the value, avoiding a lot of unnecessary code. *)
  let value_comp = if safe_def then value_comp else it0 def_test value_comp in
  { def_test; value_comp }

type local_decls = int * int (* in practice, stacks sizes *)

(* evaluate a complete (AKA, context free) expression. Not to be used for
   further construction. *)
let build_expression (expr_comp : expression_composition) : local_decls * t * t
    =
  let empty_stacks = { def_top = 0; val_top = 0; var_substs = [] } in
  let empty_locals = [] in
  let ((_, _, def_locals) as def_test) =
    collapse_constr empty_stacks empty_locals expr_comp.def_test
  in
  let ((_, _, value_locals) as value_comp) =
    collapse_constr empty_stacks empty_locals expr_comp.value_comp
  in
  let stacks_size =
    List.fold_left
      (fun (def_s, val_s) (_, { slot; _ }) ->
        match slot.kind with
        | Def -> (max slot.depth def_s, val_s)
        | Val -> (def_s, max slot.depth val_s))
      (-1, -1)
      (def_locals @ value_locals)
  in
  (stacks_size, def_test, value_comp)

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
        (generate_variable ~trace_flag:dgfip_flags.flg_trace vm offset ~def_flag
           var)

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
      Format.fprintf fmt "@[<hov 2>(%a@ && %a@])" format_dexpr de1 format_dexpr
        de2
  | Dor (de1, de2) ->
      Format.fprintf fmt "@[<hov 2>(%a@ || %a@])" format_dexpr de1 format_dexpr
        de2
  | Dunop (op, de) -> Format.fprintf fmt "@[<hov 2>(%s%a@])" op format_dexpr de
  | Dbinop (op, de1, de2) -> begin
      match op with
      | ">" ->
          Format.fprintf fmt "@[<hov 2>(GT_E((%a),(%a))@])" format_dexpr de1
            format_dexpr de2
      | "<" ->
          Format.fprintf fmt "@[<hov 2>(LT_E((%a),(%a))@])" format_dexpr de1
            format_dexpr de2
      | ">=" ->
          Format.fprintf fmt "@[<hov 2>(GE_E((%a),(%a))@])" format_dexpr de1
            format_dexpr de2
      | "<=" ->
          Format.fprintf fmt "@[<hov 2>(LE_E((%a),(%a))@])" format_dexpr de1
            format_dexpr de2
      | "==" ->
          Format.fprintf fmt "@[<hov 2>(EQ_E((%a),(%a))@])" format_dexpr de1
            format_dexpr de2
      | "!=" ->
          Format.fprintf fmt "@[<hov 2>(NEQ_E((%a),(%a))@])" format_dexpr de1
            format_dexpr de2
      | _ ->
          Format.fprintf fmt "@[<hov 2>((%a)@ %s (%a)@])" format_dexpr de1 op
            format_dexpr de2
    end
  | Dfun (funname, des) ->
      Format.fprintf fmt "@[<hov 2>%s(%a@])" funname
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           format_dexpr)
        des
  | Dinstr instr -> Format.fprintf fmt "%s" instr
  | Daccess (var, dflag, de) ->
      Format.fprintf fmt "(%s[(int)%a])"
        (generate_variable ~def_flag:(dflag = Def)
           ~trace_flag:dgfip_flags.flg_trace vm PassPointer var)
        format_dexpr de
  | Dite (dec, det, dee) ->
      Format.fprintf fmt "@[<hov 2>(%a ?@ %a@ : %a@])" format_dexpr dec
        format_dexpr det format_dexpr dee

let rec format_local_declarations fmt
    ((def_stk_size, val_stk_size) : local_decls) =
  if def_stk_size >= 0 then (
    Format.fprintf fmt "@[<hov 2>register int int%d;@]@," def_stk_size;
    format_local_declarations fmt (def_stk_size - 1, val_stk_size))
  else if val_stk_size >= 0 then (
    Format.fprintf fmt "@[<hov 2>register double real%d;@]@," val_stk_size;
    format_local_declarations fmt (def_stk_size, val_stk_size - 1))
  else ()

let format_local_vars_defs (dgfip_flags : Dgfip_options.flags)
    (vm : Dgfip_varid.var_id_map) fmt (lv : local_vars) =
  let lv = List.rev lv in
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
